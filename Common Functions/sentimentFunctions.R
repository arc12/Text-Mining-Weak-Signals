##
## Prepare lexicons from CSV dictionary lists. Assumes Harvard General Inquirer.
## @param csvFile is where to find a CSV file comprising columns from the "Basic" spreadsheet
## @param targets is a list, where each named item becomes a dictionary of the same name
##          and the list elements are vectors of column names to combine
## Returns a list of dictionaries
##
#e.g. targets<-list(Economic=c("Econ.","ECON"), Legal="Legal", Political=c("Polit.", "POLIT"), Social =c("Role", "SocRel"))

prepareLexicons<-function(csvFile, targets){
   inquirer.table<-read.csv(csvFile, header=TRUE, sep=",", quote="\"", stringsAsFactors=FALSE)
   sentiment.dics<-list()
   # for each sentiment, find out which words are relevant and for cases where there is more
   # than one usage (denoted #1 in the word), select only the first one as this is the most frequent in general
   for(i in 1:length(targets)){
      dic<-inquirer.table[,"Entry"]
      target.cols<-targets[[i]]
      if(length(target.cols)>1){
         merged.filter<-row_sums(inquirer.table[,target.cols]!="")>0
      }else{
         merged.filter<-inquirer.table[,target.cols]!=""
      }
      dic<-dic[merged.filter]#limit to words for sentiment
      dic<-sub("#1","",dic)#remove '#1' from any words containing it
      dic<-dic[-grep("#",dic)]#remove all words still containing #
      #manually remove some terms that cause bias
      ii<-which(dic=="PROJECT")
      if(length(ii)>0){
         dic<-dic[-ii]
      }
      #store the dictionary
      sentiment.dics[[i]]<-tolower(dic)
   }
   names(sentiment.dics)<-names(targets)
   return (sentiment.dics)
}