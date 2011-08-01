## ***Made available using the The MIT License (MIT)***
# Copyright (c) 2011, Adam Cooper
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
## ************ end licence ***************
## ----------------------------------------
## Inspect a corpus segmented into "the past" and "recent"
## And identify rising, falling and newly-occurring terms
## ----------------------------------------
library("tm")
library("Snowball")
library("slam")
library("colorspace")

##
## NB {data_set_name}/RF_Init.R should be run first to establish the run parameters
##

#var to use to make some more space at the bottom of plots for long label (use par(mar=mar.bigmar) )
mar.default<-par("mar")
mar.bigmar<-c(6,4,4,2)

#start logging all std output to a file in addition to "printing" to console
#first clean up old sinks
while(sink.number()>0)
  {sink()}
sink(file="RF_Terms.log", append=FALSE, type="output", split=TRUE)
cat(paste(paste(rep("=",79),collapse="")),"\n")
cat(paste(title,"\n"))
cat(paste(paste(rep("=",79),collapse="")),"\n\n")
print(paste("From:",start.date," Split at:",key.date, sep=""))
timestamp(stamp=date(), prefix="##TIMESTAMP: ")

##
## Read in the abstracts. NB this code allows for vectors of csv file names, titles etc in RF_Init.R
##
#(aside) rbinding tables is faster and better than merging corpora using tm_combine, which leads to problematical duplicate document ids. handling Origin as below rather than meta(.. tag="Origin")<-value is also MUCH FASTER
table<-NULL
for (src in 1:length(abstracts.csv)){
   # read in CSV with format year,pages,title,authors,abstract,keywords. There is a header row. title/authors/keywords are delimited by "
   tmp_table<-read.csv(abstracts.csv[[src]],header=TRUE,sep=",",quote="\"")
   #insert the "origin" as a new column
   origin<-rep(src, length(tmp_table[,1]))
   tmp_table<-cbind(origin,tmp_table)
   #accumulate the table            
   table<-rbind(table,tmp_table)
   tmp_table<-NULL
}
# now read in the possibly-cumulated table to a corpus, handling the metadata via mapping
#create a mapping from datatable column names to PlainTextDocument attribute names
#"Keywords" is a user-defined "localmetadata" property while the rest are standard tm package document metadata fields
map<-list(Content="abstract", Heading="title", Author="authors", DateTimeStamp="year", Origin="origin", Keywords="keywords")
#use the mapping while reading the dataframe source to create a coprus with metadata
corp<-Corpus(DataframeSource(table), readerControl=list(reader= readTabular(mapping=map)))

# trim the corpus so that it begins at the specified date - "past" abstracts only go back so far -
# and also not later than the target year
#corp<-tm_filter(corp,FUN=sFilter, doclevel = TRUE, useMeta = FALSE, paste("datetimestamp>'",start.date,"'",sep=""))
corp<-tm_filter(corp,FUN=sFilter, doclevel = TRUE, useMeta = FALSE, paste("datetimestamp<='",last.date,"' & datetimestamp>'",start.date,"'",sep=""))

## -----
## Standard pre-processing to get a document-term matrix of the entire corpus.
## -----
#use the standard stopword set with a few modifications!
#+  "paper" (which is common in journal/proceedings abstracts!)
stop.words<-c(stopwords(language = "en"),"paper")
#- some terms (and various expansions) that are relevant to the education domain
stop.words<-stop.words[-grep("group", stop.words)]
stop.words<-stop.words[-grep("problem", stop.words)]
stop.words<-stop.words[-grep("present", stop.words)]
stop.words<-stop.words[-grep("work", stop.words)]
print("Stopwords being used")
print(stop.words)
cat(paste(paste(rep("-",79),collapse="")),"\n\n")
#Here we go////
dtm.tf<-DocumentTermMatrix(corp,
  control=list(stemming=TRUE, stopwords=stop.words, minWordLength=3, removeNumbers=TRUE, removePunctuation=TRUE))
dtm.bin<-weightBin(dtm.tf)#used later to count docs containing at least 1 occurrence of a term
#compute some corpus and term statistics FOR INFORMATION
print("Computed Document Term Matrix, Term-Frequency")
print(dtm.tf)
dtm.tf.sums<-col_sums(dtm.tf)
cat("\n")
print("Summary Stats of (Total) Term Occurrences in the Corpus")
print(summary(dtm.tf.sums))

# read in the latest conference themes and extract term frequencies using same method as abstracts
# This may (or may not) correlate with "rising" or new terms in the abstracts, in which case the rise may be explained away.
if(!is.na(recent.themes.txt)){
      themes<-PlainTextDocument(x = paste(scan(file=recent.themes.txt, what="raw"),collapse=" "), language="english")
      themes.words<-tolower(stripWhitespace(removeWords(removeNumbers(removePunctuation(themes)),words=stop.words)))
      themes.terms<-stemDocument(themes.words, language="english")
      themes.tf<-termFreq(themes.terms)
      themes.tw<-termFreq(themes.words)    #unstemmed. Done this way because stemCompletion needs a corpus
      #show them all.
      barplot(themes.tw, las=2, cex.names=0.7, main="Words In the Conference Themes Description",
              ylab="Frequency")
      # Repeat the previous to create a PNG
      png("Images/ThemesWords.png", width=1000, height=1000,pointsize=12, res=150)
      barplot(themes.tw, las=2, cex.names=0.7, main="Words In the Conference Themes Description",
              ylab="Frequency")
      ad<-dev.off()
      # TO DO? - gephi output to create a tag cloud to match
}


##
## APply a filter using key.date to get two lists of document ids, one for past and one for recent
##
past.doc_ids.bool<-tm_index(corp,FUN=sFilter, doclevel = TRUE, useMeta = FALSE, paste("datetimestamp<'",key.date,"'",sep=""))
print(paste(sum(past.doc_ids.bool),"documents in the reference set and",sum(!past.doc_ids.bool)," documents in the target set",sep=" "))
term.sums.past<-col_sums(dtm.tf[past.doc_ids.bool,])
tsp.all<-sum(term.sums.past)
term.sums.recent<-col_sums(dtm.tf[!past.doc_ids.bool,])
tsr.all<-sum(term.sums.recent)
#term sums of 0 in the past must be new terms, since we know the corpus sum>0
new.term_ids.bool <- (term.sums.past==0)
#which terms should be considered in rising/falling? must occur above the "enough" theshold in both sets
rise.term_ids.bool <-(term.sums.past>=enough.thresh.rise) & (term.sums.recent/tsr.all>term.sums.past/tsp.all)
enough.thresh.fall <- quantile(term.sums.past, probs=0.9)#compute the enough.thresh.fall at the top 10% of past term frequencies
print(paste("Threshold of",enough.thresh.fall,"used for the occurrence of terms in the previous set when considering falling frequencies"))
fall.term_ids.bool <-(term.sums.past>=enough.thresh.fall) & (term.sums.recent/tsr.all<term.sums.past/tsp.all)
# "nearly-new" terms are those that don't make the grade for the rising/falling calculation yet are not "new"
nearly_new.term_ids.bool <- (term.sums.past<=n_new.thresh) & !new.term_ids.bool & (term.sums.recent >0)

##
## Inspect the distribution of new terms.
##
print("Summary of New Terms")
dtm.tf.new<-dtm.tf[!past.doc_ids.bool,new.term_ids.bool]
term.sums.new<-col_sums(dtm.tf.new)
term.sums.new.t<-tabulate(term.sums.new)
print(summary(term.sums.new))
print("Deciles")
print(quantile(term.sums.new, probs=seq(0.1,0.9,0.1)))
par(mar=mar.bigmar)
barplot(term.sums.new.t,main="New Term Occurrence", xlab="Term Frequency (count)", ylab="Number of Terms", names.arg=seq(1:max(term.sums.new)))
# Repeat the previous to create a png
png("Images/NewTermFrequencies.png", width=1000, height=1000,pointsize=12, res=150)
barplot(term.sums.new.t,main="New Term Occurrence", xlab="Term Frequency (count)", ylab="Number of Terms", names.arg=seq(1:max(term.sums.new)))
ad<-dev.off()
#select those terms meeting the thresholds, use the binary DTM to count document occurrences
dtm.tf.new<-dtm.tf.new[,term.sums.new>=new.thresh]
dtm.bin.new<-weightBin(dtm.tf.new) ########################
dtm.tf.new<-dtm.tf.new[,col_sums(dtm.bin.new)>=doc_count.thresh]
dtm.tf.new<-dtm.tf.new[row_sums(dtm.tf.new)>0,]
dtm.bin.new<-weightBin(dtm.tf.new)
cnt.docs_with_new<-col_sums(dtm.bin.new)
print(paste("Doc-Term Matrix after Filtering for New Terms Above Threshold (>=",new.thresh," occurrences)", sep=""))
print(dtm.tf.new)
term.sums.new.sel<-col_sums(dtm.tf.new) #########################
corp.new<-corp[Docs(dtm.tf.new)]
#find unstemmed words for the selected terms to make for prettier plots
new.sel.words<-stemCompletion(names(term.sums.new.sel),corp.new,type="shortest")
new.sel.words[is.na(new.sel.words)]<-names(new.sel.words[is.na(new.sel.words)])
# chart the term frequency for above-thresh new terms 
par(mar=mar.default)
x.pos<-barplot(term.sums.new.sel,main="Above-Threshold New Terms", ylab="Term Frequency", names.arg="")
par(srt=90)
text(x=x.pos, y=0.1, new.sel.words, adj=c(0,0.5))# puts labels inside bars
par(srt=0)
# Repeat the previous plot to create png
png("Images/NewTermFrequencies_Selected.png", width=1000, height=1000,pointsize=12, res=150)
x.pos<-barplot(term.sums.new.sel,main="Above-Threshold New Terms", ylab="Term Frequency", names.arg="")
par(srt=90)
text(x=x.pos, y=0.1, new.sel.words, adj=c(0,0.5))# puts labels inside bars
par(srt=0)
ad<-dev.off()
#plot the docyument occurrences
par(mar=mar.default)
x.pos<-barplot(cnt.docs_with_new, las=2,  main="Occurrence of New Terms",
        ylab="Number of Docs", names.arg="", axes=FALSE)
axis(2, las=1, at=0:max(cnt.docs_with_new))# to get integers only
par(srt=90)
text(x=x.pos, y=0.1, new.sel.words, adj=c(0,0.5))# puts labels inside bars
par(srt=0)
png("Images/NewTerm_DocOccurrence_Bar.png", width=1000, height=1000,pointsize=12, res=150)
x.pos<-barplot(cnt.docs_with_new, las=2, main="Occurrence of New Terms",
        ylab="Number of Docs", names.arg="", axes=FALSE)
axis(2, las=1, at=0:max(cnt.docs_with_new))
par(srt=90)
text(x=x.pos, y=0.1, new.sel.words, adj=c(0,0.5))
par(srt=0)
ad<-dev.off()
par(mar=mar.bigmar)        
heatmap(as.matrix(dtm.tf.new), main="New Term Occurrence", labCol=new.sel.words, margins=c(10,5))
png("Images/NewTerm_DocOccurrence_Heat.png", width=1000, height=1000,pointsize=12, res=150)
heatmap(as.matrix(dtm.tf.new), main="New Term Occurrence", labCol=new.sel.words, margins=c(10,5))
ad<-dev.off()

##
## Consider the "nearly-new" terms. These appear <enough.thresh but >0 in the past
##
print("Summary of \"Nearly New\" Terms")
dtm.tf.n_new<-dtm.tf[!past.doc_ids.bool,nearly_new.term_ids.bool]
term.sums.n_new<-col_sums(dtm.tf.n_new)
summary(term.sums.n_new)
print("Deciles")
print(quantile(term.sums.n_new, probs=seq(0.1,0.9,0.1)))
par(mar=mar.bigmar)
barplot(tabulate(term.sums.n_new),main="Nearly-New Term Occurrence", xlab="Term Frequency (count)", ylab="Number of Terms", names.arg=seq(1:max(term.sums.n_new)))
# Repeat the previous plot to create png
png("Images/NearlyNewTermFrequencies.png", width=1000, height=1000,pointsize=12, res=150)
barplot(tabulate(term.sums.n_new),main="Nearly-New Term Occurrence", xlab="Term Frequency (count)", ylab="Number of Terms", names.arg=seq(1:max(term.sums.n_new)))
ad<-dev.off()
#select those terms meeting the thresholds, use the binary DTM to count document occurrences
dtm.tf.n_new<-dtm.tf.n_new[,term.sums.n_new>=(new.thresh+n_new.thresh)]
dtm.bin.n_new<-weightBin(dtm.tf.n_new)
dtm.tf.n_new<-dtm.tf.n_new[,col_sums(dtm.bin.n_new)>=doc_count.thresh]
dtm.tf.n_new<-dtm.tf.n_new[row_sums(dtm.tf.n_new)>0,]
dtm.bin.n_new<-weightBin(dtm.tf.n_new)
cnt.docs_with_nnew<-col_sums(dtm.bin.n_new)
print(paste("Doc-Term Matrix after Filtering for Nearly New Terms Above Threshold (>=",new.thresh+n_new.thresh," occurrences)", sep=""))
print(dtm.tf.new)
term.sums.n_new.sel<-col_sums(dtm.tf.n_new)
corp.n_new<-corp[Docs(dtm.tf.n_new)]
#find unstemmed words for the selected terms to make for prettier plots
n_new.sel.words<-stemCompletion(names(term.sums.n_new.sel),corp.n_new,type="shortest")
n_new.sel.words[is.na(n_new.sel.words)]<-names(n_new.sel.words[is.na(n_new.sel.words)])
# chart the term frequency for above-thresh nearly-new terms
par(mar=mar.default)
barplot(term.sums.n_new.sel,main="Above-Threshold Nearly New Terms", ylab="Term Frequency", names.arg="")
par(srt=90)
text(x=x.pos, y=0.1, n_new.sel.words, adj=c(0,0.5))# puts labels inside bars
par(srt=0)     
# Repeat the previous plot to create png
png("Images/NearlyNewTermFrequencies_Selected.png", width=1000, height=1000,pointsize=12, res=150)
barplot(term.sums.n_new.sel,main="Above-Threshold Nearly New Terms", ylab="Term Frequency", names.arg="")
par(srt=90)
text(x=x.pos, y=0.1, n_new.sel.words, adj=c(0,0.5))# puts labels inside bars
par(srt=0)
ad<-dev.off()
#document occurrences
par(mar=mar.default)
x.pos<-barplot(cnt.docs_with_nnew, las=2, 
        main="Occurrence of Nearly New Terms", ylab="Number of Docs", names.arg="", axes=FALSE)
axis(2, las=1, at=0:max(cnt.docs_with_nnew))# to get integers only
par(srt=90)
text(x=x.pos, y=0.1, n_new.sel.words, adj=c(0,0.5))# puts labels inside bars
par(srt=0)
png("Images/NearlyNewTerm_DocOccurrence_Bar.png", width=1000, height=1000,pointsize=12, res=150)
x.pos<-barplot(cnt.docs_with_nnew, las=2, main="Occurrence of Nearly New Terms",
        ylab="Number of Docs", names.arg="", axes=FALSE)
axis(2, las=1, at=0:max(cnt.docs_with_nnew))# to get integers only
par(srt=90)
text(x=x.pos, y=0.1, n_new.sel.words, adj=c(0,0.5))# puts labels inside bars
par(srt=0)
ad<-dev.off()
par(mar=mar.bigmar)
heatmap(as.matrix(dtm.tf.n_new), main="Nearly New Term Occurrence", labCol=n_new.sel.words, margins=c(10,5))
png("Images/NearlyNewTerm_DocOccurrence_Heat.png", width=1000, height=1000,pointsize=12, res=150)
heatmap(as.matrix(dtm.tf.n_new), main="Nearly New Term Occurrence", labCol=n_new.sel.words, margins=c(10,5))
ad<-dev.off()

##
## Inspect the distribution of rise/fall. This should indicate a choice of ratio.thresh
## (maybe should calc the thresh as the 9th decile)
##
#calculate a rise/fall factor excluding any new terms
#basically: (recent fraction - past fraction)/(past fraction) but rescaled to a % to give "nicer" numbers for display
# rise and fall have different threshold frequencies for inclusion so are dealt with separately
tsp.rise<-term.sums.past[rise.term_ids.bool]
tsr.rise<-term.sums.recent[rise.term_ids.bool]
rise.ratio<-(tsr.rise*tsp.all/(tsr.all*tsp.rise) -1)* 100
tsp.fall<-term.sums.past[fall.term_ids.bool]
tsr.fall<-term.sums.recent[fall.term_ids.bool]
fall.ratio<-(tsr.fall*tsp.all/(tsr.all*tsp.fall) -1)* 100
print("Summary of Rising Terms: Distribution of % Rise")
print(summary(rise.ratio))
print("Deciles:")
print(quantile(rise.ratio, probs=seq(0.1,0.9,0.1)))
par(mar=mar.default)
barplot(rise.ratio,main="Rising Terms", ylab="Rise Ratio (%)",names.arg="")
png("Images/RisingTerm_Spectrum.png", width=1000, height=1000,pointsize=12, res=150)
barplot(rise.ratio,main="Rising Terms", ylab="Rise Ratio (%)",names.arg="")
ad<-dev.off()
par(mar=mar.bigmar)
hist(rise.ratio,breaks=50, main="Histogram of Rising Terms", xlab="Rise Ratio (%)", xlim=c(0, max(rise.ratio)))
png("Images/RisingTerm_Distribution.png", width=1000, height=1000,pointsize=12, res=150)
hist(rise.ratio,breaks=50, main="Histogram of Rising Terms", xlab="Rise Ratio (%)", xlim=c(0, max(rise.ratio)))
ad<-dev.off()
print("Summary of Falling Terms")
print(summary(fall.ratio))
print("Deciles:")
print(quantile(fall.ratio, probs=seq(0.1,0.9,0.1)))
par(mar=mar.default)
barplot(fall.ratio,main="Falling Terms", ylab="Fall Ratio (%)",names.arg="")
png("Images/FallingTerm_Spectrum.png", width=1000, height=1000,pointsize=12, res=150)
barplot(fall.ratio,main="Falling Terms", ylab="Fall Ratio (%)",names.arg="")
ad<-dev.off()
par(mar=mar.bigmar)
hist(fall.ratio,breaks=50, main="Histogram of Falling Terms", xlab="Fall Ratio (%)", xlim=c(-110, max(fall.ratio)))
png("Images/FallingTerm_Distribution.png", width=1000, height=1000,pointsize=12, res=150)
hist(fall.ratio,breaks=50, main="Histogram of Falling Terms", xlab="Fall Ratio (%)", xlim=c(-110, max(fall.ratio)))
ad<-dev.off()

##
## Now use the thresholds to select "interesting" results
## 
rising.selected<-rise.ratio[rise.ratio>=rise.pc.thresh]
falling.selected<-fall.ratio[fall.ratio<=fall.pc.thresh]
dtm.bin.rising<-dtm.bin[!past.doc_ids.bool,rise.term_ids.bool]#recent docs with rising terms
dtm.bin.rising<-dtm.bin.rising[,rise.ratio>=rise.pc.thresh]#filter to >= the threshold for rising
dtm.bin.rising<-dtm.bin.rising[row_sums(dtm.bin.rising)>0,]#remove docs without any above-thresh terms
#get the TF doc-term-matrix to match
dtm.tf.rising<- dtm.tf[Docs(dtm.bin.rising),rise.term_ids.bool][,rise.ratio>=rise.pc.thresh]
corp.rising<-corp[Docs(dtm.bin.rising)]
#no doc selection for falling as it wouldn't make sense

##
## Get full-words in place of stemmed terms to make for prettier presentation
##replace NAs by the stemmed term (which is often the answer, dunno why stemCompletion doesn't do this)
#NB rising and falling use different copus argument
rising.selected.words<-stemCompletion(names(rising.selected),corp.rising,type="shortest")
rising.selected.words[is.na(rising.selected.words)]<-names(rising.selected[is.na(rising.selected.words)])
falling.selected.words<-stemCompletion(names(falling.selected),corp,type="shortest")#NB the default type is "prevalent"
falling.selected.words[is.na(falling.selected.words)]<-names(falling.selected[is.na(falling.selected.words)])

##
## If theme information is available, list the new, nearly-new and rising terms mentioned
##
in_themes.msg.new<-""
in_themes.msg.n_new<-""
in_themes.msg.rising<-""
if(!is.na(recent.themes.txt)){
   # match returns a vector with elements containing either NA or an integer.
   # an integer i in position k indicates that themes.tf[k] has name = that of rising.selected[i]
   new.terms.in_themes <- na.exclude(match(names(themes.tf), names(term.sums.new.sel)))
   n_new.terms.in_themes <- na.exclude(match(names(themes.tf), names(term.sums.n_new.sel)))
   rising.terms.in_themes <- na.exclude(match(names(themes.tf), names(rising.selected)))
   if(length(new.terms.in_themes)>0){
      in_themes.msg.new<-paste("The following are matches between the conference theme description and above-threshold New Terms:",paste(as.character(new.sel.words[new.terms.in_themes]),collapse=", "))
   }else{
      in_themes.msg.new<-"No terms in the conference theme appear as above-threshold New Terms"
   }
   print(in_themes.msg.new)
   if(length(n_new.terms.in_themes)>0){
       in_themes.msg.n_new<-paste("The following are matches between the conference theme description and above-threshold Nearly New Terms:", paste(as.character(n_new.sel.words[n_new.terms.in_themes]),collapse=", "))
   }else{
      in_themes.msg.n_new<-"No terms in the conference theme appear as above-threshold Nearly New Terms"
   }
   print(in_themes.msg.n_new)                                          
   if(length(rising.terms.in_themes)>0){
        in_themes.msg.rising<-paste("The following are matches between the conference theme description and above-threshold Rising Terms:", paste(as.character(rising.selected.words[rising.terms.in_themes]),collapse=", "))
   }else{
      in_themes.msg.rising<-"No terms in the conference theme appear as above-threshold Rising Terms"
   }
   print(in_themes.msg.rising)     
}

##
##Plot the interesting results (above-thresh risers & fallers)
##
#prep palette for rising/falling
rf.ratio.int.min<-min(as.integer(fall.ratio))
rf.ratio.int.max<-max(as.integer(rise.ratio))
cols<-diverge_hcl(rf.ratio.int.max-rf.ratio.int.min, c = 200, l = c(40, 120), power = 1)
# rising
par(mar=mar.bigmar)
barplot(rising.selected, las=2, cex.names=0.7, col=cols[as.integer(rising.selected)-rf.ratio.int.min-1],
        main="Rising Terms", ylab="% Rise in Target Set", names.arg=rising.selected.words)
png("Images/RisingTerms_PC.png", width=1000, height=1000,pointsize=12, res=150)
barplot(rising.selected, las=2, cex.names=0.7, col=cols[as.integer(rising.selected)-rf.ratio.int.min-1],
        main="Rising Terms", ylab="% Rise in Target Set", names.arg=rising.selected.words)
ad<-dev.off()
# Plot the frequency of occurrence of the rising terms in the past and recent sets as a stacked bar chart
r.term.sums.recent<-col_sums(dtm.tf[!past.doc_ids.bool,rise.term_ids.bool][,rise.ratio>=rise.pc.thresh])
r.term.sums.past<-col_sums(dtm.tf[past.doc_ids.bool,rise.term_ids.bool][,rise.ratio>=rise.pc.thresh])
barplot(rbind(r.term.sums.past,r.term.sums.recent),las=2, cex.names=0.7,
        names.arg=rising.selected.words, main="Un-scaled Rising Terms",
        legend.text=c("Previous","Target"), ylab="Term Occurrence")
png("Images/RisingTerms_PastRecent_Counts.png", width=1000, height=1000,pointsize=12, res=150)
barplot(rbind(r.term.sums.past,r.term.sums.recent),las=2, cex.names=0.7,
        names.arg=rising.selected.words, main="Un-scaled Rising Terms",
        legend.text=c("Previous","Target"), ylab="Term Occurrence")
ad<-dev.off()
barplot(100*rbind(r.term.sums.past/tsp.all,r.term.sums.recent/tsr.all),las=2, cex.names=0.7,
        names.arg=rising.selected.words, main="Re-scaled Rising Terms",
        legend.text=c("Previous","Target"), beside=TRUE, ylab="Term Frequency (%)")
png("Images/RisingTerms_PastRecent_PC.png", width=1000, height=1000,pointsize=12, res=150)
barplot(100*rbind(r.term.sums.past/tsp.all,r.term.sums.recent/tsr.all),las=2, cex.names=0.7,
        names.arg=rising.selected.words, main="Re-scaled Rising Terms",
        legend.text=c("Previous","Target"), beside=TRUE, ylab="Term Frequency (%)")
ad<-dev.off()
# falling
barplot(falling.selected, las=2, cex.names=0.7, col=cols[as.integer(falling.selected)-rf.ratio.int.min+1],
        main="Falling Terms", ylab="% Fall in Target Set", names.arg=falling.selected.words)
png("Images/FallingTerms_PC.png", width=1000, height=1000,pointsize=12, res=150)
barplot(falling.selected, las=2, cex.names=0.7, col=cols[as.integer(falling.selected)-rf.ratio.int.min+1],
        main="Falling Terms", ylab="% Fall in Target Set", names.arg=falling.selected.words)
ad<-dev.off()
f.term.sums.recent<-col_sums(dtm.tf[!past.doc_ids.bool,fall.term_ids.bool][,fall.ratio<=fall.pc.thresh])
f.term.sums.past<-col_sums(dtm.tf[past.doc_ids.bool,fall.term_ids.bool][,fall.ratio<=fall.pc.thresh])
barplot(rbind(f.term.sums.past,f.term.sums.recent),las=2, cex.names=0.7,
        names.arg=falling.selected.words, main="Un-scaled Falling Terms",
        legend.text=c("Previous","Target"), ylab="Term Occurrence")
png("Images/FallingTerms_PastRecent_Counts.png", width=1000, height=1000,pointsize=12, res=150)
barplot(rbind(f.term.sums.past,f.term.sums.recent),las=2, cex.names=0.7,
        names.arg=falling.selected.words, main="Un-scaled Falling Terms",
        legend.text=c("Previous","Target"), ylab="Term Occurrence")
ad<-dev.off()
barplot(100* rbind(f.term.sums.past/tsp.all,f.term.sums.recent/tsr.all),las=2, cex.names=0.7,
        names.arg=falling.selected.words, main="Re-scaled Falling Terms",
        legend.text=c("Previous","Target"), beside=TRUE, ylab="Term Frequency (%)")
png("Images/FallingTerms_PastRecent_PC.png", width=1000, height=1000,pointsize=12, res=150)
barplot(100* rbind(f.term.sums.past/tsp.all,f.term.sums.recent/tsr.all),las=2, cex.names=0.7,
        names.arg=falling.selected.words, main="Re-scaled Falling Terms",
        legend.text=c("Previous","Target"), beside=TRUE, ylab="Term Frequency (%)")
ad<-dev.off()

## checkpoint (manual!) if the above-thrsh plots include common terms then maybe the threshold is too low
# i.e. we are in "noise"

##
## Analyse the distribution of the rising terms between "recent" documents
##
#plots of number of docs containing the above-threshold terms
cnt.docs_with_rising<-col_sums(dtm.bin.rising)
cnt.recent<-sum(!past.doc_ids.bool)
par(mar=mar.bigmar)
barplot(cnt.docs_with_rising, las=2, cex.names=0.7, col=cols[as.integer(rising.selected)-rf.ratio.int.min-1], main="Occurrence of Top Rising Terms", ylab="Number of Docs (Target)", names.arg=rising.selected.words)
png("Images/RisingTerm_DocOcurrence_Bar.png", width=1000, height=1000,pointsize=12, res=150)
barplot(cnt.docs_with_rising, las=2, cex.names=0.7, col=cols[as.integer(rising.selected)-rf.ratio.int.min-1], main="Occurrence of Top Rising Terms", ylab="Number of Docs (Target)", names.arg=rising.selected.words)
ad<-dev.off() 
#Scatter plot of the % rise vs the number of documents containing
# this isn't as good to look at as the previous colourised bar plot but is more quantitative
# plot(x=rising.selected, y=cnt.docs_with_rising, main="% Rise vs Documents Containing",
#      xlab="% Rise", ylab="Documents Containing Term",
#      ylim=c(1,max(cnt.docs_with_rising)+1), xlim=c(rise.pc.thresh,max(rising.selected)*1.2), pch=20)
# par(srt=45)
# text(x=rising.selected, y=cnt.docs_with_rising, rising.selected.words, adj=c(-0.2,0.8), offset=1, cex=0.6)
# par(srt=0)      
# png("Images/RisingTerm_DocOcurrence_Scatter.png", width=1000, height=1000,pointsize=12, res=150)
# plot(x=rising.selected, y=cnt.docs_with_rising, main="% Rise vs Documents Containing",
#      xlab="% Rise", ylab="Documents Containing Term",
#      ylim=c(1,max(cnt.docs_with_rising)+1), xlim=c(rise.pc.thresh,max(rising.selected)*1.2), pch=20)
# par(srt=45)
# text(x=rising.selected, y=cnt.docs_with_rising, rising.selected.words, adj=c(-0.2,0.8), offset=1, cex=0.6)
# par(srt=0)
# ad<-dev.off()
# try a heatmap to visualise term-document correlations
par(mar=mar.bigmar)
heatmap(as.matrix(dtm.tf.rising), main="Rising Terms Among Documents", labCol=rising.selected.words, margins=c(10,5))
png("Images/RisingTerm_DocOcurrence_Heat.png", width=1000, height=1000,pointsize=12, res=150)
heatmap(as.matrix(dtm.tf.rising), main="Rising Terms Among Documents", labCol=rising.selected.words, margins=c(10,5))
ad<-dev.off()
         
##
## gephi output of term associations for above-threshold rising terms
##
#node=term, weighting=rising ratio
#edge=co-occurrence in a documet, weighting = number of docs in which the terms co-occur (not weighted by freq!)
nodes1.df<-data.frame(Id=names(rising.selected),Label=rising.selected.words,Weight=as.numeric(rising.selected))
edges1.df<-data.frame()
#use the binary occurrence doc-term-matrix in plain matrix form for easy calcs
mat.bin<-as.matrix(dtm.bin.rising)
#loop over terms, this loop is one end of each edge
for(t in 2:length(rising.selected)){
   edge.weights<-colSums(mat.bin[,t]*mat.bin)
   edges1.df<-rbind(edges1.df,data.frame(
              Source=names(rising.selected)[t],
              Target=names(rising.selected)[1:(t-1)],#omits the self-referential edge and avoids double counting edges (A-B and B-A)
              Type="Undirected",Weight=edge.weights[1:(t-1)]))
}
#find the min/max co-occurrences for reporting
min.cooccurrence<-min(edges1.df["Weight"])
max.cooccurrence<-max(edges1.df["Weight"])
mean.cooccurrence<-mean(edges1.df["Weight"])
max.edges.df<-edges1.df[which(edges1.df["Weight"] == max(edges1.df["Weight"])),]
print("Rising Term Co-occurrence Stats")
print(summary(edges1.df["Weight"]))
#remove cases where there is no co-occurrence (oddly this seems quite rare)
edges1.df<-subset(edges1.df,Weight>0)
#save to disk
write.csv(nodes1.df, file="Gephi/RisingTerm-Co-occurence Nodes.csv")
write.csv(edges1.df, file="Gephi/RisingTerm-Co-occurence Edges.csv")
## **** notes on importing into Gephi (v0.8 alpha used)
# import nodes then edges from CSV files. Make Node Weight be a float and de-select the "*" columns
# show node labels, use "statistics" to calculate an unweighted degree
# Use "ranking" to set node and edge size/colour
# - node size = imported weight (=%rise). Scale the size proportional to the sqrt(% rise)
# - node label size = same treatment as node size
# - node colour = degree. Set the colour range from #FF5000 to #00D620
# - edge size = imported weight (=number of co-occurrences). Scale proportional to the weight
# NB the actual scale may need a multiplier/factor to be applied.
# Use a circular auto-layout, with "no overlap" and with nodes ordered by degree
# - may need to do a label adjust too.
# - generally apply one step of expansion x1.2
     
     
##       
## find docs containing the above-threshold new terms, sorted most-numerous first
##
# improve this - find URL for each 
new.selected.term.ids.asc<-order(term.sums.new.sel, decreasing=TRUE) ###################
dtm.bin.new<-dtm.bin.new[,names(term.sums.new.sel[new.selected.term.ids.asc])]
print("Documents for New Terms (most numerous term first) - see RF_Terms.log")
# re-jig the sink to only print this stuff to file
sink()
sink(file="RF_Terms.log", append=TRUE, type="output", split=FALSE)
#data frames are used to store this stuff in new.doclist for simple use using Brew for report creation
new.doclist<-NULL
ii<-1
for (i in Terms(dtm.bin.new)){
   print(paste("Documents containing term:",i)) #########################
   corp.for.term<-corp.new[Docs(dtm.bin.new[row_sums(dtm.bin.new[,i])>0,])]
   empty.field.c<-rep(NA,length(corp.for.term))
   df.for.term<-data.frame(origin=empty.field.c, date=empty.field.c, heading=empty.field.c,authors=empty.field.c,id=empty.field.c,abstract=empty.field.c, stringsAsFactors=FALSE)
   jj<-1
   for (j in corp.for.term){
      df.for.term[jj,]<-c(conference.name[as.integer(meta(j, tag="Origin"))], as.character(meta(j, tag="DateTimeStamp")), as.character(meta(j, tag="Heading")),  as.character(meta(j, tag="Author")), as.character(meta(j, tag="ID")), as.character(j))
        print("")
        print(df.for.term[jj,"heading"])
        print(paste(df.for.term[jj,"origin"],df.for.term[jj,"date"],", ",df.for.term[jj,"authors"],", ", "ID=", df.for.term[jj,"id"], sep=""))
        print(df.for.term[jj,"abstract"])
      jj<-jj+1
   }
   new.doclist[[ii]]<-df.for.term
   ii<-ii+1
   print("============")
}
names(new.doclist)<-new.sel.words[new.selected.term.ids.asc]
# re-jig back to showing on console and logging
sink()
sink(file="RF_Terms.log", append=TRUE, type="output", split=TRUE)

##       
## find docs containing the above-threshold nearly-new terms, sorted most-numerous first
##
# improve this - find URL for each 
n_new.selected.term.ids.asc<-order(term.sums.n_new.sel, decreasing=TRUE)
dtm.bin.n_new<-dtm.bin.n_new[,names(term.sums.n_new[term.sums.n_new>=(new.thresh+n_new.thresh)][n_new.selected.term.ids.asc])]
print("Documents for Nearly New Terms (most numerous term first) - see RF_Terms.log")
# re-jig the sink to only print this stuff to file
sink()
sink(file="RF_Terms.log", append=TRUE, type="output", split=FALSE)
#data frames are used to store this stuff in new.doclist for simple use using Brew for report creation
n_new.doclist<-NULL
ii<-1
for (i in Terms(dtm.bin.n_new)){
   print(paste("Documents containing term:",i))
   corp.for.term<-corp.n_new[Docs(dtm.bin.n_new[row_sums(dtm.bin.n_new[,i])>0,])]
   empty.field.c<-rep(NA,length(corp.for.term))
   df.for.term<-data.frame(origin=empty.field.c, date=empty.field.c, heading=empty.field.c,authors=empty.field.c,id=empty.field.c,abstract=empty.field.c, stringsAsFactors=FALSE)
   jj<-1
   for (j in corp.for.term){
      df.for.term[jj,]<-c(conference.name[as.integer(meta(j, tag="Origin"))] , as.character(meta(j, tag="DateTimeStamp")), as.character(meta(j, tag="Heading")),  as.character(meta(j, tag="Author")), as.character(meta(j, tag="ID")), as.character(j))
        print("")
        print(df.for.term[jj,"heading"])
        print(paste(df.for.term[jj,"origin"],df.for.term[jj,"date"],", ",df.for.term[jj,"authors"],", ", "ID=", df.for.term[jj,"id"], sep=""))
        print(df.for.term[jj,"abstract"])
      jj<-jj+1
   }
   n_new.doclist[[ii]]<-df.for.term
   ii<-ii+1
   print("============")
}
names(n_new.doclist)<-n_new.sel.words[n_new.selected.term.ids.asc]
# re-jig back to showing on console and logging
sink()
sink(file="RF_Terms.log", append=TRUE, type="output", split=TRUE)
        
##       
## find docs containing the above-threshold rising terms, sorted most-rising first
##
# improve this - find URL for each 
rising.selected.term.ids.asc<-order(rise.ratio[rise.ratio>=rise.pc.thresh], decreasing=TRUE)
dtm.bin.rising<-dtm.bin.rising[,names(rise.ratio[rise.ratio>=rise.pc.thresh][rising.selected.term.ids.asc])]
print("Statistics for the number of different above-threshold rising terms in each Target doc with at least one such term")
print(dtm.bin.rising)
summary(cnt.docs_with_rising)
print("Documents for Rising Terms (most rising term first) - see RF_Terms.log")
# re-jig the sink to only print this stuff to file
sink()
sink(file="RF_Terms.log", append=TRUE, type="output", split=FALSE)
#data frames are used to store this stuff in new.doclist for simple use using Brew for report creation
rising.doclist<-NULL
ii<-1
for (i in Terms(dtm.bin.rising)){
   print(paste("Documents containing term:",names(rise.ratio[rise.ratio>=rise.pc.thresh][i])))
   corp.for.term<-corp.rising[Docs(dtm.bin.rising[row_sums(dtm.bin.rising[,i])>0,])]
   empty.field.c<-rep(NA,length(corp.for.term))
   df.for.term<-data.frame(origin=empty.field.c, date=empty.field.c, heading=empty.field.c,authors=empty.field.c,id=empty.field.c,abstract=empty.field.c, stringsAsFactors=FALSE)
   jj<-1
   for (j in corp.for.term){
      df.for.term[jj,]<-c(conference.name[as.integer(meta(j, tag="Origin"))] , as.character(meta(j, tag="DateTimeStamp")), as.character(meta(j, tag="Heading")),  as.character(meta(j, tag="Author")), as.character(meta(j, tag="ID")), as.character(j))
        print("")
        print(df.for.term[jj,"heading"])
        print(paste(df.for.term[jj,"origin"],df.for.term[jj,"date"],", ",df.for.term[jj,"authors"],", ", "ID=", df.for.term[jj,"id"], sep=""))
        print(df.for.term[jj,"abstract"])
      jj<-jj+1
   }
   rising.doclist[[ii]]<-df.for.term
   ii<-ii+1
   print("============")
}
names(rising.doclist)<-rising.selected.words[rising.selected.term.ids.asc]
  
# re-jig back to showing on console and logging
sink()
sink(file="RF_Terms.log", append=TRUE, type="output", split=TRUE)
        
##
## find docs containing several different above-threshold rising terms. 
##
dtm.bin.key_docs<-dtm.bin.rising[row_sums(dtm.bin.rising)>quantile(cnt.docs_with_rising,probs=0.75),]
print("How many documents are above the thrid quartile for (different) rising term count")
print(dtm.bin.key_docs)
#Inspect these "Key docs"
key_doc.ids<-Docs(dtm.bin.key_docs)
key_doc.ids<-key_doc.ids[order(row_sums(dtm.bin.key_docs),decreasing=TRUE)]#re-order to put the doc with the most diff rising terms first
corp.key_docs<-corp[key_doc.ids]
cat("\n")
print("Key Documents - Contain Several Rising Terms - see RF_Terms.log")
sink()
sink(file="RF_Terms.log", append=TRUE, type="output", split=FALSE)
#give metadata, abstract and list the above-thresh rising terms in each one.
i<-0
for (j in corp.key_docs){
   doc.date<- as.character(meta(j, tag="DateTimeStamp"))
   doc.heading <- as.character(meta(j, tag="Heading"))
   doc.authors <- as.character(meta(j, tag="Author"))
   doc.id <- as.character(meta(j, tag="ID"))
   doc.abstract <- as.character(j)
   print("")
   print(doc.heading)
   print(paste(doc.date,", ",doc.authors,", ", "ID=", doc.id, sep=""))
   print(doc.abstract)
   i<-i+1
   print(paste("Contains Terms:", paste( Terms(dtm.bin.rising[key_doc.ids[i],col_sums(dtm.bin.rising[key_doc.ids[i],])>0]),collapse=", ") ,sep=" "))
}   
        
#stop logging
timestamp(stamp=date(),prefix="##TIMESTAMP: ")
sink()
   
## -----
## save the whole workspace
## -----
save.image(file="RF_Terms.RData")
