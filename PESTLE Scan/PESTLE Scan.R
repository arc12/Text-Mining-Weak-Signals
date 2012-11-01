## ***Made available using the The MIT License (MIT)***
# Copyright (c) 2012, Adam Cooper
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
## ************ end licence ***************

##
## Based on pre-process.R - a quick and dirty scan for blogs/abstracts with a strong
## "sense" of PESTLE (actually PES.L.). according to scoring using the Harvard General Inquirer
##
## NB there is some munging of the Inquirer basic list to merge categories
##
## This handles conference abstracts and blog posts in slightly different ways
##       abstracts: each CSV is assumed to be a set of abstracts from a 
##                  single conference series (aka "origin"). The origin is
##                  added as part of the pre=processing
##      blogs: each CSV is assumed to contain posts from several blogs
##             and the "origin" is in the CSV as the blog "home" URL
##
## (the column headings also differ, sometimes only for "historical reasons")
##
library("tm")
library("slam")
library("brew")
library("RColorBrewer")

# load some functions, effectively "macros". Some day make a proper package for the whole show
source("/home/arc1/R Projects/Text Mining Weak Signals/commonFunctions.R")
source("/home/arc1/R Projects/Text Mining Weak Signals/sentimentFunctions.R")

home.dir<-"/home/arc1/R Projects/Text Mining Weak Signals/PESTLE Scan"
data.dir<-"/home/arc1/R Projects/Text Mining Weak Signals/Source Data"
output.dir<-"/home/arc1/R Projects/Text Mining Weak Signals Output/PESTLE Scan"


#each one of these will be looped over NB the origin.tag must be in the same order as set.csv
# set.csv <- c("ICALT Abstracts 2005-2011.csv",
#                    "CAL Abstracts 2007-2009.csv",
#                    "ECTEL Abstracts 2006-2011.csv",
#                    "ICWL Abstracts 2005-2011.csv")
# origin.tag <- c("ICALT",
#                    "CAL",
#                    "ECTEL",
#                    "ICWL")#only used for abstracts
#set.csv <- c("CETIS Blogs 20110101-20120301.csv","CETIS Blogs 20090101-20120301.csv","NonCETIS Blogs 20110101-20120301.csv")
set.csv <- c("MB/MB Blogs 20100101-20120630.csv")
# this determines the source type: conference abstracts or blog content
source.type="b"#a is for abstracts, b is for blogs

##
## Sometimes it is useful to pre-filter to use only documents containing set words
##
prefilter.name<-"-"#used to name the output file.
#at least prefilter.thresh.1 of the the prefilter.words.1 must appear for inclusion. Use 0 to skip filtering
#in addition, if prefilter.thresh.2>0 members of prefilter.words.2 MUST ALSO be present
prefilter.thresh.1<-0
prefilter.words.1<-c("open", "educational", "resource", "resources", "oer", "content")
#prefilter.words.1<-c("school","schools", "schooling","pupil","pupils","highschool","high-school","class","classroom","teacher","child","children","parent","parents","child's","parent's","teachers","teacher's","junior","infant","nursery")
prefilter.thresh.2<-0
#prefilter.words.2<-c("Europe","Ruropean","UK","Britain","Germany","France","British","German","French","Spain","Spanish","Italy","Italian","Norway","Norwegian","Swedish","Sweden","Finland","Finnish","Baltic")
prefilter.words.2<-c("assessment","assess","test","assessing","testing")
if(prefilter.thresh.1<1){
   prefilter.name<-""
   prefilter.thresh.1<-0
   prefilter.thresh.2<-0
}
#force lower case to match doc-term matrix
prefilter.words.1<-tolower(prefilter.words.1)
prefilter.words.2<-tolower(prefilter.words.2)

##
## Prepare lexicon
##
# Read in the sentiment word lists (cols extracted from the Harvard Inquirer spreadsheet http://www.wjh.harvard.edu/~inquirer/)
#The column headings MUST be unchanged (but NB data.frame colnames do not allow "@":
#       Entry   Econ@	ECON	Legal	Polit@	POLIT	Role	SocRel
targets<-list(Economic=c("Econ.","ECON"), Legal="Legal", Political=c("Polit.", "POLIT"), Doing=c("Need","Goal","Try","Means","Persist","Complet","Fail"), Knowing=c("Know","Solve"))
sentiment.dics<-prepareLexicons(paste(home.dir,"InquirerPESTLE2.csv",sep="/"), targets)

##
## MAIN LOOP over the sets: Read-in, add columns of metrics and write-out
##
for (src in 1:length(set.csv)){
   inFile<-paste(data.dir,set.csv[src],sep="/")
   print(paste("******************** Processing:",set.csv[src],"********************"))
   inFileStem<-strtrim(set.csv[src],nchar(set.csv[src])-4)
   outFile<-paste(output.dir,paste(inFileStem, prefilter.name,"PESTLE.csv",sep=" "),sep="/")
   brewFile<-paste(output.dir,paste(inFileStem, prefilter.name,"PESTLE.html",sep=" "),sep="/")
   # read in CSV with format year,pages,title,authors,abstract,keywords,url,dblp_url.
   #There is a header row. DBLP_URL is the vital key into the author centrality data
   table<-read.csv(inFile,header=TRUE,sep=",",quote="\"",stringsAsFactors=FALSE)
   # choose an appropriate mapping and other source-specific preliminaries
   #"Keywords" and after are user-defined "localmetadata" properties while the rest are standard tm package document metadata fields
   if(source.type == "a"){
      #insert the "origin" as a new column
      origin<-rep(origin.tag[src], length(table[,1]))
      table<-cbind(origin,table)
      map<-list(Content="abstract", Heading="title", Author="authors", DateTimeStamp="year", Origin="origin", Keywords="keywords", URL="url", DBLP_URL="dblp_url")
   }else if(source.type == "b"){
      map<-list(Content="content", Heading="title", Author="authors", DateTimeStamp="datestamp", Origin="origin",URL="url")
   }else{
      stop("Unknown source type:",source.type)
   }
   n.docs<-length(table[,1])
   print(paste(n.docs,"documents read in"))
   # create a corpus, handling the metadata via mapping from datatable column names to PlainTextDocument attribute names
   corp<-Corpus(DataframeSource(table), readerControl=list(reader= readTabular(mapping=map)))
   ##
   ## Lexical Analysis at a document level.
   ##
   # Use the Harvard Inquirer word lists to score sets of responses against several sentiments.
   # NB this is an UNSTEMMED treatment
   # the baseline is all terms used
   stop.words<-c(CustomStopwords(),"social")
   dtm.tf.unstemmed.all<-DocumentTermMatrix(corp,
                                            control=list(stemming=FALSE, stopwords=stop.words, minWordLength=3, removeNumbers=TRUE, removePunctuation=FALSE))
   #do pre-filtering if required
   if(prefilter.thresh.1>0){
      print(paste("Filtering to only use documents containing at least",prefilter.thresh.1,
                  " of the following words:", paste(prefilter.words.1, collapse=", ")))
      #ensure the list only contains words that are present (otherwise get an "out of bounds" error later)
      prefilter.words.1<-prefilter.words.1[prefilter.words.1 %in% Terms(dtm.tf.unstemmed.all)]
      dtm.bin.pfw1<-weightBin(dtm.tf.unstemmed.all[,prefilter.words.1])
      if(prefilter.thresh.2>0){
         print(paste("as well as at least",prefilter.thresh.2,"of these words:",
                                          paste(prefilter.words.2,collapse=", ")))
         prefilter.words.2<-prefilter.words.2[prefilter.words.2 %in% Terms(dtm.tf.unstemmed.all)]
         dtm.bin.pfw2<-weightBin(dtm.tf.unstemmed.all[,prefilter.words.2])
         filter.docs<-(row_sums(dtm.bin.pfw1)>=prefilter.thresh.1) &
                           (row_sums(dtm.bin.pfw2)>=prefilter.thresh.2)
      }else{
         filter.docs<-row_sums(dtm.bin.pfw1)>=prefilter.thresh.1
      }
      dtm.tf.unstemmed.all<-dtm.tf.unstemmed.all[filter.docs,]
      table<-table[filter.docs,] #required for output csv
      n.docs.f<-sum(filter.docs)
      print(paste(n.docs.f,"documents fulfil criteria"))
   }
   doc.term.sums<-row_sums(dtm.tf.unstemmed.all)
   
   #Create a separate report for each input data file.
   #This is done in 3 parts: header, a repeated central chunk per lexicon, a footer
   brew.conn<-file(brewFile, open="wt")
   brew(file=paste(home.dir,"BrewHeader.html",sep="/"), output=brew.conn)
   #this palette is used in brew for color-coding scores
   score.pal<-brewer.pal(11,"RdBu")[11:1]#reverse it to make red highest scoring
   
   
   
   #Loop over the "sentiment dics", each column is a potentially-merged Gen Inquirer category
   for(lex in 1:length(sentiment.dics)){
      dtm.tf.unstemmed.lex<-DocumentTermMatrix(corp,
                                               control=list(stemming=FALSE, stopwords=stop.words, minWordLength=3, removeNumbers=TRUE, removePunctuation=FALSE,dictionary=tolower(sentiment.dics[[lex]])))
      #do pre-filtering if required
      if(prefilter.thresh.1>0){
         dtm.tf.unstemmed.lex<-dtm.tf.unstemmed.lex[filter.docs,]
      }
      lex.score<-row_sums(dtm.tf.unstemmed.lex)/doc.term.sums
      #force any v. short docs to have scores = 0.0
      lex.score[doc.term.sums<40]<-0.0
      lex.score<-as.data.frame(lex.score)
      colnames(lex.score)<-names(sentiment.dics)[[lex]]
      # add to the data.table
      table<-cbind(table,lex.score)
      print(paste("====","Processed lexicon for", names(sentiment.dics)[[lex]],"===="))
      print("Summary stats and histograms AFTER removing score=0 documents")
      lex.score.nz<-lex.score[lex.score[,1]>0.0,1]
      summary.nz<- summary(lex.score.nz)
      print(summary.nz)
      print(paste("Summary stats after scaling by 1000/lexicon length (length=",length(sentiment.dics[[lex]]),")"))
      print(summary(lex.score.nz*1000/length(sentiment.dics[[lex]])))
      hist(lex.score.nz, main=names(sentiment.dics)[[lex]], breaks=20)
      print("Report Top 20 using Brew")
      topDocs<-order(lex.score, decreasing=T)[1:20]
      #write out the highest scoring documents to the formatted report
      brew(file=paste(home.dir,"BrewChunk.html",sep="/"), output=brew.conn)
   }
   # tidy up
#    rm(dtm.tf.unstemmed.all)
#    rm(dtm.tf.unstemmed.lex)
#    rm(lex.score)
   #write out the full PESTLE-scored data and complete the formatted report
   write.csv(table, outFile, quote=TRUE, row.names=FALSE)
   brew(file=paste(home.dir,"BrewFooter.html",sep="/"), output=brew.conn)
   close(brew.conn)
   
}
