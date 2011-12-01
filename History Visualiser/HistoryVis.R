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
## This code builds a Google Charts "bubble chart" to track (stemmed) terms over several years
## shows: term freq, document freq, positive and negative sentiment, "subjectivity" (pos or neg sentiment)
## ----------------------------------------
library("tm")
library("Snowball")
library("slam")
library("brew")
#library("rjson")

##
## NB {data_set_name}/HV_Init.R should be run first to establish the run parameters
##

while(sink.number()>0)
  {sink()}
sink(file="HistoryVis.log", append=FALSE, type="output", split=TRUE)

##
## Prepare lexicon for sentiment analysis
##
##
# Read in the sentiment word lists (cols extracted from the Harvard Inquirer spreadsheet http://www.wjh.harvard.edu/~inquirer/)
# and process to obtain a list of dictionaries. 
#The column headings MUST be "Entry,Positive,Negative"
inquirer.table<-read.csv("/home/arc1/R Projects/Text Mining Weak Signals/InquirerPosNeg.csv",
                         header=TRUE, sep=",", quote="\"", stringsAsFactors=FALSE)
sentiment.dics<-list()
# for each sentiment, find out which words are relevant and for cases where there is more
# than one usage (denoted #1 in the word), select only the first one as this is the most frequent in general
for(i in 2:length(inquirer.table[1,])){
   dic<-inquirer.table[,"Entry"]
   dic<-dic[inquirer.table[,i]!=""]#limit to words for sentiment
   dic<-sub("#1","",dic)#remove '#1' from any words containing it
   dic<-dic[-grep("#",dic)]#remove all words still containing #
   sentiment.dics[[i-1]]<-dic
   names(sentiment.dics)[[i-1]] <- colnames(inquirer.table)[i]
}


##
## Read in the abstracts. NB this code allows for vectors of csv file names
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
map<-list(Content="abstract", Heading="title", Author="authors", DateTimeStamp="year", Origin="origin", Keywords="keywords", URL="url")
#use the mapping while reading the dataframe source to create a coprus with metadata
corp<-Corpus(DataframeSource(table), readerControl=list(reader= readTabular(mapping=map)))

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

#Here we go////
dtm.tf<-DocumentTermMatrix(corp,
  control=list(stemming=TRUE, stopwords=stop.words, minWordLength=3, removeNumbers=TRUE, removePunctuation=TRUE))
dtm.bin<-weightBin(dtm.tf)

#compute some corpus and term statistics FOR INFORMATION
print("Computed Document Term Matrix, Term-Frequency")
print(dtm.tf)
dtm.tf.sums<-col_sums(dtm.tf)
cat("\n")
print("Summary Stats of (Total) Term Occurrences in the Corpus")
print(summary(dtm.tf.sums))

##
## Sentiment Analysis, specifically "subjectivity" at a document level. Used later to calc sentiment of docs containing term 
##
# Use the Harvard Inquirer word lists to score sets of responses against several sentiments.
# Do this for the recent set of abstracts only and NB this is an UNSTEMMED treatment
# Count each occurrence of each word as a score of 1 for every word in the sentiment list.
# "subjectivity" is the sum of positive and negative scores divided by the number of terms in the doc
# -- positive scores
dtm.tf.unstemmed.p<-DocumentTermMatrix(corp,
  control=list(stemming=FALSE, stopwords=stop.words, minWordLength=3, removeNumbers=TRUE, removePunctuation=FALSE,dictionary=tolower(sentiment.dics[["Positive"]])))
pos.score<-row_sums(dtm.tf.unstemmed.p)/row_sums(dtm.bin)[Docs(dtm.tf.unstemmed.p)]
pos.terms.sums<-col_sums(dtm.tf.unstemmed.p[,col_sums(dtm.tf.unstemmed.p)>0])
# -- negative scores
dtm.tf.unstemmed.n<-DocumentTermMatrix(corp,
  control=list(stemming=FALSE, stopwords=stop.words, minWordLength=3, removeNumbers=TRUE, removePunctuation=FALSE,dictionary=tolower(sentiment.dics[["Negative"]])))
neg.score<-row_sums(dtm.tf.unstemmed.n)/row_sums(dtm.bin)[Docs(dtm.tf.unstemmed.n)]
neg.terms.sums<-col_sums(dtm.tf.unstemmed.n[,col_sums(dtm.tf.unstemmed.n)>0])
# -- subjectivity
subj.score<-pos.score + neg.score
# tidy up
rm(dtm.tf.unstemmed.p)
rm(dtm.tf.unstemmed.n)

##
## Start to get results.
## The outer loop is over the 1..* lists of terms (term.lists in the HV_Init.R file). Separate viz for each
## The inner loop is over the years
##
for (i.run in 1:length(term.lists)){
   run.title<-titles[i.run]
   run.terms<-unlist(term.lists[i.run])
   run.name<-names(term.lists[i.run])
   run.words<-unlist(word.lists[i.run])
   data.slices.freq<-data.frame()
   data.slices.docs<-data.frame()
   data.slices.positive<-data.frame()
   data.slices.negative<-data.frame()
   data.slices.subjectivity<-data.frame()
   docs.used<-0
   print(paste("Run:",run.name, " Terms:", paste(run.terms, collapse=", ")))
   for(slice in 1:num.slices){
      #date range for this row
      start.date<-slice.start.dates[slice]
      end.date<-start.date
      end.date$mon<-end.date$mon+slice.size
      print(paste("Period: ",start.date,"<= T <",end.date))
      # get the indeces of the documents that come from the date range
      #the peculiar "mday-1" is needed to trick the filter since the source data has a year number only
      q.start.date<-start.date
      q.start.date$mday<-q.start.date$mday-1
      q.end.date<-end.date
      q.end.date$mday<-q.end.date$mday-1
      slice.doc_bool<-tm_index(corp,FUN=sFilter, doclevel = TRUE, useMeta = FALSE,
            paste("datetimestamp<='",q.end.date,"' & datetimestamp>'",q.start.date,"'",sep=""))
      slice.docs.n<-sum(slice.doc_bool)
      #this is lazy, should really do imputation
      if(slice.docs.n == 0){
         stop(paste("STOPPING: No documents found in period",start.date,"<= T <",end.date))
      }
      print(paste(slice.docs.n,"documents in period"))
      docs.used<-docs.used+slice.docs.n
      #select only these documents in the DTMs
      dtm.tf.slice<-dtm.tf[slice.doc_bool,]
      dtm.bin.slice<-dtm.bin[slice.doc_bool,]
      #get the total term count for the slice
      total.terms<-sum(row_sums(dtm.tf.slice))     
      #calculate term frequency (%) and document count ** for the terms aposite to the current run
      #NB: if run.terms contains a term which is not in the DTM, this gives a "subscript out of bounds" error - these MUST be the stemmed forms.
      dtm.tf.slice<-dtm.tf.slice[,run.terms]
      dtm.tf.slice<-dtm.tf.slice[col_sums(dtm.tf.slice)>0,]
      slice.freq<-100*col_sums(dtm.tf.slice)/total.terms
      data.slices.freq<-rbind(data.slices.freq,slice.freq)
      print("Frequences (%):")
      print(slice.freq)
      dtm.bin.slice<-dtm.bin.slice[,run.terms]
      dtm.bin.slice<-dtm.bin.slice[col_sums(dtm.bin.slice)>0,]
      slice.docs<-100*col_sums(dtm.bin.slice)/length(Docs(dtm.bin.slice))
      data.slices.docs<-rbind(data.slices.docs,slice.docs)
      print("Document fraction (%):")
      print(slice.docs)
      #calculate the "subjectivity" for each term according to the subjectivity of containing documents
      #and simultaneiously fill out rows of the data to be visualised
      sent.positive = vector()
      sent.negative = vector()
      subjectivity = vector()
      for (nt in Terms(dtm.tf.slice)){
         sent.positive[nt]<-mean(pos.score[Docs(dtm.tf.slice[as.matrix(dtm.tf.slice[,nt])>0,nt])])
         sent.negative[nt]<-mean(neg.score[Docs(dtm.tf.slice[as.matrix(dtm.tf.slice[,nt])>0,nt])])
         subjectivity[nt]<-mean(subj.score[Docs(dtm.tf.slice[as.matrix(dtm.tf.slice[,nt])>0,nt])])
      }
      data.slices.positive<-rbind(data.slices.positive,sent.positive)
      data.slices.negative<-rbind(data.slices.negative,sent.negative)
      data.slices.subjectivity<-rbind(data.slices.subjectivity,subjectivity)
      print("Subjectivity:")
      print(subjectivity)
   }
   # assemble the data into a convenient array for processing in a Brew template, interpolating if necessary
   row.count<-num.interpolate*length(run.terms)
   #data.rows<-array(0,c(row.count,7))#this is destined for JSON in the Brew template
   data.rows<-data.frame()
   #interpolate 
   if(slice.size > interpolate.size){
        data.slices.freq<-sapply(data.slices.freq,
                                    function(x) spline(x, method="natural", n=num.interpolate)$y)
        data.slices.docs<-sapply(data.slices.docs,
                                    function(x) spline(x, method="natural", n=num.interpolate)$y)
        data.slices.positive<-sapply(data.slices.positive,
                                    function(x) spline(x, method="natural", n=num.interpolate)$y)
        data.slices.negative<-sapply(data.slices.negative,
                                    function(x) spline(x, method="natural", n=num.interpolate)$y)
        data.slices.subjectivity<-sapply(data.slices.subjectivity,
                                    function(x) spline(x, method="natural", n=num.interpolate)$y)
   }
   #loop over terms to build a "denormalised" form of the data for the google chart code
   for(t in 1:length(run.terms)){
      data.rows<-rbind(data.rows, data.frame(rep(run.words[t],num.interpolate),
                                             as.character(interpolate.start.dates),
                                             data.slices.freq[,t],
                                             data.slices.docs[,t],
                                             data.slices.subjectivity[,t],
                                             data.slices.positive[,t],
                                             data.slices.negative[,t]))
   }
   
   #Create the HTML/JS for the Google Chart using a Brew Template
   isGadget=FALSE
   brew(file=paste(brew.dir,"HV Brew Template.html",sep="/"),
     output=paste(run.name,".html",sep=""),run=TRUE)
# isGadget=TRUE
# brew(file=paste(brew.dir,"Brew Template - visapi.html",sep="/"),
#      output="gadget.xml",run=TRUE)
}

#stop logging
sink()
   

