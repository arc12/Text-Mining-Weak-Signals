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
source(paste(base.dir,"commonFunctions.R",sep="/"))
while(sink.number()>0)
  {sink()}
sink(file="HistoryVis.log", append=FALSE, type="output", split=TRUE)

##
## Compute the time-slice operation parameters from the "init" values
##
num.slices<-(end.month-start.month+12*(end.year-start.year))/slice.size+1
if(slice.size==interpolate.size){
   num.interpolate <- num.slices
}else{
   num.interpolate<-slice.size*(num.slices-1)/interpolate.size+1
}
init.date<-as.POSIXlt(paste(start.year,start.month,"1",sep="-"), tz = "GMT")
#slice dates define the filtering of documents.
slice.start.dates<-as.POSIXlt(seq.POSIXt(init.date, by=paste(slice.size,"months"), length.out=num.slices))
#interpolate dates define the plotting, whether or not an interpolation has actually occurred
# hence they are offset to the centre of slices as well as (usually) interspersing the slice periods
interpolate.start.dates<-as.POSIXlt(seq.POSIXt(init.date, by=paste(interpolate.size,"months"), length.out=num.interpolate))
interpolate.start.dates$mon<-interpolate.start.dates$mon+(slice.size/2)

##
## Read in the abstracts. NB this code allows for vectors of csv file names
##
#(aside) rbinding tables is faster and better than merging corpora using tm_combine, which leads to problematical duplicate document ids.
table<-NULL
for (src in 1:length(sets.csv)){
   # read in CSV with format year,pages,title,authors,abstract,keywords. There is a header row. title/authors/keywords are delimited by "
   tmp_table<-read.csv(paste(source.dir,sets.csv[[src]],sep="/"),header=TRUE,sep=",",quote="\"")
   #accumulate the table            
   table<-rbind(table,tmp_table)
   tmp_table<-NULL
}
# now read in the possibly-cumulated table to a corpus, handling the metadata via mapping
#create a mapping from datatable column names to PlainTextDocument attribute names
#"Keywords" and after are user-defined "localmetadata" properties while the rest are standard tm package document metadata fields
if(brew.type=="c"){
   map<-list(Content="abstract", Heading="title", Author="authors", DateTimeStamp="year", Origin="origin", Keywords="keywords", URL="url", DBLP_URL="dblp_url", Positive="pos.score", Negative="neg.score", Subjectivity="subj.score")
   }else{
       map<-list(Content="content", DateTimeStamp="datestamp", Positive="pos.score", Negative="neg.score", Subjectivity="subj.score")  
   }
#use the mapping while reading the dataframe source to create a coprus with metadata
corp<-Corpus(DataframeSource(table), readerControl=list(reader= readTabular(mapping=map)))
# Standard document-term matrix of the entire corpus, use the standard stopword set with a few modifications!
stop.words<-CustomStopwords()
dtm.tf<-DocumentTermMatrix(corp,
  control=list(stemming=TRUE, stopwords=stop.words, minWordLength=3, removeNumbers=TRUE, removePunctuation=TRUE))
dtm.bin<-weightBin(dtm.tf)
# pull out the sentiment data
pos.score<-unlist(meta(corp,tag="Positive", type="local"))
if(is.null(pos.score)){
   stop("Source data should be pre-processed to add sentiment/subjectivity - usee Pre-process.R")
}
neg.score<-unlist(meta(corp,tag="Negative", type="local"))
subj.score<-unlist(meta(corp,tag="Subjectivity", type="local"))

#compute some corpus and term statistics FOR INFORMATION
print("Computed Document Term Matrix, Term-Frequency")
print(dtm.tf)
dtm.tf.sums<-col_sums(dtm.tf)
cat("\n")
print("Summary Stats of (Total) Term Occurrences in the Corpus")
print(summary(dtm.tf.sums))

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
#       slice.doc_bool<-tm_index(corp,FUN=sFilter, doclevel = TRUE, useMeta = FALSE,
#             paste("datetimestamp<='",q.end.date,"' & datetimestamp>'",q.start.date,"'",sep=""))
      #changed from using tm_index because it seemed not to work with month-level periods
      dts<-as.POSIXlt(unlist(meta(corp,"DateTimeStamp",type="local")))
      slice.doc_bool<- (dts>q.start.date) & (dts<=q.end.date)
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
      sent.positive[is.nan(subjectivity)]<-0.0
      sent.negative[is.nan(subjectivity)]<-0.0
      subjectivity[is.nan(subjectivity)]<-0.0
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
   html.filename<-paste(run.name,".html",sep="")   
   web.page.url<-paste(web.page.base,html.filename,sep="/")
   gadget.filename<-paste(run.name,"gadget.xml",sep=" ")
   gadget.url.encoded<-URLencode(paste(web.page.base,gadget.filename,sep="/"), reserved=TRUE)
   
   isGadget=FALSE   
   brew(file=paste(brew.dir,"HV Brew Template.html",sep="/"),
     output=html.filename,run=TRUE)
   isGadget=TRUE
   brew(file=paste(brew.dir,"HV Brew Template.html",sep="/"),
     output=gadget.filename,run=TRUE)
}

#stop logging
sink()
   

