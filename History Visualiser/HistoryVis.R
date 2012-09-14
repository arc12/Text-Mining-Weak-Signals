## ***Made available using the The MIT License (MIT)***
# Copyright (c) 2012, Adam Cooper
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
library("RSQLite")
#library("rjson")

## SET UP DATABASE
if(use.sqlite){
   # instantiate the SQLite driver in the R process
   sqlite<- dbDriver("SQLite")
   # open sqlite connection. db is a "connection"
   db<- dbConnect(sqlite, dbname=paste(source.dir,sqlite.filename,sep="/"))
   summary(db)
}

## CONVENIENT TO USE FUNCTION FOR BREWING
doBrew<-function(page.name, isGroup=FALSE){
   #Create the HTML/JS for the Google Chart using a Brew Template
   html.filename<-paste(page.name,".html",sep="")   
   web.page.url<-paste(web.page.base,html.filename,sep="/")
   gadget.filename<-paste(page.name,"gadget.xml",sep=" ")
   gadget.url.encoded<-URLencode(paste(web.page.base,gadget.filename,sep="/"), reserved=TRUE)
   
   isGadget=FALSE   
   brew(file=paste(brew.dir,"HV Brew Template.html",sep="/"),
        output=html.filename,run=TRUE)
   isGadget=TRUE
   brew(file=paste(brew.dir,"HV Brew Template.html",sep="/"),
        output=gadget.filename,run=TRUE)
}

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
## Read in the abstracts. NB this code allows for vectors of csv file names or SQLite (the latter is now preferred)
## REFACTOR the main code when CSV input is no longer supported
##
table<-NULL
if(use.sqlite){
   table<-dbGetQuery(db,sql)#query, fetch all records to dataframe and clear resultset in one go
}else{ #read csv [deprecated]
   for (src in 1:length(sets.csv)){
      # read in CSV with format year,pages,title,authors,abstract,keywords. There is a header row. title/authors/keywords are delimited by "
      tmp_table<-read.csv(paste(source.dir,sets.csv[[src]],sep="/"),header=TRUE,sep=",",quote="\"",stringsAsFactors=FALSE)
      #accumulate the table            
      #(aside) rbinding tables is faster and better than merging corpora using tm_combine, which leads to problematical duplicate document ids.
      table<-rbind(table,tmp_table)
      tmp_table<-NULL
   }
}

# now read in the possibly-cumulated table to a corpus, handling the metadata via mapping
#create a mapping from datatable column names to PlainTextDocument attribute names
#"Keywords" and after are user-defined "localmetadata" properties while the rest are standard tm package document metadata fields
if(brew.type=="c"){
   table[,"year"]<-ISOdate(table[,"year"],7,1)
   map<-list(Content="abstract", Heading="title", Author="authors", DateTimeStamp="year", Origin="origin", Keywords="keywords", URL="url", DBLP_URL="dblp_url", Positive="pos_score", Negative="neg_score", Subjectivity="subj_score")
}else{
   map<-list(Content="content", Heading="title", Author="authors", DateTimeStamp="datestamp", Origin="origin",URL="url", Positive="pos_score", Negative="neg_score", Subjectivity="subj_score")  
}
#use the mapping while reading the dataframe source to create a coprus with metadata
corp<-Corpus(DataframeSource(table), readerControl=list(reader= readTabular(mapping=map)))
# Standard document-term matrix of the entire corpus, use the standard stopword set with a few modifications!
stop.words<-CustomStopwords()
# corp<-tm_map(corp,removeNumbers)
# corp<-tm_map(corp,removePunctuation)
dtm.tf<-DocumentTermMatrix(corp, control=list(stemming=TRUE, stopwords=stop.words, minWordLength=3, removePunctuation=TRUE, removeNumbers=TRUE))
dtm.bin<-weightBin(dtm.tf)
# pull out the sentiment data
pos_score<-unlist(meta(corp,tag="Positive", type="local"))
if(is.null(pos_score)){
   stop("Source data should be pre-processed to add sentiment/subjectivity - usee Pre-process.R")
}
neg_score<-unlist(meta(corp,tag="Negative", type="local"))
subj_score<-unlist(meta(corp,tag="Subjectivity", type="local"))

#compute some corpus and term statistics FOR INFORMATION
print("Computed Document Term Matrix, Term-Frequency")
print(dtm.tf)
dtm.tf.sums<-col_sums(dtm.tf)
cat("\n")
print("Summary Stats of (Total) Term Occurrences in the Corpus")
print(summary(dtm.tf.sums))

# a bunch of matrices to accumulate the grouped results while the following nest of 2 loops operates
gmat.template<-matrix(ncol=length(term.lists),nrow=num.slices)
gmat.or.slices.freq<-gmat.template
gmat.or.slices.docs<-gmat.template
gmat.or.slices.positive<-gmat.template
gmat.or.slices.negative<-gmat.template
gmat.or.slices.subjectivity<-gmat.template

# to keep the number of docs per slice (shown in HTML output)
slice.docs.cnt<-rep(0,num.slices)

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
   # make sure the specified terms are actually present in the DTM (otherwise get an error)
   ok.terms.bool<-run.terms %in% Terms(dtm.tf)
   if(sum(!ok.terms.bool)>0){
      print("The following are not present and will not be plotted:")
      print(paste(run.terms[!ok.terms.bool], sep=", "))
      run.terms<-run.terms[ok.terms.bool]
      run.words<-run.words[ok.terms.bool]
   }
   #prep dataframes to receive the slice data
   data.slices.freq<-data.frame()
   data.slices.docs<-data.frame()
   data.slices.positive<-data.frame()
   data.slices.negative<-data.frame()
   data.slices.subjectivity<-data.frame()
   docs.used<-0
   print(paste("Run:",run.name, " Terms:", paste(run.terms, collapse=", ")))
   ## REFACTOR this code when CSV input is no longer supported;
   ## direct DB query much neater!!!!!!!!!!!!!!!!!!!
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
      dts<-as.POSIXlt(unlist(meta(corp,"DateTimeStamp",type="local")), origin="1970-01-01")
      slice.doc_bool<- (dts>q.start.date) & (dts<=q.end.date)
      slice.docs.n<-sum(slice.doc_bool)
      #this is lazy, should really do imputation
      if(slice.docs.n == 0){
         stop(paste("STOPPING: No documents found in period",start.date,"<= T <",end.date))
      }
      #store for Brew. This is repeated (pointlessly) for each i.run
      slice.docs.cnt[slice]<-slice.docs.n
      print(paste(slice.docs.n,"documents in period"))
      docs.used<-docs.used+slice.docs.n
      #select only these documents in the DTMs
      dtm.tf.slice<-dtm.tf[slice.doc_bool,]
      dtm.bin.slice<-dtm.bin[slice.doc_bool,]
      #get the total term and counts for the slice
      total.terms<-sum(row_sums(dtm.tf.slice))     
      total.docs<-sum(slice.doc_bool)
      #calculate term frequency (%) and document count ** for the terms aposite to the current run
      #NB: if run.terms contains a term which is not in the DTM, this gives a "subscript out of bounds" error - these should be the stemmed forms (previously checked).
      dtm.tf.slice<-dtm.tf.slice[,run.terms]
      #all.term.docs<-row_sums(dtm.bin.slice)==length(run.terms) #which docs contain all terms
      dtm.tf.slice<-dtm.tf.slice[col_sums(dtm.tf.slice)>0,]
      slice.freq<-100*col_sums(dtm.tf.slice)/total.terms
      data.slices.freq<-rbind(data.slices.freq,slice.freq)
      gmat.or.slices.freq[slice,i.run]<-sum(slice.freq)

      print("Frequences (%):")
      print(slice.freq)
      dtm.bin.slice<-dtm.bin.slice[,run.terms]
      dtm.bin.slice<-dtm.bin.slice[col_sums(dtm.bin.slice)>0,]
      slice.docs<-col_sums(dtm.bin.slice)#*100/total.docs - used to be %
      data.slices.docs<-rbind(data.slices.docs,slice.docs)
      gmat.or.slices.docs[slice,i.run]<-sum(slice.docs)

      print("Document count:")
      print(slice.docs)
      #calculate the "subjectivity" for each term according to the subjectivity of containing documents
      #and simultaneiously fill out rows of the data to be visualised
      sent.positive = vector()
      sent.negative = vector()
      subjectivity = vector()
      for (nt in Terms(dtm.tf.slice)){
         sent.positive[nt]<-mean(pos_score[Docs(dtm.tf.slice[as.matrix(dtm.tf.slice[,nt])>0,nt])])
         sent.negative[nt]<-mean(neg_score[Docs(dtm.tf.slice[as.matrix(dtm.tf.slice[,nt])>0,nt])])
         subjectivity[nt]<-mean(subj_score[Docs(dtm.tf.slice[as.matrix(dtm.tf.slice[,nt])>0,nt])])
      }
      sent.positive[is.nan(sent.positive)]<-0.0
      sent.negative[is.nan(sent.negative)]<-0.0
      subjectivity[is.nan(subjectivity)]<-0.0
      data.slices.positive<-rbind(data.slices.positive,sent.positive)
      data.slices.negative<-rbind(data.slices.negative,sent.negative)
      data.slices.subjectivity<-rbind(data.slices.subjectivity,subjectivity)
      #the groups require special treatment
      or.docs<-Docs(dtm.tf.slice)[row_sums(dtm.tf.slice)>0]
      gmat.or.slices.positive[slice,i.run]<-mean(pos_score[or.docs])
      gmat.or.slices.negative[slice,i.run]<-mean(neg_score[or.docs])
      gmat.or.slices.subjectivity[slice,i.run]<-mean(subj_score[or.docs])
      gmat.or.slices.positive[is.nan(gmat.or.slices.positive)]<-0.0
      gmat.or.slices.negative[is.nan(gmat.or.slices.negative)]<-0.0
      gmat.or.slices.subjectivity[is.nan(gmat.or.slices.subjectivity)]<-0.0
      #       gmat.and.slices.positive[slice,i.run]<-mean(pos_score[and.docs])
      #       gmat.and.slices.negative[slice,i.run]<-mean(neg_score[and.docs])
      #       gmat.and.slices.subjectivity[slice,i.run]<-mean(subj_score[and.docs])
      print("Subjectivity:")
      print(subjectivity)
   }
   # assemble the data into a convenient array for processing in a Brew template, interpolating if necessary
   #row.count<-num.interpolate*length(run.terms)
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
      # cancel out negative values, make 0.0 be a "hard floor"
      data.slices.freq[data.slices.freq<0.0]<-0.0
      data.slices.docs[data.slices.docs<0.0]<-0.0
      data.slices.positive[data.slices.positive<0.0]<-0.0
      data.slices.negative[data.slices.negative<0.0]<-0.0
      data.slices.subjectivity[data.slices.subjectivity<0.0]<-0.0
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
   doBrew(run.name)
}



##
## Groups special treatment
##
if(length(do.groups)>0){
   #pick out only those grouped stats that have been selected
   gmat.or.slices.freq<-data.frame(gmat.or.slices.freq[,names(term.lists)%in%do.groups])
   gmat.or.slices.docs<-data.frame(gmat.or.slices.docs[,names(term.lists)%in%do.groups])
   gmat.or.slices.positive<-data.frame(gmat.or.slices.positive[,names(term.lists)%in%do.groups])
   gmat.or.slices.negative<-data.frame(gmat.or.slices.negative[,names(term.lists)%in%do.groups])
   gmat.or.slices.subjectivity<-data.frame(gmat.or.slices.subjectivity[,names(term.lists)%in%do.groups])
   
   #interpolate 
   if(slice.size > interpolate.size){
      gmat.or.slices.freq<-sapply(gmat.or.slices.freq,
                                  function(x) spline(x, method="natural", n=num.interpolate)$y)
      gmat.or.slices.docs<-sapply(gmat.or.slices.docs,
                                  function(x) spline(x, method="natural", n=num.interpolate)$y)
      gmat.or.slices.positive<-sapply(gmat.or.slices.positive,
                                      function(x) spline(x, method="natural", n=num.interpolate)$y)
      gmat.or.slices.negative<-sapply(gmat.or.slices.negative,
                                      function(x) spline(x, method="natural", n=num.interpolate)$y)
      gmat.or.slices.subjectivity<-sapply(gmat.or.slices.subjectivity,
                                          function(x) spline(x, method="natural", n=num.interpolate)$y)
   }
   data.rows<-data.frame()
   do.groups.pretty<-gsub("\\."," ",do.groups) #prettify
   for(g in 1:length(do.groups)){
      data.rows<-rbind(data.rows, data.frame(rep(do.groups.pretty[g],num.interpolate),
                                             as.character(interpolate.start.dates),
                                             gmat.or.slices.freq[,g],
                                             gmat.or.slices.docs[,g],
                                             gmat.or.slices.subjectivity[,g],
                                             gmat.or.slices.positive[,g],
                                             gmat.or.slices.negative[,g]))
   }
   
   run.title<-"Groups of Terms"#used in brew template... messy coding :-(
   doBrew("Groups", isGroup=TRUE)
}
#stop logging
sink()

# properly terminate database use
if(use.sqlite){
   dbDisconnect(db)
}
