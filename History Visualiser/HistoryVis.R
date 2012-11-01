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

##
## NB {data_set_name}/HV_Init.R should be run first to establish the run parameters
##
#source(paste(base.dir,"commonFunctions.R",sep="/"))

# Filepaths
base.dir<-"/home/arc1/R Projects/Text Mining Weak Signals"
source.dir<-paste(base.dir,"Source Data",sep="/")
output.dir<-paste("/home/arc1/R Projects/Text Mining Weak Signals Output/History Visualiser",set.name,sep="/")
brew.dir<-paste(base.dir,"History Visualiser",sep="/")
web.page.base<-paste("http://arc12.github.com/Text-Mining-Weak-Signals-Output/History Visualiser",set.name, sep="/")
dir.create(output.dir, showWarnings=FALSE)
setwd(output.dir)
if(source.type=="a"){
   file.postfix<-"conf"
}else{
   file.postfix<-"blog"
}

## SET UP DATABASE
sqlite.filename<-"TMWS Data A.sqlite"
use.sqlite<-!is.na(sqlite.filename)
if(use.sqlite){
   # instantiate the SQLite driver in the R process
   sqlite<- dbDriver("SQLite")
   # open sqlite connection. db is a "connection"
   db<- dbConnect(sqlite, dbname=paste(source.dir,sqlite.filename,sep="/"))
   #summary(db)
}

##
## Choose some run parameters depending on the source type
##    start and end dates are largely plucked from database limits
##
today<-as.POSIXlt(Sys.Date(), tz = "GMT")
if(source.type=="a"){
   slice.size<-12 #how many months in a time slice used in the analysis.
   interpolate.size<-3 #number of months between interpolated points in the output; 1 "row" is created for each interval. No interpolation if slice.size = interpolate.size
   minmax.years<-dbGetQuery(db,"SELECT min(year) min, max(year) max from abstract")[1,]
   start.year<-max(2006, as.numeric(minmax.years$min))
   start.month<-1 #default = 1
   end.year<-as.numeric(minmax.years$max)
   end.month<-1 #default = 1. NB this determines the start of the last slice
   
}else if(source.type=="b"){
   slice.size<-2 #how many months in a time slice used in the analysis.
   interpolate.size<-1 #number of months between interpolated points in the output; 1 "row" is created for each interval. No interpolation if slice.size = interpolate.size
   minmax.dates<-dbGetQuery(db,"SELECT min(datestamp) min, max(datestamp) max from blog_post")[1,]
   min.date<-as.POSIXlt(minmax.dates$min, tz="GMT")
   max.date<-as.POSIXlt(minmax.dates$max, tz="GMT")
   start.year<- min.date$year+1900
   if(!is.na(override.start.year)){start.year<-override.start.year}
   start.month<-min.date$mon+1#generally set to 1
   #since we want the end.date to actually be the START of the last slice and this must be a whole number of "slice.size" slices, there is some fiddling to do
   m.diff<-12*(max.date$year+1900-start.year)+max.date$mon-min.date$mon-1
   if(max.date$mday<28){m.diff<-m.diff-1}#remove months that are not [almost] complete
   end.date<-as.POSIXlt(paste(start.year,start.month,"1",sep="-"), tz = "GMT")
   end.date$mon<- end.date$mon+floor(m.diff/slice.size)*slice.size
   end.date<-as.POSIXlt(as.character(end.date))#otherwise $mon and $year are not changed right
   end.year<-end.date$year+1900
   end.month<-end.date$mon+1
   
}else{
   stop(paste("Unknown source type",source.type))
}

#create a mapping from datatable column names to PlainTextDocument attribute names
#"Keywords" and after are user-defined "localmetadata" properties while the rest are standard tm package document metadata fields
if(source.type=="a"){
   map<-list(Content="treated", Heading="title", Author="authors", DateTimeStamp="datestamp", Origin="origin", URL="url", DBLP_URL="dblp_url", Positive="pos_score", Negative="neg_score", Subjectivity="subj_score")
}else{
   map<-list(Content="treated", Heading="title", Author="authors", DateTimeStamp="datestamp", Origin="origin",URL="url", Positive="pos_score", Negative="neg_score", Subjectivity="subj_score")  
}

## CONVENIENT TO USE FUNCTION FOR BREWING
doBrew<-function(page.name, isGroup=FALSE){
   #Create the HTML/JS for the Google Chart using a Brew Template
   html.filename<-paste(page.name," ",file.postfix,".html",sep="")   
   web.page.url<-paste(web.page.base,html.filename,sep="/")
   gadget.filename<-paste(page.name,file.postfix,"gadget.xml",sep=" ")
   gadget.url.encoded<-URLencode(paste(web.page.base,gadget.filename,sep="/"), reserved=TRUE)
   
   isGadget=FALSE   
   brew(file=paste(brew.dir,"HV Brew Template.html",sep="/"),
        output=html.filename,run=TRUE)
   isGadget=TRUE
   brew(file=paste(brew.dir,"HV Brew Template.html",sep="/"),
        output=gadget.filename,run=TRUE)
}

# adapted from confidence.band (Derek Young and David Hunter)
# based on Ellipses, by J. Fox and G. Monette, from
# car package
confidence.band = function(model, levels=0.95, segments=50, col.points=palette()[1], 
                           col.line=palette()[1], col.bands=palette()[2], 
                           lty.line=1, lty.bands=2, ...) {
   if (attr(model$terms,"intercept")!=1 || length(model$coef) !=2) {
      stop(paste("condifence.bands only works for simple linear regression\n",
                 "with one predictor and an intercept"))
   }
   # plot(model$model[,2:1], col=col.points, ...)
   # abline(model, col=col.line, lty=lty.line, lwd=2)
   angles=(0:segments)*pi/segments
   halfcircle = cbind(cos(angles), sin(angles))  
   chol.shape = chol(vcov(model))
   slopes = (halfcircle %*% chol.shape)[,2]
   angles = angles+angles[which.max(slopes)]
   halfcircle = cbind(cos(angles), sin(angles))  
   center = model$coef
   radius = sqrt(2*qf(levels, 2, df.residual(model)))
   for (r in radius) {
      for (i in 1:2) {
         halfcircle = -halfcircle
         ellipse = sweep(r*(halfcircle %*% chol.shape), 2, center, "+")
         int = ellipse[,1]
         slope = ellipse[,2]
         x = -diff(int)/diff(slope)
         y = int[-1]+slope[-1]*x
         lines(x, y, lwd=2, lty=lty.bands, col=col.bands)
      }
   }
}

# convienience for plotting - incl linear fit and indication of confidence band if there is >= an approx fit
do.plot<-function(outFile, xdates,xvals,yvals,main.txt,sub.txt,y.txt, hiPts1=NULL, hiPts2=NULL){
   png(file=outFile, width=1000, height=1000,pointsize=12, res=150)
   plot(xdates, yvals, main=main.txt, sub=sub.txt, xlab="", ylab=y.txt, type="b")
   if(sum(yvals>0)>2){
      model<-lm(yvals ~ xvals)# +I(xvals^2))
      r.squared<-summary(model)$r.squared
      if(r.squared>0.3){
         yy <- model$coef %*% rbind(1,xvals)#,xvals^2)
         confidence.band(model)
         lines(xvals,yy,lwd=2,col=3)
      }
   }
   #two levels of highlighting - intended for positive sentiment.
   #the arguments hiPts are indexes into the x and y vectors
   if(length(hiPts1)>0){
      points(xdates[hiPts1],yvals[hiPts1], col="red")#open red circles
   }
   if(length(hiPts2)>0){
      points(xdates[hiPts2],yvals[hiPts2],pch=19, col="red")#filled red circles
   }
   ad<-dev.off()   
}

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
if((run.mode=="motion")){
   #interpolate dates define the plotting, whether or not an interpolation has actually occurred
   # hence they are offset to the centre of slices as well as (usually) interspersing the slice periods
   interpolate.start.dates<-as.POSIXlt(seq.POSIXt(init.date, by=paste(interpolate.size,"months"), length.out=num.interpolate))
   interpolate.start.dates$mon<-interpolate.start.dates$mon+(slice.size/2)
}
##
## Main work done now
##
if(do.groups){
   # a bunch of matrices to accumulate the grouped results while the following nest of 2 loops operates
   gmat.template<-matrix(ncol=length(word.lists),nrow=num.slices)
   gmat.or.slices.freq<-gmat.template
   gmat.or.slices.docs<-gmat.template
   gmat.or.slices.positive<-gmat.template
   gmat.or.slices.negative<-gmat.template
   gmat.or.slices.subjectivity<-gmat.template
}

# to keep the number of docs per slice (shown in HTML output)
slice.docs.cnt<-rep(0,num.slices)

titles<-gsub("\\."," ",names(word.lists))#lazy way to titles is to replace "." in the list element names - override if necessary
file.names<-names(word.lists[])

##
## Start to get results.
## The outer loop is over the 1..* lists of terms (term.lists in the HV_Init.R file). Separate viz for each
## The inner loop is over the years
##
for (i.run in 1:length(word.lists)){
   run.title<-titles[i.run]#For presentation
   run.name<-file.names[i.run]#for output file
   run.words<-unlist(word.lists[i.run])
   run.terms<-stemDocument(tolower(removePunctuation(run.words)))
   #eliminate any words that reduce to the same stem
   use<-match(unique(run.terms),run.terms)
   if(length(use)<length(run.terms)){
      run.words<-run.words[use]
      run.terms<-run.terms[use]
      print(paste("Eliminating words with same stem. Now using:",paste(run.words, collapse=",")))
   }
   #suppress groups if just 1 term
   do.groups.run<-do.groups && (length(run.terms)>1)
   
   ##logging
   while(sink.number()>0)
   {sink()}
   sink(file=paste(run.name," ",file.postfix,".log",sep=""), append=FALSE, type="output", split=TRUE)
   
   #prepare 0-containing vector to receive the results. NB some of the terms may not appear in some/all slices
   results.template<-rep(0,length(run.terms))
   names(results.template)<-run.terms
   #prep dataframes to receive the slice data
   data.slices.freq<-data.frame()
   data.slices.docs<-data.frame()
   data.slices.positive<-data.frame()
   data.slices.negative<-data.frame()
   data.slices.subjectivity<-data.frame()
   docs.used<-0
   print(paste("Run:",run.name, " Terms:", paste(run.terms, collapse=", ")))
   
   #Loop over the time slices, querying the database for each slice and for the user-designated search terms
   for(slice in 1:num.slices){
      #date range for this row
      start.date<-slice.start.dates[slice]
      end.date<-start.date
      end.date$mon<-end.date$mon+slice.size
      end.date<-as.POSIXlt(as.character(end.date))#otherwise $mon and $year are not changed right on year-crossing slices
      print(paste("Period: ",start.date,"<= T <",end.date))
      # get the indeces of the documents that come from the date range
      #the peculiar "mday-1" is needed to trick the filter since the source data has a year number only
      #      q.start.date<-start.date
      #       q.start.date$mday<-q.start.date$mday-1
      #       q.end.date<-end.date
      #       q.end.date$mday<-q.end.date$mday-1
      
      ## SQL creation.
      # NB: these fetch the "treated" text - i.e stopword removal and stemming etc already done
      if(source.type=="a"){
         sqlDateClause<-paste("year >= '",start.date$year+1900,"' AND year<'",end.date$year+1900,"'",sep="")
         sqlIdClause<-paste("id in (SELECT docid from abstract_fts4 WHERE treated MATCH '",
                            paste(run.terms, collapse=" OR "),"')",sep="")
         sql<-paste("SELECT origin, year||'-07-01' datestamp, pages, title, authors, treated, url, dblp_url, pos_score, neg_score, subj_score FROM abstract WHERE",sqlDateClause,"AND",sqlIdClause,sep=" ")
         sqlSums<-paste("SELECT SUM(treated_words) terms, COUNT(1) docs FROM abstract WHERE",sqlDateClause)
         
      }else if(source.type=="b"){
         sqlDateClause<-paste("datestamp >= '",as.character(start.date),"' AND datestamp<'",as.character(end.date),"'",sep="")
         sqlIdClause<-paste("id in (SELECT docid from blog_post_fts4 WHERE treated MATCH '",
                            paste(run.terms, collapse=" OR "),"')",sep="")
         sql<-paste("SELECT origin, datestamp, title, authors, treated, url, pos_score, neg_score, subj_score FROM blog_post WHERE",sqlDateClause,"AND",sqlIdClause,sep=" ")                  
         sqlSums<-paste("SELECT SUM(treated_words) terms, COUNT(1) docs FROM blog_post WHERE",sqlDateClause)         
      }
      
      #
      table<-NULL
      if(use.sqlite){
         #query, fetch all records to dataframe and clear resultset in one go
         table<-dbGetQuery(db,sql)
         #also extract the post-stopword (and other pre-processing) word count so that we can calculate frequencies
         slice.totals<-dbGetQuery(db, sqlSums)
      }
      
      #store for Brew. This is repeated (pointlessly) for each i.run
      slice.docs.cnt[slice]<-slice.totals$docs
      print(paste(slice.totals$docs,"documents in period"))
      docs.used<-docs.used+slice.totals$docs
      
      #only need to build a corpus and DTM if there are >0 documents in the slice with at least 1 run.term appearing. Otherwise just need to fill in zeros
      if(length(table[,1])>0){
         # now read in a corpus, handling the metadata via mappings previously declared
         corp<-Corpus(DataframeSource(table), readerControl=list(reader= readTabular(mapping=map)))
         #and build the DTM for the slice
         #NB1: no stopwords, stemming since this is already done in DB prep
         #NB2: use the run.terms as a dictionary so the DTM has minimal terms
         dtm.tf.slice<-DocumentTermMatrix(corp, control=list(stemming=FALSE, removePunctuation=FALSE, removeNumbers=FALSE, stopwords=FALSE, dictionary=run.terms))
         dtm.bin.slice<-weightBin(dtm.tf.slice)
         #compute some corpus and term statistics FOR INFORMATION
         print("Slice Document Term Matrix, Term-Frequency")
         print(dtm.tf.slice)
         dtm.tf.sums<-col_sums(dtm.tf.slice)      
         
         #calculate term frequency (%) and document count ** for the terms aposite to the current run
         #NB1: document count is normalised to **per month** for blogs and **per year** for conference abstracts
         ##make sure the term order is as in run.terms for consistency across all parts
         reorder<-match(run.terms,Terms(dtm.tf.slice))
         slice.freq<-100*col_sums(dtm.tf.slice)[reorder]/slice.totals$terms
         print("Frequences (%):")
         print(slice.freq)
         if(source.type=="b"){
            res<-col_sums(dtm.bin.slice)/slice.size   
         }else if(source.type=="a"){
            res<-col_sums(dtm.bin.slice)*12/slice.size
         }
         slice.docs<-res[reorder]
         print("Document count:")
         print(slice.docs)
         
         #calculate the "subjectivity" for each term according to the subjectivity of containing documents
         #and simultaneiously fill out rows of the data to be visualised
         # pull out the sentiment data
         pos_score<-table[,"pos_score"]
         neg_score<-table[,"neg_score"]
         subj_score<-table[,"subj_score"]
         sent.positive = vector()
         sent.negative = vector()
         subjectivity = vector()
         for (nt in run.terms){
            sent.positive[nt]<-mean(pos_score[as.numeric(Docs(dtm.tf.slice[as.matrix(dtm.tf.slice[,nt])>0,nt]))])
            sent.negative[nt]<-mean(neg_score[as.numeric(Docs(dtm.tf.slice[as.matrix(dtm.tf.slice[,nt])>0,nt]))])
            subjectivity[nt]<-mean(subj_score[as.numeric(Docs(dtm.tf.slice[as.matrix(dtm.tf.slice[,nt])>0,nt]))])
         }
         #get NaNs when terms not present since the Docs() selector above is then empty, which are a nuisance!
         sent.positive[is.nan(sent.positive)]<-0.0
         sent.negative[is.nan(sent.negative)]<-0.0
         subjectivity[is.nan(subjectivity)]<-0.0
         
         print("Subjectivity:")
         print(subjectivity)
         
         if(do.groups.run){
            #the groups require special treatment
            gmat.or.slices.freq[slice,i.run]<-sum(slice.freq)
            gmat.or.slices.docs[slice,i.run]<-sum(slice.docs)
            or.docs<-as.numeric(Docs(dtm.tf.slice)[row_sums(dtm.tf.slice)>0])
            gmat.or.slices.positive[slice,i.run]<-mean(pos_score[or.docs])
            gmat.or.slices.negative[slice,i.run]<-mean(neg_score[or.docs])
            gmat.or.slices.subjectivity[slice,i.run]<-mean(subj_score[or.docs])
            # next lines obsolete
            #         gmat.or.slices.positive[is.nan(gmat.or.slices.positive)]<-0.0
            #         gmat.or.slices.negative[is.nan(gmat.or.slices.negative)]<-0.0
            #         gmat.or.slices.subjectivity[is.nan(gmat.or.slices.subjectivity)]<-0.0
         }
      }else{
         #no documents in slice contain terms
         print("No documents in slice")
         slice.freq<-results.template
         slice.docs<-results.template
         sent.positive<-results.template
         sent.negative<-results.template
         subjectivity<-results.template
         if(do.groups.run){
            #the groups require special treatment
            gmat.or.slices.freq[slice,i.run]<-0.0
            gmat.or.slices.docs[slice,i.run]<-0.0
            gmat.or.slices.positive[slice,i.run]<-0.0
            gmat.or.slices.negative[slice,i.run]<-0.0
            gmat.or.slices.subjectivity[slice,i.run]<-0.0
         }
      }
      #accumulate the current slice with the previous
      data.slices.freq<-rbind(data.slices.freq,slice.freq)
      data.slices.docs<-rbind(data.slices.docs,slice.docs)
      data.slices.positive<-rbind(data.slices.positive,sent.positive)
      data.slices.negative<-rbind(data.slices.negative,sent.negative)
      data.slices.subjectivity<-rbind(data.slices.subjectivity,subjectivity)
   }
   # assemble the data into a convenient array for processing in a Brew template, interpolating if necessary
   #row.count<-num.interpolate*length(run.terms)
   #data.rows<-array(0,c(row.count,7))#this is destined for JSON in the Brew template
   data.rows<-data.frame()
   if((run.mode=="motion")){
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
   if((run.mode=="simple")){
      #loop over the terms in the run, plotting a chart according to the source data type
      #there is a little frigging to add a plot for all words in the set "OR" grouped
      #by adding the sum onto the dataframe as the last column, i.e. a pseudo-word
      run.length<-length(run.terms)
      words<-run.words
      if(do.groups.run){
         run.length<-length(run.terms)+1
         words<-c(words,paste("Words about",run.title))
         data.slices.freq<-cbind(data.slices.freq, gmat.or.slices.freq[,i.run])
         data.slices.docs<-cbind(data.slices.docs, gmat.or.slices.docs[,i.run])
         data.slices.positive<-cbind(data.slices.positive, gmat.or.slices.positive[,i.run])
      }
      sub.txt<-""
      for(i.term in 1:run.length){
         word<-words[i.term]
         if(source.type=="a"){
            xvals<-slice.start.dates$year+1900
            xdates<-xvals
            main.txt<-paste("\"",word,"\" in Conference Abstracts",sep="")
            y.d.txt<-"Abstracts per Year"
         }else{
            xvals<-as.numeric(slice.start.dates)#needed in order to fit squared term
            xdates<-slice.start.dates
            main.txt<-paste("\"",word,"\" in Blog Posts",sep="")
            y.d.txt<-"Posts per Month"
         }
         #tweaks to file name and titles depending whether or not this is the "group"
         if(do.groups.run && (i.term == run.length)){
            sub.txt<-paste("(shows any of ",paste(run.words,collapse=", "),")",sep="")
            main.txt<-gsub("\"","",main.txt)#remove quotes to look better
            outFile<-paste(run.name,"all",sep="-")
         }else{
            if(run.length==1){
               outFile<-run.name
            }else{
               outFile<-paste(run.name,word,sep="-")
            }
         }
         outFile<-paste(outFile, file.postfix)
         y.f.txt<-"Word Frequency (%)"
         #set highlighting for positive sentiment
         hi1<-data.slices.positive[,i.term]>0.08
         hi2<-data.slices.positive[,i.term]>0.1
         #frequency
         yvals<-data.slices.freq[,i.term]
         do.plot(paste(outFile,"freq.png"),xdates,xvals,yvals,main.txt,sub.txt,y.f.txt, hi1, hi2) 
         #docs
         yvals<-data.slices.docs[,i.term]
         do.plot(paste(outFile,"docs.png"),xdates,xvals,yvals,main.txt,sub.txt,y.d.txt, hi1, hi2)
       }
   }
   #stop logging
   sink()
}

##
## Groups special treatment
if(do.groups && ((run.mode=="motion"))){
   
   #interpolate 
   if(slice.size > interpolate.size){
      gmat.or.slices.freq<-apply(gmat.or.slices.freq, MARGIN=2,
                                  function(x) spline(x, method="natural", n=num.interpolate)$y)
      gmat.or.slices.docs<-apply(gmat.or.slices.docs, MARGIN=2,
                                  function(x) spline(x, method="natural", n=num.interpolate)$y)
      gmat.or.slices.positive<-apply(gmat.or.slices.positive, MARGIN=2,
                                      function(x) spline(x, method="natural", n=num.interpolate)$y)
      gmat.or.slices.negative<-apply(gmat.or.slices.negative, MARGIN=2,
                                      function(x) spline(x, method="natural", n=num.interpolate)$y)
      gmat.or.slices.subjectivity<-apply(gmat.or.slices.subjectivity, MARGIN=2,
                                          function(x) spline(x, method="natural", n=num.interpolate)$y)
   }
   data.rows<-data.frame()
   #do.groups.pretty<-gsub("\\."," ",names(word.lists)) #prettify
   for(g in 1:length(word.lists)){
      data.rows<-rbind(data.rows, data.frame(rep(titles[g],num.interpolate),
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

# properly terminate database use
if(use.sqlite){
   dbDisconnect(db)
}
