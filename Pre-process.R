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
## Pre-processing of abstracts/documents for Text Mining Weak Signals
## Includes option to stuff data into a SQLite database
##
## Adds the following to raw CSV files as additional columns and saves to a "...with metrics.csv" file:
##    - subjectivity
##    - positive sentiment score
##    - negative sentiment score
##
## (these are document-specific metrics, hence "novelty" is not included since it is defined relative to a corpus)
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
library("RSQLite")

source("/home/arc1/R Projects/Text Mining Weak Signals/commonFunctions.R")
home.dir<-"/home/arc1/R Projects/Text Mining Weak Signals"
output.dir<-paste(home.dir,"Source Data/MB",sep="/")
db.dir<-paste(home.dir,"Source Data",sep="/")
setwd(output.dir)

#each one of these will be looped over NB the origin.tag must be in the same order as set.csv
# set.csv <- c("ICALT Abstracts 2005-2011.csv",
#                    "CAL Abstracts 2007-2011.csv",
#                    "ECTEL Abstracts 2006-2011.csv",
#                    "ICWL Abstracts 2005-2011.csv",
#                    "ICHL Abstracts 2008-2011.csv")
origin.tag <- c("ICALT",
                "CAL",
                "ECTEL",
                "ICWL",
                "ICHL")#only used for abstracts
set.csv <- c("MB Blogs 20090101-20100101.csv")
# this determines the source type: conference abstracts or blog content
source.type="b"#a is for abstracts, b is for blogs
sqlite.filename <- "TMWS Data A.sqlite" #set to NA for output to a CSV file

# preparation for output destination
to.sqlite<-!is.na(sqlite.filename)
if(to.sqlite){
   # instantiate the SQLite driver in the R process
   sqlite<- dbDriver("SQLite")
   # open sqlite connection. db is a "connection"
   db<- dbConnect(sqlite, dbname=paste(db.dir,sqlite.filename,sep="/"))
   summary(db)
   
   ## effectively "macros" for try/catch transaction
   doInserts <- function(){
      dbGetPreparedQuery(db, sqlTemplate, bind.data = table)
      print("Commit:")
      dbCommit(db)
   }
   didFail <- function(e){
      print(paste("Caught an error. DB Exception No=",dbGetException(db)$errorNum, " ", dbGetException(db)$errorMsg, sep=""))
      print("Roll-back:")
      dbRollback(db)
   }
}

##
## Prepare lexicon for sentiment analysis
##
# Read in the sentiment word lists (cols extracted from the Harvard Inquirer spreadsheet http://www.wjh.harvard.edu/~inquirer/)
#The column headings MUST be "Entry,Positive,Negative"
inquirer.table<-read.csv(paste(home.dir,"InquirerPosNeg.csv",sep="/"),
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
## MAIN LOOP over the sets: Read-in, add columns of metrics and write-out
##
for (src in 1:length(set.csv)){
   inFile<-set.csv[src]
   print(paste("Processing: ",inFile))
   outFile<-paste(strtrim(inFile,nchar(inFile)-4),"with metrics.csv")
   # read in CSV with format year,pages,title,authors,abstract,keywords,url,dblp_url.
   #There is a header row. DBLP_URL is the vital key into the author centrality data
   table<-read.csv(inFile,header=TRUE,sep=",",quote="\"",stringsAsFactors=FALSE)
   #remove cases where date is empty
   table<-table[!table[,"datestamp"]=="",]
   # choose an appropriate mapping and other source-specific preliminaries
   #"Keywords" and after are user-defined "localmetadata" properties while the rest are standard tm package document metadata fields
   if(source.type == "a"){
      #insert the "origin" as a new column
      origin<-rep(origin.tag[src], length(table[,1]))
      table<-cbind(origin,table)
      map<-list(Content="abstract", Heading="title", Author="authors", DateTimeStamp="year", Origin="origin", Keywords="keywords", URL="url", DBLP_URL="dblp_url")
      sqlTemplate<-"insert or replace into abstract (origin, year, pages, title, authors, abstract, keywords, url, dblp_url, pos_score, neg_score, subj_score) values ($origin, $year, $pages, $title, $authors, $abstract, $keywords, $url, $dblp_url, $pos_score, $neg_score, $subj_score)"
      sqlCount<- "select count(1) from abstract"
   }else if(source.type == "b"){
      map<-list(Content="content", Heading="title", Author="authors", DateTimeStamp="datestamp", Origin="origin",URL="url")
      sqlTemplate<-"insert or replace into blog_post (content, title, authors, datestamp, origin, url, pos_score, neg_score, subj_score) values ($content, $title, $authors, $datestamp, $origin, $url, $pos_score, $neg_score, $subj_score)"
      sqlCount<- "select count(1) from blog_post"
   }else{
      stop("Unknown source type:",source.type)
   }
   # create a corpus, handling the metadata via mapping from datatable column names to PlainTextDocument attribute names
   corp<-Corpus(DataframeSource(table), readerControl=list(reader= readTabular(mapping=map)))
   ##
   ## Sentiment Analysis, specifically "subjectivity" at a document level.
   ##
   # Use the Harvard Inquirer word lists to score sets of responses against several sentiments.
   # NB this is an UNSTEMMED treatment
   # Sentiment is scored as the fraction of words in the document that are listed in the relevant sentiment dictionary/lexicon. Multiple occurrences count.
   # "subjectivity" is the sum of positive and negative scores
   stop.words<-CustomStopwords()
   # no dictionary to get the total word count
   dtm.tf.unstemmed.all<-DocumentTermMatrix(corp,
                                            control=list(stemming=FALSE, stopwords=stop.words, minWordLength=3, removeNumbers=TRUE, removePunctuation=FALSE))
   doc.term.sums<-row_sums(dtm.tf.unstemmed.all)
   #make sure we remove empty documents
   empty.docs.bool<-doc.term.sums<1
   if(sum(empty.docs.bool)>0){
      doc.term.sums<-doc.term.sums[!empty.docs.bool]
      dtm.tf.unstemmed.all<-dtm.tf.unstemmed.all[!empty.docs.bool]
      corp<-corp[!empty.docs.bool]
      table<-table[!empty.docs.bool,]
      print(paste("Removed document item with no text content:",paste(which(empty.docs.bool))))
   }
   # -- positive scores
   dtm.tf.unstemmed.p<-DocumentTermMatrix(corp,
                                          control=list(stemming=FALSE, stopwords=stop.words, minWordLength=3, removeNumbers=TRUE, removePunctuation=FALSE,dictionary=tolower(sentiment.dics[["Positive"]])))
   pos.score<-row_sums(dtm.tf.unstemmed.p)/doc.term.sums
   # -- negative scores
   dtm.tf.unstemmed.n<-DocumentTermMatrix(corp,
                                          control=list(stemming=FALSE, stopwords=stop.words, minWordLength=3, removeNumbers=TRUE, removePunctuation=FALSE,dictionary=tolower(sentiment.dics[["Negative"]])))
   neg.score<-row_sums(dtm.tf.unstemmed.n)/doc.term.sums
   # -- subjectivity
   subj.score<-pos.score + neg.score
   # tidy up
   rm(dtm.tf.unstemmed.all)
   rm(dtm.tf.unstemmed.p)
   rm(dtm.tf.unstemmed.n)
   # add to the data.table
   table<-cbind(table,pos_score = pos.score, neg_score = neg.score, subj_score = subj.score)
   
   ##
   ## OUTPUT to CSV or Database
   ##
   if(to.sqlite){
      ##each of the input data sets is handled as an independent transaction of inserts
      # hence, if there is a failure to insert a single record in a set, no records in the set will be added
      # the sqlTemplate should contain an "or replace" to avoid records with the same URL (which is a UNIQUE constraint in the database)      
      print("Begin Transaction:")
      dbBeginTransaction(db)
      print(paste("Initial # records", dbGetQuery(db, sqlCount)[[1]],sep="="))
      tryCatch(doInserts(),  error=didFail)
      print(paste("Final # records", dbGetQuery(db, sqlCount)[[1]],sep="="))
   }else{
      #write out the new file
      write.csv(table, outFile, quote=TRUE, row.names=FALSE)
   }
}

# properly terminate database use
if(to.sqlite){
   dbDisconnect(db)
}
