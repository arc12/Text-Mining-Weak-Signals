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
## Stuff data into a SQLite database, including stemmed/stopworded forms and FT indexing
##
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
source("/home/arc1/R Projects/Text Mining Weak Signals/sentimentFunctions.R")

home.dir<-"/home/arc1/R Projects/Text Mining Weak Signals"
db.dir<-paste(home.dir,"Source Data",sep="/")
setwd(db.dir)


#each one of these will be looped over NB the origin.tag must be in the same order as set.csv
# set.csv <- c("Abstracts/ICALT Abstracts 2005-2011.csv",
#                   "Abstracts/CAL Abstracts 2007-2011.csv",
#                   "Abstracts/ECTEL Abstracts 2006-2011.csv",
#                   "Abstracts/ICWL Abstracts 2005-2011.csv",
#                   "Abstracts/ICHL Abstracts 2008-2011.csv")
# origin.tag <- c("ICALT",
#                "CAL",
#                "ECTEL",
#                "ICWL",
#                "ICHL")#only used for abstracts

# 
# set.csv <- c("Abstracts/ICALT Abstracts 2012.csv",
#              "Abstracts/ICWL Abstracts 2012.csv",
#              "Abstracts/ICHL Abstracts 2012.csv",
#                "Abstracts/ECTEL Abstracts 2012.csv")
# origin.tag <- c("ICALT",
#                 "ICWL",
#                 "ICHL",
#                 "ECTEL")#only used for abstracts


set.csv<-  c("MB/R2 Blogs 20090101-20090701.csv", "MB/R2 Blogs 20090701-20100101.csv",
             "MB/R2 Blogs 20100101-20100701.csv", "MB/R2 Blogs 20100701-20110101.csv",
             "MB/R2 Blogs 20110101-20110701.csv", "MB/R2 Blogs 20110701-20120101.csv",
             "MB/R2 Blogs 20120101-20120701.csv", "MB/R2 Blogs 20120701-20121023.csv",
             "MB/R2 Blogs 20121022-20121031.csv")
#the database to store to
sqlite.filename <- "TMWS Data A.sqlite"

# this determines the source type: conference abstracts or blog content
#a is for abstracts, b is for blogs
#auto-detect the source type from the CSV header row of the 1st set
header<-names(read.csv(set.csv[1],nrows=1))
if(!is.na(match("abstract",header))){
   source.type<-"a"
}else if(!is.na(match("content",header))){
   source.type<-"b"  
}else{
      stop("Cannot detect source type from csv header:",header)
}

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
      #fetch the max id at the start
      prev.id<-dbGetQuery(db, sqlMaxId)[[1]]
      if(is.na(prev.id)){ prev.id=0}
      #this does the main content
      dbGetPreparedQuery(db, sqlTemplate, bind.data = table)
      #this does the FT indexes - this is a bit of a palaver for SQLite since FT is in a separate table
      #first retrieve the newly added doc ids
      to.add<-dbGetQuery(db,paste(sqlDocIds,prev.id,sep=""))
      #select these (in order) from the data.frame. NB THIS MESSES UP TABLE
      table<-table[match(to.add[,"url"], table[,"url"]),c("content","treated")]
      table<-cbind(data.frame(id=to.add[,"id"]),table)
      #remove "-" characters since we need to find terms like "e-book" but putting "e-book" in a FTS4 query searches for "e" NOT "book". i.e. we will also need to strip "-" from query terms.  
      table[,"content"] <- gsub("-","",table[,"content"])
      #add to the index
      dbGetPreparedQuery(db, sqlFullText, bind.data = table)
      
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
##  there are two for historical reasons. refactor to one CSV and change code later
##
# Read in the sentiment word lists (cols extracted from the Harvard Inquirer spreadsheet http://www.wjh.harvard.edu/~inquirer/)
# A) positive/negative sentiment
#The column headings MUST be "Entry,Positive,Negative"
targets<-list(Positive="Positive", Negative="Negative")
sentiment.dics<-prepareLexicons(paste(home.dir,"InquirerPosNeg.csv",sep="/"), targets)
# B) "PESTLE" parts
#The column headings MUST be unchanged 
#  but NB data.frame colnames do not allow "@" so "Econ@" becomes "Econ."
#  list element names map to the database field names
targets.pestle<-list(econ_score=c("Econ.","ECON"), legal_score="Legal", polit_score=c("Polit.", "POLIT"), doing_score=c("Need","Goal","Try","Means","Persist","Complet","Fail"), knowing_score=c("Know","Solve"))
sentiment.dics.pestle<-prepareLexicons(paste(home.dir,"PESTLE Scan/InquirerPESTLE2.csv",sep="/"), targets.pestle)

##
## Database queries and corpus mapppings are source-dependent
##
#"Keywords" and after are user-defined "localmetadata" properties while the rest are standard tm package document metadata fields
if(source.type == "a"){
   map<-list(Content="content", Heading="title", Author="authors", DateTimeStamp="year", Origin="origin", Keywords="keywords", URL="url", DBLP_URL="dblp_url")
   sqlTemplate<-"insert or replace into abstract (origin, year, pages, title, authors, abstract, keywords, url, dblp_url, pos_score, neg_score, subj_score, econ_score, polit_score, legal_score, doing_score, knowing_score, treated, non_stopwords, treated_words) values ($origin, $year, $pages, $title, $authors, $content, $keywords, $url, $dblp_url, $pos_score, $neg_score, $subj_score, $econ_score, $polit_score, $legal_score, $doing_score, $knowing_score, $treated, $non_stopwords, $treated_words)"
   sqlMaxId<-"SELECT MAX(id) FROM abstract"
   sqlDocIds<-"SELECT id, url FROM abstract where id>"
   sqlFullText<-"INSERT OR REPLACE INTO abstract_fts4 (docid, abstract, treated) values ($id, $content, $treated)"
   sqlCount<- "select count(1) from abstract"
}else if(source.type == "b"){
   map<-list(Content="content", Heading="title", Author="authors", DateTimeStamp="datestamp", Origin="origin",URL="url")
   sqlTemplate<-"insert or replace into blog_post (content, title, authors, datestamp, origin, url, pos_score, neg_score, subj_score, econ_score, polit_score, legal_score, doing_score, knowing_score, treated, non_stopwords, treated_words) values ($content, $title, $authors, $datestamp, $origin, $url, $pos_score, $neg_score, $subj_score, $econ_score, $polit_score, $legal_score, $doing_score, $knowing_score, $treated, $non_stopwords, $treated_words)"
   sqlMaxId<-"SELECT MAX(id) FROM blog_post"
   sqlDocIds<-"SELECT id, url FROM blog_post where id>"
   sqlFullText<-"INSERT OR REPLACE INTO blog_post_fts4 (docid, content, treated) values ($id, $content, $treated)"
   sqlCount<- "select count(1) from blog_post"
}

##
## MAIN LOOP over the sets: Read-in, add columns of metrics and write-out
##
for (src in 1:length(set.csv)){
   inFile<-set.csv[src]
   print(paste("Processing: ",inFile))
   # read in CSV with format year,pages,title,authors,abstract,keywords,url,dblp_url.
   #There is a header row. DBLP_URL is the vital key into the author centrality data
   table<-read.csv(inFile,header=TRUE,sep=",",quote="\"",stringsAsFactors=FALSE)
   
   #some source-specific preliminaries
   if(source.type == "a"){
      #remove cases where date is empty
      table<-table[!table[,"year"]=="",]
      #insert the "origin" as a new column
      origin<-rep(origin.tag[src], length(table[,1]))
      table<-cbind(origin,table)
      #rename "abstract" as "content"; this is an accident of history in the source data
      colnames(table)[colnames(table)=="abstract"]<-"content"
   }else if(source.type == "b"){
      #remove cases where date is empty
      table<-table[!table[,"datestamp"]=="",]
   }
   
   # do the usual treatments to remove stopwords, punctuation etc. This is NOT fed into the TM that
   # scores sentiment (this needs unstemmed) but is saved to the database for quick access in future
   stop.words<-CustomStopwords()
   treated<-removeNumbers(removeWords(tolower(removePunctuation(table[,"content"])),stop.words))
   #min word length is 3
   treated<-gsub("\\s[a-z][a-z]\\s","",treated)
   #collapse multiple spaces
   treated <- gsub(" {2,}"," ",treated)
   #stemDocument only acts on the last word if just fed a text string. Next line works around this
   treated<-unlist(lapply(treated,function(x)paste(stemDocument(unlist(strsplit(x," "))), collapse=" ")))
   table<-cbind(table,treated)   
   # calculate the stemmed word count for each document - not directly used but stored in database
   treated_words<-sapply(gregexpr("\\W+", treated), length) + 1
   table<-cbind(table, treated_words)
   
   # create a corpus, handling the metadata via mapping from datatable column names to PlainTextDocument attribute names
   corp<-Corpus(DataframeSource(table), readerControl=list(reader= readTabular(mapping=map)))
   ##
   ## Sentiment Analysis, specifically "subjectivity" at a document level.
   ##
   # Use the Harvard Inquirer word lists to score sets of responses against several sentiments.
   # NB this is an UNSTEMMED treatment
   # Sentiment is scored as the fraction of words in the document that are listed in the relevant sentiment dictionary/lexicon. Multiple occurrences count.
   # "subjectivity" is the sum of positive and negative scores
   # NB: this IS doing stopword removal etc again rather than using "treated" because sentiment analysis does NOT use the stemmed forms
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
   #stash the document word count (NB this is unstemmed but stop words havebeen removed)
   table<-cbind(table,data.frame(non_stopwords=doc.term.sums))
   # -- positive scores
   dtm.tf.unstemmed.p<-DocumentTermMatrix(corp,
                                          control=list(stemming=FALSE, stopwords=stop.words, minWordLength=3, removeNumbers=TRUE, removePunctuation=FALSE,dictionary=tolower(sentiment.dics[["Positive"]])))
   pos.score<-row_sums(dtm.tf.unstemmed.p)/doc.term.sums
   #force any v. short docs to have scores = 0.0
   pos.score[doc.term.sums<40]<-0.0
   print("Summary for positive score")
   summary(pos.score)
   # -- negative scores
   dtm.tf.unstemmed.n<-DocumentTermMatrix(corp,
                                          control=list(stemming=FALSE, stopwords=stop.words, minWordLength=3, removeNumbers=TRUE, removePunctuation=FALSE,dictionary=tolower(sentiment.dics[["Negative"]])))
   neg.score<-row_sums(dtm.tf.unstemmed.n)/doc.term.sums
   #force any v. short docs to have scores = 0.0
   neg.score[doc.term.sums<40]<-0.0
   print("Summary for negative score")
   summary(neg.score)
   # -- subjectivity
   subj.score<-pos.score + neg.score
   # add to the data.table
   table<-cbind(table,pos_score = pos.score, neg_score = neg.score, subj_score = subj.score)
   
   #Loop over the "PESTLE"-related dictionaries, each column is a potentially-merged Gen Inquirer category
   for(lex in 1:length(sentiment.dics.pestle)){
      dtm.tf.unstemmed.lex<-DocumentTermMatrix(corp,
           control=list(stemming=FALSE, stopwords=stop.words, minWordLength=3,
            removeNumbers=TRUE, removePunctuation=FALSE,dictionary=tolower(sentiment.dics.pestle[[lex]])))
      lex.score<-row_sums(dtm.tf.unstemmed.lex)/doc.term.sums
      #force any v. short docs to have scores = 0.0
      lex.score[doc.term.sums<40]<-0.0
      lex.score<-as.data.frame(lex.score)
      colnames(lex.score)<-names(sentiment.dics.pestle)[[lex]]
      # add to the data.table
      table<-cbind(table,lex.score)
      print(paste("====","Processed lexicon for", names(sentiment.dics.pestle)[[lex]],"===="))
      print("Summary stats and histograms AFTER removing score=0 documents")
      lex.score.nz<-lex.score[lex.score[,1]>0.0,1]
      summary.nz<- summary(lex.score.nz)
      print(summary.nz)
   }
   
   # tidy up
   rm(dtm.tf.unstemmed.all)
   rm(dtm.tf.unstemmed.p)
   rm(dtm.tf.unstemmed.n)

   
   ##
   ## OUTPUT to Database
   ##
   if(to.sqlite){
      ##each of the input data sets is handled as an independent transaction of inserts
      # hence, if there is a failure to insert a single record in a set, no records in the set will be added
      # the sqlTemplate should contain an "or replace" to avoid records with the same URL (which is a UNIQUE constraint in the database)      
      print("Begin Transaction:")
      dbBeginTransaction(db)
      begin.count <-dbGetQuery(db, sqlCount)[[1]]
      tryCatch(doInserts(),  error=didFail)
      end.count <-dbGetQuery(db, sqlCount)[[1]]
      print(paste("Initial # DB records=",begin.count,
                  "; Final # DB records=", end.count,
                  "; Inserted=", end.count-begin.count,
                  "; Replaced=",length(table[,1])-end.count+begin.count))
                  
   }else{
      
   }
}

# properly terminate database use
if(to.sqlite){
   dbDisconnect(db)
}
