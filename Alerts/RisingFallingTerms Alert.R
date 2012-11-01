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
## A Simple alerter. Produce a concise report to highligh blog posts in a (short) timewindow
##    containing statistically significant rising/falling/new terms
## Relies on Database for source data (no CSV) and assumes that pre-process.R has already been run
##
library("RSQLite")
library("tm")
library("brew")

source("/home/arc1/R Projects/Text Mining Weak Signals/commonFunctions.R")

home.dir<-"/home/arc1/R Projects/Text Mining Weak Signals"
output.dir<-"/home/arc1/R Projects/Text Mining Weak Signals Output"
db.dir<-paste(home.dir,"Source Data",sep="/")
template.dir<-paste(home.dir,"Alerts",sep="/")

##
## RUN PARAMETERS - Often Changed
# The date of the report. Posts up to and including this date are candidates for output
report.date<-as.POSIXlt("2012-09-26")
# how many days to include when seeking candidates
report.days<-7
# How many months to use for baseline.
baseline.months<-12

title<-paste("TEL Blog Scan - Last",report.days,"Days")

##
## RUN PARAMETERS - Not often changed
# locate the database
sqlite.filename <- "TMWS Data A.sqlite"
# set output subfolder
output.subdir <- "RFTerms Alert"
# Thresholds for selection
# only ever show the top few. This is a max; if the score thresholds are not met nothing shows
top.n<-6
# how many documents must the term appear in to be listed. This is in addition to the frequency thresholds. A value of 2 is expected, i.e. ignore terms that appear in only one doc
# values higher than 2 may be needed for blogs where a given blog may have an oddity that appears in several posts.
doc_count.thresh <- 4
# p-value to accept the "alternative hypothesis" that there is something interesting
thresh.pval<-0.0001 #i.e. accept a .01% chance that null hypothesis falsely rejected
thresh.pval.falling<-0.005 #sometimes need to use a more lenient threshold for falling terms
#max frequency of term in the past set for eligibility as a weak signal.
#Above this, sigifnicant risers are "established terms"
max.past.freq<-0.0002
# PREVENT any of these origins from appearing in the Zeitgeist list. They are usually just link lists!
zg.blacklist<-c("http://www.lucygray.org/weblog")

##
## PRELIMINARIES - some initial setup-specific working
# database query
qdate<-function(d){
   return (paste("'",as.character(d),"'", sep=""))
}
# this query defines the posts to be considered for report. BETWEEN is inclusive
report.start<-report.date
report.start$mday<-report.start$mday - report.days
sql.recentIds<-paste("select id from blog_post where datestamp between",qdate(report.start),"and", qdate(report.date))
# this query defines the baseline (large), aka the "past" set of documents to compare against
baseline.end<-report.start
baseline.end$mday<-baseline.end$mday-1
baseline.start<-baseline.end
baseline.start$mon<-baseline.start$mon-baseline.months
sql.pastIds<- paste("select id from blog_post where datestamp between",qdate(baseline.start),"and", qdate(baseline.end))
#this query fetches the text content and metadata for both recent and past sets
sql<- paste("select id, content, title, authors, datestamp, url, origin from blog_post where datestamp between",qdate(baseline.start),"and", qdate(report.date))

# initialise database access
# instantiate the SQLite driver in the R process
sqlite<- dbDriver("SQLite")
# open sqlite connection. db is a "connection"
db<- dbConnect(sqlite, dbname=paste(db.dir,sqlite.filename,sep="/"))
summary(db)
# preparation for output destination
#setwd(paste(output.dir, output.subdir,sep="/"))
reportFile<-paste(paste(output.dir, output.subdir,paste(report.date,".html",sep=""),sep="/"))

#ID="id",
map<-list(id="id", Content="content", DateTimeStamp="datestamp")# Heading="title", Author="authors",    URL="url")

##
## MAIN
##
# get the ID lists for previous and current sets and the corpus of all documents in either set
recentIds<-as.numeric(dbGetQuery(db,sql.recentIds)[,1])
pastIds<-as.numeric(dbGetQuery(db,sql.pastIds)[,1])
corpus.table<-dbGetQuery(db,sql)
corpus<-Corpus(DataframeSource(corpus.table), readerControl=list(reader= readTabular(mapping=map)))
# REALLY we would use ID="id" in the mapping and then the vectors recentIds and pastIds would simply be passed to PearsonChanges.
# BUT there seems to be a bug in "tm" hence a mapping of id="id" is used and the next 4 lines map from "id" values as obtained from the database to ID metadata in the corpus
db.ids<-unlist(meta(corpus,"id",type="local"))
corpus.ids<-unlist(meta(corpus, "ID", type="local"))
recentIds.c<-corpus.ids[db.ids %in% recentIds]
pastIds.c<-corpus.ids[db.ids %in% pastIds]
# compute the significant rising and falling terms
swords<-CustomStopwords()
rfTerms<-PearsonChanges.Corpus(corpus, pastIds.c, recentIds.c,
                               doc_count.thresh = doc_count.thresh,
                               thresh.pval = thresh.pval,
                               thresh.pval.falling = thresh.pval.falling,
                               max.past.freq = max.past.freq,
                               stem  = TRUE, stop.words = swords)

##
## Additional work to prepare for writing the report
##
# lookup best guesses for unstemmed forms for the significant lists.
# New is not done since it is not used in the report
# replace NAs by the stemmed term (which is often the answer, dunno why stemCompletion doesn't do this)
stemmed<-names(rfTerms$Rising$P)
rising.words<-stemCompletion(stemmed,corpus,type="shortest")
rising.words[is.na(rising.words)]<-stemmed[is.na(rising.words)]
stemmed<-names(rfTerms$Established$P)
established.words<-stemCompletion(stemmed,corpus,type="shortest")
established.words[is.na(established.words)]<-stemmed[is.na(established.words)]
stemmed<-names(rfTerms$Falling$P)
falling.words<-stemCompletion(stemmed,corpus,type="shortest")
falling.words[is.na(falling.words)]<-stemmed[is.na(falling.words)]
# find the "top n" blog posts as being those with the most rising/established terms in
# 1st recover the DTM for all docs and filter down to the rising/est doc set
dtm.re.bin<-weightBin(rfTerms$DTM.tf[union(Docs(rfTerms$Rising$DTM),Docs(rfTerms$Established$DTM)),])
# limit to the rising/est term set
dtm.re.bin<-dtm.re.bin[,c(names(rfTerms$Rising$P), names(rfTerms$Established$P))]
#eliminate any posts from blacklisted blogs
filter<-!(corpus.table[Docs(dtm.re.bin),"origin"]%in%zg.blacklist)
dtm.re.bin<-dtm.re.bin[filter,]
## find the "top n"
term.cnt<-row_sums(dtm.re.bin)
top.n.selector<-order(term.cnt, decreasing = TRUE)[1:top.n]
ratings<-round(100*term.cnt[top.n.selector]/length(Terms(dtm.re.bin)))
top.doc.ids<-names(term.cnt[top.n.selector])
#get the terms appearing in each top doc#and map these to the unstemmed words
re.words<-c(rising.words, established.words)
top.doc.words<-lapply(top.doc.ids,function(x){re.words[Terms(dtm.re.bin[x,col_sums(dtm.re.bin[x,])>0])]})
# get the results to show, eliminating any black-listed blogs
hits<-corpus.table[top.doc.ids,]

##
## Produce a HTML Report
##
#this palette is used in brew for color-coding scores
score.pal<-c("#00FFFF","#0000FF","#800080","#FF0000","#FF8040")
pal.len<-length(score.pal)
# Write out the HTML Header
brew.conn<-file(reportFile, open="wt")
brew(file=paste(template.dir,"RisingFallingTerms BrewTemplate.html",sep="/"), output=brew.conn)
close(brew.conn)

dbDisconnect(db)