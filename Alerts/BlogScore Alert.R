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
## A Simple alerter. Produce a concise report to highligh blog posts in a (short) timewindow with
##    pos/neg/subj, econ/polit/legal/knowing/doing, novelty scores.
## Relies on Database for source data (no CSV) and assumes that pre-process.R has already been run
##
library("RSQLite")
library("tm")
library("slam")
library("brew")
#library("RColorBrewer")

home.dir<-"/home/arc1/R Projects/Text Mining Weak Signals"
output.dir<-"/home/arc1/R Projects/Text Mining Weak Signals Output"
db.dir<-paste(home.dir,"Source Data",sep="/")
template.dir<-paste(home.dir,"Alerts",sep="/")

##
## RUN PARAMETERS - Often Changed
title<-"TEL Blog Scan - Last 14 Days"
# The date of the report. Posts up to and including this date are candidates for output
report.date<-as.POSIXlt("2012-09-26")
# how many days to include when seeking candidates
report.days<-14

##
## RUN PARAMETERS - Not often changed
# locate the database
sqlite.filename <- "TMWS Data A.sqlite"
# set output subfolder
output.subdir <- "BlogScore Alert"
# score columns. **** NB score.mins must follow this order *****
score.cols<-c("pos_score", "neg_score", "subj_score", "econ_score", "polit_score", "legal_score",
              "doing_score", "knowing_score")
# Reader-fiendly captions to match. These determine what is reported (set an element to NA to omit)
score.title<-c("Positive Sentiment", "Negative Sentiment",NA,"Economics Topic", "Political Topic", "Legal Topic", "Doing: Aims and Means", "Knowing and Problem-solving")
# These are shown as explanatory text under the titles in the report.
score.caption<-c("Posts containing a high proportion of words associated with positive sentiment",
                 "Posts containing a high proportion of words associated with negative sentiment",
                 "Posts with high subjective character (both positive and negative sentiment",
                 "Posts containing typically economic keywords",
                 "Posts containing typically political keywords",
                 "Posts containing typically legal keywords",
                 "Posts containing keywords associated with motivation, goals, the means of achieving goals and also the state of failure",
                 "Posts containing keywords associated with knowledge, knowledge acquisition and problem solving")
# These are used to work out a scale the scores to give a "rating" for approximate comparability.
# i.e. to that readers are not distracted by inherent differences between the dictionary word occurrence in the corpus. Use something like  1.1*(corpus max score)
rating.fullscale<-c(.325,.181,.382,
                 .275,.240,.154, #econ, polit, legal
                 .1925,.224)
# How many months to use for novelty baseline.
nov.baseline.months<-12
# Thresholds for selection
# only ever show the top few. This is a max; if the score thresholds are not met nothing shows
top.n<-4
# dictionary score minima by dictionary. Order must be same as score.cols.
# NB: these are unweighted and are used to define the point at which rating=1
score.mins<-c(0.15, #pos
              0.05, #neg
              0.15, #subj
              0.12,  #econ
              0.12, #ploit
              0.04,  #legal
              0.08,#doing
              0.08)#knowing
# novelty score min
nov.min<-0.82
# novelty rating zero-point. make this less than nov.min. This is the far-left position on the rating gauge
nov.ref<-0.7

##
## PRELIMINARIES - some initial setup-specific working
# database query
qdate<-function(d){
   return (paste("'",as.character(d),"'", sep=""))
}
# this query defines the posts to be considered for report. BETWEEN is inclusive
report.start<-report.date
report.start$mday<-report.start$mday - report.days
sql<- paste("select content, title, authors, datestamp, url,",paste(score.cols, collapse=", "),
            "from blog_post where datestamp between",qdate(report.start),"and", qdate(report.date))
# this query fetches the baseline (large) for novelty calculation
nov.end<-report.start
nov.end$mday<-nov.end$mday-1
nov.start<-nov.end
nov.start$mon<-nov.start$mon-nov.baseline.months
baseline.sql<- paste("select content, datestamp from blog_post where datestamp between",qdate(nov.start),"and", qdate(nov.end))
# initialise database access
# instantiate the SQLite driver in the R process
sqlite<- dbDriver("SQLite")
# open sqlite connection. db is a "connection"
db<- dbConnect(sqlite, dbname=paste(db.dir,sqlite.filename,sep="/"))
summary(db)
# preparation for output destination
#setwd(paste(output.dir, output.subdir,sep="/"))
reportFile<-paste(paste(output.dir, output.subdir,paste(report.date,".html",sep=""),sep="/"))

map<-list(Content="content", DateTimeStamp="datestamp")# Heading="title", Author="authors",    URL="url")

##
## MAIN
# Write out the HTML Header
#this palette is used in brew for color-coding scores
score.pal<-c("#00FFFF","#0000FF","#800080","#FF0000","#FF8040")
pal.len<-length(score.pal)
brew.conn<-file(reportFile, open="wt")
brew(file=paste(template.dir,"BlogScore BrewHeader.html",sep="/"), output=brew.conn)

# Loop over the dictionaries, emitting a section of HTML if there are any posts matching the thresholds
for(i in 1:length(score.caption)){
   section<-score.title[i]
   caption<-score.caption[i]
   if(!is.na(section)){
      sect.sql<-paste(sql,"and ", score.cols[i],">",score.mins[i], "order by", score.cols[i],"desc limit", as.character(top.n))
      hits<-dbGetQuery(db,sect.sql)#query, fetch all records to dataframe and clear resultset in one go
      #only create output if there are some "hits"
      if(length(hits[,1])>0){
         #extract and massage the scores for "friendly" display
         scores<-hits[,score.cols[i]]
         ratings<-round(100*(scores-score.mins[i])/(rating.fullscale[i]-score.mins[i]))
         ratings.capped<-pmin(100,ratings)
         #write out
         brew(file=paste(template.dir,"BlogScore BrewChunk.html",sep="/"), output=brew.conn)
      }
   }
}
# The novelty calculation requires some real work since we need to compare all posts in the reporting-period window against all those in the baseline period.
# This is not quite the same as in "Rising and Falling Terms" since the two sets are disjoint here
#candidates are in the reporting time window
candidates<-dbGetQuery(db,sql)
candidates.corp<-Corpus(DataframeSource(candidates), readerControl=list(reader= readTabular(mapping=map)))
candidates.corp<-tm_map(candidates.corp,removeNumbers)
candidates.corp<-tm_map(candidates.corp,removePunctuation)
candidates.dtm.bin<-DocumentTermMatrix(candidates.corp, control=list(stemming=TRUE, stopwords=TRUE, minWordLength=3, weighting=weightBin))
#eliminate very short docs as they give unreliable novelty calcs (too sparse). must have >15 different non-stopwords
ok.posts<-row_sums(candidates.dtm.bin)>15
candidates<-candidates[ok.posts,]
candidates.dtm.bin<-candidates.dtm.bin[ok.posts,]
# the baseline is what novelty is calculated with respect to
baseline<-dbGetQuery(db,baseline.sql)
baseline.corp<-Corpus(DataframeSource(baseline), readerControl=list(reader= readTabular(mapping=map)))
baseline.corp<-tm_map(baseline.corp,removeNumbers)
baseline.corp<-tm_map(baseline.corp,removePunctuation)
baseline.dtm.bin<-DocumentTermMatrix(baseline.corp, control=list(stemming=TRUE, stopwords=TRUE, minWordLength=3, weighting=weightBin))
# Calculate distance using cosine measure, i.e. as a scalar product
#first vector norms
doc.norm.mat<-sqrt(tcrossprod(row_sums(candidates.dtm.bin),row_sums(baseline.dtm.bin)))
#now eliminate terms that are not shared since their product will be zero anyway and we need to avoid attempts to allocate memory beyond what is permitted. Norms calc must be BEFORE this.
shared.terms<-Terms(candidates.dtm.bin)[Terms(candidates.dtm.bin) %in% Terms(baseline.dtm.bin)]
candidates.dtm.bin<-candidates.dtm.bin[,shared.terms]
baseline.dtm.bin<-baseline.dtm.bin[,shared.terms]
#the dot product
difference.mat<-1.0-tcrossprod(as.matrix(candidates.dtm.bin),
                               as.matrix(baseline.dtm.bin)) / doc.norm.mat
#this should not be necessary since sets are disjoint but it is possible identical posts are present, one in both sets, so make sure that any such do not interfere
difference.mat[difference.mat[,]==0]<-1.0
#sometimes NaNs creep in (not sure why)
difference.mat[is.nan(difference.mat[,])]<-1.0
#novelty means there is no other close doc so find the smallest difference
novelty<-apply(difference.mat,1,min)
# "hits" are those candidates above the threshold and in the top.n
top.n.bool<-order(novelty, decreasing=T)[1:min(top.n,length(novelty))]
hits<-candidates[top.n.bool,]
nov.top.n<-novelty[top.n.bool]
hits<-hits[nov.top.n>=nov.min,]
#only create output if there are some "hits"
if(length(hits[,1])>0){
   section<-"Novelty"
   caption<-paste("Posts with an unusual combination of words compared to posts from the previous",nov.baseline.months,"months")
   #extract and massage the scores for "friendly" display
   scores<-nov.top.n[nov.top.n>=nov.min]
      ratings<-round(100*(scores-nov.min)/(1-nov.ref))
      ratings.capped<-pmin(100,ratings)
      #write out
      brew(file=paste(template.dir,"BlogScore BrewChunk.html",sep="/"), output=brew.conn)     
}

#Write out the HTML Footer and close the report file
brew(file=paste(template.dir,"BlogScore BrewFooter.html",sep="/"), output=brew.conn)
close(brew.conn)


dbDisconnect(db)