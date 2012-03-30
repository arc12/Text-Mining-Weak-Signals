## ***Made available using the The MIT License (MIT)***
# Copyright (c) 2011, Adam Cooper
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
## ************ end licence ***************
##
## Perform sentence-level term extraction and association rule mining.
##

library("tm")
library("slam")
library("arules")
library("arulesViz")
library("Snowball")
library("openNLP")

## Run Properties - dependent on the source
base.dir<-"/home/arc1/R Projects/Text Mining Weak Signals"
source.dir<-paste(base.dir,"Source Data",sep="/")
#the output directory. NB convention to include the year
output.dir<-"/home/arc1/R Projects/Text Mining Weak Signals Output/Term Cluster/Union B/testing"
dir.create(output.dir, showWarnings=FALSE)
setwd(output.dir)
title<-"Term Clusters - Conference Proceedings from ICALT, CAL, ECTEL and ICWL"
abstracts.csv <- c("ICALT Abstracts 2005-2011 with metrics.csv",
                   "ECTEL Abstracts 2006-2011 with metrics.csv",
                   "ICWL Abstracts 2005-2011 with metrics.csv",
                   "CAL Abstracts 2007-2009 with metrics.csv")
conference.name <- c("ICALT",
                     "ECTEL",
                     "ICWL",
                     "CAL")
conference.title <- c("IEEE International Conference on Advanced Learning Technologies",
                      "European Conference on Technology Enhanced Learning",
                      "International Conference on Web-based Learning",
                      "Computer Assisted Learning Conference")
last.conference.url <- c("http://www.ask4research.info/icalt/2010/",
                         "http://www.ectel2010.org/",
                         "http://www.hkws.org/conference/icwl2010/",
                         "")
publisher.series <- c("IEEE",
                      "Springer Lecture Notes in Computer Science (LNCS)",
                      "Springer Lecture Notes in Computer Science (LNCS)",
                      "Springer Lecture Notes in Computer Science (LNCS)",
                      "Elsevier Computers and Education Journal")
publisher.url <- c("http://ieeexplore.ieee.org/xpl/mostRecentIssue.jsp?punumber=5570018",
                   "http://www.springerlink.com/content/978-3-642-16019-6/contents/",
                   "http://www.springer.com/computer/general+issues/book/978-3-642-17406-3",
                   "http://www.journals.elsevier.com/computers-and-education/")
                   
##
## Run properties - normally the same between different sources of the same kind for comparability
##
#time frame
selected.year<-2010
#remove very frequent and infrequent terms to be more efficient
min.doc.freq<-0.001 #terms must appear in >= this frac of docs
max.doc.freq<-0.8 #terms appearing in > this fraction of docs are removed
#a "transaction" is defined as a sentence and items to be the non-stopword terms
#Which association rules should be extracted? The LHS and RHS filters are applied separately
lhs.select<-c("serious","mobile")#the LHS of the association rule must contain one of these words
rhs.select<-c("game")

##future extension
#this option allows for metadata to also be treated as an item. These are column names in the CSV.
#the metdata is converted to pseudo-terms of the form "year=...." where "...." is a year from the CSV
#add.meta<-c("origin","year")

##
## Read in the data, aggregating the sources
##
table<-NULL
for (src in 1:length(abstracts.csv)){
   # read in CSV with format year,pages,title,authors,abstract,keywords. There is a header row. 
   tmp_table<-read.csv(paste(source.dir,abstracts.csv[[src]],sep="/"),header=TRUE,sep=",",quote="\"")
   #accumulate the table            
   table<-rbind(table,tmp_table)
   tmp_table<-NULL
}

# bring ONLY records with the required year out before stuffing into a corpus since it is more efficient to do so
table<-table[table[,"year"]==as.character(selected.year),]

#Take the text content and: split into sentences, remove punctuation
sentences <- unlist(sapply(table[,"abstract"],sentDetect, language="en"))

# now read in the sentences to a corpus
corp<-Corpus(VectorSource(sentences))

##
## Compute 
##
dtm <- DocumentTermMatrix(corp, control = list(weighting = weightBin,                                        stemming=FALSE, stopwords=TRUE, minWordLength=3, removeNumbers=TRUE, removePunctuation=TRUE))
#pruning
min.doc.count<-length(Docs(dtm))*min.doc.freq
dtm<-dtm[,col_sums(dtm)>=min.doc.count]
max.doc.count<-length(Docs(dtm))*max.doc.freq
dtm<-dtm[,col_sums(dtm)<=max.doc.count]
dtm<-dtm[row_sums(dtm)>0,]#clean out useless docs
print(dtm)
t<-as(as.matrix(dtm),"transactions")
#support is fraction of transactions containing the LHS set in the rule
#confidence is the fraction of transactions containing the LHS where the RHS is true
ap<-apriori(t, parameter=list(support=0.005, confidence=0.6))
print(summary(ap))
print(inspect(ap))
print("Sub-set with lift>4")
ap.highlift<-subset(ap,subset=lift>2) #subset can also select rules with terms
inspect(ap.highlift)

# try using arulesviz
plot(ap)
#the closes we can get to controling colour is
plot(ap, control=list(gray_range=c(0.2,1)))
#sel<-plot(ap, interactive=T) #interactive mode
#antecedent-consequent matrix plot (interactive)
plot(ap, method="matrix", interactive=T)
plot(ap, method="matrix", interactive=T, measure=c("lift","confidence"))
#this is a nicer viz
plot(ap, method="grouped")#can also be interactive but doesnt add muxch
#graph representation only readable for small rule-sets so only do subset
plot(ap.highlift, method="graph")
#which can be saved using
saveAsGraph(ap, file="rules.graphml")
#a more easily-read version (although more complex on 1st sight), but no graphml save
plot(ap.highlift, method="graph", control=list(type="items"))
