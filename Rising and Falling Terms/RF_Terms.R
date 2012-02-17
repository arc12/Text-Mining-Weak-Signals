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
## Inspect a corpus segmented into "the past" and "recent"
## And identify rising, falling and newly-occurring terms
## ----------------------------------------
library("tm")
library("Snowball")
library("slam")
library("colorspace")
library("corpora")

##
## NB {data_set_name}/RF_Init.R should be run first to establish the run parameters
##

# various plot functions, for writing to file and the normal graphics device
source("/home/arc1/R Projects/Text Mining Weak Signals/commonFunctions.R")
source("/home/arc1/R Projects/Text Mining Weak Signals/Rising and Falling Terms/plotFunctions.R")

#var to use to make some more space at the bottom of plots for long label (use par(mar=mar.bigmar) )
mar.default<-par("mar")
mar.bigmar<-c(6,4,4,2)

#start logging all std output to a file in addition to "printing" to console
#first clean up old sinks
while(sink.number()>0)
  {sink()}
sink(file="RF_Terms.log", append=FALSE, type="output", split=TRUE)
cat(paste(paste(rep("=",79),collapse="")),"\n")
cat(paste(title,"\n"))
cat(paste(paste(rep("=",79),collapse="")),"\n\n")
print(paste("From:",start.date," Split at:",key.date, sep=""))
timestamp(stamp=date(), prefix="##TIMESTAMP: ")



##
## Read in the abstracts. NB this code allows for vectors of csv file names, titles etc in RF_Init.R
##
#(aside) rbinding tables is faster and better than merging corpora using tm_combine, which leads to problematical duplicate document ids.
table<-NULL
for (src in 1:length(sets.csv)){
   # read in CSV with format year,pages,title,authors,abstract,keywords. There is a header row. 
   tmp_table<-read.csv(paste(source.dir,sets.csv[[src]],sep="/"),header=TRUE,sep=",",quote="\"")
   #accumulate the table            
   table<-rbind(table,tmp_table)
   tmp_table<-NULL
}
# now read in the possibly-cumulated table to a corpus, handling the metadata via mapping
#create a mapping from datatable column names to PlainTextDocument attribute names
#"Keywords" is a user-defined "localmetadata" property while the rest are standard tm package document metadata fields
if(source.type=="c"){
   map<-list(Content="abstract", Heading="title", Author="authors", DateTimeStamp="year",
                  Origin="origin", Keywords="keywords", URL="url", DBLP_URL="dblp_url",
                  Positive="pos.score", Negative="neg.score", Subjectivity="subj.score")
}else{
   #dblp_url is a dummy col in source data; put the field in corpus metadata for ease of handling later
   table<-cbind(table,data.frame(dblp_url = rep(NA,length(table[,1]))))
   map<-list(Content="content", Heading="title", Author="authors", DateTimeStamp="datestamp",
                  Origin="origin",  URL="url",  DBLP_URL="dblp_url",
                  Positive="pos.score", Negative="neg.score", Subjectivity="subj.score")  
}
#use the mapping while reading the dataframe source to create a coprus with metadata
corp<-Corpus(DataframeSource(table), readerControl=list(reader= readTabular(mapping=map)))

# trim the corpus so that documents come only from the used date range
#changed from using tm_index because it seemed not to work with month-level periods
dts<-as.POSIXlt(unlist(meta(corp,"DateTimeStamp",type="local")))
filter.bool<- (dts>start.date) & (dts<=last.date)
corp<-corp[filter.bool]

#this for additional "metadata" calculated below. This is more efficient than storing inside the corpus
extra.meta<-data.frame(row.names="DocId", DocId=unlist(meta(corp,tag="ID",type="local")))

# pull out the sentiment data
pos.score<-unlist(meta(corp,tag="Positive", type="local"))
if(is.null(pos.score)){
   stop("Source data should be pre-processed to add sentiment/subjectivity - usee Pre-process.R")
}
neg.score<-unlist(meta(corp,tag="Negative", type="local"))
subj.score<-unlist(meta(corp,tag="Subjectivity", type="local"))

## -----
## Standard pre-processing to get a document-term matrix of the entire corpus.
## -----
#use the standard stopword set with a few modifications!
#+  "paper" (which is common in journal/proceedings abstracts!)
stop.words<-stop.words<-CustomStopwords()
print("Stopwords being used")
print(stop.words)
cat(paste(paste(rep("-",79),collapse="")),"\n\n")
#Here we go////
#changed from a simple DocumentTermMatrix(corp, control=list(stemming=TRUE, stopwords=stop.words, minWordLength=3, removeNumbers=TRUE, removePunctuation=TRUE))
#because the new release of "tm" seeks to EFF things up (e.g. "badges," does not stem to "badg" but "badges " does)
corp2<-tm_map(corp,removeNumbers)#corp is left since we want to get the doc text later for users to see
corp2<-tm_map(corp2,removePunctuation)
dtm.tf<-DocumentTermMatrix(corp2, control=list(stemming=TRUE, stopwords=stop.words, minWordLength=3))
# dtm.tf<-DocumentTermMatrix(corp,
#   control=list(stemming=TRUE, stopwords=stop.words, minWordLength=3, removeNumbers=TRUE, removePunctuation=TRUE))
#dtm.bin<-weightBin(dtm.tf)
#terms.doc.cnt<-col_sums(dtm.bin) #the number of docs containing >=1 occurrence of the term
#compute some corpus and term statistics FOR INFORMATION
print("Computed Document Term Matrix, Term-Frequency")
print(dtm.tf)
dtm.tf.sums<-col_sums(dtm.tf)
cat("\n")
print("Summary Stats of (Total) Term Occurrences in the Corpus")
print(summary(dtm.tf.sums))

# read in the latest conference themes and extract term frequencies using same method as abstracts
# This may (or may not) correlate with "rising" or new terms in the abstracts, in which case the rise may be explained away.
if(!is.na(recent.themes.txt)){
      themes<-PlainTextDocument(x = paste(scan(file=recent.themes.txt, what="raw"),collapse=" "), language="english")
      themes.words<-tolower(stripWhitespace(removeWords(removeNumbers(removePunctuation(themes)),words=stop.words)))
      themes.terms<-stemDocument(themes.words, language="english")
      themes.tf<-termFreq(themes.terms)
      themes.tw<-termFreq(themes.words)    #unstemmed. Done this way because stemCompletion needs a corpus
      #show them all.
      barplot(themes.tw, las=2, cex.names=0.7, main="Words In the Conference Themes Description",
              ylab="Frequency")
      # Repeat the previous to create a PNG
      png("Images/ThemesWords.png", width=1000, height=1000,pointsize=12, res=150)
      barplot(themes.tw, las=2, cex.names=0.7, main="Words In the Conference Themes Description",
              ylab="Frequency")
      ad<-dev.off()
}

##
## APply a filter using key.date to get two lists of document ids, one for past and one for recent
##
#past.doc_ids.bool<-tm_index(corp,FUN=sFilter, doclevel = TRUE, useMeta = FALSE,
#paste("datetimestamp<'",key.date,"'",sep=""))
#changed since tm_index seems broken
dts<-as.POSIXlt(unlist(meta(corp,"DateTimeStamp",type="local")))
past.doc_ids.bool<- (dts<key.date)
print(paste(sum(past.doc_ids.bool),"documents in the reference set and",sum(!past.doc_ids.bool)," documents in the target set",sep=" "))
term.sums.past<-col_sums(dtm.tf[past.doc_ids.bool,])
tsp.all<-sum(term.sums.past)
term.sums.recent<-col_sums(dtm.tf[!past.doc_ids.bool,])
tsr.all<-sum(term.sums.recent)
#the number of docs in the new setcontaining >=1 occurrence of the term
dtm.bin<-weightBin(dtm.tf)
dtm.bin.recent<-dtm.bin[!past.doc_ids.bool,]
terms.doc.cnt.recent<-col_sums(dtm.bin.recent)

## ********************************
## author centrality needs to be calculated for each paper in the recent set.
##
if(is.na(authors.table)){
   max.betweenness<-NA
   extra.meta[,"MaxBetweenness"]<-NA
}else{
   dblp_url<-unlist(meta(corp,tag="DBLP_URL", type="local"))
   dblp_url<-dblp_url[!past.doc_ids.bool]
   max.betweenness<-vector()
   for(i in 1:length(dblp_url)){
      authors<-papers.table[as.character(dblp_url[[i]]),"AUTHOR_IDS"]
      if(is.na(authors)){
         print(paste("Paper in abstracts CSV was not found in authorship CSV: DBLP URL=",as.character(dblp_url[[i]])))
      }else{
         authors.sep<-unlist(strsplit(authors,","))
         betweenness<-authors.table[authors.sep,]
         max.betweenness[i]<-max(betweenness, na.rm=TRUE)
         if(max.betweenness[i]==-Inf){
            print(paste("Cannot find centrality measure for any of authors:",authors,"defaulting to 0.0"))
            max.betweenness[i]<-0.0            
         }
      }
   }
   #use corpus doc ids as names
   names(max.betweenness)<-names(dblp_url)
   print("Summary of per-paper max author betweenness:")
   print(summary(max.betweenness))
   # add as corpus metadata - this is very slow so I should attempt to improve it
   # for(i in 1:length(max.betweenness)){
   #    meta(corp[[names(max.betweenness)[i]]],tag="MaxBetweenness")<-
   #       round(max.betweenness[[i]],digits=5)
   # }
   extra.meta[names(max.betweenness),"MaxBetweenness"]<-max.betweenness
}

##
## Compute document distances based on binary term occurrence (all terms, not just WS terms)
## to look for "novel" abstracts. Consider documents in the recent set in relation to ALL docs
##
# remove common terms so that the selectivity is enhanced
common.terms<-col_sums(dtm.bin)/length(Docs(dtm.bin))>term.doc_occurrence.max
dtm.bin.trimmed<-dtm.bin[,!common.terms]
dtm.bin.recent.trimmed<-dtm.bin.recent[,!common.terms]
doc.norm.mat<-sqrt(tcrossprod(row_sums(dtm.bin.recent.trimmed),row_sums(dtm.bin.trimmed)))
difference.mat<-1.0-tcrossprod(as.matrix(dtm.bin.recent.trimmed),
                               as.matrix(dtm.bin.trimmed)) / doc.norm.mat
#self referential terms need "removing" (since the mat is not square, cant use "diag")
difference.mat[difference.mat[,]==0]<-1.0
#sometimes NaNs creep in (not sure why)
difference.mat[is.nan(difference.mat[,])]<-1.0
#novelty means there is no other close doc so find the smallest difference
novelty<-apply(difference.mat,1,min)
#the summary stats
novelty.summary<-summary(novelty)
print("Summary Stats for Novelty")
print(novelty.summary)
#if the histogram is very skew to high values (e.g. 0.8) then it isn't possible to ident novel
basic.hist(novelty,"Absolute Novelty Values","minimum(1 - similarity)","Images/AbsoluteNovelty.png", Breaks=20)
# calculate a standardised novelty to try to compensate for varying skewness
#NB the median is calculated FOR ALL document novelty in the corpus, not just for the recent set, so repeat earlier calc but now for full cross product
full.doc.norm.mat<-sqrt(tcrossprod(row_sums(dtm.bin.trimmed),row_sums(dtm.bin.trimmed)))
full.difference.mat<-1.0-tcrossprod(as.matrix(dtm.bin.trimmed),
                               as.matrix(dtm.bin.trimmed)) / full.doc.norm.mat
diag(full.difference.mat)<-1.0
full.difference.mat[is.nan(full.difference.mat[,])]<-1.0
full.novelty<-apply(full.difference.mat,1,min)
#
standardised.novelty<-(novelty-median(full.novelty))/(1-median(full.novelty))
std.novelty.summary<-summary(standardised.novelty)
print("Summary of Standardised Novelty")
print(std.novelty.summary)

std.nov.extreme <-standardised.novelty[standardised.novelty>=std.novelty.min]
print(paste("Docs above standard novelty threshold",std.novelty.min))
print(std.nov.extreme)
basic.hist(standardised.novelty,"Standardised Novelty","","Images/StandardisedNovelty.png", Breaks=30)
# add "standardised novelty" as corpus metadata
# this is very slow so I should attempt to improve it
# for(i in 1:length(standardised.novelty)){
#    meta(corp[[names(standardised.novelty)[i]]],tag="StdNovelty")<-
#       round(standardised.novelty[[i]],digits=2)
# }
extra.meta[names(standardised.novelty),"StdNovelty"]<-standardised.novelty

##
## Sentiment Analysis, specifically "subjectivity" was calculated in pre-processing
pos.score.recent<-pos.score[!past.doc_ids.bool]
neg.score.recent<-neg.score[!past.doc_ids.bool]
subj.score.recent<-subj.score[!past.doc_ids.bool]
#pos.terms.sums<-col_sums(dtm.tf.unstemmed.p[,col_sums(dtm.tf.unstemmed.p)>0])
print("Summary of Positive Sentiment Scores in Recent Set")
summary(pos.score.recent)
basic.hist(pos.score.recent,Main="Positive Sentiment (Recent Documents)", Xlab="Score",
           OutputFile="Images/PosSentiment.png", Breaks=20)
# -- negative scores
print("Summary of Negative Sentiment Scores")
summary(neg.score.recent)
basic.hist(neg.score.recent,Main="Negative Sentiment", Xlab="Score",
           OutputFile="Images/NegSentiment.png", Breaks=20)
# -- subjectivity
subj.summary<-summary(subj.score.recent)
print("Summary of Subjectivity Scores (positive+negative)")
print(subj.summary)
#upper outliers as plotted on the boxplot (plotted further on)
subj.thresh<-(5*subj.summary[["3rd Qu."]]-3*subj.summary[["1st Qu."]])/2
subj.outliers<-subj.score.recent[subj.score.recent>subj.thresh]
# plot sentiment balance for most "sentinental" docs
subj.9th.dec<-quantile(subj.score.recent,0.9)
sentiment.barplot(neg.score.recent[subj.score.recent>=subj.9th.dec],
                  pos.score.recent[subj.score.recent>=subj.9th.dec], Main="9th Decile Subjectivity",
                  Ylab="Subjectivity", OutputFile="Images/TopSubjectivity.png")

# quick view of sentiment and subjectivity distributions and outliers
# upper outliers are above a limit = 3rd quartile + 1.5*inter_quartile_range
#  (inter quartile range = difference between 1st and 3rd quartiles)
# lower outliers are not expected but have an analogously-defined limit relative to the 1stQ
# the plot "whiskers" default to the outlier limits described above or to the most extreme data-point if less than these limits.
basic.boxplot(list(Positive=pos.score.recent,Negative=neg.score.recent,Subjectivity=subj.score.recent), Ylab="Sentiment Score", OutputFile="Images/SentimentComparison.png")

#GET some boolean filters for three groups, Rising, Falling and New. No decision on significance yet!
#term sums of 0 in the past must be new terms, since we know the corpus sum>0
#ONLY select those with a mininum number of document occurrences
new.term_ids.bool <- (term.sums.past==0) & (terms.doc.cnt.recent>=doc_count.thresh)
#which terms should be considered in rising/falling?
#for rising terms (which includes the old "nearl-new" concept, again require a minimum number of documents)
rise.term_ids.bool <- (term.sums.recent/tsr.all>term.sums.past/tsp.all) &
                       (term.sums.past>0) & (terms.doc.cnt.recent>=doc_count.thresh)
fall.term_ids.bool <- (term.sums.recent/tsr.all<term.sums.past/tsp.all)

##
## compute the term occurrences in each set (past/recent) and group (rising/falling/new)
##
dtm.tf.new<-dtm.tf[!past.doc_ids.bool,new.term_ids.bool]
dtm.tf.rising<-dtm.tf[!past.doc_ids.bool,rise.term_ids.bool]
term.sums.new<-term.sums.recent[new.term_ids.bool]#the "past" is 0 of course
term.sums.past.rising<-term.sums.past[rise.term_ids.bool]
term.sums.recent.rising<-term.sums.recent[rise.term_ids.bool]
term.sums.past.falling<-term.sums.past[fall.term_ids.bool]
term.sums.recent.falling<-term.sums.recent[fall.term_ids.bool]
#calculate a rise/fall factor excluding any new terms
#basically: (recent fraction - past fraction)/(past fraction) but rescaled to a % to give "nicer" numbers for display
tsp.rise<-term.sums.past[rise.term_ids.bool]
tsr.rise<-term.sums.recent[rise.term_ids.bool]
rise.ratio<-(tsr.rise*tsp.all/(tsr.all*tsp.rise) -1)* 100
tsp.fall<-term.sums.past[fall.term_ids.bool]
tsr.fall<-term.sums.recent[fall.term_ids.bool]
fall.ratio<-(tsr.fall*tsp.all/(tsr.all*tsp.fall) -1)* 100

##
# Some basic stats and plots of the distributions before applying significance testing
##
# Inspect the distribution of new terms.
print("Summary of New Terms (before significance test)")
raw.summary.new<-summary(term.sums.new)
print(raw.summary.new)
print("Deciles")
print(quantile(term.sums.new, probs=seq(0.1,0.9,0.1)))
# Rising Terms
print("Summary of Rising Terms: Distribution of % Rise (before significance test)")
raw.summary.rise<-summary(rise.ratio)
print(raw.summary.rise)
print("Deciles:")
print(quantile(rise.ratio, probs=seq(0.1,0.9,0.1)))
#Falling Terms
print("Summary of Falling Terms: Distribution of % Fall (before significance test)")
raw.summary.fall<-summary(fall.ratio)
print(raw.summary.fall)
print("Deciles:")
print(quantile(fall.ratio, probs=seq(0.1,0.9,0.1)))

#plot the distribution of new terms that appear in at least 2 docs (whether "significant" or not)
term.sums.new.t<-tabulate(term.sums.new)
basic.barplot(term.sums.new.t, "Histogram of New Term Occurrence (before significance test)", "Term Occurrence",
              "Number of Terms", seq(1:max(term.sums.new)), 
              "Images/NewTermFrequencies.png")
#plot a histogram of the % rises
basic.hist(rise.ratio, Main="Histogram of Rising Terms (before significance test)",
           Xlab="Rise Ratio (%)", OutputFile="Images/RisingTerm_Distribution.png")
#plot a histogram of the % falls
basic.hist(fall.ratio, Main="Histogram of Falling Terms (before significance test)",
           Xlab="Fall Ratio (%)", OutputFile="Images/FallingTerm_Distribution.png")


##
## Apply Pearson's Chi^2 Test to each group, then filter down as appropriate
##
p.rising<-chisq.pval(term.sums.past.rising,tsp.all ,term.sums.recent.rising, tsr.all)
p.falling<-chisq.pval(term.sums.past.falling,tsp.all ,term.sums.recent.falling, tsr.all)
p.new<-chisq.pval(0.0, tsp.all, term.sums.new, tsr.all)
names(p.rising)<-names(term.sums.past.rising)
names(p.falling)<-names(term.sums.past.falling)
names(p.new)<-names(term.sums.new)
# use the chosen level of significance to filter-down the DTMs
dtm.tf.new<-dtm.tf.new[,p.new<=thresh.pval]
dtm.tf.new<-dtm.tf.new[row_sums(dtm.tf.new)>0]
dtm.tf.rising<-dtm.tf.rising[,p.rising<=thresh.pval]
dtm.tf.rising<-dtm.tf.rising[row_sums(dtm.tf.rising)>0,]
#these are used later to output the relevant docs/metadata
#corp.rising<-corp[Docs(dtm.tf.rising)] moved to later
corp.new<-corp[Docs(dtm.tf.new)]
#filter down the vectors of the important measures
#fall.term_ids.bool<- fall.term_ids.bool & (p.falling<=thresh.pval)
rise.ratio<-rise.ratio[p.rising<=thresh.pval]
fall.ratio<-fall.ratio[p.falling<=thresh.pval.falling]
#similarly finally truncate the p-value vectors now we have finished using them as filters
p.rising<-p.rising[p.rising<=thresh.pval]
p.falling<-p.falling[p.falling<=thresh.pval.falling]# NB different threshold
p.new<-p.new[p.new<=thresh.pval]
#prep palette for rising/falling %s
rf.ratio.int.min<-min(as.integer(fall.ratio))
rf.ratio.int.max<-max(as.integer(rise.ratio))
rf.palette<-diverge_hcl(rf.ratio.int.max-rf.ratio.int.min, c = 200, l = c(40, 120), power = 1)

##
## remove "established terms" from the rising set and hive them off to a separate list
##
past.freq.rising <- col_sums(dtm.tf[past.doc_ids.bool,Terms(dtm.tf.rising)])/tsp.all
p.established.rising <- p.rising[past.freq.rising>max.past.freq]
p.rising <- p.rising[past.freq.rising<=max.past.freq]
dtm.tf.est<-dtm.tf.rising[,past.freq.rising>max.past.freq]
dtm.tf.est<-dtm.tf.est[row_sums(dtm.tf.est)>0]
dtm.tf.rising<-dtm.tf.rising[,past.freq.rising<=max.past.freq]
dtm.tf.rising<-dtm.tf.rising[row_sums(dtm.tf.rising)>0]
corp.rising<-corp[Docs(dtm.tf.rising)]
established.rise.ratio<-rise.ratio[past.freq.rising>max.past.freq]
rise.ratio<-rise.ratio[past.freq.rising<=max.past.freq]

##
## find documents containing A LOT of rising/new terms of any kind
##
dtm.bin.recent.any<-dtm.bin.recent[,c(names(established.rise.ratio), names(rise.ratio),  names(p.new))]
rs<-row_sums(dtm.bin.recent.any)
top10<-rs[order(rs,decreasing=TRUE)][1:10]
print("Top 10 documents containing the most distinct rising, established or new terms:")
print(top10)

##
## Find novelty, max author centrality and subjectivity of documents containing the significant new/rising terms
##
# new
std.nov.terms.new<-NULL
for (nt in Terms(dtm.tf.new)){
   std.nov.terms.new[[nt]]<-standardised.novelty[Docs(dtm.tf.new[as.matrix(dtm.tf.new[,nt])>0,nt])]
}
subj.terms.new<-NULL
for (nt in Terms(dtm.tf.new)){
   subj.terms.new[[nt]]<-subj.score.recent[Docs(dtm.tf.new[as.matrix(dtm.tf.new[,nt])>0,nt])]
}
betweenness.terms.new<-NULL
for (nt in Terms(dtm.tf.new)){
   betweenness.terms.new[[nt]]<-max.betweenness[Docs(dtm.tf.new[as.matrix(dtm.tf.new[,nt])>0,nt])]
}
#rising
std.nov.terms.rising<-NULL
for (nt in Terms(dtm.tf.rising)){
   std.nov.terms.rising[[nt]]<-standardised.novelty[Docs(dtm.tf.rising[as.matrix(dtm.tf.rising[,nt])>0,nt])]
}
subj.terms.rising<-NULL
for (nt in Terms(dtm.tf.rising)){
   subj.terms.rising[[nt]]<-subj.score.recent[Docs(dtm.tf.rising[as.matrix(dtm.tf.rising[,nt])>0,nt])]
}
betweenness.terms.rising<-NULL
for (nt in Terms(dtm.tf.rising)){
   betweenness.terms.rising[[nt]]<-max.betweenness[Docs(dtm.tf.rising[as.matrix(dtm.tf.rising[,nt])>0,nt])]
}

##
## Output of New Terms Results
##
#use the binary DTM to count document occurrences
dtm.bin.new<-weightBin(dtm.tf.new)
cnt.docs_with_new<-col_sums(dtm.bin.new)
print("Doc-Term Matrix after Filtering Most Statistically Significant")
print(dtm.tf.new)
term.sums.new.sel<-col_sums(dtm.tf.new) #########################
#find unstemmed words for the selected terms to make for prettier plots
new.sel.words<-stemCompletion(names(term.sums.new.sel),corp.new,type="shortest")
new.sel.words[is.na(new.sel.words)]<-names(new.sel.words[is.na(new.sel.words)])
# chart the term frequency for significant new terms 
insideLabel.barplot(term.sums.new.sel, Main="Most Significant New Terms",
                              Ylab="Term Frequency", Names=new.sel.words, 
                              "Images/NewTermFrequencies_Selected.png")
# do similarly for the p-values, plotting -log10 to give an "Unlikelihood power"
insideLabel.barplot(-log10(p.new), Main="Most Significant New Terms",
                              Ylab="Significance (-log10(p))", Names=new.sel.words, 
                              "Images/NewTermSignificance.png")
#plot the document occurrences
insideLabel.barplot(cnt.docs_with_new, Main="Occurrence of New Terms",
                              Ylab="Number of Docs", Names=new.sel.words, 
                              "Images/NewTerm_DocOccurrence_Bar.png", ForceIntYAxis=TRUE)
basic.heatmap(as.matrix(dtm.tf.new), Main="New Term Occurrence",
              ColumnLabels=new.sel.words,
              OutputFile="Images/NewTerm_DocOccurrence_Heat.png")
#Plots for novelty and subjectivity
basic.boxplot(std.nov.terms.new,Ylab="Standardised Novelty",Main="Novelty of Documents Containing New Terms",Names=new.sel.words, OutputFile="Images/NewTermNovelty.png")
basic.boxplot(subj.terms.new,Ylab="Subjectivity Score",Main="Subjectivity of Documents Containing New Terms",Names=new.sel.words, OutputFile="Images/NewTermSubjectivity.png")
# write out input text for Wordle
write.table(-log10(p.new), "Wordle/NewTermsSignificance.txt", quote=FALSE, sep=":", col.names=FALSE)

##
## Output of Rising Term Results 
##
# Get full-words in place of stemmed terms to make for prettier presentation
# replace NAs by the stemmed term (which is often the answer, dunno why stemCompletion doesn't do this)
rising.selected.words<-stemCompletion(names(rise.ratio),corp.rising,type="shortest")
rising.selected.words[is.na(rising.selected.words)]<-names(rise.ratio[is.na(rising.selected.words)])
#plot the rises as a bar chart with the most-rising coloured red/pink
rising.cols<-rf.palette[as.integer(rise.ratio)-rf.ratio.int.min-1]
# truncate very large bars for display
rise.ratio.trunc<-rise.ratio
rise.ratio.trunc[rise.ratio>rising.plot.max]<-rising.plot.max
colorized.barplot(rise.ratio.trunc, Main="Rising Terms", Ylab="% Rise in Target Set",
                  Names=rising.selected.words, Colours=rising.cols,
                  OutputFile="Images/RisingTerms_PC.png")
# Plot the frequency of occurrence of the rising terms in the past and recent sets as a stacked bar chart
r.term.sums.recent<-col_sums(dtm.tf.rising)
r.term.sums.past<-col_sums(dtm.tf[past.doc_ids.bool,Terms(dtm.tf.rising)])
pair.barplot(X.past=r.term.sums.past/past.recent.ratio, X.target=r.term.sums.recent,
                Main="Un-scaled Rising Terms", Ylab="Term Occurrence",
                Names=rising.selected.words,
                OutputFile="Images/RisingTerms_PastRecent_Counts.png")
pair.barplot(X.past=100*r.term.sums.past/tsp.all, X.target=100*r.term.sums.recent/tsr.all,
             Main="Rising Term Proportions", Ylab="Term Frequency (%)",
             Names=rising.selected.words, Beside=TRUE,
             OutputFile="Images/RisingTerms_PastRecent_PC.png")
#prep palette for "unlikelihood power"
up.palette<-diverge_hcl(20, c = 200, l = c(40, 120), power = 1)
logp.rising <- -log10(p.rising)
palette.index.rising <- as.integer(logp.rising + log10(thresh.pval))*4 +1
palette.index.rising[palette.index.rising>20]<-20 #flatten off extreme peaks
palette.index.rising[palette.index.rising<0]<-0
up.rising.cols <- up.palette[palette.index.rising]
#plot the colorised significace for all rising terms
colorized.barplot(logp.rising, Main="Rising Terms", Ylab="Significance (-log10(p))",
                  Names=rising.selected.words, Colours=up.rising.cols,
                  OutputFile="Images/RisingSignificance.png")
#scatter plot significance vs %rise
log.scatter(rise.ratio, logp.rising, Main="Rising Terms", Xlab="% Rise",
     Ylab="Significance (-log10(p))", OutputFile="Images/RisingSigPC_Scatter.png")
#wordle weighting data
write.table(logp.rising, "Wordle/RIsingTermSignificance.txt", quote=FALSE, sep=":", col.names=FALSE)
write.table(rise.ratio, "Wordle/RIsingTermPC.txt", quote=FALSE, sep=":", col.names=FALSE)
#Plots for novelty and subjectivity
basic.boxplot(std.nov.terms.rising,Ylab="Standardised Novelty",Main="Novelty of Documents Containing Rising Terms",Names=rising.selected.words, OutputFile="Images/RisingTermNovelty.png")
basic.boxplot(subj.terms.rising,Ylab="Subjectivity Score",Main="Subjectivity of Documents Containing Rising Terms",Names=rising.selected.words, OutputFile="Images/RisingTermSubjectivity.png")

##
## Output of **ESTABLISHED* but still sigificantly Rising Term Results 
##
est.rising.selected.words<-stemCompletion(names(established.rise.ratio),corp.rising,type="shortest")
est.rising.selected.words[is.na(est.rising.selected.words)]<-names(established.rise.ratio[is.na(est.rising.selected.words)])
#use same palette as before
est.rising.cols<-rf.palette[as.integer(established.rise.ratio)-rf.ratio.int.min-1]
#plot the rises as a bar chart with the most-rising coloured red/pink
colorized.barplot(established.rise.ratio, Main="Established Rising Terms", Ylab="% Rise in Target Set",
                  Names=est.rising.selected.words, Colours=est.rising.cols,
                  OutputFile="Images/EstablishedRisingTerms_PC.png")
#prep palette for "unlikelihood power"
up.palette<-diverge_hcl(20, c = 200, l = c(40, 120), power = 1)
logp.established.rising <- -log10(p.established.rising)
palette.index.est.rising <- as.integer(logp.established.rising + log10(thresh.pval))*4 +1
palette.index.est.rising[palette.index.est.rising>20]<-20 #flatten off extreme peaks
palette.index.est.rising[palette.index.est.rising<0]<-0
up.est.rising.cols <- up.palette[palette.index.est.rising]
#plot the colorised significace for all rising terms
colorized.barplot(logp.established.rising, Main="Established Rising Terms", Ylab="Significance (-log10(p))",
                  Names=est.rising.selected.words, Colours=up.est.rising.cols,
                  OutputFile="Images/EstablishedRisingSignificance.png")

##
## Output of Falling Term Results
##
# Get full-words in place of stemmed terms to make for prettier presentation
# replace NAs by the stemmed term (which is often the answer, dunno why stemCompletion doesn't do this)
#NB falling use different copus argument
falling.selected.words<-stemCompletion(names(fall.ratio),corp,type="shortest")#NB the default type is "prevalent"
falling.selected.words[is.na(falling.selected.words)]<-names(fall.ratio[is.na(falling.selected.words)])
#plot the rises as a bar chart with the most-rising coloured red/pink
falling.cols<-rf.palette[as.integer(fall.ratio)-rf.ratio.int.min+1]
colorized.barplot(fall.ratio, Main="Falling Terms", Ylab="% Fall in Target Set",
                  Names=falling.selected.words, Colours=falling.cols,
                  OutputFile="Images/FallingTerms_PC.png")
#plots showing previous/target occurrences for falling terms, sim to rising
f.term.sums.recent<-col_sums(dtm.tf[!past.doc_ids.bool,names(p.falling)])
f.term.sums.past<-col_sums(dtm.tf[past.doc_ids.bool,names(p.falling)])
pair.barplot(X.past=f.term.sums.past/past.recent.ratio, X.target=f.term.sums.recent,
                Main="Un-scaled Falling Terms", Ylab="Term Occurrence",
                Names=falling.selected.words,
                OutputFile="Images/FallingTerms_PastRecent_Counts.png")
pair.barplot(X.past=100*f.term.sums.past/tsp.all, X.target=100*f.term.sums.recent/tsr.all,
             Main="Falling Term Proportions", Ylab="Term Frequency (%)",
             Names=falling.selected.words, Beside=TRUE,
             OutputFile="Images/FallingTerms_PastRecent_PC.png")
#prep palette for "unlikelihood power"
logp.falling <- -log10(p.falling)
palette.index.falling <- as.integer(logp.falling + log10(thresh.pval.falling))*4 +1
palette.index.falling[palette.index.falling>20]<-20 #flatten off extreme peaks
up.falling.cols <- up.palette[palette.index.falling]
colorized.barplot(logp.falling, Main="Falling Terms", Ylab="Significance (-log10(p))",
                  Names=falling.selected.words, Colours=up.falling.cols,
                  OutputFile="Images/FallingP-Vals.png")
#scatter plot significance vs %fall
log.scatter(fall.ratio, logp.falling, Main="Falling Terms", Xlab="% Fall",
     Ylab="Significance (-log10(p))", OutputFile="Images/FallingSigPC_Scatter.png")

##
## If theme information is available, list the new and rising terms mentioned
##
in_themes.msg.new<-""
in_themes.msg.rising<-""
if(!is.na(recent.themes.txt)){
   # match returns a vector with elements containing either NA or an integer.
   # an integer i in position k indicates that themes.tf[k] has name = that of rising.selected[i]
   new.terms.in_themes <- na.exclude(match(names(themes.tf), names(term.sums.new.sel)))
   rising.terms.in_themes <- na.exclude(match(names(themes.tf), names(rising.selected)))
   if(length(new.terms.in_themes)>0){
      in_themes.msg.new<-paste("The following are matches between the conference theme description and above-threshold New Terms:",paste(as.character(new.sel.words[new.terms.in_themes]),collapse=", "))
   }else{
      in_themes.msg.new<-"No terms in the conference theme appear as above-threshold New Terms"
   }
   print(in_themes.msg.new)                                        
   if(length(rising.terms.in_themes)>0){
        in_themes.msg.rising<-paste("The following are matches between the conference theme description and above-threshold Rising Terms:", paste(as.character(rising.selected.words[rising.terms.in_themes]),collapse=", "))
   }else{
      in_themes.msg.rising<-"No terms in the conference theme appear as above-threshold Rising Terms"
   }
   print(in_themes.msg.rising)     
}

##
## Analyse the distribution of the rising terms between "recent" documents
##
#plots of number of docs containing the above-threshold terms
dtm.bin.rising<-weightBin(dtm.tf.rising)
cnt.docs_with_rising<-col_sums(dtm.bin.rising)
cnt.recent<-sum(!past.doc_ids.bool)
colorized.barplot(cnt.docs_with_rising, Main="Documents with Rising Terms",
                  Ylab="Number of Docs (Target)",
                  Names=rising.selected.words, Colours=rising.cols,
                  OutputFile="Images/RisingTerm_DocOcurrence_Bar.png")
# try a heatmap to visualise term-document correlations
basic.heatmap(as.matrix(dtm.tf.rising), Main="Rising Terms Among Documents",
                        ColumnLabels=rising.selected.words,
                        OutputFile="Images/RisingTerm_DocOcurrence_Heat.png")
#Scatter plot of the % rise vs the number of documents containing
# this isn't as good to look at as the previous colourised bar plot but is more quantitative
# plot(x=rising.selected, y=cnt.docs_with_rising, main="% Rise vs Documents Containing",
#      xlab="% Rise", ylab="Documents Containing Term",
#      ylim=c(1,max(cnt.docs_with_rising)+1), xlim=c(rise.pc.thresh,max(rising.selected)*1.2), pch=20)
# par(srt=45)
# text(x=rising.selected, y=cnt.docs_with_rising, rising.selected.words, adj=c(-0.2,0.8), offset=1, cex=0.6)
# par(srt=0)      
# png("Images/RisingTerm_DocOcurrence_Scatter.png", width=1000, height=1000,pointsize=12, res=150)
# plot(x=rising.selected, y=cnt.docs_with_rising, main="% Rise vs Documents Containing",
#      xlab="% Rise", ylab="Documents Containing Term",
#      ylim=c(1,max(cnt.docs_with_rising)+1), xlim=c(rise.pc.thresh,max(rising.selected)*1.2), pch=20)
# par(srt=45)
# text(x=rising.selected, y=cnt.docs_with_rising, rising.selected.words, adj=c(-0.2,0.8), offset=1, cex=0.6)
# par(srt=0)
# ad<-dev.off()

##
## Create separate small log files containing the sets of stemmed terms. (designed for use in HistoryVis)
##
#start logging all std output to a file in addition to "printing" to console

#log to separate files
LogTerms("NewTerms.log",names(p.new), new.sel.words)
LogTerms("RisingTerms.log",names(p.rising),rising.selected.words)
LogTerms("EstablishedTerms.log",names(p.established.rising), est.rising.selected.words)
LogTerms("FallingTerms.log",names(p.falling),falling.selected.words)
#re-instate the general logging
sink(file="RF_Terms.log", append=TRUE, type="output", split=TRUE)


##
## Gephi co-occurrence between a set of New and Rising Terms
##
terms.both<-c(names(term.sums.new.sel),names(rise.ratio))
labels.both<-c(new.sel.words,rising.selected.words)
weight.both<-c(-log10(p.new),as.numeric(-log10(p.rising)))
nodes1.df<-data.frame(Id=terms.both,Label=labels.both,Weight=weight.both)
edges1.df<-data.frame()
#start again from the full dtm otherwise we will not get cross-set co-occurrence showing (if used new and recent dtms)
dtm.bin.both<-dtm.bin.recent[,terms.both]
dtm.bin.both<-dtm.bin.both[row_sums(dtm.bin.both)>0]
#use the binary occurrence doc-term-matrix in plain matrix form for easy calcs
mat.bin<-as.matrix(dtm.bin.both)
#loop over terms, this loop is one end of each edge
for(t in 2:length(terms.both)){
   edge.weights<-colSums(mat.bin[,t]*mat.bin)
   edges1.df<-rbind(edges1.df,data.frame(
              Source=terms.both[t],
              Target=terms.both[1:(t-1)],#omits the self-referential edge and avoids double counting edges (A-B and B-A)
              Type="Undirected",Weight=edge.weights[1:(t-1)]))
}
#find the min/max co-occurrences for reporting
max.cooccurrence<-max(as.matrix(edges1.df["Weight"]))
#max.edges.df<-edges1.df[which(edges1.df["Weight"] == max(edges1.df["Weight"])),]
print("Significant (New and Rising) Term Co-occurrence Stats")
print(summary(edges1.df["Weight"]))
#remove cases where there is no co-occurrence
edges1.df<-subset(edges1.df,Weight>0)
#save to disk. we do not want row names since these end up being scrubbed on import to gephi
write.csv(nodes1.df, file="Gephi/SignificantTerm-Co-occurence Nodes.csv", row.names=FALSE)
write.csv(edges1.df, file="Gephi/SignificantTerm-Co-occurence Edges.csv", row.names=FALSE)
## **** notes on importing into Gephi (v0.8 alpha used)
# import nodes then edges from CSV files. Make Node Weight be a float [it is ESSENTIAL not to leave it as a String]
# show node labels, use "statistics" to calculate modularity
# Use "ranking" to set node size = imported weight 
# edge size = imported weight (=number of co-occurrences). by default
# Use "partition" to set node colour = modularity class
# NB the actual scale may need a multiplier/factor to be applied.
# Use a circular auto-layout with nodes ordered by modularity class then use Frucherman Reingold
# - may need to do a label adjust too.
# ** for the "preview"
# - set edge thickness to 5 and label font to 36pt. curved edges sometimes work OK
# - set the opacity to somewhere between 60 and 80% so that labels show up better
# - uncheck "proportional size" on node and edge
# - when exporting to PNG, set a 25% margin otherwise it gets cropped!


# ##
# ## gephi output of term associations for NEW terms
# ##
# #node=term, weighting=freq in new set
# #edge=co-occurrence in a documet, weighting = number of docs in which the terms co-occur (not weighted by freq!)
# nodes1.df<-data.frame(Id=names(term.sums.new.sel),Label=new.sel.words,Weight=as.numeric(term.sums.new.sel))
# edges1.df<-data.frame()
# #use the binary occurrence doc-term-matrix in plain matrix form for easy calcs
# mat.bin<-as.matrix(dtm.bin.new)
# #loop over terms, this loop is one end of each edge
# for(t in 2:length(term.sums.new.sel)){
#    edge.weights<-colSums(mat.bin[,t]*mat.bin)
#    edges1.df<-rbind(edges1.df,data.frame(
#               Source=names(term.sums.new.sel)[t],
#               Target=names(term.sums.new.sel)[1:(t-1)],#omits the self-referential edge and avoids double counting edges (A-B and B-A)
#               Type="Undirected",Weight=edge.weights[1:(t-1)]))
# }
# #find the min/max co-occurrences for reporting
# min.cooccurrence<-min(edges1.df["Weight"])
# max.cooccurrence<-max(edges1.df["Weight"])
# mean.cooccurrence<-mean(edges1.df["Weight"])
# max.edges.df<-edges1.df[which(edges1.df["Weight"] == max(edges1.df["Weight"])),]
# print("New Term Co-occurrence Stats")
# print(summary(edges1.df["Weight"]))
# #remove cases where there is no co-occurrence
# edges1.df<-subset(edges1.df,Weight>0)
# #save to disk
# write.csv(nodes1.df, file="Gephi/NewTerm-Co-occurence Nodes.csv")
# write.csv(edges1.df, file="Gephi/NewTerm-Co-occurence Edges.csv")
#          
# 
# 
# ##
# ## gephi output of term associations for above-threshold rising terms
# ##
# #node=term, weighting=rising ratio
# #edge=co-occurrence in a documet, weighting = number of docs in which the terms co-occur (not weighted by freq!)
# nodes1.df<-data.frame(Id=names(rise.ratio),Label=rising.selected.words,Weight=as.numeric(rise.ratio))
# edges1.df<-data.frame()
# #use the binary occurrence doc-term-matrix in plain matrix form for easy calcs
# mat.bin<-as.matrix(dtm.bin.rising)
# #loop over terms, this loop is one end of each edge
# for(t in 2:length(rise.ratio)){
#    edge.weights<-colSums(mat.bin[,t]*mat.bin)
#    edges1.df<-rbind(edges1.df,data.frame(
#               Source=names(rise.ratio)[t],
#               Target=names(rise.ratio)[1:(t-1)],#omits the self-referential edge and avoids double counting edges (A-B and B-A)
#               Type="Undirected",Weight=edge.weights[1:(t-1)]))
# }
# #find the min/max co-occurrences for reporting
# min.cooccurrence<-min(edges1.df["Weight"])
# max.cooccurrence<-max(edges1.df["Weight"])
# mean.cooccurrence<-mean(edges1.df["Weight"])
# max.edges.df<-edges1.df[which(edges1.df["Weight"] == max(edges1.df["Weight"])),]
# print("Rising Term Co-occurrence Stats")
# print(summary(edges1.df["Weight"]))
# #remove cases where there is no co-occurrence (oddly this seems quite rare)
# edges1.df<-subset(edges1.df,Weight>0)
# #save to disk
# write.csv(nodes1.df, file="Gephi/RisingTerm-Co-occurence Nodes.csv")
# write.csv(edges1.df, file="Gephi/RisingTerm-Co-occurence Edges.csv")
# ## **** notes on importing into Gephi (v0.8 alpha used)
# # import nodes then edges from CSV files. Make Node Weight be a float and de-select the "*" columns
# # show node labels, use "statistics" to calculate an unweighted degree
# # Use "ranking" to set node and edge size/colour
# # - node size = imported weight (=%rise). Scale the size proportional to the sqrt(% rise)
# # - node label size = same treatment as node size
# # - node colour = degree. Set the colour range from #FF5000 to #00D620
# # - edge size = imported weight (=number of co-occurrences). Scale proportional to the weight
# # NB the actual scale may need a multiplier/factor to be applied.
# # Use a circular auto-layout, with "no overlap" and with nodes ordered by degree
# # - may need to do a label adjust too.
# # - generally apply one step of expansion x1.2

##       
## find docs containing the above-threshold new terms, sorted most-numerous first
##
#new.selected.term.ids.asc<-order(term.sums.new.sel, decreasing=TRUE)
#dtm.bin.new<-dtm.bin.new[,names(term.sums.new.sel[new.selected.term.ids.asc])]
print("Documents for New Terms (alphabetical) - see RF_Terms.log")
# re-jig the sink to only print this stuff to file
sink()
sink(file="RF_Terms.log", append=TRUE, type="output", split=FALSE)
#data frames are used to store this stuff in new.doclist for simple use using Brew for report creation
new.doclist<-NULL
ii<-1
for (i in Terms(dtm.bin.new)){
   print(paste("Documents containing term:",i)) #########################
   doc_ids.for.term<-Docs(dtm.bin.new[row_sums(dtm.bin.new[,i])>0,])
   corp.for.term<-corp.new[doc_ids.for.term]
   new.doclist[[ii]]<-ExtractDocs(corp.for.term, ExtraMeta=extra.meta, DocIds=doc_ids.for.term)
   ii<-ii+1
   print("============")
}
names(new.doclist)<-new.sel.words
# re-jig back to showing on console and logging
sink()
sink(file="RF_Terms.log", append=TRUE, type="output", split=TRUE)
        
##       
## find docs containing the above-threshold rising terms, sorted most-rising first
##
# improve this - find URL for each 
#rising.selected.term.ids.asc<-order(rise.ratio, decreasing=TRUE)
#dtm.bin.rising<-dtm.bin.rising[,rising.selected.term.ids.asc]
print("Statistics for the number of different above-threshold rising terms in each Target doc with at least one such term")
print(dtm.bin.rising)
summary(cnt.docs_with_rising)
print("Documents for Rising Terms (alphabetical) - see RF_Terms.log")
# re-jig the sink to only print this stuff to file
sink()
sink(file="RF_Terms.log", append=TRUE, type="output", split=FALSE)
#data frames are used to store this stuff in rising.doclist for simple use using Brew for report creation
rising.doclist<-NULL
ii<-1
for (i in Terms(dtm.bin.rising)){
   print(paste("Documents containing term:",i))
   doc_ids.for.term<-Docs(dtm.bin.rising[row_sums(dtm.bin.rising[,i])>0,])
   corp.for.term<-corp.rising[doc_ids.for.term]
   rising.doclist[[ii]]<-ExtractDocs(corp.for.term, ExtraMeta=extra.meta, DocIds=doc_ids.for.term)
   ii<-ii+1
   print("============")
}
names(rising.doclist)<-rising.selected.words
  
# re-jig back to showing on console and logging
sink()
sink(file="RF_Terms.log", append=TRUE, type="output", split=TRUE)
     
##
## Documents containing extreme or outlier "auxillary metrics" - novelty or subjectivity
##
#novelty
df.std.nov.extreme<-ExtractDocs(corp,  ExtraMeta=extra.meta, DocIds=names(std.nov.extreme))
#Subjectivity
df.subj.outliers<-ExtractDocs(corp, ExtraMeta=extra.meta, DocIds=names(subj.outliers))

##
## find docs containing several different above-threshold rising terms. 
##
dtm.bin.key_docs<-dtm.bin.rising[row_sums(dtm.bin.rising)>quantile(row_sums(dtm.bin.rising),probs=0.75),]
print("How many documents are above the thrid quartile for (different) rising term count")
print(dtm.bin.key_docs)
#Inspect these "Key docs"
key_doc.ids<-Docs(dtm.bin.key_docs)
key_doc.ids<-key_doc.ids[order(row_sums(dtm.bin.key_docs),decreasing=TRUE)]#re-order to put the doc with the most diff rising terms first
corp.key_docs<-corp[key_doc.ids]
cat("\n")
print("Key Documents - Contain Several Rising Terms - see RF_Terms.log")
sink()
sink(file="RF_Terms.log", append=TRUE, type="output", split=FALSE)
#give metadata, abstract and list the above-thresh rising terms in each one.
i<-0
for (j in corp.key_docs){
   doc.date<- as.character(meta(j, tag="DateTimeStamp"))
   doc.heading <- as.character(meta(j, tag="Heading"))
   doc.authors <- as.character(meta(j, tag="Author"))
   doc.id <- as.character(meta(j, tag="ID"))
   doc.abstract <- as.character(j)
   print("")
   print(doc.heading)
   print(paste(doc.date,", ",doc.authors,", ", "ID=", doc.id, sep=""))
   print(doc.abstract)
   i<-i+1
   print(paste("Contains Terms:", paste( Terms(dtm.bin.rising[key_doc.ids[i],col_sums(dtm.bin.rising[key_doc.ids[i],])>0]),collapse=", ") ,sep=" "))
}   
        
#dump a CSV of the recent docs, with lists of terms and ancillary measures
#DO not include the abstracts so that it may be published with less risk of IPR issues
df.dump<-ExtractDocs(corp, ExtraMeta=extra.meta, DocIds=Docs(dtm.tf)[!past.doc_ids.bool], Print=FALSE)
df.dump["abstract"]<-NULL
rownames(df.dump)<-df.dump[["id"]]#make rownames be the doc ids
#this cannot be done with sapply() since col_sums cannot currently cope with repeated indeces
Doc_ids<-Docs(dtm.tf.new)
for(i in Doc_ids){
   df.dump[i,"new.terms"]<- paste(Terms(dtm.tf.new[i,col_sums(dtm.tf.new[i,])>0]),collapse=",")
}
Doc_ids<-Docs(dtm.tf.rising)
for(i in Doc_ids){
   df.dump[i,"rising.terms"]<- paste(Terms(dtm.tf.rising[i,col_sums(dtm.tf.rising[i,])>0]),collapse=",")
}
Doc_ids<-Docs(dtm.tf.est)
for(i in Doc_ids){
   df.dump[i,"established.terms"]<- paste(Terms(dtm.tf.est[i,col_sums(dtm.tf.est[i,])>0]),collapse=",")
}
write.csv(df.dump, "Raw Results.csv", quote=TRUE, row.names=FALSE)
rm(df.dump)
        
#stop logging
timestamp(stamp=date(),prefix="##TIMESTAMP: ")
sink()
   
## -----
## save the whole workspace
## -----
save.image(file="RF_Terms.RData")
