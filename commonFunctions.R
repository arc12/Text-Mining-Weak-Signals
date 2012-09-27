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
## common functions for RF_Terms. No attempt is made to make these generic utility functions (a job for a rainy day or another life)
##

# Given a corpus, extra metadata and a list of document IDs, extract document metadata and content into a dataframe
# and optionally print out the extracted info
#ExtraMeta is a data frame with Doc IDs as row names, columns for max betweenness and Std Novelty
ExtractDocs<-function(Corp, ExtraMeta, DocIds, DateLength=4, Print=TRUE){
   empty.field.c<-rep(NA,length(DocIds))
   df<-data.frame(origin=empty.field.c, date=empty.field.c,
         heading=empty.field.c, authors=empty.field.c, id=empty.field.c,
         url=empty.field.c, dblp_url=empty.field.c,
         abstract=empty.field.c, std.novelty=empty.field.c, positive=empty.field.c,
         negative=empty.field.c, subjectivity=empty.field.c, max.betweenness=empty.field.c,
         stringsAsFactors=FALSE)
   jj<-1
   for (j in DocIds){
      #BEWARE order is critical!
      df[jj,]<-c(as.character(meta(Corp[[j]], tag="Origin")),
         substr(as.character(meta(Corp[[j]], tag="DateTimeStamp")),1,DateLength),
         as.character(meta(Corp[[j]], tag="Heading")),
         as.character(meta(Corp[[j]], tag="Author")), as.character(meta(Corp[[j]], tag="ID")),
         as.character(meta(Corp[[j]], tag="URL")), as.character(meta(Corp[[j]], tag="DBLP_URL")),
         as.character(Corp[[j]]),
         as.numeric(ExtraMeta[j,"StdNovelty"]),
         as.numeric(meta(Corp[[j]], tag="Positive")),
         as.numeric(meta(Corp[[j]], tag="Negative")),
         as.numeric(meta(Corp[[j]], tag="Subjectivity")),
         as.numeric(ExtraMeta[j,"MaxBetweenness"]))
      if(Print){
         print("")
         print(df[jj,"heading"])
         print(paste("Metrics: std. novelty=", df[jj,"std.novelty"],
              "  + sentiment=", df[jj,"positive"],
              "  - sentiment=", df[jj,"negative"],
              "  subjectivity=", df[jj,"subjectivity"],
              "  max betweenness=",df[jj,"max.betweenness"],sep=""))
         print(paste(df[jj,"origin"],df[jj,"date"],", ",df[jj,"authors"],", ", "ID=", df[jj,"id"], sep=""))
         print(df[jj,"abstract"])
      }
      jj<-jj+1
   }

   return(df)
}

LogTerms<-function(fileName, terms, words=NULL){
   #first clean up old sinks
   while(sink.number()>0)
      {sink()}
   sink(file=fileName, append=FALSE, type="output", split=TRUE)
   cat(paste("c(\"",paste(terms,collapse="\",\""),"\")",sep=""))
   cat("\n")
   if(!is.null(words)){
      cat(paste("c(\"",paste(words,collapse="\",\""),"\")",sep=""))
   }
   
   while(sink.number()>0)
      {sink()}
}

CustomStopwords<-function(){
   #+  "paper" (which is common in journal/proceedings abstracts!)
   SW<-c(stopwords(kind = "en"),"paper","studentspsila","conference",
         "january","february","march","april","may","june",
         "july","august","september","october","november","december",
         "jan","feb","mar","apr","jun","jul","aug","sept","oct","nov","dec")
   #- some terms (and various expansions) that are relevant to the education domain
   SW<-SW[-grep("group", SW)]
   SW<-SW[-grep("problem", SW)]
   SW<-SW[-grep("present", SW)]
   SW<-SW[-grep("work", SW)]
   return(SW)
}

##
## Apply Pearson Chi^2 test to term distribution in the documents of 2 corpora to identify
## statistically-significant rising, falling, new terms in a "recent" set vs a "past" set
## [derived from RF_Terms.R]
## A list is returned for New/Rising/Established/Falling.
## NB the DTM in $Falling is of the docs in the pastIds set whereas the other DTMs are for the recentIds
##

# how many documents must the term appear in to be listed. This is in addition to the frequency thresholds. A value of 2 is expected, i.e. ignore terms that appear in only one doc
#doc_count.thresh <- 2
# p-value to accept the "alternative hypothesis" that there is something interesting
#thresh.pval<-0.005 #i.e. accept a .5% chance that null hypothesis falsely rejected
#thresh.pval.falling<-0.01 #use a more lenient threshold for falling terms
#max frequency of term in the past set for eligibility as a weak signal.
#Above this, sigifnicant risers are "established terms"
#max.past.freq<-0.0002


PearsonChanges.Corpus<-function(corpus,
                                pastIds, recentIds,
                                doc_count.thresh = 2,
                                thresh.pval = 0.005,
                                thresh.pval.falling = 0.01,
                                max.past.freq = 0.0002,
                                stem  = TRUE, stop.words = TRUE){
   #sanity checks
   if(length(intersect(pastIds,recentIds))>0){
      stop("Error pastIds contains at least one Id also in recentIds.")
   }
   
   #libraries
   require("tm")
   require("Snowball")
   require("slam")
   require("corpora")
   
   ##
   ## process the corpus to create a doc-term matrix using default or passed parameters
   ##
   corpus<-tm_map(corpus,removeNumbers)
   corpus<-tm_map(corpus,removePunctuation)
   dtm.tf<-DocumentTermMatrix(corpus, control=list(stemming=stem, stopwords=stop.words, minWordLength=3))
   #finally trim the DTM so that it only contains docs that we need and only terms in the needed docs
   dtm.tf<-dtm.tf[c(pastIds, recentIds),]
   dtm.tf<-dtm.tf[,col_sums(dtm.tf)>0]
      
   ##
   ## segment the DTM according to the two sets of document ids passed in.
   ##
   # it is helpful to retain the same Terms in each DTM for when comparisons are made
   dtm.tf.past<-dtm.tf[pastIds,]
   dtm.tf.recent<-dtm.tf[recentIds,]
   
   ##
   ##aggregate statistics before eliminating any terms according to doc_count thresh
   ##
   term.sums.past<-col_sums(dtm.tf.past)
   tsp.all<-sum(term.sums.past)
   term.sums.recent<-col_sums(dtm.tf.recent)
   tsr.all<-sum(term.sums.recent)
   
   ##
   ## Make sure that there are at least doc_count.thresh docs containing any given term
   ##
   dtm.bin<-weightBin(dtm.tf)
   dtm.tf.past<-dtm.tf.past[,col_sums(dtm.bin)>=doc_count.thresh]
   dtm.tf.recent<-dtm.tf.recent[,col_sums(dtm.bin)>=doc_count.thresh]
   
   ##
   ## GET some boolean filters for three groups, Rising, Falling and New. No decision on significance yet!
   ##
   #the number of docs in the new set containing >=1 occurrence of the term 
   dtm.bin.recent<-dtm.bin[recentIds,]
   terms.doc.cnt.recent<-col_sums(dtm.bin.recent)
   #ONLY select those with a mininum number of document occurrences doc_count.thresh (except "falling")
   #term sums of 0 in the past must be new terms, since we know the corpus sum>0
   new.term_ids.bool <- (term.sums.past==0) & (terms.doc.cnt.recent>=doc_count.thresh)
   #which terms should be considered in rising/falling?
   rise.term_ids.bool <- (term.sums.recent/tsr.all>term.sums.past/tsp.all) &
      (term.sums.past>0) & (terms.doc.cnt.recent>=doc_count.thresh)
   fall.term_ids.bool <- (term.sums.recent/tsr.all<term.sums.past/tsp.all)
   
   ##
   ## compute the term occurrences in each set (past/recent) and group (rising/falling/new)
   ##
   term.sums.new<-term.sums.recent[new.term_ids.bool]#the "past" is 0 of course
   term.sums.past.rising<-term.sums.past[rise.term_ids.bool]
   term.sums.recent.rising<-term.sums.recent[rise.term_ids.bool]
   term.sums.past.falling<-term.sums.past[fall.term_ids.bool]
   term.sums.recent.falling<-term.sums.recent[fall.term_ids.bool]
   #calculate a rise/fall factor excluding any new terms
   #basically: (recent fraction - past fraction)/(past fraction) but rescaled to a % to give "nicer" numbers for display
   rise.ratio<-(term.sums.recent.rising*tsp.all/(tsr.all*term.sums.past.rising) -1)* 100
   fall.ratio<-(term.sums.recent.falling*tsp.all/(tsr.all*term.sums.past.falling) -1)* 100
      
   ##
   ## Apply Pearson's Chi^2 Test to each group, then filter down as appropriate
   ##
   p.rising<-chisq.pval(term.sums.past.rising,tsp.all ,term.sums.recent.rising, tsr.all)
   p.falling<-chisq.pval(term.sums.past.falling,tsp.all ,term.sums.recent.falling, tsr.all)
   p.new<-chisq.pval(0.0, tsp.all, term.sums.new, tsr.all)
   names(p.rising)<-names(term.sums.past.rising)
   names(p.falling)<-names(term.sums.past.falling)
   names(p.new)<-names(term.sums.new)   
   #which are significant is defined by the thresholds
   sig.new.bool<-p.new<=thresh.pval
   sig.rising.bool<-p.rising<=thresh.pval
   sig.falling.bool<-p.falling<=thresh.pval.falling
   #filter down the vectors of the important measures
   rise.ratio<-rise.ratio[sig.rising.bool]
   fall.ratio<-fall.ratio[sig.falling.bool]
   term.sums.new<-term.sums.new[sig.new.bool]
   #similarly finally truncate the p-value vectors now we have finished using them as filters
   p.rising<-p.rising[sig.rising.bool]
   p.falling<-p.falling[sig.falling.bool]# NB different threshold
   p.new<-p.new[sig.new.bool]
   # remove "established terms" from the rising set and hive them off to a separate list
   established.term_ids.bool<-(term.sums.past.rising[sig.rising.bool]/tsp.all)>max.past.freq
   p.established.rising <- p.rising[established.term_ids.bool]
   p.rising <- p.rising[!established.term_ids.bool]
   established.rise.ratio<-rise.ratio[established.term_ids.bool]
   rise.ratio<-rise.ratio[!established.term_ids.bool]
   

   ##
   ## Which documents in the corpus contain terms in the relevant new/rising/falling/established sets
   ## Lists are convenient structures for returning values
   ##

   if(is.na(p.new[1])){
      l.new<-NULL
   }else{
      dtm.tf.new<-dtm.tf.recent[,names(p.new)]
      dtm.tf.new<-dtm.tf.new[row_sums(dtm.tf.new)>0]
      newIds<-Docs(dtm.tf.new)
      l.new<-list(DTM = dtm.tf.new,
                  Frequency = term.sums.new,
                  P = p.new)
   }
   if(is.na(p.rising[1])){
      rising<-NULL
   }else{
      dtm.tf.rising<-dtm.tf.recent[,names(p.rising)]
      dtm.tf.rising<-dtm.tf.rising[row_sums(dtm.tf.rising)>0]
      risingIds<-Docs(dtm.tf.rising)
      rising<-list(DTM = dtm.tf.rising,
                   Frequency = col_sums(dtm.tf.rising)/tsr.all,
                   BaselineFrequency = col_sums(dtm.tf.past[,names(p.rising)])/tsp.all,
                   Change = rise.ratio,
                   P = p.rising)
   }
   if(is.na(p.established.rising[1])){
      established<-NULL
   }else{
      dtm.tf.established<-dtm.tf.recent[,names(p.established.rising)]
      dtm.tf.established<-dtm.tf.established[row_sums(dtm.tf.established)>0]
      establishedIds<-Docs(dtm.tf.established)
      established<-list(DTM = dtm.tf.established,
                        Frequency = col_sums(dtm.tf.established)/tsr.all,
                        BaselineFrequency = col_sums(dtm.tf.past[,names(p.established.rising)])/tsp.all,
                        Change = established.rise.ratio,
                        P = p.established.rising)
   }
   if(is.na(p.falling[1])){
      falling<-NULL   
   }else{
   dtm.tf.falling<-dtm.tf.past[,names(p.falling)]
   dtm.tf.falling<-dtm.tf.falling[row_sums(dtm.tf.falling)>0]
   fallingIds<-Docs(dtm.tf.falling)
   falling<-list(DTM = dtm.tf.falling,
                 Frequency = col_sums(dtm.tf.recent[,names(p.falling)])/tsr.all,
                 BaselineFrequency = col_sums(dtm.tf.falling)/tsp.all,
                 Change = fall.ratio,
                 P = p.falling)
   }  
   
   #combine into a single list for the return "class"
   pearsonChanges<-list(DTM.tf=dtm.tf, New=l.new, Established=established, Rising=rising, Falling=falling)
   class(pearsonChanges)<-"PearsonChanges"
   
   pearsonChanges #return value
   }