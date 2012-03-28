##
## Score documents according to the occurrence of N words with 
## variable penalty for gaps, and options for permuation and synonum use
## (also with variable penalty)
##

library("stringkernels")#also loads kernlab and openNLP
#NB: this needs e.g. openNLPmodels.en package installed too
library("combinat")
library("wordnet")
library("tm") 
#needed to allow for OpenNLP to have a large heap space
options(java.parameters="-Xmx2g")

##======================
## LOAD IN THE DOCUMENTS
##======================
#Source type and corpus control
source.type<-"corpus.RData"
RData.source<-"/home/arc1/R Projects/Text Mining Weak Signals Output/Rising and Falling Terms/Union B/2010/Corpus.RData"
#only use documents with a DateStamp in the following range
start.date=as.POSIXlt("2010-01-01")
last.date=as.POSIXlt("2010-12-31")
# using a tm package corpus is not strictly necessary but it allows cross-over to other text mining code, allows for metadata etc...
# for now, a dummy corpus....
if(source.type=="corpus.RData"){
   load(RData.source)
   dts<-as.POSIXlt(unlist(meta(corp,"DateTimeStamp",type="local")), origin="1970-01-01")
   filter.bool<- (dts>start.date) & (dts<=last.date)
   corp<-corp[filter.bool]
   print(paste("Filtering in date range",start.date,"to",last.date, "=>"))
   print(corp)
}else{
   #a dummy tesr case is fall-back
   corp<-Corpus(VectorSource(c("The Cat sat on the mat","The Cat sat on the mat and figged about with a mouse","The cat sat on the carpet","The cat sat. On the mat was a dog", "On the mat there was a cat","On the mat a cat was sat","On the mat was a picture of a cat","A matte cat sat on the grass")))
   print("Using test case")
}

##================
## RUN PARAMETERS
##================
# The query is considered ordered by default.
query<-"learning design"
#this is a normal calculation, normally TRUE but maybe useful to set to F when using do.pairs and a longer query to investigate proximity of a cluster of terms.
#NB: all words in the query must appear in a document for a score of >0 to be calculated
do.fullQuery<-T

# option to select pairs of distinct words from the query
# and re-run each is if it had been the query, so building up a proximity matrix
do.pairs<-F

#gap penalty parameter = lambda for "gapweightkernel" in package stringkernels
#a match is scored as lambda^number_of_gaps, hence a value of 1 does NOT penalise gaps
gap.lambda<-0.75

#when using synonyms or permutations, documents are selected based on the highest score of any variant

#option to permute the words in the query. Take care if the query is >3 words; the number of perms explodes!
#NB: when do.pairs=T and the pair proximity is calculated, an equal-weighted perm is automatically done
#(i.e. the pair is considered un-ordered). This does not alter the way the normal calc is performed.
do.permute<-F
#factor to apply to gapweightkernel scores when the permutation is not the original query
#1 gives all permutations equal weight. 0 only counts the original query
perm.factor<-0.75 

#option to use synonyms.
#     NB1: only 1 word is synonymised at a time,
#     NB2: synonyms applied in addition to permutation if do.permute==T
do.synonyms<-F
#factor to apply to gapweightkernel scores for each synonym substitution
#1 applies no penalty
synonym.factor<-0.8
# if do-synonyms==T then the following should match the length (in words) of "query" and the word order in it.
#NA or any control word not specified below means no synonyms for the corresponding word will be used. (no warning given)
#the permitted values are
#   "MANUAL" - means the synonyms are manually specified in the named list, "synonym.manual"
#   "NOUN","VERB","ADJECTIVE","ADVERB" - WordNet synonyms are found of the specified part of speech
#   "" - means no synonyms are to be used for the corresponding word
#     !! use WordNet with care. preferably check what it will give you before using. also NB: "sat" is not considered a VERB
synonym.control<-c(NA,"","MANUAL")
synonym.manual<-list(recommender=c("recommend"))

#show documents with scores >= this quantile. NB only docs with non-zero score are used to determine the quantile.
score.quantile<-0.8

#set to true for verbose information
debug=FALSE

##=====================
##PREP SYNONYMS
##=====================
syn.list<-list()
if(do.synonyms){
   gotWordNet <- initDict()
   #loop over all synonym control, building up a list where each named entry
   # is a vector of syn-terms of length >=0
   for(i in 1:length(synonym.control)){
      sc1<-synonym.control[i]
      if(is.na(sc1)){
         #skip
      }else if(sc1=="MANUAL"){
         syn.list=c(syn.list,synonym.manual)
      }else if(sc1=="NOUN" || sc1=="VERB" || sc1=="ADVERB" || sc1=="ADJECTIVE"){
         if(!gotWordNet) stop(paste("Attempt to use synonym control",sc1,"but cannot find WordNet"))
         word<-tolower(q.split[i])
         word.syns<-list(synonyms(word,sc1))
         names(word.syns)<-word
         syn.list=c(syn.list,word.syns)
      }
   }
   print("Enumerations of Synonyms for Each Query Term")
   if(length(syn.list)>0){
      print(syn.list)
   }else{
      print("No synonyms specified or found in spite of do.synonyms==TRUE")
   }
}





##==================
## WORKER FUNCTION
##==================
proximateWorker<-function(work.query, print.results=FALSE){
   #allowing for work.query to be a subquery in future code
   #separate the words - used for length and possibly permutations too
   q.split<-strsplit(work.query," ")[[1]]
   
   # expand the query according to permutations and synonyms, calculating weightings for these
   if(do.permute){
      #creates all word permutations in the query
      q.permutes <- sapply(permn(q.split),paste,collapse=" ")
      expanded.query<-data.frame(q=q.permutes, w=rep(perm.factor,length(q.permutes)))
      #sets the original query weight to 1.0
      expanded.query[q.permutes==work.query,"w"]<-1.0
   }else{
      #use only the original query
      expanded.query<-data.frame(q=work.query, w=1.0)#q=query, w= weighting
   }
   if(length(syn.list)>0){
      #for each of the existing entries in the expanded query, add new expanded queries with appropriate
      #weighting for synonymisation of one word at a time. skip if the synonym == the original word
      df<-expanded.query
      #ripple through the full synonym list
      root.words<-names(syn.list)
      for(j in 1:length(root.words)){#the word to be replaced  
         #sub("cat","mog",expanded.query[,"q"],ignore.case=TRUE)
         root.word<-root.words[j]
         syn.words<-unlist(syn.list[root.word])
         if(length(syn.words)>0){
            for(k in 1:length(syn.words)){#loop over the synonym to replace the j'th word
               rep.word<-tolower(syn.words[k])
               if(rep.word!=root.word){
                  new.queries<-sub(root.word,rep.word,df[,"q"],ignore.case=TRUE)
                  new.weights<-synonym.factor*df[,"w"]
                  expanded.query<-rbind(expanded.query,data.frame(q=new.queries,w=new.weights))
               }
            }
         }
      }
   }
   
   #calculate un-normalised gap weighted scores for the match between the (expanded) query and all documents in the corpus. Using un-normalised avoids penalising an exact match for the query inside a longer document.
   gwk = gapweightkernel(length=length(q.split),lambda=gap.lambda,normalized=F)
   scores.mat <- kernelMatrix(gwk, as.list(expanded.query[,"q"]), as.list(tolower(corp)))
   rownames(scores.mat)<-expanded.query[,"q"]
   
   # apply the down-weighting for permutations or synonyms
   scores.weighted<-scores.mat * expanded.query[,"w"]
   
   if(debug){
      print("Expanded Queries and Weights")
      print(expanded.query)
      
      print("Expanded Query-Document Un-Weighted Score Matrix")
      print(scores.mat)
      
      print("Expanded Query-Document Weighted Score Matrix")
      print(scores.weighted)
   }
   
   # use the maximum kernel function value from the expanded queries to get the best score for each document
   scores.max <-apply(scores.weighted,MARGIN=2,max)
   
   #it is convenient to deal with only docs with non-zero score
   nz.scores.max <- scores.max[scores.max>0]
   if(length(nz.scores.max)>0){
      # cutoff score for showing documents
      score.thresh<-quantile(nz.scores.max,score.quantile)
      
      #select and order the above-threshold documents
      docs.sel.bool<-scores.max>=score.thresh
      scores.sel<-scores.max[docs.sel.bool]
      corp.sel<-corp[docs.sel.bool]
      corp.sel<-corp.sel[order(scores.sel,decreasing=TRUE)]
      scores.sel<-scores.sel[order(scores.sel,decreasing=TRUE)]
      
      if(print.results){  
         print("Best Weighted Score Per Document")
         print(scores.max)
         
         summary(scores.max)
         hist(scores.max, breaks=50)
         
         print("Following statistics are *after* suppressing zero-scoring documents")
         summary(nz.scores.max)
         hist(nz.scores.max, breaks=50)
         
         print(paste("Documents with score >= the",score.quantile*100,"% quantile"))
         for(i in 1:length(scores.sel)){
            print(paste("score=",scores.sel[i]))
            print(as.character(corp.sel[i]))
         }
      }
   }else{
      print("All documents scored 0")
   }
   return(scores.max)
}
## -------------- end function def

##==================
## CALCULATION
##==================
## >>>>>>>>> if necessary, do pairwise word combinations
if(do.pairs){
   #temp set permutation on and without penalty
   do.permute.save<-do.permute
   perm.factor.save<-perm.factor
   do.permute<-T
   perm.factor<-1.0
   #prepare the pairs
   split.query<-strsplit(query," ")[[1]];
   q.pairs<-combn2(split.query)
   #prepare objects to receive results
   df.pairs<-data.frame()#catches the vectors of scores from proximateWorker
   pair.mat<-matrix(nrow = length(split.query), ncol = length(split.query))#for the mean of the proximateWorker score of each pair
   rownames(pair.mat)<-split.query
   colnames(pair.mat)<-split.query
   #diag(pair.mat)<-NA
   for(p in 1:length(q.pairs[,1])){#loop over rows is a loop over word pairs
      pair<-paste(q.pairs[p,], collapse=" ")
      pair.scores.max<-proximateWorker(pair)
      pair.summary<-summary(pair.scores.max)
      df.pairs<-rbind(df.pairs,data.frame(row.names=pair,as.list(pair.summary)))
      pair.mat[q.pairs[p,1],q.pairs[p,2]]<-pair.summary["Mean"]
      pair.mat[q.pairs[p,2],q.pairs[p,1]]<-pair.summary["Mean"]
      print(paste("For pair [", pair,"] summary document score ="),sep="")
      print(pair.summary)
   }
   
   barplot(as.matrix(df.pairs)[,3], las=2,main="Median of Proximity Score")
   barplot(as.matrix(df.pairs)[,4], las=2,main="Mean of Proximity Score")
   barplot(as.matrix(df.pairs)[,5], las=2,main="3rd Quartile of Proximity Score")
   barplot(as.matrix(df.pairs)[,6], las=2,main="Max of Proximity Score")
   
   heatmap(pair.mat, main="Un-ordered Word Proximity Map")
   
   #restore the params for a full/normal run
   do.permute<-do.permute.save
   perm.factor<-perm.factor.save
}

## >>>>>>>>> this is the full query run
if(do.fullQuery){
   scores.max<-proximateWorker(query, print.results=TRUE)
}

