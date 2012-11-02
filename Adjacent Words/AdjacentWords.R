##
## find which words preceed and follow a given word(words) and create a directed graph to visualise this pattern, showing frequency by node weight

library("igraph")
library("RSQLite")

## TO DO refactor to use database queries for year-splitting?????

if(profile){
   Rprof()
}
# filesystem setup
base.dir<-"/home/arc1/R Projects/Text Mining Weak Signals"
source.dir<-paste(base.dir,"Source Data",sep="/")
output.dir<-paste("/home/arc1/R Projects/Text Mining Weak Signals Output/Adjacent Words",set.name,sep="/")
#brew.dir<-paste(base.dir,"AdjacentWords",sep="/")
dir.create(output.dir, showWarnings=FALSE)
setwd(output.dir)

## Preliminaries to get the run parameters ready
if(source.type=="a"){
   ## ADJUST THESE to get "nice" results (otherwise may be over-sparse, "no useful results" or over-dense)
   #the quantile to be applied to adjacent word frequency when visualising (only cases above the cut appear)
   cut.quantile<-0.95
   cut.quantile.split<-0.9 #used for a split-off year. should be smaller than cut.quantile otherwise they are sparse
   title<-"Conference Proceedings from ICALT, ECTEL, CAL, ICHL and ICWL"
   short.title<-"Conferences"
   filename.postfix<-"conf"
}else if(source.type=="b"){
   cut.quantile<-0.99
   cut.quantile.split<-0.95
   title<-"TEL Blogs Jan 2009-"
   short.title<-"TEL Blogs"
   filename.postfix<-"blog"
}else{
   stop(paste("Unknown Source Type:",source.type))
}
if(is.na(focus.placeholder)){
   focus.placeholder<-focus.words[1]
}
   
## SET UP DATABASE and SQL according to source type
sqlite.filename<-"TMWS Data A.sqlite"
use.sqlite<-!is.na(sqlite.filename)
if(use.sqlite){
   # instantiate the SQLite driver in the R process
   sqlite<- dbDriver("SQLite")
   # open sqlite connection. db is a "connection"
   db<- dbConnect(sqlite, dbname=paste(source.dir,sqlite.filename,sep="/"))
   summary(db)
}
#must not have a "-" in query because it means Boolean NOT
queryTerms<-unique(gsub("-","",focus.words))
if(source.type=="a"){
   sql<-"SELECT year, title, abstract as content FROM abstract WHERE year > '2006'"
   sql<-paste(sql," AND id in(SELECT docid from abstract_fts4 where abstract match '",
              paste(queryTerms, collapse=" OR "),"')",sep="")
}else{
   sql<-"SELECT content,datestamp FROM blog_post WHERE datestamp > '2009-01-01'"
   sql<-paste(sql," AND id in(SELECT docid from blog_post_fts4 where content match '",
              paste(queryTerms, collapse=" OR "),"')",sep="")
}


## USEFUL FUNCTIONS
make.table<-function(x){
   t<- table(as.factor(x))
   return(t)
}

##
## Log run info and stats
##
name.suffix <-gsub("/","-",focus.placeholder)
log.file<-file(paste("AdjacentWords - ",name.suffix,".log", sep=""), open="wt")
cat(title, file = log.file, sep = "\n", append = FALSE)
cat(paste("Run for", focus.placeholder), file = log.file, sep = "\n")
cat(paste("Central node is:", paste(focus.words,sep=",")), file = log.file, sep = "\n")
cat(file = log.file, sep = "\n")
cat(paste("Only showing words above the ", cut.quantile*100, "% quantile",sep=""),file = log.file, sep = "\n")
if(split.years){
   cat(paste("(or for single years, the ", cut.quantile.split*100, "% quantile)",sep=""),file = log.file, sep = "\n")
}
cat(file = log.file, sep = "\n")

##
## Read in the documents. from SQLite
##
source.table<-dbGetQuery(db,sql)#query, fetch all records to dataframe and clear resultset in one go

#some massaging depending on the source
if (source.type=="b"){
   if(split.years){
      source.table<-cbind(source.table, data.frame(year=substr(source.table[,"datestamp"],1,4)))# for splitting
   }
}

#prepare to split by years
#a bit of a hack - dummy year = "" is used for all-years
if(split.years){
   y<-as.factor(c(as.character(source.table[,"year"]),""))
}else{
   y<-as.factor("")
}

# lloop over the years or a dummy loop over all-years as required
for(y.filter in levels(y)){
   if(y.filter==""){
      cat("Processing all entries", file = log.file, sep = "\n")
      p<-source.table[,"content"]      
   }else{
      y.bool<-y==y.filter
      cat(paste("Loop for:",y.filter, " N(docs)=",sum(y.bool)), file = log.file, sep = "\n")
      p<-source.table[y.bool,"content"]
   }
   print(paste("**************", y.filter,"**************"))
   
   #change "." to something that matches as a "word"; this is used to mark end and beginning of sentence as a pseudo-word in the results
   sent.mark=" eos "
   p<- gsub(pattern="\\.", replacement=sent.mark, p)
   #make sure we start and end with markers
   p<-gsub(pattern="\\A|\\Z", replacement = sent.mark, p, perl=T)
   
   #kill remaining punctuation
   p<- gsub(pattern="[[:punct:]]", replacement="", p)
   #force lower case since we do not care about case differences
   p<-tolower(p)
   
   
   # find words before the given one. Convention is "b" means before, "a" means after
   # spaces are kept in the match because ?<= doesn't allow repetition (remove spaces later)
   # - patterns
   b.pattern<- paste("(\\w+\\s+)(",paste("(?=",focus.words, "\\s)", collapse="|",sep=""), ")", sep="")
   a.pattern<- paste("(",paste("(?<=\\s",focus.words,")",collapse="|", sep=""),")(\\s+\\w+)", sep="")
   # - matching
   mp.b<-gregexpr(b.pattern,p, perl=T, ignore.case=T)
   rm.b<-regmatches(p,mp.b)
   mp.a<-gregexpr(a.pattern,p, perl=T, ignore.case=T)
   rm.a<-regmatches(p,mp.a)
   # - normalise case and eliminate spaces
   words.b<-gsub(pattern=" ", replacement="", tolower(unlist(rm.b)))
   words.a<-gsub(pattern=" ", replacement="", tolower(unlist(rm.a)))
   # - tabulation of the word occurrences
   t.b<-make.table(words.b) 
   t.a<-make.table(words.a)
   
   # ABORT if nothing found - i.e. if the focus words never appear in a matchable context
   if(y.filter=="" && (sum(t.a)==0 || sum(t.b)==0)){
      stop("FOCUS WORDS NOT FOUND. STOPPING")
   }
   
   #which terms are above the Qth quantile?
   if(y.filter==""){
      q<-cut.quantile
   }else{
      q<-cut.quantile.split
   }
   cut.b<-floor(quantile(t.b, probs=q))
   cut.a<-floor(quantile(t.a, probs=q))
   t.b.sel<- t.b[t.b>=cut.a]
   t.a.sel<- t.a[t.a>=cut.b]
   
   if(length(t.a.sel)==0 || length(t.b.sel)==0){
      print(paste("No useful results for", y.filter," - skipping"))
      cat("No useful results", file=log.file, sep="\n")
   }else{
      #print/log actual word counts as these are sometimes useful
      total.count<-sum(t.a)
      cat(paste("Word count equivalent to 100% is:",total.count) ,file = log.file, sep = "\n")
      print("Word frequencies (before):")
      print(t.b.sel)
      print("Word frequencies (after):")
      print(t.a.sel)
      
      #revert the "eos" placeholder to a full stop for display
      names(t.a.sel)[names(t.a.sel)=="eos"]<-"."
      names(t.b.sel)[names(t.b.sel)=="eos"]<-"."
      
      #scale factor -> percentages as weights
      scale<-100.0 / total.count
      
      ##
      ## VISUALISATION
      ##
      ## in Gephi use graphviz layout engine.
      #node output for Gephi. NB: weight is sqrt((freq)
      focus.id<-"_focus_"
      df.nodes<-data.frame(Id=focus.id, Label=focus.placeholder, Weight=sqrt(sum(t.a)))
      before.ids<-paste(names(t.b.sel), "_", sep="")
      df.nodes<-rbind(df.nodes, data.frame(Id=before.ids, Label=names(t.b.sel), Weight=sqrt(as.numeric(t.b.sel))))
      after.ids<-paste("_", names(t.a.sel), sep="")
      df.nodes<-rbind(df.nodes, data.frame(Id=after.ids, Label=names(t.a.sel), Weight=sqrt(as.numeric(t.a.sel))))
      #edges. NB: weight = freq, scaled to %
      df.edges<-data.frame(Source=before.ids, Target=rep(focus.id,length(before.ids)), Type=rep("Directed", length(before.ids)), Weight=scale*as.numeric(t.b.sel))
      df.edges<-rbind(df.edges,data.frame(Source=rep(focus.id,length(after.ids)), Target=after.ids, Type=rep("Directed", length(after.ids)), Weight=scale*as.numeric(t.a.sel)))
      
      #scale size to area for node weight before writing out
      write.csv(df.nodes, file=paste(name.suffix, y.filter, filename.postfix, "AdjacentWord Nodes.csv",sep=" "), row.names=FALSE)
      write.csv(df.edges, file=paste(name.suffix, y.filter, filename.postfix, " AdjacentWord Edges.csv",sep=" "), row.names=FALSE)
      
      df.nodes
      df.edges
      
      # graph using igraph - intended for interactive use; gephi for "smart" presentation
      # rename the columns as required
      colnames(df.nodes)[2]<-"label"
      colnames(df.nodes)[3]<-"size"
      #edge label = weight (= linked word weighted freq) but neatened up!
      df.edges<-cbind(df.edges, data.frame(label = paste(round(df.edges[,"Weight"]),"%", sep="")))
      df.nodes[,"size"]<-df.nodes[,"size"]*10*sqrt(scale) #make disaply easier to look at!
      g <- graph.data.frame(df.edges, directed=TRUE, vertices=df.nodes)
      subText<-paste(total.count,"words = 100%")
      plot.igraph(g, main=paste(short.title,y.filter), sub=subText, edge.arrow.size=2)
      png(file=paste(name.suffix," ", y.filter, " ", filename.postfix,".png", sep=""), width=1000, height=1000,pointsize=12, res=150)
      plot.igraph(g, main=paste(short.title,y.filter), xlab=subText, xlim=c(-0.8,0.8),ylim=c(-0.8,0.8), edge.arrow.size=2)
      ad<-dev.off()
   }# end if useful results
   
}#end the y.filter loop

#stop logging
close(log.file)

# properly terminate database use
if(use.sqlite){
   dbDisconnect(db)
}

if(profile){
   Rprof(NULL)
   #   summaryRprof("Rprof.out")$by.self
}