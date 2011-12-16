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
## 
##

library("tm")
library("slam")#row_sums
library("Snowball")#stemming
library("plyr")# used for combining term freq vectors
library("corpora")
library("shape")#used for colour legend
library("brew")

## Run Properties - dependent on the source
base.dir<-"/home/arc1/R Projects/Text Mining Weak Signals"
my.dir<-paste(base.dir,"Compair", sep="/")
source.dir<-paste(base.dir,"Source Data",sep="/")
#the output directory. NB convention to include the year
output.dir<-"/home/arc1/R Projects/Text Mining Weak Signals Output/Compair/2011 ICALT and ICCE"
dir.create(output.dir, showWarnings=FALSE)
setwd(output.dir)

title<-"Comparison: 2011 Conference Proceedings from ICALT and ICCE"
dir.A <- "ICALT Full 2011"
dir.B<-"ICCE Full 2011"
name.A <- "ICALT"
name.B<-  "ICCE"
title.A <- "IEEE International Conference on Advanced Learning Technologies"
title.B<-"ICCE"
url.A <- "http://www.ask4research.info/icalt/2010/"
url.B<-"http://www.nectec.or.th/icce2011/"
# additional stopwords, e.g. to remove words appearing in boilerplate of one conf and not the other
extra.stopwords<-c("IEEE","International","Conference","Advanced","Learning",
                   "Technologies","Proceedings","Computers","Education","Asia-Pacific",
                   "Society")

##
## Run properties
##
# minimum term frequency for inclusion (aggregate over both sets)
# NB as a fraction of terms
min.term.freq<-0.002 #0.2%
#max term freq is meant to be used to eliminate terms that mess up the plots
max.term.freq<-0.02
#min number of docs term must appear in (agg over both sets)
min.docs<-4
# statistical significance threshold
p.max<-0.001

#
#start logging run info
log.file<-file("Compair.log", open="wt")
cat(title, file = log.file, sep = "\n", append = FALSE)
cat(paste(paste(rep("=",79),collapse="")),"\n")
cat(paste("Set A=",name.A," Set B=",name.B,sep=""), file = log.file, sep = "\n", append = TRUE)
cat(paste("Run Parameters: min term freq=",min.term.freq,"; min docs=", min.docs,"; max p=",p.max, sep=""), file = log.file, sep = "\n", append = TRUE)
cat(paste(paste(rep("-",79),collapse="")),"\n\n")

##
##
##
readCorp<-function(dir.name){
   Corpus(DirSource(paste(source.dir,dir.name,sep="/"),pattern="*.pdf"), readerControl=list(reader=readPDF(pdfinfoOptions="", pdftotextOptions="-nopgbrk -q")))
}
corp.A<-readCorp(dir.A)
corp.B<-readCorp(dir.B)

##
## Compute 
##
makeDTM<-function(corp){
   DocumentTermMatrix(corp, control = list(stemming=TRUE, stopwords=c(stopwords(language = "en"),tolower(extra.stopwords)), minWordLength=3, removeNumbers=TRUE, removePunctuation=TRUE))
}

dtm.A <- makeDTM(corp.A)
dtm.bin.A<- weightBin(dtm.A)
dtm.B <- makeDTM(corp.B)
dtm.bin.B<- weightBin(dtm.B)
cat(paste("Summary for",name.A,":", " Docs=",length(Docs(dtm.A)),
          " Terms=",length(Terms(dtm.A))), file = log.file, sep = "\n", append = TRUE)
cat(paste("Summary for",name.B,":", " Docs=",length(Docs(dtm.B)),
          " Terms=",length(Terms(dtm.B))), file = log.file, sep = "\n", append = TRUE)

freqs.A<-col_sums(dtm.A)
freqs.B<-col_sums(dtm.B)
sum.terms.A<-sum(freqs.A)
sum.terms.B<-sum(freqs.B)
docs.A<-col_sums(dtm.bin.A)
docs.B<-col_sums(dtm.bin.B)

#combine the two named lists into a data frame - i.e. with columns as the union set of terms and zeros for missing values
combine<-function(A,B){
   cc<-rbind.fill(as.data.frame(t(A)),as.data.frame(t(B)))
   cc[1,is.na(cc[1,])]<-0 #rbind.fill leaves NAs
   cc[2,is.na(cc[2,])]<-0
   return(cc)
}
df<-combine(freqs.A,freqs.B)
df<-rbind(df,combine(docs.A,docs.B))
rownames(df)<-c("freqA","freqB","docsA","docsB")
cat(paste("Merging the terms lists ->",length(df[1,]),"terms"), file = log.file, sep = "\n", append = TRUE)

#remove low freq and low doc-count terms (and apply the max freq too)
min.count<-as.integer(min.term.freq*(sum.terms.A+sum.terms.B))
cat(paste("Will eliminate terms with freq <", min.term.freq*100,
          "%, which equates to ",min.count,
          " term occurrences (aggregated over both sets)", sep=""),
                        file = log.file, sep = "\n", append = TRUE)
df<-df[,(df["freqA",]+df["freqB",])>=min.count]
df<-df[,(df["docsA",]+df["docsB",])>=min.docs]
df<-df[,df["freqA",]<=max.term.freq*sum.terms.A]
df<-df[,df["freqB",]<=max.term.freq*sum.terms.B]
cat(paste("Now using",length(df[1,]),"terms"), file = log.file, sep = "\n", append = TRUE)
cat(paste(paste(rep("-",79),collapse="")),"\n\n")

#find the statistically significant term freq differences
pvals<-chisq.pval(df["freqA",],sum.terms.A, df["freqB",],sum.terms.B)
df["p",]<-pvals

#quick plot for diagnosis
hist(pvals)

#select only those passing the threshold
#and create two groups depending on whether set A or B is more frequent ** use fractional freqs
df<-df[,df["p",]<=p.max]
df.A<-df[,df["freqA",]/sum.terms.A>df["freqB",]/sum.terms.B]
df.B<-df[,df["freqA",]/sum.terms.A<df["freqB",]/sum.terms.B]

#find unstemmed words for the selected terms to make for prettier plots
words.A<-stemCompletion(colnames(df.A),corp.A,type="prevalent")
words.A[words.A==""]<-names(words.A[words.A==""])
words.B<-stemCompletion(colnames(df.B),corp.B,type="prevalent")
words.B[words.B==""]<-names(words.B[words.B==""])

# plot an all-together
squares.plot<-function(X,Y,Z,Labels,Main,FileName){
   rt.Z<-sqrt(Z)
   Z.pal<-as.integer(Z)-min(Z)+1
   pal<-heat.colors(max(Z),alpha=0.7)
   type<-unlist(strsplit(FileName,"\\."))[[2]]#file type
   switch(type,
      png=png(filename=FileName, width=1000, height=1000, pointsize=14, res=72),
      pdf=pdf(file=FileName, paper="special", width=14,height=14,pointsize=14))
   symbols(X, Y, squares=rt.Z,
        fg="black", bg=pal[Z.pal], inches=0.3, log="y", ylim=c(-log10(p.max),max(Y)),
        xlab="Term Frequency(%)", ylab="Significance (-log10(p))", main=Main, cex.main=1.0)
   text(X, Y, Labels, cex=1.0, pos=4, offset=0.8)
   colorlegend(pal, zlim=c(min(Z.pal), max(Z.pal)),
               main="Docs", cex=1.0, main.cex=1.5, posx=c(0.93,0.96), left=TRUE)
   dev.off()
}
squares.plot(100*df.A["freqA",]/sum.terms.A, -log10(df.A["p",]), df.A["docsA",],words.A,
             paste("Terms more frequent at",name.A),FileName="A.png")
squares.plot(100*df.B["freqB",]/sum.terms.B, -log10(df.B["p",]), df.B["docsB",],words.B,
             paste("Terms more frequent at",name.B),FileName="B.png")

squares.plot(100*df.A["freqA",]/sum.terms.A, -log10(df.A["p",]), df.A["docsA",],words.A,
             paste("Terms more frequent at",name.A),FileName="A.pdf")
squares.plot(100*df.B["freqB",]/sum.terms.B, -log10(df.B["p",]), df.B["docsB",],words.B,
             paste("Terms more frequent at",name.B),FileName="B.pdf")

##
## Gephi co-occurrence within each set
##
gephi.csv<-function(DF, Labels, DTM.bin, SetLetter){
   terms<-colnames(DF)
   nodes1.df<-data.frame(Id=terms,Label=Labels,Weight=-log10(as.numeric(DF["p",])))
   edges1.df<-data.frame()
   #use the binary occurrence doc-term-matrix in plain matrix form for easy calcs
   mat.bin<-as.matrix(DTM.bin)[,terms]#subset of columns
   #loop over terms, this loop is one end of each edge
   for(t in 2:length(terms)){
      edge.weights<-colSums(mat.bin[,t]*mat.bin)
      edges1.df<-rbind(edges1.df,data.frame(
              Source=terms[t],
              Target=terms[1:(t-1)],#omits the self-referential edge and avoids double counting edges (A-B and B-A)
              Type="Undirected",
              Weight=edge.weights[1:(t-1)]))
   }
   cat(paste("Term Co-occurrence calculation and Gephi data creation"), file = log.file, sep = "\n", append = TRUE)
   cat(paste("Significant Term Co-occurrence Stats for Set",SetLetter), file = log.file, sep = "\n", append = TRUE)
   ew.summary<-summary(unlist(edges1.df["Weight"]))
   cat(paste(names(ew.summary),ew.summary,sep="="), file = log.file, sep = "\n", append = TRUE)
   #remove cases with less than median co-occurrence
   cat("Only exporting co-occurrence edges >= the 3rd Quartile in weighting.", file = log.file, sep = "\n\n", append = TRUE)
   edges1.df<-subset(edges1.df,Weight>=ew.summary["3rd Qu."])
   #save to disk. we do not want row names since these end up being scrubbed on import to gephi
   write.csv(nodes1.df, file=paste(SetLetter, "Term-Co-occurrence Nodes.csv",sep=" "),
             row.names=FALSE)
   write.csv(edges1.df, file=paste(SetLetter, "Term-Co-occurrence Edges.csv",sep=" "),
             row.names=FALSE)
}

gephi.csv(df.A,words.A,dtm.bin.A,"A")
gephi.csv(df.B,words.B,dtm.bin.B,"B")
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
# - set edge thickness to 0.1.  curved edges sometimes work OK
# - set label font to 36pt bold.
# - set the opacity to somewhere between 60 and 80% so that labels show up better
# - uncheck "proportional size" on node and edge
# - when exporting to PNG, usually need to set a 25% margin (or more) otherwise it gets cropped!

#Create HTML report
brew(file=paste(my.dir,"BrewTemplate.html",sep="/"), output="Report.html",run=TRUE)

#stop logging
close(log.file)


#dump the TM/stats results
write.csv(df.A, "Raw Results A.csv", quote=TRUE, row.names=TRUE)
write.csv(df.A, "Raw Results B.csv", quote=TRUE, row.names=TRUE)
zip("data.zip",c("Raw Results A.csv", "Raw Results B.csv", "A Term-Co-occurrence Edges.csv","A Term-Co-occurrence Nodes.csv", "B Term-Co-occurrence Edges.csv","B Term-Co-occurrence Nodes.csv"))
