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

## Run Properties - dependent on the source
base.dir<-"/home/arc1/R Projects/Text Mining Weak Signals"
source.dir<-paste(base.dir,"Source Data",sep="/")
#the output directory. NB convention to include the year
output.dir<-"/home/arc1/R Projects/Text Mining Weak Signals Output/Compair"
dir.create(output.dir, showWarnings=FALSE)
setwd(output.dir)

title<-"Comparison - 2011 Conference Proceedings from ICALT and ICCE"
dir.A <- "ICALT Full 2011"
dir.B<-"ICCE Full 2011"
name.A <- "ICALT"
name.B<-  "ICCE"
title.A <- "IEEE International Conference on Advanced Learning Technologies"
title.B<-"ICCE"
url.A <- "http://www.ask4research.info/icalt/2010/"
url.B<-""
# additional stopwords, e.g. to remove words appearing in boilerplate of one conf and not the other
extra.stopwords<-c("IEEE","International","Conference","Advanced","Learning",
                   "Technologies","Proceedings","Computers","Education","Asia-Pacific",
                   "Society")

## ensure subdirectories exist
# dir.create("Gephi", showWarnings=FALSE)
# dir.create("Images", showWarnings=FALSE)
# dir.create("Wordle", showWarnings=FALSE)

##
## Run properties
##
# minimum term frequency for inclusion (aggregate over both sets)
# NB as a fraction of terms
min.term.freq<-0.002 #0.2%
#min number of docs term must appear in (agg over both sets)
min.docs<-4
# statistical significance threshold
p.max<-0.001

#
#start logging all std output to a file in addition to "printing" to console
#first clean up old sinks
while(sink.number()>0)
  {sink()}
sink(file="Compair.log", append=FALSE, type="output", split=TRUE)
print(title)
print(paste("Set A=",name.A," Set B=",name.B,sep=""))
print(paste("Run Parameters: min term freq=",min.term.freq,"; min docs=", min.docs,"; max p=",p.max, sep=""))

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
print(paste("Summary for",name.A))
print(dtm.A)
print(paste("Summary for",name.B))
print(dtm.B)

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
print(paste("Merging the terms lists ->",length(df[1,]),"terms"))

#remove low freq and low doc-count terms
min.count<-as.integer(min.term.freq*(sum.terms.A+sum.terms.B))
print(paste("Will eliminate terms with freq <", min.term.freq*100,"%, which equates to ",min.count," term occurrences (aggregated over both sets)"),sep="")
df<-df[,(df["freqA",]+df["freqB",])>=min.count]
df<-df[,(df["docsA",]+df["docsB",])>=min.docs]
print(paste("Now using",length(df[1,]),"terms"))

#find the statistically significant term freq differences
pvals<-chisq.pval(df["freqA",],sum.terms.A, df["freqB",],sum.terms.B)
df["p",]<-pvals

#quick plot for diagnosis
hist(pvals)

#select only those passing the threshold
#and create two groups depending on whether set A or B is more frequent
df<-df[,df["p",]<=p.max]
df.A<-df[,df["freqA",]>df["freqB",]]
df.B<-df[,df["freqA",]<df["freqB",]]

#find unstemmed words for the selected terms to make for prettier plots
words.A<-stemCompletion(colnames(df.A),corp.A,type="prevalent")
words.A[words.A==""]<-names(words.A[words.A==""])
words.B<-stemCompletion(colnames(df.B),corp.B,type="prevalent")
words.B[words.B==""]<-names(words.B[words.B==""])

# plot an all-together
squares.plot<-function(X,Y,Z,Labels,Main,FileName,type="png"){
   rt.Z<-sqrt(Z)
   Z.pal<-as.integer(Z)-min(Z)+1
   pal<-heat.colors(max(Z),alpha=0.7)
   switch(type,
      png=png(filename=FileName, width=1000, height=1000, pointsize=14, res=72),
      pdf=pdf(file=FileName, paper="a4r"))
   symbols(X, Y, squares=rt.Z,
        fg="black", bg=pal[Z.pal], inches=0.3, log="y", ylim=c(-log10(p.max),max(Y)),
        xlab="Term Frequency(%)", ylab="Significance (-log10(p))", main=Main)
   text(X, Y, Labels, cex=0.8, pos=4, offset=0.8)
   colorlegend(pal, zlim=c(min(Z.pal), max(Z.pal)),
               main="Docs", cex=1.0, main.cex=1.5, posx=c(0.93,0.96), left=TRUE)
   dev.off()
}
squares.plot(100*df.A["freqA",]/sum.terms.A, -log10(df.A["p",]), df.A["docsA",],words.A,
             paste("Terms more frequent at",name.A),FileName="A.png")
squares.plot(100*df.B["freqB",]/sum.terms.B, -log10(df.B["p",]), df.B["docsB",],words.B,
             paste("Terms more frequent at",name.B),FileName="B.png")


#term correlations data for Gephi, each set separate.
#GEPHI RECIPE HERE::::::: (use modularity)


#stop logging
sink()