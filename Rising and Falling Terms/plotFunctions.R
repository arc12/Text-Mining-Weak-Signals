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
## Utility plot functions, plotting to standard output and to a file, which is generally
## assumed to be in a working directory defined in the calling code.#
##

#var to use to make some more space at the bottom of plots for long label (use par(mar=mar.bigmar) )
mar.default<-par("mar")
mar.bigmar<-c(6,4,4,2)

# basic barplot with labels horizontal and under x axis
basic.barplot<-function(X,Main,Xlab,Ylab,Names,OutputFile, ForceIntYAxis=FALSE){
   par(mar=mar.bigmar)
   barplot(X,main=Main, xlab=Xlab, ylab=Ylab, names.arg=Names, axes=!ForceIntYAxis)
   if(ForceIntYAxis){
      axis(2,  at=0:max(X))# to get integers only
   }
   # Repeat the previous to create a png
   png(OutputFile, width=1000, height=1000,pointsize=12, res=150)
   barplot(X,main=Main, xlab=Xlab, ylab=Ylab, names.arg=Names, axes=!ForceIntYAxis)
   if(ForceIntYAxis){
      axis(2,  at=0:max(X))# to get integers only
   }
   ad<-dev.off()   
}

# no x axis title or labels; labels appear inside bars
insideLabel.barplot<-function(X,Main,Ylab,Names,OutputFile, ForceIntYAxis=FALSE){
   par(mar=mar.default)
   x.pos<-barplot(X,main=Main, ylab=Ylab, names.arg="", axes=!ForceIntYAxis)
   if(ForceIntYAxis){
      axis(2,  at=0:max(X))# to get integers only
   }
   par(srt=90)
   text(x=x.pos, y=0.1, Names, adj=c(0,0.5))# puts labels inside bars
   par(srt=0)
   # Repeat the previous plot to create png
   png(OutputFile, width=1000, height=1000, pointsize=12, res=150)
   x.pos<-barplot(X,main=Main, ylab=Ylab, names.arg="", axes=!ForceIntYAxis)
   if(ForceIntYAxis){
      axis(2,  at=0:max(X))# to get integers only
   }
   par(srt=90)
   text(x=x.pos, y=0.1, Names, adj=c(0,0.5))# puts labels inside bars
   par(srt=0)
   ad<-dev.off()
}

basic.hist<-function(X,Main,Xlab,OutputFile, Breaks=10){
   par(mar=mar.bigmar)
   if(min(X)>=0){
      Xlim<-c(0, max(X))
   }else{
      Xlim=c(min(X), 0)
   }
   hist(X, breaks=Breaks, main=Main, xlab=Xlab, xlim=Xlim)
   png(OutputFile, width=1000, height=1000,pointsize=12, res=150)
   hist(X, breaks=Breaks, main=Main, xlab=Xlab, xlim=Xlim)
   ad<-dev.off()
}


colorized.barplot<-function(X, Main, Ylab, Names, Colours, OutputFile){
   par(mar=mar.bigmar)
   barplot(X, las=2, cex.names=0.7, col=Colours, main=Main, ylab=Ylab, names.arg=Names)
   png(OutputFile, width=1000, height=1000,pointsize=12, res=150)
   barplot(X, las=2, cex.names=0.7, col=Colours, main=Main, ylab=Ylab, names.arg=Names)
   ad<-dev.off()
}

#for showing comparison between previous and current sets as stack or side-by-side
pair.barplot<-function(X.past, X.target, Main, Ylab, Names, OutputFile, Beside=FALSE){
   barplot(rbind(X.past, X.target),las=2, cex.names=0.7, names.arg=Names, main=Main,
           legend.text=c("Previous","Target"), ylab=Ylab, beside=Beside)
   png(OutputFile, width=1000, height=1000,pointsize=12, res=150)
   barplot(rbind(X.past, X.target),las=2, cex.names=0.7, names.arg=Names, main=Main,
           legend.text=c("Previous","Target"), ylab=Ylab, beside=Beside)
   ad<-dev.off()
}


#simplest heat map
basic.heatmap<-function(M, Main, ColumnLabels, OutputFile){
   par(mar=mar.bigmar)        
   heatmap(M, main=Main, labCol=ColumnLabels, margins=c(10,5))
   png(OutputFile, width=1000, height=1000,pointsize=12, res=150)
   heatmap(M, main=Main, labCol=ColumnLabels, margins=c(10,5))
   ad<-dev.off()
}

#basic scatter plot with log y exis
log.scatter<-function(X, Y,  Main, Xlab, Ylab, OutputFile){
   plot(X, Y ,main=Main, xlab=Xlab, ylab=Ylab, log="y")
   png(OutputFile, width=1000, height=1000, pointsize=12, res=150)
   plot(X, Y ,main=Main, xlab=Xlab, ylab=Ylab, log="y")
   ad<-dev.off()
}

