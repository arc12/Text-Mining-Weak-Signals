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
## This contains the parameters for running a specific dataset against the HistoryVis.R Method
## It should be executed first
##

## Run Properties - dependent on the source
base.dir<-"/home/arc1/R Projects/Text Mining Weak Signals"
source.dir<-paste(base.dir,"Source Data",sep="/")
set.name<-"CETIS Conf 2012 - CETIS Blogs"
output.dir<-paste("/home/arc1/R Projects/Text Mining Weak Signals Output/History Visualiser",set.name,sep="/")
brew.dir<-paste(base.dir,"History Visualiser",sep="/")
web.page.base<-paste("http://arc12.github.com/Text-Mining-Weak-Signals-Output/History Visualiser",set.name, sep="/")
brew.type<-"b"# c=conference abstracts, b=blog posts

dir.create(output.dir, showWarnings=TRUE)
setwd(output.dir)
#these are combined into one corpus
sets.csv <- "CETIS Blogs 20090101-20120301 with metrics.csv"
#c("CETIS Blogs 20110101-20120301 with metrics.csv","NonCETIS Blogs 20110101-20120301 with metrics.csv")
                  
##
## Run properties - normally the same between different sources of the same kind for comparability
##
today<-as.POSIXlt(Sys.Date(), tz = "GMT")
start.year<-2009
start.month<-1#generally set to 1
end.year<-2012
end.month<-1#generally set to 1 unless total period<2 years or so NB this determines the start of the last slice
# data interval control.
slice.size<-4 #how many months in a time slice used in the analysis. the last slice is is from start month 1 in the end.year to the end of month "slice.size" in end.year
interpolate.size<-1 #number of months between interpolated points in the output; 1 "row" is created for each interval. No interpolation if slice.size = interpolate.size


##
## Which Terms to run for
##
title.common<-"CETIS Blogs - Jan 2009-Feb 2012"
#the following titles should match each of the entries in the following list
titles<-c("select dominant terms (c.f. EdTech generally)",
          "select rising terms (last 6 months)",
          "select established rising (last 6 months)",
          "select falling terms (last 6 months)")
          
# NB!!!!!!!!! these are the STEMMED terms
# NB2!!! if you get the stemmed term wrong then a "subscript out of bounds" error occurs later on
term.lists<-list(Familiar=c("data","level","design","innov","inform", "manag","institut"),
                 Rising=c("eassess","badg","literaci","registri","mobil","skill"),
                 Established=c("assess","feedback","compet","staff"),
                 Falling=c("cloud","oer","rdfs","semant","wave","repositori"))
# .... and these are the pretty versions for display
word.lists<-list(Familiar=c("data","level","design","innovation","information", "management","institutions"),
                 Rising=c("e-assessment","badge","literacies","registry","mobile","skill"),
                 Established=c("assessment","feedback","competence","staff"),
                 Falling=c("cloud","oer","RDFS","semantic","wave","repositories"))

##
## End setup
##



# in interactive execution it may be best to skip this command and to manually switch to it
#source("../RF_Terms.R")
