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
set.name<-"Union B 2005 to 2010_"
output.dir<-paste("/home/arc1/R Projects/Text Mining Weak Signals Output/History Visualiser",set.name,sep="/")
brew.dir<-paste(base.dir,"History Visualiser",sep="/")
web.page.base<-paste("http://arc12.github.com/Text-Mining-Weak-Signals-Output/History Visualiser",set.name, sep="/")
brew.type<-"c"# c=conference abstracts, b=blog posts

dir.create(output.dir, showWarnings=TRUE)
setwd(output.dir)
#these are combined into one corpus
sets.csv <- c("ICALT Abstracts 2005-2011 with metrics.csv",
                   "ECTEL Abstracts 2006-2011 with metrics.csv",
                   "ICWL Abstracts 2005-2011 with metrics.csv",
                   "CAL Abstracts 2007-2009 with metrics.csv")
                  
##
## Run properties - normally the same between different sources of the same kind for comparability
##
today<-as.POSIXlt(Sys.Date(), tz = "GMT")
start.year<-2005
start.month<-1 #default = 1
end.year<-2010
end.month<-1 #default = 1. NB this determines the start of the last slice
# data interval control.
slice.size<-12 #how many months in a time slice used in the analysis. the last slice is is from start month 1 in the end.year to the end of month "slice.size" in end.year
interpolate.size<-3 #number of months between interpolated points in the output; 1 "row" is created for each interval. No interpolation if slice.size = interpolate.size

##
## Which Terms to run for
##
title.common<-"Conference Proceedings from ICALT, ECTEL and ICWL" #CAL
titles<-c("terms that dipped in 2010 compared to the previous 4 years",
          "terms that rose in 2010 and where established in the previous 4 years",
          "terms that rose in 2010 from a low level in the previous 4 years")
          #"Run the Second")#should match each of the entries in the following list
# NB!!!!!!!!! these are the STEMMED terms
term.lists<-list(Falling=c("blog","content","databas","ontolog","project","technolog"),             Established=c("activ","condit","conduct","differ","emot","game","gamebas","motiv","path","profil","strategi","tutor","video"),
Rising=c("besid","competit","eassess","figur","gameplay","gender","hybrid","negat","oral","ples","probabilist","public","qti","risk","selfreflect","serious","statement","tablet","tangibl","uptak","wil"))
# .... and these are the pretty versions for display
word.lists<-list(Falling=c("blog","content","database","ontology","project","technology"),          Established=c("active","condition","conduct","different","emotion","game","game-based","motivator","path","profile","strategies","tutor","video"),
Rising=c("besides","competitive","e-assessment","figure","game-play","gender","hybrid","negative","oral","PLEs","probabilistic","public","qti","risk","self-reflect","serious","statements","tablet","tangible","uptake","will"))

##
## End setup
##



# in interactive execution it may be best to skip this command and to manually switch to it
#source("../RF_Terms.R")
