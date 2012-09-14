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
## This contains the parameters for running a specific dataset against the HistoryVis.R Method
## It should be executed first
##

## Run Properties - dependent on the source
base.dir<-"/home/arc1/R Projects/Text Mining Weak Signals"
source.dir<-paste(base.dir,"Source Data",sep="/")
set.name<-"Union C 2006 to 2012"
output.dir<-paste("/home/arc1/R Projects/Text Mining Weak Signals Output/History Visualiser",set.name,sep="/")
brew.dir<-paste(base.dir,"History Visualiser",sep="/")
web.page.base<-paste("http://arc12.github.com/Text-Mining-Weak-Signals-Output/History Visualiser",set.name, sep="/")
brew.type<-"c"# c=conference abstracts, b=blog posts

dir.create(output.dir, showWarnings=TRUE)
setwd(output.dir)

## SOURCE DATA SELECTION
# Either a) [DEPRECATED] list csv files "with metrics" as produced by Pre-process.R These are combined into one corpus or
#        b) Locate a SQLite Database and define a query to extract
#sets.csv <- c("ICALT Abstracts 2005-2011 with metrics.csv",
                   #"ECTEL Abstracts 2006-2011 with metrics.csv",
              #"ICWL Abstracts 2005-2011 with metrics.csv",
                  #"ICHL Abstracts 2008-2011 with metrics.csv",
#                   "CAL Abstracts 2007-2011 with metrics.csv")
set.csv<-NA
sqlite.filename<-"TMWS Data A.sqlite" #set this to NA to use [deprecated] option a
sql<-"SELECT origin, year, pages, title, authors, abstract, keywords, url, dblp_url, pos_score, neg_score, subj_score FROM abstract WHERE year >= '2006'"#BETWEEN '2006' AND '2011'"
use.sqlite<-!is.na(sqlite.filename)
                  
##
## Run properties - normally the same between different sources of the same kind for comparability
##
today<-as.POSIXlt(Sys.Date(), tz = "GMT")
start.year<-2006
start.month<-1 #default = 1
end.year<-2012
end.month<-1 #default = 1. NB this determines the start of the last slice
# data interval control.
slice.size<-12 #how many months in a time slice used in the analysis.
interpolate.size<-3 #number of months between interpolated points in the output; 1 "row" is created for each interval. No interpolation if slice.size = interpolate.size

##
## Which Terms to run for
##
title.common<-"Conference Proceedings from ICALT, ECTEL, CAL, ICHL and ICWL"
# NB!!!!!!!!! these are the STEMMED terms
term.lists<-list(Cloud=c('cloud','virtualis','virtual','saa','paa'),
                 eBooks=c('ebook', 'etextbook'),
                 Analytics=c('analyt','data'),
                 Gesture=c('gesturebas','gestur'),
                 Context.Sensitive.Services=c('context','contextsensit','contextawar','contextenrich','locat','locationbas','locationawar','geospati'),
                 Games=c('game', "gamif","gamebas", "gameplay"),
                 Mobile=c('tablet','smartphon','mobil','ubiquit','pervas'),
                 Learning.Platforms=c('lms','vle','lcms','eportfolio'))
# .... and these are the pretty versions for display
word.lists<-list(Cloud=c('Cloud','Virtualisation','Virtual','SaaS','PaaS'),
                 eBooks=c('eBook', 'eTextbook'),
                 Analytics=c('Analytics','Data'),
                 Gesture=c('Gesture-based','Gesture'),
                 Context.Sensitive.Services=c('Context','Context-sensitive','Context-aware','Context-enriched','Location','Location-based','Location-aware','Geospatial'),
                 Games=c('Game', "Gamification","Game-based", "Game-play"),
                 Mobile=c('Tablet','Smartphone','Mobile','Ubiquitous','Pervasive'),
                 Learning.Platforms=c('LMS','VLE','LCMS','E-Portfolio','Platform'))

titles<-gsub("\\."," ",names(term.lists))#lazy way to titles is to replace "." in the list element names - override if necessary

# The folloowing indicates which of the lists above is treated as a group.
# NB: the strings in go.groups must exactly match the list item names in term.lists (and "." characters are replaced with " " in output)
do.groups=c("Cloud","eBooks","Analytics","Gesture","Context.Sensitive.Services","Games","Mobile","Learning.Platforms")

# in interactive execution it may be best to skip this command and to manually switch to it
#source(paste(base.dir,"History Visualiser/HistoryVis.R", sep="/"))
