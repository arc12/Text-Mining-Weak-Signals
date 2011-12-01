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
## This contains the parameters for running a specific dataset against the RF_Terms.R Method
## It should be executed first
##

## Run Properties - dependent on the source
#the output directory. NB convention to include the year
output.dir<-"/home/arc1/R Projects/Text Mining Weak Signals Output/Rising and Falling Terms/Union B/testing"
dir.create(output.dir, showWarnings=FALSE)
setwd(output.dir)
title<-"Rising and Falling Terms - Conference Proceedings from ICALT, CAL, ECTEL and ICWL"
abstracts.csv <- c("/home/arc1/R Projects/Source Data/ICALT Abstracts 2004-2011.csv",
                   "/home/arc1/R Projects/Source Data/CAL Abstracts 2007-2009.csv",
                   "/home/arc1/R Projects/Source Data/ECTEL Abstracts 2006-2011.csv",
                   "/home/arc1/R Projects/Source Data/ICWL Abstracts 2004-2011.csv")
recent.themes.txt <- NA # file containing invited conference themes. Use NA if analysing blogs.
conference.name <- c("ICALT",
                     "CAL",
                     "ECTEL",
                     "ICWL")
last.conference.url <- c("http://www.ask4research.info/icalt/2010/",
                         "",
                         "http://www.ectel2010.org/",
                         "http://www.hkws.org/conference/icwl2010/")
conference.title <- c("IEEE International Conference on Advanced Learning Technologies",
                      "Computer Assisted Learning Conference",
                      "European Conference on Technology Enhanced Learning",
                      "International Conference on Web-based Learning")
publisher.series <- c("IEEE",
                      "Elsevier Computers and Education Journal",
                      "Springer Lecture Notes in Computer Science (LNCS)",
                      "Springer Lecture Notes in Computer Science (LNCS)",
                      "Springer Lecture Notes in Computer Science (LNCS)")
publisher.url <- c("http://ieeexplore.ieee.org/xpl/mostRecentIssue.jsp?punumber=5570018",
                   "http://www.journals.elsevier.com/computers-and-education/",
                   "http://www.springerlink.com/content/978-3-642-16019-6/contents/",
                   "http://www.springer.com/computer/general+issues/book/978-3-642-17406-3")
                   
## ensure subdirectories exist
dir.create("Gephi", showWarnings=FALSE)
dir.create("Images", showWarnings=FALSE)
dir.create("Wordle", showWarnings=FALSE)

##
## Run properties - normally the same between different sources of the same kind for comparability
##
# key date is the one after which documents are considered to be recent
# for conference abstracts, this is just the most-recent conference
# for blogs is is a 6 month period before now
today<-as.POSIXlt(Sys.Date(), tz = "GMT")
#key.date<-today;key.date$mon<-key.date$mon-6 #blog version, 6 mon window
conf.year.recent<-2010 #conference abs version => 2010 confs are "recent"
conf.years.in_past<-4 # abstracts from the previous 4 years are counted as "past"
key.date<-as.POSIXlt(paste(conf.year.recent-1,"12","31",sep="-"), tz = "GMT")
start.date<-key.date; start.date$year<-start.date$year-conf.years.in_past 
last.date<-key.date;last.date$year<-last.date$year+1

# thresholds
# how many documents must the term appear in to be listed. This is in addition to the frequency thresholds. A value of 2 is expected, i.e. ignore terms that appear in only one doc
doc_count.thresh <- 2
# p-value to accept the "alternative hypothesis" that there is something interesting
thresh.pval<-0.005 #i.e. accept a .5% chance that null hypothesis falsely rejected
#max frequency of term in the past set for eligibility as a weak signal.
#Above this, sigifnicant risers are "established terms"
max.past.freq<-0.0002 #i.e. 0.02%
# *for plotting* cap rising % at this level. If value greater then plot is effectively cut off
rising.plot.max<-800

#novelty calc
term.doc_occurrence.max<-0.1#remove terms appearing in more than 10% of documents
std.novelty.min<-0.33 #a min value of the "standard novelty"

##
## End setup
##

# in interactive execution it may be best to skip this command and to manually switch to it
#source("../RF_Terms.R")
