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
## This is an initialiser for Compair.R; it contains run-specific setup 
##      it should be run before ../Compair.R
##
# Irritating absolute path specification
base.dir<-"/home/arc1/R Projects/Text Mining Weak Signals"
my.dir<-paste(base.dir,"Compair", sep="/")
source.dir<-paste(base.dir,"Source Data",sep="/")

## Run Properties - dependent on the source
#the output directory. NB convention to include the year
output.dir<-"/home/arc1/R Projects/Text Mining Weak Signals Output/Compair/_2011 ICALT and ICCE"
dir.create(output.dir, showWarnings=FALSE)
setwd(output.dir)

#informative labels etc for each set
title<-"Comparison: 2011 Conference Proceedings from ICALT and ICCE"
name.A <- "ICALT"
name.B<-  "ICCE"
title.A <- "IEEE International Conference on Advanced Learning Technologies"
title.B<-"ICCE"
url.A <- "http://www.ask4research.info/icalt/2010/" #only used if brew.type indicates conference
url.B<-"http://www.nectec.or.th/icce2011/" #only used if brew.type indicates conference

#specification of the source for each set
#may be either a directory containing N PDFs or a CSV
source.type<-"PDF"#"CSV"
#subdirectories of source.dir for source.type="PDF"
dir.A <- "ICALT Full 2011"
dir.B<-"ICCE Full 2011"
#file names in source.dir for source.type="CSV"
#file.A<-""
#file.B<-""
# the intro text for the Brew output will mostly be the same but needs adaption
brew.type<-"c2"

# additional stopwords, e.g. to remove words appearing in boilerplate of one set and not the other
extra.stopwords<-c("IEEE","International","Conference","Advanced","Learning",
                   "Technologies","Proceedings","Computers","Education","Asia-Pacific",
                   "Society")



##
## Run properties - statistical
##
# minimum term frequency for inclusion (aggregate over both sets)
# NB as a fraction of terms
min.term.freq<-0.002 #0.2%
#max term freq is meant to be used to eliminate terms that mess up the plots
max.term.freq<-0.02
#min number of docs in each set separately. Set=1 if sets small and use min.docs to control
#     set >1 for large sets to increase speed (reduces compute for term-set merging)
min.docs.single<-1
#min number of docs term must appear in (agg over both sets)
min.docs<-4
# statistical significance threshold
p.max<-0.001
##
## Output control
##
# edges (for gephi) will only be exported if the weight is >= the following quantile of calculated edge weights
edge.quantile <- 0.9 #suggest default of 0.75

