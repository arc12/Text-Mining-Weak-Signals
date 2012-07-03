##
## Take a previous run of RF_Terms.R and create a HTML report
##
library("tm")

# Load the RF_Terms workspace
setwd("/home/arc1/R Projects/Text Mining Weak Signals Output/Rising and Falling Terms/Union C/2011")

show.abstracts<-TRUE
show.nov.detail<-TRUE #novelty plots
show.sub.detail<-show.nov.detail #subjectivity plots

#temporary - to allow for use of older .RData
# source.type="c"
# set.title<-"Union B"
# set.name<-set.title

load("RF_Terms.RData")

source("/home/arc1/R Projects/Text Mining Weak Signals/Rising and Falling Terms/RF_Brew_Core.R")


