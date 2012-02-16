##
## Take a previous run of RF_Terms.R and create a HTML report
##
library("tm")

# Load the RF_Terms workspace
setwd("/home/arc1/R Projects/Text Mining Weak Signals Output/Rising and Falling Terms/TELBlogs/CETIS 2012-02")
load("RF_Terms.RData")

##
## Run parameter
##
show.abstracts<-TRUE

source("/home/arc1/R Projects/Text Mining Weak Signals/Rising and Falling Terms/RF_Brew_Core.R")


