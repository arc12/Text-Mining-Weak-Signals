##
## Take a previous run of RF_Terms.R and create a HTML report
## THIS VERSION refers to the conference abstracts template
##
library("tm")
#library("Snowball")
library("brew")

# Load the RF_Terms workspace
setwd("/home/arc1/R Projects/Text Mining Weak Signals Output/Rising and Falling Terms/Union B/testing")
load("RF_Terms.RData")

# the template is assumed to be in the parent directory since this script is assumed to be in a conference-specific directory
show.abstracts<-TRUE
brew(file="/home/arc1/R Projects/Text Mining Weak Signals/Rising and Falling Terms/Conference BrewTemplate.html",output="Report.html",run=TRUE)
#brew(file="/home/arc1/R Projects/Text Mining Weak Signals/Rising and Falling Terms/Conference BrewTemplate ODT content.xml",output="ODT content.xml",run=TRUE)
# add one for ODP (als need to fix the template for ODT; it comes out rather mashed)
