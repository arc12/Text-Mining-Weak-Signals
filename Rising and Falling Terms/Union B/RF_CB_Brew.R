##
## Take a previous run of RF_Terms.R and create a HTML report
## THIS VERSION refers to the combined blogs and conference abstracts template
##
library("tm")
library("brew")

# Load the RF_Terms workspace
setwd("/home/arc1/R Projects/Text Mining Weak Signals Output/Rising and Falling Terms/Union B/2010")
source.type="c"#temporary - to allow for use of older .RData
load("RF_Terms.RData")

# the template is assumed to be in the parent directory since this script is assumed to be in a run-specific directory

##
## Run parameter
##
show.abstracts<-FALSE
date.format = "%b %d %Y"

#compute some tet tragments that depend on whether this is a conference or blog case
if(source.type=="b"){
   #blog
   h1<-paste(title, format(last.date, date.format))
   doc.pseudonym<-"blog post"
   doc.Pseudonym<-"Blog post"
   docs.pseudonym.long<-"text content of blog posts"
}else{
   #conference abstracts
   h1<-paste("Conference Scan:", title, conf.year.recent)
   doc.pseudonym<-"abstract"
   doc.Pseudonym<-"Abstract"
   docs.pseudonym.long<-"abstracts of the conference proceedings"
}
brew(file="/home/arc1/R Projects/Text Mining Weak Signals/Rising and Falling Terms/CB BrewTemplate.html",output="_Report.html",run=TRUE)
#rew(file="/home/arc1/R Projects/Text Mining Weak Signals/Rising and Falling Terms/Conference BrewTemplate ODT content.xml",output="ODT content.xml",run=TRUE)
# add one for ODP (als need to fix the template for ODT; it comes out rather mashed)
