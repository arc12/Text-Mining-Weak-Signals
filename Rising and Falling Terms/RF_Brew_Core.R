##
## Take a previous run of RF_Terms.R and create a HTML report
##           MUST be preceeded by a load the RF_Terms workspace
##           SHOULD be preceeded by setting of working dir to appropriate report destination
## THIS VERSION refers to the combined blogs and conference abstracts template
##
library("brew")

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
   h1<-paste(title, conf.year.recent)
   doc.pseudonym<-"abstract"
   doc.Pseudonym<-"Abstract"
   docs.pseudonym.long<-"abstracts of the conference proceedings"
}

# shortens a string at the closest space character to the limit "len" and appends ellipsis if necessary
# used from within Brew template
Head.Text<-function(string2shorten, len=800){
   if(nchar(string2shorten)<=len){
      return (string2shorten)
   }
   res<-substring(string2shorten,1,len)
   if((substr(string2shorten,len,len)!=" ") & (substr(string2shorten,len,len)!=" ")){
      #adjust to word boundary
      res<-substr(res,1,regexpr("\\s+\\S*$", res))
   }
   return (paste(res,"...")   )
}

brew(file="/home/arc1/R Projects/Text Mining Weak Signals/Rising and Falling Terms/CB BrewTemplate.html",output="Report.html",run=TRUE)
#rew(file="/home/arc1/R Projects/Text Mining Weak Signals/Rising and Falling Terms/Conference BrewTemplate ODT content.xml",output="ODT content.xml",run=TRUE)
# add one for ODP (als need to fix the template for ODT; it comes out rather mashed)
