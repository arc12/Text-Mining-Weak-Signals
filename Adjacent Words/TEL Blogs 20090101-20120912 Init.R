
## Run Properties - dependent on the source
base.dir<-"/home/arc1/R Projects/Text Mining Weak Signals"
source.dir<-paste(base.dir,"Source Data",sep="/")
set.name<-"TEL Blogs 20090101-20120912" #<<<<<<<<<<<<< edit me - for the output dir
output.dir<-paste("/home/arc1/R Projects/Text Mining Weak Signals Output/Adjacent Words",set.name,sep="/")
#brew.dir<-paste(base.dir,"AdjacentWords",sep="/")

dir.create(output.dir, showWarnings=TRUE)
setwd(output.dir)

## SOURCE DATA SELECTION
# Either a) [DEPRECATED] list csv files "with metrics" as produced by Pre-process.R These are combined into one corpus or
#        b) Locate a SQLite Database and define a query to extract
set.csv<-NA
sqlite.filename<-"TMWS Data A.sqlite" #set this to NA to use [deprecated] option a
sql<-"SELECT content,datestamp FROM blog_post WHERE datestamp BETWEEN '2009-01-01' AND '2012-09-12'"
use.sqlite<-!is.na(sqlite.filename)

title<-"TEL Blogs Jan 2009 - Mid Sept 2012"

data.type<-"b" #a = conference abstracts, b = blogs

#whether to split the dataset by year and create separate plots for each
split.years=TRUE

# allow for multiple words to be the "word in focus", around which the previous/following words are counted. a compound word is OK (e.g. "learning object")
focus.words=c("cloud")
focus.placeholder<-"cloud" #used as the label in the visualistion to represent the focus word(s)

#the quantile to be applied to adjacent word frequency when visualising (only cases above the cut appear)
cut.quantile<-0.99
cut.quantile.split<-0.98 #used for a split-off year. often smaller than cut.quantile otherwise they are sparse

source(paste(base.dir,"Adjacent Words/AdjacentWords.R", sep="/"))