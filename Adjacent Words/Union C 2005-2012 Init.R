
## Run Properties - dependent on the source
base.dir<-"/home/arc1/R Projects/Text Mining Weak Signals"
source.dir<-paste(base.dir,"Source Data",sep="/")
set.name<-"Union C 2006 to 2012"
output.dir<-paste("/home/arc1/R Projects/Text Mining Weak Signals Output/Adjacent Words",set.name,sep="/")
#brew.dir<-paste(base.dir,"AdjacentWords",sep="/")

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
sql<-"SELECT year, title, abstract FROM abstract WHERE year BETWEEN '2005' AND '2012'"
use.sqlite<-!is.na(sqlite.filename)

title<-"Conference Proceedings from ICALT, ECTEL, CAL, ICHL and ICWL"

data.type<-"a" #a = conference abstracts, b = blogs

#whether to split the dataset by year and create separate plots for each
split.years=TRUE

# allow for multiple words to be the "word in focus", around which the previous/following words are counted. a compound word is OK (e.g. "learning object")
focus.words=c("gesture")
focus.placeholder<-"gesture" #used as the label in the visualistion to represent the focus word(s)

## ADJUST THESE to get "nice" results (otherwise may be over-sparse, "no useful results" or over-dense)
#the quantile to be applied to adjacent word frequency when visualising (only cases above the cut appear)
cut.quantile<-0.5#0.95
cut.quantile.split<-0.0 #used for a split-off year. should be smaller than cut.quantile otherwise they are sparse

source(paste(base.dir,"Adjacent Words/AdjacentWords.R", sep="/"))