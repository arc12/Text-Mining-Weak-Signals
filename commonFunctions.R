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
## common functions for RF_Terms. No attempt is made to make these generic utility functions (a job for a rainy day or another life)
##

# Given a corpus, extra metadata and a list of document IDs, extract document metadata and content into a dataframe
# and optionally print out the extracted info
#ExtraMeta is a data frame with Doc IDs as row names, columns for max betweenness and Std Novelty
ExtractDocs<-function(Corp, ExtraMeta, DocIds, Print=TRUE){
   empty.field.c<-rep(NA,length(DocIds))
   df<-data.frame(origin=empty.field.c, date=empty.field.c,
         heading=empty.field.c, authors=empty.field.c, id=empty.field.c,
         url=empty.field.c, dblp_url=empty.field.c,
         abstract=empty.field.c, std.novelty=empty.field.c, positive=empty.field.c,
         negative=empty.field.c, subjectivity=empty.field.c, max.betweenness=empty.field.c,
         stringsAsFactors=FALSE)
   jj<-1
   for (j in DocIds){
      #BEWARE order is critical!
      df[jj,]<-c(as.character(meta(Corp[[j]], tag="Origin")),
         as.character(meta(Corp[[j]], tag="DateTimeStamp")), as.character(meta(Corp[[j]], tag="Heading")),
         as.character(meta(Corp[[j]], tag="Author")), as.character(meta(Corp[[j]], tag="ID")),
         as.character(meta(Corp[[j]], tag="URL")), as.character(meta(Corp[[j]], tag="DBLP_URL")),
         as.character(Corp[[j]]),
         as.numeric(ExtraMeta[j,"StdNovelty"]),
         as.numeric(meta(Corp[[j]], tag="Positive")),
         as.numeric(meta(Corp[[j]], tag="Negative")),
         as.numeric(meta(Corp[[j]], tag="Subjectivity")),
         as.numeric(ExtraMeta[j,"MaxBetweenness"]))
      if(Print){
         print("")
         print(df[jj,"heading"])
         print(paste("Metrics: std. novelty=", df[jj,"std.novelty"],
              "  + sentiment=", df[jj,"positive"],
              "  - sentiment=", df[jj,"negative"],
              "  subjectivity=", df[jj,"subjectivity"],
              "  max betweenness=",df[jj,"max.betweenness"],sep=""))
         print(paste(df[jj,"origin"],df[jj,"date"],", ",df[jj,"authors"],", ", "ID=", df[jj,"id"], sep=""))
         print(df[jj,"abstract"])
      }
      jj<-jj+1
   }

   return(df)
}

LogTerms<-function(fileName, terms, words=NULL){
   sink(file=fileName, append=FALSE, type="output", split=TRUE)
   cat(paste("c(\"",paste(terms,collapse="\",\""),"\")",sep=""))
   cat("\n")
   if(!is.null(words)){
      cat(paste("c(\"",paste(words,collapse="\",\""),"\")",sep=""))
   }
   sink()
}

CustomStopwords<-function(){
   #+  "paper" (which is common in journal/proceedings abstracts!)
   SW<-c(stopwords(language = "en"),"paper","studentspsila")
   #- some terms (and various expansions) that are relevant to the education domain
   SW<-SW[-grep("group", SW)]
   SW<-SW[-grep("problem", SW)]
   SW<-SW[-grep("present", SW)]
   SW<-SW[-grep("work", SW)]
   return(SW)
}
