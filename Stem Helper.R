##
## simple helper to find the stemmed forms of candidate words and also to determine the shortes form in a corpus
##

library("tm")

#where to find a corpus
load("/home/arc1/R Projects/Text Mining Weak Signals Output/Rising and Falling Terms/Union C/2011/Corpus.RData")

#space-separated list of words to stem
#look.for<-"LMS VLE LCMS E-Portfolio Games Gesture Metadata Adaptive Open Social Ubiquitous Semantic Agents Cloud Broadband Video"
look.for<-"cloud virtualisation virtualization saas paas"
look.for<-"ebook etextbooks"
look.for<-"analytics analytical analysis data"
look.for<-"gesturebased gestural"
look.for<-"context contextsensitive contextaware contextenriched location locationbased locationaware geospatial"
look.for<-"Game gaming gamification game-based game-play"
look.for<-"Immersive Standards OSS Blog Twitter Wiki Tablet Smartphone Mobile Streaming"

#look.for<-tolower(removePunctuation(look.for))
look.for.vec<- unlist(strsplit(look.for," "))
look.for.stems<-stemDocument(tolower(removePunctuation(look.for.vec)))

#now lookup shortest and prevalent forms leading to stem within the corpus
shortest.words<-stemCompletion(look.for.stems,corp,type="shortest")
completion.fails<-is.na(shortest.words) | shortest.words==""
shortest.words[completion.fails]<-names(shortest.words[completion.fails])
prevalent.words<-stemCompletion(look.for.stems,corp,type="prevalent")
completion.fails<-is.na(prevalent.words) | prevalent.words==""
prevalent.words[completion.fails]<-names(prevalent.words[completion.fails])

#some output for easy cut/paste
print("Looked-for words:")
print(paste("c('",paste(look.for.vec,collapse="','"),"')", sep=""))

print("Stemmed words:")
print(paste("c('",paste(look.for.stems,collapse="','"),"')", sep=""))

print("Shortest words in the given corpus to match these stems:")
print(paste("c('",paste(shortest.words,collapse="','"),"')", sep=""))
print("Prevalent words in the given corpus to match these stems:")
print(paste("c('",paste(prevalent.words,collapse="','"),"')", sep=""))
