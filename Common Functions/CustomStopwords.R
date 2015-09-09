## ***Made available using the The MIT License (MIT)***
# Copyright (c) 2012, Adam Cooper
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
## ************ end licence ***************

require("tm", quietly=TRUE)
CustomStopwords<-function(){
   #+  "paper" (which is common in journal/proceedings abstracts!)
   SW<-c(stopwords(kind = "en"),"paper","studentspsila","conference",
         "january","february","march","april","may","june",
         "july","august","september","october","november","december",
         "jan","feb","mar","apr","jun","jul","aug","sept","oct","nov","dec",
         "pixelmaid","onomatopoeia","pizzaro","kali","ignatius","grizzla", "iggi")
   #- some terms (and various expansions) that are relevant to the education domain
   SW<-SW[-grep("group", SW)]
   SW<-SW[-grep("problem", SW)]
   SW<-SW[-grep("present", SW)]
   SW<-SW[-grep("work", SW)]
   return(SW)
}