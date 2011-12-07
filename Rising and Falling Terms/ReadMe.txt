ReadMe file for "Rising and Falling Terms" directory. The contents of this are all about using a simple (Naive) text mining approach to look for "weak signals". The current version considers conference abstracts but the code should be easily adapted to work with blog sources also.

The files in this directory are the worker files for use with R [R Development Core Team (2010). R: A language and environment for statistical  computing. R Foundation for  Statistical Computing, Vienna, Austria. ISBN   3-900051-07-0, URL http://www.R-project.org/.]. Sub-directories MOSTLY contain either:
a) initialiser R code for a single conference
b) initialiser R code to merge abstract texts from several conferences (aka "union" run)


The initialisers are:
* RF_Init.R -  this is the for a conference or "union" and sets up variables necessary for the RF_Terms.R worker code, which does the analysis and writes out an .RData file and PNG images of the plots.
* RF_Conference_Brew.R - takes the .RData and uses Brew with the template file "Conference BrewTemplate.html" to create an HTML report.

"Conference BrewTemplate ODT content.xml" is a rather poor experimental attempt at using Brew to generate an ODT report. It relies on files in "Conference BrewTemplate ODT files" and a small script "Prepare ODT Report.sh" in the conference/union subdirectory.

----------
## **** notes on importing co-occurrence graph into Gephi (v0.8 alpha used)
# import nodes then edges from CSV files. Make Node Weight be a float [it is ESSENTIAL not to leave it as a String]
# show node labels, use "statistics" to calculate modularity
# Use "ranking" to set node size = imported weight 
# edge size = imported weight (=number of co-occurrences). by default
# Use "partition" to set node colour = modularity class
# NB the actual scale may need a multiplier/factor to be applied.
# Use a circular auto-layout with nodes ordered by modularity class then use Frucherman Reingold
# - may need to do a label adjust too.
# ** for the "preview"
# - set edge thickness to 5 and label font to 36pt. curved edges work OK
# - uncheck "proportional size" on node and edge
# - set the opacity to somewhere between 60 and 80% so that labels show up better
# - when exporting to PNG, set a 25% margin otherwise it gets cropped!
