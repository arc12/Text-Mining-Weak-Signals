ReadMe file for "Rising and Falling Terms" directory. The contents of this are all about using a simple (Naive) text mining approach to look for "weak signals". The current version considers conference abstracts but the code should be easily adapted to work with blog sources also.

The files in this directory are the worker files for use with R [R Development Core Team (2010). R: A language and environment for statistical  computing. R Foundation for  Statistical Computing, Vienna, Austria. ISBN   3-900051-07-0, URL http://www.R-project.org/.]. Sub-directories MOSTLY contain either:
a) initialiser R code for a single conference
b) initialiser R code to merge abstract texts from several conferences (aka "union" run)


The initialisers are:
* RF_Init.R -  this is the for a conference or "union" and sets up variables necessary for the RF_Terms.R worker code, which does the analysis and writes out an .RData file and PNG images of the plots.
* RF_Conference_Brew.R - takes the .RData and uses Brew with the template file "Conference BrewTemplate.html" to create an HTML report.

"Conference BrewTemplate ODT content.xml" is a rather poor experimental attempt at using Brew to generate an ODT report. It relies on files in "Conference BrewTemplate ODT files" and a small script "Prepare ODT Report.sh" in the conference/union subdirectory.
