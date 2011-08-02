outbasedir="/home/arc1/R Projects/Text Mining Weak Signals Output/Rising and Falling Terms/Union A/2010"
templatedir="/home/arc1/R Projects/Text Mining Weak Signals/Rising and Falling Terms/Conference BrewTemplate ODT files"
cd "$outbasedir"
if [ -f "ODT content.xml" ]
then
	rm -f "Union A Report 2010.odt"
	rm -fr ODT-Temp
	mkdir ODT-Temp
	cp -r "$templatedir"/* ODT-Temp
	cp Images/* ODT-Temp/Pictures
	mv "ODT content.xml" ODT-Temp/content.xml
	cd "ODT-Temp"
	zip -r ../"Union A Report 2010.odt" *
	cd ..
	rm -fr "ODT-Temp"
else
     echo 'File ODT content.xml not found. Run Brew.'
fi


