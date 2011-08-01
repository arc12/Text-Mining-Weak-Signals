if [ -f ./"ODT content.xml" ]
then
	rm -f "Union Report 2010.odt"
	rm -fr "Union Report ODT"
	mkdir ./"Union Report ODT"
	cp -r ../"Conference BrewTemplate ODT files"/* ./"Union Report ODT"
	cp Images/* ./"Union Report ODT/Pictures"
	mv "ODT content.xml" ./"Union Report ODT"/content.xml
	cd "Union Report ODT"
	zip -r ../"Union Report 2010.odt" *
	cd ..
	#rm -fr "Union Report ODT"
else
     echo 'File ODT content.xml not found. Run Brew.'
fi


