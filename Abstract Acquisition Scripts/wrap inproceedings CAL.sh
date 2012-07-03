#makes the inproceedings.xml file be "proper"
#also changes the XML from <article>...</article> form to the <inproceedings> form
# Also removes unhelpful HTML tags as mixed content is unhelpful!
echo "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n<root>" > ./inproceedings.tmp
cat "CAL inproceedings 2011.xml" | sed -e 's/<article/<inproceedings/' -e 's/<\/article/<\/inproceedings/' -e 's/journal>/booktitle>/g' -e 's/<i>/ /'  -e 's/<\/i>/ /'  >> ./inproceedings.tmp
echo "\n</root>" >> ./inproceedings.tmp
mv "CAL inproceedings 2011.xml" "CAL inproceedings 2011.old"
mv inproceedings.tmp "CAL inproceedings 2011.xml"


