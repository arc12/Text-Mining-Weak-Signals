#makes the inproceedings.xml file be "proper". Also removes unhelpful HTML tags as mixed content is unhelpful!
echo "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n<root>" > ./inproceedings.tmp
cat "ICHL inproceedings.xml" | sed -e 's/<i>/ /'  -e 's/<\/i>/ /'  >> ./inproceedings.tmp
echo "\n</root>" >> ./inproceedings.tmp
mv "ICHL inproceedings.xml" "ICHL inproceedings.old"
mv inproceedings.tmp "ICHL inproceedings.xml"
