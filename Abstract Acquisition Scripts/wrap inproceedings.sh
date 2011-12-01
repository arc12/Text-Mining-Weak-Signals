#makes the inproceedings.xml file be "proper". Also removes unhelpful HTML tags as mixed content is unhelpful!
echo "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n<root>" > ./inproceedings.tmp
cat "ECTEL inproceedings.xml" | sed -e 's/<i>/ /'  -e '/<\/i>/ /'  >> ./inproceedings.tmp
echo "\n</root>" >> ./inproceedings.tmp
mv "ECTEL inproceedings.xml" "ECTEL inproceedings.old"
mv inproceedings.tmp "ECTEL inproceedings.xml"

echo "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n<root>" > ./inproceedings.tmp
cat "ICALT inproceedings.xml" | sed -e 's/<i>/ /'  -e '/<\/i>/ /'  >> ./inproceedings.tmp
echo "\n</root>" >> ./inproceedings.tmp
mv "ICALT inproceedings.xml" "ICALT inproceedings.old"
mv inproceedings.tmp "ICALT inproceedings.xml"

echo "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n<root>" > ./inproceedings.tmp
cat "ICWL inproceedings.xml" | sed -e 's/<i>/ /'  -e '/<\/i>/ /'  >> ./inproceedings.tmp
echo "\n</root>" >> ./inproceedings.tmp
mv "ICWL inproceedings.xml" "ICWL inproceedings.old"
mv inproceedings.tmp "ICWL inproceedings.xml"

echo "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n<root>" > ./inproceedings.tmp
cat "ICHL inproceedings.xml" | sed -e 's/<i>/ /'  -e '/<\/i>/ /'  >> ./inproceedings.tmp
echo "\n</root>" >> ./inproceedings.tmp
mv "ICHL inproceedings.xml" "ICHL inproceedings.old"
mv inproceedings.tmp "ICHL inproceedings.xml"
