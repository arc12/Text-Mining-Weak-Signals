#makes the inproceedings.xml file be "proper". Also removes unhelpful HTML tags as mixed content is unhelpful!
#echo "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n<root>" > ./inproceedings.tmp
#cat "ECTEL inproceedings 2012.xml" | sed -e 's/<i>/ /'  -e 's/<\/i>/ /'  >> ./inproceedings.tmp
#echo "\n</root>" >> ./inproceedings.tmp
#mv "ECTEL inproceedings 2012.xml" "ECTEL inproceedings 2012.old"
#mv inproceedings.tmp "ECTEL inproceedings 2012.xml"

echo "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n<root>" > ./inproceedings.tmp
cat "ICALT inproceedings 2012.xml" | sed -e 's/<i>/ /'  -e 's/<\/i>/ /'  >> ./inproceedings.tmp
echo "\n</root>" >> ./inproceedings.tmp
mv "ICALT inproceedings 2012.xml" "ICALT inproceedings 2012.old"
mv inproceedings.tmp "ICALT inproceedings 2012.xml"

echo "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n<root>" > ./inproceedings.tmp
cat "ICWL inproceedings 2012.xml" | sed -e 's/<i>/ /'  -e 's/<\/i>/ /'  >> ./inproceedings.tmp
echo "\n</root>" >> ./inproceedings.tmp
mv "ICWL inproceedings 2012.xml" "ICWL inproceedings 2012.old"
mv inproceedings.tmp "ICWL inproceedings 2012.xml"

echo "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n<root>" > ./inproceedings.tmp
cat "ICHL inproceedings 2012.xml" | sed -e 's/<i>/ /'  -e 's/<\/i>/ /'  >> ./inproceedings.tmp
echo "\n</root>" >> ./inproceedings.tmp
mv "ICHL inproceedings 2012.xml" "ICHL inproceedings 2012.old"
mv inproceedings.tmp "ICHL inproceedings 2012.xml"
