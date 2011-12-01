== Scripts etc to acquire abstracts for various conferences ==
Requires use of BaseX

Notes on how to do it follow:

Download the DBLP master XML file and unzip

In BaseX, create a new DB, setting the option to use the internal parser and to parse entities (not doing this causes a barf due to too many entities)

Use one of the saved XQueries (.xq file) to create an extract: save this

Add an xml decl and root element to it (this can be done with the "wrap inproceedings.sh":
<?xml version="1.0" encoding="ISO-8859-1"?>
<root>
...
</root>

NB: CAL requires some additional processing to fudge the XML element names to "inproceedings" style - see "wrap inproceedings CAL.sh"

Run the PERL "DBLP XML fetch abstracts.pl", having set the file locations in it

Import into LibreOffice as UTF-8
NB sometimes it seems necessary to set the encoding to Western Europe (ISO-8859-1) since this is what comes through, originally from DBLP (this may have been caused by incorrect XML decl addition to inproceedings in the past)

NB!!!!!!!!!
Since CAL proceedings appear rather long after the conference, the data in DBLP is likely to be +1 year. CHeck and edit in the CSV!!!!!!!
(this may also apply to ICWL since it is late in the year)

