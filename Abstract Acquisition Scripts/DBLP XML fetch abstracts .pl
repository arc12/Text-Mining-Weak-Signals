#!/usr/bin/perl
## Use XML as extracted from DBLP XML dump as a source of the metadata and a URL
## from which to retrieve a HTML page containing (among other things) the abstract
## Extract the abstract and compose a CSV output along with the metadata from the XML
## ******** designed to work with IEEE ICALT, Elsevier/Sciencedirect CAL and Springer EC-TEL, ICWL, ICHL proceedings 

##TOTAL FUCKERS - SPRINGER HAVE CHANGED IT SO THAT ABSTRACTS ARE CONCEALED, THIS NO LONGER WORKS

##NB: some older papers (especially) in DBLP do not have <ee> content

use warnings;
use XML::Simple;
use LWP::Simple;
use Data::Dumper;

#preparatory stuff - the agent to fetch from the URL and prep an output file
my $ua = LWP::UserAgent->new();
$ua->agent("Mozilla/4.0");

my $url_found =0;
my $abs_got = 0;
my $no_url = 0;
my $booktitle="";
my $publisher="";
my $min_year = 2012;
my $max_year = 2012;
my $conf = "ICALT";
my $infile = "../Source Data/Raw and Part-Processed/".$conf."/".$conf." inproceedings ".$max_year.".xml";
my $outfile = "../Source Data/Abstracts/".$conf." Abstracts ".$min_year."-".$max_year.".csv";
print "Starting - acquire abstracts $min_year -> $max_year \n";

unlink $outfile;
open(OUTFILE, ">".$outfile);
#uncomment the following line UNLESS intending to append the outfile to an existing CSV
#print OUTFILE "year,pages,title,authors,abstract,keywords,url,dblp_url\n";

# Read in the XML and start looping over <inproceedings> elements
# forcearry ensures that "author" is always read in as an array even if only 1 author element
# the "keyattr" voodoo prevents the @key attribute being interpreted as an indexing-in term
my $xml = XMLin($infile, forcearray => [ 'author' ], keyattr => []);
#print Dumper ($xml); #for inspecting/testing contents

#this gets the first item only (test) @{$xml->{inproceedings}}[0]

foreach my $item (@{$xml->{inproceedings}}){
	# get the metadata
	$year = $item->{year};
	# skip 2001, 2003, 2004 since experience shows there is either no "ee" URL or the resolved page does not contain an abstract
	if($year < $min_year || $year > $max_year){
		print "skipping $year\n";
		next;
	}
	if($item->{pages}){
		$pages = $item->{pages};
	}else{
		$pages="";
	}
	$title = $item->{title};	
	$authors = join(', ', @{$item->{author}});
	$booktitle = $item->{booktitle};
    for ($booktitle) {
	   	if    (/ICALT/)  {$publisher = "IEEE";}
	    elsif (/EC-TEL/)  {$publisher = "Springer"; }
	    elsif (/ICWL/)  {$publisher = "Springer";}
	    elsif (/ICHL/)  {$publisher = "Springer";}
	    elsif (/Computers & Education/) {$publisher = "Sciencedirect";}
	    else            {$publisher = "unknown";}
	}
	
	#Does this script know how to scrape? (is the publisher known)
	if($publisher=~m/"unknown"/){
		print "Cannot process; publisher not known for title \"$booktitle\"\n";
	}else{
		#print "Publisher: $publisher\n";
		# the <ee> element contains a DOI URL we can use to get the abstract e.g. http://dx.doi.org/10.1109/ICALT.2010.13
		if($item->{ee}){
			$url_found++;
			my $url = $item->{ee};#this is [usually] a DOI URL to publisher page giving abstract
			my $dblp_url = $item->{url};#this is to a page on DBLP (which is used in the AERCS database)
			print "Fetching $url\n";
			#the DOI request is redirected, this will happen automatically...
			my $request = HTTP::Request->new(GET => $url);
			my $response = $ua->request($request);
			my $abstract = "";
			my $donext=0;
			#guard against no response
			if ($response->is_success) {
				# A DIFFERENT SCRAPER FOR EACH PUBLISHER (which is indicated by the <booktitle> element
				for ($publisher) {
					if    (/Springer/){
						#Springer hides their abstract - only shown using ajax on a "show..." click.
						#However, we can frig the URL to fetch what ajax does
						my $newurl = $response->request()->url();
						$newurl =~ s/\?MUD=MP/primary/;
						my $newrequest = HTTP::Request->new(GET => $newurl);
						my $response = $ua->request($newrequest);
						my @html = split /\n/, $response->content();
						if ($response->is_success) {
							foreach my $line (@html){ #reads line by line from response content
								#print "L:". $line ."\n\n";
								if($donext == 1 && $line =~ /<\/div>/){
									$abs_got++;
									last;
								}
								if($line =~ /class=\"Abstract\".*>(.*)/){
									$line=$1;
									$donext=1;							
								}
								if($donext == 1){
									$line =~ s/<.*?>//g;#remove html tags
									$line=~s/^\s+//; #remove leading spaces
									chomp($line);
									$abstract .= $line . " ";
								}
							}
							
							#print "ABS:". $abstract . "\n\n";
						}		
					}
					elsif (/Sciencedirect/){
						#scrape the abstract directly from the HTTP response
						my @html = split /\n/, $response->content();
						foreach my $line (@html){ #reads line by line from response content
							#WAS PREVIOUSLY if($line =~ /<div><h3 class=\"h3\">Abstract<\/h3>(.*)<\/p><\/div>/){
							if($line =~ /<div class=\"abstract svAbstract\"><h2 class=\"secHeading\" id=\"section_abstract\">Abstract<\/h2>(.*)<\/p><\/div>/){
								$abstract = $1;
								$abstract =~ s/<.*?>//g;#remove html tags
								$abstract=~s/^\s+//; #remove leading spaces
								chomp($abstract);
								$abs_got++;
								last;
							}
						}
					}						
					elsif (/IEEE/){
						#scrape the abstract directly from the HTTP response
						#since 2012 everything is on one line. This makes sure divs start on a new line otherwise later select fails
						my $responseContent = $response->content();
						$responseContent =~ s/<div\s/\n<div\s/g;
						my @html = split /\n/, $responseContent;
						foreach my $line (@html){ #reads line by line from response content
#							print ">>> $line \n";
							$line =~ s/\r//; #get \r in 2008 and 2009!
							#This works with one style (2008)
							if($donext == 1){
								$line =~ s/<.*?>//g;#remove html tags
								$line=~s/^\s+//; #remove leading spaces
								chomp($line);
								$abstract = $line;
								$abs_got++;
								last;
							}
							if($line =~ /<h2>Abstract<\/h2>/){
								$donext=1;
							}
							#This works with another style (the main one)
							if($line =~ /class=\"abs-articlesummary\"/){
								$line =~ s/<.*?>//g;#remove html tags
								$line=~s/^\s+//; #remove leading spaces
								chomp($line);
								$abstract = $line;
								$abs_got++;
								last;
							}
						}
					}
					else{
						print "**** error ****\n";#this should never happen
					}
				}     
				
				
				$abstract=~s/^\s+//; #remove leading spaces
				$abstract=~s/\s+$//; #remove trailing spaces			
				#deal with pesky symbols - left and right single quote marks, normal quotes etc
				#a general s/\&.*;// would be too risky
				$abstract =~ s/\"//g;
				$abstract =~ s/'//g; #occurs as students' and later gets converted to &psila; leading to studentspsila in the TM
				$abstract =~ s/\&nbsp;/ /g;
				$abstract =~ s/\&amp;/and/g;
				$abstract =~ s/\&ndash;/-/g;
				$abstract =~ s/\&.5;//g;
				$abstract =~ s/\&.4;//g;
				$abstract =~ s/\&.3;//g;
				$abstract =~ s/\&.2;//g;
				$abstract =~ s/\&#.*;//g;
				
				$abstract =~ s/\&lsquo;//g;
				$abstract =~ s/\&rsquo;//g;
				$abstract =~ s/\&ldquo;//g;
				$abstract =~ s/\&rdquo;//g;
				#$abstract =~ s/\&psila;//g;
				#$abstract =~ s/\&trade;//g;
				#$abstract =~ s/\&bull;//g;
				$title =~ s/\"//g;

				print "$year $title \n $abstract\n";
				print OUTFILE "$year,$pages,\"$title\",\"$authors\",\"$abstract\",\"\",$url,\"$dblp_url\"\n"
			}else{
				print "Fetch of DOI URL ". $url . " failed\n\n";
			}
		}else{
			print "No DOI URL found: $year $title\n\n";
			$no_url++;
		}
	}
}
close OUTFILE;
print "\nSUMMARY: $url_found URLs found, from which $abs_got abstracts were recovered. $no_url items had no URL to retrieve\n";
