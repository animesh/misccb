#!/usr/local/bin/perl -T
# $Id: multipipmaker.pl,v 1.26 2006/10/30 18:39:17 rico Exp $

use 5.006_000;
use warnings;
use strict;

use CGI qw/:standard/;
use CGI::Carp 'fatalsToBrowser';
# use CGI::Pretty qw( :html3 );

my $twid = 45;
my $fwid = 55;
my $maxlen = 128;
my $maxnum = 20; # 99 is max possible

sub introduction
{
  print header, "\n";
  print start_html("MultiPipMaker"), "\n";
  print p,
    i("MultiPipMaker"), "\n(",
    a({href=>"/pipmaker/mpm-instr.html"},"instructions"),
    ")\n",
    "aligns several DNA sequences and returns a percent identity plot",
    " of that alignment.",
    "\n";
}

sub conclusion
{
  print hr, "\n";
  print a({href=>"/pipmaker/privacy.html"}, "Privacy policy.");
  print p, "\n";
  print a({href=>"mailto:&#60;pipmaster&#64;bio.cse.psu.edu&#62;"},
          "Email the authors.");
  print p, "\n";
  print "Development and maintenance of PipMaker is supported by grant\n";
  print "HG02238 from the National Human Genome Research Institute.\n";

  #print "Development and maintenance of PipMaker is supported by grant\n";
  #print "LM05110 from the National Library of Medicine.\n";

  print p, "\n";
  print "If you publish results obtained using PipMaker, please cite ";
  print a({href=>"/pipmaker/papers/genome-research/"},
	"Schwartz et al., Genome Research 10:577-586, April 2000.");
  print end_html, "\n";
}

sub begin_the_form
{
  my $cgi = shift;
  print p,
    start_form(-method=>"POST", -enctype=>&CGI::MULTIPART,
	      -action=>"/cgi-bin/$cgi/?"),
    "\n";
}
sub end_the_form
{
  print submit('Submit','Submit');
  print reset, "\n";
  print end_form, "\n";
}

sub choose_how_many
{
  introduction();
  begin_the_form("multipipmaker");
  print br, "\n", "How many sequences? [2..$maxnum] ",
	textfield(-name=>'num',
			-default=>'', -size=>5, -maxlength=>$maxlen),
	br, "\n";
  endform();
  conclusion();
  exit 0;
}

# main...

my $num = param('num');
if (!defined($num) || $num !~ /^\d+$/ || $num < 2) {
  choose_how_many;
}

if ($num > $maxnum) {
  die "Sanity check: $num sounds like too many sequences.\n";
}
 
introduction();
begin_the_form("mpm-submit");

my $fasta = "(" . b("FASTA") . " format; " . b("plain text only") . ")";
print hidden(-name=>'num', -default=>$num), "\n";

print "Your email address: (pip output will be mailed to you)", br, 
 textfield(-name=>'email', -default=>'', -size=>64, -maxlength=>$maxlen),
 br, p, "\n";

# print "Pick one:\n";
# print radio_group(-name=>'genpdf', -values=>['0','1'], -default=>'1',
#	-labels=>{'0' => "Generate PostScript", '1' => "PDF"});
# print br, "\n";

print "Options: ", br, "\n";
print hidden(-name=>'genpdf', -default=>1), "\n";
print hidden(-name=>'genpip', -default=>1), "\n";

print radio_group(-name=>'genwww', -values=>['0','1'], -default=>'1',
	-labels=>{'0' => "Retrieve results with email",
	          '1' => "Retrieve results with WWW"});
print br, "\n";

print checkbox_group(-name=>'genmaps', -values=>['1'], -default=>['0'],
	-labels=>{'1' => "Generate nucleotide level view (PDF)"});
print br, "\n";

print checkbox_group(-name=>'gentext', -values=>['1'], -default=>['0'],
	-labels=>{'1' => "Generate very verbose text (ASCII, compressed)"});
print br, "\n";

print checkbox_group(-name=>'genuly', -values=>['1'], -default=>['0'],
	-labels=>{'1' => "Retrieve underlay files generated for the overview"});
print br;
print <<'END';
 Warning: this will be at least as large as all the data you submit.
Unless your system administrator has configured your mail software
to accept huge messages, the email will bounce and you will never
see any output.
END
print br, "By ";
print checkbox_group(-name=>'gengzip',
	 -values=>['1'], -default=>['1'], -labels=>{'1'=>"default"});
print " we compress this file using ";
print a({href=>"http://www.gzip.org/"}, "gzip");
print ".", br, p, "\n";

print "Your data: ", br, "\n";
print ul(
  "\n",
  li("First sequence $fasta:",
     br, "\n",
     "label:", textfield(-name=>'seq01name',
			-default=>'', -size=>$twid, -maxlength=>$maxlen),
     br, "\n",
     "sequence filename:", filefield(-name=>'seq01data',
			-default=>'', -size=>$fwid, -maxlength=>$maxlen),
     br, "\n",
     "mask filename:", filefield(-name=>'seq01mask',
			-default=>'', -size=>$fwid, -maxlength=>$maxlen),
     br, "\n",
     "exons filename:", filefield(-name=>'exons',
			-default=>'', -size=>$fwid, -maxlength=>$maxlen),
     br, "\n",
     "underlay filename:", filefield(-name=>'underlay',
			-default=>'', -size=>$fwid, -maxlength=>$maxlen),
     checkbox_group(
	    -name=>"dfltulay",
            -values=>['1'], -default=>['0'],
            -labels=>{'1' => "use as default in pip "}
     ),
     br, "\n",
     "annotation filename:", filefield(-name=>'annotation',
			-default=>'', -size=>$fwid, -maxlength=>$maxlen),
     br),
  "\n", br,
  &nth_seq($num)
);
print "\n";

print p;


print br, "\n";

end_the_form();
conclusion();
exit 0;

sub nth_seq {
  my $n = shift;
  my $i;
  my $s = "";

  for ($i = 2; $i <= $n; ++$i) {
     $s .= 
       li("sequence #$i $fasta:",
         br, "\n",
         "label:", 
         textfield(-name=>sprintf("seq%02dname",$i),
		     -default=>'', -size=>$twid, -maxlength=>$maxlen),
         br, "\n",

         "filename:", 
         filefield(-name=>sprintf("seq%02ddata",$i),
		     -default=>'', -size=>$fwid, -maxlength=>$maxlen),
	 br, "\n",

         "underlay filename:",
	 filefield(-name=>sprintf("seq%02dunderlay",$i),
		     -default=>'', -size=>$fwid, -maxlength=>$maxlen),
         br, "\n",

	 "Pick one: ",
	 radio_group(
	    -name=>sprintf("seq%02dstrand",$i),
	    -values=>['0','1'], -default=>'1',
	    -labels=>{'0' => "Search one strand",
		      '1' => "Search both strands"}
	 ),
	 br, "\n",

	 "Pick one: ",
	 radio_group(
	    -name=>sprintf("seq%02dchain",$i),
	    -values=>['0','2','3'], -default=>'0',
	    -labels=>{'0' => "Show all matches",
		      '2' => "Chaining",
		      '3' => "Single coverage", }
	 ),
	 br, "\n",

	 "Select: ",
	 checkbox_group(
	    -name=>sprintf("seq%02dsensitivity",$i),
	    -values=>['1'], -default=>['0'],
	    -labels=>{'1' => "High sensitivity and low time limit"}
	 ),
	 br, "\n"
    ) . br . "\n";
  }
  return $s;
}
