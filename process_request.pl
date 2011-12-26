#!@WHICHPERL@
##
## $Id: process_request.pl 1807 2007-05-30 01:34:31Z tbailey $
##
## $Log: process_request.pl,v $
## Revision 1.6.6.1  2006/02/16 23:22:35  nadya
## update path to show the background image on the page.
##
## Revision 1.6  2005/12/01 01:18:53  tbailey
## Add handling of JASPAR searches with MEME motifs.
##
## Revision 1.5  2005/10/02 01:00:10  nadya
## move meme-client and mast-client names into Globals and use variables instead.
##
## Revision 1.4  2005/09/16 01:00:40  nadya
## add csh extention to capture output for the web page in process-request.cgi
##
## Revision 1.3  2005/09/13 18:40:01  nadya
## rm "web" from url string
##
## Revision 1.2  2005/08/07 05:58:26  nadya
## use vairables in Globals for meme locations,
## fix locatio of website.
##
## Revision 1.1.1.1  2005/07/25 23:27:26  nadya
## Importing from meme-3.0.14, and adding configure/make
##
##

use lib qw(@PERLLIBDIR@);
use Globals;

#
# Process a request submitted from a MEME results form.
# Requests include:
#	Submit to MAST
#	View hidden field
#	Submit to BLOCKS
#	Submit to Meta-MEME
#	Print a man page
#
#
# Changes:
# print header ( Content-type:  text/html\n\n in the beginning of the script )
# this fixes the 'Error 500 malformed header from script' error
# 12/12/2002	JKP
# 03/24/2003	TLB; fixed problems with all types of submissions
#		
#
# USER SETTABLE LINKS
#
################################################################################
#
# You can change this if you wish to use a different BLOCKS server
$blocks_url = "http://blocks.fhcrc.org/blocks-bin/process_blocks.pl";
#
# You can change this if you wish to use a different JASPAR server
#$jaspar_root = "http://mordor.cgb.ki.se";
$jaspar_root = "http://asp.ii.uib.no:8090";
$jaspar_url = "$jaspar_root/cgi-bin/jaspar2005/jaspar_db.pl";
#
# You can change this if you wish to use a different Meta-MEME server
#$metameme_url = "http://metameme.sdsc.edu/beta";
$metameme_url = "http://metameme.sdsc.edu";
################################################################################

# requires
# use the CGI package
use CGI qw/:standard/;			# use the CGI package
use HTTP::Request::Common qw(POST);
use LWP::UserAgent;

# get the directories relative to working directory
$root = "..";

#
# get the action (type of submit button pressed) and branch on it
#
$action = param('action');			# which submit button pressed
$nmotifs = param('nmotifs');			# number of motifs
#print header;

if ($action eq "MAST") {
  mast_search($nmotifs);
} elsif ($action =~ /^BLOCKS/) {
  submit_block(1..$nmotifs);
} elsif ($action =~ /^View (\w+) (\d+)/) {
  hidden_field($1, $2);
} elsif ($action =~ /^Submit BLOCK (\d+)/) {
  submit_block($1);
} elsif ($action =~ /^COMPARE PSPM (\d+)/) {
  ($motif_db, $motif_sub_db) = split(/\s+/, param("motif_db_$1"));
  # for backwards compatibility:
  if ($motif_db eq "") { $motif_db = 'JASPAR'; $motif_sub_db = 'CORE'; }
  submit_pspm_to_jaspar($1, $motif_db, $motif_sub_db);
} elsif ($action =~ /^View motif summary/) {
  hidden_field('motif-summary');
} elsif ($action =~ /MetaMEME/){
  submit_metameme();
} elsif ($action =~ /MEME Man Page/){
  print_man("meme");
} elsif ($action =~ /MAST Man Page/){
  print_man("mast");
} else {
  print_header($action);			# print a response header
  print "action: $action\n";
  print "<H1>Feature not implemented yet.</H1>\n";
  exit(1);
}

# all done!
exit(0);

#
# submit a MetaMEME query
#
sub submit_metameme {
  print_header("Submit MetaMEME Query");
  # Get the contents of the MEME form and hash
  foreach $pname (param()) { $params{$pname} = param($pname); }
  # post the contents of the MEME form to MetaMEME
  $ua = LWP::UserAgent->new();
  my $req = POST "$metameme_url/cgi-bin/mhmm_process_request.cgi", [%params];
  my $request = $ua->request($req);
 # $content = $ua->request($req)->as_string;
  $content = $request->content;
  print $content;
} # submit_metameme

#
# Submit a MAST query
#
sub mast_search {
  my($nmotifs) = @_;

  print "Content-type: text/html\n\n";		# start form

  #
  # get the fields MAST needs
  #
  $url = param('url');
  $name = param('name');
  $alphabet = param('alphabet');
  for ($i=1; $i<=$nmotifs; $i++) {		# get the per-motif fields
    $block[$i] = param('BLOCKS'.$i);		# BLOCKS
    $pssm[$i] = param('pssm'.$i);		# pssm
    $pspm[$i] = param('pspm'.$i);		# pspm
  }

  #
  # check input
  #
  if ($nmotifs <= 0) {
    print "<PRE>There are no valid motifs in your MEME file.</PRE>\n";
    exit(1);
  }

  #
  # create a mast submission form by replacing the motif-file field
  # with the in-line motifs
  #

  # open the mast submission form
  open(MASTFORM, "< $root/mast.html") || 
    die("Cannot open MAST submission form mast.html: $!");

  # read the submission form and remove the motifs field and put in inline motifs
  while (<MASTFORM>) {
	if (/INPUT class=\"maininput\" NAME=\"motifs\"/) {# remove this line
      print "MEME results on <B>$name</B>\n";
      # put the inline motifs here
      print "<INPUT TYPE = HIDDEN NAME = inline_name VALUE = \"MEME motifs from sequences in $name\">\n";
      print "<INPUT TYPE = HIDDEN NAME = inline_motifs VALUE = \n\"";
      print "ALPHABET= $alphabet\n";
      for ($i=1; $i<=$nmotifs; $i++) { print(param('pssm'.$i)); }
      print "\">\n";
    } elsif (/^\s*ACTION =/) {			# use specified url for MAST
      print "ACTION = \"$url/cgi-bin/mast.cgi\"\n";
    } elsif (/\/HEAD/) {
      print "<BASE HREF=\"$url/\">";	# base url 
    } else {
      print;					# copy other lines
    }
  }
  close(MASTFORM);
} # mast_search

#
# view a hidden field
#
sub hidden_field {
  my($name, $number) = @_;

  print_header($action);
  if ($name eq "BLOCK") {
    $field = param('BLOCKS'.$number);		# get BLOCK
  } elsif ($name eq "FASTA") {
    $field = param('BLOCKS'.$number);
    $field = block2fasta($field);		# convert to FASTA
  } elsif ($name eq "RAW") {
    $field = param('BLOCKS'.$number);
    $field = block2raw($field);			# convert to raw format 
  } elsif ($name eq "PSSM") {
    $field = param('pssm'.$number);		# get PSSM
  } elsif ($name eq "PSPM") {
    $field = param('pspm'.$number);		# get PSPM
  } elsif ($name eq "motif-summary") {
    $field = param('motif-summary');		# get motif-summary
  } else { 
    print "Unknown hidden field type: $name\n";
    exit(1)
  }
  print "<PRE>$field</PRE>";
} # hidden_field

#
# Convert a BLOCK to RAW sequence format
#
sub block2raw {
  my($block) = @_;
  my($i, @lines, @words, $raw);

  $fasta = "";					# return value
  @lines = split(/\n/, $block); 		# split block into lines
  for ($i = 2; $i<$#lines; $i++) {
    last if $lines[$i] =~ /^\/\//;
    @words = split(/\s+/, $lines[$i]);		# split line into words
    # get sequence line
    $raw .= "$words[3]\n";
  }
  $raw;
} # block2raw

#
# Convert a BLOCK to FASTA
#
sub block2fasta {
  my($block) = @_;
  my($i, @lines, @words, $fasta, $start);

  $fasta = "";					# return value
  @lines = split(/\n/, $block); 		# split block into lines
  for ($i = 2; $i<$#lines; $i++) {
    last if $lines[$i] =~ /^\/\//;
    @words = split(/\s+/, $lines[$i]);		# split line into words
    # get id line and sequence line
    $start = substr($words[2], 0, length($words[2])-1);
    $fasta .= ">$words[0] ( start= $start )\n$words[3]\n";
  }
  $fasta;
} # block2fasta

#
# Submit a block to the blocks processor
#
sub submit_block {
  my(@numbers) = @_;
  my($blocks);

  print_header("Submit BLOCKS");
 
  # get the BLOCK(S)
  $blocks = "";
  foreach $number (@numbers) {
    $blocks .= param('BLOCKS'.$number);
  }

  $ua = LWP::UserAgent->new();
  my $req = POST $blocks_url, [ sequences => $blocks ];
  my $request = $ua->request($req);
  #$content = $ua->request($req)->as_string;
  $content = $request->content;
  # put in the absolute url's : this is FRAGILE!
  $content =~ s#HREF=\"#HREF=\"http://blocks.fhcrc.org#g;
  print $content;
} # submit_block

#
# Compare a PSPM to the JASPAR database of DNA motifs
#
sub submit_pspm_to_jaspar {
  my($number, $db, $sub_db) = @_;
  my($pspm, @fields, $i, $n, $w, $row, $col);

  print_header("Search $db $sub_db database with motif $number");

  # get the motif PSPM 
  $_ = param('pspm'.$number);
  @fields = split;

  # ignore 1st 10 entries
  for ($i=0; $i<=$#fields-10; $i++) { $fields[$i] = $fields[$i+10]; }
  $#fields = $#fields - 10;

  # rotate PSPM 90 degrees (natural format) as a string with newlines
  $n = $#fields + 1;		# number of entries in motif
  $w = $n/4;			# motif width
  $pspm = "";
  for ($row=0; $row<4; $row++) {
    for ($col=0; $col<$w; $col++) {
      $pspm .= " " . $fields[($col*4) + $row];
    }
    $pspm .= "\n";		# terminate row with newline
  }

  # create the request
  $ua = LWP::UserAgent->new();
  my $req = POST $jaspar_url,
    Content_Type => 'multipart/form-data',
    Content => [ 'matrix_string' => $pspm,  'rm' => 'compare', 'db_for_compare' => $sub_db ];
  my $request = $ua->request($req);
  $content = $request->content;

  # fix bug in JASPAR output; add database field to view buttons
  # remove fix: JASPAR fixed the bug
  # $content =~ s/rm=present/rm=present&db=$sub_db/g;

  # display the page
  print $content;
} # submit_pspm_to_jaspar


#
# Print a man page
#
sub print_man {
  my($command) = @_;
  print <<END; 
  Content-type: text/plain

END
  chdir("$MEME_LOGS");
  $bin = "$MEME_BIN";
  @tmp = `$bin/$command`;
  print @tmp[3..$#tmp];
}

#
# start a response form
#
sub print_header {
  my($action) = @_;
  print <<END; 
Content-type: text/html

<HTML>
<TITLE> MEME - $action</TITLE>
<BODY BACKGROUND=\"../images/bkg.jpg\">
END
} # print_header
