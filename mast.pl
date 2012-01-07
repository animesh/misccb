#!@WHICHPERL@
##
## $Id: mast.pl 583 2006-03-07 23:30:25Z nadya $
##
## $Log$
## Revision 1.8  2006/03/07 23:30:19  nadya
## merge branches v3_5_1 and v3_5_2 back to the trunk
##
## Revision 1.7.6.1  2006/02/16 23:22:35  nadya
## update path to show the background image on the page.
##
## Revision 1.7  2005/10/02 01:00:10  nadya
## move meme-client and mast-client names into Globals and use variables instead.
##
## Revision 1.6  2005/09/16 01:44:55  wilfred
## url fixed
##
## Revision 1.5  2005/08/24 05:42:39  nadya
## move variables to Globals
##
## Revision 1.4  2005/08/19 22:59:20  nadya
## move email validation to Validation module from check_address.cgi
##
## Revision 1.3  2005/08/19 01:26:56  nadya
## change check_address to do email address verification.
## all functionality is there. update mast.pl and meme.pl for a new call.
##
## Revision 1.2  2005/08/07 05:58:26  nadya
## use vairables in Globals for meme locations,
## fix locatio of website.
##
## Revision 1.1.1.1  2005/07/25 23:26:17  nadya
## Importing from meme-3.0.14, and adding configure/make
##
##

use lib qw(@PERLLIBDIR@);
use Globals;
use Validation;

# use the CGI package
use CGI qw/:standard/;          # use the CGI package

# get the directory for executables relative to working directory 
$scratch = "$MEME_LOGS";
$bin = "$MEME_BIN";
$cgidir = "$MEME_WEB";
# email address of maintainer; this is set by the install script
$maint = "$SITE_MAINTAINER";

# start the response form

print <<END; 
Content-type: text/html

<HTML>
<TITLE> MAST - Verification </TITLE>
<BODY BACKGROUND=\"../images/bkg.jpg\">
<HR>
END

# no errors yet
$nerrors = 0;

# set working directory to LOGS directory
chdir($scratch) || &whine("Can't cd to $scratch");
#$fred = `groups`;
#print "$fred\n";


if (0) {
  $debugfile = "debug.$$";
  open (DEB, ">$debugfile") || whine("Can't open file $debugfile");
  print "<PRE>\n";
  while (<stdin>) {
    print $_;
    print DEB $_;
  }
  print "</PRE><HR>\n";
  close (DEB);
  chmod 0777, $debugfile;
}

# retrieve the fields from the form
$address = param('address');
$subject = param('subject');
$subject =~ s/'/"/g;		# prevent problems with single quotes in subject
@database = split(/ +/, param('database'));
$upload_db_name = param('upload_db');
$dna = param('dna');
$use_seq_p = param('use_seq_p');
$ev = param('ev');
$use_seq_comp = param('use_seq_comp');
$rank = defined param('rank') ? param('rank') : 1;
$motif_file_name = param('motifs');	# name and file handle of motif file
$inline_name = param('inline_name');
$inline_motifs = param('inline_motifs');
$text = param('text');
($strands, $strands_text) = split(/,/, param('strands'));
$mev = param('mev');

#
# check the user input
#

# check that valid email address was provided
&check_address;

#
# create file containing motifs
#
$motif_file = "mast.motifs.$$";
open(MOTIFS, ">$motif_file") || &whine("Can't open file $motif_file: $!");
if ($inline_motifs) {				# process inline motifs
  $_ = $inline_motifs;
  s/\r\n/\n/g;					# Windows -> UNIX eol
  s/\r/\n/g;					# MacOS -> UNIX eol
  print MOTIFS $_;
  $motif_file_name = $inline_name;
} else {					# process uploaded motifs
  while (<$motif_file_name>) {
    s/\r\n/\n/g;				# Windows -> UNIX eol
    s/\r/\n/g;					# MacOS -> UNIX eol
    print MOTIFS $_;
  }
}
close (MOTIFS);
chmod 0777, $motif_file;

#
# check that motif file was found and not empty
#
$_ = `wc $motif_file`;
($lines) = /\s+(\S+).*/;
if ($lines == 0) {
  &whine("Your motif file $motif_file_name was not found or was empty.
    Please check its name and retry.");
} else {
  $motifs_found = 1;
}

#
# convert the meme file to log-odds format and get the sequence alphabet
#
$lo_file = "mast.logodds.$$";
if ($motifs_found) {
  $_ = `csh $bin/make_logodds $motif_file $lo_file 2>&1`;
  # check that conversion succeeded
  @words = split();
  $size = @words;
  if ($size >= 1 && $size <= 2) { 
    $alphabet = $words[0];
  } else {
    &whine("@words");
    $motifs_found = 0;
  }
}

#
# check that there are log-odds matrices
#
if ($motifs_found) {
  open (LOGODDS, $lo_file);
  $nmatrices = 0;
  $total_cols = 0;
  # read the logodds matrices one at a time
  while (<LOGODDS>) {
    $nmatrices++;
    ($w, @rest) = $_;
    $total_cols += $w;
    for ($i=0; $i<$w; $i++) { <LOGODDS>; }
  }
  close (LOGODDS);
  # make sure at least one motif was found
  if ($nmatrices == 0) {
    &whine("Your motif file was not in the correct format.");
  }
}

#
# get the motifs
#
chop($motifs = `cat $lo_file`);

#
# get the uploaded sequences if there are any
#
if ($upload_db_name eq "") {		# no uploaded sequences
  $db_type = "local";
  $db_descr = "";
  $seqs = "";
  if ($database[0] eq "none") {
    &whine("You must specify a MAST database or a database to upload.<BR>
	Please go back and specify one.");
  }
} else {
  $db_type = "uploaded";		# upload sequences
  @seqs = <$upload_db_name>;
  $seqs = join "", @seqs;
  # count the characters in the sequences
  $total = 0;
  $letters = "";
  $nseqs = 0;
  foreach $_ (@seqs) {
    if (/^>/) { $nseqs++; next;}# skip header lines
    s/\s*//g;			# remove all whitespace
    tr/a-z/A-Z/;		# make letters upper case
    $total += length($_);	# get length
    $letters .= $_;		# save letters for checking alphabet
  }
  if ($total > $MAX_UPLOAD_SIZE) { 
    &whine("Your uploaded sequence file is too large ($total characters).  Please limit it to $MAX_UPLOAD_SIZE characters.<BR>\n"); 
  } elsif ($total == 0) {
    &whine("Your uploaded sequence file ($upload_db_name) appears to be empty.<BR>\n");
  }
  # get the sequence alphabet
  $upload_alphabet = get_alph($letters);
  # set up the database names
  $database[0] = $upload_db_name;
  if ($upload_alphabet eq "DNA") { 
    $database[1] = "none";
    $database[2] = $upload_db_name;
  } else {
    $database[1] = $upload_db_name;
    $database[2] = "none";
  }
  $db_descr = "\
<LI>Database type: <B>$upload_alphabet</B>
<LI>Database sequences: <B>$nseqs</B>
<LI>Database size: <B>$total</B>\
";
}

# Check that the alphabet is known and choose the appropriate database.
# The actual name of the database file is in positions 1 (peptide)
# or 2 (nucleotide) of $database.
# If no such type exists, that name is "none".
$df = "-df $database[0]";		# common name of database
if ($alphabet) { 
  chop($_ = `$bin/alphtype $alphabet 2>&1`);
  if ($_ eq "DNA" || ($_ eq "PROTEIN" && $dna)) {	# nucleotide alphabet
    if ($_ eq "DNA") { $dna = "";}	# avoid common bug
    $alph = "DNA";
    $db = $database[2];
  } elsif ($_ eq "PROTEIN") {		# peptide alphabet
    $db = $database[1];
    $alph = "PROTEIN";
    $strands = $strands_text = "";	# disallow strands if DB is peptide
  } else {				# unknown alphabet
    &whine($_);
  }
  # make sure right flavor of database exists
  if ($db eq "none") {
    if ($db_type eq "uploaded") {
      &whine("The uploaded database $database[0] seems to be $upload_alphabet but your motifs are $alph.");
    } elsif ($database[0] ne "none") {
      &whine("There is no $alph version of $database[0].");
    }
  }
}

#
# create a message file and submit to the client if form is ok
#
if ($nerrors == 0) {

  # create the mast client input file
  $msgfile = "mast.msg.$$";
  open (OUTFILE, ">$msgfile") || &whine("Can't open $msgfile");
  chmod 0777, $msgfile;

  # add notice to use web browser to description
  if (!$text) {$viewer = "(Use web browser to view results)";}

  # set up the additional switches
  $switches = "$df $ev $text $dna $use_seq_p $evt $use_seq_comp -rank $rank -smax 500 $strands $mev"; 

  # create the message file
  # Note: changes here must be reflected in mast-server.c
  #
  print OUTFILE <<END;
ADDRESS $address
PROGRAM mast
DESCRIPTION $subject $viewer
MOTIFS $motif_file_name
DB_TYPE $db_type
DATABASE $db
ALPHABET $alphabet
SWITCHES $switches
LOGINFO
BEGIN_MOTIFS
$motifs
END
if ($db_type eq "uploaded") {
  print OUTFILE <<END;
%
BEGIN_DB
$seqs
%
END
}

  close (OUTFILE);

  if ($debug) {
    print "<pre>\n";
    open (O, $msgfile); while (<O>) {print;} close ($msgfile);
    print "</pre>\n";
  }

  # send the data to the mast client
  if (!$debug) {
    $status = system ("$bin/$MAST_CLIENT $msgfile 1>mast.client.msg.$$ 2>&1");
  }
  $status /= 256;
  if ($status) {
    open (ERROR, "mast.client.msg.$$");
    while (<ERROR>) {$error = $_}		# get last line of message 
    &whine("An error occured while submitting your job: status = $status. <BR>
      Error message: $error. <BR>
      Please try again later.");
    close (ERROR);
    # warn maintainer
    $wd = `pwd`; chop $wd;
    $command =
      "cat mast.client.msg.$$ | \
        @mail@ -s \'$MAST_CLIENT error: $status   user: $address   dir: $wd\' $maint"; 
    system($command);
  } else {
    # set up the message
    if ($strands_text) { 			# strands
      $strands_text = 
        "<LI> Treatment of reverse complement strands: <B>$strands_text</B>";
    }
    if ($dna) {					# translate dna
      $translate_dna = 
        "<LI> Searching <B>nucleotide</B> database with <B>protein</B> motifs";
    }
    if ($use_seq_p) { 				# type of motif threshold
      $pvalue_text = "sequence"; 
    } else { 
      $pvalue_text = "motif"; 
    }
    if ($mev) {					# ignore motifs with high E-valu
      ($dummy, $mev_text) = split(/ +/, $mev);
      $mev_text = 
        "<LI> Using only motifs with <I>E</I>-value < <B>$mev_text</B>";
    }
    if ($ev) {					# E-value threshold
      ($dummy, $ev_text) = split(/ +/, $ev);
      $ev_text = 
        "<LI>Displaying sequences with <I>E</I>-value < <B>$ev_text</B>";
    }
    if ($use_seq_comp) {
      $comp_text = 
        "<LI>Adjusting p-values and E-values for <B>sequence composition</B>";
    }

    if ($db_type eq "uploaded") { $db = "uploaded"; }
    
    print "
      Your MAST search results will be sent to: <b> $address </b><br>
      If you do not receive a confirming email message, there could be an 
      error in your email address.
      <HR>
      <UL>
      <LI> E-mail address: <B>$address</B>
      <LI> Description: <B>$subject</B>
      <LI> Motif file name: <B>$motif_file_name</B>
      <LI> Number of motifs: <B>$nmatrices</B>
      <LI> Total motif columns: <B>$total_cols</B>
      <LI> Motif alphabet: <B>$alphabet</B>
      <LI> Database to search: <B>$database[0] ($db)</B>
      $db_descr
      $strands_text
      $translate_dna
      $ev_text
      $comp_text
      <LI> Motif display threshold: <B>$pvalue_text</B> <I>p</I>-value < 0.0001
      $mev_text
      <LI> Rank of first match to report: <B>$rank</B>
      </UL>
    ";
  }
}

#
# delete all scratch files
#
if ($motif_file) {unlink $motif_file}
if ($lo_file) {unlink $lo_file}		# delete the logodds file
if ($msgfile) {unlink $msgfile}		# delete the file sent to the server
unlink "mast.client.msg.$$";

#
# finish the form
#
if ($nerrors) {
  if ($nerrors == 1) {
    $tobe = "was";
    $booboo = "error";
    $pronoun = "it";
  } else {
    $tobe = "were";
    $booboo = "errors";
    $pronoun = "them";
  }
  print "</B></OL>\n";
  print "<B>There $tobe $nerrors $booboo on the form.\n";
  print "Please correct $pronoun and try again.</B>\n";
}

print "
  <HR>
  </BODY>
  </HTML>
";

exit(0);

##############################
# subroutines		     #
##############################

#
# print an error message, bump the error count and continue
#
sub whine
{
  if ($nerrors == 0) {
    print "
      <H1>Error Report:</H1>
      <HR>
      <OL>
      <B>
    ";
  }
  print "<LI><B>@_<B>\n";
  $nerrors++;
}

#
# Check to see whether the email address is valid.
#
sub check_address
{
  if (!$address) {
    &whine("
      You must include a return e-mail address to receive your results.<BR>
      Please go back and include one.
    ");
  } else {
    $status = valid_address($address);
    if ($status == 0) {
      &whine("
        There is an error in your return email address:<BR>
        &nbsp&nbsp&nbsp&nbsp <TT>$address</TT><BR>
        It is possible that your email address is correct, in which case
        the problem may be that your host is behind a firewall and
        is consequently not found by the nslookup routines.  Consult with
        your systems people to see if you have an nslookup-visible email
        address.  If none is available, please send email to <BR>
        &nbsp&nbsp&nbsp&nbsp <TT>meme\@nbcr.net</TT> <BR>
        mentioning this problem.
      ");
    }
  }
}

#
# get the alphabet: DNA or PROTEIN
#
sub get_alph
{
  local ($letters) = @_;                # get arguments

  $_ = $letters;
  $old = length;

  # check against allowed nucleotide letters
  $x = $_;
  $x =~ tr/ABCDGHKMNRSTUVWY//cd;
  $new = length $x;
  if ($old == $new) {
    return("DNA");
  } else {
    # check against allowed protein letters
    $x = $_;
    $x =~ tr/ABCDEFGHIKLMNPQRSTUVWXYZ//cd;
    $new = length $x;
    if ($old == $new) {
      return("PROTEIN");
    } else {
      # get the unknown letters
      $x = $_;
      $x =~ tr/ABCDEFGHIKLMNPQRSTUVWXYZ//d;
      &whine("
        Your sequences contained the following unrecognized letters: $x.
        <BR>
        Please convert your sequences to one of the sequence
        <A HREF=../help_alphabet.html>alphabets</A> which MAST recognizes.
      ");
    }
  }
} # get_alph
