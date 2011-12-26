#!@WHICHPERL@
##
## $Id: meme.pl 583 2006-03-07 23:30:25Z nadya $
##
## $Log$
## Revision 1.12  2006/03/07 23:30:19  nadya
## merge branches v3_5_1 and v3_5_2 back to the trunk
##
## Revision 1.11.6.1  2006/02/16 23:22:35  nadya
## update path to show the background image on the page.
##
## Revision 1.11  2006/01/03 06:42:24  tbailey
## Fix use of "min()".
##
## Revision 1.10  2005/12/15 06:27:42  tbailey
## Don't allow more than $MAXSITES sequences in OOPS mode.
## Set maxsites to minimum of number of sequences and $MAXSITES in ZOOPS mode.
##
## Revision 1.9  2005/10/02 01:00:10  nadya
## move meme-client and mast-client names into Globals and use variables instead.
##
## Revision 1.8  2005/09/16 01:44:55  wilfred
## url fixed
##
## Revision 1.7  2005/08/24 05:42:39  nadya
## move variables to Globals
##
## Revision 1.6  2005/08/19 22:59:20  nadya
## move email validation to Validation module from check_address.cgi
##
## Revision 1.5  2005/08/19 01:26:56  nadya
## change check_address to do email address verification.
## all functionality is there. update mast.pl and meme.pl for a new call.
##
## Revision 1.4  2005/08/10 21:02:39  nadya
## mv MAXTIME to Globals module
##
## Revision 1.3  2005/08/10 20:33:24  nadya
## use variable for MAXTIME, set by configure
##
## Revision 1.2  2005/08/07 05:58:26  nadya
## use vairables in Globals for meme locations,
## fix locatio of website.
##
## Revision 1.1.1.1  2005/07/25 23:26:55  nadya
## Importing from meme-3.0.14, and adding configure/make
##
##

#$debug = 1;                     # uncomment to debug this script

use lib qw(@PERLLIBDIR@);
use Globals;
use Validation;
use CGI qw/:standard/;          # use the CGI package

$scratch = "$MEME_LOGS";        # directory for temp files
$bin = "$MEME_BIN";             # directory for executables 
$cgidir = "$MEME_WEB";
$maint = "$SITE_MAINTAINER";    # email address of site maintainer; 

# start the response form
print <<END; 
Content-type: text/html

<HTML>
<TITLE> MEME - Verification </TITLE>
<BODY BACKGROUND=\"../images/bkg.jpg\">
<HR>
END

# no errors yet
$nerrors = 0;
$sequences_given = 1;

# change working directory to LOGS
chdir($scratch) || &whine("Can't cd to $scratch");

# retrieve the fields from the form
$address = param('address');
$subject = param('subject');
$subject =~ s/[^\w:;-_"()<>%]/ /g;	# remove funny characters
$datafile_name = param('datafile');
$dist = param('dist');
$nmotifs = param('nmotifs');
$data = param('data');
$text = param('text');
$minsites = param('minsites');
$maxsites = param('maxsites');
$minw = param('minw');
$maxw = param('maxw');
$bfile = param('bfile');
$evt = param('evt');
$shuffle = param('shuffle'); $shuffle = 0 unless ($shuffle);
$pal = param('pal');
$posonly = param('posonly');

# dna switches
$dna_switches = "$pal $posonly";

# add notice to use web browser to description
if (!$text) {$viewer = "(Use web browser to view results)";}

# check that valid email address was provided
&check_address;

# check that sequence data was provided
if (!$datafile_name && !$data) {
  &whine("
    You haven't entered any sequence data. <BR>
    If you still wish to submit a query, please go back and enter the
    name of a sequence file or the actual sequences.
  ");
  $sequences_given = 0;
}

# don't allow both datafile and data
if ($datafile_name && $data) {
  &whine("
    You may not enter <I>both</I> the name of a sequence file and sequences.<BR>
    If you still wish to submit a query, please go back and erase either
    what you have written in the <I>name of a file</I> field or 
    in the <I>actual sequences</I> field.
  ");
  $sequences_given = 0;
}

#
# create file containing sequences
#
$datafile = "meme.seqs.$$";
open(SEQS, ">$datafile") || &whine("Can't open file $datafile: $!");
if ($data) {					# process inline data
  $_ = $data;
  s/\r\n/\n/g;                                  # Windows -> UNIX eol
  s/\r/\n/g;                                    # MacOS -> UNIX eol
  print SEQS $_;
} else {					# process uploaded sequences
  while (<$datafile_name>) {
    s/\r\n/\n/g;                                # Windows -> UNIX eol
    s/\r/\n/g;                                  # MacOS -> UNIX eol
    print SEQS $_;
  }
}
print SEQS "\n";				# make sure there is a last nl
close (SEQS);
chmod 0777, $datafile;

# use datafile_name for printing by MEME
$datafile_name = "pasted sequences" unless($datafile_name); 

# make sure sequence file isn't empty
if ($datafile_name) {
  $_ = `wc $datafile`;
  @tmp = split (' '); 
  if ($tmp[0] == 0) {
    &whine("
      MEME could not read your sequence file or it is empty. <BR>
      Make sure the name is correct and that you have read access
      to the file.
    ");
    $sequences_given = 0;
  }
}

# Get information on the sequences
if ($sequences_given) {
  &check_data;
}

# Make sure the data was in FASTA format or got converted correctly. 
if ($nerrors == 0 && $num == 0 ) {
  &whine("
    MEME was unable to read your data. <BR>
    Please check to be sure that your data is 
    <A HREF=../help_format.html> formatted</A> properly.  <BR>
    If you are still having trouble, you can try to convert
    your data to 
    <A 
      HREF=http://dot.imgen.bcm.tmc.edu:9331/seq-util/Help/example_input.html>
    FASTA format</A> using the
    <A HREF=http://dot.imgen.bcm.tmc.edu:9331/seq-util/readseq.html>
    ReadSeq</A> program and then resubmit it.
  ");
}

# Make sure there isn't too much data.
if ($total > $MAXDATASET) {
  &whine("
    The data you have entered contains more than $MAXDATASET characters.  
    MEME cannot process it at this time. <BR>
    Please submit a smaller dataset.
  ");
}

# Make sure there aren't too many sequences in OOPS mode 
if ($dist eq "oops" && $num > $MAXSITES) {
  &whine("
    Your dataset must contain no more than $MAXSITES sequences when you specify
    that the motif is <I>distributed</I> one per sequence.
    Please input a dataset with no more than $MAXSITES sequences or chose a
    different motif distribution.
  ");
}

# Set the maximum number of sites in ZOOPS mode if not specified
if ($dist eq "zoops" && !$maxsites) {
  $maxsites = ($num <= $MAXSITES ? $num : $MAXSITES);
}

# check that DNA dataset specified if using DNA switches
if (($dna_switches =~ /\S/) && !($alphabet eq "dna")) {
  &whine("
    You may not use DNA only options with a protein dataset.<BR>
    Please specify a DNA dataset 
     or 
    make sure that the <I>strand</I> and <I>palindromes</I> boxes
    are not checked.
  "); 
}

# check that TCM specified if only one sequence
if (($num == 1) && !($dist eq "tcm")) {
  &whine("
    You must specify <I>Any number of repetitions</I> under the 
    <I>distribution</I> option
    since your dataset contains only one sequence.  Alternatively, you might 
    wish to break your sequence into several sequences.
  ");
}

# check that number of motifs is OK
if ($nmotifs < 1 || $nmotifs > $MAXMOTIFS) {
  &whine("
     You must specify <I> number of motifs</I> between 1 and $MAXMOTIFS,
     inclusive.
  ");
}

# check that number of sites is OK
&check_nsites($minsites, $maxsites, $dist, $num);

# check that width is OK
&check_width;

# set the revcomp switch
if (!$posonly && $alphabet eq "dna") {
  $revcomp = "-revcomp";
}

# set the number of sites switches
$mins = " -minsites $minsites" if ($minsites);
$maxs = " -maxsites $maxsites" if ($maxsites);

# set the background model file switch
if ($bfile) {
  $bf = "-bfile $bfile";
}

# combine all switches
$switches = "-$alphabet -mod $dist -nmotifs $nmotifs $mins $maxs -minw $minw -maxw $maxw -evt $evt $pal $revcomp $bf $text -time $MAXTIME -maxsize $MAXDATASET";

# remove spaces, non-ASCII and single quotes from $datafile_name
$datafile_name =~ s/[ \'\x80-\xFF]/\_/g;

#
# Create the file to be sent to the client
#
&print_file;

#
# submit the job
#
if ($nerrors == 0) {
  &submit;
}

#
# delete the temporary files
#
unlink "meme.msg.$$";
unlink $datafile;
unlink "meme.data.$$";
unlink "meme.client.msg.$$";

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


###############################################################################
####		SUBROUTINES:
###############################################################################

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
# check that number of sites is OK
#
sub check_nsites {
  my(
    $minsites, 			# minimum nsites
    $maxsites, 			# maximum nsites
    $dist, 			# type of distribution
    $num			# number of sequences
  ) = @_;

  if ($minsites && $minsites < $MINSITES) {
    whine("You must specify a minimum number of sites >= $MINSITES");
  }
  if ($maxsites && $maxsites > $MAXSITES) {
    whine("You must specify a maximum number of sites <= $MAXSITES");
  }
  if (($minsites && $maxsites) && $minsites > $maxsites) {
    whine(
     "The minimum number of sites is larger than the maximum number of sites");
  }
} # check_nsites

#
# check that width is OK
#
sub check_width
{
  if ($minw < $MINW) {
    whine("The minimum width you specified ($minw) is too small.<BR>")
  }
  if ($maxw > $MAXW) {
    whine("The maximum width you specified ($maxw) is too large.<BR>")
  }
  if ($minw > $maxw) {
    whine("The minimum width you specified ($minw) is larger than the
	maximum width you specified ($maxw).<BR>")
  }
} # check_width

#
# Check to see whether the data are valid.
#
sub check_data
{
  # Convert the data to FASTA format.
  $status = system 
    ("$bin/readseq -a -f=8 $datafile 1>meme.data.$$ 2>meme.error.$$");
  $error = `cat meme.error.$$`; unlink "meme.error.$$";
  if ($status) {
    &whine("An error occurred when the READSEQ program attempted to convert
      your dataset to FASTA format.<BR>
      READSEQ returned: $error
    ");
   return(1);
  }

  # Run the 'getsize' program to get information on the sequence data. 
  $status = system 
    ("$bin/getsize $datafile 1>getsize.$$ 2>getsize.error.$$");
  unlink "getsize.error.$$";
  chop($getsize1 = `cat getsize.$$`); unlink "getsize.$$";
  ($num, $min, $max, $ave, $total, $letters) = split (' ', "$getsize1");

  # Run the 'getsize' program to get information on converted data; will
  # be unchanged if MEME knows the format.
  $status = system 
    ("$bin/getsize -nd meme.data.$$ 1>getsize.$$ 2>getsize.error.$$");
  $error = `cat getsize.error.$$`; unlink "getsize.error.$$";
  if ($error || $status) {
    &whine("After converting to FASTA format using the READSEQ program,
      the following errors in your dataset were detected:<BR>$error
      <BR>Make sure all your sequences are in the same format since READSEQ
      assumes that all sequences are in the same format as the first sequence.
    ");
    unlink "getsize.$$";
    return(1);
  }
  chop($getsize2 = `cat getsize.$$`); unlink "getsize.$$";
  ($num, $min, $max, $ave, $total, $letters) = split (' ', "$getsize2");

  # Use original dataset if MEME recognizes it
  if ($getsize1 eq $getsize2) {
    #print "<P>Using original dataset<P>";
    $error = `cp $datafile meme.data.$$`;
    if ($error ne "") {
      &whine("An error occurred while trying to copy your data.<BR>
        cp returned: $error
      ");
    }
  }

  # check for problem reading the dataset
  if ($num <= 0) {
    &whine("Your dataset appears to be in a format that MEME does not recognize.
      <BR> Please check to be sure that your data is 
      <A HREF=../help_format.html> formatted</A> properly.
    ");
    &copy_stdout ("$datafile");
  }

  # check for bad sequences
  if ($num > 0 && $min == 0) {
    &whine("Your dataset appears to contain one or more zero-length sequences.
      <BR> Please check to be sure that your data is 
      <A HREF=../help_format.html> formatted</A> properly.
    ");
  }

  # get the alphabet used in the sequences
  $alphabet = get_alph($letters);
} # check_data

#
# get the alphabet: DNA or PROTEIN
#
sub get_alph 
{
  local ($letters) = @_;		# get arguments

  $_ = $letters;
  $old = length; 

  # check against allowed dna letters
  $x = $_;
  $x =~ tr/ABCDGHKMNRSTUVWY//cd;
  $new = length $x;
  if ($old == $new) {
    "dna";
  } else {
    # check against allowed protein letters
    $x = $_;
    $x =~ tr/ABCDEFGHIKLMNPQRSTUVWXYZ//cd;
    $new = length $x;
    if ($old == $new) {
      "protein";
    } else {
      # get the unknown letters
      $x = $_;
      $x =~ tr/ABCDEFGHIKLMNPQRSTUVWXYZ//d;
      &whine("
	Your sequences contained the following unrecognized letters: $x.
	<BR>
	Please convert your sequences to one of the sequence
	<A HREF=../help_alphabet.html>alphabets</A> which MEME recognizes.
      ");
    }
  }
} # get_alph

#
# Submit job to meme client 
#
sub submit
{
  # submit it
  if (!$debug) {
    $status = system ("$bin/$MEME_CLIENT meme.msg.$$ 1>meme.client.msg.$$ 2>&1");
    $status /= 256;
  }

  # check for errors
  if ($status) {

    open (ERROR, "meme.client.msg.$$");
    $_ = <ERROR>;
    while (<ERROR>) {$error = $_}               # get last line of message
    if ($status == 12) {$msg = `cat ../meme-downtimes`;}
    &whine("An error occured while submitting your job: status = $status. <BR>
      Error message: $error <BR>
      $msg
      Please try again later.");
    close (ERROR);
    # warn maintainer
    $wd = `pwd`; chop $wd;
    $command = 
      "cat meme.client.msg.$$ | \
        @mail@ -s \'$MEME_CLIENT error: $status  user: $address  dir: $wd\' $maint";
    # warn maintainer unless debugging
    if (!$debug) { system($command); }

  } else {			# print verification form
    print "
      Your MEME search results will be sent to: <b> $address </b><br>
      If you do not receive a confirming email message, there could be an
      error in your email address.
      <HR>
      <UL>
      <LI> E-mail address:<B> $address</B>
      <LI> Sequence file:<B> $datafile_name </B>
      <LI> Description:<B> $subject</B>
      <LI> Distribution of motif occurrences:<B>
    ";
    if ($dist eq "oops") {
      print  "One per sequence</B>\n";
    } elsif ($dist eq "zoops") {
      print  "Zero or one per sequence</B>\n";
    } else {
      print  "Any number of repetitions</B>\n";
    }
    print "
      <LI> Number of different motifs:<B> $nmotifs </B>
      <LI> Minimum number of sites:<B> $minsites</B>
      <LI> Maximum number of sites:<B> $maxsites</B>
      <LI> Minimum motif width:<B> $minw</B>
      <LI> Maximum motif width:<B> $maxw</B>
    ";
    if ($posonly) {
      print "<LI> Searching given strand only";
    }
    if ($pal) {
      print "<LI> Looking for palindromes only";
    }
    if ($text) {
      print "<LI> Output is text (not HTML)";
    }
    if ($shuffle) {
      print "<LI> Shuffling letters in input sequences.";
    }
    if ($bfile) {
      print "<LI> Background model file: $bfile";
    }
    print "
      <LI> Statistics on your dataset:
        <TABLE BORDER>
	  <TR> <TD> type of sequence <TH ALIGN=RIGHT> $alphabet
	  <TR> <TD> number of sequences <TH ALIGN=RIGHT> $num
	  <TR> <TD> shortest sequence (residues) <TH ALIGN=RIGHT> $min 
	  <TR> <TD> longest sequence (residues) <TH ALIGN=RIGHT> $max 
	  <TR> <TD> average sequence length (residues) <TH ALIGN=RIGHT> $ave 
	  <TR> <TD> total dataset size (residues) <TH ALIGN=RIGHT> $total 
        </TABLE>
      </UL>
    ";
  }
  # print the message if debug
  if ($debug) {&copy_stdout ("meme.msg.$$");}
} # submit

#
# Print the output file to be sent to the meme-client.
#
sub print_file
{
  # First put a header on the file.
  open(outfile, "> meme.msg.$$") || whine("Can't open meme.msg.$$"); 
  print outfile <<END;
ADDRESS $address
PROGRAM meme
REMOTENAME $datafile_name
DESCRIPTION $subject $viewer
SWITCHES $switches
LOGINFO	n=$num N=$total shuffle=$shuffle
BEGIN
END
  close (outfile);

  # Then append the sequence data to the message file
  if ($shuffle) {
    $error = `$bin/fasta-shuffle-letters -tod < meme.data.$$ >> meme.msg.$$`;
  } else {
    $error = `cat meme.data.$$ >> meme.msg.$$`;
  }
} # print_file

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
} # whine

#
# copy a file to standard output
#
sub copy_stdout
{
  open (F, "@_");
  print "<PRE>";
  while (<F>) {
    print $_;
  }
  print "</PRE>";
} #copy_stdout
