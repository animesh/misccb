#!@WHICHPERL@
#
# $Id: meme2html.pl 1800 2007-05-23 09:57:55Z tbailey $
# $Log$
# Revision 1.7  2005/12/01 03:38:44  tbailey
# Add new button to MEME DNA output to submit PSPM to JASPAR database for
# searching against known DNA motifs.
#
# Revision 1.6  2005/10/13 21:15:52  nadya
# move meme-explanation.html to etc/ to allow html conversion without installing web site
#
# Revision 1.5  2005/09/13 18:52:56  nadya
# rm "web" from url string
#
# Revision 1.4  2005/08/30 22:39:19  nadya
# fix link path
#
# Revision 1.3  2005/08/23 23:57:56  nadya
# change use of convert2html from module to perl file
# update regexpr string that makes a tag
# update doc string to get file locally
#
# Revision 1.2  2005/08/12 17:52:20  nadya
# rename 2html into convert2html. Perl needs module name start with alpha char,
# not numeric
#
# Revision 1.1.1.1  2005/07/30 02:13:51  nadya
# Importing from meme-3.0.14, and adding configure/make
#
#

use lib qw(@PERLMODDIR@);
use Globals;
require "@PERLMODDIR@/convert2html.pl";

$pgm = $0;                      # name of program
$pgm =~ s#.*/##;                # remove part up to last slash
@args = @ARGV;                  # arguments to program
$status = 0;                    # exit status
$SIG{'INT'} = 'cleanup';        # interrupt handler

#
# get url; set during install in GLobals.pm
#
$url = "$SITE_URL";

$usage = <<USAGE;		# usage message
  USAGE:
        $pgm 

USAGE

# Starting with standard MEME output:
#     format in html
#     add internal navigation links
#     add html motif diagrams
#     add documentation
#
# adapted from the earlier diagram program mast2html
# v1.00		23 August 2000	Tim Bailey

#
# get input arguments
#
while ($#ARGV >= 0) {
  if ($ARGV[0] eq "-h") {
    print $usage;
    exit(0);
  } else {
    print $usage;
    exit(1);
  }
  shift;
} # input argument

# Define buttons for next and previous motifs
$MBPSUB = "XXX---XXX";
%buttons = (
  "next", "<A HREF=\"\#summary$MBPSUB\"><B>N</B></A>!#FFFFOO!#000000",
  "prev", "<A HREF=\"\#summary$MBPSUB\"><B>P</B></A>!#OOFFFF!#000000",
  "summary", "<A HREF=\"\#motif-summary\"><B>P</B></A>!#OOFFFF!#000000",
  "command", "<A HREF=\"\#command\"><B>Command line</B></A>!#DDDDFF!#000000",
  "tset", "<A HREF=\"\#sequences\"><B>Training Set</B></A>!#00FFFF!#000000",
  "first", "<A HREF=\"\#summary1\"><B>First Motif</B></A>!#DDFFDD!#000000",
  "msum","<A HREF=\"\#motif-summary\"><B>Summary of Motifs</B></A>!#FFDDFF!#000000",
  "stop", "<A HREF=\"\#stopped\"><B>Termination</B></A>!#00FF00!#000000", 
  "explain", "<A HREF=\"\#explanation\"><B>Explanation</B></A>!#FFFF00!#000000", 
  "top", "<A HREF=\"\#top_buttons\"><B>Go to top</B></A>!#DDDDFF!#000000"
);

# header

#print "<HTML>\n<HEAD>\n<TITLE>MEME</TITLE>\n</HEAD>\n<BODY BGCOLOR="$BODY">\n";
print_header("MEME");

# top button panel
$button = make_button_panel("!", \%buttons, "command", "tset", "first", "msum",
  "stop", "explain");
print "<A NAME='top_buttons'></A><HR>\n$button\n<BR CLEAR=LEFT>\n";

# make it a form
print "\n<FORM ENCTYPE = \"application/x-www-form-urlencoded\" METHOD = POST\n";
print "TARGET = \"_new\"\n";
print "ACTION = \"$url/cgi-bin/process_request.cgi\">\n";

# submit buttons at top

# mast submit
$line = "<B>\n";
$line .= "<BR><INPUT TYPE = 'SUBMIT' NAME = 'action' VALUE = 'MAST'>";
$line .= " Search sequence databases with these motifs using ";
$line .= "<A HREF='$url/mast-intro.html'>MAST.</A>\n";
# all blocks submit
$line .= "<BR><INPUT TYPE = 'SUBMIT' NAME = 'action' VALUE = 'BLOCKS'>";
$line .= " Submit these motifs to <A HREF='http://blocks.fhcrc.org/blocks/process_blocks.html'>BLOCKS multiple alignment processor.</A>\n";
# metameme submit
$line .= "<BR><INPUT TYPE = 'SUBMIT' NAME = 'action' VALUE = 'MetaMEME'>";
$line .= " Build and use a motif-based hidden Markov model (HMM) using ";
$line .= "<A HREF='http://metameme.sdsc.edu'>Meta-MEME.</A>\n";
# print the buttons
print "$line</B><BR CLEAR=LEFT>\n";

# title

$line = &next_section();
$line = &format_section( "", $line, "", "version");
print "$line\n";
$line = &next_block();
$line =~ s/(http:.*)/<A HREF=\"\1\">\1<\/A>/g;		# make links to MEME
$line = &format_para( $line );
print "$line\n";

# reference

$line = &next_section();
$line = &format_section( "", $line, "", "reference" );
print "$line\n";
$line = &next_block();
$line = &format_para( $line );
print "$line\n";

# training set

$line = &next_section();
$line = &format_section( "", $line, "", "sequences" );
print "$line\n";
$line = &next_block();
$line = &format_training_set( $line );
# get the datafile name and alphabet and make hidden fields
$line =~ /^DATAFILE= (.+)$/m; $datafile = $1;
$line =~ /^ALPHABET= (\S+)$/m; $alphabet = $1; 
$line .= "<INPUT TYPE = HIDDEN NAME = name VALUE = \"$datafile\" >\n";
$line .= "<INPUT TYPE = HIDDEN NAME = alphabet VALUE = \"$alphabet\" >\n";
$line .= "<INPUT TYPE = HIDDEN NAME = url VALUE = \"$url\" >\n";
print "$line\n";

# command line summary

$line = &next_section();
$line = &format_section( "", $line, "", "command" );
print "$line\n";
$line = &next_block();
$line =~ /^Background letter frequencies.+\:$/m; $bgfreq = $';
$line = &format_pre( $line );
$line .= "<INPUT TYPE = HIDDEN NAME = bgfreq VALUE = \"$bgfreq\" >\n";
print "$line\n";

# figure DB type out how are DNA strands are scored
if ($line =~ /strands: + -/) {
  $db = "dna";
  $stype = "c"; 					# combined
} elsif ($line =~ /strands: +/) {
  $db = "dna";
  $stype = "n";               				# noic
} else {
  $db = "protein";
  $stype = "p";						# protein
}
$xlate = 0;						# not translating DNA

#
# read motif sections, summary section and "stopped" section
#
$f = +4;						# font size
$sp = "&nbsp;&nbsp;&nbsp;";
@section_names = ("description", "sites", "diagrams", "BLOCKS", "pssm", "pspm",
  "regular_expression");
$n_sections = $#section_names+1;
  
$motif_num = 1;
while (($line = &next_section())) {
  $mm1 = $motif_num - 1;				# previous motif
  $mp1 = $motif_num + 1;				# next motif

  $button = "";

  # determine what type of section: MOTIF, SUMMARY OF MOTIFS, STOPPED
  if ($line =~ /^\s*(MOTIF\s+\d+)\s+width =\s+(\d+)/) {	# motif start
    $name = "summary";
    $ext = $motif_num;
    $link = $1;						# motif number
    $width{$motif_num} = $2;				# save width in global
    $rest = "width = $2 $'<BR CLEAR=LEFT>";
    # make a button panel for going to next motif (and back)
    if ($motif_num == 1) {				# first motif
      $button = make_button_panel("!", \%buttons, "summary", "next!$mp1");
    } else {						# not first 
      $button = make_button_panel("!", \%buttons, "prev!$mm1", "next!$mp1");
    }
    #$link = "$button\n$link\n";
  } elsif ($line =~ /SUMMARY/) {			# summary of motifs
    $name = "motif-summary";
    $ext = "";
    print "<A NAME='summary$motif_num'></A>";		# make like a motif
    $button = make_button_panel("!", \%buttons, "prev!$mm1", "next!1");
    $link = "$line";
    $rest = "<BR CLEAR=LEFT>";
  } elsif ($line =~ /Stopped/) {			# stopped line
    $line = &format_section( "", $&, $', "stopped" );
    print "$line\n";
    $line = &next_block();
    $line = &format_pre( $line );
    print "$line\n";
    last;
  }

  $line = &format_section( $button, $link, $rest, $name, $ext );
  # put spaces between the "nam=val" pairs in the summary lines
  $line =~ s/([\w\-]+\s+=\s+\w+)/ $sp \1/g unless ($line =~ /Stopped/);
  print "$line\n";

  if ($line =~ /MOTIF/ ) {				# motif|summary section
    for ($i=0; $i<$n_sections; $i++) {
      $name = &next_section($SUBDIV);
      $section_name = $section_names[$i];
      unless ($name =~ /Description|sorted|Combined/) {
        $line = &format_section( "", $name, "", $section_name, $motif_num );
        print "$line\n";
      }
      $line = &next_block($SUBDIV);
      if ($name =~ /block diagrams/) {			# block diagrams
        $hdr = ($name =~ /Combined/) ?
          "Combined<BR>p-value" : "Lowest<BR>p-value";
	$line = &format_diagrams($SCALE, $MAX_DIAGRAM, $line, $db, $stype,
	  $xlate, 0, $hdr, 0, ' ', undef, \%width);
	if ($name =~ /Combined/) {			# hidden combined summary
          $line .= &format_hidden( "motif-summary", $META );
          $nmotifs = $motif_num - 1;
          $line .= "<INPUT TYPE = HIDDEN NAME = nmotifs VALUE = $nmotifs>";
          $line .= "<BR><INPUT TYPE = 'SUBMIT' NAME = 'action' VALUE = 'View motif summary'>";
          $line .= " <B>Motif summary in machine readable format.</B>";
          $line .= "\n";
        }
        if ($name =~ /Combined/) { 
          $tmp = &format_section( "", $name, "", "" );
          print $tmp; 
          print $line; 
          last; 
        }						# combined diagrams
      } elsif ($name =~ /Motif \d+ Description/) {	# motif description
	$line = &format_description( $line, $motif_num, $f );
      } elsif ($name =~ /Motif \d+ sites sorted/) {	# sorted/aligned sites
	$line = &format_sorted_sites( $line, $motif_num, $f );
      } elsif ($name =~ /Motif \d+ in BLOCKS/) {	# BLOCK
	$line1 = &format_hidden( "$section_name$motif_num", $line );
	$line = $line1 . "<INPUT TYPE = 'SUBMIT' NAME = 'action' VALUE = 'View BLOCK $motif_num'>\n";
        $line .= "<INPUT TYPE = 'SUBMIT' NAME = 'action' VALUE = 'View FASTA $motif_num'>\n";
        $line .= "<INPUT TYPE = 'SUBMIT' NAME = 'action' VALUE = 'View RAW $motif_num'>\n";
	$line .= "<BR><INPUT TYPE = 'SUBMIT' NAME = 'action' VALUE = 'Submit BLOCK $motif_num'>\n";
        $line .= "<B>to <A HREF='http://www.blocks.fhcrc.org/blocks/process_blocks.html'>BLOCKS multiple alignment processor.</A></B>";
      } elsif ($name =~ /Motif \d+ position-specific s/) {	# pssm
	$line1 = &format_hidden( "$section_name$motif_num", $line );
	$line = $line1 . "<INPUT TYPE = 'SUBMIT' NAME = 'action' VALUE = 'View PSSM $motif_num'>\n";
      } elsif ($name =~ /Motif \d+ position-specific p/) {	# pspm
	$line1 = &format_hidden( "$section_name$motif_num", $line );
	$line = $line1 . "<INPUT TYPE = 'SUBMIT' NAME = 'action' VALUE = 'View PSPM $motif_num'>\n";
        if ($db eq "dna") {
	  $line .= "<BR><INPUT TYPE = 'SUBMIT' NAME = 'action' VALUE = 'COMPARE PSPM $motif_num'>\n"; 
	  $line .= "<B>to known motifs in <A HREF='http://mordor.cgb.ki.se/cgi-bin/jaspar2005/jaspar_db.pl'>JASPAR database:</A></B>\n";
  	  $line .= "<SELECT SIZE=3 NAME=\"motif_db_$motif_num\">\n";
          $line .= "  <OPTION VALUE='JASPAR CORE' SELECTED>JASPAR CORE </OPTION>\n";
          $line .= "  <OPTION VALUE='JASPAR PHYLOFACTS'>JASPAR PHYLOFACTS </OPTION>\n";
          $line .= "  <OPTION VALUE='JASPAR FAM'>JASPAR FAM </OPTION>\n";
  	  $line .= "</SELECT>\n"; 
        } # dna
      } elsif ($name =~ /Motif \d+ regular expression/) {	# regular expression
	$line1 = &format_hidden( "$section_name$motif_num", $line );
        $line1 =  $line;
      } else {		# other sections
	$line = &format_pre( $line );
      }
      print "$line\n";
    }
    $motif_num++;
  } # motif or summary section

  # print the time lines (or CPU if last block)
  $line = &next_block();
  $line = &format_pre( $line );
  print "$line\n";
}

#
# print the documentation section
#
$docfile = "$MEME_DIR/etc/meme-explanation.html";
open(IN, "<$docfile") || die("Can not open file $docfile: $!\n");
while (<IN>) { print; }
close(IN);

#
# Print the end of the HTML file.
#
print(&make_end(\%buttons));


# cleanup files
# note: "if ($status == 130) {cleanup(1);}" must follow $status = system(...)
&cleanup($status);

#-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
#-*  SUBROUTINES
#-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

#------------------------------------------------------------------------------
# Cleanup any temporary files
#------------------------------------------------------------------------------
sub cleanup {
  #system "rm $pgm.$$.*.tmp";
  if ($_[0] eq "INT") { exit(1); } else { exit($_[0]); }
} # cleanup
 
#------------------------------------------------------------------------------
# 	format_description
#
#	Format the Motif Description section of a MEME document.
#	Prints the probability matrix, information content diagram
#	and multilevel consensus as a single table with the columns 
#	aligned.
#	USAGE: format_description( line, ext, f )
#------------------------------------------------------------------------------
sub format_description {
  my(
    $line, 					# lines of text to format
    $ext, 					# motif number to append to name
    $f						# font to use
  ) = @_;
  my($i, $x, $w);
  my($result, @lines, @fields, $nf, $head, $letter, $label, $col, $mat, $diag);
  my($cons, $extra, $bold, $unbold, $fsize);

  @lines = split(/\n/, $line);			# break at newline

  $result = "<TABLE SUMMARY='motif description' CELLPADDING=0 CELLSPACING=1>\n";

  $extra = "<TD COLSPAN=8>";			# unused extra columns


  #
  # parse simplified probability matrix
  #
  undef(%majority);
  for ($i=0; $line = $lines[$i]; $i++) {	# until blank line
    @fields = split(/\s+/, $line);
    $nf = @fields;				# number of fields
    $head = join(" ", @fields[0..$nf-3]);	# the matrix heading
    $letter = $fields[$nf-2];			# the letter
    $mat = $fields[$nf-1];			# the matrix line
    if ($head =~ /Simplified/) { 
      $head = "<A NAME='simplified$ext' HREF=\"#simplified_doc\">$head</A>";
    } elsif ($head =~ /pos\.|prob|matrix/) {
      $head = "<A HREF=\"#simplified_doc\">$head</A>";
    }
    $result .= "<TR>$extra<TH>$head<TH ALIGN=RIGHT>$fsize$letter<TH>";
    $nfields = 0;
    # get color group for amino acid
    if ($db eq "protein") {			# get amino color group
      $c = get_color($db, $letter);
      $colors{$c} = 1;
    }
    foreach $x (split(//, $mat)) {
      $y = $x; $y = 0 if ($x eq ":"); $y = 10 if ($x eq "a");
      if ($db eq "dna") {			# get majority DNA letter
        if ($y > 5) { $majority{$nfields} = $letter; } 
      } else {					# record freq of amino category
        $majority{$nfields . $c} += $y; 
      } 
      $result .= ($nfields++ % 6 == 0) ? "<TD \n  >" : "<TD>";
      if ($x eq ":") {
        $result .= ":";				# matrix columns
      } else {
        $result .= "$fsize$x";			# matrix columns
      }
    }
    $result .= "\n";			# end of matrix line
  } # probability matrix

  #
  # parse information content diagram
  #
  $result .= "<TR><TD CLASS='invisible'>.</TD>\n";	# empty row
  for ($i++; $line = $lines[$i]; $i++) {	# until blank line
    $line =~ 
      /(\s+bits\s+|\S+\s|\(\d+\.\d+ bits\)|\s+|)\s*(\d+\.\d+) ([ \-\*]+)/;
    $head = $1;
    $label = $2;
    $diag = $3;
    $col = index($line, "-") if $diag =~ /^\-/;	# save column for next
    if ($head =~ /^\s*bits/) { 
      $head = "<A NAME='IC$ext'>$fsize $head</A>";
    } elsif ($head =~ /Information|content/) {
      $head = "<A HREF=\"#IC_doc\">$fsize $head</A>";
    }
    $result .= "<TR>$extra<TH>$fsize $head<TH ALIGN=RIGHT>$label<TH>";
    if ($diag =~ /^-/) {			# last row
      $result .= "<TD COLSPAN=" . length($diag) . "> <HR>";
    } else {
      $nfields = 0;
      foreach $x (split(//, $diag)) {
	# get color 
	if ($x eq "*") {
          $color = "black";
	  if ($db eq "protein") {		# get majority amino category
	    foreach $c (sort keys(%colors)) { 
	      if ($majority{$nfields . $c} > 5) { $color = $c; }
	    }
	  } else {				# get DNA color
	    $color = get_color($db, $majority{$nfields});
	  }
	} # get color
        $result .= ($nfields++ % 3 == 0) ? "<TD\n  " : "<TD";
	if ($x eq "*") {
	  $result .= " BGCOLOR=\"$color\">&nbsp;";
	} else {
	  $result .= ">"; 
	}
      }
    }
    $result .= "\n";			# end of plot line
  } # info content diagram

  #
  # parse multilevel consensus sequence
  #
  $result .= "<TR><TD CLASS='invisible'>.</TD>\n";	# empty row
  $bold = "<B>";
  $unbold = "</B>";
  for ($i++; $line = $lines[$i]; $i++) {	# until blank line
    $head = substr($line, 0, $col);
    $cons = substr($line, $col);
    if ($head =~ /Multilevel/) { 
      $head = "<A NAME='consensus$ext' HREF=\"#consensus_doc\">$head</A>";
    } elsif ($head =~ /consensus|sequence/) {
      $head = "<A HREF=\"#consensus_doc\">$head</A>";
    }
    $result .= "<TR>$extra<TH>$head<TD><TH>";
    $nfields = 0;
    foreach $x (split(//, $cons)) {
      $color = get_color($db, $x);
      $result .= ($nfields++ % 2 == 0) ? "<TD CLASS=$color\n  >" : "<TD CLASS=$color>";
      if ($x ne " ") {
	$result .= "<TT>$bold$x$unbold</TT>"; 
      }
      $result .= "</TD>";
    }
    $result .= "\n";			# end of consensus line
    $bold = $unbold = "";
  } # multilevel consensus

  return( $result );
} # format_description

#------------------------------------------------------------------------------
# 	format_sorted_sites
#
#	Add the aligned sites to the table created by format_description
#
#	USAGE: format_sorted_sites( line, ext, f )
#------------------------------------------------------------------------------
sub format_sorted_sites{
  my(
    $line, 					# lines of text to format
    $ext, 					# motif number to append to name
    $f						# font to use
  ) = @_;
  my($i, @lines, $cols, $result);
  my($seq, $str, $pv, $pre, $site, $post, $sp, $fsize);

  @lines = split(/\n/, $line);			# break at newline

  $line = $lines[0];				# first line
  $cols = ($line =~ /Strand/) ? 7 : 6;		# number of columns
  $str = "";					# if no strands
  $sp = "&nbsp;";				# space between columns

  #
  # print aligned sites
  #
  for ($i=2; $line = $lines[$i]; $i++) {	# skip first two (hdr) lines
    if ($cols == 6) {
      ($seq, $start, $pv, $pre, $site, $post) = split(/\s+/, $line);
    } else {
      ($seq, $str, $start, $pv, $pre, $site, $post) = split(/\s+/, $line);
    }
    $pre = "" if ($pre eq ".");			# remove placeholder $pre
    # print header first time thru
    if ($i==2) {
      $result .= "<TR><TD CLASS='invisible'>.</TD>\n";	# empty row
      $result .= "<TR><TH ALIGN=LEFT>NAME<TH>$sp";
      $len = length($site);
      $result .= ($cols == 6) ?  "<TH>$sp<TH>$sp" : "<TH>STRAND<TH>$sp";
      $result .= "<TH>START<TH>$sp<TH>P-VALUE<TH>$sp";
      $result .= "<TH>$sp<TH>$sp<TH>$sp";			# pre
      $result .= "<TH><TH COLSPAN=$len ALIGN=CENTER>";	# site
      $result .= "<A NAME='sites$ext' HREF=\"#sites_doc\">SITES</A>";
      $result .= "<TH>$sp\n";			# post
    } # header
    $result .= "<TR><TD>$seq<TD>";
    $result .= "<TD ALIGN=CENTER>$str<TD>";
    $result .= "<TD ALIGN=RIGHT>$start<TD>";
    $result .= "<TD ALIGN=RIGHT>$pv<TD>\n";
    $result .= "  <TD COLSPAN=2 ALIGN=RIGHT>$fsize<TT>$pre</TT><TD>";
    $nfields = 0;
    foreach $x (split(//, $site)) {		# print site in columns
      $color = get_color($db, $x);
      $result .= ($nfields++ % 2 == 0) ? "<TD CLASS=$color\n  >" : "<TD CLASS=$color>";
      $result .= "<TT><B>$x</B></TT>";
      $result .= "</TD>";
    }
    $result .= "<TD><TD><TD ALIGN=LEFT>$fsize<TT>$post</TT>\n";
  } # sorted site

  $result .= "</TABLE>";

  return($result); 
} # format_sorted_sites

#------------------------------------------------------------------------------
# 	format_training set
#
#	Format the training set description and set the %SEQNO global variable.
#
#------------------------------------------------------------------------------
sub format_training_set{
  my( $line ) = @_;
  my( $i, @lines, $nlines, $seqno );

  @lines = split /\n/, $line;
  $nlines = @lines;

  # find the start of the sequence lines
  for ($i=0; $i<$nlines; $i++) {
    last if ($lines[$i] =~ /^----/);
  } # line

  # get the sequence names and save in SEQNO
  $seqno = 0;                     		# sequence number
  for ($i++; $i<$nlines; $i++) {
    @words = split(/\s+/, $lines[$i]);
    $SEQNO{$words[0]} = $seqno++;
    if ($words[3]) { $SEQNO{$words[3]} = $seqno++; }
  }

  # use <pre> format
  return(&format_pre( $line ));
} # format_training set
