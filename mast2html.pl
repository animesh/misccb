#!@WHICHPERL@
#
# $Id: mast2html.pl 1800 2007-05-23 09:57:55Z tbailey $
# $Log$
# Revision 1.3  2005/08/23 23:56:33  nadya
# change use of convert2html from module to perl file
# update regexpr string that makes a tag
#
# Revision 1.2  2005/08/12 17:52:20  nadya
# rename 2html into convert2html. Perl needs module name start with alpha char,
# not numeric
#
# Revision 1.1.1.1  2005/07/30 02:00:59  nadya
# Importing from meme-3.0.14, and adding configure/make
#
#

use lib qw(@PERLMODDIR@);
require "@PERLMODDIR@/convert2html.pl";

$pgm = $0;                      # name of program
$pgm =~ s#.*/##;                # remove part up to last slash
@args = @ARGV;                  # arguments to program
$status = 0;                    # exit status
$SIG{'INT'} = 'cleanup';        # interrupt handler

#
# get directories
#
 
$usage = <<USAGE;		# usage message
  USAGE:
        $pgm [-e_skip <min> <max>]

	[-e_skip <min> <max>]	skip sequences where <max> > e-value > <min>
USAGE

# mast2html:
# Starting with standard MAST output:
#     add external links to NCBI
#     add internal navigation links
#     add html motif diagrams
#
# adapted from the earlier diagram program dh3.pl
#
# v1.00     24 October 1997     Michael Gribskov
# v1.01		18 November 1997    Tim Bailey
#	update to MAST 2.3, include nucleic acid sequences
# v1.02     18 November 1997    Michael Gribskov
#	added buttons 
# v1.03		 4 December 1997	Michael Gribskov
#	added handling for non NCBI/gi sequences
# v1.04		 5 December 1997	Tim Bailey
#	converted non-motif regions to lines
#	bug fixes
# v1.05		 5 December 1997	Michael Gribskov
#	bug fixes
#   	reduce HTML table output and format source
# v1.06		16 December 1997	Tim Bailey
#	Make diagrams each a separate table embedded in single table for sec ii.
#	Spacer lines increased in thickness when sequence too long to fit.
#	Weak motifs shown in smaller font.
#	Translated DNA shown at 1/3 scale.
#	Minimum motif width in pixels enforced.
#	Fixed splitting of diagrams in annotation section.
#	Relies on peptide/nucleotide flags printed by MAST for db and motifs.
# v1.07 	17 March 1997		Tim Bailey
#	Fix Netscape 4.0 bug with embedded tables;
#	Required 1) setting WIDTH in embedded table to width + 40 (magic number)
#		 2) adding an invisible last <TD> to give all tables same width
# v1.08 	23 March 1997		Tim Bailey
#	Add -e_skip switches for use in preparing sample MAST output
# v1.09 	30 March 1997		Tim Bailey
#	Add \n after elipsis; change padding for Netscape 4.0 to
#		4 + 2*#motifs + 3*#spacers
# v1.10
#	change padding for Netscape 4.0 to
#		14 + 2*#motifs + 3*#spacers
#	remove trailing "|" in sequence name in annotation section (PDB bug)
# v2.0	major rewrite for MEME 3.0

$MIN_E_SKIP = 1;	# skip sequences in this evalue range 
$MAX_E_SKIP = 0;	

# Define the buttons used for internal and external links

$ENTREZ = "http://www.ncbi.nlm.nih.gov/entrez/query.fcgi";
$MBPSUB = "XXX---XXX";
%buttons = ( 
  "nentrez", 
  "<A HREF='$ENTREZ?db=nucleotide&cmd=Search&term=$MBPSUB&doptcmdl=GenPept'>E</A>!#DDDDFF!#000000",
  "pentrez", 
  "<A HREF='$ENTREZ?db=protein&cmd=Search&term=$MBPSUB&doptcmdl=GenPept'>E</A>!#DDDDFF!#000000",
  "hentrez", "<A HREF='\#bh'>E</A>!#DDDDFF!#000000",
  "diagram", "<A HREF='\#d$MBPSUB'>D</A>!#DDFFDD!#000000",
  "align",   "<A HREF='\#a$MBPSUB'>A</A>!#FFDDDD!#000000",
  "score",   "<A HREF='\#s$MBPSUB'>S</A>!#DDDD88!#000000",
  "help",    "<A HREF='\#bh'>?</A>!#FFFFFF!#000000",
  "motifs", "<A HREF='\#motifs'><B>Dataset and Motifs</B></A>!#00FFFF!#000000",
  "hm","<A HREF='\#sec_i'><B>High-scoring Sequences</B></A>!#DDFFDD!#000000",
  "md","<A HREF='\#sec_ii'><B>Motif Diagrams</B></A>!#FFDDFF!#000000",
  "ma", "<A HREF='\#sec_iii'><B>Annotated Sequences</B></A>!#00FF00!#000000",
  "debug", "<A HREF='\#debug'><B>Debugging Information</B></A>!#DDDDFF!#000000",
  "top", "<A HREF='\#top_buttons'><B>Go to top</B></A>!#DDDDFF!#000000" 
);

#
# get input arguments
#
while ($#ARGV >= 0) {
  if ($ARGV[0] eq "-e_skip") {
    shift; $MIN_E_SKIP = $ARGV[0];
    shift; $MAX_E_SKIP = $ARGV[0];
  } else {
    print stderr $usage;
    exit(1);
  }
  shift;
}

# header

#print "<HTML>\n<HEAD>\n<TITLE>MAST</TITLE>\n</HEAD>\n<BODY BGCOLOR=$BODY>\n";
&print_header("MAST");

# top button panel

$button = make_button_panel("!", \%buttons, "motifs", "hm", "md", "ma","debug");
$button = "<A NAME=top_buttons></A><HR>\n$button\n<BR CLEAR=LEFT>\n";

# title

$line = &next_section();			# get start of MAST output
# detect error: first word must be "MAST" 
if (!($line =~ /^MAST/)) {			# MAST error; exit
  print "<PRE>$line";
  while (<STDIN>) { print; } 
  print "</PRE>";
  exit(1);
} else {					# no error; print buttons
  print $button;
}
#$line = &format_section( "", "", $line );
$line = &format_section( "", $line );
print "$line\n";
$line = &next_block();
$line =~ s/(http:.*)/<A HREF='\1'>\1<\/A>/g;
$line = &format_para( $line );
print "$line\n";

# reference

$line = &next_section();
#$line = &format_section( "", "", $line );
$line = &format_section( "", $line );
print "$line\n";
$line = &next_block();
$line = &format_para( $line );
print "$line\n";

# database and motifs

$line = &next_section();
$line = &format_section( "", "", $line, "motifs" );
print "$line\n";
$line = &next_block();
%width = &read_motif( $line );
# figure out type of database and motifs
if ($line =~ /DATABASE[^\n]*(nucleotide)/) { $db = "n"; } else { $db = "p"; }
if ($line =~ /MOTIF[^\n]*(nucleotide)/) { $mt = "n"; } else { $mt = "p"; }
# decrease number of pixels per database letter if motifs are protein and DB DNA
$xlate = $db eq "n" && $mt eq "p";				# tranlating DNA?
if ($xlate) { $SCALE *= 3; }
# figure out how are DNA strands scored
if ($db == "n") {			# DNA
  if ($line =~ /are combined/) { $stype = "c"; }		# combined
  if ($line =~ /are scored separately/) { $stype = "s"; }	# separate
  if ($line =~ /are not scored/) { $stype = "n"; }		# norc
} else {				# protein
  $stype = "p";				# protein
}
$line = &format_pre( $line );
print "$line\n";

# Section I introduction

if (!$brief) {
  $line = &next_section();
  $line = &format_section( "", "", $line, "sec_i" );
  print "$line\n";
  $line = &next_block();
  $line = &format_list( $line, "-o" );
  print "$line\n";
} else {			# header already read
  $line = &format_section( "", "", $line, "sec_i" );
  print "$line\n";
}

# Section I

$line = &next_block( "---" );
$line = &next_block();
$line = &format_sec_i( $line, $db, $stype, $xlate );
print "$line\n";

# Section II introduction

$line = &next_section();
$line = &format_section( "", "", $line, "sec_ii" );
print "$line\n";
if (!$brief) {
  $line = &next_block( );				# get introduction

  # remove "-d-" line
  $line =~ s/o -d-.+//; 

  # remove occurrence ... occurrence line
  $line =~ s/occurrence.+occurrence//;

  # replace [snf] and <snf> with snf in appropriate font
  $thdr = "<TABLE SUMMARY='intro' BORDER=0 CELLPADDING=0><TR ALIGN=CENTER>";
  $spacer = "<TD WIDTH=10><HR SIZE=$THIN_LINE NOSHADE>";
  $fspacer = "<TD WIDTH=10> <HR SIZE=$FAT_LINE NOSHADE>";
  $block = "<TD CLASS='c1' WIDTH=20>";
  $wblock = "<TD CLASS='cw1' WIDTH=20>";
  $line =~ s/\[(\w+)\]/$thdr $spacer $block \1 $spacer<\/TABLE>/;
  $line =~ s/<(\w+)>/$thdr $spacer $wblock \1 $spacer<\/TABLE>/;
  $max_len = int($MAX_DIAGRAM*$SCALE);

  # create a line talking about long sequences and thick lines
  $line .= "o $thdr $fspacer $block 1 $fspacer <\/TABLE> ";
  $line .= "Sequences longer than $max_len are not shown to scale and are indicated by thicker lines.\n";

  # make an HTML list out of the introduction
  $line = &format_list( $line, "-o" );
  print "$line\n<BR>\n";
} # print section II introduction

# Section II

$line = &next_block();
$line = &format_diagrams( $SCALE, $MAX_DIAGRAM, $line, $db, $stype, $xlate,
  1, "Expect", 1, ' ', $SKIP, \%width);
print "$line\n";

# Section III introduction

$line = &next_section();
$line = &format_section( "", "", $line, "sec_iii" );
print "$line\n";
if (!$brief) {
  $line = &next_block();
  $line = &format_list( $line, "-o" );
  print "$line\n<HR>\n";
}

# Section III

$line = &next_block();

$line = &format_sec_iii( $db, $line );
print "$line\n";

# everything else

print "<A NAME=debug></A>";
print "<HR><CENTER><H3>Debugging Information</H3></CENTER><HR>\n";
print "<PRE>\n";
while ( <STDIN> ) { print; }
print "</PRE>\n";


# help for buttons

$line = &button_help;
print "$line\n";

# button to top

$button = make_button_panel("!", \%buttons, "top");
print "<HR>$button<BR>\n";

# finish off HTML

print "</BODY>\n</HTML>\n";

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
  if ($_[0] eq "INT") { exit(1); } else { exit($_[0]); }
}

#------------------------------------------------------------------------------
# read_motif:
# read the motif widths
#
# USAGE: %width = &read_motif( text );
#------------------------------------------------------------------------------
sub read_motif {
	local( $text ) = @_;
	local( @line, $width );

	# split the text into lines

	@line = split /\n/, $text;

	for ( $i=0; $i<@line; $i++ ) {
		if ( $line[$i]=~/----- / ) { last; }
	}
	$i++;
	for ( ; $i<@line; $i++ ) {
		if ( $line[$i] =~ /^\s*$/ ) { last; }
		($num,$width,$seq) = split( ' ', $line[$i] );
		$width{$num} = $width;
	}
	return( %width );

}

#------------------------------------------------------------------------------
# format_def:
# Add HTML formatting for a definition block.  
#
# USAGE: $new_text = &format_def( text, term_marker, definition_marker );
#------------------------------------------------------------------------------
sub format_def {
	local ( $line, $markterm, $markdef ) = @_;
	local( $out );

	$out = "<DL>\n$line</DL>";
	$out =~ s/$markterm/<DT>$markterm/g;
	$out =~ s/$markdef/<DD>/g;

	return( $out );
}

#------------------------------------------------------------------------------
# format_tab:
# format the text into a table with the specified field widths.  Text is
# broken at white space.
#
# USAGE: $text = &format_tab( N, [N widths], text );
#        $text = &format_tab( 2, 100, 150, text );
#------------------------------------------------------------------------------
sub format_tab {

	local( @param ) = @_;
	local( $out, $ncol, $i, $wid, $wid_sum );

	$ncol = $param[0];
	$wid_sum = 0;
	for ( $i=0; $i<$ncol; $i++ ) {
		$wid[$i] = $param[$i+1];
		$wid_sum += $wid[$i];
	}

	# split the text into lines

	@line = split /\n/, $param[$ncol+1];

	$out = "<TABLE SUMMARY='format_tab' BORDER=0 CELLPADDING=0 WIDTH=$wid_sum>\n";
	foreach $l (@line) {
		$out .= "     <TR>\n";
		@field = split /\s+/, $l;
		for ( $i=0; $i<$ncol; $i++ ) {
			$out .= " <TD WIDTH=$wid[$i]>$field[$i]\n";
		}
	}
	$out .= "</TABLE>\n";
	return $out;
} # format_tab

#------------------------------------------------------------------------------
# format_list:
# format the text as a bulletted list using the characters in mark as markers
# for the list items.
#
# USAGE: $line = &format_list( text, mark );
#------------------------------------------------------------------------------
sub format_list {
	local( $text, $mark ) = @_;
	local( @markers, $m, $newm, $oldm, @line, $l, $i, $out );

	# change the marker list to an array
	
	@markers = split //, $mark;
	$nmarks = @markers;

	# split the text into lines

	@line = split /\n/, $text;

	$oldm = 0;

	foreach $l ( @line ) {

		#print stderr "$l\n";
		$l =~ /^\s*(\S.*$)/;	# get first word of line	
		$l = $1;

		# find out what line starts with (bullet or not)
		$newm = 1;
		marker:
		foreach $m (@markers) {
			$mm = $m;
			if ( $l =~ /^$m / ) { last marker; }	# word is marker
			$newm++;
		}
		#print stderr "mark=$newm     oldm =$oldm   nmarks=$nmarks\n";

		# if line starts with a bullet (marker) adjust indentation leve
		if ( $newm <= $nmarks ) {	
			#print stderr "     matched :$mm:\n";
			$l =~ s/^$mm/<LI>/;	# changer marker to <LI>
			# finish previous indent levels if at lower level now
			for ( $i=$newm+1; $i<=$oldm; $i++ ) {
				$out .= "</UL>\n";
				#print stderr "</UL> level $i\n";
			}
                        # start new indent levels if at higher level now
			for ( $i=$oldm+1; $i<=$newm; $i++ ) {
				$out .= "<UL>\n";
				#print stderr "<UL> level $i\n";
			}
			$out .= "$l\n";
			$oldm = $newm;
		} else {
			$out .= "$l\n";
			#print stderr "     not matched\n";
		}

	}

	# end of text: close off indentation levels
	for ( $i=$oldm; $i>=1; $i--) {
		$out .= "\n</UL>\n";
		#print stderr "</UL> level $i newm = $oldm\n";
	}
	return( $out );

} 
#------------------------------------------------------------------------------
# format_sec_i:
# Format section i by adding external links to sequence names and internal
# links to section ii from e-values.
#
# USAGE: $text = &format_sec_i( text, db, stype, xlate );
#------------------------------------------------------------------------------
sub format_sec_i {
	local( $text, $db, $stype, $xlate ) = @_;
	local( @line, $l, $out, $n, $d, $i, $descr, $evalue, $length, $seqno );

	$out .= "<TABLE SUMMARY='sec_i' BORDER><TR><TH>Links<TH ALIGN=LEFT>Sequence Name";
	if ($xlate) {				# translate DNA
	  $out .= "<TH ALIGN=LEFT>Description<TH>Frame<TH>E-value<TH>Length\n";
	  $n = 2;
        } elsif ($stype eq "s") {		# strands scored separately
	  $out .= "<TH ALIGN=LEFT>Description<TH>Strand<TH>E-value<TH>Length\n";
	  $n = 2;
	} else {				# single strand/frame
	  $out .= "<TH ALIGN=LEFT>Description<TH>E-value<TH>Length\n";
	  $n = 1;
	}

	@line = split /\n/, $text;
	$seqno = 0;			# sequence number
	line:
	foreach $l (@line) {
		if ( $l =~/^\s+$/ ) {
			$out .= "$l\n";
			next line;
		}
		( $name, @rest ) = split /\s+/, $l;
		$d = $#rest;
		# store sequence number indexed by name of sequence; prepend
		# "-" if this is the negative DNA strand
		if ($stype eq "s" && $rest[$d-2] =~ /-/) {
		  $SEQNO{"-".$name} = ++$seqno;
		} else {
		  $SEQNO{$name} = ++$seqno;
		}
	        $no_gi_name = $name;
                $no_gi_name =~ s/^gi\|\d+\|//;			# leading gi|123| removed 
		$link = "<A NAME=s$seqno></A>$no_gi_name";
		if ( $name =~ /\|/ ) {
    		  $button = make_button_panel("!",\%buttons, $db."entrez!$name","diagram!$seqno","align!$seqno","help" );
		} else { 
		  $button = make_button_panel("!",\%buttons, "diagram!$seqno","align!$seqno", "help" );
		}
		$descr = "";
		for ($i=0; $i<$d-$n; $i++) { $descr .= "$rest[$i] "; }
		$descr =~ s/(.*)gi\|.*/$1/;	# remove appended gi numbers

		$length = $rest[$d];
		$evalue = "$rest[$d-1]";

		# don't output sequences with e-values in middle range
		if ($evalue>$MIN_E_SKIP && $evalue<$MAX_E_SKIP) {
                  $SKIP[$seqno] = 1;
		}

		if (! $SKIP[$seqno]) {
		  $out .= sprintf " <TR><TD>%s\n <TD>%s\n <TD>%s\n", 
		    $button, $link, $descr;
		  if ($xlate || $stype eq "s") {	# print strand/frame
		    $frame = $rest[$d-2];
		    $out .= sprintf " <TD ALIGN=CENTER>%s\n", $frame;
		  } # strand/frame 
		  $out .= sprintf " <TD ALIGN=RIGHT>%s\n <TD ALIGN=RIGHT>%s\n", 
		    $evalue, $length;
		} else {
                  if(!$SKIP[$seqno-1]) {$out .= "<TR><TD COLSPAN=6>$ELIPSIS\n";}
		}
	}

	$out .= "</TABLE>\n";

	return( $out );
} # format_sec_i

#------------------------------------------------------------------------------
# format_sec_iii:
# Format section iii by adding external links to sequence names
#
# USAGE: $text = &format_sec_iii( db, text );
#------------------------------------------------------------------------------
sub format_sec_iii {
	local( $db, $text ) = @_;
	local( @line, $l, $out, $rest, $name, $gi, $source, $sdb, $acc);
	local( $short, $doc, $first, $seqno );

	@line = split /\n/, $text;

	$first = 1;
	$out = "";
	$l = 0;
	line:
	while ( $l < @line ) {

		# skip blanks
		if ( $line[$l] =~ /^\s*$/ ) { $l++;  next line; }

		# initialize current entry
		$entry = "";

		# title line; don't print HR for first one
                if ($first) { $first = 0;} else { $entry .= "\n<HR>\n";}

		( $name, $rest ) = split /\s+/, $line[$l++], 2;
                $name =~ s/^>//;                # remove ">" if present

		# get sequence number
                $trunc_name = substr($name, 0, $MAX_NAME_LEN);	# other sections have truncated names
		if ($db eq 'n' && $rest =~ /-/) { 
		  $seqno = $SEQNO{"-".$trunc_name};	# DNA - sequence
		} else {
		  $seqno = $SEQNO{$trunc_name};
		}
		if ( $name =~ /\|/ ) {
    		  $button = make_button_panel("!",\%buttons, 
		    $db."entrez!$name", "score!$seqno", "diagram!$seqno","help");
		} else { 			#assume 1st token is name
		  $button = make_button_panel("!",\%buttons,
		    "score!$seqno","diagram!$seqno", "help" );
		}

		$entry .= "<A NAME=a$seqno></A>$name $rest";
		$entry .= "$button<BR CLEAR=LEFT>\n";

		# strip out the documentation and reformat
		$doc = "";
		while ( $l < @line ) {
  		  if ( $line[$l] =~ /^\s*LENGTH/ ) { last; }
		  $doc .= $line[$l++];
		}
		if ( $doc =~ /gi\|/s ){
		  $doc =~ s/gi\|/<UL>gi\|/;
		  $doc =~ s/\n//g;
		  $doc =~ s/(gi)\|/<BR CLEAR=LEFT>\n<LI>$1\|/g;
		  $doc .= "\n</UL>";
		}
		
		# now convert each gi name in the list to an entrez link 
		# using the gi as the query
		@gilist = ($doc =~ /gi\|\S+/gs);
		foreach $g (@gilist) {
		  $link = &link_entrez( $db, $g );
		  $g =~ s/\|/\\|/g;
		  $doc =~ s/$g/$link/s;
		}

		$doc .= "<BR>\n";
		$entry .= $doc;
		$entry .= "$line[$l++]<BR>\n";
		# print diagram lines
                $diagram = "";
		while ( $l < @line && $line[$l] =~ /\S/) { 
		  $diagram .= $line[$l++];
		}
    		$diagram =~ s/_/-/g;			# replace _ with -
		$entry .= $diagram;

		# the alignment section
		$entry .= "<PRE>";
		while ( $l < @line ) {
		  if ( $line[$l] =~ /^[>a-zA-Z].*/ ) { last; }
		  $entry .= "$line[$l++]\n";
		}
		$entry .= "</PRE>\n";
		if (! $SKIP[$seqno]) { 
		  $out .= $entry; 
		} else {
                  if (! $SKIP[$seqno-1]) { $out .= "$ELIPSIS"; }
		}
	}

	return( $out );
} # format_sec_iii

#------------------------------------------------------------------------------
# link_entrez:
# Convert a name to Entrez link. gi is used for entrez query. source name is 
# used for tag text.
#
# USAGE: $text = &link_entrez( <database>, <name> );
#------------------------------------------------------------------------------
sub link_entrez {
	local( $db, $name ) = @_;
	local( $long, $gi, $source, $sdb, $acc, $short );
	local( $out );

	($long, $gi, $source, $sdb, $acc, $short ) = &get_name($name);
	if ( $gi eq "" ) { $gi = $source; }
  	$out = "<A HREF='http://www.ncbi.nlm.nih.gov/htbin-post/Entrez/query?db=$db&form=6&uid=$gi&dopt=g'>";
	$out .= $source;
	$out .= "</A>";

	return( $out );
}

#------------------------------------------------------------------------------
# entrez_link
# Convert a name to Entrez link 
# and make a local link to this point if prefix is not empty string.
#
# USAGE: $text = &entrez_link( db, name, prefix );
#------------------------------------------------------------------------------
sub entrez_link {
	local( $db, $name, $prefix ) = @_;
	local( $button, $on );
	local( $out );

	if ($prefix ne "") {
          $out .= "<A NAME=$prefix.$name></A>";
	}
	$type=$db."entrez!".$name;
  	$out .= "<A HREF='http://www.ncbi.nlm.nih.gov/htbin-post/Entrez/query?db=$db&form=6&uid=$name&dopt=g'>";
	$on = sprintf "%-24.24s", $name;
	$on  =~ s/(\S+)(\s+)/$1<\/A>$2/;
	$out .= $on;

	return( $out );
}

#------------------------------------------------------------------------------
# link_internal:
# add the specified internal link to all occurances of the key string.
#
# USAGE: $text = &link_internal( text, name, key );
#------------------------------------------------------------------------------
sub link_internal {
	local ($line, $name, $key ) = @_;

	$line =~ s/$key/<A HREF='#$name'>$key<\/A>/g;
	return( $line );
}

#------------------------------------------------------------------------------
# target_internal:
# add the specified string to the text as a NAME target for an internal
# link
#
# USAGE: $text = &target_internal( text, name );
#------------------------------------------------------------------------------
sub target_internal {
	local ($line, $name );
	local ( $out );

	$out = "<A NAME=$name>&nbsp;</A>";
	$out .= $line;

	return( $out );
}

#------------------------------------------------------------------------------
# get_name:
# Extract a gi identifier string and break it up into some commonly used 
# components: the name (i.e rightmost identifier), the gi (number following
# gi|), and the complete identifier.
#
# note that PDB entries have the chain concatenated to the entry name.
#
# USAGE:
#	($long, $gi, $source, $db, $acc, $short ) = &get_name($line);
#
# 19 November 1997     Michael Gribskov
#------------------------------------------------------------------------------
sub get_name {
    local( $line ) = @_;
    local( $short, $long, $source, $db, $acc, $gi );

	if ( $line=~/gi\|/ ) {
		($long) = $line =~ /(gi\|[^\s]*).*/;
    	($short) = $long =~ /\|([^|]*)$/;
    	($gi, $source) = $line =~ /gi\|([^| ]*)(\S*).*/;
    	unless ( ($source=~/\S/)  ) {			# if no source info following gi
        	$source = "gi|".$gi;
    	} else {
        	$source =~ s/^\|(.*)/$1/;
    	}
	} elsif ( $line=~/\|/ ) {							# if no gi|, assume token with | is a name
		($source) = $line =~ /^\s*(\S*\|\S+)\s*.*/;
		$long = $source;
	} 
		
    ($db, $acc, $short) = split /\|/, $source;
    unless ( $acc=~/\S/ ) { $acc = $short; }

	# special treatment for PDB chains

	if ( $db eq "pdb" ) {
		$short = $acc.$short;
	}

    return( $long, $gi, $source, $db, $acc, $short );
}

#------------------------------------------------------------------------------
# button_help:
#
# make a key for the buttons. This is not a general procedure - you must know
# the divider and button definitions.
#
# USAGE:
#	$text = &button_help;
#
#------------------------------------------------------------------------------
sub button_help {

	local( $out );

	$out  = "<A NAME=bh></A>\n";
	$out .= "<A NAME=sbh></A>\n";
	$out .= "<A NAME=dbh></A>\n";
	$out .= "<A NAME=abh></A>\n";
	$out .= "<HR><CENTER><H3>Button Help</H3></CENTER><HR>\n";
	$out .= make_button_panel("!",\%buttons, "hentrez!\#bh" );
	$out .= "Links to Entrez database at <A HREF='http://www.ncbi.nlm.nih.gov'>NCBI</A> <BR CLEAR=LEFT>\n";
	$out .= make_button_panel("!",\%buttons, "score!bh" );
	$out .= "Links to sequence scores (<A HREF='\#sec_i'>section I</A>) <BR CLEAR=LEFT>\n";
	$out .= make_button_panel("!",\%buttons, "diagram!bh" );
	$out .= "Links to motif diagrams (<A HREF='\#sec_ii'>section II</A>) <BR CLEAR=LEFT>\n";
	$out .= make_button_panel("!",\%buttons, "align!bh" );
	$out .= "Links to sequence/motif annotated alignments (<A HREF='\#sec_iii'>section III</A>) <BR CLEAR=LEFT>\n";
	$out .= make_button_panel("!",\%buttons, "help!#bh" );
	$out .= "This information <BR CLEAR=LEFT>\n";

	return $out;
}

#------------------------------------------------------------------------------
