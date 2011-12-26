#!/usr/bin/perl
# convert2html.pl - this file is created from convert2html.txt  
#
# $Id: convert2html.txt 1800 2007-05-23 09:57:55Z tbailey $
# $Log$
# Revision 1.2  2005/08/23 23:52:41  nadya
# change form module back into pl file. Too much trouble to
# do separation of functions with *2html.pl files.
#
# Revision 1.1  2005/08/11 18:53:09  nadya
# rename from 2html. Perl does not understand modules if their name start as numeric.
#
# Revision 1.3  2005/08/11 17:42:58  nadya
# add exporting functionality
#
# Revision 1.2  2005/08/06 01:22:37  nadya
# add exporting info to the package
#
# Revision 1.1.1.1  2005/07/28 23:51:34  nadya
# Importing from meme-3.0.14, and adding configure/make
#


#
# subroutines and globals used by meme2html, mast2html and mhmm2html
#

$DIVIDER = "^\\*\\*\\*\\*\\*";		# section divider in output
$SUBDIV = "^--------------------------------------------------------------------------------";				# subsection divider in output
$ELIPSIS = "<B> &middot <BR> &middot <BR> &middot </B>";
$BODY = "#D5F0FF";	# the background color of the page (light blue by default)
$WEAK_FONT = "50% sans-serif";	# font size for weak motifs
$SCALE = 0.5;		# (sequence position)/(number of pixels)
$MAX_DIAGRAM = 2000;	# maximum number of pixels per diagram
$THIN_LINE = 4;		# thickness of thin spacer lines
$FAT_LINE = 8;		# thickness of fat spacer lines (for too long seqs)
$MIN_WIDTH = 30;	# minimum width (in pixels) for motifs
$MAX_NAME_LEN = 34;	# maximum length of truncated sequence name

# Colors for the motifs and their labels (motif numbers).
@MOTIF_COLORS=( "aqua", "blue", "red", "fuchsia",
        "yellow", "lime", "teal", '#444444',
        "green", "silver", "purple", "olive",
        "navy", "maroon", "black", "white" );
@MOTIF_LABEL_COLORS=( "black", "white", "white", "black",
        "black", "black", "white", "white",
        "white", "black", "white", "black",
        "white", "white", "white", "black" );
@IC_COLORS=( "red", "blue", "orange", "green", "black",
        "magenta", "pink", "yellow", "turquoise" );

# Added this global here--must be the same as the global in all the calling
# programs (meme2html, mast2html), but they will obsolete soon!
$MBPSUB = "XXX---XXX";

#-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
#-*  SUBROUTINES
#-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

#------------------------------------------------------------------------------
# print the HTML header (including style sheet) and set the body color
#	$title		title of HTML page
#	$body_color	background color; light blue if omitted
#------------------------------------------------------------------------------
sub print_header {
  local (
	$title,			# title of HTML page
	$body_color		# background color; light blue if omitted
  ) = @_;
  local ($i);

  if (defined($body_color)) { $BODY = $body_color; }

  print <<END;
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<HTML>
<HEAD>
<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
<TITLE>$title</TITLE>
END

  # print the style sheet
  print '<STYLE type="text/css">', "\n";

  # these save space compared with <FONT></FONT>

  # color for invisible lines
  print "  TD.invisible { color: '$BODY'; }\n";

  # TD classes for motifs
  for ($i=0; $i<@MOTIF_COLORS; $i++) {
    # normal motifs
    print "  TD.c$i { background: $MOTIF_COLORS[$i]; color: $MOTIF_LABEL_COLORS[$i]; }\n";
    # weak motifs
    print "  TD.cw$i { background: $MOTIF_COLORS[$i]; color: $MOTIF_LABEL_COLORS[$i]; font: $WEAK_FONT; }\n";
  }

  # B, TD and TH classes for IC diagrams and scale
  foreach $color (@IC_COLORS) {
    print "  B.$color { color: $color; }\n";
    print "  TD.$color { color: $color; }\n";
    print "  TH.$color { color: $color; }\n";
  }

  print "</STYLE>\n";

  #
  # end the header and start the body
  #
  print "</HEAD>\n";
  print "<BODY BGCOLOR='$BODY'>\n";
} # print_header
 
#------------------------------------------------------------------------------
# find a section marker with the specified keyword (case sensitive)
#------------------------------------------------------------------------------
sub find_section {

	local( $key ) = @_;
	local( $line );
	LINE:
	while ( <STDIN> ) {
		unless ( /\*\*\*\*\*/ ) { next LINE; }

		$line = <STDIN>;
		unless ( $line =~ /$key/ ) {
			<STDIN>;
			next LINE;
		}
		last;
	}
	return( $line );
} # find_section

#------------------------------------------------------------------------------
# next_section:
# Find a section marker and return the title line.  The following line of
# stars is removed.
#
# USAGE: $text = &next_section();
#------------------------------------------------------------------------------
sub next_section {
	local( $div ) = @_;
	local( $line );

	unless ( defined($div) ) {
		$div = $DIVIDER;
	} 

	while ( <STDIN> ) {
		unless ( /^$div/ ) { next; }
		$line = <STDIN>;
		<STDIN>;
		last;
	}
	chop $line;
	return( $line );
} # next_section

#------------------------------------------------------------------------------
# read_block:
# Read a block of text until terminated by a blank line.
#------------------------------------------------------------------------------
sub read_block {
	local( $line );

	LINE:
	while ( <STDIN> ) {
		if ( /^\s*$/ ) { last LINE; }
		$line .= $_;
	}
	return( $line );
} # read_block

#------------------------------------------------------------------------------
# next_block:
# Read the next block of text until terminated by a divider line.  The divider
# by default, is specificed in $DIVIDER (normally a line of *).  If a 
# parameter is passed in, it is used as the divider.
# Removes \r from input.
#
# USAGE: $text = &next_block();
#        $text = &next_block( divider );
#------------------------------------------------------------------------------
sub next_block {
	local( $div ) = @_;
	local( $line );

	unless ( defined($div) ) {
		$div = $DIVIDER;
	} 

	while ( <STDIN> ) {
		s/\r//g;
		if ( /$div/ ) { last; }
		$line .= $_;
	}
	return( $line );
} # next_block

#------------------------------------------------------------------------------
# format_section:
# Add HTML formatting for a section or subsection head.  
# The specified heading appears as
# the section title with the specified name as an internal link.
#
# USAGE: $text = &format_section( pre, link, rest, name, ext)
#------------------------------------------------------------------------------
sub format_section {
  my(
    $pre,					# just print this part
    $link,	 				# add link to this part
    $rest, 					# just print this part
    $name, 					# tag is "$name$ext"
    $ext 					# ref is to "$name_doc"
  ) = @_;
  my( $out, $ref, $tag );

  if ( $name =~ /^\s*$/ ) {			# all blank name
    $out = "<HR><CENTER> $pre <BIG><B>\n $link $rest\n</B></BIG></CENTER><HR>";
  } else {					# name given
    $ref = $name . "_doc";
    $tag = $name . $ext;
    $link = "<A HREF=\"#$ref\">$link</A>";
    $out = "<HR><CENTER><A NAME=$tag></A>\n $pre <BIG><B> $link $rest\n</B></BIG></CENTER><HR>";
  }
  return($out);
} # format_section

#------------------------------------------------------------------------------
# format_para:
# Add HTML formatting for a paragraph.  New lines are ignored, blank lines are
# converted to paragraphs.
#
# USAGE: $new_text = &format_para( text );
#------------------------------------------------------------------------------
sub format_para {
	local ( $line ) = @_;
	local( $out );

	$out = "<P>\n$line";
	$out =~ s/\n\s*\n/\n<P>\n/g;

	return( $out );
} # format_para

#------------------------------------------------------------------------------
# format_pre:
# Add HTML formatting for a preformatted block.  
#
# USAGE: $new_text = &format_pre( text );
#------------------------------------------------------------------------------
sub format_pre {
	local ( $line ) = @_;
	local( $out );

	$out = ($line=~ /\w/) ? "<PRE>\n$line</PRE>\n" : "";

	return( $out );
} # format_pre

#------------------------------------------------------------------------------
# format_diagrams:
# Convert the text diagrams to colored diagrams in HTML.  The scale is 1/$scale
# pixels per sequence position for sequences of up to $max_diagram*$scale pixel.
# Diagrams for longer sequences are scaled to fit in $max_diagram pixels 
# and the spacer lines are made thicker (and motif boxes may shrink).
# Weak motifs are labeled with font size $WEAK_FONT.
# When the motifs are protein and the database DNA, the motif widths are
# multiplied by 3 since they are in codon units to start with.
# Uses global variables @MOTIF_COLORS, $WEAK_FONT, $FAT_LINE, $THIN_LINE.
# 
#
# Sets global $META to contain information from SUMMARY of MOTIFS for 
# Meta-MEME.
#
#------------------------------------------------------------------------------
sub format_diagrams {
  my( $scale, $max_diagram, $text, $db, $stype, $xlate, $make_buttons,
    $col2hdr, $no_gi_names, $field_delim, $skip, $width_ref) = @_;
  my( $i, $out, @line, $l, $w, $wid_sum, $nmotifs, $nspacers);
  my( $max_spacers, $max_motifs, $name, $evalue, $diagram);
  my( @field, $f, $motif, $link, $seqno );
  my( @scale1, $col, $color, $font, $fsize, $wide, $fill, $mscale );
  my( $ncol, $dist, $w2, $loc, @nocc, @seqlen, $lno, $ncolors );

  $re_en = "\\([+-]?\\d*\\.?\\d*e[+-]?\\d+\\)";  # an e-format in parens

  # start a table and header row
  $out = "<TABLE SUMMARY='motif diagrams' BORDER=1>\n<TR>";
  $ncol = 0;				# number of columns in table
  $ncolors = scalar(@MOTIF_LABEL_COLORS);	# number of distinct motif colors

  # put buttons linking to score and annotation?
  if ($make_buttons) { $out .= "<TH>Links"; $ncol += 1; }

  if ($stype eq "s") {			# scoring DNA strands separately
    $out .= "<TH>Name<TH>Strand<TH>$col2hdr<TH ALIGN=LEFT>&nbsp;&nbsp; Motifs\n";
    $ncol += 4;
  } else {				# PROTEIN
    $out .="<TH>Name<TH>$col2hdr<TH ALIGN=LEFT>&nbsp;&nbsp; Motifs\n";
    $ncol += 3;
  }

  # split the text into lines
  $text =~ s/\n\s+//g;			# concat continued lines
  @line = split /\n/, $text;

  # find the width of each diagram
  if ($xlate) {				# translating DNA
    $mscale = 3;
  } else {
    $mscale = 1;
  }
  $max_width = 0;

  # remove header lines
  for ($i=0; $i<=$#line; $i++) {
    $l = $line[$i];
    last if ($l =~ /-------------/);
  }
  while ($i>=0) { shift @line; $i--; }

  #
  # calculate the approximate width of diagram
  #
  $lno = 0;				# line number
  $max_motifs = $max_spacers = 0;	# no diagrams read yet
  $seqno = 0;
  foreach $l (@line) {
	  if ($stype eq "s") {
	    ($name,$strand,$evalue,$diagram) = ($field_delim eq " ") ?
              split( " ", $l ) :
              split( $field_delim, $l );
	  } else {
	    ($name,$evalue,$diagram) = ($field_delim eq " ") ?
	      split( " ", $l ) :
	      split( $field_delim, $l );
	  }

	  # get sequence number
          if (!defined(%SEQNO)) {	# metameme
            $seqno++;
	  } elsif ($stype eq "s" && $strand eq "-") {
	    $seqno = $SEQNO{"-".$name};	# negative DNA strand
	  } else {
	    $seqno = $SEQNO{$name};
	  }

	  # skip this sequence?
	  if (defined($skip) && defined($skip[$seqno])) { next; }

	  # split the diagram into fields
	  @field = split( '[ _]', $diagram );
	  # calculate the approximate width
	  $wid_sum = $nocc[$lno] = 0;
	  foreach $f (@field) {
	    if ( $f =~ /[<>\[\]]/ ) {		# motif occurrence
	      ($motif) = $f =~ /[<\[][+-]?(\d+)[abc]?($re_en)?[>\]]/;
	      $wid_sum += $$width_ref{$motif} * $mscale;
              $seqlen[$lno] += $$width_ref{$motif};	# length of sequence
	      $nocc[$lno]++;			# number of motif occurrences
	    } elsif ($f ne "") {			# spacer
              $seqlen[$lno] += $f;		# length of sequence
	      $wid_sum += $f;
	    }
	  }
	  # calculate a scale so that diagram fits in $max_diagram pixels
	  $scale1[$seqno] = $scale;
	  if ($wid_sum/$scale > $max_diagram) { 
	    $scale1[$seqno] = $wid_sum/$max_diagram;
	  }
	  # calculate the exact scaled diagram width
	  $wid_sum = 0;
	  $nmotifs = 0;
	  $nspacers = 0;
	  foreach $f (@field) {
	    if ( $f =~ /[<>\[\]]/ ) {
	      ($motif) = $f =~ /[<\[][+-]?(\d+)[abc]?($re_en)?[>\]]/;
	      $wide = int($mscale*$$width_ref{$motif}/$scale1[$seqno]+0.5);
	      if ($wide < $MIN_WIDTH) { $wide = $MIN_WIDTH; }
	      $nmotifs++;
	    } elsif ($f ne "") {
	      $wide = $f/$scale1[$seqno];
	      $wide = int($wide + 0.5);		# round to integer
	      $nspacers++;
	    } else {
	      $wide = 0;
            } 
	    $wid_sum += $wide;
	  }
	  # save the length of the longest diagram
	  if ($wid_sum > $max_width) { 
            $max_width = $wid_sum; 
	    $max_motifs = $nmotifs;
	    $max_spacers = $nspacers;
	  }
    $lno++;					# line number
  } # line

  # kludge for Netscape 4.0; make width larger
  $max_width += 14 + (2*$max_motifs) + (3*$max_spacers);
  
  # set max_width to at least
  $min_width = int(50.0/$scale + 0.5); 
  if ($max_width < $min_width) { $max_width = $min_width; }
  
  # make the diagrams
  $META = "";
  $lno = 0;					# line number
  $seqno = 0;
  foreach $l (@line) {
    # length of space holder at end of diagram
    if ($stype eq "s") {
      ($name,$strand,$evalue,$diagram) = ($field_delim eq " ") ?
        split( " ", $l ) :
        split( $field_delim, $l );
    } else {
      ($name,$evalue,$diagram) = ($field_delim eq " ") ?
	split( " ", $l ) :
	split( $field_delim, $l );
    }
    if (!defined(%SEQNO)) {	# metameme
      $seqno++;
    } elsif ($stype eq "s" && $strand eq "-") {
      $seqno = $SEQNO{"-".$name};		# negative DNA strand
    } else {
      $seqno = $SEQNO{$name};
    }

    if (defined($skip) && defined($skip[$seqno])) {
      if (!defined($skip[$seqno-1])) { $out .= "<TR><TD COLSPAN=5>$ELIPSIS\n"; }
      next;				# skip this sequence
    } 

    # start row of table
    $out .= "<TR>\n ";

    if ($make_buttons) {
      $evalue = "$evalue";
      if ( $name =~ /\|/ ) {
	$button = make_button_panel("!", \%buttons, 
	  $db."entrez!$name", "score!$seqno", "align!$seqno", "help" );
      } else { 
	$button = make_button_panel("!", \%buttons, 
	  "score!$seqno", "align!$seqno", "help" );
      }
      $out .= "<TD>$button\n";
    } # make_buttons

    # write name of sequence 
    if ($no_gi_names) {
      $name =~ s/^gi\|\d+\|//;			# leading gi|123 removed
    }
    $link = "<A NAME=d$seqno></A>$name";
    $out .= " <TD>$link\n";
    $META .= "$name";

    if ($stype eq "s") {
      $out .= " <TD ALIGN=CENTER>$strand\n";
    }
    $out .= " <TD ALIGN=RIGHT NOWRAP>$evalue\n";
    $META .= " $evalue $nocc[$lno] $seqlen[$lno]";
    $out .= " <TD><TABLE SUMMARY='diagram $seqno' WIDTH=$max_width BORDER=0 ALIGN=LEFT CELLSPACING=0 CELLPADDING=0><TR ALIGN=CENTER>\n";
    @field = split( '[ _]', $diagram );
    $fsize = $THIN_LINE;		# font size for spacer line
    if ($scale1[$seqno] != $scale) { $fsize = $FAT_LINE; }
    $tail = $max_width;
    $position = 0;				# position in sequence
    foreach $f (@field) {
      if ($f eq "") {
        next;
      } elsif ( $f =~ /[<>\[\]]/ ) {		# motif occurence
	($st, $motif, $frame, $pv) = $f =~ 
          /[<\[]([+-]?)(\d+)([abc]?)($re_en)?[>\]]/;
        if (!defined $pv) {
          $pv = "(?)";
        }
        ($pv) = $pv =~ /\(([^)]+)\)/;
	$wide = int($mscale*$$width_ref{$motif}/$scale1[$seqno]+0.5);
	if ($wide < $MIN_WIDTH) { $wide = $MIN_WIDTH; }
        if ($motif eq ""){
          $color = "gray";
          $motif = "&nbsp";
        } else {
          $col = ( $motif - 1 ) % $ncolors; 
          $color = $MOTIF_COLORS[$col];
        }
	# set color and size of motif label;
	# weak motifs have font size $WEAK_FONT
	$font = "";
        $endfont = "";
        $class = ($f =~ /</) ? "cw$col" : "c$col"; 
	$out .= "  <TD CLASS='$class' WIDTH=$wide>$st$motif$frame\n";
        $META .= "  $st$motif $position $pv";	# for meta-meme
        $position += $$width_ref{$motif};	# letter position in sequence
      } elsif ($f ne "") {			# spacer
	$wide = $f/$scale1[$seqno];
	$wide = int($wide + 0.5);		# round to integer
	$out .= 
	 "  <TD WIDTH=$wide><HR SIZE=$fsize NOSHADE>\n";
        $position += $f;			# letter position in sequence
      }
      $tail -= $wide;
    }
    if ($tail > 0) { $out .= "  <TD WIDTH=$tail>\n"; }
    $out .= " </TABLE>\n";
    $META .= "\n";				# metameme data
    $lno++;					# line number
  } # line

  #
  # print a scale
  #
  $dist = 50;					# pixel distance between rules
  $w2 = $dist - 1.0/$scale;			# distance to second rule
  $ncol--;					# number of columns scale spans
  $out .= "<TR><TH CLASS='blue' COLSPAN=$ncol ROWSPAN=2 ALIGN=LEFT>SCALE\n";
  $out .= "  <TD><TABLE SUMMARY='scale' WIDTH=$max_width BORDER=0 ALIGN=LEFT CELLSPACING=0 CELLPADDING=0><TR ALIGN=CENTER>\n";
  $out .= "    <TD CLASS='blue' WIDTH=$w2 ALIGN=LEFT>|</TD>\n";
  for ($i=$dist; $i<$max_width; $i+=$dist) {
    $loc = $i * $scale;
    last if ($i > $max_width-50);	# make sure number will fit
    $out .= "    <TD CLASS='blue' WIDTH=$dist ALIGN=LEFT>|</TD>\n";
  }
  $out .= "  <TR><TD CLASS='blue' WIDTH=$w2 ALIGN=LEFT>1</TD>\n";
  for ($i=$dist; $i<$max_width; $i+=$dist) {
    $loc = $i * $scale;
    last if ($i > $max_width-50);	# make sure number will fit
    $out .= "    <TD CLASS='blue' WIDTH=$dist ALIGN=LEFT>$loc</TD>\n";
  }
  $out .= "  </TABLE>\n";

  # end the table
  $out .= "</TABLE>\n";

  return( $out );
} # format_diagrams

#------------------------------------------------------------------------------
#	find_line
#------------------------------------------------------------------------------
sub find_line {

    local( $key ) = @_;
    local( $line );

    LINE:
    while ( $line = <STDIN> ) {

        unless ( $line =~ /$key/ ) {
            next LINE;
        }
        last;
    }
    return( $line );
} # find_line

#------------------------------------------------------------------------------
# make_button_panel:
#
# Use the input associative array, and the specified keys to create a HTML
# table with labeled buttons.
#
# The buttons are defined in the associative array passed in the second
# parameter.  This array is indexed by the key, and contains three fields
# separated by the divider specified in the 1st parameter.  The fields 
# are the tag template, the background color, and the font color.  Note that
# a tag template that is a link will override the font color.
#
# The tag template is a string that, optionally, can have the key substituted
# into it at all positions marked by $MBPSUB (this is a symbol to avoid 
# collisions with arbitrary strings).
#
# Note that each line containing a button panel should be followed with a
# <BR CLEAR=LEFT> to prevent the next line from being indented to the position
# following the panel.  This is because, even though the panel uses a -1 font,
# it is still taller than the text and therefore creates an apparent indent in
# the same way a drop capital would.
#
# USAGE:
#	<string>  = &make_button_panel( <div>, <button def>, [<key1>,<key2>...] );
#
# button def:
# button defs have three elements separated by the divider (div) :
#	Element 1: template for text that will appear on the button.  This 
#	can be static text, or contain a place hold where key information will
#	be inserted ($MBPSUB).
#	Element 2: background color for the button.
#	Element 3: foreground color for the button.  Note if button is a link, 
#	this color will be overridden by the link color.
#
# key:
# keys have one or two elements separated by the divider:
#	Element 1: mandatory.  this is the button type and is used to look up
#			   the button definition.
#	Element 2: optional.  If present, this is the key information that will
#	be substituted for $MBPSUB.  If absent, the button type is used.
#
# default behavior:
# if a button type is undefined, a black and white button will be displayed 
# using the key information as the label on the button. 
#
# EXAMPLE
#	$MBPSUB = "XXXXXXX";
#	
#	%p1 = ( 
#         'aaa', 
#         '<A HREF=http://www.sdsc.edu/~gribskov>$MBPSUB</A>&#FFAAAA&white', 
#	  'b', 'B&#DDDDFF&white', 
#	  'c', 'C&white&black' 
#	);
#	
#	%p2 = ( 'fff', 'F green white',
#	        'ggg', 'G blue white' );
#	
#	$bpanel = &make_button_panel( "&", \%def, "a&?", "c" );
#	print "$bpanel text following the buttons goes here<BR CLEAR=LEFT>\n";
#	$bpanel =  &make_button_panel( " ",  \%p2, "fff", "ggg", "hhh" );
#	print "$bpanel A second panel with text<BR CLEAR=LEFT>\n";
#
#	19 November 1997     Michael Gribskov
#------------------------------------------------------------------------------
sub make_button_panel {

        local ($div, *def, @parm) = @_;
        local ($p, $n, $type, $text, $tag, $bkg, $bcolor, $font, $fcolor, $out);

        $out = "<TABLE SUMMARY='buttons' ALIGN=LEFT CELLSPACING=0><TR>";
        foreach $p ( @parm ) {

            # split the key into the tag (type) and text
            $n = ($type, $text) = split /$div/, $p;
            if ( $n > 1 ) {
                $key = $text;
            } else { $type=$p; $key = $p; }

            #look up the button definition using the type as key
            ($tag, $bcolor, $fcolor) = split /$div/, $def{$type};

            unless ( defined($tag) ) { $tag = $key; }
            unless ( defined($bcolor) ) { $bcolor = "white"; }
            unless ( defined($fcolor) ) { $fcolor = "black"; }

            # replace all occurrences of $MBPSUB with the key
            while ( $tag =~ s/$MBPSUB/$key/ ){};

            $out .= "\n  <TD BGCOLOR='$bcolor'>$tag";
            $c++;
        }
        $out .= "</TABLE>";

        return( $out );
} # make_button_panel

################################################################################
#	get_color
#
#	Get the name of the color for the given DNA or Protein letter. 
#	Colors are similar to those used by CLUSTAL.
#
################################################################################
sub get_color {
  local ($type, $letter) = @_;

  $_ = $letter;

  if ($type =~ /DNA|dna/) {
    if (/[Aa]/) {
      $color = "red";
    } elsif (/[Cc]/) {
      $color = "blue";
    } elsif (/[Gg]/) {
      $color = "orange";
    } elsif (/[Tt]/) {
      $color = "green";
    } else {
      $color = "black";
    }
  } elsif ($type =~ /PROTEIN|protein/) {
    if (/[ACFILMVWacfilmvw]/) {
      $color = "blue";
    } elsif (/[NQSTnqst]/) {
      $color = "green";
    } elsif (/[DEde]/) {
      $color = "magenta";
    } elsif (/[KRkr]/) {
      $color = "red";
    } elsif (/[Hh]/) {
      $color = "pink";
    } elsif (/[Gg]/) {
      $color = "orange";
    } elsif (/[Pp]/) {
      $color = "yellow";
    } elsif (/[Yy]/) {
      $color = "turquoise";
    } else {
      $color = "black";
    }
  }
  $color;
} # get_color 

#------------------------------------------------------------------------------
# Cleanup any temporary files
#------------------------------------------------------------------------------
sub cleanup {
  #system "rm $pgm.$$.*.tmp";
  if ($_[0] eq "INT") { exit(1); } else { exit($_[0]); }
} # cleanup

#
# Haoyuan added some functions here in order to convert mhmm result from 
# plain text to html (mhmm2html)
#

#-----------------------------------------------------------------------------
# format_subtile
# Add HTML sub_title, <hr><b><center> .... </center></b><hr>.
# 
# USAGE: $new_text = &format_subtitle(text)
#-----------------------------------------------------------------------------
sub format_subtitle {
    my ($line) = @_;
    my ($out);
    $out = "<HR>\n<CENTER><B>$line</B></CENTER>\n<HR>\n";
    return($out);
} #format_subtitle

#----------------------------------------------------------------------------
# format_hidden
# Add HTML Hidden Field
#
# USAGE: $new_text = &format_hidden(textname,textfield)
#----------------------------------------------------------------------------
sub format_hidden {
  my ($name, $field) = @_;
  my ($out);

  # replace confounding characters (">) that won't work in HIDDEN values
  $field =~ s/\"/&quot;/g;
  $field =~ s/\>/&gt;/g;

  $out = "<INPUT TYPE = \"HIDDEN\" NAME = \"$name\"".
    " VALUE = \"\n".
    "$field".
    " \">\n";
} #format_hidden

#----------------------------------------------------------------------------
# delete_first_letter
# Delete the first letter of a sentence
# In mhmm2html file, it is used to chop "#"
# USAGE: $new_text = &delete_first_letter(text)
#----------------------------------------------------------------------------
sub delete_first_letter{
    my ($line) = @_;
    my ($out);
    if (length($line) == 0){
	print (STDERR "Error: The length of your input is 0\n");
	exit(0);
    }
    else{
	my($length) = length($line);
	$out = substr($line,1,$length-1);
    }
    return($out);
}

#--------------------------------------------------------------------------
# make_end
# Add some "last" info. to the html output page
#
# USAGE: $end = &make_end(\%buttons)
#--------------------------------------------------------------------------
sub make_end{
    my ($buttons_ref) = @_;
    my ($out, $button);

    # button to take user to top of form
    $button = make_button_panel("!", $buttons_ref, "top");
    # finish form, body and documument
    $out = "<HR>$button<BR>\n</FORM>\n</BODY>\n</HTML>\n";
    return ($out);
}

#-------------------------------------------------------------------------
# format_table
# format html table from input plain text
#
# USAGE: $new_text = &format_table($text,$num_col)
#-------------------------------------------------------------------------
sub format_table{
    my ($out,@temp1,$i,@temp2,$end_mark,$j);
    my ($line,$num_col) = @_;
    print "<table border = \"1\">\n";
    @temp1 = split("\n",$line);
    for ($i=0;$i<=$#temp1;$i++){
	@temp2 = split(" ",$temp1[$i]);
	if ($num_col eq "0"){
	    $end_mark = $#temp2;
	}
	else{
	    $end_mark = $num_col-1;
	    for ($j=$num_col;$j<=$#temp2;$j++){
		$temp2[$num_col-1] = $temp2[$num_col-1]." ".$temp2[$j];
	    }
	}
	print "<tr>\n";
	for ($j=0;$j<=$end_mark;$j++){
	    print "<td>";
	    print "$temp2[$j]";
	    print "</td>";
	}
	print "\n</tr>\n";
    }
    print "</table>\n";
}

#######################################################################
# Add Entrez link
#######################################################################
sub add_entrez_link{
  my ($line, $alphabet) = @_;
  my($return_value);

  $return_value = 
    "<A HREF=\'http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=";
  if ($alphabet eq "dna"){
    $return_value .= "Nucleotide";
  } else {
    $return_value .= "Protein";
  }
  $return_value .= "&amp;cmd=Search&amp;term=$line&amp;doptcmdl=GenBank\' TARGET =\'_blank\'>$line</a>";

  return($return_value);
} # add_entrez_link
