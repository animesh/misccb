#!/util/bin/perl -w
use strict;



##
# ParseBlastOutput
#
# Minimalistic parser for blast output. It saves all the
# subject-query sets for which the e value (Blast probability) is
# smaller than a threshold value. Setting threshold_e = -1
# will save all the sets.
#
# How to use:
#
#  perl ParseBlastOutput.pl blast_in threshold
#
# where blast_in is the input file (the blast output), and threshold is
# either -1 or an integer > 0 (for example, if you want to save only
# blast hits with a probability of e-150 or better, set threshold = 150).
# Setting threshold = -1 will save all the hits.
#



# Parse arguments (if threshold_e = -1, all hits are saved).
my $file_name = $ARGV[0];
my $threshold_e = $ARGV[1] || -1;

open ( BLAST_OUT, $file_name );

# Some variables.
my @splitted_line;
my @splitted_prob;
my $a_line;

my $query_name;
my $query_length;
my $subj_name;
my $subj_length;
my $blast_prob;
my $n_matches;
my $align_length;
my $subj_strand;
my $query_strand;

my $on_align = 0;
my $subj_start = -1;
my $subj_stop = - 1;
my $query_start = -1;
my $query_stop = -1;

# Main loop.
while ( $a_line = <BLAST_OUT> ) {  

  # Entering an alignment region.
  if ( $on_align == 0 && $a_line =~ "Query:" ) {
    $on_align = 1;
  }
  
  # Leaving an alignment region (print previous alignment data).
  if ( $on_align &&
       ( $a_line=~ /Query= / || $a_line=~ />/
	 || $a_line=~ /Score = / || $a_line=~ /Database:/ ) ) {
    @splitted_prob = split /e-/, $blast_prob;
    if ( -1 == $threshold_e
	 || $blast_prob eq "0.0"
	 || ( $splitted_prob[1] > $threshold_e ) ) {
      print
	$subj_name, "  ",
	$subj_length, "  ",
	$query_name, "  ",
	$query_length, "  ",
	$blast_prob, "  ",
	$n_matches, "  ",
	$align_length, " ",
	$subj_strand, " ",
	$query_strand, " ",
	$subj_start, " ",
	$subj_stop, " ",
	$query_start, " ",
	$query_stop, "\n";
    }
    
    $on_align = 0;
    $subj_start = -1;
    $subj_stop = -1;
    $query_start = -1;
    $query_stop = -1;
  } 

  # If in an align region.
  if ( $on_align ) {
    if ( $a_line =~ /Query:/ || $a_line =~ /Sbjct:/ ) {
      @splitted_line = split " ", $a_line;

      if ( $splitted_line[0] =~ /Query:/ ) {
	if ( $query_strand =~ /\+/ ) {
	  if ( $query_start == -1 ) {
	    $query_start = $splitted_line[1];
	  }
	  $query_stop = $splitted_line[3];
	}
	else {
	  if ( $query_start == -1 ) {
	    $query_stop = $splitted_line[1];
	  }
	  $query_start = $splitted_line[3];
	}
      }
      
      if ( $splitted_line[0] =~ /Sbjct:/ ) {
	if ( $subj_strand =~ /\+/ ) {
	  if ( $subj_start == -1 ) {
	    $subj_start = $splitted_line[1];
	  }
	  $subj_stop = $splitted_line[3];
	}
	else {
	  if ( $subj_start == -1 ) {
	    $subj_stop = $splitted_line[1];
	  }
	  $subj_start = $splitted_line[3];
	}
      }
    }
  }
  
  # Query name.
  if ( $a_line =~ "Query=" ) {
    @splitted_line = split /= / , $a_line;
    $a_line = $splitted_line[1];
    @splitted_line = split / / , $a_line;
    $query_name = $splitted_line[0];
    chomp $query_name;
  }
  
  # Query length.
  if ( $a_line =~ /letters\)/ ) {
    @splitted_line = split " ", $a_line;
    $query_length = substr( $splitted_line[0], 1);
    $query_length =~ s/,//;
  }
  
  # Subject name.
  if ( $a_line =~ />/ ) {
    @splitted_line = split />/ , $a_line;
    $a_line = $splitted_line[1];    
    @splitted_line = split / / , $a_line;
    $subj_name = $splitted_line[0];
    chomp $subj_name; 
  }
  
  # Subject length.
  if ( $a_line =~ /Length = / ) {
    @splitted_line = split / = / , $a_line;
    $subj_length = $splitted_line[1];
    chomp $subj_length;

    $subj_length =~ s/,//;
  }
  
  # Probability.
  if ( $a_line =~ /Expect =/ ) {
    @splitted_line = split / = /, $a_line;
    $blast_prob = $splitted_line[2];
    chomp $blast_prob;
  }

  # Nmuber of matches and length of alignment.
  if ( $a_line =~ /Identities =/ ) {
    @splitted_line = split / = | \(/, $a_line;
    my $match_length = $splitted_line[1];
    @splitted_line = split /\//, $match_length;
    
    $n_matches = $splitted_line[0];
    $align_length = $splitted_line[1];
    $n_matches =~ s/,//;
    $align_length =~ s/,//;
  }  

  # Strands.
  if ( $a_line =~ /Strand =/ ) {
    @splitted_line = split / = | \//, $a_line;

    if ( $splitted_line[1] =~ /Plus/ ) { $query_strand = "+"; }
    else { $query_strand = "-"; }
    
    if ( $splitted_line[2] =~ /Plus/ ) { $subj_strand = "+"; }
    else { $subj_strand = "-"; }      
  }

}
