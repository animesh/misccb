#!/util/bin/perl -w
use strict;



##
# GetChromosome
#
# Select one chromosome from a fasta file in the MGSC_2002April11_V3 format.
#
# How to use:
#
#  GetChromosome   file.fasta   chr
#
# where file.fasta is the fasta file and chr is the chromosome (eg "X",
# "12", etc.)
#


# Parse arguments
my $file_name = $ARGV[0];
my $chromosome = $ARGV[1];

open ( FASTA, $file_name );

# Some variables.
my @splitted_line;
my $a_line;
my $switch = 0;

# Main loop.
while ( $a_line = <FASTA> ) {  
  if ( ! $switch ) {
    if ( $a_line =~ />$chromosome\./ ) {
      $switch = 1;
      print $a_line;
    }
    else {
      next;
    }
  }
  else {
    if ( $a_line =~ />/ ) {
      if ( $a_line =~ />$chromosome\./ ) {
	print $a_line;
      }
      else {
	last;
      }
    }
    else {
      print $a_line;
    }
  }

}
