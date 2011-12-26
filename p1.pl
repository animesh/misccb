#!/usr/bin/perl -w
# mjd_permute: permute each word of input
use strict;



while (<>) {
	my @data = split;
	my $num_permutations = factorial (scalar @data);
	for (my $i =0; $i < $num_permutations ; $i++) {
	   my @permutation = @data [n2perm ($i , $#data)];
	   print " @permutation \n";
	   }
	}
	
	# Utility function: factorial with memorizing
BEGIN {
  my @fact = (1);
  sub factorial($) {
      my $n = shift;
      return $fact[$n] if defined $fact[$n];
      $fact[$n] = $n * factorial($n - 1);
  }
}