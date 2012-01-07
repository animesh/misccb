#
# Author: Jarrod Chapmon, Isaac Ho
#
# Copyright 2011 The Regents of the University of California.
# All rights reserved.

# The United States Government has rights in this work pursuant
# to contracts DE-AC03-76SF00098, W-7405-ENG-36 and/or
# W-7405-ENG-48 between the United States Department of Energy
# and the University of California.

# Redistribution and use in source and binary forms are permitted
# provided that: (1) source distributions retain this entire
# copyright notice and comment, and (2) distributions including
# binaries display the following acknowledgement:  "This product
# includes software developed by the University of California,
# JGI-PSF and its contributors" in the documentation or other
# materials provided with the distribution and in all advertising
# materials mentioning features or use of this software.  Neither the
# name of the University nor the names of its contributors may be
# used to endorse or promote products derived from this software
# without specific prior written permission.

# THIS SOFTWARE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE.

use strict;
# Args:
# - mercount ( sorted in asc order )
# - number of chunks

# OUTPUT - list of prefix start/prefix end pairs

open( F, $ARGV[0]) || die "couldn't open $ARGV[0]\n";

my $numChunks = $ARGV[1];

my $prefixLen = int( $numChunks ** ( 1/4 ) ) + 4;

# first scan: tally up total # of mers
my $totalMers = 0;
while( <F> )
{
  my( $mer, $cnt ) = split;
  $totalMers += $cnt;
}

sysseek( F, 0, 0 );

my $idealMersPerChunk = int( $totalMers / $numChunks );
my $runningSurplus = 0;  # how far from the ideal the total number of mers is
                         # amongst the divided set
my $totalInCurrentChunk = 0;
my $lastPrefix = "";
my $startPrefix = "";

while( <F> )
{
  # assume in sorted order
  # divide greedily
  my ( $mer, $cnt ) = split;

  my $prefix = substr( $mer, 0, $prefixLen );
  if ( $startPrefix eq "" ) { $startPrefix = $prefix }
  if ( !($prefix eq $lastPrefix) && !( $lastPrefix eq "" ) )
  {
     if ( $totalInCurrentChunk > $idealMersPerChunk )
     {
       # split here
       print "$startPrefix\t$lastPrefix\t$totalInCurrentChunk\n";
      
       # reset
       $totalInCurrentChunk = 0;
       $startPrefix = $prefix;
     }

  }
   
  $totalInCurrentChunk += $cnt;
  $lastPrefix = $prefix;
}
print "$startPrefix\t$lastPrefix\t$totalInCurrentChunk\n";
