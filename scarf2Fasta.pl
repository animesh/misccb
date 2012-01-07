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

sub pad
{
	my( $x, $desiredLen ) = @_;
  while( length( $x) < $desiredLen )
  {
		 $x = "0".$x;
	}
	return $x;
}
die "Usage: <libPrefix>\nSTDIN: scarf seq\n" unless ( scalar( @ARGV ) == 1 );
my $lib = $ARGV[0];

while( <STDIN> )
{
	chomp;
	my @a = split( /:/, $_ );
	$_ =~ /(\S+)\#(\S+)\/(\d+)/;
	my $xOrY = "x";
	if ( $3 == 2 ) { $xOrY = "y" }
  
	my $originalName = $a[0].":".$a[1].":".$a[2].":".$a[3].":".$a[4];

  # replace text in columns
  $a[0] =~ /(\d+)/;
	$a[0] = $1;
	$a[4] =~ /(\d+)/;
	$a[4] = $1;

	# max columns for ID:
  # This is an assumption made my looking at ~50x of pichia data.  not
  # to be relied on long-term
  # 1 : 3
  # 2 : 1
  # 3 : 3
  # 4 : 4
  # 5 : 4
  
	my $a = pad( $a[0], 3 );
	my $b = pad( $a[1], 1 );
	my $c = pad( $a[2], 3 );
	my $d = pad( $a[3], 4 );
	my $e = pad( $a[4], 4 );

	my $seq = $a[5];
	my $replacementName = $lib.$a.$b.$c.$d.$e.".$xOrY";

	# we decide to ignore the replacement name and just use the original name
  #   because that's what the blastOno code wants.  But we keep the code
  #   here for future reference ( a future mode can be installed )
	print ">$originalName\n";
	for( my $i = 0; $i < length( $seq ); $i++ )
	{
		print substr( $seq, $i, 1 );
		if ( ( $i % 80 ) == 79 ) { print "\n" }
	}
	print "\n";
}
