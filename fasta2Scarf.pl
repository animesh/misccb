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

# verifies that seq and qual are in same order/same lengths
sub readSeq
{
	my ( $pFh, $pHeader, $pName, $pLen, $pNextHeader ) = @_;
  my $fh = $$pFh;
	my $seq = "";

	die unless $$pHeader =~ />(\S+)/;
	$$pName = $1;
	$$pLen = 0;
	while( <$fh> )
	{
		if ( $_ =~ />/ ) 
	  {
			$$pNextHeader = $_;
			return $seq;   # typical exit case
		}

		chomp;
		$$pLen += length( $_ );
		$seq .= $_;
	}
  return $seq;  #incase EOF
} 

sub readQual
{
  my ( $pFh, $pHeader, $pName, $pLen, $pNextHeader ) = @_;
	my $fh = $$pFh;
  my @qual = ();

	die unless $$pHeader =~ />(\S+)/;
	$$pName = $1;
	$$pLen = 0;
	while( <$fh> )
  {
    if ( $_ =~ />/ )
		{
			$$pNextHeader = $_;
			return ( \@qual );  # typical exit case
		}
		chomp;
		my @q = split(/\s+/, $_ );
		$$pLen += scalar( @q );
    push @qual, @q;
	}
  return( \@qual ); #incase EOF
}

open( my $seqFh, $ARGV[0] ) || die "couldn't open $ARGV[0]\n";
open( my $qualFh, $ARGV[1] ) || die "couldn't open $ARGV[1]\n";

my $sHeader = <$seqFh>;
my $qHeader = <$qualFh>;

while( !eof( $seqFh )  )
{
	 die "qual file ends prematurely compared to seq file" if eof( $qualFh );
	 my $sName = "";
	 my $sLen = 0;
	 my $nextSHeader = "";
   my $qName = "";
	 my $qLen = 0;
	 my $nextQHeader = "";

   my $seq = readSeq( \$seqFh, \$sHeader, \$sName, \$sLen, \$nextSHeader );

	 my $pQual = readQual( \$qualFh, \$qHeader, \$qName, \$qLen, \$nextQHeader );

	 die "sequence/qual out of order: seq = $sName, qual = $qName\n" unless
		 $sName eq $qName;
	 die "lengths do not match for seq $sName ( $sLen ) and qual $qName ( $qLen )\n" unless $sLen == $qLen;
  

   # print scarf out
   $sName =~ /(\S+)\.(\S+)/; 
   my $prefix = $1;
   my $suffix = $2;
   my $pairInfo = -1;
   if ( $suffix =~ /[xXbB]/ )
   {
			 $pairInfo = 1;
   }
   elsif ( $suffix =~ /[yYgG]/ )
   {
       $pairInfo = 2;
   }
   die "couldn't recognize pairing info for $sName\n" unless $pairInfo != -1;

   #### Begin printing SCARF read
   print $prefix.":0:0:0:0/".$pairInfo.":";    # print the name/pair info
   
   print $seq; # print seq
   print ":";
   
   # convert the qual into scarf non-numeric string: print it
   foreach my $q ( @{ $pQual } )
   {
			print chr( 64 + $q );
	 }
	 print "\n";  
   ### done printing SCARF read

	 $sHeader = $nextSHeader;
	 $qHeader = $nextQHeader;
}
die "seq file ends prematurely compared to qual file\n" if !eof( $qualFh );
