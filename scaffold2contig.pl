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

use Bio::Seq;
use Bio::SeqIO;

if ( scalar( @ARGV ) != 1 )
{
		die "usage: <scaffold_seq_file>\nOUT: contigs in fasta format\n";
}

my $out = Bio::SeqIO->new( '-format' => 'Fasta', -file => ">-");
my $in = Bio::SeqIO->new( '-format' => 'Fasta' , -file => $ARGV[0]);
# read in list of names to flip

my @contigIds = ();

my @seqHashList = ();
while (( my $seqObj = $in->next_seq() ) )
{
		my $remainingSeq = $seqObj->seq();
		my $labelPrefix = $seqObj->display_id();
		my $contigCnt = 1;
		my $scaffoldStart = 1;
		my $scaffoldStop = 1;
		while ( $remainingSeq =~ /(\S+?)(N{10,})(\S+)/ )
		{
				my $contig = $1;
				my $gap = $2;
				$remainingSeq = $3;	
				$scaffoldStop = $scaffoldStart + length( $contig ) - 1;
				my $newSeqObj = Bio::Seq->new( '-seq' => $contig, '-display_id' => $labelPrefix."\.".$contigCnt."\.".$scaffoldStart."-".$scaffoldStop );
				$out->write_seq( $newSeqObj );
				
				$contigCnt++;
				$scaffoldStart += length( $contig ) + length( $gap );
		}
		# write the remainder of this scaffold entry
		my $contig = $remainingSeq;
		my $scaffoldStop = $scaffoldStart + length( $contig );
		my $newSeqObj =  Bio::Seq->new( '-seq' => $contig, '-display_id' => $labelPrefix."\.".$contigCnt."\.".$scaffoldStart."-".$scaffoldStop );
		$out->write_seq( $newSeqObj );
}

