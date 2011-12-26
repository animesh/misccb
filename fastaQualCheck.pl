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

#!/home/nputnam/perl56/bin/perl
use balign;
use SeqWithQual;
use Bio::SeqIO;
use strict;

if ($#ARGV<1) {die "usage: fastaQualCheck.pl <fasta in> <qual in>\n";}

my $i;
my $gapqual;
my $scaffold_qual;
my $gapseq;
my $scaffold_seq;
my $seqo;
my $qualo;
my $seq="";
my $qual="";
my $contig=0;
my $BW=$ARGV[4];
my $pre=-1;
my @queue=();
my $cscaffold=0;
my %contig;
my $emit;
my $gap;
my $scaffold=0;
my $length=0;


my $fin  = Bio::SeqIO->new(-file => $ARGV[0] , '-format' => 'Fasta');
open ( QIN, $ARGV[1]);
my (@n,$n,$qname);

my ($qb,$qs);
$qb = <QIN>;
($qname)=$qb=~/^>(\S+)/;
while ( $seqo = $fin->next_seq() ) {

	($qname)=$qb=~/^>(\S+)/;
	$qb = <QIN>; 
	$qs=""; 
	chomp($qb); 
	while ((! ($qb =~ /^>/)) && (! eof(QIN))) { 
		$qs .= $qb." "; 
		$qb = <QIN>; 
		chomp($qb); 
	}
	if (eof(QIN)) {$qs.=$qb}

	$seq = $seqo->seq ();
	$n= split(/\s+/,$qs);
#	print $seqo->id(),"\t$qname\t",$seqo->desc(),"\t",length($seq),"\t$n\n";
	if ( ! (($seqo->id() eq $qname ) && (length($seq) == $n)) ) {
		print "inconsistent: ",$seqo->id(),"\t$qname\t",$seqo->desc(),"\t",length($seq),"\t$n\n";
	}
}
