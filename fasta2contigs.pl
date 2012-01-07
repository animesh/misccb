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

#!/usr/bin/perl
#
# script by Nik Putnam <nputnam@uclink4.berkeley.edu>
#
#$|=1;
#!/home/nputnam/perl56/bin/perl

use Getopt::Std;

sub dump_usage {
	print "parse a fasta file to find the contigs and report various output formats.
recognized options:
	-f input fasta (defaults to stdin)
	-q input fasta qualities
	-c output contigs fasta (optional)
	-w output quality score file (optional)
	-s output supercontigs format file (mullikin format)  (optional)
	-S output .contigsize file (optional.  defaults to stdout)
	-l layout file
\n" ;
	die();
}

$valid = getopts('l:f:q:c:w:s:S:', \%opts); 

if (!$valid) {dump_usage();}

$input_fasta = $opts{'f'} || "-" ;
$input_qual  =  $opts{'q'} || 0  ;
$contigbases  =  $opts{'c'}|| 0 ;
$contigquals  =  $opts{'w'}|| 0 ;
$supercontig  =  $opts{'s'}|| 0;
$contigsize_file =  $opts{'S'} || 0 ;
$layoutfile =  $opts{'l'} || 0 ;

if (! $contigquals ) {$input_qual=0;}

@lengths=();

if (! ($layoutfile || $contigbases || $contigquals || $supercontig || $contigsize_file) ) { $contigsize_file = "-";}

open(F,$input_fasta) || die ($!);
if ($input_qual) { open(Q,$input_qual) || die ($!); }
if ($contigbases) { open(CB,">$contigbases") || die ($!); }
if ($contigquals && $input_qual) { open(QB,">$contigquals") || die ($!); }
if ($supercontig) {open(SC,">$supercontig")|| die($!);} 
if ($contigsize_file) {open (CS,">$contigsize_file") || die($!) ;}
if ($layoutfile) {open (L,">$layoutfile") || die($!) ;}

sub len {
	$s=shift;
	$name = shift;
	$qs = shift;

	if ($contigquals && (length($s) != length($qs))) {
		die ( "length of s is ".length($s)." but length of qual string is ".length($qs)."\n" );
	}
	
	$count = 1;
	if ($s ne "") {
		$scaffold_length = length($s);
		$total_contig_length = 0;
		if ($supercontig) { print SC "supercontig $name\n"; }

#if ( $s=~/[Nn]$/ ) { $s =~ s/[Nn]*$//; }
#if ( $s=~/^[Nn]/ ) {	$s =~ s/^[Nn]*//; }

		@l = split(/([Nn][Nn][Nn][Nn]+)/,$s);
		$index = 1;
		map {
			$thislength = length($_);
			$thisend = $index + $thislength - 1;
			if (/^[Nn]*$/) {
				if ($supercontig) { print SC "gap ",length($_), " * * *\n"; }

			} else {
				$thisname = "${name}_c$count" ;
				$thisname2= "contig_$count";
				if ($supercontig) { print SC "contig $thisname\n"; } 
				if ($contigsize_file) { 
					print CS "$name\t$thisname2\t$index\t$thisend\t$thislength\n";
					$total_contig_length += $thislength;	
				}
				if ($layoutfile) {
					print L "$name\t$thisname\t$index\t$thisend\n";
				}
				if ($contigbases) {
					$seq = $_; 
					$seq =~ s/(.{50})/$1\n/g;	
					$seq =~ s/\n$//;
					print CB ">$thisname\n$seq\n";
				}
				if ($contigquals) {
					print QB ">$thisname\n";
					$qbcount=0;
					$qr = substr($qs,$index-1,$thislength);
					#print length($qr),"\n";
					map { print QB "$_ "; if (((1+$qbcount)%35) == 0) {print QB "\n"} $qbcount++;} unpack("C*",$qr);
					print QB "\n";
				}
				$count++;  
			}
			$index += length($_);
		} @l;
		if ($contigsize_file) { print CS "$name\t$scaffold_length\t$total_contig_length\t",$count-2,"\n";}
	}
}

my $s="";
while(<F>) {
	chomp;
	if (/^>/) {
		/^>(\S+)/;
		$nname=$1; 

		$qual = ""; $bb=0;
		$ls = length($s);
		if ($input_qual) {  while (length($qual) < $ls) { 
			$ql = <Q>;
			if (! ($ql=~/^>/)) {
				chomp($ql);
				@q = split(/\s+/,$ql);
				$qual .= pack("C*",@q);
				$bb++;
				#if (($bb % 10000) == 0) {print "$ls\t",length($qual),"\n";}
			} 
		} }

		len($s,$xname,$qual);
		$xname = $nname;
		$s="";
	} else {
		$s.=$_;
	}
}



$qual = ""; $bb=0;
$ls = length($s);
if ($input_qual) {  while (length($qual) < $ls) { 
	$ql = <Q>;
	if (! ($ql=~/^>/)) {
		chomp($ql);
		@q = split(/\s+/,$ql);
		$qual .= pack("C*",@q);
		$bb++;
#if (($bb % 10000) == 0) {print "$ls\t",length($qual),"\n";}
	} 
} }
len($s,$xname,$qual);

