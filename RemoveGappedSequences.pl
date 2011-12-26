#!/usr/bin/env perl

if ($#ARGV != 0) {
		print "usage: $0 sequence file\n";
		print "Removes sequences that have 'N' or other unknown \n";
		print "nucleotides, since they are not handled by euler.\n";
		exit(1);
}
$in = shift @ARGV;

open(SEQ, $in) or die "cannot open $in\n";

$gapped = -1;
$numGapped = 0;
$numSeq    = 0;
$seq = "";
while(<SEQ>) {
		$line = $_;
		if ($line =~ /^>/) {
				$numSeq++;
				if ($gapped == 1) {
						$numGapped++;
				}
				elsif ($gapped == 0) {
						print $seq;
				}
				$seq = "";
				$gapped = 0;
		}
		else {
				if (($line =~ /N/) || ($line =~ /\./)) {
						$gapped = 1;
				}
		}
		$seq .= $line;
}
if ($seq ne "" and $gapped == 0) {
		print $seq;
}
				
						
