#!/usr/local/bin/perl
# $Id: rpts-lint.pl,v 1.3 2003/03/04 23:25:50 schwartz Exp $

use 5.006_000;
use strict;
use warnings;

# XXX - should read from lib/rpts.txt
my @RPTS = (
"Alu",
"B1",
"B2",
"BUR1",
"DNA",
"HERV",
"L1",
"L2",
"LINE",
"LINE1",
"LINE2",
"LTR",
"MIR",
"MML",
"Other",
"RNA",
"RNA",
"Simple",
"SINE",
"Unknown",
);

my @DIR = ("Left", "Right");

#our $opt_v = 0;
#getopts('v');

while (<>) {
    if (/^%:/) {
	print;
    } elsif (/^\s*$/) {
	next;
    } elsif (/^\s*(\d+)\s+(\d+)\s+(\w+)\s+(\w+)\s*$/) {
	die "invalid direction: $_" unless grep { $_ eq $3 } @DIR;
	die "invalid repeat type: $_" unless grep { $_ eq $4 } @RPTS;
	print "$1 $2 $3 $4\n";
    } elsif (/^\s*(\d+)\s+(\d+)\s+(\w+)\s*$/) {
	die "invalid repeat type: $_" unless grep { $_ eq $3 } @RPTS;
	print "$1 $2 $3\n";
    } else {
	die "invalid input: $_";
    }
}
