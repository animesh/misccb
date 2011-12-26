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

#!/usr/local/bin/perl -w

$n_args = @ARGV;
if (($n_args != 1) && ($n_args != 2) && ($n_args != 3)) {
    die "usage: unique.pl <input file> <<column>> <<sum column?>>\n";
}

open(F,$ARGV[0]) || die "Couldn't open file $ARGV[0]\n";
my $use_col = -1;

if ($n_args > 1) {
    $use_col = $ARGV[1] - 1;
    if ($use_col < 0) {
	die "error: column specification must be >= 1\n";
    }
}

my %sums = ();
my $do_sum = 0;
my $sum_col = 0;
if ($n_args == 3) {
    $do_sum = 1;
    $sum_col = $ARGV[2] - 1;
}

my %attempts = ();

while (my $i = <F>) {
    chomp $i;

    my $val = 0;
    if ($use_col != -1) {
	my @cols = split(/\s+/,$i);
#	my @cols = split(/\t/,$i);
	my $n_cols = @cols;
	if ($use_col+1 > $n_cols) {
	    warn "Warning: column specification exceeds number of columns in line: $i\n";
	    next;
	}
	$i = $cols[$use_col];
	if ($do_sum == 1) {
	    $val = $cols[$sum_col];
	} 
    }

    if (exists($attempts{$i})) {
	$attempts{$i} += 1;
	if ($do_sum == 1) {
	    $sums{$i} += $val;
	}
    } else {
	$attempts{$i} = 1;
	if ($do_sum == 1) {
	    $sums{$i} = $val;
	}
    }
}
close F;

while (my ($id, $count) = each(%attempts)) {
    if ($do_sum == 1) {
	my $sum = $sums{$id};
	print "$id\t$count\t$sum\n";
    } else {
	print "$id\t$count\n";
    }
}


