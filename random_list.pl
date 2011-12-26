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
if ($n_args != 2) {
    print "Generate a random subset of a list.\n";
    print "Usage: ./random_list.pl <list> <n out>\n";
    exit;
}

my $n_out = $ARGV[1];
my $list_count = 0;

open(F,$ARGV[0]) || die "Couldn't open file $ARGV[0]\n";
my @list = ();
while (my $i = <F>) {
    chomp $i;
    push (@list, $i);
    $list_count += 1;
}
close F;

if ($n_out == 0) {
    $n_out = $list_count;
}

if ($n_out > $list_count) {
    die "You asked for $n_out items, but $ARGV[0] only contains $list_count!\n";
}

srand;
my @order = ();
my $index = 0;
foreach(@list) {
    $order[$index] = [$index,rand(@list)];
    $index++;
}

my @random_order = sort {$a->[1] <=> $b->[1]} @order;

for (my $count = 0; $count < $n_out; $count++) {
    print "$list[$random_order[$count]->[0]]\n";
}

