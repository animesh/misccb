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

#!/jgi/tools/bin/perl

$n_args = @ARGV;
if (($n_args != 4) && ($n_args != 5)) {
    print "Filter a file for/against a list of entry names.\n";
    print "Usage: ./screen_list.pl <list file> <list column> <screen file> <screen column> <<keep?>>\n";
    print "If a third argument is given the list entries will be kept,\n";
    print "otherwise they will be rejected.\n";
    exit;
}

my $invert_sense = 0;
if ($n_args == 5) {$invert_sense = 1;}

my $list_column = $ARGV[1];
my $screen_column = $ARGV[3];

open(F,$ARGV[0]) || die "Couldn't open file $ARGV[0]\n";
my %bad_ones = (); 
while (my $i = <F>) {
    chomp $i;
    my @cols = split(/\s+/, $i);
    my ($id) = $cols[$list_column-1];
    $bad_ones{$id} = 1;
}
close F;

open(F,$ARGV[2]) || die "Couldn't open file $ARGV[1]\n";
my $good_one = 1;
while (my $i = <F>) {
    my @cols = split(/\s+/, $i);
    my ($id) = $cols[$screen_column-1];
    if (exists($bad_ones{$id})) {$good_one = 0;}
    else {$good_one = 1;}
    if (($invert_sense == 0) &&  ($good_one == 1)) {print $i;}
    elsif (($invert_sense == 1) &&  ($good_one == 0)) {print $i;}
}
close F;
