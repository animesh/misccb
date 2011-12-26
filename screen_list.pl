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

#!/usr/local/bin/perl

$n_args = @ARGV;
if (($n_args != 2) && ($n_args != 3)) {
    print "Filter a fasta file for/against a list of entry names.\n";
    print "Usage: ./screen_list.pl <list> <fasta file> <<keep?>>\n";
    print "If a third argument is given the list entries will be kept,\n";
    print "otherwise they will be rejected.\n";
    exit;
}

my $invert_sense = 0;
if ($n_args == 3) {$invert_sense = 1;}

open(F,$ARGV[0]) || die "Couldn't open file $ARGV[0]\n";
my %bad_ones = (); 
while (my $i = <F>) {
    chomp $i;
    $bad_ones{$i} = 1;
}
close F;

open(F,$ARGV[1]) || die "Couldn't open file $ARGV[1]\n";
my $good_one = 1;
while (my $i = <F>) {
    if ($i =~ />/) {
	my ($id) = $i =~ />(\S+)/;
	if (exists($bad_ones{$id})) {$good_one = 0;}
	else {$good_one = 1;}
    }
    if (($invert_sense == 0) &&  ($good_one == 1)) {print $i;}
    elsif (($invert_sense == 1) &&  ($good_one == 0)) {print $i;}
}
close F;
