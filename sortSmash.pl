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

#!/jgi/tools/bin/perl -w
#
# sortSmash.pl by Jarrod Chapman <jchapman@lbl.gov> Wed Jun 10 22:41:54 PDT 2009
# Copyright 2009 Jarrod Chapman. All rights reserved.
#

use Getopt::Std;
my %opts = ();
my $validLine = getopts('c:', \%opts);
my @required = ("c",);
my $nRequired = 0;
map {$nRequired += exists($opts{$_})} @required;
$validLine &= ($nRequired == @required);
if ($validLine != 1) {
    print "Usage: ./sortSmash.pl <-c countFile>\n";
    exit;
}

my $countFile = $opts{"c"};

my $count = 0;
my $name = undef;

open (C,$countFile) || die "Couldn't open $countFile\n";
while (my $line = <C>) {
    chomp $line;
    my @cols = split(/\s+/,$line);
    unless (defined($name)) {
	$name = $cols[0];
	$count = $cols[1];
    } else {

	if ($cols[0] eq $name) {
	    $count += $cols[1];
	} else {
	    print "$name\t$count\n";
	    $name = $cols[0];
	    $count = $cols[1];
	}
    }
}
close C;
print "$name\t$count\n";
