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

#!/jgi/tools/bin//perl -w
$n_args = @ARGV;
if ( ($n_args != 3) && (($n_args%3 != 0) || ($n_args == 0)) ) {
    die "Usage: ./histogram2.pl datafile input_column binsize <<discrim_column min_data max_data>> ...\n";
}

open(F,$ARGV[0]);
my $bincolumn = $ARGV[1];
my $binsize = $ARGV[2];

my %discriminators = ();
my $bounded = 0;

if ($n_args > 3) {
    $bounded = 1;
    for (my $i = 3; $i < $n_args; $i+=3) {
	$discriminators{$ARGV[$i]} = $ARGV[$i+1] . "_" . $ARGV[$i+2];
    }
}

my $total_values = 0;
my $sum_of_values = 0;
my $sum_of_squared_values = 0;


my %histogram = ();
while (my $i = <F>) {
    chomp $i;
    $i =~ s/^\s+//;
    my @entries = split(/\s+/,$i);
    my $value = $entries[$bincolumn-1];

    my $good_value = 1;

    if ($bounded == 1) {

	while (my ($disc_col,$minmax) = each(%discriminators)) {
	    
	    my $discriminator = $entries[$disc_col-1];
	    my ($d_min,$d_max) = $minmax =~ /(.+)_(.+)/;

	    if ( ($discriminator < $d_min) || ($discriminator > $d_max) ) {
		$good_value = 0;
	    }
	}
    }

    if ($good_value == 1) {

	if ($total_values == 0) {
	    $max_value = $value;
	    $min_value = $value;
	} else {
	    if ($value > $max_value){$max_value = $value;}
	    if ($value < $min_value){$min_value = $value;}
	}
	$total_values++;
	$sum_of_values += $value;
	$sum_of_squared_values += $value*$value;
	$bin = sprintf("%.0f", $value/$binsize - 0.4999);
	if ($bin eq "-0") {$bin = "0"};

	if (!exists($histogram{$bin})) {
	    $histogram{$bin} = 1;
	} else {
	    $histogram{$bin}++;
	}
    }
}
close F;

my $max_counts = 1;
my $most_likely_bin = "NULL";

while (($bin, $value) = each(%histogram)) {

    if ($value > $max_counts) {
	$max_counts = $value;
	$most_likely_bin = $bin;
    }
}

$average = $sum_of_values/$total_values;
$std_dev = sqrt($sum_of_squared_values/$total_values - $average*$average);

printf("#Found %d total values totalling %.4f. <%.6f +/- %.6f>\n", 
       $total_values, $sum_of_values, $average, $std_dev);

print "#Range: [ $min_value - $max_value ]\n";
printf("#Most likely bin: [ %s - %s ] $max_counts counts\n",
       $most_likely_bin*$binsize,($most_likely_bin+1)*$binsize);


my @sorted_bins = sort {$a <=> $b} keys(%histogram);
my $so_far = 0;
my $bin_index=-1;
while ($so_far < $total_values/2.0) {
    $bin_index++;
    $so_far += $histogram{$sorted_bins[$bin_index]};
}

my $median_bin = $sorted_bins[$bin_index];
printf("#Median bin: [ %s - %s ] %s counts\n",
       $median_bin*$binsize, ($median_bin+1)*$binsize, $histogram{$median_bin});

my $entropy = 0;
foreach my $key (keys(%histogram)) {
    my $p = $histogram{$key}/$total_values;
    if ($p > 0) {
	$entropy -= $p*log($p)/log(2);
    }
}
printf("#Entropy = %.4f bits\n",$entropy);

my $max_bin = sprintf("%.0f", $max_value/$binsize - 0.4999);
my $min_bin = sprintf("%.0f", $min_value/$binsize - 0.4999);
if ($min_bin eq "-0") {$min_bin = "0"};
if ($max_bin eq "-0") {$max_bin = "0"};

$running_total = 0;
$last_bin_occupied = 0;
for ($bin = $min_bin; $bin <= $max_bin; ++$bin)
{
    if ($histogram{$bin})
    {
	$min = $bin*$binsize;
	$max = $min + $binsize;
#	if ( ($bounded == 1) && ($bincolumn == $disc_column) && ($min < $min_data) ) {$min = $min_data;}
#	if ( ($bounded == 1) && ($bincolumn == $disc_column) && ($max > $max_data) ) {$max = $max_data;}
	$running_total += $histogram{$bin};
	$cumul = sprintf("%.2e",$running_total/$total_values);
	$instant = sprintf("%.2e",$histogram{$bin}/$total_values);
#	$picture = "|" . "X" x sprintf("%.0f",40*$histogram{$bin}/$total_values);
	$picture = "|" . "X" x sprintf("%.0f",40*$histogram{$bin}/$max_counts);

#	$space = " " x sprintf("%.0f",40*(1-$histogram{$bin}/$total_values));
	$space = " " x sprintf("%.0f",40*(1-$histogram{$bin}/$max_counts));


	print "$picture$space $min - $max : [ $histogram{$bin} $instant $cumul $running_total ]\n"; 
	$last_bin_occupied = 1;
    } else {
	if ($last_bin_occupied == 1) {
	    print "#...\n";
	    $last_bin_occupied = 0;
	}
    }
}




