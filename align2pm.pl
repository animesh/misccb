#!/usr/local/bin/perl -w
# $Id: align2pm.pl,v 1.9 2003/03/03 21:41:17 schwartz Exp schwartz $
use 5.006_000;
use strict;
use warnings;

use Getopt::Std;
our $opt_j = 0;
our $opt_h = 0;
our $opt_W = -1;
our $opt_x = -1;
our $opt_X = -1;
our $opt_Y = 0;
getopts("jhaW:x:X:Y:") or die;

my $flag_ends = 0;
my $W = $opt_W;
my $X0 = $opt_x;
my $X1 = $opt_X;
my $Y = $opt_Y;

$[ = 1;			# set array base to 1
$, = ' ';		# set output field separator
$\ = "\n";		# set output record separator

my %linestyle;
$linestyle{'l'} = 'Line';
$linestyle{'L'} = 'LLine';
$linestyle{'R'} = 'RLine';

if ($W == -1) {
    $W = 2;
}
my $needx = ($X0 eq -1);	#???
my $needy = ($Y eq -1);	#???
print '% W ' . $W;
print '% Y ' . $Y;
print '% X ' . $X0 . ' ' . $X1;

my $block = '';
my $lastp = 0;

line: while (<>) {
    chop;	# strip record separator
    my @Fld = split(' ', $_, 9999);

    if (/^}/) {
	if ($block eq 'd') {
	    print ') def';
	}
	if ($block eq 's' || $block eq 'S') {
	    print '/x0 ' . $X0 . ' def';
	    print '/x1 ' . $X1 . ' def';
	    print '/y0 ' . 0 . ' def';
	    $Y = $needy ? ($X1 - $X0 + 1) / 5 : $Y;
	    print '/y1 ' . int($Y + 0.5) . ' def';
	    print '/vtic-step ' . $Y / 10 . ' def';
	    print '/y-scale-fudge ' . $Y / 100 . ' def';
	}
	$block = ' ';
	next line;
    }

    if (/^s/) {
	$block = 's';
	next line;
    }

    if (/^a/) {
	$block = 'a';
	print 'plotdata';
	next line;
    }

    if (/^d/) {
	$block = 'd';

	print '/description (';
	next line;
    }

    if ($block eq 'd') {
	print $_;
    }

    if ($block eq 's' && $#Fld == 3) {
	if ($needx) {
	    $X0 = $Fld[2];
	    $X1 = $Fld[3];
	}
	$block = 'S';
    }

    if ($flag_ends && $block eq 'a' && $_ =~ /^[   ]*[b]/ ) {
	$lastp = 0;
# printf STDERR "b $_\n";
	printf "%d %d %d %d %d %d %s\n",
	    $Fld[2] + $X0 - 1, 0 * $Y / 100,
	    $Fld[2] + $X0 - 1, 100 * $Y / 100,
	    0.0001, 0.5, "Red CLine";
    }
    if ($flag_ends && $block eq 'a' && $_ =~ /^[   ]*[e]/ && $#Fld == 3) {
	$lastp = 0;
# printf STDERR "e $_\n";
	printf "%d %d %d %d %d %d %s\n",
	    $Fld[2] + $X0 - 1, 0 * $Y / 100,
	    $Fld[2] + $X0 - 1, 100 * $Y / 100,
	    0.0001, 0.5, "Blue CLine";
    }

    # l x0 y0 x1 y1 
    if ($block eq 'a' && $_ =~ /^[   ]*[lLR]/ && $#Fld == 5) {
	&outline_y($Fld[2], $Fld[3], $Fld[4], $Fld[5], $Fld[1])
		if (!$opt_h);
    }

    # l x0 y0 x1 y1 p	
    if ($block eq 'a' && $_ =~ /^[   ]*[lLR]/ && $#Fld == 6) {
	&outline_p($Fld[2], $Fld[4], $Fld[6], $Fld[1], $lastp)
		if (!$opt_h);
	$lastp = $Fld[6];
	chomp $lastp;
    }
}

sub outline_y {
    my($x0, $y0, $x1, $y1, $t) = @_;
    if ($y0 != $y1) {	#???
	print '% ' . $y0 . ' != ' . $y1;
    }
    printf "%d %d %d %d %d %d %s\n",
	$x0 + $X0 - 1,
	$y0 * $Y / 100 ,
	$x1 + $X0 - 1,
    	$y1 * $Y / 100,
	$W,
	0,
	$linestyle{$t};
}

sub outline_p {
    my($x0, $x1, $p, $t, $p0) = @_;
    if ($opt_j && $p0 != 0) {
	printf "%d %d %d %d %g %g %s\n",
	    $x0 + $X0 - 1,
	    $p0 * $Y / 100,
	    $x0 + $X0 - 1,
	    $p * $Y / 100,
	    0.0001, 0.85, "Line";
    }
    printf "%d %d %d %d %g %g %s\n",
	$x0 + $X0 - 1,
	$p * $Y / 100,
	$x1 + $X0 - 1,
	$p * $Y / 100,
	$W, 0, $linestyle{$t};
}
