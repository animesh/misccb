#!/usr/bin/perl -w
# $Id: mkunderlays.pl,v 1.13 2009/03/25 19:06:29 rico Exp $

# Blue EST
# 0  10 Green
# 20 50 EST
# 60 90 (label) EST

use warnings;
use strict;

# input format:
#  \w \w
#  \d \d \w
#  \d \d ([^)]*) \w

my @colors = (
	"Black",       "Blue",       "Cyan",        "DarkBlue",    "DarkCyan",
	"DarkGray",    "DarkGreen",  "DarkOrange",  "DarkPink",    "DarkPurple",
	"DarkRed",     "DarkYellow", "Gray",        "Green",       "LightBlue",
	"LightCyan",   "LightGray",  "LightGreen",  "LightOrange", "LightPink",
	"LightPurple", "LightRed",   "LightYellow", "Orange",      "Pink",
	"Purple",      "Red",        "White",       "Yellow"
);

my %alias;

sub check_color
{
	my $c = shift;
	$c = "" unless defined $c;
	die "$0: Bad color '$c' at line $.\n" unless grep(/^\Q$c\E$/, @colors);
}

print "% mkunderlays\n";

# This should probably go in the prolog
# print "/Colorized_Regions {\n";

while (<>) {
	# remove comments
	s/#.*$//;

	# strip leading & trailing whitespace
	s/(?:^\s+|\s+$)//g;

	# skip blank lines
	next if (/^$/); 

	# (?:\s+\([^)]*\))?
	#   ignore labels added by the -laj option of exons2underlays
	if (/^(\d+)\s+(\d+)(?:\s+\([^)]*\))?\s+(\w+)(?:\s+([+-]))?/) {
		# 0 10 Green +
		my ($x0,$x1,$c,$r) = ($1, $2, $3, $4);

		$r = "" unless defined($r);

		if ($r eq "+") {
			$r = "UnderlayRectTopHalf";
		} elsif ($r eq "-") {
			$r = "UnderlayRectBottomHalf";
		} else {
			$r = "UnderlayRect";
		}

		$c = ($alias{$c} || $c);
		check_color $c;

		print "$x0 $x1 $c $r\n";
	} elsif (/^(\w+)\s+(\w+)$/) {
		# Blue EST
		check_color $1;
		$alias{$2} = $1;
		print "%c $1 $2\n";
	} else {
		die "$0: Bad input line '$_' at line $.\n";
	}
}

# This should probably go in the tags
# print "Colorized_Regions\n";

