#!/usr/local/bin/perl -w
# $Id: lav-filter.pl,v 1.1 2003/02/10 20:36:51 schwartz Exp $
while (<>) {
    if (/^s/) {
	print "s {\n";
	# "foo.fa" 1 2 0 1
	$a = <>; chomp $a;
	$b = <>; chomp $b;
	$c = <>; chomp $c;
	die unless $a =~ /^\s*"[^"]*" (\d+) (\d+) (\d+) (\d+)\s*$/;
	printf "  \"seq1data\" %d %d %d %d\n", $1, $2, $3, $4;
	die unless $b =~ /^\s*"[^"]*" (\d+) (\d+) (\d+) (\d+)\s*$/;
	printf "  \"seq2data\" %d %d %d %d\n", $1, $2, $3, $4;
	die unless $c eq "}";
	print "}\n";
    } else {
	print;
    }
}
