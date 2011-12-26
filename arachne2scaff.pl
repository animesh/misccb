#!/usr/local/bin/perl

use TIGR::Foundation;

my $base = new TIGR::Foundation;

if (! defined $base){
    die ("Walk, do not run to the nearest EXIT!\n");
}

my $VERSION = '1.0 $Revision: 1.1 $ ';
$base->setVersionInfo($VERSION);
my $HELPTEXT = q~
    arachne2scaff -o <prefix> assembly.links
    ~;

$base->setHelpInfo($HELPTEXT);

my $outname;
my $err = $base->TIGR_GetOptions("o=s" => \$outname);

if ($err == 0){
    $base->bail("Command line parsing failed\n");
}

if (! defined $outname) {
    $base->bail("You must specify an output file name\n");
}

if ($#ARGV != 0){
    $base->bail("You must specify exactly one input file\n");
}

open(IN, $ARGV[0]) ||
    $base->bail("Cannot open $ARGV[0]: $!\n");

my $oofile = "$outname.oo";
my $sumfile = "$outname.sum";
my $gapfile = "$outname.gaps";

open(OO, ">$oofile") ||
    $base->bail("Cannot open $oofile: $!\n");
open(SUM, ">$sumfile") ||
    $base->bail("Cannot open $sumfile: $!\n");
open(GAP, ">$gapfile") ||
    $base->bail("Cannot open $gapfile: $!\n");

my $last;
my $lastctg;
my $lastspan;
my $lastnctg;
my $size = 0;
while (<IN>){
    chomp;

    if (/^\#/) {
	next; # skip comments
    }

    if (/^\s*$/){
	next; # skip empty lines
    }

    my @fields  = split('\t', $_);
    my $id      = $fields[0];
    my $bases   = $fields[1];
    my $nctg    = $fields[2];
    my $ctg     = $fields[4];
    my $ctglen  = $fields[5];
    my $gap     = $fields[6];

    if (! defined $last || $id != $last){
	print OO ">$id\n";
	if (defined $last) {
	    print SUM "$last $lastnctg $size $lastspan\n";
	}
	print GAP ">$id\n";
	$lastctg = undef;
	$size = 0;
    }
    $last = $id;
    if (defined $lastctg){
	print GAP "$lastctg $ctg $gap\n";
    }
    $size += $ctglen;
    $lastctg = $ctg;
    $lastnctg = $nctg;
    $lastspan = $bases;
    print OO "$ctg BE\n";
}
print SUM "$last $lastnctg $size $lastspan\n";

close(OO);
close(SUM);
close(GAP);
close(IN);
exit(0);

