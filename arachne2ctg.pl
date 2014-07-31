#!/usr/local/bin/perl

use TIGR::Foundation;

my $base = new TIGR::Foundation;

if (! defined $base){
    die ("Walk, do not run to the nearest EXIT!\n");
}

my $VERSION = '1.0 $Revision: 1.1 $ ';
$base->setVersionInfo($VERSION);
my $HELPTEXT = q~
    arachne2ctg -o <file>.ctg assembly.reads
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

open(OUT, ">$outname") ||
    $base->bail("Cannot open $outname: $!\n");

my $last;

while (<IN>){
    chomp;

    if (/^\#/) {
	next; # skip comments
    }

    if (/^\s*$/){
	next; # skip empty lines
    }

    my @fields  = split('\t', $_);
    my $read    = $fields[0];
    my $sta     = $fields[1];
    my $seqlen  = $fields[4];
    my $seql    = $fields[3];
    my $seqr    = $seql + $seqlen;
    my $asmblid = $fields[5];
    my $asmlen  = $fields[6];
    my $asml    = $fields[7];
    my $asmr    = $fields[8];
    my $rc      = $fields[9];

    my $tmp;

    if ($rc eq "-") {
	$rc = "RC";
	$tmp = $seql;  # also invert $seql and $seqr
	$seql = $seqr;
	$seqr = $tmp;
    } else {
	$rc = "";
    }

    if (! defined $last || $asmblid != $last){
	print OUT "##$asmblid 0 $asmlen bases, 00000000 checksum.\n";
    }
    $last = $asmblid;

    print OUT "#$read($asml) [$rc] $seqlen bases, 00000000 checksum. {$seql $seqr} <$asml $asmr>\n";
}

close(OUT);
close(IN);
exit(0);

