#!/usr/local/bin/perl

# $Id: ca2ctg.pl,v 1.6 2004/08/11 22:42:23 aphillip Exp $
#
# This program creates an empty .ctg file that only contains
# the headers for the sequences and contigs
#

use IO::File;
use File::Basename;
use TIGR::Foundation;
use AMOS::AmosLib;
use strict;

my $MY_VERSION = " Version 1.0 (Build " . (qw/$Revision: 1.6 $/ )[1] . ")";

# Constants

my $MY_HELPTEXT = qq~
    ca2ctg -i file.asm -o file.ctg -f file.frg -s select.list [-p]
    
    you can specify multiple frg files on the command line
~;

my $base = new TIGR::Foundation;

if (! defined $base){
    print STDERR "Nasty error, hide!\n";
    exit(1);
}



$base->setHelpInfo($MY_HELPTEXT);
$base->setVersionInfo($MY_VERSION);

my $infile;
my $outfile;
my @frgfiles;
my $selfile;
my $promote; # promote unitigs?

my $err = $base->TIGR_GetOptions("i=s" => \$infile,
				 "o=s" => \$outfile,
				 "f=s" => \@frgfiles,
				 "s=s" => \$selfile,
				 "p" => \$promote);


if ($err == 0){
    $base->bail("Command line parsing failed.  See -h option");
}

if (! defined $infile){
    if ($#ARGV < 0){
	$base->bail("Must specify an input file name.  See -h option");
    } else {
	$infile = $ARGV[0];
    }
}

my %selected;

if (defined $selfile){
    open(SEL, $selfile) || $base-bail("Cannot open $selfile :$!\n");
    while (<SEL>){
	chomp;
	$selected{$_} = 1;
    }
    close(SEL);
}

my $record;

my %seqnames;
for (my $i = 0; $i <= $#frgfiles; $i++){
    my $frgfile = $frgfiles[$i];
    print STDERR "Doing $frgfile\n";
    open(FRG, $frgfile) || $base->bail("Cannot open $frgfile: $!\n");
    while ($record = getRecord(\*FRG)){
	my ($type, $fields, $recs) = parseRecord($record);
	if ($type eq "FRG"){
	    my $id = getCAId($$fields{acc});
	    my $nm = $$fields{src};
	    my @lines = split('\n', $nm);
	    $nm = join(' ', @lines);
	    if ($nm ne "" && $nm !~ /^\s*$/){
		$seqnames{$id} = $nm;
	    }
	}
    }
    close(FRG);
}
print STDERR "done\n";

if (defined $outfile){
    open(STDOUT, ">$outfile") ||
	$base->bail("Cannot open \"$outfile\": $!\n");
}

open(IN, $infile) ||
    $base->bail("Cannot open $infile: $!");
my $prefix = (split /\./,basename($infile))[0];

print STDERR "first pass through asm\n";
my %clear;
my %referenced;  # number of times each unitig is referenced

if (defined $promote) {
    while ($record = getRecord(\*IN)){
	my ($type, $fields, $recs) = parseRecord($record);
	if ($type eq "CCO"){
	    for (my $i = 0; $i <= $#$recs; $i++){
		my ($sid, $sfs, $srecs) = parseRecord($$recs[$i]);
		if ($sid eq "UPS") {
		    if ($$sfs{typ} eq "S"){
			$referenced{$$sfs{lid}}++;
		    }
		}
	    }
	}
    }
    seek(IN, 0, 0); # rewind the input
}

print STDERR "done\n";
my %readmap; # map of reads in each unitig

print STDERR "second pass through asm\n";
while ($record = getRecord(\*IN)){
    my ($type, $fields, $recs) = parseRecord($record);
    
    if ($type eq "CCO" || $type eq "UTG"){
	my $id = getCAId($$fields{"acc"});
	
	if ($type eq "CCO" && defined $selfile && ! exists $selected{$id}){
	    next; # contig wasn't selected
	}
	if ($type eq "UTG" && ! defined $promote && ! defined $selfile) { 
	    next; # don't do untigs unless selected
	}
	if ($type eq "UTG" && defined $promote && 
	    $referenced{$id} != 1 && ! exists $selected{$id}){
	    next;
	}
	if ($type eq "UTG" && ! defined $promote && ! exists $selected{$id}) {
	    next;
	}
	
	print STDERR ">$id\n";
	my @gaps = ();
	my $contiglen = $$fields{"len"};
	while ($$fields{"cns"} =~ /-/g){
	    push(@gaps, $-[0]);
	    print STDERR "found gap at ", $gaps[$#gaps], " $-[0]\n";
	    $contiglen--;
	}
	
	my $nreads = ($type eq "UTG") ? $$fields{"nfr"} : $$fields{"npc"};

	if ((defined $selfile && exists $selected{$id}) || $type eq "CCO"){
	    print "##$id $nreads $contiglen bases\n";
	}
	
	for (my $i = 0; $i <= $#$recs; $i++){
	    my ($sid, $sfs, $srecs) = parseRecord($$recs[$i]);
	    if ($sid eq "MPS"){
		my $fid = getCAId($$sfs{"mid"});
		my ($cll, $clr) = split(',', $clear{$fid});
		my $flen = $clr - $cll + 1;
		my ($asml, $asmr) = split(',', $$sfs{"pos"});
		if ($asml > $asmr) {
		    my $tmp = $cll;
		    $cll = $clr;
		    $clr = $tmp;
		    $tmp = $asml;
		    $asml = $asmr;
		    $asmr = $tmp;
		}
		
		my $g = 0;
		while ($g <= $#gaps && $gaps[$g] < $asml){
		    $g++;
		}
		print STDERR "$asml saw $g gaps\n";
		$asml -= $g;
		while ($g <= $#gaps && $gaps[$g] < $asmr){
		    $g++;
		}
		print STDERR "$asmr saw $g gaps\n";
		$asmr -= $g;

		$asmr--;

		if ($type eq "UTG" && defined $promote && ! exists $selected{$id}){
		    $readmap{$id} .= "$fid,$asml,$asmr ";
		    next;
		}

                my $off = $asml;
		if (exists $seqnames{$fid}){
		    $fid = $seqnames{$fid};
		}

		if ((defined $selfile && exists $selected{$id}) || $type eq "CCO"){
		    print "#$fid($off) $flen bases {$cll $clr} <$asml $asmr>\n";}
	    } # if MPS
	    if ($sid eq "UPS"){
		if ($$sfs{"typ"} ne "S" || ! defined $promote || $referenced{$$sfs{"lid"}} != 1){
		    next;
		}
		my @reads = split(' ', $readmap{$$sfs{"lid"}});
		my ($ul, $ur) = split(',', $$sfs{"pos"});
		my $sign = ($ul < $ur) ? 1 : -1;
		for (my $rr = 0; $rr <= $#reads; $rr++){
		    my ($rid, $rlef, $rrig) = split(',', $reads[$rr]);
		    my ($cll, $clr) = split(',', $clear{$rid});
		    my $flen = $clr - $cll;
		    if (exists $seqnames{$rid}){
			$rid = $seqnames{$rid};
		    }
		    my $asml = $ul + $sign * $rlef;
		    my $asmr = $ul + $sign * $rrig;
		    if ($asml > $asmr) {
			my $tmp = $cll;
			$cll = $clr;
			$clr = $tmp;
			$tmp = $asml;
			$asml = $asmr;
			$asmr = $tmp;
		    }
		    $asmr--;
		    my $off = $asml;
		    print "#$rid($off) $flen bases {$cll $clr} <$asml $asmr>\n";		}
	    } # if UPS
	} # for each record
	
	
    } # if $type = CCO
    
    
    if ($type eq "AFG")
    {
	my $id = getCAId($$fields{"acc"});
	my $clrs = $$fields{"clr"};
	$clear{$id} = $clrs;
    }
    
} # while $record
print STDERR "done\n";
