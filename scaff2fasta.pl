#!/usr/bin/perl

#$Id: scaff2fasta.pl,v 1.2 2007/10/19 21:22:49 jamesrwhite Exp $

use strict;
use Fcntl;
use AMOS::AmosFoundation;
use AMOS::AmosLib;
use IO::Handle;

my $FILEBUF;

my %seekpos;     # id -> file offset correspondence
my %reverse;
my $pos;

my %scaffolds;
my %scaffgaps;

my $fastafname;

my $version = " 1.01 (Build " . (qw/$Revision: 1.2 $/ )[1] . ")";
my $helptext = qq~
.USAGE.
  scaff2fasta [-gaps] [-linker <linker>] [-contig <ctg>] <prefix>.asm

.DESCRIPTION.
  Produces a .scaffolds.fasta file of the bases of contigs in the
  order and orientation found in a Celera scaffold.  <prefix>.asm 
  file is required as input.

.OPTIONS.
  if -gaps is provided outputs a number of Ns proportional to gap size (a minimum of 100 Ns)
  if -linker is provided uses the <linker> sequence as separator between contigs
  if neither -gaps nor -linker are given, the space between contigs is filled with 100 Ns
  if -contig is provided also generates a .contig-like file for the scaffold

.KEYWORDS.
  converters, scaffolds
    ~;

my $base = new AMOS::AmosFoundation;

if (! defined $base){
    print STDERR "the sky is falling, run away!\n";
    exit(1);
}

$base->setHelpText($helptext);
$base->setUsage("scaff2fasta [-gaps] [-linker <linker>] [-contig <ctg>] <prefix>.asm");
$base->setVersion($version);

my $dogaps;
my $linker;
my $contig;

my $err = $base->getOptions("gaps" => \$dogaps,
				 "linker=s" => \$linker,
				 "contig=s" => \$contig);
if ($err == 0){
    $base->bail("Command line parsing failed.  See -h option");
}

my $fh = new IO::Handle;

open(IN, "$ARGV[0]") ||
    $base->bail("Cannot open \"$ARGV[0]\": $!\n");

$fh->fdopen(fileno(IN), "r") || $base->bail("Cannot open IN: $!\n");

$ARGV[0] =~ /(\S+)\.asm/;
if (! defined $1){
    $base->bail("\"$ARGV[0]\" not recognized, must have extension .asm or .frg\n");
}

$fastafname = "$1.scaffolds.fasta";
my $record = "";

my %seqname;
my %clears;

if (defined $contig){
    open(CTG, ">$contig") ||
	$base->bail("Cannot open $contig: $!\n");

    open(FRG, "$1.frg") || 
	$base->bail("Cannot open $1.frg needed by -contig: $!\n");
    while ($record = getRecord(\*FRG)){
	my ($rec, $fields, $recs) = parseRecord($record);
	if ($rec eq "FRG"){
	    my @nm = split('\n', $$fields{src});
	    my $nm = join('', @nm);
	    if ($nm =~ /^\s*$/){
		$nm = $$fields{acc};
	    }
	    $seqname{$$fields{acc}} = $nm;
	    $clears{$$fields{acc}} = $$fields{clr};
	}
    }
}

open(FASTA, "> $fastafname") ||
    $base->bail("Cannot open \"$fastafname\": $!\n");

my %ctglen;
my %scafflen;
my %scaffread;
my %ctgread;

my $pos = tell IN;
while ($record = getRecord(\*IN)){
    my ($rec, $fields, $recs) = parseRecord($record);

    if ($rec eq "CCO"){ # contig record
	my $id = getCAId($$fields{acc});

	$seekpos{$id} = $pos;
        my $cns = $$fields{cns};
	my @cns = split('\n', $cns);
	my $cns = join('', @cns);
	$cns =~ s/-//g;
	$ctglen{$id} = length($cns);
	$ctgread{$id} = $$fields{npc};
    }

    if ($rec eq "SCF"){ #scaffold record
	my $id = getCAId($$fields{"acc"});

	for (my $i = 0; $i <= $#$recs; $i++){
	    my ($lrec, $lfield, $lrecs) = parseRecord($$recs[$i]);
	    
	    my $ct1 = $$lfield{"ct1"};
	    my $ct2 = $$lfield{"ct2"};
	    my $gap = int($$lfield{"mea"});
	    if (! defined $dogaps || $gap < 100) {$gap = 100;}
	    
	    if ($$lfield{"ori"} eq "A"){
		if (exists $reverse{$ct1} &&
		    $reverse{$ct1} != 1){
		    print STDERR "$ct1\'s orientation is inconsistent\n";
		}
		if (exists $reverse{$ct2} &&
		    $reverse{$ct2} != 1){
		    print STDERR "$ct2\'s orientation is inconsistent\n";
		}
		$reverse{$ct1} = 1;
		$reverse{$ct2} = 1;
	    } elsif ($$lfield{"ori"} eq "O"){
		if (exists $reverse{$ct1} &&
		    $reverse{$ct1} != 1){
		    print STDERR "$ct1\'s orientation is inconsistent\n";
		}
		$reverse{$ct1} = 1;
		if (exists $reverse{$ct2} &&
		    $reverse{$ct2} == 1){
		    print STDERR "$ct2\'s orientation is inconsistent\n";
		}
		$reverse{$ct2} = 0;
	    } elsif ($$lfield{"ori"} eq "I"){
		if (exists $reverse{$ct1} &&
		    $reverse{$ct1} == 1){
		    print STDERR "$ct1\'s orientation is inconsistent\n";
		}
		$reverse{$ct1} = 0;
		if (exists $reverse{$ct2} &&
		    $reverse{$ct2} != 1){
		    print STDERR "$ct2\'s orientation is inconsistent\n";
		}
		$reverse{$ct2} = 1;
	    } else {
		if (exists $reverse{$ct1} &&
		    $reverse{$ct1} == 1){
		    print STDERR "$ct1\'s orientation is inconsistent\n";
		}
		$reverse{$ct1} = 0;
		if (exists $reverse{$ct2} &&
		    $reverse{$ct2} == 1){
		    print STDERR "$ct2\'s orientation is inconsistent\n";
		}
		$reverse{$ct2} = 0;
	    }
	    print "Adding $ct1 to scaffold $id\n";
	    $scaffolds{$id} .= "$ct1 ";
	    $scafflen{$id} += $ctglen{$ct1};
	    $scaffread{$id} += $ctgread{$ct1};
	    $scaffgaps{$id} .= "$gap ";
#	    if (! defined $dogaps || $gap < 100) {$gap = 100;}
	    if ($ct1 != $ct2){ # if same contig don't need to add the gap
		$scafflen{$id} += $gap;
	    }
	    
	    if ($ct1 != $ct2 && $i == $#$recs){
		$scaffolds{$id} .= "$ct2";
		$scafflen{$id} += $ctglen{$ct2};
		$scaffread{$id} += $ctgread{$ct2};
		print "Adding $ct2 to scaffold $id\n";
	    }
	}
    }
    $pos = tell IN;
}

while (my ($scaff, $ctgs) = each (%scaffolds)){
    my @contigs = split(' ', $ctgs);
    my @gaps = split(' ', $scaffgaps{$scaff});

    print "handling $scaff: $ctgs\n";

    my $scaffconsensus = "";

    if (defined $contig){
	print CTG "##$scaff $scaffread{$scaff} $scafflen{$scaff}\n";
    }

    my $off = 0;
    for (my $i = 0; $i <= $#contigs; $i++){
	seek IN, $seekpos{$contigs[$i]}, 0; # seek set
	my $record = getRecord(\*IN);
	if (! defined $record){
	    print "wierd error\n";
	    return;
	}
	
	my ($rec, $fields, $recs) = parseRecord($record);
	
	if ($rec ne "CCO"){
	    print STDERR "wierd error in get_seq, expecting frg\n";
	    return;
	}

	my $id = getCAId($$fields{"acc"});
	my $consensus = $$fields{"cns"};
	my @fields = split('\n', $consensus);
	$consensus = join('', @fields);
	my @offsets = ();

	if (defined $contig){
	    my $coord = 0;
	    for (my $j = 0; $j < length($consensus); $j++){
		if (substr($consensus, $j, 1) ne "-"){
		    $coord++;
		}
		$offsets[$j] = $coord;
	    }
	}
	$consensus =~ s/-//g;

	my $len = length($consensus);

	if (defined $contig){
	    for (my $j = 0; $j <= $#$recs; $j++){
		my ($sid, $sfs, $srecs) = parseRecord($$recs[$j]);
		if ($sid eq "MPS"){
		    my $o;
		    my ($l, $r) = split(',', $$sfs{pos});
		    if ($l > $r) {$l--; $o = $r;} else {$r--; $o = $l;};
		    $l = $offsets[$l];
		    $r = $offsets[$r];
		    if ($reverse{$id}){
			my $tmp = $l;
			$l = $len - $r + 1;
			$r = $len - $tmp + 1;
		    }
		    my ($cll, $clr) = split(',', $clears{$$sfs{mid}});
		    if ($l > $r){
			my $tmp = $cll;
			$cll = $clr;
			$clr = $tmp;
			$tmp = $l;
			$l = $r;
			$r = $tmp;
		    }
		    
		    $l += $off;
		    $r += $off;
		    $o += $off;

		    print CTG "#$seqname{$$sfs{mid}}($o) {$cll $clr} <$l $r>\n";
		}
	    }
	} # if contig

	$off += $len;
		 
	if ($reverse{$id}){
	    $consensus = reverseComplement($consensus);
	}
	$scaffconsensus .= $consensus;

	if ($i != $#contigs){ # add Ns
	    if (defined $linker){
		$scaffconsensus .= $linker;
		$off += length($linker);
	    } else {
		$off += $gaps[$i];
		for (my $pad = 0; $pad < $gaps[$i]; $pad++){
		    $scaffconsensus .= "N";
		}
	    }
	}
    }
    printFastaSequence(\*FASTA, sprintf("%s %d bp %d contigs", $scaff, length($scaffconsensus), $#contigs + 1), $scaffconsensus);
} # for scaff

close(FASTA);
close(IN);
if (defined $contig){
    close(CTG);
}

# The end
exit(0);


