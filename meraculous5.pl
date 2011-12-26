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
#
# meraculous.pl by Jarrod Chapman <jchapman@lbl.gov> Wed Jun 13 11:42:09 PDT 2007
# Copyright 2007 Jarrod Chapman. All rights reserved.
#

use strict;
use Getopt::Std;
use KeylessHash;

my %opts = ();
my $validLine = getopts('s:m:c:D:M:U:d:P:', \%opts);
$validLine &= ( (exists($opts{"s"}) && exists($opts{"m"})) || exists($opts{"M"}) || exists($opts{"U"}));

my $usageString = "Usage: ./meraculous.pl [ <-U UFXfileGlob> || <-M merGraphFile> || (<-m merCountFile> <-s sequenceFileGlob> <<-P prefix>>) ] <<-d counts|UFX>> <<-c minContigSize>> <<-D significantDepth>>\n"; 

if ($validLine != 1) {
    print STDERR $usageString;
    exit;
}

my $merCountFile = undef; 
if (exists($opts{"m"})) {
    $merCountFile = $opts{"m"};
}
my $sequenceFileGlob = undef;
if (exists($opts{"s"})) {
    $sequenceFileGlob = $opts{"s"};
}
my $merGraphFile = undef;
if (exists($opts{"M"})) {
    $merGraphFile = $opts{"M"};
}

my $UFXFileGlob = undef;
if (exists($opts{"U"})) {
    $UFXFileGlob = $opts{"U"};
}

my $minContigSize = 500;
if (exists($opts{"c"})) {
    $minContigSize = $opts{"c"};
}

my $significantDepth = 2;
if (exists($opts{"D"})) {
    $significantDepth = $opts{"D"};
}

my $dumpMers = 0;
if (exists($opts{"d"})) {
    $dumpMers = $opts{"d"};
    unless (($dumpMers eq "counts") || ($dumpMers eq "UFX")) {
	print STDERR $usageString;
    }

}

my %baseCodes = ("A",0,"C",1,"G",2,"T",3,"N",4,"X",5);
my @bases = ("A","C","G","T");
my %mers = ();
my $merSize = undef;

# Input set of k-mers to use and read seq/quals
# Generate mergraph

if (defined($merCountFile) && defined($sequenceFileGlob)) {
    print STDERR "Reading $merCountFile...\n";
    my $nMers = 0;
    open (M,$merCountFile) || die "Couldn't open $merCountFile\n";
    while (my $line = <M>) {
	chomp $line;

	my $mer = undef;
	my $count = undef;

	if ($line =~ /^([acgtACGT]+)\s+(\d+)$/) {
	    ($mer,$count) = $line =~ /^([acgtACGT]+)\s+(\d+)$/;
	} elsif ($line =~ /^([acgtACGT]+)\s+(\d+)\s+(\d+)$/) {
	    my ($m,$count1,$count2) = $line =~ /^([acgtACGT]+)\s+(\d+)\s+(\d+)$/;
	    $mer = $m;
	    $count = $count1+$count2;
	} else {
	    die "Error:  line [$line] not a valid mercount format.\n";
	}

	$mer = uc($mer);
	
	if (exists($opts{"P"})) {
	    my $prefix = $opts{"P"};
	    unless ($mer =~ /^$prefix/) {
		next;
	    }
	}

	unless (defined($merSize)) {
	    $merSize = length($mer);
	}
	unless (exists($mers{$mer}) || ($count < $significantDepth)) {
	    $mers{$mer} = [0,0,0,0,0,0,0,0,0,0,0,0,0];
	    $nMers++;
	}
    }
    close M;
    print STDERR "Done. Found $nMers $merSize-mers with at least $significantDepth counts.\n";


    my $nReads = 0;
    my $totalReadLength = 0;
    my $nValidMers = 0;
    my $minQuality = 19;
    my $reportFreq = 100000;
    my @sequenceFiles = glob($sequenceFileGlob);
    my $matchString = "[ACGT]{$merSize}";
    if (exists($opts{"P"})) {
	my $prefix = $opts{"P"};
	my $matchLen = $merSize-length($prefix);
	if ($matchLen > 0) {
	    $matchString = $prefix . "[ACGT]{$matchLen}";
	} else {
	    die "Bad options: prefix ($prefix) must be less than mer-size ($merSize)\n";
	}
    }
	
    foreach my $sequenceFile (@sequenceFiles) {
	
	open (S,$sequenceFile) || die "Couldn't open $sequenceFile\n";

	print STDERR "Reading in $sequenceFile...\n";

	while (my $line = <S>) {
	    chomp $line;
	    my @cols = split(/\:/,$line);
	    my $readName = "$cols[0].$cols[1].$cols[2].$cols[3].$cols[4]";
	    my $nts = $cols[5];
	    my $quals = $cols[6]; 
	    
	    my $seqLen = length($nts);
	    $totalReadLength += $seqLen;
	    $nReads++;
	    if ($nReads%$reportFreq == 0) {
		print STDERR ".";
	    }

	    my $qSeq = "X".$quals."X";
	    my $seq = "X".$nts."X";
	    
	    while ($seq =~ /(.)($matchString)(.)/g) {
		
		my ($pre,$core,$post) = ($1,$2,$3);
		pos($seq) -= ($merSize+1);
		my $sCoord = pos($seq)-1;
		
		if (exists($mers{$core})) {
		    
		    $nValidMers++;
		    
		    my $preCode = $baseCodes{$pre};
		    my $postCode = 6+$baseCodes{$post};
		    
		    my $qpre = substr($qSeq,$sCoord,1);
		    my $qpost = substr($qSeq,$sCoord+$merSize+1,1);
		    
		    my $Qpre = ord($qpre) - 64;
		    my $Qpost = ord($qpost) - 64;
		    
		    if ($Qpre > $minQuality) {
			$mers{$core}->[$preCode] += 1;
		    } 
		    if ($Qpost > $minQuality) {
			$mers{$core}->[$postCode] += 1;
		    }
		}
	    }
	    
	    $seq = revComp($seq);
	    $qSeq = reverse($qSeq);
	    
	    while ($seq =~ /(.)($matchString)(.)/g) {
		
		my ($pre,$core,$post) = ($1,$2,$3);
		pos($seq) -= ($merSize+1);
		my $sCoord = pos($seq)-1;
		
		if (exists($mers{$core})) {
		    
		    my $preCode = $baseCodes{$pre};
		    my $postCode = 6+$baseCodes{$post};
		    
		    my $qpre = substr($qSeq,$sCoord,1);
		    my $qpost = substr($qSeq,$sCoord+$merSize+1,1);
		    
		    my $Qpre = ord($qpre) - 64;
		    my $Qpost = ord($qpost) - 64;
		    
		    if ($Qpre > $minQuality) {
			$mers{$core}->[$preCode] += 1;
		    }
		    if ($Qpost > $minQuality) {
			$mers{$core}->[$postCode] += 1;	    
		    }
		}
	    }
	}
	print STDERR "Done.\n";
	close S;
    }
    
    print STDERR "Found $nReads total reads spanning $totalReadLength total bases and including $nValidMers valid $merSize-mers.\n";

# Output mer-graph

    print STDERR "Dumping $merSize-mer count graph...\n";

    foreach my $mer (keys(%mers)) {
	my @counts = @{$mers{$mer}};
	my @ins = @counts[0..5];
	my $in = 0;
	map {$in += $_} @ins;
	my @outs = @counts[6..11];
	my $out = 0;
	map {$out += $_} @outs;
	if ($in || $out) {
	    print "$mer\t@counts\n";
	}
    }
    print STDERR "Done.\n";

}

# Input mergraph(s)
# Generate UFX

if (defined($merGraphFile)) {
    print STDERR "Reading merGraph from $merGraphFile...\n";

    my $distinctMers = 0;
    open (G,$merGraphFile) || die "Couldn't open $merGraphFile\n";
    while (my $line = <G>) {
	chomp $line;
	my ($mer,@counts) = split(/\s+/,$line);
	my $nCounts = scalar(@counts);
	unless ($nCounts == 13) {
	    die "Invalid merGraphFile line: $line\n";
	}
	$mer = uc($mer);
	unless (defined($merSize)) {
	    $merSize = length($mer);
	}

	if (exists($mers{$mer})) {
	    for (my $i = 0; $i < 13; $i++) {
		$mers{$mer}->[$i] += $counts[$i];
	    }
	} else {
	    $mers{$mer} = [ @counts ];
	    $distinctMers++;
	}
    }

    print STDERR "Dumping $merSize-mer UFX graph...\n";

    while (my ($mer,$value) = each(%mers)) {
	    my $merType = markMer($mer, $value);
	    print "$mer\t$merType\n";

    }
    close G;

    print STDERR "Done.\n";
    print STDERR "Found $distinctMers distinct $merSize-mers.\n";
}


# Input UFX-graph
# Output contigs

#Starting with meraculous5, this expects normalized, validated U-U files

my %valueMap = ();
my $termCode = "**";
$valueMap{"AA"} = 10; $valueMap{"AC"} = 11; $valueMap{"AG"} = 12; $valueMap{"AT"} = 13;
$valueMap{"CA"} = 14; $valueMap{"CC"} = 15; $valueMap{"CG"} = 16; $valueMap{"CT"} = 17;
$valueMap{"GA"} = 18; $valueMap{"GC"} = 19; $valueMap{"GG"} = 20; $valueMap{"GT"} = 21;
$valueMap{"TA"} = 22; $valueMap{"TC"} = 23; $valueMap{"TG"} = 24; $valueMap{"TT"} = 25;
$valueMap{$termCode} = 26; 

my %invValueMap = ();
while (my ($key,$val) = each(%valueMap)) {
    $invValueMap{$val} = $key;
}

if (defined($UFXFileGlob)) {

    my $minHashSize = 100000;

    my @UFXFiles = glob($UFXFileGlob);
    my %merTypes = ();
    my $nNodes = 0;
    my @primers = ();

    my $tmpFilePrefix = "merac5.toHash.";
    my $tmpFile = $tmpFilePrefix . "0";
    open (T,">$tmpFile") || die "Couldn't open $tmpFile\n";

    foreach my $UFXFile (@UFXFiles) {

	print STDERR "Reading UU-graph from $UFXFile...\n";

	my $localNodes = 0;
	open (U,$UFXFile) || die "Couldn't open $UFXFile\n";
	while (my $line = <U>) {
	    chomp $line;
	    my ($mer,$merType) = $line =~ /^([ACGT].+)\s+(..)$/;
	    unless (defined($merSize)) {
		$merSize = length($mer);
	    }

	    my $rcMer = reverse($mer);
	    $rcMer =~ tr/ACGT/TGCA/;

	    unless ($mer le $rcMer) {
		die "Error in $UFXFile: $line . Not a normalized mer.\n";
	    }
	    
	    $localNodes++;
	    $merTypes{$merType}++;

	    print T "$mer\n";

	}
	close U;
	$nNodes += $localNodes;
	print STDERR "Done. Found $localNodes nodes.\n";
    }
    close T;

    my @types = sort {$merTypes{$b} <=> $merTypes{$a}} keys(%merTypes);

    my $uuTypes = 0;
    my $termina = 0;
    foreach my $type (@types) {
	my $n = $merTypes{$type};
	if ($type =~ /[ACGT][ACGT]/) {
	    $uuTypes += $n;
	} else {
	    $termina += $n;
	}
	print STDERR "\t$type\t$n\n";
    }
    print STDERR "--------------------\nFound $uuTypes U-U mers, $termina terminators\n";

    print STDERR "Priming hash...\n";

    my $stillToHash = $nNodes;
    my $hashDepth = 0;

    while ($stillToHash) {

	my $readFile = $tmpFilePrefix . $hashDepth;
	my $writeFile = $tmpFilePrefix . ($hashDepth+1);

	my $hashSize = ($stillToHash > $minHashSize) ? $stillToHash : $minHashSize;
	my $primer = new KeylessHash::KeylessHashPrimer( $hashSize );

	$stillToHash = 0;

	open (R,$readFile) || die "Couldn't open $readFile\n";
	while (my $line = <R>) {
	    chomp $line;
	    $primer->prime($line,$hashDepth);
	}
	seek(R,0,0);
	open (W,">$writeFile") || die "Couldn't open $writeFile\n";
	while( my $line = <R> ) {
	    chomp $line;
	    if ($primer->isConflicted(KeylessHash::computeHashValue($line,$primer->getHashSize(),$hashDepth))) {
		print W "$line\n";
		$stillToHash++;
	    }
	}
	close R;
	unlink $readFile;
	close W;
	push (@primers, $primer);
	
	print STDERR "At hash depth $hashDepth, $stillToHash keys in collision.\n";

	if ($stillToHash) {
	    $hashDepth++;
	} else {
	    unlink $writeFile;
	}
    }

    print STDERR "Done.\nBuilding hash...\n";

    my $hash = new KeylessHash::KeylessHash();
    foreach my $primer( @primers ) {
	$hash->addPrimer( $primer );
    }
    $hash->finalize();

    # Populate hash
    print STDERR "Populating hash...\n";

    foreach my $UFXFile (@UFXFiles) {

	open (U,$UFXFile) || die "Couldn't open $UFXFile\n";
	while (my $line = <U>) {
	    chomp $line;
	    my ($mer,$merType) = $line =~ /^([ACGT].+)\s+(..)$/;
	    unless (exists($valueMap{$merType})) {
		die "Error: invalid extension code in $UFXFile ($line)\n";
	    }
	    putCode($mer,$merType,$hash);
	}
	close U;
    }
    print STDERR "Done.\n";

    # Traverse U-U k-mers generating contigs

    print STDERR "Generating contigs...\n";

    my $contigId = 0;
    my $totalShortContigBases = 0;
    my $nShortContigs = 0;
    my $totalLongContigBases = 0;
    my $nLongContigs = 0;

    foreach my $UFXFile (@UFXFiles) {

	open (U,$UFXFile) || die "Couldn't open $UFXFile\n";
	while (my $line = <U>) {
	    chomp $line;
	    my ($mer,$merType) = $line =~ /^([ACGT].+)\s+(..)$/;

	    my $merCode = getCode($mer,$hash);
	    
	    if ($merCode eq $termCode) {
		next;
	    } elsif ($merCode ne $merType) {
		die "Hash value ($merCode) inconsistent with value from file ($merType) for $mer.\n";
	    }

	    my $forwardWalk = walk($mer,$hash);
	    putCode($mer,$merCode,$hash);
	    my $reverseWalk = walk(revComp($mer),$hash);
	    putCode($mer,$termCode,$hash);
	    
	    $reverseWalk = revComp($reverseWalk);
	    my $result = $reverseWalk . $mer . $forwardWalk;
	    my $contigLength = length($result);

#	print STDERR "CONTIG $contigLength\n";

	    if ($contigLength > $minContigSize-1) {
		printFasta("Contig$contigId",$result);
		$contigId++;
		$totalLongContigBases += $contigLength;
		$nLongContigs++;
	    } else {
		$totalShortContigBases += $contigLength;
		$nShortContigs++;
	    }
	}
    }

    print STDERR "Done.\n";
    print STDERR "Generated $nLongContigs contigs >= $minContigSize bases totalling $totalLongContigBases bases\n";
    print STDERR "Discarded $nShortContigs contigs < $minContigSize bases totalling $totalShortContigBases bases\n";
}


# --- SUBROUTINES ---

sub markMer {
    my ($mer, $pCounts) = @_;

    my @counts = @{ $pCounts };

    my @fPaths = @counts[6..11];
    my @rPaths = @counts[0..5];

    my $fPaths = "";	
    my $rPaths = "";	
    for (my $p = 0; $p < 4; $p++) {
	if ($fPaths[$p] >= $significantDepth) {
	    $fPaths .= $bases[$p];
	}
	if ($rPaths[$p] >= $significantDepth) {
	    $rPaths .= $bases[$p];
	}
    }
    my $nfPaths = length($fPaths);
    my $nrPaths = length($rPaths);

    my $merType = "";
    if ($nrPaths == 1) {
	$merType .= $rPaths;
    } elsif ($nrPaths > 1) {
	$merType .= "F";
    } else {
	$merType .= "X";
    }
    if ($nfPaths == 1) {
	$merType .= $fPaths;
    } elsif ($nfPaths > 1) {
	$merType .= "F";
    } else {
	$merType .= "X";
    }

    return $merType;
}

sub getCode {
    my ($mer,$hash) = @_;
    
    my $rcMer = reverse($mer);
    $rcMer =~ tr/ACGT/TGCA/;
    
    my $val = ($mer le $rcMer) ? ord($hash->get($mer)) : ord($hash->get($rcMer));
    unless (exists($invValueMap{$val})) {
	warn "Error: invalid value returned by hash->get ($val) for mer ($mer)\n";
	return 0;
    }
    my $code = $invValueMap{$val}; 

    unless ($mer le $rcMer) {
	$code = reverse($code);
	$code =~ tr/ACGT/TGCA/;
    }

    return $code;
}

sub putCode {
    my ($mer,$code,$hash) = @_;
    
    my $rcMer = reverse($mer);
    $rcMer =~ tr/ACGT/TGCA/;

    if ($mer le $rcMer) {
	my $value = $valueMap{$code};
	$hash->put($mer,chr($value));
    } else {
	$code = reverse($code);
	$code =~ tr/ACGT/TGCA/;
	my $value = $valueMap{$code};
	$hash->put($rcMer,chr($value));
    }
}

sub walk {
    my ($seed,$hash) = @_;
    
    my $currentMer = $seed;
    my $walk = "";
    my $merCode = getCode($currentMer,$hash);

    unless($merCode) {
	die "Error in walk for seed $seed getCode failed for seed.\n";
    }

    putCode($currentMer,$termCode,$hash);
    
    while () {

	my ($prevBase,$nextBase) = $merCode =~ /(.)(.)/;
	my $nextMer = substr($currentMer,1) . $nextBase;
	$currentMer = $nextMer;

	$merCode = getCode($currentMer,$hash);

	unless($merCode) {
	    die "Error in walk for seed $seed getCode failed during walk ($walk)\n";
	}

	if ($merCode eq $termCode) {
	    last;
	}

	putCode($currentMer,$termCode,$hash);
	$walk .= $nextBase;
    }

    return $walk;

}

sub revComp {
    my ($seq) = @_;
    my $rc = reverse($seq);
    $rc =~ tr/ACGT/TGCA/;
    return $rc;
}

sub printFasta {
    my ($name,$seq) = @_;
    my $bpl = 50;
    my $nLines = sprintf("%d",length($seq)/$bpl);
    if ((length($seq) % $bpl) != 0) {
        $nLines++;
    }

    print ">$name\n";
    for (my $j = 0;$j<$nLines;$j++) {
        my $seqLine = substr($seq,$j*$bpl,$bpl);
        my $text = ($seqLine . "\n");
        print $text;
    }
}
