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
# merauder.pl by Jarrod Chapman <jchapman@lbl.gov> Tue Jun  2 07:48:59 PDT 2009
# Copyright 2009 Jarrod Chapman. All rights reserved.
#

use Getopt::Std;

my %opts = ();
my $validLine = getopts('g:b:s:m:D:i:S:A', \%opts);
my @required = ("g","b","s","m","i","S");
my $nRequired = 0;
map {$nRequired += exists($opts{$_})} @required;
$validLine &= ($nRequired == @required);
if ($validLine != 1) {
    print "Usage: ./merauder.pl <-b blastMapFile> <-g gapFile> <-m merSize> <-s scaffoldFasta> <-i insertSize:insertSigma> <-S seqFileGlob> <<-D minDepth>> <<-A(ggressive?)>>\n";
    exit;
}


my ($insertSize,$insertSigma) = $opts{"i"} =~ /^(.+):(.+)$/;
my $blastMapFile = $opts{"b"};
my $seqFileGlob = $opts{"S"};
my $gapFile = $opts{"g"};
my $merSize = $opts{"m"};
my $scaffFastaFile = $opts{"s"};
my $minDepth = 2;
if (exists($opts{"D"})) {
    $minDepth = $opts{"D"};
}
my $agrressiveClosure = 0;
if (exists($opts{"A"})) {
    $aggressiveClosure = 1;
}

print STDERR "Reading $scaffFastaFile...\n";

my %scaffoldSequence = ();
my $currentEntry = undef;
my @scaffNames = ();
my $seq = "";
open (F,$scaffFastaFile) || die "Couldn't open $scaffFastaFile\n";
while (my $line = <F>) {
    chomp $line;
    if ($line =~ /^>/) {
	if (defined($currentEntry)) {
	    $scaffoldSequence{$currentEntry} = $seq;
	}
	$seq = "";
	($currentEntry) = $line =~ />(.+)/;
	push(@scaffNames,$currentEntry);
    } else {
#	$seq .= uc($line);
	$seq .= $line;
    }
}
close F;

if (defined($currentEntry)) {
    $scaffoldSequence{$currentEntry} = $seq;
}

print STDERR "Done.\n";

my %gapInfo = ();
my @gapOrder = ();
my $nGaps = 0;
print STDERR "Reading $gapFile...\n";
open (G,$gapFile) || die "Couldn't open $gapFile\n";
while (my $line = <G>) {
    chomp $line;
    if (my ($scaffold,$contig1,$primer1,$gapSize,$contig2,$primer2) = $line =~ /^GAP:\s+(.+)\s+(.+)\s+([ACGT]{$merSize})\s+(\-?\d+)\s+(.+)\s+([ACGT]{$merSize})$/ ) {
	$gapInfo{$contig1} = [$nGaps,$scaffold,$contig1,$primer1,$gapSize,$contig2,$primer2];
	$gapInfo{$contig2} = [$nGaps,$scaffold,$contig1,$primer1,$gapSize,$contig2,$primer2];
	push(@gapOrder,$contig1);
	$nGaps++;
    }
}
close G;

print STDERR "Done. Found $nGaps (potentially) closable gaps.\n";

print STDERR "Reading $blastMapFile...\n";

my $nMappedReads = 0;

my %projectors = ();
my %fillers = ();

open (B,$blastMapFile) || die "Couldn't open $blastMapFile.\n";
while (my  $line = <B>) {
    chomp $line;
    my @cols = split(/\t/,$line);

    my ($pairStatus,$linkType) = ($cols[0],$cols[1]);

    if ($pairStatus eq "SINGLE") {
	my $contigEnd = $cols[2];
	my ($align) = $cols[3] =~ /^\[(.+)\]$/;
	my ($rStat,$r,$r0,$r1,$rL,$c,$c0,$c1,$cL,$strand) = split(/\s+/,$align);
	my ($readName,$readEnd) = $r =~ /^(.+)\/(\d)$/;

	if ( ($linkType eq "OUTGAP") && ($rStat =~ /OUT$/) ) {
	    my ($contig,$end) = $contigEnd =~ /^(.+)\.([35])$/;
	    $end = ($end == 3) ? 5 : 3;
	    $contigEnd = "$contig.end";
	}
	
	unless (exists($gapInfo{$contigEnd})) {
	    next;
	}

	if ($linkType eq "ANCHOR") {
	    $projectors{$readName} = "$readEnd:$contigEnd";
	} elsif ($linkType eq "INTGAP") {
	    $fillers{$r} = $contigEnd;
	} elsif ($linkType eq "OUTGAP") {
	    $fillers{$r} = "~$contigEnd";
	} elsif ($linkType eq "SPLINT") {
	    if ($rStat =~ /^GAP/) {
		$fillers{$r} = "~$contigEnd";
	    } elsif ($rStat =~ /^.+\.GAP/) {
		$fillers{$r} = "$contigEnd";
	    }
	}

    } elsif ($pairStatus eq "PAIR") {
	my ($lt1,$lt2) = $linkType =~ /^(.+)\.(.+)$/;

	my $contigEnd = $cols[2];
	my ($align) = $cols[3] =~ /^\[(.+)\]$/;
	my ($rStat,$r,$r0,$r1,$rL,$c,$c0,$c1,$cL,$strand) = split(/\s+/,$align);
	if ( ($lt1 eq "OUTGAP") && ($rStat =~ /OUT$/) ) {
	    my ($contig,$end) = $contigEnd =~ /^(.+)\.([35])$/;
	    $end = ($end == 3) ? 5 : 3;
	    $contigEnd = "$contig.end";
	}

	if (exists($gapInfo{$contigEnd})) {
	    if ($lt1 eq "INTGAP") {
		$fillers{$r} = $contigEnd;
	    } elsif ($lt1 eq "OUTGAP") {
		$fillers{$r} = "~$contigEnd";
	    } elsif ($lt1 eq "SPLINT") {
		if ($rStat =~ /^GAP/) {
		    $fillers{$r} = "~$contigEnd";
		} elsif ($rStat =~ /^.+\.GAP/) {
		    $fillers{$r} = "$contigEnd";
		}
	    }
	}

	$contigEnd = ($lt1 eq "SPLINT") ? $cols[6] : $cols[4];
	($align) = $cols[5] =~ /^\[(.+)\]$/;
	if ($lt1 eq "SPLINT") {
	    ($align) = $cols[7] =~ /^\[(.+)\]$/;
	}
	($rStat,$r,$r0,$r1,$rL,$c,$c0,$c1,$cL,$strand) = split(/\s+/,$align);
	if ( ($lt2 eq "OUTGAP") && ($rStat =~ /OUT$/) ) {
	    my ($contig,$end) = $contigEnd =~ /^(.+)\.([35])$/;
	    $end = ($end == 3) ? 5 : 3;
	    $contigEnd = "$contig.end";
	}

	if (exists($gapInfo{$contigEnd})) {
	    if ($lt2 eq "INTGAP") {
		$fillers{$r} = $contigEnd;
	    } elsif ($lt2 eq "OUTGAP") {
		$fillers{$r} = "~$contigEnd";
	    } elsif ($lt2 eq "SPLINT") {
		if ($rStat =~ /^GAP/) {
		    $fillers{$r} = "~$contigEnd";
		} elsif ($rStat =~ /^.+\.GAP/) {
		    $fillers{$r} = "$contigEnd";
		}
	    }
	}
    }
}
close B; 

my $nFillers = keys(%fillers);
my $nProjectors = keys(%projectors);

print STDERR "Done. Found $nFillers (potential) gap fillers; $nProjectors (potential) gap projectors.\n";


my %fillSeq = ();
my $readLength = undef;

$nFillers = 0;

my @seqFiles = glob($seqFileGlob);

foreach my $seqFile (@seqFiles) {

    print STDERR "Reading $seqFile...\n";
    open (S,$seqFile) || die "Couldn't open $seqFile\n";
    while (my $line = <S>) {
	chomp $line;
    
	my @cols = split(/\:/,$line);
	my $readName = "$cols[0]:$cols[1]:$cols[2]:$cols[3]:$cols[4]";
	my $nts = $cols[5];
	my $quals = $cols[6];
	unless(defined($readLength)) {
	    $readLength = length($nts);
	}

	my ($pairName,$pairEnd) = $readName =~ /(.+)\/(\d)/;

	if (exists($projectors{$pairName})) {
	    my $pInfo = $projectors{$pairName};
	    my ($placedEnd,$contig) = $pInfo =~ /^(\d):(.+)$/;
	    if ($pairEnd ne $placedEnd) {
	    
		$nFillers++;	    
		unless (exists($gapInfo{$contig})) {
		    print STDERR "fail: $contig\n";
		    die;
		}
	    
		my ($gapID,$scaffold,$contig1,$primer1,$gapSize,$contig2,$primer2) = @{$gapInfo{$contig}};
		if ($contig eq $contig1) {
		    $nts = reverse($nts);
#		    $nts =~ tr/ACGT/TGCA/;
		    $nts =~ tr/ACGTacgt/TGCAtgca/;
		    $quals = reverse($quals);
		}
	    
		if (exists($fillSeq{$gapID})) {
		    $fillSeq{$gapID} .= "[><]$nts:$quals";
		} else {
		    $fillSeq{$gapID} = "$nts:$quals";
		}
	    }

	} elsif (exists($fillers{$readName})) {

	    $nFillers++;
	    my ($reverse,$contig) = $fillers{$readName} =~ /^(\~?)(.+)$/;
	
	    unless (exists($gapInfo{$contig})) {
		print STDERR "fail: $contig\n";
		die;
	    }
	
	    my ($gapID,$scaffold,$contig1,$primer1,$gapSize,$contig2,$primer2) = @{$gapInfo{$contig}};
	    if ( ( ($contig eq $contig1) && ($reverse) ) || ( ($contig eq $contig2) && (!$reverse) ) ) {
		$nts = reverse($nts);
#		$nts =~ tr/ACGT/TGCA/;
		$nts =~ tr/ACGTacgt/TGCAtgca/;
		$quals = reverse($quals);
	    }
	
	    if (exists($fillSeq{$gapID})) {
		$fillSeq{$gapID} .= "[><]$nts:$quals";
	    } else {
		$fillSeq{$gapID} = "$nts:$quals";
	    }
	}
    }
    close S;
}
print STDERR "Done. Confirmed $nFillers (potential) gap-filling reads.\n";

my $nSuccess = 0;
my $nFailure = 0;
my %closures = ();
my $gapUncertainty = $insertSigma;

for (my $g=0;$g < $nGaps; $g++) {
    my $leadContig = $gapOrder[$g];
    my ($gapID,$scaffold,$contig1,$primer1,$gapSizeEstimate,$contig2,$primer2) = @{$gapInfo{$leadContig}};

    my $scaffSeq = $scaffoldSequence{$scaffold};
    my ($c1Seq) = $scaffSeq =~ /([ACGTacgt]+$primer1)/;
    my ($c2Seq) = $scaffSeq =~ /($primer2[ACGTacgt]+)/;

    if ($fillSeq{$gapID}) {

	my $fillerSequence = $fillSeq{$gapID};
	my @fillReads = split(/\[\>\<\]/,$fillerSequence);

	my $nFillReads = scalar(@fillReads);
	my $gapInfo = "[$nFillReads\t$primer1\t$gapSizeEstimate\t$primer2]";

	my $closure = span($primer1,$primer2,$fillerSequence);
	if ($closure) {
	    my $closedGapSize = length($closure) - 2*$merSize;
	    my $gapDiff = $closedGapSize - $gapSizeEstimate;
	    unless ($aggressiveClosure || (($gapDiff < $gapUncertainty) && ($gapDiff > -$gapUncertainty))) {
#		$closure = bridge($primer1,$primer2,$fillerSequence);
		$closure = bridgeIter($c1Seq,$c2Seq,$fillerSequence);
	    }
	} else {
#	    $closure = bridge($primer1,$primer2,$fillerSequence);
	    $closure = bridgeIter($c1Seq,$c2Seq,$fillerSequence);
	}

#	if ($closure =~ /$primer2/) {
	if ($closure =~ /$primer2$/) {

	    my $closedGapSize = length($closure) - 2*$merSize;
	    my $gapDiff = $closedGapSize - $gapSizeEstimate;
	    if ($aggressiveClosure || (($gapDiff < $gapUncertainty) && ($gapDiff > -$gapUncertainty))) {

		$nSuccess++;

		print STDERR "$gapInfo successfully closed : $closedGapSize $closure\n";

#		$closure = lc($closure);

#    Lower case as little of the primer sequences as possible:

		my $minGapMask = 2*5;
		if ($closure =~ /$primer1.{$minGapMask,}$primer2/) {
		    my ($p1,$g,$p2) = $closure =~ /($primer1)(.+)($primer2)/;
		    $g = lc($g);
		    $closure = $p1.$g.$p2;
		} else {
		    my $l = length($closure);
		    if ($l % 2 == 1) {
			$minGapMask++;
		    }
		    my $d = ($l-$minGapMask)/2;
		    my ($p1,$g,$p2) = $closure =~ /^(.{$d})(.{$minGapMask})(.{$d})/;
		    $g = lc($g);
		    $closure = $p1.$g.$p2;
		}

		if (exists($closures{$scaffold})) {
		    $closures{$scaffold} .= "[><]$primer1,$primer2,$closure";
		} else {
		    $closures{$scaffold} = "$primer1,$primer2,$closure";
		}

	    } else {
		
		print STDERR "$gapInfo failed to close due to improper closure length ($closedGapSize v. $gapSizeEstimate).\n";
		$nFailure++;

	    }
	} elsif ($closure =~ /X/) {
	    print STDERR "$gapInfo failed to close due to lack of coverage.\n";
	    $nFailure++;
	} elsif ($closure =~ /R/) {
	    print STDERR "$gapInfo failed to close due to repeat loop.\n";
	    $nFailure++;
	} elsif ($closure =~ /F/) {
	    my ($choices) = $closure =~ /F(.+)/; 
	    print STDERR "$gapInfo failed to close due to an unresolved repeat: $choices\n";
	    $nFailure++;
	} else {
	    print STDERR "$gapInfo failed to close ... and I don't know why...\n";
	    $nFailure++;	    
	}
    } else {
	my $gapInfo = "[0\t$primer1\t$gapSizeEstimate\t$primer2]";
	print STDERR "$gapInfo failed to close due to lack of projected closing reads.\n";
	$nFailure++;	    
    }
}

print STDERR "Done.  Successfully closed $nSuccess gaps. ($nFailure failed to close)\n";

foreach my $scaffold (@scaffNames) {
    my $scaffSeq = $scaffoldSequence{$scaffold};
    
    if (exists($closures{$scaffold})) {
	my @gapInfo = split(/\[\>\<\]/,$closures{$scaffold});
	foreach my $gap (@gapInfo) {
	    my ($primer1,$primer2,$closure) = $gap =~ /^(.+)\,(.+)\,(.+)$/;
	    $scaffSeq =~ s/$primer1.+$primer2/$closure/;
	}
    }

    printFasta($scaffold,$scaffSeq);
}

#
# SUBROUTINES
#

sub printFasta {
    my ($name,$seq) = @_;
    my $seqLen = length($seq);

    my $bpl = 50;
    my $nLines = sprintf("%d",$seqLen/$bpl);
    if ($seqLen%$bpl != 0) {
        $nLines++;
    }

    print ">$name\n";
    for (my $j = 0;$j<$nLines;$j++) {
        my $seqLine = substr($seq,$j*$bpl,$bpl);
        my $text = $seqLine;
        $text.= "\n";
        print $text;
    }
}

sub span {
    my ($primer1,$primer2,$sequences) = @_;
    
    my @reads = split(/\[\>\<\]/,$sequences);
    my $nReads = scalar(@reads);

    my %pureSpans = ();
    foreach my $read (@reads) {
	my ($nts,$quals) = $read =~ /^(.+)\:(.+)$/;
	if (($nts =~ /$primer1/) && ($nts =~ /$primer2/)) {
	    my ($tmpSpan) = $nts =~ /($primer1.+)$/;
	    my ($span) = $tmpSpan =~ /^(.+$primer2)/; 
	
	    if ($span) {
		$pureSpans{$span}++;
	    }
	}
    }
    my @spans = keys(%pureSpans);
    my $nSpans = scalar(@spans);
    if (($nSpans == 1) && ($pureSpans{$spans[0]} > 1)) {
	print STDERR "Unique spanning sequence found: $pureSpans{$spans[0]}/$nReads reads span the gap.\n";
	return $spans[0];
    } else {
	return 0;
    }
}


sub bridge {
    my ($primer1,$primer2,$sequences) = @_;
    
    my $minQ = 50;
  
    my @reads = split(/\[\>\<\]/,$sequences);
    my $nReads = scalar(@reads);

    my %mers = ();
    foreach my $read (@reads) {

	my ($nts,$quals) = $read =~ /^(.+)\:(.+)$/;

	my $merPlus = $merSize+1;
	while ($nts =~ /([ACGT]{$merPlus})/g) {
	    my $merExt = $1;

	    my ($mer,$extension) = $merExt =~ /(.+)(.)/;
	    my $q = substr($quals,pos($nts)-1,1);
	    my $Q = ord($q)-33;
	    
	    if ($Q > $minQ) {
		if (exists($mers{$mer})) {
		    $mers{$mer} .= $extension;
		} else {
		    $mers{$mer} = $extension;
		}
	    }
	    pos($nts) -= $merSize;
	}
    }

    while (my ($mer,$extensions) = each(%mers)) {
	my %votes = ();
	while ($extensions =~ /(.)/g) {
	    $votes{$1}++;
	}
	my @options = keys(%votes);
	my $nGoodOptions = 0;
	my $goodOptions = "";
	my $optionInfo = "";
	foreach my $choice (@options) {
	    if ($votes{$choice} >= $minDepth) {
		$optionInfo .= "$choice$votes{$choice}";
		$goodOptions .= $choice;
		$nGoodOptions++;
	    }
	}

	if ($nGoodOptions == 0) {
	    $mers{$mer} = "X";
	} elsif ($nGoodOptions == 1) {
	    $mers{$mer} = $goodOptions;
	} else {
	    $mers{$mer} = "F$optionInfo";
	}
    }

    my $walk = $primer1;
    my $step = $primer1;
    my %loopCheck = ();
    while () {
	if (exists($loopCheck{$step})) {
	    $walk .= "R";
	    last;
	} else {
	    $loopCheck{$step} = 1;
	}

        if (exists($mers{$step})) {
	    
	    my $next = $mers{$step};
	    if ($next =~ /^([ACGT])$/) {
		$step = substr($step,1);
		$step .= $next;
		$walk .= $next;
		if ($step eq $primer2) {
		    last;
		}
	    } else {
		$walk .= $next;
		last;
	    }
	} else {
	    $walk .= "X";
	    last;
	}
    }

    return ($walk);

}

# Iterative bridging allows increased k-mer size to resolve repeats
# and reduced depth stringency in uniquely extendable regions if aggressiveClosure is set

sub bridgeIter {
    my ($c1seq,$c2seq,$sequences) = @_;
    
    my $minQ = 19;
  
    my @reads = split(/\[\>\<\]/,$sequences);
    my $nReads = scalar(@reads);

    my $merLen = $merSize;

    my $iterating = 1;
    while ($iterating) {

	my ($primer1) = $c1seq =~ /(.{$merLen})$/;
	my ($primer2) = $c2seq =~ /^(.{$merLen})/;

	unless ($primer1 && $primer2) {
	    return "X";
	}

	my %mers = ();
	foreach my $read (@reads) {

	    my ($nts,$quals) = $read =~ /^(.+)\:(.+)$/;

	    my $merPlus = $merLen+1;
	    while ($nts =~ /([ACGT]{$merPlus})/g) {
		my $merExt = $1;

		my ($mer,$extension) = $merExt =~ /(.+)(.)/;
		my $q = substr($quals,pos($nts)-1,1);
		my $Q = ord($q)-64;
	    
		if ($Q > $minQ) {
		    if (exists($mers{$mer})) {
			$mers{$mer} .= $extension;
		    } else {
			$mers{$mer} = $extension;
		    }
		}
		pos($nts) -= $merLen;
	    }
	}

	while (my ($mer,$extensions) = each(%mers)) {
	    my %votes = ();
	    while ($extensions =~ /(.)/g) {
		$votes{$1}++;
	    }
	    my @options = keys(%votes);
	    my $nGoodOptions = 0;
	    my $goodOptions = "";
	    my $optionInfo = "";

	    foreach my $choice (@options) {
		if ($votes{$choice} >= $minDepth) {
		    $optionInfo .= "$choice$votes{$choice}";
		    $goodOptions .= $choice;
		    $nGoodOptions++;
		}
	    }

	    if ($nGoodOptions == 0) {
		if (($aggressiveClosure) && (scalar(@options) == 1)) {
		    $mers{$mer} = $options[0];
		} else {
		    $mers{$mer} = "X";
		}
	    } elsif ($nGoodOptions == 1) {
		$mers{$mer} = $goodOptions;
	    } else {
		$mers{$mer} = "F$optionInfo";
	    }
	}

	my $walk = $primer1;
	my $step = $primer1;
	my %loopCheck = ();
	my $success = 0;
	while () {
	    if (exists($loopCheck{$step})) {
		$walk .= "R";
		last;
	    } else {
		$loopCheck{$step} = 1;
	    }

	    if (exists($mers{$step})) {
	    
		my $next = $mers{$step};
		if ($next =~ /^([ACGT])$/) {
		    $step = substr($step,1);
		    $step .= $next;
		    $walk .= $next;
		    if ($step eq $primer2) {
			$success = 1;
			last;
		    }
		} else {
		    $walk .= $next;
		    last;
		}
	    } else {
		$walk .= "X";
		last;
	    }
	}

	if (($walk =~ /F/) || ($walk =~ /R/)) {
	    $merLen += 2;
	    if ($merLen >= $readLength) {
		return($walk);
	    }
	} else {
	    if ($success) {
		my $additionalBases = $merLen-$merSize;
		my $reducedWalk = $walk;
		if ($additionalBases) {
		    ($walk) = $reducedWalk =~ /^.{$additionalBases}(.+).{$additionalBases}$/;
		}
	    } 
	    return($walk);
	}
    }
}



	
