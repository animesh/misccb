#!/usr/bin/perl

# This program reads a Celera .asm file and produces aggregate information
# about the assembly

use strict;
use Statistics::Descriptive;
use AMOS::AmosFoundation;
use AMOS::AmosLib;

my $MY_VERSION = '$Revision $';

# Constants
my $MINQUAL = 20;
my $MINCONTIG = 10000;

my $MY_HELPTEXT = qq~
.USAGE.
  castats  <prefix>.asm  [options]

.DESCRIPTION.
  Generate quality statistics from the specified Celera assembly .asm file.

.OPTIONS.
  prefix.asm   The Celera .asm file
  -minqual   <n>   Minimum quality value threshhold to report as bad
                   quality (default $MINQUAL)
  -mincontig <n>   Minimum contig size to report as a big contig
                   (default $MINCONTIG)
  -g <n>           Genome size used in the calculation of N50 numbers
                   (default: TotalBasesInContigs)

.KEYWORDS.
  converter, Celera, statistics
~;

my $base = new AMOS::AmosFoundation;

if (! defined $base){
    print STDERR "Nasty error, hide!\n";
    exit(1);
}

$base->setHelpText($MY_HELPTEXT);
$base->setUsage("castats  <prefix>.asm  [options]");
$base->setVersion($MY_VERSION);

my $infile;
my $genomesize;

my $err = $base->getOptions("i=s"         => \$infile,
		 	    "minqual=i"   => \$MINQUAL,
				 "mincontig=i" => \$MINCONTIG,
				 "g=i"         => \$genomesize
				 );
    
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

open(IN, $infile) or $base->bail("Cannot open $infile: ($!)");

my $record;

my %lens;                # contig lengths wihtout gaps
my %sizes;               # contig sizes with gaps
my %bads;                # contig bad qualities
my %seqs;                # contig number of reads
my %scaflen;             # sum of contig sizes in scaffold
my %scafcontig;          # # contigs in scaffold
my %inscaff;             # contigs in scaffolds
my %adjscaflen;          # length of scaffold including inner-scaffold gaps
my %readlens;            # sum of lengths of reads in contig
my %libs_initial;        # the libraries for a project before assembly
my %libs_final;          # the libraries for a project after assembly

my $totalReadLength = 0; # Aggregate length of CLR of input reads used in assembly
my $totalSeqs       = 0; # Total number of reads used in assembly

my $ncontigs = 0;
my $nunitigs = 0;
my $utgSeqs = 0;
my $surrReadLen = 0;     # reads in surrogates
my $contigReadLen = 0;   # reads in contigs
my $degenReadLen = 0;    # reads in degenerates
my %utglens = ();
my $utgs = Statistics::Descriptive::Sparse->new();
my %Results = ();
my %readLen;
my $scaffLinkWeights = 0; # sum of links connecting scaffolds

while ($record = getRecord(\*IN)){
    my ($type, $fields, $recs) = parseRecord($record);
    
    if ($type eq "CCO"){
	$ncontigs++;
	my $contiglen = $$fields{"len"};
	my $nreads = $$fields{"npc"};
	my $qual = $$fields{"qlt"};
	my $id = getCAId($$fields{"acc"});
	
	my @quals = split('\n', $qual);
	$qual = join("", @quals);
	
	my $badquals = 0;
	for (my $i = 0; $i < length($qual); $i++){
	    my $thisqual = substr($qual, $i, 1);
	    
	    my $qualval = ord($thisqual) - ord('0');
	    
	    if ($qualval < $MINQUAL){
		$badquals++;
	    }
	} # for $i < length($qual)
	$sizes{$id} = $contiglen;
	$lens{$id} = $contiglen;
	my $ngaps = 0;
	while ($$fields{"cns"} =~ /-/g){
	    $ngaps++;
	}
	$lens{$id} -= $ngaps;
	$bads{$id} = $badquals;
	$seqs{$id} = $nreads;
	for (my $rd = 0 ;$rd <= $#$recs; $rd++){
	    my ($sid, $sfs, $srecs) = parseRecord($$recs[$rd]);
	    if ($sid eq "MPS"){
		my ($l, $r) = $$sfs{pos};
		$readlens{$id} += $readLen{$$sfs{"mid"}};#abs($r - $l);
	    }
	}
	
    } # if $type = CCO
    
    if ($type eq "UTG"){
	if ($$fields{"sta"} ne "U" &&
	    $$fields{"sta"} ne "N"){
	    $nunitigs++;
	    $utgs->add_data($$fields{"len"});
	    $utgSeqs += $$fields{"nfr"};
	    $utglens{getCAId($$fields{"acc"})} = $$fields{"len"};
	    for (my $rd = 0 ;$rd <= $#$recs; $rd++){
		my ($sid, $sfs, $srecs) = parseRecord($$recs[$rd]);
		if ($sid eq "MPS"){
		    my ($l, $r) = $$sfs{pos};
		    $surrReadLen += $readLen{$$sfs{"mid"}};#abs($r - $l);
		}
	    }
	}
    }
    
    if ($type eq "SCF"){
	my $id = getCAId($$fields{"acc"});
	my %scaffcontigs = ();
	my $adj = 0;
	for (my $i = 0; $i <= $#$recs; $i++){
	    my ($lrec, $lfield, $lrecs) = parseRecord($$recs[$i]);
	    
	    $scaffcontigs{$$lfield{"ct1"}}++;
	    $scaffcontigs{$$lfield{"ct2"}}++;
	    $inscaff{$$lfield{"ct1"}} = 1;
	    $inscaff{$$lfield{"ct2"}} = 1;
	    $adj += $$lfield{"mea"};
	}
	
	$scafcontig{$id} = 0;
	$scaflen{$id} = 0;
	while (my ($ct, $freq) = each %scaffcontigs){
	    $scafcontig{$id}++;
	    $scaflen{$id} += $lens{$ct};
	}
	
	$adjscaflen{$id} = $scaflen{$id} + int($adj);
    } # if $type = SCF
    
    if ($type eq "SLK") { # scaffold link
	$Results{TotalScaffoldLinks}++;
	$scaffLinkWeights += $$fields{num};
    }
    
    if ($type eq "AFG")
    {
	my $id = getCAId($$fields{"acc"});
	my $clrs = getCAId($$fields{"clr"});
	my ($clrl,$clrr) = split /,/,$clrs;
	my $length = $clrr - $clrl;
	$totalReadLength += $length;
	$readLen{$id} = $length;
	$totalSeqs++;
	if ($$fields{"mst"} eq "N"){
	    $Results{ReadsWithNoMate}++;
	} elsif ($$fields{"mst"} eq "B"){
	    $Results{ReadsWithBadMate}++;
	} elsif ($$fields{"mst"} eq "G"){
	    $Results{ReadsWithGoodMate}++;
	} elsif ($$fields{"mst"} eq "U"){
	    $Results{ReadsWithUnusedMate}++;
	}
    }
} # while $record


# Initialize the Results hash so that we never get blanks for values

$Results{TotalScaffolds}                 = 0;
$Results{TotalContigsInScaffolds}        = 0;
$Results{MeanContigsPerScaffold}         = 0.0;
#$Results{MinContigsPerScaffold}          = 0;
$Results{MaxContigsPerScaffold}          = 0;
$Results{TotalBasesInScaffolds}          = 0;
$Results{MeanBasesInScaffolds}           = 0.0;
#$Results{MinBasesInScaffolds}            = 0;
$Results{MaxBasesInScaffolds}            = 0;
$Results{N50ScaffoldBases}               = 0;
$Results{TotalSpanOfScaffolds}           = 0;
$Results{MeanSpanOfScaffolds}            = 0.0;
#$Results{MinScaffoldSpan}                = 0;
$Results{MaxScaffoldSpan}                = 0;
$Results{N50ScaffoldSpan}                = 0;
$Results{IntraScaffoldGaps}              = 0;
$Results{MeanSequenceGapSize}            = 0.0;
$Results{TotalContigsInScaffolds}        = 0;
$Results{TotalBasesInScaffolds}          = 0;
$Results{MeanContigSize}                 = 0.0;
#$Results{MinContigSize}                  = 0;
$Results{MaxContigSize}                  = 0;
$Results{N50ContigBases}                 = 0;
$Results{TotalBigContigs}                = 0;
$Results{BigContigLength}                = 0;
$Results{MeanBigContigSize}              = 0.0;
#$Results{MinBigContig}                   = 0;
$Results{MaxBigContig}                   = 0;
$Results{BigContigsPercentBases}         = 0.0;
$Results{TotalSmallContigs}              = 0;
$Results{SmallContigLength}              = 0;
$Results{MeanSmallContigSize}            = 0.0;
#$Results{MinSmallContig}                 = 0;
$Results{MaxSmallContig}                 = 0;
$Results{SmallContigsPercentBases}       = 0.0;
$Results{TotalDegenContigs}              = 0;
$Results{DegenContigLength}              = 0;
$Results{MeanDegenContigSize}            = 0.0;
#$Results{MinDegenContig}                 = 0;
$Results{MaxDegenContig}                 = 0;
$Results{DegenPercentBases}              = 0.0;
$Results{TotalReads}                     = 0;
$Results{ReadsInContigs}                 = 0;
$Results{BigContigReads}                 = 0;
$Results{SmallContigReads}               = 0;
$Results{DegenContigReads}               = 0;
$Results{ReadsInSurrogates}              = 0;
$Results{SingletonReads}                 = 0;
$Results{AllReads}                       = 0.0;
$Results{ContigsOnly}                    = 0.0;
$Results{ContigsAndDegens}               = 0.0;
$Results{NumSurrogates}                  = $utgs->count();
$Results{SurrogateSize}                  = $utgs->sum();
$Results{MeanSurrogateSize}              = $utgs->mean();
$Results{MinSurrogateSize}               = $utgs->min();
$Results{MaxSurrogateSize}               = $utgs->max();
$Results{SDSurrogateSize}                = $utgs->standard_deviation();
$Results{RangeSurrogateSize}             = $utgs->sample_range();
if (! exists $Results{ReadsWithNoMate}){
    $Results{ReadsWithNoMate} = 0;
}
if (! exists $Results{ReadsWithBadMate}){
    $Results{ReadsWithBadMate} = 0;
}
if (! exists $Results{ReadsWithGoodMate}){
    $Results{ReadsWithGoodMate} = 0;
}
if (! exists $Results{ReadsWithUnusedMate}){
    $Results{ReadsWithUnusedMate} = 0;
}
if (! exists $Results{TotalScaffoldLinks}){
    $Results{TotalScaffoldLinks} = 0;
    $Results{MeanScaffoldLinkWeight} = 0;
} else {
    $Results{MeanScaffoldLinkWeight} = 
	$scaffLinkWeights / $Results{TotalScaffoldLinks};
}

my $totlen = 0;
my $totbads = 0;
my $totseqs = 0;

my $biglen = 0;
my $bigbads = 0;
my $bigseqs = 0;
my $nbigs = 0;

my $maxcontig = 0;

for (my $i = 0; $i < $ncontigs; $i++){
    my ($id, $len) = each(%lens);
    
    my $number;
    my $min;
    my $max;
    my $totlen;
    my $nseq;
    
    if (exists $inscaff{$id}){ # the good guys
	if ($len >= $MINCONTIG) { # the good big guys
	    $number = "TotalBigContigs";
	    $min = "MinBigContig";
	    $max = "MaxBigContig";
	    $totlen = "BigContigLength";
	    $nseq = "BigContigReads";
	} else { # the small good guys
	    $number = "TotalSmallContigs";
	    $min = "MinSmallContig";
	    $max = "MaxSmallContig";
	    $totlen = "SmallContigLength";
	    $nseq = "SmallContigReads";
	}
	$contigReadLen += $readlens{$id};
    } else { # the chaff
	$number = "TotalDegenContigs";
	$min = "MinDegenContig";
	$max = "MaxDegenContig";
	$totlen = "DegenContigLength";
	$nseq = "DegenContigReads";
	$degenReadLen += $readlens{$id};
    }
    
    $Results{$max} = max($len, $Results{$max});
    $Results{$min} = (exists $Results{$min}) ? min($len, $Results{$min}) : $len;
    $Results{$number}++;
    $Results{$totlen} += $len;
    $Results{$nseq} += $seqs{$id};
} # for $i < $ncontigs

$Results{'TotalContigs'} = 0 +
    $Results{'TotalBigContigs'} + $Results{'TotalSmallContigs'};
$Results{'TotalBasesInContigs'} = 0 +
    $Results{'BigContigLength'} + $Results{'SmallContigLength'};
$Results{'MeanContigSize'} = ($Results{'TotalContigs'} > 0)? 
    $Results{'TotalBasesInContigs'} * 1.0 
    / $Results{'TotalContigs'} :
    0;

$Results{'ReadsInContigs'} = 0 + 
    $Results{'BigContigReads'} + $Results{'SmallContigReads'};
$Results{'MinContigSize'} = ($Results{MinSmallContig} > 0) ? $Results{'MinSmallContig'} : $Results{'MinBigContig'};
$Results{'MaxContigSize'} = max($Results{'MaxBigContig'}, $Results{'MaxSmallContig'});

$Results{'MeanBigContigSize'} = ($Results{'TotalBigContigs'} > 0)?
    $Results{'BigContigLength'} * 1.0 
    / $Results{'TotalBigContigs'} :
    0;

$Results{'MeanSmallContigSize'} = ($Results{'TotalSmallContigs'})? 
    $Results{'SmallContigLength'} * 1.0 
    / $Results{'TotalSmallContigs'} :
    0;

$Results{'BigContigsPercentBases'} = ($Results{'TotalBasesInContigs'} > 0)? 
    $Results{'BigContigLength'} * 100.0 
    / $Results{'TotalBasesInContigs'} :
    0;

$Results{'SmallContigsPercentBases'} = ($Results{'TotalBasesInContigs'} > 0)?
    $Results{'SmallContigLength'} * 100.0 
    / $Results{'TotalBasesInContigs'} :
    0;

$Results{'MeanDegenContigSize'} = ($Results{'TotalDegenContigs'})?
    $Results{'DegenContigLength'} * 1.0 
    / $Results{'TotalDegenContigs'} :
    0;

$Results{'DegenPercentBases'} = ($Results{'TotalBasesInContigs'} > 0)?
    $Results{'DegenContigLength'} * 100.0 
    / $Results{'TotalBasesInContigs'} :
    0;

# compute N50 values and top 5 guys
my @sortContig = sort {$lens{$b} <=> $lens{$a}} (keys %inscaff);
my $top5contig;
my $sum = 0;
my $reads_tot = 0;
my $bases_tot = 0; 
for (my $cc = 0; $cc <= $#sortContig; $cc++){
    if ($cc < 5){
	$top5contig .= 
	    "$cc $seqs{$sortContig[$cc]} $lens{$sortContig[$cc]}\n";
	$reads_tot += $seqs{$sortContig[$cc]};
	$bases_tot += $lens{$sortContig[$cc]};
    }
}

for (my $cc = 0; $cc <= $#sortContig; $cc++){
    $sum += $lens{$sortContig[$cc]};
    my $gsz;
    if (! defined $genomesize){
	$gsz = $Results{'TotalBasesInContigs'};
	$Results{"EstimatedGenomeSize"} = $gsz;
    } else {
	$gsz = $genomesize;
	$Results{"FromCommandLine"} = 1;
    }
    $Results{"EstimatedGenomeSize"} = $gsz;
    if ($sum > $gsz / 2){
	$Results{'N50ContigBases'} = $lens{$sortContig[$cc]};
	last;
    }
}

# do scaffold stats
while (my ($id, $ctgs) = each %scafcontig)
{
    $Results{'TotalScaffolds'}++;
    $Results{'TotalContigsInScaffolds'} += $scafcontig{$id};
    $Results{'IntraScaffoldGaps'} += $scafcontig{$id} - 1;
    
    $Results{'MinContigsPerScaffold'} = 
	(exists $Results{MinContigsPerScaffold}) ? min($Results{'MinContigsPerScaffold'}, $scafcontig{$id}): $scafcontig{$id};
    $Results{'MaxContigsPerScaffold'} = 
	max($Results{'MaxContigsPerScaffold'}, $scafcontig{$id});
    $Results{'TotalSpanOfScaffolds'} += $adjscaflen{$id}; 
    
    $Results{'MinScaffoldSpan'} = (exists $Results{MinScaffoldSpan})?
	min($Results{'MinScaffoldSpan'}, $adjscaflen{$id}): $adjscaflen{$id}; 
    $Results{'MaxScaffoldSpan'} = 
	max($Results{'MaxScaffoldSpan'}, $adjscaflen{$id});
    
    $Results{'TotalBasesInScaffolds'} += $scaflen{$id};
    
    $Results{'MinBasesInScaffolds'} = 
	(exists $Results{MinBasesInScaffolds}) ? min($Results{'MinBasesInScaffolds'}, $scaflen{$id}) : $scaflen{$id};
    $Results{'MaxBasesInScaffolds'} =
	max($Results{'MaxBasesInScaffolds'}, $scaflen{$id});
} # while each %scafcontigs

$Results{'MeanContigsPerScaffold'} = 
    ($Results{'TotalScaffolds'} > 0)
    ? $Results{'TotalContigsInScaffolds'} * 1.0 
    / $Results{'TotalScaffolds'}
: 0.0;
$Results{'MeanBasesInScaffolds'} = 
    ($Results{'TotalScaffolds'} > 0)
    ? $Results{'TotalBasesInScaffolds'} * 1.0 
    / $Results{'TotalScaffolds'}
: 0.0;
$Results{'MeanSpanOfScaffolds'} = 
    ($Results{'TotalScaffolds'} > 0)
    ? $Results{'TotalSpanOfScaffolds'} * 1.0 
    / $Results{'TotalScaffolds'}      
: 0.0;
$Results{'MeanSequenceGapSize'} = 
    ($Results{'IntraScaffoldGaps'} > 0)
    ? ($Results{'TotalSpanOfScaffolds'} 
       - $Results{'TotalBasesInScaffolds'}) * 1.0 
    / $Results{'IntraScaffoldGaps'}
: 0.0;

# compute N50 values and top 5 guys
my @sortScaff = sort {$scaflen{$b} <=> $scaflen{$a}} (keys %scaflen);
my $top5scaff;
$sum = 0;

my $contigs_tot = 0;
my $size_tot = 0;
my $span_tot = 0;
my $avgContig_tot = 0;
my $avgGap_tot = 0;

for (my $ss = 0; $ss <= $#sortScaff; $ss++){
    my $scf = $sortScaff[$ss];
    if ($ss < 5){
	$top5scaff .= 
	    "$ss $scafcontig{$scf} $scaflen{$scf} $adjscaflen{$scf} " .  
	    sprintf("%.2f", $scaflen{$scf} * 1.0 / $scafcontig{$scf}) .  " " .
	    sprintf("%.2f", ($scafcontig{$scf} - 1 > 0)?
		    (($adjscaflen{$scf} - $scaflen{$scf}) * 1.0 / ($scafcontig{$scf} - 1)) :
		    0.0
		    ) . 
		    "\n"
		    ;
	
	$contigs_tot += $scafcontig{$scf};
	$size_tot += $scaflen{$scf};
	$span_tot += $adjscaflen{$scf};
	$avgContig_tot += sprintf("%.2f", $scaflen{$scf} * 1.0 / $scafcontig{$scf});
	$avgGap_tot += sprintf("%.2f", ($scafcontig{$scf} - 1 > 0)?
			       (($adjscaflen{$scf} - $scaflen{$scf}) * 1.0 / ($scafcontig{$scf} - 1)) :
			       0.0);
    }
}
my $gsz;
for (my $ss = 0; $ss <= $#sortScaff; $ss++){
    $sum += $scaflen{$sortScaff[$ss]};
    if (! defined $genomesize){
	$gsz = $Results{'TotalBasesInScaffolds'};
    } else {
	$gsz = $genomesize;
    }
    if ($sum > $gsz / 2){
	$Results{'N50ScaffoldBases'} = $scaflen{$sortScaff[$ss]};
	last;
    }
}


$Results{'AllReads'} = 
    ($gsz > 0)
    ? sprintf("%0.2f", $totalReadLength / $gsz)
    : 0.0;

$Results{'ContigsOnly'} = ($Results{'TotalBasesInScaffolds'} > 0) ? 
    (($surrReadLen + $contigReadLen) / $Results{'TotalBasesInScaffolds'}) : 
    0.0;


$Results{'ContigsAndDegens'} = ($Results{'TotalBasesInScaffolds'} +
				$Results{'DegenContigLen'} > 0) 
    ? (($surrReadLen + $contigReadLen + $degenReadLen) /
       ($Results{'TotalBasesInScaffolds'} + $Results{'DegenContigLen'}))
    : 0.0;


# Surrogates
$Results{'ReadsInSurrogates'}       = $utgSeqs;

# Coverage
$Results{'TotalReads'}              = $totalSeqs;
$Results{'SingletonReads'}          = $Results{'TotalReads'} 
- $Results{'BigContigReads'} 
- $Results{'SmallContigReads'} 
- $Results{'DegenContigReads'} 
- $Results{'ReadsInSurrogates'};

# Parameter settings
$Results{'MinBigContigSizeParm'}    = $MINCONTIG;
$Results{'MinQualityParm'}          = $MINQUAL;

my $tag = -25;
my $val = 10;
my $fval = 13;

if (exists $Results{FromCommandLine}){
    print "Genome size specified on command line\n";
}
print sprintf("%${tag}s %${val}d\n", "EstimatedGenomeSize", $Results{EstimatedGenomeSize});
print "\n";

print "Scaffolds\n";
print "---------\n";

print sprintf("%${tag}s %${val}d\n", "TotalScaffolds", 
	      $Results{TotalScaffolds});
print sprintf("%${tag}s %${val}d\n", "TotalContigsInScaffolds",  
	      $Results{TotalContigsInScaffolds});
print sprintf("%${tag}s %${fval}.2f\n", "MeanContigsPerScaffold",  
	      $Results{MeanContigsPerScaffold});
print sprintf("%${tag}s %${val}d\n", "MinContigsPerScaffold",  
	      $Results{MinContigsPerScaffold});
print sprintf("%${tag}s %${val}d\n", "MaxContigsPerScaffold",  
	      $Results{MaxContigsPerScaffold});
print "\n";
print sprintf("%${tag}s %${val}d\n", "TotalBasesInScaffolds",  
	      $Results{TotalBasesInScaffolds});
print sprintf("%${tag}s %${val}d\n", "MeanBasesInScaffolds",  
	      $Results{MeanBasesInScaffolds});
print sprintf("%${tag}s %${val}d\n", "MinBasesInScaffolds",  
	      $Results{MinBasesInScaffolds});
print sprintf("%${tag}s %${val}d\n", "MaxBasesInScaffolds",  
	      $Results{MaxBasesInScaffolds});
print sprintf("%${tag}s %${val}d\n", "N50ScaffoldBases",  
	      $Results{N50ScaffoldBases});
print "\n";
print sprintf("%${tag}s %${val}d\n", "TotalSpanOfScaffolds",  
	      $Results{TotalSpanOfScaffolds});
print sprintf("%${tag}s %${val}d\n", "MeanSpanOfScaffolds",  
	      $Results{MeanSpanOfScaffolds});
print sprintf("%${tag}s %${val}d\n", "MinScaffoldSpan",  
	      $Results{MinScaffoldSpan});
print sprintf("%${tag}s %${val}d\n", "MaxScaffoldSpan",  
	      $Results{MaxScaffoldSpan});
print sprintf("%${tag}s %${val}d\n", "IntraScaffoldGaps",  
	      $Results{IntraScaffoldGaps});
print sprintf("%${tag}s %${val}d\n", "MeanSequenceGapSize",  
	      $Results{MeanSequenceGapSize});
print "\n";
    
print "Top 5 Scaffolds: \#contigs size span avgContig avgGap\n";
print "-----------------------------------------------------\n";
print $top5scaff;
print "\n";
    
print "Contigs\n";
print "-------\n";
print sprintf("%${tag}s %${val}d\n", "TotalContigsInScaffolds",  
	      $Results{TotalContigsInScaffolds});
print sprintf("%${tag}s %${val}d\n", "TotalBasesInScaffolds",  
	      $Results{TotalBasesInScaffolds});
print sprintf("%${tag}s %${fval}.2f\n", "MeanContigSize",  
	      $Results{MeanContigSize});
print sprintf("%${tag}s %${val}d\n", "MinContigSize",  
	      $Results{MinContigSize});
print sprintf("%${tag}s %${val}d\n", "MaxContigSize",  
	      $Results{MaxContigSize});
print sprintf("%${tag}s %${val}d\n", "N50ContigBases",  
	      $Results{N50ContigBases});
print "\n";
    
print "Big Contigs (>$MINCONTIG)\n";
print "-----------------\n";
print sprintf("%${tag}s %${val}d\n", "TotalBigContigs",  
	      $Results{TotalBigContigs});
print sprintf("%${tag}s %${val}d\n", "BigContigLength",  
	      $Results{BigContigLength});
print sprintf("%${tag}s %${fval}.2f\n", "MeanBigContigSize",  
	      $Results{MeanBigContigSize});
print sprintf("%${tag}s %${val}d\n", "MinBigContig",  
	      $Results{MinBigContig});
print sprintf("%${tag}s %${val}d\n", "MaxBigContig",  
	      $Results{MaxBigContig});
print sprintf("%${tag}s %${fval}.2f%%\n", "BigContigsPercentBases",  
	      $Results{BigContigsPercentBases});
print "\n";

print "Small Contigs (<$MINCONTIG)\n";
print "-----------------\n";
print sprintf("%${tag}s %${val}d\n", "TotalSmallContigs",  
	      $Results{TotalSmallContigs});
print sprintf("%${tag}s %${val}d\n", "SmallContigLength",  
	      $Results{SmallContigLength});
print sprintf("%${tag}s %${fval}.2f\n", "MeanSmallContigSize",  
	      $Results{MeanSmallContigSize});
print sprintf("%${tag}s %${val}d\n", "MinSmallContig",  
	      $Results{MinSmallContig});
print sprintf("%${tag}s %${val}d\n", "MaxSmallContig",  
	      $Results{MaxSmallContig});
print sprintf("%${tag}s %${fval}.2f%%\n", "SmallContigsPercentBases",  
	      $Results{SmallContigsPercentBases});
print "\n";
    
print "Degenerate Contigs\n";
print "------------------\n";
print sprintf("%${tag}s %${val}d\n", "TotalDegenContigs",  
	      $Results{TotalDegenContigs});
print sprintf("%${tag}s %${val}d\n", "DegenContigLength",  
	      $Results{DegenContigLength});
print sprintf("%${tag}s %${fval}.2f\n", "MeanDegenContigSize",  
	      $Results{MeanDegenContigSize});
print sprintf("%${tag}s %${val}d\n", "MinDegenContig",  
	      $Results{MinDegenContig});
print sprintf("%${tag}s %${val}d\n", "MaxDegenContig",  
	      $Results{MaxDegenContig});
print sprintf("%${tag}s %${fval}.2f%%\n", "DegenPercentBases",  
	      $Results{DegenPercentBases});
print "\n";

print "Top 5 Contigs: reads bases\n";
print "--------------------------\n";
print $top5contig;
print "\n";

print "Surrogates\n";
print "----------\n";
print sprintf("%${tag}s %${val}d\n", "NumSurrogates",  
	      $Results{NumSurrogates});
print sprintf("%${tag}s %${val}d\n", "SurrogateSize",  
	      $Results{SurrogateSize});
print sprintf("%${tag}s %${val}d\n", "MinSurrogateSize",  
	      $Results{MinSurrogateSize});
print sprintf("%${tag}s %${val}d\n", "MaxSurrogateSize",  
	      $Results{MaxSurrogateSize});
print sprintf("%${tag}s %${fval}.2f\n", "MeanSurrogateSize",  
	      $Results{MeanSurrogateSize});
print sprintf("%${tag}s %${fval}.2f\n", "SDSurrogateSize",  
	      $Results{SDSurrogateSize});
print "\n";

print "Mates\n";
print "-----\n";
print sprintf("%${tag}s %${val}d\n", "ReadsWithNoMate",  
	      $Results{ReadsWithNoMate});
print sprintf("%${tag}s %${val}d\n", "ReadsWithBadMate",  
	      $Results{ReadsWithBadMate});
print sprintf("%${tag}s %${val}d\n", "ReadsWithGoodMate",  
	      $Results{ReadsWithGoodMate});
print sprintf("%${tag}s %${val}d\n", "ReadsWithUnusedMate",  
	      $Results{ReadsWithUnusedMate});
print sprintf("%${tag}s %${val}d\n", "TotalScaffoldLinks",  
	      $Results{TotalScaffoldLinks});
print sprintf("%${tag}s %${fval}.2f\n", "MeanScaffoldLinkWeight",  
	      $Results{MeanScaffoldLinkWeight});
print "\n";

print "Reads\n";
print "-----\n";
print sprintf("%${tag}s %${val}d\n", "TotalReads",  $Results{TotalReads});
print sprintf("%${tag}s %${val}d\n", "ReadsInContigs",  
	      $Results{ReadsInContigs});
print sprintf("%${tag}s %${val}d\n", "BigContigReads",  
	      $Results{BigContigReads});
print sprintf("%${tag}s %${val}d\n", "SmallContigReads",  
	      $Results{SmallContigReads});
print sprintf("%${tag}s %${val}d\n", "DegenContigReads",  
	      $Results{DegenContigReads});
print sprintf("%${tag}s %${val}d\n", "ReadsInSurrogates",  
	      $Results{ReadsInSurrogates});
print sprintf("%${tag}s %${val}d\n", "SingletonReads",  
	      $Results{SingletonReads});
print "\n";

print "Coverage\n";
print "--------\n";
print sprintf("%${tag}s %${fval}.2f\n", "ContigsOnly",  $Results{ContigsOnly});
print sprintf("%${tag}s %${fval}.2f\n", "ContigsAndDegens",  
	      $Results{ContigsAndDegens});
print sprintf("%${tag}s %${fval}.2f\n", "AllReads",  $Results{AllReads});
print "\n";


exit(0);

####################
sub max($$)
{
  my ($a,$b) = @_;
  return ($a > $b)? $a : $b;
}

sub min($$)
{
  my ($a,$b) = @_;
  return ($a < $b)? $a : $b;
}

