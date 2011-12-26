#!/usr/local/bin/perl

use AMOS::AmosLib;
use TIGR::Foundation;
use POSIX;
use strict;

my $VERSION = '$Revision: 1.5 $ ';
my $HELPTEXT = q~ 
cvgchop (-a <asm_file> | -c <contig_file>) [-sum] [-map] [-cvg <min_cvg>] 
        [-byscf] [-split]

-sum - summarize the coverages
-map - build map of coverage areas
-cvg <min_cvg> - create multi-fasta file of regions
                 in genome with > <min_cvg> coverage. (Default: 2) 
-byscf - the contigs are reported in the order and orientation implied by
    the scaffolds.  Option requires -a.
-split - splits the contigs into chunks that have > 0 coverage over all length
                 
If no option or just -cvg is passed to the program the output file will be
named <asm_file>.<min_cvg>x  or <contig_file>.<min_cvg>x depending on the
type of input.

If -sum or -map is passed, the output will be reported to STDOUT.
~;

my $base = new TIGR::Foundation;

if (! defined $base){
    die("Crazy error\n");
}
$base->setHelpInfo($HELPTEXT);
$base->setVersionInfo($VERSION);

my $MIN_CVG = 2;
my $asmfile;
my $contigfile;
my $summary;
my $map;
my $frgfile;
my $byscaff;
my $dosplit;
my $err = $base->TIGR_GetOptions("a=s" => \$asmfile,
				 "f=s" => \$frgfile,
				 "c=s" => \$contigfile,
				 "sum" => \$summary,
				 "map" => \$map,
				 "byscf" => \$byscaff,
				 "cvg=i" => \$MIN_CVG,
				 "split" => \$dosplit);


if ($err == 0){
    $base->bail("Command line processing failed.  See -h option");
}

if (defined $asmfile && defined $contigfile){
    $base->bail("You can only use one of -a and -c options");
}

if (! defined $asmfile && ! defined $contigfile) {
    $base->bail("You must define either -a or -c option");
}

if (defined $byscaff && ! defined $asmfile){
    $base->bail("You must use -a with -byscf");
}

my $opts = 0;
if (defined $summary) {$opts++};
if (defined $map) {$opts++};
if ($MIN_CVG != 2) {$opts++};

if ($opts > 1){
    $base->bail("You can use at most one of -sum, -map, and -cvg options");
}

select STDERR; $| = 1;
select STDOUT;

my $outfname;

if (defined $contigfile){
    $outfname = $contigfile . ".$MIN_CVG" . "x";
} else {
    $outfname = $asmfile . ".$MIN_CVG" . "x";
}

if (! $summary && ! $map){
    open(OUT, ">$outfname") || die ("Cannot open \"$outfname\": $!\n");
}

if ((defined $frgfile && ! defined $asmfile) ||
    (defined $frgfile && defined $contigfile)){
    $base->bail("You can use the -f option with -a alone");
}

my $contiglen = 0;
my $incontig = 0;
my %eventcoord;
my $thiscontig;
my $seq;
my $chunk; # chunk in contig

if (defined $contigfile){
    open(CTG, "$contigfile") || die ("Cannot open \"$contigfile\": $!\n");
    while (<CTG>){
	if (/^\#\#/){

	}
	if (/^\#\#(\S+) \d+ (\d+)/){
	    my $tmp = $1;
	    my $tmp2 = $2;
	    $chunk = 1;
	    
	    if (defined $thiscontig){
		$seq =~ s/-//g;
		if ($seq ne "") {$contiglen = length($seq);}
		process_contig($thiscontig);
	    }
	    
	    $contiglen = $tmp2;
	    $thiscontig = $tmp;
	    %eventcoord = ();
	    $seq = "";
	    $incontig = 1;
	    
	    next;
	}
	if (/\#.*<(\d+) (\d+)>/){
	    my $first = $1 - 1; my $last = $2; # convert to 0 based coordinates
	    
	    if ($incontig == 1){
		$seq =~ s/-//g;
		$incontig = 0;
	    }
	    
	    while (exists $eventcoord{$first}){
		$first += 0.001;
	    }
	    while (exists $eventcoord{$last}){
		$last += 0.001;
	    }
	    
	    $eventcoord{$first} = "B";
	    $eventcoord{$last} = "E";
	    
	    next;
	}
	if ($incontig == 1){
	    chomp;
	    $seq .= $_;
	}
    }
    
    if (defined $thiscontig) {
	$seq =~ s/-//g;
	if ($seq ne "") {$contiglen = length($seq);}
	process_contig($thiscontig);
    }
    close(CTG);
}

my %seqnames;

if (defined $frgfile){
    open(FRG, "$frgfile") || $base->bail("Cannot open $frgfile: $!\n");
    while (my $record = getRecord(\*FRG)){
	my ($rec, $fields, $recs) = parseRecord($record);
	if ($rec eq "FRG"){
	    my $nm = $$fields{src};
	    my @lines = split('\n', $nm);
	    $nm = join(' ', @lines);
	    if ($nm eq "" || $nm =~ /^\s*$/){
		$nm = $$fields{acc};
	    }
	    $seqnames{$$fields{acc}} = $nm;
	}
    }
    close(FRG);
}

my $rec;
my $fields;
my $recs;

if (defined $asmfile){
    open(ASM, "$asmfile") || $base->bail("Cannot open $asmfile: $!\n");

    my %contigidx;
    my %scaffctg;
    my @scaffolds;
    if (defined $byscaff){
	while (my $record = ((my $seekpos = tell ASM), getRecord(\*ASM))){
	    ($rec, $fields, $recs) = parseRecord($record);
	    if ($rec eq "CCO"){
		$contigidx{getCAId($$fields{acc})} = $seekpos;
	    }
	    if ($rec eq "SCF"){
		my $id = getCAId($$fields{acc});
		push(@scaffolds, $id);
		
		for (my $i = 0; $i <= $#$recs; $i++){
		    my ($sid, $sfs, $srecs) = parseRecord($$recs[$i]);
		    if ($sid eq "CTP"){
			if ($$sfs{ori} eq "N" ||
			    $$sfs{ori} eq "I"){
			    $scaffctg{$id} .= "$$sfs{ct1},BE ";
			} else {
			    $scaffctg{$id} .= "$$sfs{ct2},EB ";
			}
			if ($$sfs{ct1} != $$sfs{ct2} &&
			    $i == $#$recs){
			    if ($$sfs{ori} eq "N" ||
				$$sfs{ori} eq "O"){
				$scaffctg{$id} .= "$$sfs{ct2},BE ";
			    } else {
				$scaffctg{$id} .= "$$sfs{ct2},EB ";
			    }
			} # just for the last CTP record
		    } # if sid eq CTP
		} # for each subrec
	    } # if rec eq SCF
	} # while ASM
	for (my $scf = 0; $scf <= $#scaffolds; $scf++){
	    my @contigs = split(' ', $scaffctg{$scaffolds[$scf]});
	    for (my $cc = 0 ; $cc <= $#contigs; $cc++){
		my ($cid, $cori) = split(',', $contigs[$cc]);
		seek ASM, $contigidx{$cid}, 0; #seek set
		my $record = getRecord(\*ASM);
		if (! defined $record){
		    $base->bail("Cannot find record for contig $cid\n");
		}
		($rec, $fields, $recs) = parseRecord($record);
		if ($rec ne "CCO" || getCAId($$fields{acc}) != $cid){
		    $base->bail("Incorrect record retrieved for contig $cid:\n$record\n");
		}
		if ($cori eq "BE"){
		    doCCO(0, "$scaffolds[$scf]_${cori}_");
		} else {
		    doCCO(1, "$scaffolds[$scf]_${cori}_");
		}
	    }
	}
    } else { # if byscaff
	while (my $record = getRecord(\*ASM)){
	    ($rec, $fields, $recs) = parseRecord($record);
	    if ($rec eq "CCO"){
		doCCO(0, ""); # forward contig
	    } # if CCO
	} # while record
    } # if not byscaff
    close(ASM);
} # if defined $asmfile

if (! $summary){
    close(OUT);
}

exit(0);

########################################################################

#print STDERR "done\n";
sub doCCO {
    my $orient = shift;
    my $scaffold = shift;
    my $id = getCAId($$fields{acc});
    
    my $len = $$fields{len};
    $contiglen = $len;
    my $lseq = $$fields{cns};
    my @fields = split('\n', $lseq);
    $lseq = join('', @fields);
    $seq = $lseq;
    $seq =~ s/-//g;
    
    my @offsets = ();
    $#offsets = length($lseq);
    my $coord = 0;
    for (my $i = 0; $i < length($lseq); $i++){
	if (substr($lseq, $i, 1) ne "-"){
	    $coord++;
	} else {
	    $contiglen--;
	}
	$offsets[$i] = $coord;
    }
    
    %eventcoord = ();
    
    for (my $i = 0; $i <= $#$recs; $i++){
	my ($sid, $sfs, $srecs) = parseRecord($$recs[$i]);
	
	if ($sid eq "MPS"){
	    my $rid = getCAId($$sfs{mid});
	    my $asms = $$sfs{pos};
	    $asms =~ /(\d+),(\d+)/;

	    my $asml;
	    my $asmr;
	    if ($1 > $2){
		$asml = $2;
		$asmr = $1;
	    } else {
		$asml = $1;
		$asmr = $2;
	    }

	    $asml = $offsets[$asml - 1];
	    $asmr = $offsets[$asmr - 1];

	    if ($orient == 1){
		my $tmpr = $contiglen - $asml;
		my $tmpl = $contiglen - $asmr;
		$asml = $tmpl;
		$asmr = $tmpr;
	    }
	    while (exists $eventcoord{$asml}){
		$asml += 0.001;
	    }
	    while (exists $eventcoord{$asmr}){
		$asmr += 0.001;
	    }
	    
	    $eventcoord{$asml} = "B";
	    $eventcoord{$asmr} = "E";
	}
    } # for each rec
    
    process_contig("$scaffold" . "$id");
} # doCCO

sub process_contig {
    my $ctg = shift;
    if ($map){
	print ">$ctg\n";
    }
    
    my $cvg = 0; 
    my $start = 0;
    my $ss = 0;
    my $se = 0;
    my $end = 0;
    my $piece = 0;
    my $coverages = 0;
    my $maxcvg = 0;
    foreach my $coord (sort {$a <=> $b} keys (%eventcoord)){
	if ($eventcoord{$coord} eq "B"){
	    if ($summary){
		$coverages += $cvg * ($coord - $ss);
		$ss = $coord;
	    }
	    if ($map) {
		if (POSIX::floor($coord) != POSIX::floor($ss)) {
		    printf ("%d %d %d\n", POSIX::floor($ss), POSIX::floor($coord), int($cvg));
		}
		$ss = $coord;
	    }
	    $cvg++;
	    if ($start == 0 && $cvg >= $MIN_CVG){
		$start = $coord;
	    }
	} # if beginning

	if ($eventcoord{$coord} eq "E"){
	    if ($summary){
		$coverages += $cvg * ($coord - $ss);
                if ($maxcvg == 0 || $cvg > $maxcvg){
	            $maxcvg = $cvg;
		}
		$ss = $coord;
	    }
	    if ($map){
		if (POSIX::floor($coord) != POSIX::floor($ss)){
		    printf ("%d %d %d\n", POSIX::floor($ss), POSIX::floor($coord), int($cvg));
		}
		$ss = $coord;
	    }
	    $cvg--;
	    if ($start != 0 && $cvg < $MIN_CVG){
		$end = $coord;

		if (! $summary && ! $map){
		    $piece++; 
		    printFastaSequence(\*OUT, 
				       "${ctg}_${piece} $start $end", 
				       substr($seq, $start, $end - $start));
		}
		$start = $end = 0;
	    }
	} # if end
    } # foreach

    if ($ss < $contiglen && $map){
	printf ("%d %d %d\n", POSIX::floor($ss), $contiglen, int($cvg));
    }
    if ($summary){
	print "$ctg $contiglen ", ($coverages / $contiglen), " ", $maxcvg, "\n";
    }
} # process_contig





