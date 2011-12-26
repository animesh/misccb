#!/usr/local/bin/perl

use TIGR::Foundation;
use AMOS::ParseFasta;
use strict;

my $VERSION = '$Revision: 1.4 $ ';
my $HELP = q~
    Joiner -ce contig_edge_file -co contig_overlap_file -c contig_file -o out

    assumes each pair of contigs occurs once in the contig_edge file
    assumes all overlaps are listed in contig_overlap file (more than one
   per contig pair)
~;

my $base = new TIGR::Foundation();
if (! defined $base) {
    die("A horrible death\n");
}

$base->setVersionInfo($VERSION);
$base->setHelpInfo($HELP);

my $chainfile;
my $contigfile;
my $outfile;

my $err = $base->TIGR_GetOptions("ch=s"  => \$chainfile,
				 "c=s"   => \$contigfile,
                                 "o=s"   => \$outfile);

my %ctgpos;
my %ctglen;
my %ctgseq;
my $pos;

open(CTG, $contigfile) || $base->bail("Cannot open $contigfile: $!\n");

my $maxCtgId = 0;

# first we build an index
$pos = tell CTG;
while (<CTG>){
    if (/^\#\#(\S+) (\d+) (\d+)/){
	$ctgpos{$1} = $pos;
	$ctglen{$1} = $3;
	$ctgseq{$1} = $2;
	if ($1 > $maxCtgId){
	    $maxCtgId = $1;
	}
    }
    $pos = tell CTG;
}

$maxCtgId++;

if (defined $outfile){
    open(STDOUT, ">$outfile") || $base->bail("cannot open $outfile: $!\n");
}

open(CH, $chainfile)|| $base->bail("Cannot open $chainfile: $!\n");
my $parser = new ParseFasta(\*CH, '>', "\n");
if (! defined $parser){
    $base->bail("Cannot parse fasta file...");
}

print STDERR "Starting numbering from $maxCtgId\n";
while (my($head, $data) = $parser->getRecord()){
    
    my ($chainId, $ctgs, $seqs, $bases) = split(' ', $head);
    my @contigs = split('\n', $data);

    my $ctgId;

    if ($ctgs > 1){
	$ctgId = $maxCtgId + $chainId;
    } else {
	my ($id, $off, $ori) = split(' ', $contigs[0]);
	$ctgId = $id;
    }

    print "##$ctgId $seqs $bases bases\n";
    
    my %byoffset;

    for (my $ct = 0; $ct <= $#contigs; $ct++){
	my ($id, $off, $ori) = split(' ', $contigs[$ct]);
	my $len;
	print STDERR "Doing $id $off $ori\n";

	seek(CTG, $ctgpos{$id}, 0) 
	    || $base->bail("Cannot find contig $id in file: $!\n");
        $_ = <CTG>;
	if ($_ =~ /^\#\#(\d+) (\d+) (\d+)/){
	    if ($1 != $id){
		$base->bail("Contig $id  does not match file: $_");
	    }
	    $len = $3;
	} else {
	    $base->bail("Contig $id does not exist in file: $_");
	}
	
	while (<CTG>){
	    if (/^\#\#/){ 
		last;
	    }
	    if (/^\#([^(]+)\((\d+)\) (\d+) bases {(\d+) (\d+)} <(\d+) (\d+)>/){
		my $seqid = $1;
		my $offset = $2;
		my $seqlen = $3;
		my $seq_lend = $4;
		my $seq_rend = $5;
		my $asm_lend = $6;
		my $asm_rend = $7;
		
		if ($ori eq '<'){
		    my $tmp = $seq_lend;
		    $seq_lend = $seq_rend;
		    $seq_rend = $tmp;
		    $tmp = $asm_lend;
		    $asm_lend = $len - $asm_rend;
		    $asm_rend = $len - $tmp;
		    $offset = $len - $offset;
		}
		$offset += $off;
		$asm_lend += $off;
		$asm_rend += $off;
		
		my $idx = $offset;
		
		while (exists $byoffset{$idx}){
		    $idx += 0.0001;
		}
		
		$byoffset{$idx} = "\#$seqid($offset) $seqlen bases {$seq_lend $seq_rend} <$asm_lend $asm_rend>\n";
	    } # if read record
        } # while <CTG>
    } # for my $ct in %contigs
    foreach my $i (sort {$a <=> $b} keys %byoffset){
	print $byoffset{$i};
    }
} # for each chain

close(CTG);
exit(0);
