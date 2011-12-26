#!/usr/bin/perl

#
# This program outputs the set of singleton reads in an assembly in one of
# three formats:
#
# FASTA file (either the entire read or just the clear range)
# Contig file (just the header for use in Bambus)
# list (simply a list of the sequence names)
#
# A singleton is a read present in the .frg file that is not referenced by any 
# CCO or UTG record in the .asm file

use IO::File;
use File::Basename;
use AMOS::AmosFoundation;
use AMOS::AmosLib;
use strict;

my $MY_VERSION = " Version 1.0 (Build " . (qw/$Revision: 1.2 $/ )[1] . ")";

# Constants

my $MY_HELPTEXT = qq~
.USAGE.
  ca2singletons -i file.asm -o file.fasta -f file.frg [-contig|-clear|-list]

.DESCRIPTION.
  This program converts from a Celera .frg and .asm file to a list of singleton
  read sequences in fasta format.

.OPTIONS.
  -i input  .asm file
  -o output .fasta file name
  -f input  .frg file 
  -clear    outputs just the clear range of the singletons
  -contig   outputs singletons in TIGR .contig format
  -list     outputs a list of the singleton names

.KEYWORDS.
  converter, celera, singletons
~;

my $base = new AMOS::AmosFoundation;

if (! defined $base){
    print STDERR "Nasty error, hide!\n";
    exit(1);
}



$base->setHelpText($MY_HELPTEXT);
$base->setUsage("getSingletons -i file.asm -o file.fasta -f file.frg [-contig|-clear|-list]");
$base->setVersion($MY_VERSION);

my $infile;
my $outfile;
my $frgfile;
my $selfile;
my $clear;
my $contig;
my $list;

my $err = $base->getOptions("i=s" => \$infile,
				 "o=s" => \$outfile,
				 "f=s" => \$frgfile,
				 "contig" => \$contig,
				 "clear" => \$clear,
				 "list" => \$list);


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

my $record;
my %seqnames;

open(IN, $infile) ||
    $base->bail("Cannot open $infile: $!");
my $prefix = (split /\./,basename($infile))[0];

if (defined $outfile){
    open(STDOUT, ">$outfile") ||
	$base->bail("Cannot open \"$outfile\": $!\n");
}

print STDERR "pass through asm\n";
while ($record = getRecord(\*IN)){
    my ($type, $fields, $recs) = parseRecord($record);
    
    if ($type eq "CCO"){# || $type eq "UTG"){
	for (my $i = 0; $i <= $#$recs; $i++){
	    my ($sid, $sfs, $srecs) = parseRecord($$recs[$i]);
	    if ($sid eq "MPS"){
		my $fid = getCAId($$sfs{"mid"});
		$seqnames{$fid} = 1;
	    } # if MPS
	} # for each record
	
    } # if $type = CCO
    if ($type eq "UTG"){
	if ($$fields{"sta"} eq "N" ||
	    $$fields{"sta"} eq "U"){
	    next;
	}
	for (my $i = 0; $i <= $#$recs; $i++){
            my ($sid, $sfs, $srecs) = parseRecord($$recs[$i]);
            if ($sid eq "MPS"){
                my $fid = getCAId($$sfs{"mid"});
		$seqnames{$fid} = 1;
            } # if MPS
        } # for each record
    }   
    
} # while $record
print STDERR "done\n";


if (defined $frgfile){
    print STDERR "Doing $frgfile\n";
    open(FRG, $frgfile) || $base->bail("Cannot open $frgfile: $!\n");
    while ($record = getRecord(\*FRG)){
	my ($type, $fields, $recs) = parseRecord($record);
	if ($type eq "FRG"){
	    my $id = getCAId($$fields{acc});
	    my $nm = $$fields{src};
	    my @lines = split('\n', $nm);
	    $nm = join(' ', @lines);
	    if ($nm =~ /^\s*$/){
		$nm = $id;
	    }
	    if (! exists $seqnames{$id}){
		# print record
		my ($cll, $clr) = split(',', $$fields{clr});
		my $len = $clr - $cll;
		if (defined $contig){
		    print "##$nm 1 $len bases, 00000000 checksum.\n";
		    print "$nm(0) [] $len bases, 00000000 checksum. {1 $len} <1 $len>\n";
		} elsif (defined $list){
		    print "$nm\n";
		} else {
		    my $sequence = $$fields{seq};
		    my @seq = split('\n', $sequence);
		    $sequence = join('', @seq);
		    if (defined $clear){
			$sequence = substr($sequence, $cll, $clr - $cll);
		    }
		    print ">$nm\n";
		    for (my $j = 0; $j < length($sequence); $j+=60){
			print substr($sequence, $j, 60), "\n";
		    }
		}
	    }
	}
    }
    close(FRG);
}
print STDERR "done\n";
exit(0);
