#!/usr/local/bin/perl

# $Id: ca2scaff.pl,v 1.6 2005/07/20 15:22:52 mpop Exp $
#
# This program creates an empty .ctg file that only contains
# the headers for the sequences and contigs
#

use IO::File;
use File::Basename;
use AMOS::AmosFoundation;
use AMOS::AmosLib;
use strict;

my $MY_VERSION = " Version 1.0 (Build " . (qw/$Revision: 1.6 $/ )[1] . ")";

# Constants

my $MY_HELPTEXT = qq~
    ca2scaff -i file.asm -o prefix [-details -f file.frg] -clk
~;

my $base = new AMOS::AmosFoundation;

if (! defined $base){
    print STDERR "Nasty error, hide!\n";
    exit(1);
}


$base->setHelpText($MY_HELPTEXT);
$base->setUsage($MY_HELPTEXT);
$base->setVersion($MY_VERSION);

my $infile;
my $outfile;
my $frgfile;
my $dodetails;
my $doclk;

my $err = $base->getOptions("i=s" => \$infile,
				 "o=s" => \$outfile,
				 "details" => \$dodetails,
				 "f=s" => \$frgfile,
				 "clk" => \$doclk);


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

if (! defined $outfile){
    $base->bail("Must specify an output prefix.  See -h option");
}


my $record;

my %seqnames;
if (defined $frgfile){
    open(FRG, $frgfile) || $base->bail("Cannot open $frgfile: $!\n");
    while ($record = getRecord(\*FRG)){
	my ($type, $fields, $recs) = parseRecord($record);
	if ($type eq "FRG"){
	    my $id = $$fields{"acc"};
	    my $name = $$fields{"src"};
	    my @name = split('\n', $name);
	    $name = join("", @name);
	    $seqnames{$id} = $name;
	}
    }
}

my $oofile = "$outfile.oo";
my $sumfile = "$outfile.sum";
my $gapfile = "$outfile.gaps";
my $linkfile = "$outfile.links";
my $detailfile = "$outfile.details";

open(OO, ">$oofile") ||
    $base->bail("Cannot open $oofile: $!\n");
open(SUM, ">$sumfile") ||
    $base->bail("Cannot open $sumfile: $!\n");
open(GAP, ">$gapfile") ||
    $base->bail("Cannot open $gapfile: $!\n");
open(LNK, ">$linkfile") ||
    $base->bail("Cannot open $linkfile: $!\n");
if (defined $dodetails) {
    open(DET, ">$detailfile") ||
	$base->bail("Cannot open $detailfile: $!\n");
}

print LNK "<?xml version = \"1.0\" ?>\n";
print LNK "<EVIDENCE>\n";

open(IN, $infile) ||
    $base->bail("Cannot open $infile: $!");
my $prefix = (split /\./,basename($infile))[0];


my %ctglen;
my %ctglinks;
my $lnkid = 0;
while ($record = getRecord(\*IN)){
    my ($type, $fields, $recs) = parseRecord($record);

    if ($type eq "CCO"){
	my $id = getCAId($$fields{"acc"});
	my $contiglen = $$fields{"len"};
	while ($$fields{"cns"} =~ /-/g){
	    $contiglen--;
	}

	$ctglen{$id} = $contiglen;
    }
    if ($type eq "CLK"){
	if (defined $dodetails){
	    my $pair;
	    $pair = "$$fields{co1} $$fields{co2}";
	    my @seqs = split('\n', $$fields{"jls"});
	    for (my $s = 0; $s <= $#seqs; $s++){
		my  @mates = split(',', $seqs[$s]);
		$ctglinks{$pair} .= "$seqnames{$mates[0]} $seqnames{$mates[1]} ";
	    }
	} elsif (defined $doclk){
	    my $c1 = $$fields{co1};
	    my $c2 = $$fields{co2};
	    my $o1;
	    my $o2;
	    
	    if ($$fields{ori} eq "I"){ # innie
		$o1 = "BE";
		$o2 = "EB";
	    } elsif ($$fields{ori} eq "O") { #outie
		$o1 = "EB";
		$o2 = "BE";
	    } elsif ($$fields{ori} eq "N") { #normal
		$o1 = "BE";
		$o2 = "BE";
	    } elsif ($$fields{ori} eq "A") { # anti
		$o1 = "EB";
		$o2 = "EB";
	    }
	    my $gap = $$fields{mea};
	    
	    for (my $ll = 0; $ll < $$fields{num}; $ll++){
		print LNK "<LINK ID = \"lnk_$lnkid\" SIZE = \"$gap\" TYPE =\"CA\">\n";
		print LNK "  <CONTIG ID=\"contig_$c1\" ORI=\"$o1\"></CONTIG>\n";
		print LNK "  <CONTIG ID=\"contig_$c2\" ORI=\"$o2\"></CONTIG>\n";
		print LNK "</LINK>\n";
		$lnkid++;
	    }
	}
    }
    if ($type eq "SCF"){
	my $id = getCAId($$fields{"acc"});
	my $scflen = 0;
	my $scfspan = 0;
	my $nctg = $$fields{"noc"} + 1;

	print OO ">$id\n";
	print GAP ">$id\n";

	if (defined $dodetails && $nctg > 1){
	    print DET "\nScaffold $id\n";
	}
	for (my $i = 0; $i <= $#$recs; $i++){
	    my ($sid, $sfs, $srecs) = parseRecord($$recs[$i]);
	    if ($sid eq "CTP"){
		my $c1 = $$sfs{"ct1"};
		my $c2 = $$sfs{"ct2"};
		my $pair = "$c1 $c2";
		my $rev = 0;
		if (! exists $ctglinks{$pair}){
		    $pair = "$c2 $c1";
		    $rev = 1;
		}
		my $ori = $$sfs{"ori"};
		my $o1;
		my $o2;
		if ($ori eq "N" || $ori eq "I"){
		    $o1 = "BE";
		} else {
		    $o1 = "EB";
		}

		if ($ori eq "N" || $ori eq "O"){
		    $o2 = "BE";
		} else {
		    $o2 = "EB";
		}

		if (defined $dodetails && $nctg > 1){
		    if ($rev){
			print DET "$c2 $o2 ---- $c1 $o1\n";
		    } else {
			print DET "$c1 $o1 ---- $c2 $o2\n";
		    }
		    my @mates = split(' ', $ctglinks{$pair});
		    for (my $m = 0; $m <= $#mates; $m+= 2){
			print DET "  $mates[$m] - ", $mates[$m + 1], "\n";
		    }
		}

		if ($i == 0){
		    print OO "$c1 $o1\n";
		    $scflen += $ctglen{$c1};
		    $scfspan += $ctglen{$c1};
		}
		if ($nctg != 1){
		    print OO "$c2 $o2\n";
		    $scflen += $ctglen{$c2};
		    $scfspan += $ctglen{$c2};
		
		    print GAP "$c1 $c2 $$sfs{mea}\n";
		    my $gap = $$sfs{mea};
		    $scfspan += $gap;
		    $gap = int($gap);
# 		if ($o1 eq "BE"){
# 		    $gap += $ctglen{$c1};
# 		}
# 		if ($o2 eq "EB"){
# 		    $gap += $ctglen{$c2};
# 		}
		    my $gapmin = $gap - 10;
		    my $gapmax = $gap + 10;
		    if (! defined $doclk){
			print LNK "<LINK ID = \"lnk_${id}_$lnkid\" SIZE = \"$gap\" TYPE =\"CA\">\n";
			print LNK "  <CONTIG ID=\"contig_$c1\" ORI=\"$o1\"></CONTIG>\n";
			print LNK "  <CONTIG ID=\"contig_$c2\" ORI=\"$o2\"></CONTIG>\n";
			print LNK "</LINK>\n";
		    }
#		print LNK "link ins_" . "$id" . "$lnkid" . " contig_$c1 $o1 contig_$c2 $o2 J " . "$gapmin $gapmax Y\n";
		    $lnkid++;
		}
	    } # if CTP
	} # for each rec
	print SUM "$id $nctg $scflen ", int($scfspan), "\n";

    } # if $type = SCF
} # while $record
print LNK "</EVIDENCE>\n";
close(LNK);
close(SUM);
close(GAP);
close(OO);
close(IN);
if (defined $dodetails){ close(DET);}
exit(0);
