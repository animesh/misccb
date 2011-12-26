#!/usr/local/bin/perl

# $Id: ace2contig.pl,v 1.6 2007/10/19 21:22:48 jamesrwhite Exp $
#
# Converts from a TIGR .asm file to a new .ace file
#
#  Copyright @ 2002, The Institute for Genomic Research (TIGR). 

use strict;
use AMOS::AmosLib;
use TIGR::Foundation;

my $base = new TIGR::Foundation;
if (! defined $base){
    die("Weird problem!\n");
}

my $VERSION = '1.0 $Revision: 1.6 $ ';

my $acefile;
my $contigfile;

my $HELP_INFO = q~
.USAGE.
  ace2contig [-o <output prefix>] [-i <acefile>]

.DESCRIPTION.
  This program extracts contig information from an Ace file and outputs
  a .contig file.

.OPTIONS.
  -i Ace file
  -o output prefix (output is <prefix>.contig)

.KEYWORDS.
  converter, ace, contig

~;

$base->setHelpInfo($HELP_INFO);

my $err = $base->TIGR_GetOptions("i=s" => \$acefile,
				 "o=s" => \$contigfile
				 );

if (! defined $acefile){
    $acefile = $ARGV[0];
    if (! defined $acefile) {
	$base->bail("You must specify an .ACE file\n");
    }
}

if ($acefile !~ /\.ace$/){
    $base->bail("Ace file \"$acefile\" must end in .ace\n");
}

if (! defined $contigfile){
    my $prefix;
    $acefile =~ /(.*)\.ace$/;
    $prefix = $1;
    $contigfile = $prefix . ".contig";
}

open(ACE, $acefile) || $base->bail("Cannot open \"$acefile\": $!\n");
open(CONTIG, ">$contigfile") || $base->bail("Cannot open \"$contigfile\": $!\n");

my $contigName;
my $contigLen;
my $contigSeqs;
my $inContig = 0;
my $inSequence = 0;
my $seqName;
my $seq;
my $ctgSeq;
my %offset;
my %rc;
my @gaps;
my $contigSequence;
my $ctgLeft;
my $ctgRight;   # where we'll trim the contig
my $asmLeft;
my $asmRight;
my $nseq;
my $tmpfile = "tmpfile.$$";
while (<ACE>){
    if (/^CO (\S+) (\d+) (\d+)/){
	if (defined $ctgSeq){
	    $ctgSeq = substr($ctgSeq, $ctgLeft, $ctgRight - $ctgLeft);
#	    print "trimming to $ctgLeft $ctgRight\n";
	    printContigRecord(\*CONTIG, $contigName, length($ctgSeq), $nseq, $ctgSeq, "contig");
	    close(TMP);
	    open(TMP, $tmpfile) || $base->bail("Cannot open $tmpfile: $!\n");
	    while (<TMP>){
		chomp;
		if (/^\#(\S+)\((\d+)\)(.*)\{(\d+) (\d+)\} <(\d+) (\d+)>$/){
		    print CONTIG sprintf("#%s(%d)%s{%d %d} <%d %d>\n",
					 $1, $2 - $ctgLeft, $3, $4, $5, $6 - $asmLeft + 1, $7 - $asmLeft + 1);;
		} else {
		    print CONTIG "$_\n";
		}
	    }
	    close(TMP);
	}
	open(TMP, ">$tmpfile") || $base->bail("Cannot open $tmpfile: $!\n");
	$nseq = 0;
	$ctgLeft = $ctgRight = -1;
	$asmLeft = $asmRight = -1;
	$contigName = $1;
	$contigLen = $2;
	$contigSeqs = $3;
			   
	$inContig = 1;
	$ctgSeq = "";
	$seq = "";
	%offset = ();
	next;
    }
    if ($inContig && /^\s*$/){
	$inContig = 0;
	$seq =~ s/\*/-/g;
	@gaps = (); 
	my $gap  = index($seq, "-");
        while ($gap != -1){
            push(@gaps, $gap + 1);
            $gap = index($seq, "-", $gap + 1);
        }

	$ctgSeq = $seq;
	next;
    }
    if ($inSequence && $_ =~ /^\s*$/){
	$inSequence = 0;
	next;
    }
    if ($inContig || $inSequence) {
	chomp;
	$seq .= $_;
	next;
    }
    if (/^AF (\S+) (\w) (-?\d+)/){
	$offset{$1} = $3;
	$rc{$1} = $2;
	next;
    }
    if (/^RD (\S+)/){
	$inSequence = 1;
	$seqName = $1;
	$seq = "";
	next;
    }
    if (/^QA (-?\d+) (-?\d+) (\d+) (\d+)/){ # qual range, align range
	my $offset = $offset{$seqName};
	my $cll = $1;
	my $clr = $2;
	my $end5 = $3;
	my $end3 = $4;
	if ($cll == -1 && $clr == -1){  # poor quality sequence - exclude
	    next;                  
	}
	$nseq++;

	if ($cll > $end5) { $end5 = $cll;} # trim poor quality sequence
	if ($clr < $end3) { $end3 = $clr;}

	$seq =~ s/\*/-/g;
	my $len = length($seq);
	$offset += $end5 - 2;
	if ($ctgLeft == -1 || $offset < $ctgLeft){
#	    print "setting ctgLeft to $offset\n";
	    $ctgLeft = $offset;
	}
	if ($ctgRight == -1 || $offset + $end3 - $end5 + 1 > $ctgRight){
	    $ctgRight = $offset + $end3 - $end5 + 1;
#	    print "setting ctgRight to $ctgRight\n";
	}

	$seq = substr($seq, $end5 - 1, $end3 - $end5 + 1);
	
	my $i = 0;
	my $asml = $offset + 1;
	my $asmr = $asml + $end3 - $end5 + 1;
	while ($i <= $#gaps && $offset > $gaps[$i]){
	    $asml--; $asmr--; $i++;
	} # get rid of gaps from offset here
#	print "finding gaps between $offset and $offset + $clr - $cll + 1\n";
	while ($i <= $#gaps && ($offset + $clr - $cll + 1 > $gaps[$i])){
	    $asmr--; $i++;
	}

	if ($asmLeft == -1 || $asml < $asmLeft){
	    $asmLeft = $asml;
	}
	if ($asmRight == -1 || $asmr > $asmRight){
	    $asmRight = $asmr;
	}

	if ($rc{$seqName} eq "C"){ # make coordinates with respect to forw strand
	    $cll = $len - $cll + 1;
	    $clr = $len - $clr + 1;
	    my $tmp = $cll;
	    $cll = $clr;
	    $clr = $tmp;
	}

	while ($seq =~ /-/g){ #make $clr ungapped
	    $clr--;
	}

	if ($rc{$seqName} eq "C"){
	    my $tmp = $cll;
	    $cll = $clr;
	    $clr = $tmp;
	}
	
	printSequenceRecord(\*TMP, $seqName, $seq, $offset, (($rc{$seqName} eq "U")?"" : "RC"), $cll, $clr, $asml, $asmr, "contig");
	next;
    }
}

if (defined $ctgSeq){
#    print "trimming to $ctgLeft $ctgRight\n";
    $ctgSeq = substr($ctgSeq, $ctgLeft, $ctgRight - $ctgLeft);
    printContigRecord(\*CONTIG, $contigName, length($ctgSeq), $nseq, $ctgSeq, "contig");
    close(TMP);
    open(TMP, $tmpfile) || $base->bail("Cannot open $tmpfile: $!\n");
    while (<TMP>){
	chomp;
	if (/^\#(\S+)\((\d+)\)(.*)\{(\d+) (\d+)\} <(\d+) (\d+)>$/){
	    print CONTIG sprintf("#%s(%d)%s{%d %d} <%d %d>\n",
				 $1, $2 - $ctgLeft, $3, $4, $5, $6 - $asmLeft + 1, $7 - $asmLeft + 1);
	} else {
	    print CONTIG "$_\n";
	}
    }
    close(TMP);
}
unlink($tmpfile)|| $base->bail("Cannot remove $tmpfile: $!\n");


close(CONTIG);
close(ACE);
exit(0);
