#!/usr/local/bin/perl

# $Id: ta2ace.pl,v 1.3 2010/02/04 09:38:30 floflooo Exp $
#
# Converts from a TIGR .asm file to a new .ace file
#
#  Copyright @ 2002, The Institute for Genomic Research (TIGR).  All
#  rights reserved.

#  This software is provided "AS IS".  TIGR makes no warranties, express
#  or implied, including no representation or warranty with respect to
#  the performance of the software and derivatives or their safety,
#  effectiveness, or commercial viability.  TIGR does not warrant the
#  merchantability or fitness of the software and derivatives for any
#  particular purpose, or that they may be exploited without infringing
#  the copyrights, patent rights or property rights of others. TIGR shall
#  not be liable for any claim, demand or action for any loss, harm,
#  illness or other damage or injury arising from access to or use of the
#  software or associated information, including without limitation any
#  direct, indirect, incidental, exemplary, special or consequential
#  damages.

#  This software program may not be sold, leased, transferred, exported
#  or otherwise disclaimed to anyone, in whole or in part, without the
#  prior written consent of TIGR.

use strict;
use File::Spec;
use TIGR::Foundation;
use TIGR::FASTAreader;
use TIGR::FASTArecord;
use DBI;

my $base = new TIGR::Foundation;

if (! defined $base){
    die ("Foundation cannot be created.  FATAL!\n");
}

my $VERSION = 'ta2ace version 1.0 $Revision: 1.3 $ ';
$base->setVersionInfo($VERSION);

my $GREP = "/bin/grep";
$base->addDependInfo($GREP);

my %TA2ACEQUAL = (1 => 40,
		  2 => 40,
		  3 => 40,
		  4 => 40,
		  5 => 40,
		  6 => 40,
		  7 => 30,
		  8 => 30,
		  9 => 20,
		  10 => 20,
		  11 => 20,
		  12 => 20,
		  13 => 20,
		  14 => 20,
		  15 => 0,
		  16 => 0,
		  17 => 0,
		  18 => 0,
		  19 => 0,
		  20 => 0,
		  21 => 0,
		  22 => 0,
		  23 => 0);  # TIGR quality class to phrap quality value match
		  

my $HELPTEXT = q~
   ta2ace [opts] [infile]
       
   Options:   
        -i <infile>.asm TIGR .asm file.  Must end in .asm
        -o <outfile>    Output file, by default <infile>.ace
        -c <chromat_dir> Location of the chromatograms
        -p <phd_dir>    Location of the PHD directory
        -s <seqfile>    Sequence file, by default <infile>.seq
        -D <db>         Database (if not using -i and -s)
        -S <server>     Database info
        -U <user>
        -P <pass>
        -a <asmfile>    List of assemblies to put in .ace file
    ~;

$base->setHelpInfo($HELPTEXT);

my $infile;
my $outfile;
my $seqfile;
my $chromodir = "../chromat_dir";
my $phddir = "../phd_dir";
my $asmfile;
my $dbtype = "Sybase";
my $dbserver = "SYBTIGR";
my $dbuser = "access";
my $dbpass = "access";
my $db;

my $err = $base->TIGR_GetOptions(
				 "i=s" => \$infile,
				 "o=s" => \$outfile,
				 "c=s" => \$chromodir,
				 "p=s" => \$phddir,
				 "s=s" => \$seqfile,
				 "D=s" => \$db,
				 "a=s" => \$asmfile,
				 "S=s" => \$dbserver,
				 "U=s" => \$dbuser,
				 "P=s" => \$dbpass
				 );

if (! $err){
    $base->bail("Error processing options!");
}

if (! defined $infile){
    $infile = $ARGV[0];
}

if (defined $infile && $infile !~ /\.t*asm$/){
    $base->bail("Input file must end in .asm");
}

my $prefix;

if (defined $infile){
    $infile =~ /(.*)\.t*asm$/;
    $prefix = $1;
}

if (defined $db && defined $infile) {
    $base->bail("You must use either -D or -i");
}

if (defined $db && ! defined $asmfile){
    $base->bail("Option -D requires option -a");
}

if (! defined $prefix){
    $prefix = $asmfile;
}

if (! defined $outfile){
    $outfile = "$prefix.ace";
}

if (! defined $seqfile && defined $infile){
    $seqfile = "$prefix.seq";
}

# get rid of trailing slashes
$chromodir =~ s/\/+$//;
$phddir =~ s/\/+$//;


# Here's the process
#
# * get contig information and generate contig record in contig temp file
# * for each of the sequences generate sequence information and store coords in
# contig writing sequence records in another sequence temp file
# * generate tiling info and concatenate both temp files into an output temp
# file
#
# * when all contigs are done generate output file header and concatenate
# output temp file to output. 


# counters for the useful information
my $nContigs = 0;
my $nReads = 0;

my $dbproc;
my $fr;
if (defined $db){
    $dbproc = DBI->connect("dbi:$dbtype:server=$dbserver;packetSize=8092",
			   $dbuser, $dbpass);
    
    if (! defined $dbproc) {
	$base->bail("Cannot connect to database server \"$dbserver\"");
    }

    $dbproc->do("set textsize 50000000") ||
	$base->bail("Cannot set maximum assembly size");

    $dbproc->do("use $db") ||
	$base->bail("Cannot select database \"$db\"");

    open(IN, $asmfile) || 
	$base->bail("Cannot open input file \"$asmfile\" : $!");
} else {
    open(IN, $infile) ||
	$base->bail("Cannot open input file \"$infile\" : $!");

    my @readErr;
    $fr = new TIGR::FASTAreader($base, \@readErr, $seqfile);
    if (! defined $fr){
	while (my $err = pop(@readErr)){
	    $base->logError($err);
	}
	$base->bail("Could not read sequence file \"$seqfile\"");
    }
} # if defined $db

my $TMP = $$ . time();
my $dirTmp = $base->getTempDir();
my $ctgTmp = File::Spec->catfile($dirTmp, "$TMP.CTG.TMP");
my $seqTmp = File::Spec->catfile($dirTmp, "$TMP.SEQ.TMP");
my $outTmp = File::Spec->catfile($dirTmp, "$TMP.OUT.TMP");

open(OUT, ">$outTmp") ||
    $base->bail("Cannot open output temp \"$outTmp\" : $!");

my $inAsm = 1; # are we in an assembly record
my $nseqs;     # number of sequences in this contig
my $seenseqs;  # number of sequences we've actually seen
my $contigid;  # contigId
my $seqName;
my %seqClr;    # clear range from seq file
my %seqAlnClr; # clear range from .asm file
my %seqAlnRng; # assembly range from .asm file
my %seqOff;    # sequence offset
my %rend;      # right end of sequences
my $cll;       # aligned clear range
my $clr; 
my $al;        # aligned coordinates (asm_lend, rend)
my $ar;
my $end5;      # clear range
my $end3; 
my $contigLen; # # bases in the contig
my $contigSeq;
while (<IN>){
    my $seq;
    my $qual;
    if (defined $db){
	chomp;
	$contigid = $_;

	print STDERR "Doing contig $contigid\n";
	my $ctgQuery = qq~
	    select lsequence, quality, seq\#
	    from assembly
	    where asmbl_id = $contigid
		~;

	my $qh = $dbproc->prepare($ctgQuery) ||
	    $base->bail("Cannot prepare query: $ctgQuery");

	$qh->execute() ||
	    $base->bail("Cannot execute query: $ctgQuery");

	my $lineref = $qh->fetchrow_arrayref;
	$qh->finish;
	
	$seq = $$lineref[0];
	$qual = $$lineref[1];
	$nseqs = $$lineref[2];
	$seenseqs = 0;
    }
#    if ($inAsm){ # this is for the contig part of the record
    if (defined $db || ($inAsm && $_ =~ /^lsequence\t(.*)$/)){
	open(CTGOUT, ">$ctgTmp") ||
	    $base->bail("Cannot open contig temp \"$ctgTmp\" : $!");
	if (! defined $db) {
	    $seq = $1;
	}
	$seq =~ s/-/*/g;
	$contigSeq = $seq;
	$contigLen = length($seq);
#	    print "gotseq ", length($seq), "\n";
	for (my $i = 0; $i < length($seq); $i += 50){
	    print CTGOUT substr($seq, $i, 50), "\n";
	}
	print CTGOUT "\n";
	$nContigs++;
    } 
    
    if (defined $db || ($inAsm && $_ =~ /^quality\t0x(.*)$/)){
	if (! defined $db) {
	    $qual = $1;
	}
	my @qualvals;
	
	if (length($qual) / 2 != $contigLen){
	    $base->bail("difference in qualities contig_len=$contigLen, qual_len = " . length($qual) / 2 );
	}
	
	for (my $i = 0; $i < length($qual); $i += 2){
	    if (substr($contigSeq, $i / 2, 1) ne "*"){
		push(@qualvals, hex(substr($qual, $i, 2)));
	    } else {
		# print "skipping ", $i / 2, "\n";
	    }
	}
#	print "gotqual ", $#qualvals + 1, " ", length($qual), "\n";
	
	print CTGOUT "BQ\n";
	
	@qualvals = map {$_ = (exists $TA2ACEQUAL{$_}) ? 
			     $TA2ACEQUAL{$_} :
				 5} @qualvals;
	
	for (my $i = 0; $i <= $#qualvals; $i += 50){
	    my $end = $i + 49;
	    if ($end > $#qualvals){
		$end = $#qualvals;
	    }
	    print CTGOUT " ", join(" ", @qualvals[$i .. $end]), "\n";
	}
	
	print CTGOUT "\n";
    } 
    if (! defined $db && $inAsm && $_ =~ /^asmbl_id\t(\d+)$/){
	$contigid = $1;
	print STDERR "Doing contig $contigid\n";
    } 
    if (! defined $db && $inAsm && $_ =~ /^seq\#\t(\d+)$/){
	$nseqs = $1;
	$seenseqs = 0;
    } 
    if (defined $db || ($inAsm && $_ =~ /^frameshift/)){
	$inAsm = 0;
	%seqClr = ();
	%seqAlnClr = ();
	%seqAlnRng = ();
	%seqOff = ();
	%rend = ();
	open(SEQOUT, ">$seqTmp") ||
	    $base->bail("Cannot open seq temp \"$seqTmp\" : $!");
    } # ifs...


    my $qh;
    if (defined $db) {
	my $seqQuery = qq~
	    select al.seq_name, al.offset, al.seq_lend, al.seq_rend, al.asm_lend, al.asm_rend, f.end5, f.end3, al.lsequence, s.sequence
	    from asmbl_link al, feature f, sequence s
            where al.asmbl_id = $contigid
              and s.seq_name = al.seq_name
              and f.seq_name = al.seq_name
              and f.feat_type = "CLR"
             order by al.offset
		  ~;

	$qh = $dbproc->prepare($seqQuery) ||
	    $base->bail("Cannot prepare sequence query \"$seqQuery\"");
	
        $qh->execute() ||
	    $base->bail("Cannot execute sequence query \"$seqQuery\"");
    }

    my $step = 1;
    my $lsequence;
    my $allseq;
    while ((defined $db && (my $lineref = $qh->fetchrow_arrayref)) ||
	   (! defined $db && $step > 0)){
	$step = 0;
	
	if (defined $db) {
	    $seqName = $$lineref[0];
	    $seqOff{$seqName} = $$lineref[1] + 1;
	    $rend{$seqName} = $$lineref[1] + 1 + length($$lineref[8]);
	    $seqAlnRng{$seqName} = "$$lineref[4] $$lineref[5]";
	    $al = $$lineref[4];
	    $ar = $$lineref[5];
	    $seqAlnClr{$seqName} = "$$lineref[2] $$lineref[3]";
	    $cll = $$lineref[2];
	    $clr = $$lineref[3];
	    $end5 = $$lineref[6];
	    $end3 = $$lineref[7];
	    $lsequence = $$lineref[8];
	    $allseq = $$lineref[9];
	    $seenseqs++;
	}

	if (! defined $db && ! $inAsm && $_ =~ /^seq_name\t(\S+)$/){
	    $seenseqs++;
	    $seqName = $1;
	} elsif (! defined $db && ! $inAsm && $_ =~ /^asm_lend\t(\d+)$/){
	    $seqAlnRng{$seqName} = $1;
	    $al = $1;
	} elsif (! defined $db && ! $inAsm && $_ =~ /^asm_rend\t(\d+)$/){
	    $seqAlnRng{$seqName} .= " $1";
	    $ar = $1;
	} elsif (! defined $db && ! $inAsm && $_ =~ /^seq_lend\t(\d+)$/){
	    $seqAlnClr{$seqName} = $1;
	    $cll = $1;
	} elsif (! defined $db && ! $inAsm && $_ =~ /^seq_rend\t(\d+)$/){
	    $seqAlnClr{$seqName} .= " $1";
	    $clr = $1;
	} elsif (! defined $db && ! $inAsm && $_ =~ /^offset\t(\d+)$/){
	    $seqOff{$seqName} = $1 + 1;
	}

	if (defined $db || (! $inAsm && $_ =~ /^lsequence\t(.*)$/)){
	    if (! defined $db) {
		$lsequence = $1;
		$rend{$seqName} = $seqOff{$seqName} + length($lsequence);
	    }
	    my $ori = ($cll > $clr) ? "C" : "U";
	    
	    if (! defined $db){
		my $sr = $fr->getRecordByIdentifier($seqName);
		if (! defined $sr){
		    $base->logError("Cannot find record for sequence \"$seqName\"", 1);
		}
		my $head = $sr->getHeader();
		$allseq = $sr->getData();  # the whole sequence
		if ($head =~ /^>\S+\s+\d+\s+\d+\s+\d+\s+(\d+)\s+(\d+)\s*$/){
		    $seqClr{$seqName} = "$1 $2";
		    $end5 = $1;
		    $end3 = $2;
		} else {
		    $base->logError("Cannot find clear range for sequence \"$seqName\"", 1);
		    $end5 = 1;
		    $end3 = length($allseq);
		    $seqClr{$seqName} = "$end5 $end3";
		    $base->logError("Assuming $end5 - $end3\n", 1);
		}
	    } # if ! defined $db
	    
	    if ($ori eq "C") { # make it all forward, we'll complement it again later
		$lsequence = reverseComplement($lsequence);
		my $tmp = $cll;
		$cll = $clr;
		$clr = $tmp;
	    }
	    
#	    print "$seqName $cll $clr\n";
	    
	    my $begin = substr($allseq, 0, $cll - 1);
	    my $end = substr($allseq, $clr);
	    
#	    print length($allseq), " = ", length($begin), " + ", length($lsequence) , " + ", length($end), "\n";
	    
	    my $off = $seqOff{$seqName};
	    if ($ori eq "C") {
		$off -= length($end);
	    } else {
		$off -= length($begin);
	    }

	    print CTGOUT "AF $seqName $ori $off\n";
	    
	    my $outseq = $begin . $lsequence;
	    $end3 = $end3 - $clr + length($outseq); # adjust for gaps in lseq.
	    $clr = length($outseq);
	    $outseq .= $end;
	    
	    
	    # now outseq is an lsequence together with the untrimmed stuff
	    # moreover $clr $cll $end5 and $end3 are all coordinates in the
            # forward strand
	    if ($ori eq "C"){
		$outseq = reverseComplement($outseq);
		$cll = length($outseq) - $cll + 1;
		$clr = length($outseq) - $clr + 1;
		$end5 = length($outseq) - $end5 + 1;
		$end3 = length($outseq) - $end3 + 1;
		
		my $tmp = $cll;
		$cll = $clr;
		$clr = $tmp;

		$tmp = $end5;
		$end5 = $end3;
		$end3 = $tmp;

	    }
	    $nReads++;
	    
	    $outseq =~ s/-/*/g;

	    print SEQOUT "RD $seqName ", length($outseq), " 0 0\n";
	    for (my $i = 0; $i <= length($outseq); $i += 50){
		print SEQOUT substr($outseq, $i, 50), "\n";
	    }
	    print SEQOUT "\n";
	    
	    print SEQOUT "QA $end5 $end3 $cll $clr\n";
	    
	    my $chrmfile = $chromodir . "/$seqName";
	    my $phdfile = $phddir . "/$seqName.phd.1";
	    
	    my $time;
	    
	    if (-r $phdfile){
		$time = `$GREP TIME $phdfile`;
		$time =~ s/TIME: //;
	    }

	    if (! defined $time){
		$base->logError("Cannot stat phd file \"$phdfile\"", 1);
		$time = localtime;
	    } 
	    
	    my $dir = ($ori eq "C") ? "rev" : "forw";

	    print SEQOUT "DS CHROMAT_FILE: $chrmfile PHD_FILE: $phdfile TIME: $time\n"; # CHEM: unknown DYE: big TEMPLATE: $seqName DIRECTION: $dir\n";

	    if ($seenseqs == $nseqs){ # we are at the end of a record
		close(SEQOUT);

		# this is where we deal with BSs
		my $prev;
		my $nBS = 0;
		foreach my $sequence ( sort {($seqOff{$a} == $seqOff{$b}) ? ($rend{$b} <=> $rend{$a}) : ($seqOff{$a} <=> $seqOff{$b})} (keys %seqOff)) {
		    if (defined $prev) {
			if ($seqOff{$sequence} - 1 < $seqOff{$prev} ||
			    $rend{$sequence} < $rend{$prev}){
			    next;
			}
			$nBS++;
			print CTGOUT "BS $seqOff{$prev} ", $seqOff{$sequence} - 1, " $prev\n";
		    }
		    $prev = $sequence;
		}
		$nBS++;
		print CTGOUT "BS $seqOff{$prev} $contigLen $prev\n";
		close(CTGOUT);

		print OUT "CO $contigid $contigLen $nseqs $nBS U\n";
		open(CTGOUT, "$ctgTmp") ||
		    $base->bail("Cannot open \"$ctgTmp\": $!");
		while (<CTGOUT>){
		    print OUT;
		}
		close(CTGOUT);
		print OUT "\n";
		open(SEQOUT, "$seqTmp") ||
		    $base->bail("Cannot open \"$seqTmp\": $!");
		while (<SEQOUT>){
		    print OUT;
		}
		close(SEQOUT);
		print OUT "\n";

		$inAsm = 1; # switch back to assembly mode
		open(CTGOUT, ">$ctgTmp") ||
		    $base->bail("Cannot open contig temp \"$ctgTmp\" : $!");
	    } # if $seenseqs = $nseqs
	} # if lsequence
    } # if $inAsm
} # while (<IN>)

close(CTGOUT);
close(OUT);

# now we need to create the output file and attach to it the temp file.
open(OUT, ">$outfile") ||
    $base->bail("Cannot open output file \"$outfile\": $!");

print OUT "AS $nContigs $nReads\n\n";

open(TMP, "$outTmp") ||
    $base->bail("Cannot open temp file \"$outTmp\" : $!");
while (<TMP>){
    print OUT;
}
close(TMP);

# now print some info
print OUT "WA{\n";
print OUT "TA_convert $VERSION\n";
my $time = `date`;
chomp $time;
print OUT "Run by $ENV{USER} on $time\n";
print OUT "}\n";
close(OUT);

# then remove all the temp files
unlink($outTmp, $seqTmp, $ctgTmp);

exit(0);

sub reverseComplement {
    my($string) = @_;
    my($rev) = "";

    my(%complement) = (
                       'A' => 'T',
                       'T' => 'A',
                       'C' => 'G',
                       'G' => 'C',
                       'U'=>  'A',
                       'M'=>  'K',
                       'R'=>  'Y',
                       'W'=>  'W',
                       'S'=>  'S',
                       'Y'=>  'R',
                       'K'=>  'M',
                       'V'=>  'B',
                       'H'=>  'D',
                       'D'=>  'H',
                       'B'=>  'V',
                       'X'=>  'N',
                       'N'=>  'N',
                       '.'=>  '.',
		       '-'=>  '-',
                       'a' => 't',
                       't' => 'a',
                       'c' => 'g',
                       'g' => 'c',
                       'u'=>  'a',
                       'm'=>  'k',
                       'r'=>  'y',
                       'w'=>  'w',
                       's'=>  's',
                       'y'=>  'r',
                       'k'=>  'm',
                       'v'=>  'b',
                       'h'=>  'd',
                       'd'=>  'h',
                       'b'=>  'v',
                       'x'=>  'n',
                       'n'=>  'n'
                       );

    $string = reverse ($string);

    my ($i);
    for ($i = 0; $i < length($string); $i++){
        substr($string, $i, 1, $complement{substr($string, $i, 1)});
    }

    return $string;
} # reverseComplement
