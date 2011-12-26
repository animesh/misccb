#!/usr/bin/perl

#$Id: parsecasm.pl,v 1.1 2005/07/20 15:22:52 mpop Exp $

use strict;
use Fcntl;
use AMOS::AmosFoundation;
use AMOS::AmosLib;
use IO::File;

my %seql;       # id -> clear range correspondence
my %seqr;
my %seqpos;     # id -> file offset correspondence
my %seqnames;   # id -> seqname correspondence
my @usednames;  # seqnames used in .contig file
my %chosen;     # chosen IDs
my %deleted;    # list of deleted IDs

my $MINSEQSIZE = 64;

my $contigname;
my $tigrasmname;
my $fragfname;
my $fastafname;
my $seqfname;
my $qualfname;
my $opref;
my $frgStore;
my $gkpStore;

my @offsets;    # offset -> asm_lend, asm_rend correspondence

my $version = '$Revision: 1.1 $ ';
my $helptext = qq~
    parsecasm [options] file.asm [file1.frg file2.frg ...]

    converts CA output file.asm into file.fasta, file.contig. A file
    named file.frg must be present in the current directory,
    alternatively, a list of .frg files must be provided on the
    command line.

  OPTIONS
    -nofasta        Do not create .fasta output
    -justfasta      Only create .fasta output
    -o prefix       Output prefix
    -nonames        Uses Ids rather than trying to figure out seqnames
    ~;

my $base = new AMOS::AmosFoundation;

if (! defined $base){
    print STDERR "the sky is falling, run away!\n";
    exit(1);
}

$base->setHelpText($helptext);
$base->setUsage("parsecasm [options] file.asm [file1.frg file2.frg ...]");
$version =~ s/\$//g;
$base->setVersion($version);

my $ischeck = 0;
my $nofasta;
my $justfasta;
my $noqual;
my $filter;
my $getmates;
my %accname;        # correspondence between accessions and seqnames
my %meandist;       # library sizes
my %stdevdist; 
my %uid2iid;
my $nonames;

my $err = $base->getOptions("nofasta"   => \$nofasta,
				 "justfasta" => \$justfasta,
				 "o=s"       => \$opref,
				 "nonames"   => \$nonames);
if ($err == 0){
    $base->bail("Command line parsing failed.  See -h option");
}

open(IN, "$ARGV[0]") ||
    $base->bail("Cannot open \"$ARGV[0]\": $!\n");

$ARGV[0] =~ /(\S+)\.asm/;

if (! defined $1){
    $base->bail("\"$ARGV[0]\" not recognized, must have extension .asm\n");
}
    
if (!defined $opref){
    $opref = $1;
}

my @frags;
my @frgFiles;

if ($#ARGV == 0){
    $frags[0] = "$1.frg";
} else {
    @frags = @ARGV[1 .. $#ARGV];
}

if (! defined $justfasta){
    my $nerr = 0;
    for (my $i = 0; $i <= $#frags; $i++){
	if (! -e $frags[$i]){
	    $nerr++;
	    print STDERR "file $frags[$i] doesn't exists!\n";
	}
    }
    if ($nerr != 0){
	$base->bail("One or more frg files could not be found\n");
    }
    
    
    for (my $i = 0; $i <= $#frags; $i++){
	$frgFiles[$i] = new IO::File;
	if (! $frgFiles[$i]->open($frags[$i])){
	    $nerr++;
	    print STDERR "file $frags[$i] cannot be opened: $! \n";
	}
    }
    if ($nerr != 0){
	$base->bail("One or more frg files could not be opened\n");
    }
}

$contigname = "$opref.placed.contig";
my $surrogatename = "$opref.surrogate.contig";
my $degenname = "$opref.degen.contig";
$fastafname = "$opref.placed.fasta";
my $fastasurrname = "$opref.surrogate.fasta";
my $fastadegenname = "$opref.degen.fasta";
my $fastaqname = "$opref.placed.fasta.qual";
my $fastasurrqname = "$opref.surrogate.fasta.qual";
my $fastadegenqname = "$opref.degen.fasta.qual";
my $libname = "$opref.libs";

if (!defined $justfasta) {
    open(OUT, ">$contigname") ||
	$base->bail("Cannot open \"$contigname\": $!\n");
    open(DEG, ">$degenname") ||
	$base->bail("Cannot open $degenname: $!\n");
    open(SUR, ">$surrogatename") ||
	$base->bail("Cannot open $surrogatename: $!\n");
    open(QUAL, ">$fastaqname") ||
	$base->bail("Cannot open \"$fastaqname\": $!\n");
    open(QUALSUR, ">$fastasurrqname") ||
	$base->bail("Cannot open \"$fastasurrqname\": $!\n");
    open(QUALDEG, ">$fastadegenqname") ||
	$base->bail("Cannot open \"$fastadegenqname\": $!\n");
    open(LIB, ">$libname") ||
	$base->bail("Cannot open \"$libname\": $!\n");
}

if (!defined $nofasta){
    open(FASTA, ">$fastafname") ||
	$base->bail("Cannot open \"$fastafname\": $!\n");
    open(FASTASUR, ">$fastasurrname") ||
	$base->bail("Cannot open \"$fastasurrname\": $!\n");
    open(FASTADEG, ">$fastadegenname") ||
	$base->bail("Cannot open \"$fastadegenname\": $!\n");
}

my %seekfile;
my %stdevs;
my %means;

# get the seek positions within the FRG file
if (! defined $justfasta){
    for (my $i = 0; $i <= $#frgFiles; $i++){
	my $seekpos = tell $frgFiles[$i];
	while (my $record = getRecord($frgFiles[$i])){
	    my ($rec, $fields, $recs) = parseRecord($record);
	    if ($rec eq "FRG"){
#		print "setting $$fields{acc} to $seekpos\n";
		$seqpos{$$fields{acc}} = $seekpos;
		$seekfile{$$fields{acc}} = $i;
		my $nm = $$fields{src};
		my @lines = split('\n', $nm);
		$nm = join(' ', @lines);
		if ($nm eq "" || $nm =~ /^\s*$/){
		    $nm = "$$fields{acc}";
		}
		$seqnames{$$fields{acc}} = $nm;
	    }
	    if ($rec eq "DST"){
		$stdevs{$$fields{acc}} = $$fields{std};
		$means{$$fields{acc}} = $$fields{mea};
	    }
	    $seekpos = tell $frgFiles[$i];
	} # for each record in FRG
    } # for each input FRG file
} # if ! defined $justfasta

my $record = "";
my $first = 1;
my $number = 0;

while ($record = getRecord(\*IN)){
    my ($rec, $fields, $recs) = parseRecord($record);

    if ($rec eq "MDI"){ # mate distance
	my $id = getCAId($$fields{ref});

	if (! defined $justfasta){
	    print LIB "Distance $id changed from ($means{$id}, $stdevs{$id}) to ($$fields{mea}, $$fields{std})\n\tmin = $$fields{min}, max= $$fields{max}\n" if ($$fields{buc} != 0);
	    print LIB "Distance $id unchanged from ($means{$id}, $stdevs{$id})\n" if ($$fields{buc} == 0);
	}
	next;
    } # mate distance
	
    if ($rec eq "AFG"){ # augmented fragment
	if ($#$recs != -1){
	    print "Fragment ", getCAId($$fields{acc}), 
	    " matches ", $#$recs + 1, " screens\n";
	}
	
        # convert the external ids into internal ids
	$$fields{acc} =~ /\((\d+),(\d+)\)/;
	if (!defined $1){
	    $base->bail("Can't understand accession \"$$fields{acc}\"");
	}
	$uid2iid{$1} = $2;

        # get the new clear range
	$$fields{clr} =~ /(\d+),(\d+)/;
	if (!defined $1){
	    print STDERR "Weird clear range for fragment, ", getCAId($$fields{acc}), "\n";
	    next;
	}
	$seql{getCAId($$fields{acc})} = $1;
	$seqr{getCAId($$fields{acc})} = $2;
	next;
    } # augmented fragment
    
    if ($rec eq "UTG"){ # unitig record
	if (defined $justfasta){
	    next;
	}
	if ($$fields{"sta"} ne "S"){
	    # we only report the Separable repeats in the output
	    next;
	}

	@offsets = ();
	my $id = getCAId($$fields{acc});

	my $len = $$fields{len};
	my $lseq = $$fields{cns};
	my $nseq = $$fields{nfr};
	my $qualities = $$fields{qlt};

	my @lines = split('\n', $qualities);
        $qualities = join('', @lines);
        my $quals = "";#sprintf("%02d", ord(substr($qualities, 0, 1)) - ord('0'));
	my @fields = split('\n', $lseq);
	$lseq = join('', @fields);

        for (my $c = 0; $c < length($qualities); $c++){
	    if (substr($lseq, $c, 1) ne '-'){
		$quals .= sprintf(" %02d", 
				  ord(substr($qualities, $c, 1)) - ord('0'));
	    }
        }


	if (length($lseq) == 0){
	    next;
	}

	if (! defined $justfasta){
	    print_contig(\*SUR, $id, $len, $nseq, $lseq, "contig");
	    printFastaQual(\*QUALSUR, $id, $quals);
	}
	if (! defined $nofasta){
	    print_contig(\*FASTASUR, $id, $len, $nseq, $lseq, "fasta");
	}

# here the offsets array will contain one entry for each position
# in the consensus ($lseq).  That entry is the number of non-gap
# characters in the consensus, up to the current coordinate.
# 
# these numbers follow the asm_lend, asm_rend convention and start at 1.
	$#offsets = length($lseq) - 1;
	my $coord = 0;
	for (my $i = 0; $i < length($lseq); $i++){
	    if (substr($lseq, $i, 1) ne "-"){
		$coord++;
	    }
	    $offsets[$i] = $coord;
	}
	
# here we parse the individual sequences aligned to the contig
	for (my $i = 0; $i <= $#$recs; $i++){
	    my ($sid, $sfs, $srecs) = parseRecord($$recs[$i]);

	    if ($sid eq "MPS"){
		if ($$sfs{typ} ne "R"){
		    print STDERR "weird type $$sfs{typ}\n";
		    next;
		}
	    } else {
		next;
	    }

	    my ($id, $sequence, $asml, $asmr, $rc, $seqleft, $seqright) = 
		deal_with_mps($sfs);

	    if (! defined $justfasta) {
		print_aligned(\*SUR, $seqnames{$id}, $sequence, $asml, $rc, 
			      $seqleft, $seqright, 
			      $offsets[$asml], $offsets[$asmr], "contig");
	    }
	    $usednames[++$#usednames] = $seqnames{$id};
	}
	next;
	
    } # unitig

    if ($rec eq "CCO"){ # contig record
	@offsets = ();
	my $id = getCAId($$fields{acc});
	my $len = $$fields{len};
	my $lseq = $$fields{cns};
	my $nseq = $$fields{npc};
	my $qualities = $$fields{qlt};

	my @lines = split('\n', $qualities);
        $qualities = join('', @lines);
        my $quals = "";#sprintf("%02d", ord(substr($qualities, 0, 1)) - ord('0'));
	my @fields = split('\n', $lseq);
	$lseq = join('', @fields);

        for (my $c = 0; $c < length($qualities); $c++){
	    if (substr($lseq, $c, 1) ne '-'){
		$quals .= sprintf(" %02d", 
				  ord(substr($qualities, $c, 1)) - ord('0'));
	    }
        }

	if (length($lseq) == 0){
	    next;
	}

	if ($$fields{pla} eq "P") { # placed contig
	    if (!defined $nofasta){
		print_contig(\*FASTA, $id, $len, $nseq, $lseq, "fasta");
		printFastaQual(\*QUAL, $id, $quals);
	    }
	    if (!defined $justfasta){
		print_contig(\*OUT, $id, $len, $nseq, $lseq, "contig");
	    }
	} else {
	    if (!defined $nofasta){
		print_contig(\*FASTADEG, $id, $len, $nseq, $lseq, "fasta");
		printFastaQual(\*QUALDEG, $id, $quals);
	    }
	    if (!defined $justfasta){
		print_contig(\*DEG, $id, $len, $nseq, $lseq, "contig");
	    }
	}	    
# here the offsets array will contain one entry for each position
# in the consensus ($lseq).  That entry is the number of non-gap
# characters in the consensus, up to the current coordinate.
# 
# these numbers follow the asm_lend, asm_rend convention and start at 1.
	$#offsets = length($lseq) - 1;
	my $coord = 0;
	for (my $i = 0; $i < length($lseq); $i++){
	    if (substr($lseq, $i, 1) ne "-"){
		$coord++;
	    }
	    $offsets[$i] = $coord;
	}
	
# here we parse the individual sequences aligned to the contig
	if (!defined $justfasta){
	    for (my $i = 0; $i <= $#$recs; $i++){
		my ($sid, $sfs, $srecs) = parseRecord($$recs[$i]);
		
		if ($sid eq "MPS"){
		    if ($$sfs{typ} ne "R"){
			print STDERR "weird type $$sfs{typ}\n";
			next;
		    }
		} else {
		    next;
		}
		
		my ($id, $sequence, $asml, $asmr, $rc, $seqleft, $seqright) = 
		    deal_with_mps($sfs);
		
		if ($$fields{pla} eq "P"){
		    print_aligned(\*OUT, $seqnames{$id}, $sequence, $asml, $rc,
				  $seqleft, $seqright, 
				  $offsets[$asml], $offsets[$asmr], "contig");
		} else {
		    print_aligned(\*DEG, $seqnames{$id}, $sequence, $asml, $rc,
				  $seqleft, $seqright, 
				  $offsets[$asml], $offsets[$asmr], "contig");
		} 
		$usednames[++$#usednames] = $seqnames{$id};
	    } # for $i = 0...
	}
	next;
    } # contig
} # while getRecord

if (!defined $nofasta){
    close(FASTA);
    close(FASTADEG);
    close(FASTASUR);
}

if (! defined $justfasta){
    close(LIB);
    close(QUAL);
    close(QUALDEG);
    close(QUALSUR);
    for (my $i = 0; $i <= $#frgFiles; $i++){
	$frgFiles[$i]->close();
    }
    close(OUT);
    close(DEG);
    close(SUR);
}

close(IN);

# The end
exit(0);


########################################## 
# this tries to abstract out the section on dealing with the MPS
#records that is used in both the CCO and the UTG records
sub deal_with_mps 
{
    my $sfs = shift;
    
		
    my $id = getCAId($$sfs{mid});
    my $asms = $$sfs{pos};
    $asms =~ /(\d+),(\d+)/;
    if (! defined $1){
	die ("weird pos record: $$sfs{pos}\n");
    }
    my $asml = 0;
    my $asmr = 0;
    if ($1 > $2){
	$asml = $1 - 1;
	$asmr = $2;
    } else {
	$asml = $1;
	$asmr = $2 - 1;
    }
    
    my $gapno = $$sfs{dln};
    my $gaps = $$sfs{del};
    my @gaps = split(' ', $gaps);
    if ($gapno != $#gaps + 1){
	print STDERR "weird, $gapno != ", $#gaps + 1, "\n";
    }
    
    my $sequence = get_seq($frgFiles[$seekfile{$id}], $id);
    
    my @lines = split('\n', $sequence);
    $sequence = join('', @lines);
    $sequence = uc($sequence);
    
    $sequence = substr($sequence, $seql{$id}, $seqr{$id} - $seql{$id});

    my $seqleft = $seql{$id} + 1;
    my $seqright = $seqr{$id};
    
    my $rc = "";
    # sequence is reverse complemented
    if ($asml > $asmr){
	$sequence  = reverseComplement($sequence);
	my $tmp = $asmr;
	$asmr = $asml;
	$asml = $tmp;
	
	$tmp = $seqright;
	$seqright = $seqleft;
	$seqleft = $tmp;
	$rc = "RC";
    }
    
    # now we add in the gaps and print the sequence
    my $outseq = "";
    my $gapindex = 0;
    for (my $j = 0; $j < length($sequence); $j++){
	my $seqj = $j;# + $seql{$id} - 1; # index in untrimmed sequence
	if ($gapindex <= $#gaps && $seqj > $gaps[$gapindex]){
	    print STDERR "Weird $seqnames{$id}, $seqj > $gaps[$gapindex]\n";
	}
	# this here is a fix for cases when the last gap index 
	# is equal to the length of the sequence.  In this case
	# the sequence gets an extra gap at the very end (which
	# I might add, is completely stupid).
	while ($gapindex <= $#gaps && $seqj == $gaps[$gapindex]){
#			print "GS $gapindex $#gaps $seqj $gaps[$gapindex] ",
	    length($sequence), "\n";
	    $outseq .= "-";
	    $gapindex++;
#			print "GE $gapindex\n";
	}
	$outseq .= substr($sequence, $j, 1);
    }
    
    if ($gapindex == $#gaps && length($sequence) == $gaps[$gapindex]){
	$outseq .= "-";
    } elsif ($gapindex < $#gaps){
	print STDERR "Where are all the gaps $seqnames{$id}: $gapindex, $#gaps\n";
    }
    
    $sequence = $outseq;

    return ($id, $sequence, $asml, $asmr, $rc, $seqleft, $seqright);
} # deal_with_mps

# prints contig in specified format
sub print_contig
{
    my $file = shift;
    my $id = shift;
    my $len = shift;
    my $nseq = shift;
    my $sequence = shift;
    my $how = shift;
    my $type = shift;

    if (length($sequence) == 0){
	return;
    }

    if ($how eq "contig"){
	print $file "\#\#$id $nseq $len bases, 00000000 checksum.\n";
	print_sequence($file, $sequence);
	return;
    } # if $how eq "contig"
    elsif ($how eq "asm"){
	my $strip = $sequence;
	$strip =~ s/-//g;
# get the current date
	my $date = `date +'%D %T'`;
	chomp $date;

	my $quality = "0x";
	for (my $i = 0; $i < length($sequence); $i++){
	    $quality .= "06";
	}

	print $file "sequence\t$strip\n";
	print $file "lsequence\t$sequence\n";
	print $file "quality\t$quality\n";
	print $file "asmbl_id\t$id\n";
	print $file "seq_id\t\n";
	print $file "com_name\t\n";
	print $file "type\t\n";
	print $file "method\tCelera Assembler\n";
	print $file "ed_status\t\n";
	print $file "redundancy\t\n";
	print $file "perc_N\t\n";
	print $file "seq\#\t$nseq\n";
	print $file "full_cds\t\n";
	print $file "cds_start\t\n";
	print $file "cds_end\t\n";
	print $file "ed_pn\t$ENV{USER}\@$ENV{HOSTNAME}\n";
	print $file "ed_date\t$date\n";
	print $file "comment\t$type ID: $id\n";
	print $file "frameshift\t\n";
	return;
    } # if $how eq "asm"
    elsif ($how eq "fasta")
    {
	print $file ">$id $nseq $len bases\n";
	$sequence =~ s/-//g;  # get rid of all gaps
	print_sequence($file, $sequence);
	return;
    }
} # print_contig

sub print_aligned
{
    my($file, $name, $seq, $offset, $rc, 
       $seqleft, $seqright, $asml, $asmr, $type) = @_;

    if ($type eq "contig"){
	print $file "\#$name($offset) [$rc] ", 
	length($seq), 
	" bases, 00000000 checksum. {$seqleft $seqright} <$asml $asmr>\n";
	
	print_sequence($file, $seq);
    }

    if ($type eq "asm"){
	print $file "\n";
	print $file "seq_name\t$name\n";
	print $file "asm_lend\t$asml\n";
	print $file "asm_rend\t$asmr\n";
	print $file "seq_lend\t$seqleft\n";
	print $file "seq_rend\t$seqright\n";
	print $file "best\t\n";
	print $file "comment\t\n";
	print $file "db\t\n";
	print $file "offset\t$offset\n";
	print $file "lsequence\t$seq\n";
    }

    return;
}

# prints sequence in specified format
sub print_sequence
{
    my $file = shift;
    my $seqs = shift;

    for (my $j = 0; $j < length($seqs); $j += 60){
        print $file substr($seqs, $j, 60), "\n";
    }
} # print_sequence

# gets a sequence from the specified file depending on the seqpos array
sub get_seq
{
    my $file = shift;
    my $id = shift;

    seek $file, $seqpos{$id}, 0; # seek set
    my $record = getRecord($file);
    if (! defined $record){
	print STDERR "weird error - no record found\n";
	return;
    }

    my ($rec, $fields, $recs) = parseRecord($record);
    
    if ($rec ne "FRG"){
	print STDERR "weird error in get_seq, expecting frg\n";
	return;
    }
    if ($$fields{acc} != $id){
	print STDERR "weird error in get_seq, expecting $id, got $$fields{acc}\n";
	return;
    }
    return $$fields{seq};
}
