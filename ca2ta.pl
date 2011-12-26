#!/usr/local/bin/perl

use strict;
use Fcntl;
use TIGR::Foundation;
use AMOS::AmosLib;
use IO::Handle;

my $FILEBUF;
my %seql;       # id -> clear range correspondence
my %seqr;
my %seqpos;     # id -> file offset correspondence
my %namepos;    # name -> file offset correspondence
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
my $cfn;
my $opref;
my $frgStore;
my $gkpStore;

my @offsets;    # offset -> asm_lend, asm_rend correspondence

my $version = '$Revision: 1.7 $ ';
my $helptext = qq~
.USAGE.
  ca2ta [options] file.asm

.DESCRIPTION.
  Converts CA output file.asm into file.fasta, file.contig, file.tasm similar
  to the corresponding outputs of TA.  A file named file.frg must be present
  in the current directory. 
  
.OPTIONS.
  -select fname   Only performs this operation for contigs and unitigs
                  specified in fname
  -contigs        Does not report unitigs in .tasm
  -nofasta        Do not create .fasta output
  -justfasta      Only create .fasta output
  -o prefix       Output prefix
  -nonames        Uses Ids rather than trying to figure out seqnames


  ca2ta [options] file.frg
  converts CA input file.frg into file.seq, file.qual similar to
  inputs for TA

  -check          Prints out those seqnames that are shorter than MINSEQ
  -minseq         sets MINSEQ.  Default is 64
  -o prefix       Output prefix
  -filter         filters the .frg file for sequences shorter than MINSEQ
  -noqual         doesnt create .qual file
  -mates          creates a .mates file indicating which sequences are mates

.KEYWORDS.
  converter, Celera, TIGR assembler
~;

my $base = new TIGR::Foundation;

if (! defined $base){
    print STDERR "the sky is falling, run away!\n";
    exit(1);
}

$base->setHelpInfo($helptext);
$version =~ s/\$//g;
$base->setVersionInfo($version);

my $frgInfo = "/local/asmg/bin/CA/bin/get_frag_info";
$base->addDependInfo($frgInfo);

my $ischeck = 0;
my $contigsonly;
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

my $err = $base->TIGR_GetOptions("select=s" => \$cfn,
				 "contigs"  => \$contigsonly,
				 "check"    => \$ischeck,
				 "minseq=i" => \$MINSEQSIZE,
				 "nofasta"  => \$nofasta,
				 "justfasta" => \$justfasta,
				 "o=s"      => \$opref,
				 "filter"   => \$filter,
				 "noqual"   => \$noqual,
                                 "mates"    => \$getmates,
				 "nonames"  => \$nonames);
if ($err == 0){
    $base->bail("Command line parsing failed.  See -h option");
}

if (defined $cfn){
    open (CH, "$cfn") || 
	$base->bail("Cannot open \"$cfn\": $!\n");

    while (<CH>){
	chomp;
	$chosen{$_} = 1;
    }
    close(CH);
}
my $fh = new IO::Handle;

open(IN, "$ARGV[0]") ||
    $base->bail("Cannot open \"$ARGV[0]\": $!\n");

$fh->fdopen(fileno(IN), "r") || $base->bail("Cannot open IN: $!\n");
$fh->setvbuf($FILEBUF, IO::Handle::_IOFBF, 102400); # set a 100 k buffer

my $isfrag = 0;
if ($ARGV[0] =~ /(\S+)\.frg/){
    if (!defined $opref){
	$opref = $1;
    }
    $isfrag = 1;

    if (defined $filter){
	if ($opref eq $1){
	    $base->bail("You must choose a different output name with -o\n");
	}
	open(NEWFRG, ">$opref.frg") ||
	    $base->bail("Cannot open \"$opref.frg\": $!\n");
    }

    if ($ischeck != 1){
	open (SEQ, ">$opref.seq") ||
	    $base->bail("Cannot open \"$opref.seq\": $!\n");
	if (! defined $noqual){
	    open (QUAL, ">$opref.qual") ||
		$base->bail("Cannot open \"$opref.qual\": $!\n");
	}
        if (defined $getmates){
	    open (MATES, ">$opref.mates") ||
		$base->bail("Cannot open \"$opref.mates\": $!\n");
	}
    }
} else {
    $ARGV[0] =~ /(\S+)\.asm/;
    if (! defined $1){
	$base->bail("\"$ARGV[0]\" not recognized, must have extension .asm or .frg\n");
    }

#    $frgStore = "$1.frgStore";
#    if (! -d $frgStore){
#	$base->bail("Cannot find fragment store \"$frgStore\"");
#    }
#    $gkpStore = "$1.gkpStore";
#    if (! -d $gkpStore){
#	$base->bail("Cannot find gatekeeper store \"$gkpStore\"");
#    }
    
    if (!defined $opref){
	$opref = $1;
    }
    $contigname = "$opref.contig";
    $tigrasmname = "$opref.tasm";
    $fragfname = "$1.frg";
    $fastafname = "$opref.fasta";
    $seqfname = "$opref.seq";
    $qualfname = "$opref.qual";
    if (!defined $justfasta) {
	open(OUT, ">$contigname") ||
	    $base->bail("Cannot open \"$contigname\": $!\n");
	open(TASM, ">$tigrasmname")||
	    $base->bail("Cannot open \"$tigrasmname\": $!\n");
	open(FRG, "$fragfname") ||
	    $base->bail("Cannot open \"$fragfname\": $!\n");
    }
    if (!defined $nofasta){
	open(FASTA, "> $fastafname") ||
	    $base->bail("Cannot open \"$fastafname\": $!\n");
    }
    if (defined $cfn){
	open(SEQ, ">$seqfname") ||
	    $base->bail("Cannot open \"$seqfname\": $!\n");
	if (!defined $noqual){
	    open(QUAL, ">$qualfname") ||
		$base->bail("Cannot open \"$qualfname\": $!\n");
	}
    }
    
    # get the seek positions within the FRG file
    if (! defined $justfasta){
	my $seekpos = tell FRG;
  	while (my $record = getRecord(\*FRG)){
  	    my ($rec, $fields, $recs) = parseRecord($record);
  	    if ($rec eq "FRG"){
  		# print "setting $$fields{acc} to $seekpos\n";
  		$seqpos{$$fields{acc}} = $seekpos;
  		my $nm = $$fields{src};
  		my @lines = split('\n', $nm);
  		$nm = join(' ', @lines);
		if ($nm eq "" || $nm =~ /^\s*$/){
		    $nm = "$$fields{acc}";
		}
  		$seqnames{$$fields{acc}} = $nm;
  		$namepos{$nm} = $seekpos;
  	    }
  	    $seekpos = tell FRG;
  	}
    } # if ! defined $justfasta
}

my $record = "";
my $first = 1;
my $number = 0;

while ($record = getRecord(\*IN)){
    my ($rec, $fields, $recs) = parseRecord($record);

    if ($rec eq "ADT"){ # audit
	if (defined $filter){
	    print NEWFRG $record;
	}
	next;
    }  # audit

    if ($rec eq "MDI"){ # mate distance
	if (defined $filter){
	    print NEWFRG $record;
	}
	print "Distance id: $$fields{ref} revised to \n",
	"mean: $$fields{mea}, stddev: $$fields{std}, min: $$fields{min}, ",
	"max: $$fields{max}\n";
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
	    print STDERR "Wierd clear range for fragment, ", getCAId($$fields{acc}), "\n";
	    next;
	}
	$seql{getCAId($$fields{acc})} = $1;
	$seqr{getCAId($$fields{acc})} = $2;
	next;
    } # augmented fragment
    
    if ($rec eq "UTG"){ # unitig record
	if (defined $contigsonly || defined $justfasta){
	    next;
	}
	if ($$fields{"sta"} ne "S"){
	    # we only report the Separable repeats in the output
	    # and only in the .asm file.
	    next;
	}
	@offsets = ();
	my $id = getCAId($$fields{acc});

	if (defined $cfn && (! exists($chosen{$id}))){
	    next;
	}

	my $len = $$fields{len};
	my $lseq = $$fields{cns};
	my $nseq = $$fields{nfr};

	my @fields = split('\n', $lseq);
	$lseq = join('', @fields);

	if (length($lseq) == 0){
	    next;
	}

	if ($first != 1){
	    print TASM "|\n";
	}
	$first = 0;
	print_contig(\*TASM, $id, $len, $nseq, $lseq, "asm", "CA_FREE");
	if (defined $cfn){
	    print_contig(\*OUT, $id, $len, $nseq, $lseq, "contig");
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
		    print STDERR "wierd type $$sfs{typ}\n";
		    next;
		}
	    } else {
		next;
	    }

	    my ($id, $sequence, $asml, $asmr, $rc, $seqleft, $seqright) = 
		deal_with_mps($sfs);

	    print_aligned(\*TASM, $seqnames{$id}, $sequence, $asml, $rc, 
			  $seqleft, $seqright, 
			  $offsets[$asml], $offsets[$asmr], "asm");
	    if (defined $cfn){
		print_aligned(\*OUT, $seqnames{$id}, $sequence, $asml, $rc, 
			      $seqleft, $seqright, 
			      $offsets[$asml], $offsets[$asmr], "contig");
		$usednames[++$#usednames] = $seqnames{$id};
	    }		    
	}
	next;
	
    } # unitig

    if ($rec eq "ULK"){ # unitig link
	# don't need to worry about it now
	next;
    } # unitig link

    if ($rec eq "CCO"){ # contig record
	@offsets = ();
	my $id = getCAId($$fields{acc});

	if (defined $cfn && (! exists ($chosen{$id}))){
	    next;
	}

	my $len = $$fields{len};
	my $lseq = $$fields{cns};
	my $nseq = $$fields{npc};
	
	my @fields = split('\n', $lseq);
	$lseq = join('', @fields);

	if (length($lseq) == 0){
	    next;
	}

	if ($first != 1 && !defined $justfasta){
	    print TASM "|\n";
	}
	$first = 0;
	if (!defined $nofasta){
	    print_contig(\*FASTA, $id, $len, $nseq, $lseq, "fasta");
	}
	if (!defined $justfasta){
	    print_contig(\*TASM, $id, $len, $nseq, $lseq, "asm", "CA_CONTIG");
	    print_contig(\*OUT, $id, $len, $nseq, $lseq, "contig");
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
			print STDERR "wierd type $$sfs{typ}\n";
			next;
		    }
		} else {
		    next;
		}
		
		my ($id, $sequence, $asml, $asmr, $rc, $seqleft, $seqright) = 
		    deal_with_mps($sfs);
		
		print_aligned(\*OUT, $seqnames{$id}, $sequence, $asml, $rc, 
			      $seqleft, $seqright, 
			      $offsets[$asml], $offsets[$asmr], "contig");
		print_aligned(\*TASM, $seqnames{$id}, $sequence, $asml, $rc, 
			      $seqleft, $seqright, 
			      $offsets[$asml], $offsets[$asmr], "asm");
		$usednames[++$#usednames] = $seqnames{$id};
		
	    } # for $i = 0...
	}
	next;
    } # contig

    if ($rec eq "CLK") { # contig link
	# this will generate a group.xml file so we'll ignore it for now
	next;
    } # contig link

    if ($rec eq "FRG") { # fragment info
	my $sequence = $$fields{seq};
	my $qualities = $$fields{qlt};
	my $clear = $$fields{clr};
	my $seqname = $$fields{src};
	my $id = getCAId($$fields{acc});

	my @lines = split('\n', $sequence);
	$sequence = join('', @lines);
	@lines = split('\n', $qualities);
	$qualities = join('', @lines);
	@lines = split('\n', $seqname);
	$seqname = $lines[0];
	if ($seqname eq "" ||
	    $seqname =~ /^\s*$/){
#	    $seqname = ++$number;
	    $seqname = "$id";
	}
	if (defined $nonames){
	    $accname{$id} = $id;
	} else {
	    $accname{$id} = $seqname;
	}
	$clear =~ /(\d+),(\d+)/;

	if (defined $filter){
	    if ($2 - $1 < $MINSEQSIZE){
		$deleted{$id} = 1;
		next;
	    } else {
		print NEWFRG $record;
		next;
	    }
	}
	    
	if ($ischeck == 1){
	    if ($2 - $1 < $MINSEQSIZE){
		print "$seqname ", $2 - $1, " < $MINSEQSIZE\n";
	    }
	    next;
	}
	my $cll = $1 + 1; 
	my $clr = $2;

	my $quals = sprintf("%02d", ord(substr($qualities, 0, 1)) - ord('0'));
	for (my $c = 1; $c < length($qualities); $c++){
	    $quals .= sprintf(" %02d", ord(substr($qualities, $c, 1)) - ord('0'));
	}
	
	printFastaSequence(\*SEQ, "$seqname 0 0 0 $cll $clr", uc($sequence));
	if (!defined $noqual){
	    printFastaQual(\*QUAL, "$seqname", $quals);
	}
			    
	next;
    }

    if ($rec eq "LKG") {
	if (defined $getmates){
	    print MATES "$accname{$$fields{fg1}} $accname{$$fields{fg2}} $meandist{$$fields{dst}} $stdevdist{$$fields{dst}}\n";
	}
	if (defined $filter){
	    if (exists $deleted{$$fields{fg1}} ||
		exists $deleted{$$fields{fg2}}){
		next;
	    } else {
		print NEWFRG $record;
		next;
	    }
	}
	next;
    }

    if ($rec eq "DST"){
	if (defined $getmates){
	    $meandist{$$fields{acc}} = $$fields{mea};
	    $stdevdist{$$fields{acc}} = $$fields{std};
	}
	if (defined $filter){
	    print NEWFRG $record;
	}
	next;
    }

    if (defined $filter){
	print NEWFRG $record;
    }


    print STDERR "$rec ($$fields{acc}) is not currently parsed and has $#$recs sub-records\n";
}

if (defined $cfn){
    # now we create .seq and .qual files
    for (my $i = 0; $i <= $#usednames; $i++){
	print_seq_for_name(\*FRG , $usednames[$i], \*SEQ, \*QUAL);
    }

    close(SEQ);
    if (!defined $noqual){
	close(QUAL);
    }
    if (defined $getmates){
	close(MATES);
    }
} 

if (defined $filter){
    close(NEWFRG);
}

if ($isfrag == 0){
    if (!defined $nofasta){
	close(FASTA);
    }
    close(FRG);
    close(OUT);
    close(TASM);
} elsif ($ischeck == 0){
    close (SEQ);
    if (!defined $noqual){
	close (QUAL);
    }
    if (defined $getmates){
	close (MATES);
    }
}

close(IN);

# The end
exit(0);


##########################################

# this is not truly a function.  It tries to abstract 
# out the section on dealing with the MPS records that 
# is used in both the CCO and the UTG records
sub deal_with_mps 
{
    my $sfs = shift;
    
		
    my $id = getCAId($$sfs{mid});
    my $asms = $$sfs{pos};
    $asms =~ /(\d+),(\d+)/;
    if (! defined $1){
	die ("wierd pos record: $$sfs{pos}\n");
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
	print STDERR "wierd, $gapno != ", $#gaps + 1, "\n";
    }
    
    #	print "seql = $seql{$id}, seqr = $seqr{$id}\n";
	
      my $sequence = get_seq(\*FRG, $id);
    
    
      my @lines = split('\n', $sequence);
      $sequence = join('', @lines);
      $sequence = uc($sequence);
    
      $sequence = substr($sequence, $seql{$id}, $seqr{$id} - $seql{$id});

#    my $sequence = getStoreSeq($id);

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
	    print STDERR "Wierd $seqnames{$id}, $seqj > $gaps[$gapindex]\n";
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
	print "wierd error\n";
	return;
    }

    my ($rec, $fields, $recs) = parseRecord($record);
    
    if ($rec ne "FRG"){
	print STDERR "wierd error in get_seq, expecting frg\n";
	return;
    }
    if ($$fields{acc} != $id){
	print STDERR "wierd error in get_seq, expecting $id, got $$fields{acc}\n";
	return;
    }
    return $$fields{seq};
}

# gets the seq and qual records for the seqname given as parameter and 
# prints them in the .seq and .qual files.
sub print_seq_for_name {
    my $file = shift;
    my $nm = shift;
    my $seqfile = shift;
    my $qualfile = shift;

    seek $file, $namepos{$nm}, 0;
    my $record = getRecord($file);
    if (!defined $record){
	print STDERR "record for name \"$nm\" not found in file\n";
	return;
    }
    my ($rec, $fields, $recs) = parseRecord($record);
    if ($rec ne "FRG"){
	print STDERR "expecting FRG record for name \"$nm\"\n";
	return;
    }

    my $sequence = $$fields{seq};
    my $qualities = $$fields{qlt};
    my $clear = $$fields{clr};
    my $seqname = $$fields{src};

    my @lines = split('\n', $sequence);
    $sequence = join('', @lines);
    @lines = split('\n', $qualities);
    $qualities = join('', @lines);
    @lines = split('\n', $seqname);
    $seqname = $lines[0];
    $clear =~ /(\d+),(\d+)/;

    my $cll = $1 + 1; 
    my $clr = $2;

    my $quals = sprintf("%02d", ord(substr($qualities, 0, 1)) - ord('0'));
    for (my $c = 1; $c < length($qualities); $c++){
	$quals .= sprintf(" %02d", ord(substr($qualities, $c, 1)) - ord('0'));
    }
	
    printFastaSequence($seqfile, "$seqname 0 0 0 $cll $clr", uc($sequence));
    if (!defined $noqual){
	printFastaQual($qualfile, "$seqname", $quals);
    }
} # sub print_seq_for_name

# gets a sequence from the stores
sub getStoreSeq {
    my $uid = shift;

    my $iid = $uid2iid{$uid};

    my $infoPrg = "$frgInfo $frgStore $gkpStore $iid";
    
    print STDERR "getting info for id $uid, $iid\n";

    open(INFO, "$infoPrg 2>>info.log |") || $base->bail("Cannot open \"$infoPrg\"");
    my $seq = "";
    while (<INFO>){
	if (/^>/){
	    next;
	}
	$seq .= $_;
    }
    close(INFO);

    my @lines = split('\n', $seq);
    $seq = join('', @lines);
    $seq = uc($seq);
    return $seq;
}
