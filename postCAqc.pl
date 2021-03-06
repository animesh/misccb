#!/usr/local/bin/perl

# postCAqc.pl   - runs caqc, fixlib and uses an optional reference file to
#              compute new library sizes

use TIGR::Foundation;
use AMOS::AmosLib;
use Statistics::Descriptive;

use strict;

my $ca2ctg     = "/local/asmg/work/mpop/Tools/CA/ca2ctg.pl";
my $caqc       = "/usr/local/common/caqc";
my $fixlib     = "/local/asmg/work/mpop/Tools/asmQC/fixlib.pl";
my $nucmer     = "/usr/local/common/nucmer";
my $showtiling = "/usr/local/common/show-tiling";

my $VERSION = '$Revision: 1.5 $ ';
my $HELP = q~
    postCAqc -f frg -a asm [-i inserts | -d dstmap ] [-r reference [-circ]]
    
    -f (frgfile) and -a (asmfile) are required
    -r (reference) allows to specify a reference molecule against which all
       reads are placed
    -circ should really only be used with -r and specifies the reference
       molecule is circular
    -i (.inserts file) and -d (dstmap) allow to specify a map from the library
       ids listed in the .frg file to database ids (cat_nos).  The inserts
       file is automatically generated by pullfrag and the dstmap is
       automatically generated by pfl.pl.
~;

my $base = new TIGR::Foundation();
if (! defined $base) {
    die("A horrible death\n");
}


$base->setVersionInfo($VERSION);
$base->setHelpInfo($HELP);
$base->setDebugLevel(1);

my $frgfile;
my $asmfile;
my $insertfile;
my $libmap;
my $fastafile;
my $circular;
my $outfile;

my $err = $base->TIGR_GetOptions("f=s"   => \$frgfile,
				 "a=s"   => \$asmfile,
				 "circ"  => \$circular,
				 "o=s"   => \$outfile,
				 "i=s"   => \$insertfile,
				 "map=s" => \$libmap,
				 "r=s"   => \$fastafile);


check_file($frgfile);
check_file($asmfile);
check_file($insertfile);
check_file($libmap);
check_file($fastafile);

if (! defined $outfile){
    $base->bail("You must provide an output file with the -o option");
}

$asmfile =~ /(.*)\.asm$/;
my $asmprefix = $1;
my $tmprefix = "tmp";
my $cmd;

my $loglevel = $base->getDebugLevel();
# running caqc
if (defined $asmfile){
    $cmd = "$caqc -silent $asmfile";
    $base->logLocal("Running $cmd", 1);
    system("$cmd");
} else {
    system("touch $asmprefix.qc");
}

if (defined $fastafile){
# need to make a contig file and map all the reads to the fasta file
# making .ctg file

    if (defined $asmfile){
	$cmd = "$ca2ctg -i $asmfile -f $frgfile > $tmprefix.ctg 2>/dev/null";
	$base->logLocal("Running $cmd", 1);
	system($cmd);
    }
    
# making a reads.fasta file from the .frg file
    $base->logLocal("Creating read file", 1);
    open(FRG, $frgfile) || $base->bail("Cannot open $frgfile: $!\n");
    open(RDS, ">$tmprefix.fasta") || 
	$base->bail("Cannot open $tmprefix.fasta:$!\n");
    while (my $record = getRecord(\*FRG)){
	my ($type, $fields, $recs) = parseRecord($record);
	if ($type ne "FRG"){
	    next;
	}
	
	my $seqname = $$fields{src};
	my @lines = split('\n', $seqname);
	$seqname = join('', @lines);
	@lines = split('\n', $$fields{seq});
	my $seq = join('', @lines);
	my ($cll, $clr) = split(',', $$fields{clr});
	$seq = substr($seq, $cll, $clr - $cll);
	printFastaSequence(\*RDS, $seqname, $seq);
    }
    
    close(RDS);
    
    $cmd = "$nucmer $fastafile $tmprefix.fasta -p $tmprefix >/dev/null 2>/dev/null";
    $base->logLocal("Running $cmd", 1);
    system($cmd);
    $cmd = "$showtiling -t $tmprefix.nuc.ctg $tmprefix.delta >/dev/null";
    $base->logLocal("Running $cmd", 1);
    system($cmd);
    $cmd = "cat $tmprefix.nuc.ctg >> $tmprefix.ctg";
    $base->logLocal("Running $cmd", 1);
    system($cmd);

    if ($loglevel < 2){ # only  erase temps for normal log level
	$base->logLocal("Removing temporary files");
	unlink("$tmprefix.nuc.ctg");
	unlink("$tmprefix.delta");
	unlink("$tmprefix.cluster");
	unlink("$tmprefix.mgaps");
	unlink("$tmprefix.ntref");
	unlink("$tmprefix.fasta");
    }

    $cmd = "$fixlib -f $frgfile -c $tmprefix.ctg -o $tmprefix.lib.qc";
} else {
    if (! defined $asmfile){
	$base->bail("An asmfile (-a) must be provided unless a reference (-r) is specified");
    }
    # just run fixlib
    $cmd = "$fixlib -f $frgfile -a $asmfile -o $tmprefix.lib.qc";
}

if (defined $insertfile){
    $cmd .= " -i $insertfile";
}
if (defined $libmap){
    $cmd .= " -map $libmap";
}
if (defined $circular){
    $cmd .= " -circ";
}

$base->logLocal("Running $cmd", 1);
system("$cmd >/dev/null 2>&1");

$cmd = "cat $asmprefix.qc $tmprefix.lib.qc > $outfile";
$base->logLocal("Running $cmd", 1);
system($cmd);

if ($loglevel < 2){
    $base->logLocal("Cleaning up some more", 1);
    unlink("$asmprefix.qc");
    unlink("$tmprefix.lib.qc");
    if (defined $fastafile){
	unlink("$tmprefix.ctg");
    }
}

exit(0);

####

sub check_file
{
    my $fname = shift;

    if (defined $fname && ! -e $fname){
	$base->bail("Cannot find file $fname");
    }
    return;
} # check_file
