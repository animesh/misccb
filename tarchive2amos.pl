#!/usr/bin/perl

use strict;

use AMOS::AmosLib;
use AMOS::ParseFasta;
use AMOS::AmosFoundation;
use TIGR::Foundation;
use XML::Parser;
use IO::Handle;
use File::Spec;

my $MINSEQ = 0;
my $MAXSEQ = undef;
my $DEFAULT_QUAL = 20;

my $tag;						# XML tag
my $library;
my $template;
my $clipl;
my $clipr;
my $mean;
my $stdev;
my $end;
my $seqId;

my %end5;			 # the reads at the 5', resp. 3', end of an insert
my %end3;
my %means;				   # mean and standard deviation for libraries
my %libraries;					# all libraries in the input
my %stdevs;
my %clr;						# clear range for each sequence
my %seq2ins;					# insert name for each sequence
my %ins2lib;					# library name for each insert
my %lib2id;						# mapping from library name to iid
my %ins2id;						# mapping from insert name to iid
my %seq2id;						# mapping from read name to iid

my @libregexp; # when using mates files contains list of libraries regular expressions
my @libnames; # library names corresponding to the regular expressions
my @pairregexp;					# read mating regular expressions

my $gzip = "gzip";

#getTempDir() function is located in TIGR::Foundation
my $tigrbase = new TIGR::Foundation;
if (! defined $tigrbase)
{
    die ("Walk, do not run, to the nearest exit!\n");
}

my $tmprefix = "tmp.$$";
my $tmpdir   = $tigrbase->getTempDir();
my $tmpfile  = File::Spec->catfile($tmpdir, "$tmprefix.red");

my $base = new AMOS::AmosFoundation;
if (! defined $base) {
    die ("Walk, do not run, to the nearest exit!\n");
}
#$base->setLogLevel(1);

my $VERSION = '$Revision: 1.34 $ ';
$base->setVersion($VERSION);

$base->setLogFile("tarchive2amos.log");

my $HELPTEXT = qq~
.USAGE.
 tarchive2amos -o <prefix> [options] fasta1 ... fastan

.DESCRIPTION.
  This program takes a files from the NCBI trace archive and produces 
  an afg file for AMOS.  

  <prefix> - prefix for the output files

  fasta1 ... fastan - list of files to be converted.
           
  The program assumes that for each file called <file>.seq there
  is a <file>.qual and a <file>.xml. (alternatively the files may
  be called fasta.<file>, qual.<file> and xml.<file>).
  If no .xml file is present the program will only produce a set of
  RED (read) records.


.OPTIONS.
  -assembly <assembly> - XML file containing assembly in assembly archive format
                   http://www.ncbi.nlm.nih.gov/Traces/assembly/assmbrowser.cgi
                   usually named ASSEMBLY.xml in the tar-ball downloaded from NCBI

  -tracedir <tracedir> - directory containing the trace information as downloaded
                   from the NCBI, either from the assembly archive or through
                   a direct query in the trace archive.  This directory must contain
                   a file named TRACEINFO.xml as well as one or more subdirs
                   containing the trace information for the organism(s) whose
                   traces are being processed.

  -c <clip>      - file containing clear ranges for the reads.  If this file
                   is provided, any sequence that does not appear in it is
                   excluded from the output.

  -m <mates>     - file containing mate-pair information as specified in the
                   BAMBUS documentation.  This file replaces information
                   provided in .xml files

  -l <lib>       - file containing mean/stdev information for libraries.
                   Overrides .xml input.

  -i <id>        - start numbering messages with id <id>
                   (useful when appending to a bank)

  -min <minlen>  - reads shorter than <minlen> are rejected (default $MINSEQ)
  -max <maxlen>  - reads longer than <maxlen> are rejected (default no limit)

  -qual <qval>   - default quality value assigned when no quality file is
                   provided (default $DEFAULT_QUAL)

.KEYWORDS.
  converters, trace archive, AMOS

~;

$base->setHelpText($HELPTEXT);

my $outprefix;
my $clears;
my $mates ;						# name of file containing mate pairs
my $ID = 1;
my $TEM_ID = 1;		   # generic identifier for reads with no template
my $silent = undef;
my $libs;
my $aaxmlname = undef;
my $tracedir = undef;
my $err = $base->getOptions("o=s"    => \$outprefix,
			    "c=s"    => \$clears,
			    "m=s"    => \$mates,
			    "l=s"    => \$libs,
			    "i=i"    => \$ID,
			    "silent" => \$silent,
			    "min=i"    => \$MINSEQ,
			    "max=i"    => \$MAXSEQ,
			    "qual=i"   => \$DEFAULT_QUAL,
			    "assembly=s" => \$aaxmlname,
			    "tracedir=s" => \$tracedir
			    );

if ($err == 0) {
    $base->bail("Command line processing failed\n");
}

if (! defined $outprefix) {
    $base->bail("A prefix must be set with option -o\n");
}

my $xml = new XML::Parser(Style => 'Stream');
if (!defined $xml) {
    $base->bail("Cannot create an XML parser");
}


my %clones;
my %seqs;
my %lib ;

my $fragname;
if ($outprefix =~ /\.afg$/) {
    $fragname = $outprefix;
} else {
    $fragname = "$outprefix.afg";
}

if (defined $libs) {
    print STDERR "Reading library information\n" unless $silent;
    open(LIB, $libs) || die ("Cannot open $libs: $!");
    while (<LIB>) {
		chomp;
		if (/^(\S+)\t(\d+.?\d*)\t(\d+.?\d*)$/) {
			$means{$1} = $2;
			$stdevs{$1} = $3;
		} else {
			print STDERR "Cannot parse line $. of $libs: $_\n";
		}
    }
    close(LIB);
}

# get the clear ranges if externally determined
if (defined $clears) {
	#    print "Processing $clears\n";
    print STDERR "Processing clear ranges\n" unless $silent;
    open(CLR, $clears) || $base->bail("Cannot open $clears: $!\n");
    while (<CLR>) {
		chomp;
		my @fields = split(' ', $_);
		#	if ($fields[0] =~ /gnl\|ti\|/){
		$fields[0] =~ s/gnl\|ti\|//;
		#	}
		$clr{$fields[0]} = "$fields[1],$fields[2]";
		#	print "clear is $fields[0] $fields[1] $fields[2]\n";
    }
    close(CLR);
}

# first we figure out which files we'll use
my @seqfiles;
my @qualfiles;
my @xmlfiles;
my $hasxml;


print STDERR "Collecting file information\n" unless $silent;
if (defined $tracedir){
### Here we process an NCBI traces/ directory, as downloaded from the assembly archive
### or from a direct query from the trace archive
### Files downloaded with FTP from the trace archive directly have a different format
###
### We assume the following directory structure
###
### traces/  - directory pointed at by $tracedir
### traces/TRACEINFO.xml - XML summary
### traces/MYORGANISM/   - sub-directory(ies) organized by organism
### traces/MYORGANISM/MYCENTER - sub-directory(ies) organized by sequencing center
### traces/MYORGANISM/MYCENTER/fasta - directory containing all the sequence records
### traces/MYORGANISM/MYCENTER/qscore - directory containing all the quality records

    push @xmlfiles, "$tracedir/TRACEINFO.xml";
    my @orgnames = (); # organisms

    opendir(TOP, $tracedir) || $base->bail ("Cannot open $tracedir: $!\n");
    while (my $fn = readdir(TOP)){
	if ($fn eq "." || $fn eq "..") {next;}
	if (-d "$tracedir/$fn") {
	    push @orgnames, "$tracedir/$fn";
#	    print "GOT $fn\n";
	}
    } # for each top file
    closedir(TOP);
    
    for (my $org = 0; $org <= $#orgnames; $org++){
	my @centers = ();  # 
	print STDERR "Processing $orgnames[$org]\n" unless $silent;
	opendir(ORG, $orgnames[$org]) || $base->bail ("Cannot open $orgnames[$org]:$!\n");
	while (my $fn = readdir(ORG)){
	    if ($fn eq "." || $fn eq "..") {next;}
	    if (-d "$orgnames[$org]/$fn"){
		push @centers, "$orgnames[$org]/$fn";
#		print "GOT $fn\n";
	    }
	} # for each organism subdir

	for (my $ctr = 0; $ctr <= $#centers; $ctr++){
	    print STDERR "Processing $centers[$ctr]\n" unless $silent;
	    opendir(FA, "$centers[$ctr]/fasta") || $base->bail ("Cannot open fasta dir in $centers[$ctr]: $!\n");
	    my %qfs;
	    if (opendir(QU, "$centers[$ctr]/qscore")){
		while (my $fn = readdir(QU)){
		    $qfs{$fn} = 1;
		}
		closedir(QU);
	    }
	    
	    my $filenum = 0;
	    while (my $fn = readdir(FA)){
		if ($fn =~ /^(\S+)\.fasta$/){
		    push @seqfiles, "$centers[$ctr]/fasta/$fn";
#		    print "got $centers[$ctr]/fasta/$fn\n";
		    if (exists $qfs{"$1.qscore"}){
			push @qualfiles, "$centers[$ctr]/qscore/$1.qscore";
#			print "got $centers[$ctr]/qscore/$1.qscore\n";
		    } else {
			push @qualfiles, "";
		    }
		    if (! defined $silent && ($filenum++ % 100 == 0)) {
			print STDERR "$filenum\r";
		    }
		}
	    }
	    print STDERR "$filenum files\n" unless $silent;
	} # for each center
    } # for each organism
} else {
    for (my $f = 0; $f <= $#ARGV; $f++) {
	my $seqname = $ARGV[$f];
	my $qualname;
	my $xmlname;
	my $prefix;
	
	if ($seqname =~ /fasta\.(.*)(\.gz)?/) {
	    $prefix = $1;
	    $qualname = "qual.${prefix}$2";
	    $xmlname = "xml.${prefix}$2";
	} elsif ($seqname =~ /(.*)\.seq(\.gz)?/) {
	    $prefix = $1;
	    $qualname = "${prefix}.qual$2";
	    $xmlname = "${prefix}.xml$2";
	} else {
	    $base->bail("Fasta file ($seqname) must be called either fasta.<file> or <file>.seq\n");
	}

	if (! -e $qualname) {
	    $base->log("Cannot find the qual file corresponding to $seqname\n", 1);
	    $qualname = "";
	}

	if (! -e $xmlname) {
	    $base->log("Cannot find the xml file corresponding to $seqname\n");
	} else {
	    push @xmlfiles, $xmlname;
	}

	push @seqfiles, $seqname;
	push @qualfiles, $qualname;
    }

    if ($#xmlfiles <= $#ARGV) {		# fewer XML files than inputs
	$hasxml = 0;
    } else {
	$hasxml = 1;
    }

} # if parameters provided on command line

if ($#xmlfiles >= 0) {
    print STDERR "Parsing the XML files\n" unless $silent;
}
## Parse XML information
for (my $f = 0; $f <= $#xmlfiles; $f++) {
    my $xmlname = $xmlfiles[$f];

    my $XML = new IO::Handle;

    if ($xmlname =~ /\.gz$/) {
		open($XML, "$gzip -dc $xmlname |") ||
			$base->bail("Cannot open $xmlname: $!\n");
    } else {
		open($XML, "$xmlname") ||
			$base->bail("Cannot open $xmlname: $!\n");
    }

    $xml->parse($XML);
    
    close($XML);
} # parse xml


## Parse .mates file, if provided
if (defined $mates) {
    print STDERR "Parsing mates information\n" unless $silent;
    open(MATES, $mates) || $base->bail("Cannot open $mates: $!\n");
    
    while (<MATES>) {
	chomp;
	if (/^library/) {
	    my @recs = split('\t', $_);
	    
	    if ($#recs < 3 || $#recs > 4) {
		$base->log("Improperly formated line $. in \"$mates\".\nMaybe you didn't use TABs to separate fields\n", 1);
		next;
	    }
	    
	    if ($#recs == 4) {
		push(@libregexp, $recs[4]);
		push(@libnames, $recs[1]);
	    }
	    
	    my $mean = ($recs[2] + $recs[3]) / 2;
	    my $stdev = ($recs[3] - $recs[2]) / 6;
	    
	    $libraries{$recs[1]} = $mean;
	    
	    $means{$recs[1]} = $mean;
	    $stdevs{$recs[1]} = $stdev;
	    
	    next;
	}						# if library
	
	if (/^pair/) {
	    my @recs = split('\t', $_);
	    if ($#recs != 2) {
		$base->log("Improperly formated line $. in \"$mates\".\nMaybe you didn't use TABs to separate fields\n");
		next;
	    }
	    push(@pairregexp, "$recs[1] $recs[2]");
	    next;
	}
	
	if (/^\#/) {			# comment
	    next;
	}
	
	if (/^\s*$/) {			# empty line
	    next;
	}
	
	# now we just deal with the pair lines
	my @recs = split('\t', $_);
	if ($#recs < 1 || $#recs > 2) {
	    $base->log("Improperly formated line $. in \"$mates\".\nMaybe you didn't use TABs to separate fields\n");
	    next;
	}
	
	my $insname = getId();
	$ins2id{$insname} = $insname;
	
	if (defined $recs[2]) {
	    $ins2lib{$insname} = $recs[2];
	} else {
	    $base->log("$insname has no library\n");
	}
	
	$end5{$insname} = $recs[0];
	$end3{$insname} = $recs[1];
	
	$seq2ins{$recs[0]} = $insname;
	$seq2ins{$recs[1]} = $insname;
    }							# while <MATES>
    
    close(MATES);
} # parse .mates


print STDERR "Processing the files\n" unless $silent;

## get ready to produce the output
open(FRAG, ">$fragname") || $base->bail("Cannot open $fragname: $!");
printFragHeader(\*FRAG);

open(TMPRED, ">$tmpfile") || $base->bail("Cannot open $tmpfile: $!\n");

## now we are ready to print the library information
while (my ($lib, $mean) = each %libraries) {
    $mean = $means{$lib};
    if (! defined $mean) {
		$mean = 33333;
		$stdevs{$lib} = 3333.3;
		$base->log("No mean found for $lib, setting to 33333\n");
    }
    if (! defined $stdevs{$lib}){
	$stdevs{$lib} = $mean / 10;
    }
    my $libid = getId();
    $lib2id{$lib} = $libid;

    print FRAG "{LIB\n";
    print FRAG "iid:$libid\n";
    print FRAG "eid:$lib\n";
    print FRAG "{DST\n";
    print FRAG "mea:$mean\n";
    print FRAG "std:$stdevs{$lib}\n";
    print FRAG "}\n";
    print FRAG "}\n";
} # print library info

## process seq and qual files
my $hasqual;
for (my $f = 0; $f <= $#seqfiles; $f++) {
	# for each file
	#   read the seq and qual files
    my $seqname = $seqfiles[$f];
    my $qualname = $qualfiles[$f];
    my $prefix;

    if ($qualname eq ""){
	$hasqual = 0;
    } else {
	$hasqual = 1;
    }

    if ($seqname =~ /\.gz$/) {
	open(SEQ, "$gzip -dc $seqname |") ||
	    $base->bail("Cannot open $seqname: $!\n");
    } else {
	open(SEQ, "$seqname") ||
	    $base->bail("Cannot open $seqname: $!\n");
    }
    
    if ($hasqual == 1) {
	if ($qualname =~ /\.gz$/) {
	    open(QUAL, "$gzip -dc $qualname |") ||
		$base->bail("Cannot open $qualname: $!\n");
	} else {
	    open(QUAL, "$qualname") ||
		$base->bail("Cannot open $qualname: $!\n");
	}
    }
    
    if (! defined $silent && ! defined $tracedir) {
	print STDERR "Parsing $seqname and $qualname\n";
    }
    
    my $seqparse = new AMOS::ParseFasta(\*SEQ);
    my $qualparse;
    if ($hasqual == 1) {
	$qualparse = new AMOS::ParseFasta(\*QUAL, ">", " ");
    }
    
    my $fhead; my $frec;
    my $qhead; my $qrec;
    
    while (($fhead, $frec) = $seqparse->getRecord()) {
	my $qid;
	my $fid; 
	my $fidname;
	
	if ($hasqual == 1) {
	    ($qhead, $qrec) = $qualparse->getRecord();
	    $qhead =~ /^(\S+)/;
	    $qid = $1;
	    $fhead =~ /^(\S+)/;
	    $fid = $1;

	    #	    print "$fid and $qid match\n";
	    
	    if ($fid ne $qid) {
		$base->bail("fasta and qual records have different IDs: $fid vs $qid\n");
	    }
	}
	
	if ($fhead =~ /^(\S+)\s+\d+\s+\d+\s+\d+\s+(\d+)\s+(\d+)/) {
	    # if TIGR formatted, fetch the clear range
	    #	    print STDERR "got TIGR: $1 $2 $3\n";
	    my $l = $2;
	    my $r = $3;
	    $l--;
	    $fidname = $1;
	    $fid = $1;
	    if (defined $clears && ! exists $clr{$fid}) {
		# clear range file decides which sequences stay and which go
		next;
	    }
	    # allow XML or clear file to override the clear range info
	    if (! exists $clr{$fid}) {
		$clr{$fid} = "$l,$r";
	    }
	} elsif ($fhead =~ /^gnl\|ti\|(\d+).* name:(\S+)/ ||
		 $fhead =~ /^gnl\|ti\|(\d+)\s+(\S+)/) {
	    #	    print "got ncbi: $1 $2\n";
	    # NCBI formatted using keywords
	    $fid = $1;
	    $fidname = $2;
	    if (defined $clears && 
		! exists $clr{$fid} && 
		! exists $clr{$fidname}) {
		# clear range file decides with sequences stay and which go
		$base->log("Couldn't find clear for $fid or $fidname\n");
		next;
	    }
	    if (exists $clr{$fidname} && ! exists $clr{$fid}) {
		$clr{$fid} = $clr{$fidname};
	    }
	} elsif ($fhead =~ /^ ?(\S+) ?(\S+)?/) {
	    #	    print STDERR "got ncbi: $1 $2\n";
	    # NCBI formatted, first is the trace id then the trace name
	    $fid = $1;
	    $fidname = $2;
	    
	    if (! defined $fidname || $fidname eq "bases") {
		$fidname = $fid;
	    }
	    
	    if (defined $clears && 
		! exists $clr{$fid} && 
		! exists $clr{$fidname}) {
		$base->log("Couldn't find clear for $fid or $fidname\n");
		# clear range file decides which sequences stay and which go
		next;
	    }
	    if (exists $clr{$fidname} && ! exists $clr{$fid}) {
		$clr{$fid} = $clr{$fidname};
	    }
	}						# processing depending on FASTA header
	
	my $recId = getId();
	
	if (defined $mates) {
	    for (my $r = 0; $r <= $#pairregexp; $r++) {
		my ($freg, $revreg) = split(' ', $pairregexp[$r]);
		my $insertname = undef;
		if ($fidname =~ /$freg/) {
		    $insertname = $1;
		    $seq2ins{$fidname} = $insertname;
		    if (! exists $end5{$insertname} || 
			$end5{$insertname} lt $fidname) {
			$end5{$insertname} = $fidname;
		    }
		} elsif ($fidname =~ /$revreg/) {
		    $insertname = $1;
		    $seq2ins{$fidname} = $insertname;
		    if (! exists $end3{$insertname} ||
			$end3{$insertname} lt $fidname) {
			$end3{$insertname} = $fidname;
		    }
		} elsif ($fid =~ /$freg/) {
		    $insertname = $1;
		    $seq2ins{$fid} = $insertname;
		    if (! exists $end5{$insertname} ||
			$end5{$insertname} lt $fid) {
			$end5{$insertname} = $fid;
		    }
		} elsif ($fid =~ /$revreg/) {
		    $insertname = $1;
		    $seq2ins{$fid} = $insertname;
		    if (! exists $end3{$insertname} ||
			$end3{$insertname} lt $fid) {
			$end3{$insertname} = $fid;
		    }
		}				# if forw or rev regexp match
		
		if (defined $insertname) {
		    my $found = 0;
		    
		    if (! exists $ins2id{$insertname}) {
			$ins2id{$insertname} = getId();
		    }
		    
		    for (my $l = 0; $l <= $#libregexp; $l++) {
			if ($fidname =~ /$libregexp[$l]/) {
			    $ins2lib{$insertname} = $libnames[$l];
			    $found = 1;
			    last;
			} elsif ($fid =~ /$libregexp[$l]/) {
			    $ins2lib{$insertname} = $libnames[$l];
			    $found = 1;
			    last;
			}
		    }
		    if ($found == 0) {
			$base->log("Cannot find library for \"$insertname\"");
		    }
		    
		    last;
		}				# if found insert
	    }					# for each pairreg
	}						# if defined mates
	
	my $seqlen = length($frec);
	my @quals = ();
	if ($hasqual == 1) {
	    #	    print $fid, "\n";
	    #	    print $qid, "\n";
	    #	    print $qrec, "\n";
	    $qrec =~ s/^\s+//;
	    #	    $qrec =~ s/ +/ /g;
	    @quals = split(/\s+/, $qrec);
	    #	    print join(',', @quals), "\n";
	    if ($#quals + 1 != $seqlen) {
		#		print join(',', @quals), "\n";
		$base->bail("Fasta and quality for $fid($fidname) disagree: $seqlen vs " . sprintf("%d\n", $#quals + 1));
	    }
	} else {
	    for (my $q = 0; $q < $seqlen; $q++) { # fill in qualities with 20
		$quals[$q] = $DEFAULT_QUAL;
	    }
	}
	
	my $caqual = "";
	for (my $q = 0; $q <= $#quals; $q++) {
	    my $qv = $quals[$q];
	    if ($qv > 60) {
		$qv = 60;
	    }
	    
	    $caqual .= chr(ord('0') + $qv);
	}
	
	if (! defined $silent && ($recId % 100 == 0)) {
	    print STDERR "$recId\r";
	}
	
	if (! exists $clr{$fid}) {
	    $clr{$fid} = "0,$seqlen";
	}
	
	$seq2id{$fidname} = $recId;
	$seq2id{$fid} = $recId;
	
	my $seq_lend;
	my $seq_rend;
	
	($seq_lend, $seq_rend) = split(',', $clr{$fid});
	
	if (defined $MAXSEQ && $seqlen > $MAXSEQ) {
	    $frec = substr($frec, 0, $seq_rend + 1);
	    $caqual = substr($caqual, 0, $seq_rend + 1);
	    $seqlen = length($frec);
	    if (defined $MAXSEQ && $seqlen > $MAXSEQ) {
		if (! defined $silent) {
		    $base->log("skipping sequence $fidname due to length $seqlen\n");
		}
		delete $seq2id{$fidname};
		delete $seq2id{$fid};
		next;
	    }
	}
	if ($seq_rend - $seq_lend < $MINSEQ) {
	    if (! defined $silent) {
		$base->log("skipping sequence $fidname since it's short\n");
	    }
	    delete $seq2id{$fidname};
	    delete $seq2id{$fid};	
	    next;
	}
	
	if (! exists $seq2ins{$fid} && exists $seq2ins{$fidname}) {
	    $seq2ins{$fid} = $seq2ins{$fidname};
	}
	
	if (! exists $seq2ins{$fid} ||
	    ! exists $ins2id{$seq2ins{$fid}}) {
	    if ($hasxml || defined $mates) {
		$base->log("Found a sequence without a template - probably not in XML or mates file: $fidname\n");
		next;
	    }					#else {
	    #	$seq2ins{$fid} = 0; #insid
	    #	$ins2id{0} = 0; #$insid;
	    #    }
	}
	
	print TMPRED "{RED\n";	# read
	print TMPRED "iid:$recId\n";          
	print TMPRED "eid:$fid\n";
	print TMPRED "seq:\n";
	$frec =~ s/[^actgnACTGN]/N/g;
	for (my $s = 0; $s < $seqlen; $s += 60) {
	    print TMPRED substr($frec, $s, 60), "\n";
	}
	print TMPRED ".\n";
	print TMPRED "qlt:\n";
	for (my $s = 0; $s < $seqlen; $s += 60) {
	    print TMPRED substr($caqual, $s, 60), "\n";
	}
	if ($seq_rend > $seqlen) {
	    $base->log("right end of clear range $seq_rend > $seqlen - shrinking it\n");
	    $seq_rend = $seqlen;
	}
	print TMPRED ".\n";
	if (exists $ins2id{$seq2ins{$fid}}) {
	    print TMPRED "frg:$ins2id{$seq2ins{$fid}}\n";
	}
	print TMPRED "clr:$seq_lend,$seq_rend\n";
	print TMPRED "}\n";
    }							# while each read
    
    if (! defined $silent && ! defined $tracedir) {
	print STDERR "done\n";
    }
    close(SEQ);
    close(QUAL);
}
close(TMPRED);

if (! defined $silent) {
    print STDERR "doing fragments\n";
}

while (my ($ins, $id) = each %ins2id) {
    print FRAG "{FRG\n";
    print FRAG "iid:$id\n";
    print FRAG "eid:$ins\n";
    print FRAG "lib:$lib2id{$ins2lib{$ins}}\n";
    print FRAG "typ:I\n";

    if ( exists $end5{$ins} && exists $end3{$ins} &&
         exists $seq2id{$end5{$ins}} && exists $seq2id{$end3{$ins}}) {
        print FRAG "rds:$seq2id{$end5{$ins}},$seq2id{$end3{$ins}}\n";
    }

    print FRAG "}\n";
}

if (! defined $silent) {
    print STDERR "putting it together\n";
}

if (-f $tmpfile) {
    open(TMPRED, $tmpfile) || $base->bail("Cannot open $tmpfile:$!\n");
    while (<TMPRED>) {
        print FRAG;
    }
    close(TMPRED);
    unlink($tmpfile) || $base->bail("Cannot remove $tmpfile: $!\n");
}

###
# added assembly archive conversion
###
my $aatag;
my $aaptag;
my $aavalid = 0;
my $aaiid = 1;
my $clr1 = 0;
my $clr2 = 0;
my @congaps;
my $gval;
my $cval;

sub Start_handler {
    my $p = shift;
    my $el = shift;
    $aaptag = $aatag;
    $aatag = lc($el);
    
    if ($aatag eq "contig") {
	my ($attr, $cval) = @_;
	print "\n\n contig $cval \n";
	print FRAG "\n{CTG\n";
	print FRAG "iid:$aaiid\n";
	print FRAG "sts:C\n";
	print FRAG "eid:Contig$aaiid-$cval\n";
	$aaiid++;
    } elsif ($aatag eq "consensus") {
	print FRAG "seq:\n";
    } elsif ($aatag eq "conqualities") {
	print FRAG "qlt:\n";
    } elsif ($aatag eq "trace") {
	print FRAG "{TLE\n";
    } elsif ($aatag eq "valid") {
	$aavalid = 1;
    } elsif ($aatag eq "tiling") {
	my $attr;
	($attr, $gval) = @_;
	if (lc($gval) eq "reverse") {
	    print FRAG "clr:$clr2,$clr1\n";
	} else {
	    print FRAG "clr:$clr1,$clr2\n";
	}
	$clr1 = 0; $clr2 = 0;
    } 
}

my $contig_seq = "";
my $contig_qual = "";
my $ti_global = "";

sub Char_handler {
    my ($p, $data) = @_;
    my $el = lc($p->current_element);
    
    if ($el eq "consensus") {
	chomp($data);
	$contig_seq .= $data;
    } elsif ($el eq "conqualities") {
	chomp($data);
	$data =~ s/^ //;
	my @quals = split / +/, $data;
	
	for (my $q = 0; $q <= $#quals; $q++) {
	    my $qv = $quals[$q];
	    if ($qv > 60) {
		$qv = 60;
	    }
	    
	    $contig_qual .= chr(ord('0') + $qv);
	}
	
    } elsif (($el eq "start") && ($aavalid)) {
	$clr1 = $data -1;
	
    } elsif (($el eq "stop") && ($aavalid)) {
	$clr2 = $data -1;
    } elsif (($el eq "ti") && ($aaptag eq "trace")) {
	$ti_global .= $data;
	
	
    } elsif ($el eq "tracegaps") {
	print FRAG "gap:\n";
	chomp($data);
	$data =~ s/^ //;
	my @redgaps = split / +/, $data;
	my $c = 0;
	foreach (@redgaps) {
	    $c += $_;
	    print FRAG "$c ";
	}
	print FRAG "\n.\n";
	
    } elsif (($el eq "start") && ($aaptag eq "tiling")) {
	if (lc($gval) eq "forward") {
	    $data--;
	}
	print FRAG "off:$data\n";
    } elsif ($el eq "congaps") {
	chomp($data);
	$data =~ s/^ //;
	@congaps = split / +/, $data;
    }
}

sub End_handler {
    my $p = shift;
    my $el = lc(shift);
    $aatag = $aaptag;
    
    if (($el eq "contig") || ($el eq "trace")) {
	print FRAG "}\n";
    } elsif ($el eq "consensus") {
	my $ii= 0;
	my $ss = "";
	
	foreach (@congaps) {
	    if (($_ + $ii) > length($contig_seq)) {
		my $err = $_ + $ii;
		print " ERROR - gap placement outside of contig, placement = $err\n";
		last;
	    }
	    $ss .= substr($contig_seq, $ii, $_) . "-";
	    $ii += $_;
	}
	$ss .= substr($contig_seq, $ii);
	
	for (my $s = 0; $s < length($ss); $s += 60) {
	    print FRAG substr($ss, $s, 60), "\n";
	}
	print FRAG ".\n";
	$contig_seq = $ss;
    } elsif ($el eq "conqualities") {
	my $ii= 0;
	my $ss = "";
	
	if (length($contig_qual) != length($contig_seq)) {
	    foreach (@congaps) {
		if (($_ + $ii) > length($contig_seq)) {
		    my $err = $_ + $ii;
		    print " ERROR - gap placement outside of contig, placement = $err \n";
		    last;
		}
		$ss .= substr($contig_qual, $ii, $_);
		$ii += $_;
		$ss .= substr($contig_qual, $ii, 1);
		
	    }
	    $ss .= substr($contig_qual, $ii);
	} else {
	    $ss = $contig_qual;
	}
	
	$ss = substr($ss, 0, length($contig_seq));
	
	for (my $s = 0; $s < length($ss); $s += 60) {
	    print FRAG substr($ss, $s, 60), "\n";
	}
	print FRAG ".\n";
	$contig_qual = "";
	$contig_seq = "";
	@congaps = ();
    } elsif ($el eq "valid") {
	$aavalid = 0;
    } elsif ($el eq "ti") {
	if ( defined $seq2id{$ti_global} ) {
	    my $tid = $seq2id{$ti_global};
	    print FRAG "src:$tid\n";
	} else {
	    my $cl = $p->current_line;
	    print "ERROR no source found for ti - $ti_global\n";
	    print "   trace tag = $aaptag , ti tag $el \n";
	    print " current line in xml is $cl \n\n";
	}
	$ti_global = "";
    }
    
}

if (defined($aaxmlname)) {
    my $aaxml = new XML::Parser(Style => 'Stream');
    if (!defined $aaxml) {
	print "Cannot create an XML parser";
    }
    
    my $AAXML;
    open($AAXML, "$aaxmlname") || die " cannot open assembly archive xml file: $aaxmlname";
    
    $xml->setHandlers(Char => \&Char_handler, Start => \&Start_handler, End => \&End_handler);
    
    $xml->parse($AAXML);
    
    close($AAXML);
}
###
# end assembly archive conversion
###

close(FRAG);

if (! defined $silent) {
    print STDERR "done\n";
}

exit(0);


########
#
# The XML parser functions for parsing Trace Archive formatted .xml files

sub StartDocument
{
    if (! defined $silent) {
	print "starting\n";
    }
}

sub EndDocument
{
    if (! defined $silent) {
	print "done\n";
    }
}

sub StartTag
{
    $tag = lc($_[1]);
    
    if ($tag eq "trace") {
	$library = undef;
	$template = undef;
	$clipl = undef;
	$clipr = undef;
	$mean = undef;
	$stdev = undef;
	$end = undef;
	$seqId = undef;
    }
}

sub EndTag
{
    $tag = lc($_[1]);
    if ($tag eq "trace") {
	if (! defined $seqId) {
	    if (! defined $silent) {
		$base->log("trace has no name???\n");
	    }
	}
	#	if (defined $clears && 
	#	    ! defined $clr{$seqId}){
	#	    return; # only handle reads with a clear range
	#	}
	if (! defined $library) {
	    if (! defined $silent) {
		$base->log("trace $seqId has no library\n");
	    }
	}
	$libraries{$library} = 1;
	#	if (! defined $mean && ! exists $means{$library}){
	#	    if (! defined $silent){
	#		$base->log("library $library has no mean - replacing with 33333\n");
	#	    }
	#	    $means{$library} = 33333;
	#	    $mean = 33333;
	#	} els
	if (defined $mean && ! exists $means{$library}) { 
	    $means{$library} = $mean;
	}
	
	#	if (! defined $stdev && ! exists $stdevs{$library}){
	#	    if (! defined $silent){
	#		$base->log("library $library has no stdev - replacing with 10% of $mean\n");
	#	    }
	#	    $stdevs{$library} = $mean * 0.1;
	#	} els
	if (defined $stdev && ! exists $stdevs{$library}) {
	    $stdevs{$library} = $stdev;
	}
	
	if (! defined $template) {
	    $template = "TEM_" . $TEM_ID++;
	    if (! defined $silent) {
		$base->log("trace $seqId has no template.  Setting to $template\n");
	    }
	} 
	
	if (! defined $end) {
	    if (! defined $silent) {
		$base->log("trace $seqId has no end\n");
	    }
	}
	
	if ($end =~ /^R/i) {
	    if (! exists $end3{$template} ||
		$seqId gt $end3{$template}) {
		$end3{$template} = $seqId;
	    }
	}
	
	if ($end =~ /^F/i) {
	    if (! exists $end5{$template} ||
		$seqId gt $end5{$template}) {
		$end5{$template} = $seqId;
	    }
	}
	
	
	if (defined $clipl && defined $clipr) {
	    $clipr--;
	    # we don't care about clear ranges if defined elsewhere
	    if (! defined $clears) {
		$clr{$seqId} = "$clipl,$clipr";
	    }
	}
	$seq2ins{$seqId} = $template;
	$ins2lib{$template} = $library;
	if (! defined $ins2id{$template}) {
	    $ins2id{$template} = getId();
	}
    }
    
    $tag = undef;
}

sub Text 
{
    if (defined $tag) {
	if ($tag eq "insert_size") {
	    $mean = $_;
	} elsif ($tag eq "insert_stdev") {
	    $stdev = $_;
	} elsif ($tag eq "trace_name") {
	    $seqId = $_;
	} elsif ($tag eq "library_id") {
	    $library = $_;
	} elsif ($tag eq "seq_lib_id") {
	    if (! defined $library) {
		$library = $_;
	    }
	} elsif ($tag eq "template_id") {
	    $template = $_;
	} elsif ($tag eq "trace_end") {
	    $end = $_;
	} elsif ($tag eq "clip_quality_left" ||
		 $tag eq "clip_vector_left" ||
		 $tag eq "clip_left") {
	    if (! defined $clipl || $_ > $clipl) {
		$clipl = $_;
	    }
	} elsif ($tag eq "clip_quality_right" ||
		 $tag eq "clip_vector_right" ||
		 $tag eq "clip_right") {
	    if (! defined $clipr || $_ < $clipr) {
		$clipr = $_;
	    }
	}
    }
}

sub pi
{
    
}


## Some other useful functions

sub printFragHeader
{
    my $file = shift;
    
    my $date = localtime();
    
    print $file "{UNV\n";
    print $file "iid:1\n";
    print $file "com:\nGenerated by $ENV{USER} with tarchive2amos on $date\n.\n";
    print $file "}\n";
}

sub getId
{
    return $ID++;
}



