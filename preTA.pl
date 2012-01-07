#!/usr/local/bin/perl

# $Id: preTA.pl,v 1.2 2004/04/28 18:45:36 mpop Exp $
#
# Runs phred and the trimming software similarly to "phredPhrap".
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
use TIGR::Foundation;
use TIGR::FASTAiterator;
use TIGR::FASTArecord;

my $base = new TIGR::Foundation;
if (! defined $base){
    die("Wierd problem!\n");
}

my $VERSION = '1.0 $Revision: 1.2 $ ';

# default permissions
my $PERM = 0777;

# min sequence length
my $SHORT = 40;

# first some programs
my $PHRED = "/usr/local/bin/phred";
my $LUCY = "/usr/local/bin/lucy";
my $PHD2FASTA = "/usr/local/bin/phd2fasta";

# then the directories
my $VEC_DIR = "vector_dir";
my $CHROMO_DIR = "chromat_dir";
my $PHD_DIR = "phd_dir";

# lucy parameters
my $LUCYPARM = "-error 0.025 0.02 -window 50 0.03 -bracket 10 0.02";


my $HELP_INFO = q~
   preTA.pl [-v vector_dir] [-c chromat_dir] [-p phd_dir] [-l lucy_parms]

    lucy params must be enclosed in quotes
    ~;

$base->setHelpInfo($HELP_INFO);

my $err = $base->TIGR_GetOptions("v=s" => \$VEC_DIR,
				 "c=s" => \$CHROMO_DIR,
				 "p=s" => \$PHD_DIR,
				 "l=s" => \$LUCYPARM
				 );


if (! -d $CHROMO_DIR){
    $base->bail("Cannot open chromatogram directory \"$CHROMO_DIR\"");
}

if (! -d $VEC_DIR){
    $base->logError("Cannot open vector directory \"$CHROMO_DIR\"", 1);
    $VEC_DIR = undef;
}

if (! -d $PHD_DIR){
    mkdir($PHD_DIR, $PERM) || 
	$base->bail("Cannot create phd directory \"$PHD_DIR\"");
}

# step 0, generate temp file name
my $TMP = $$ . time();
my $TMP_PHD = "$TMP.phd_dir";
mkdir ($TMP_PHD, $PERM) ||
    $base->bail("Cannot create temporary phd directory \"$TMP_PHD\"");

# step 1, run phred
my $phred_cmd = "$PHRED -id $CHROMO_DIR -pd $TMP_PHD";
print STDERR "Running $phred_cmd\n";

my $exit_val = system($phred_cmd);
if ($exit_val != 0){
    $base->bail("Phred appears to have died\n");
}

# step 2, create .seq and .qual files
my $phd2fasta_cmd = "$PHD2FASTA -id $TMP_PHD -os $TMP.seq -oq $TMP.qual";
print STDERR "Running $phd2fasta_cmd\n";

$exit_val = system($phd2fasta_cmd);
if ($exit_val != 0){
    $base->bail("Phd2fasta appears to have died\n");
}

# step 3, run a first step of trimming
my %end5; # list of clear ranges
my %end3;
my %thisset;
my $firstTrim = 1; # we are at the first trimming stage

my $lucy_cmd = "$LUCY $LUCYPARM -output $TMP.lucy.seq $TMP.lucy.qual $TMP.seq $TMP.qual";

my @readErr;
my $fr = new TIGR::FASTAiterator($base, \@readErr);
my $inseqs;
if (! defined $VEC_DIR){
    print STDERR "Running $lucy_cmd\n";
    
    $exit_val = system($lucy_cmd);
    if ($exit_val != 0){
	$base->bail("Lucy appears to have died\n");
    }
    
    $inseqs = 0;
#    $fr = new TIGR::FASTAiterator($base, \@readErr, "$TMP.lucy.seq");
    if (! $fr->open("$TMP.lucy.seq")){
	while (my $err = pop(@readErr)){
	    $base->logError($err);
	}
	$base->bail("Could not read sequence file \"$TMP.lucy.seq\"");
    }
    
    while ($fr->hasNext){
	my $rec = $fr->next();
	$inseqs++;
	
	my $head = $rec->getHeader();
	$head =~ /^>(\S+) \d+ \d+ \d+ (\d+) (\d+)/;
	$end5{$1} = $2;
	$end3{$1} = $3;
	#   print STDERR "$head\n";
    }

    $fr->close();

    print STDERR "successfully read $inseqs sequences\n";
    $firstTrim = 0;
}

# step 4, run lucy on each vector file
if (defined $VEC_DIR){
    print STDERR "Will trim vector according to files in \"$VEC_DIR\"\n";

    opendir(VEC, $VEC_DIR) || 
	$base->bail("Cannot open vector dir \"$VEC_DIR\": $!");
    
    while ($_ = readdir(VEC)){
#	print STDERR "$_\n";
	if (/^\.$/){
	    next;
	}
	if (/^\.\.$/){
	    next;
	}
	if (-f "$VEC_DIR/$_" && $_ !~ /\.splice$/){
	    print STDERR "Trimming according to vector $_\n";
	    my $fname = "$VEC_DIR/$_";
	    my $spname;
	    
	    $ENV{VECTOR_FILE} = $fname;
	    delete $ENV{SPLICE_FILE};
	    if (-f "$fname.splice"){
		$spname = "$fname.splice";
		$ENV{SPLICE_FILE} = $spname;
	    }
	    print STDERR "Running $lucy_cmd with vector \"$fname\" and splice \"$spname\"\n";
	    %thisset = (); # clear up the space
	    
	    $exit_val = system($lucy_cmd);
	    if ($exit_val != 0){
		$base->bail("Lucy appears to have died\n");
	    }
	    
	    if (! $fr->open("$TMP.lucy.seq")){
		while (my $err = pop(@readErr)){
		    $base->logError($err);
		}
		$base->bail("Could not read sequence file \"$TMP.lucy.seq\"");
	    }
	    
	    $inseqs = 0;
	    while ($fr->hasNext){
		my $rec = $fr->next();
		$inseqs++;

		my $head = $rec->getHeader();
		$head =~ /^>(\S+) \d+ \d+ \d+ (\d+) (\d+)/;
		$thisset{$1} = 1;  # mark the sequence as seen
		if ($firstTrim == 0 && ! exists $end5{$1}){
		    $base->logLocal("Skipping sequence $1", 1);
		    next; # skip previously killed sequences
		}
		if ($firstTrim == 1 || $2 > $end5{$1}){
		    $end5{$1} = $2;
		}
		if ($firstTrim == 1 || $3 < $end3{$1}){
		    $end3{$1} = $3;
		}
	    }
	    
	    $fr->close();
	    print STDERR "successfully read $inseqs sequences\n";
	    $firstTrim = 0;
	    # now we remove sequences that do not appear in the current
	    # trimmed file
	    while (my ($s, $e) = each %end5){ 
		if (! exists $thisset{$s}){
		    delete $end5{$s};
		    delete $end3{$s};
		    $base->logLocal("Skipping sequence $s", 1);
		}
	    }

	} # if $VEC_DIR/vecfile ....
    } # while <VEC>


    closedir(VEC);

} else { # if $VEC_DIR
    print STDERR "No vector files provided.  Will skip vector trimming\n";
}

# step 5, get rid of all the lucy files
my @temps = ("$TMP.seq", "$TMP.qual", "$TMP.lucy.seq", "$TMP.lucy.qual");
for (my $i = 0; $i <= $#temps; $i++){
    unlink($temps[$i]) || 
	$base->logError("Cannot remove \"$temps[$i]\": $!", 1);
}

# step 6, move over the phd files from the temp dir to the real dir, 
# trimming everything outside of the clear range
print STDERR "Moving results to the output\n";
opendir(PHD, "$TMP_PHD") || 
    $base->bail("Cannot open temp dir \"$TMP_PHD\": $!");

while ($_ = readdir(PHD)){
    if (/\.phd\.1$/){
	my $fname = "$TMP_PHD/$_";
	my $ofname = "$PHD_DIR/$_";
	$_ =~ /(.*)\.phd\.1$/;
	my $seqname = $1;

	if (! exists $end5{$seqname}) { # no clear range - trash sequence
	    $base->logLocal("Sequence $seqname was removed by lucy", 1);
	    next;
	} 
	if ($end3{$seqname} - $end5{$seqname} + 1 < $SHORT){
	    $base->logLocal("Sequence $seqname was too short", 1);
	    next; # trash short sequence
	}

	open(IN, "$fname") ||
	    $base->bail("Cannot open \"$fname\": $!");
	open(OUT, ">$ofname") ||
	    $base->bail("Cannot open \"$ofname\": $!");
	my $inSeq = 0;
	my $pos = 0;
	while (<IN>){
	    if (/^TRIM: (\d+) (\d+) (\S+)$/){
		print OUT "TRIM: $end5{$seqname} $end3{$seqname} $3\n";
		next;
	    }
	    if (/^BEGIN_DNA/){
		print OUT;
		$inSeq = 1;
		next;
	    } 
	    if (/^END_DNA/){
		print OUT;
		$inSeq = 0;
		next;
	    }
	    if ($inSeq){
		$pos++;
		if ($pos >= $end5{$seqname} && $pos <= $end3{$seqname}){
		    print OUT;
		}
		next;
	    }
	    print OUT;
	} # while <IN>
	close(IN);
	close(OUT);
    } # if phd
} # while <PHD>

closedir(PHD);

# step 7, remove phd_dir
print STDERR  "Cleaning up\n";
system("rm -rf $TMP_PHD");

exit(0);
