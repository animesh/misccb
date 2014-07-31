#!/usr/bin/perl -w

###########################################################################
#                   SOFTWARE COPYRIGHT NOTICE AGREEMENT                   #
#       This software and its documentation are copyright (2006) by the   #
#   Broad Institute/Massachusetts Institute of Technology.  All rights    #
#   are reserved.  This software is supplied without any warranty or      #
#   guaranteed support whatsoever. Neither the Broad Institute nor MIT    #
#   can be responsible for its use, misuse, or functionality.             #
###########################################################################

use strict;
use Getopt::Long;

my $mylanes = "";

my $true_ref = "";
my $trunc = 0;

GetOptions ('TRUE_REF=s' => \$true_ref, 'TRUNC=i' => \$trunc, 
            'lanes=s' => \$mylanes) ||
die "Could not process options correctly";

sub usage {
  print "\n Usage: analyzeVAAL.pl [--TRUE_REF=full_path_to_true_ref] [--TRUNC=n] --lanes=\"<comma separated numbers with no spaces>\" BustardDir prefixdir prefixname reference\n\n",

  "example: analyzeVAAL.pl --TRUE_REF=/broad/solexaproc/pipelineOutput/070405_4344/U00096.modified.lessinversion --TRUNC=36 --lanes=\"3,4\" /broad/solexaproc/SL-XAE/analyzed/070405_SL-XAE_0003_FC4344/Data/C1-36_Firecrest1.8.28_09-04-2007_prodinfo/Bustard1.8.28_09-04-2007_prodinfo /wga/scr2/ohsumit/SNP/070405_4344 4344 /broad/solexaproc/pipelineOutput/070405_4344/536\n\n",

  "BustardDir is the absolute path to the Bustard output directory\n",
  "containing _seq, _prb and _sig files for the run.\n\n",

  "prefix will be the prefix for the generated files, and must include an\n",
  "absolute path. The prefix must also include the name of the files\n",
  "generated [a suffix ending will be added].\n\n",

  "reference is the full path and name to the reference genome.  Do not\n",
  "include the .fasta ending, though it will be assumed the reference is\n",
  "in this format with that suffix.\n\n",

  "TRUE_REF same format as the reference.\n\n",

  "n is a positive integer in the TRUNC option designating how many cycles\n",
  "to truncate the reads to.  In most cases, the TRUNC option should be\n",
  "omitted, unless one also applies SolexaTruncate.\n\n"

  ; 
}

my @lanes = split(/,/, $mylanes);
my $lane;

if (@ARGV < 3) {
  usage();
  exit;
}

my $bustard_dir = $ARGV[0];
my $prefixdir = $ARGV[1];
my $prefixname = $ARGV[2];
my $reference = $ARGV[3];

my $prefix = $prefixdir . "/" . $prefixname;

system("cp $reference.fasta $prefix.reference.fasta");
system("MakeLookupTable SOURCE=$prefix.reference.fasta OUT_HEAD=$prefix.reference");
if ($true_ref ne "") {
   system("cp $true_ref.fasta $prefix.true_ref.fasta");
   system("MakeLookupTable SOURCE=$prefix.true_ref.fasta OUT_HEAD=$prefix.true_ref");
}

chdir($prefixdir);

foreach $lane (@lanes) {
#  David Jaffe: we have to always use REF as the reference when we compute the
#  perfect alignments.  
   system("ln -s $prefixname.reference.fasta $prefixname.$lane.reference.fasta");
   system("ln -s $prefixname.reference.fastb $prefixname.$lane.reference.fastb");
   system("ln -s $prefixname.reference.fastamb $prefixname.$lane.reference.fastamb");
   system("ln -s $prefixname.reference.lookup $prefixname.$lane.reference.lookup");
   system("analyzeSolexa.pl --nofctxtok --lane=$lane --nologtime $bustard_dir $prefix");
   my $readsline = `grep \"^reads\\[$lane\\]\" $prefixname.metrics`;
   $readsline =~ s/\[$lane\]//g;
   open(OUT, ">$prefix.$lane.metrics");
   print OUT $readsline;
   close(OUT);
   system("ParamsByRead HEAD=$prefix.$lane");
   system("SolexaComputePerfectAligns HEAD=$prefix.$lane");
}


my $thecomm = "VAAL COMPOUND=True HEAD=\"" . $prefix . ".{" . $mylanes . "}\" REF=" .
              $prefixname . ".reference ";

if ($true_ref ne "") {
   $thecomm = $thecomm . "TRUE_REF=" . $prefixname . ".true_ref ";
} else {
   $thecomm = $thecomm . "SELF=True ";
}

if ($trunc != 0) {
   $thecomm = $thecomm . "TRUNC=$trunc ";
}

$thecomm = $thecomm . " QMIN=10 TRIMTO=27 TRIMTO_EXEMPT=True";

system($thecomm);

