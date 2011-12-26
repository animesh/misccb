#!/usr/bin/perl -w

###########################################################################
#                   SOFTWARE COPYRIGHT NOTICE AGREEMENT                   #
#       This software and its documentation are copyright (2007) by the   #
#   Broad Institute/Massachusetts Institute of Technology.  All rights    #
#   are reserved.  This software is supplied without any warranty or      #
#   guaranteed support whatsoever. Neither the Broad Institute nor MIT    #
#   can be responsible for its use, misuse, or functionality.             #
###########################################################################


#Create solexa files from a Bustard directory.

# @cmdline = @ARGV;
  
use Getopt::Long;
use IO::Handle;
use File::Basename;
use Cwd 'abs_path';

my $REF="/seq/references/Mycobacterium_tuberculosis_H37Rv/v0/Mycobacterium_tuberculosis_H37Rv.fasta.lookuptable.fastb";

my $OUT_PREFIX="/wga/isitest/ohsumit/H37Rv/$K/H37Rv";
  

my $MIN_DIST = $ARGV[0];

# K normally should be 20, 28, or 40
my $K = $ARGV[1];  

for ($i = 0; $i < $MIN_DIST; $i++) {
  $command = "bsub -R\"rusage[mem=4096]\" -q broad -o $OUT_PREFIX.$i.log 'TruePolySensitivityAllGenome K=$K OFFSET=$i SEED=$i OUT=$OUT_PREFIX MIN_DIST=$MIN_DIST' REF=$REF";
  # print $command . "\n";
  system($command);
}
