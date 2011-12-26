#!/usr/local/bin/perl

# $Id: runTA.pl,v 1.2 2004/04/28 18:45:36 mpop Exp $
#
# Runs TIGR Assembler.
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

my $base = new TIGR::Foundation;
if (! defined $base){
    die("Wierd problem!\n");
}

my $VERSION = '1.0 $Revision: 1.2 $ ';

# location of TIGR Assembler
my $TA = "/usr/local/bin/TIGR_Assembler";

# Default parameters
my %params = (
	      "g" => 8,
	      "l" => 40,
	      "e" => 15,
	      "p" => 97.5
	      );

my $HELP_INFO = qq~
    runTA [options] [-C contig_file] [-q qual_file] seq_file.

    Quality file is optional.  
    Contig file is only required when jumpstarting.
    For information about the other options, please run: 
    '$TA -h'
    ~;

$base->setHelpInfo($HELP_INFO);

my $err = $base->TIGR_GetOptions(
				 'C=s' => \&appOptions, # contig file
				 'e=s' => \&appOptions, # end
				 'k=i' => \&appOptions, # max match len
				 'g=i' => \&appOptions, # maxerr 32
				 'l=i' => \&appOptions, # minlen
				 'p=f'=>  \&appOptions, # min pct
				 'q=s'=>  \&appOptions, # qual file
				 's!'=>   \&appOptions, # use singletons
				 'Y=i'=>  \&appOptions, # safe file size
				 'a=s'=>  \&appOptions, # alignment_directory
				 'A=s'=>  \&appOptions, # ace_output_file
				 'b=i'=>  \&appOptions, # buffer size       
				 'c=s'=>  \&appOptions, # coverage_filename
				 'd'=>    \&appOptions,   # -
				 'D=s'=>  \&appOptions, # phd_dir
				 'E=i'=>  \&appOptions, # phd_dir
				 'f=s'=>  \&appOptions, # fasta_file
				 'F=s'=>  \&appOptions, # repeat_file
				 'G=s'=>  \&appOptions, # grouper file
				 'J=s'=>  \&appOptions, # dump_file
				 'L'=>    \&appOptions,   # -
				 'n=s'=>  \&appOptions, # asm_prefix
				 'N'=>    \&appOptions,   # -
				 'o=s'=>  \&appOptions, # score file 
				 'P'=>    \&appOptions,   # -
				 'r=i'=>  \&appOptions, # resort number
				 'R'=>    \&appOptions,   # -
				 'S=i'=>  \&appOptions, # max_span_len
				 't'=>    \&appOptions,   # -
				 'T'=>    \&appOptions,   # -
				 'u'=>    \&appOptions,   # -
				 'w=i'=>  \&appOptions,   # number of threads
				 'x'=>    \&appOptions,   # -
				 'X'=>    \&appOptions,   # -
				 'y=i'=>  \&appOptions, # repeat_num_cutoff
				 'z=i'=>  \&appOptions, # num_conflicts
				 'Z'=>    \&appOptions   # -
				 );

my $seqFile = $ARGV[0];
$seqFile =~ /(\S+)\.seq/;

my $prefix;
if (! defined $1){
    $prefix = $seqFile;
} else {
    $prefix = $1;
}


my $AlignFile = "$prefix.align"; $params{"a"} = $AlignFile;
my $OutFile = "$prefix.asm";
my $FastaFile = "$prefix.fasta"; $params{"f"} = $FastaFile;
my $SCRATCH = "$prefix.scratch";

if (! defined $seqFile){
    $base->bail("You must provide a sequence file");
}

if (! -f $seqFile){
    $base->bail("Cannot read sequence file");
}

my $TA_cmd = "$TA";
while (my ($parm, $val) = each %params){
    if (defined $val){
	$TA_cmd .= " -$parm $val";
    } else {
	$TA_cmd .= " -$parm";
    }
}

$TA_cmd .= " $SCRATCH";

open(STDIN, $ARGV[0]) || $base->bail("Cannot open $ARGV[0]: $!");
print STDERR "Running $TA_cmd\n";
open(STDERR, ">$prefix.stderr") || $base->bail("Cannot open $prefix.stderr: $!");
open(STDOUT, ">$OutFile") || $base->bail("Cannot open output \"$OutFile\": $!");
exec($TA_cmd) || $base->bail("Could not execute TIGR Assembler\n");

exit(0);

sub appOptions
{
    $params{$_[0]} = $_[1];
}
