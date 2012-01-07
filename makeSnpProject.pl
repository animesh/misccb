#!/usr/bin/perl -w

###########################################################################
#                   SOFTWARE COPYRIGHT NOTICE AGREEMENT                   #
#       This software and its documentation are copyright (2007) by the   #
#   Broad Institute/Massachusetts Institute of Technology.  All rights    #
#   are reserved.  This software is supplied without any warranty or      #
#   guaranteed support whatsoever. Neither the Broad Institute nor MIT    #
#   can be responsible for its use, misuse, or functionality.             #
###########################################################################


#Extract reads from the trace archive and trim them.

use strict;
use File::Basename;
use Getopt::Long;
#use Cwd 'abs_path';

my $VERSION="\$Revision\$ ";
my $ARACHNE_DIR=dirname(`which Assemblez`);
my $ARACHNE_PRE="/wga/dev/WGAdata/";
my $PROJBASE="projects/SNPprojects";

sub usage {
  print "\n Usage: makeSnpProject.pl --dir=output_directory --lookup=lookuptable --genomesize=genomesize projectName1  [projectName2] [projectName3] [...] \n\n",

  "example: makeSnpProject.pl /wga/dev/WGAdata/projects/Simulation/S229/ S229 S230  \n\n",

  "output_directory must be a complete pathname and is where all the files will end up\n\n",

  "lookuptable must have been generated from the reference with MakeLookupTable\n\n",

  "genomesize must be an integer estimate of the size of the genome\n\n",

  "projectNameX is the name in the trace archive database for each project.\n",
  "";
}

my $dir = "";	#
my $lookup = "";	
my $genomesize = 0;	#

GetOptions ('dir=s' => \$dir, 'lookup=s' => \$lookup,
	    'genomesize=i' => \$genomesize) ||
die "Could not process options correctly";

if (@ARGV < 1) {
  usage();
  exit;
}

mkdir($dir);
chdir($dir) || die "could not cd to $dir";
my $user = $ENV{"USER"};
my $link = $dir;
$link =~ s/\//_/g;
system ("pushd $ARACHNE_PRE/$PROJBASE; ln -s $dir $link; popd");
system ("rm -rf *");

system("mkdir -p traceinfo && mkdir -p qual && mkdir -p fasta");
system("cp /wga/dev/WGAdata/projects/useful_files/* .");
system("echo $genomesize > genome.size");
foreach my $proj (@ARGV) {
  system("/prodinfo/prodapps/arachneExtractor/ArachneExtractor $proj all $user");
}
  
system("mv *.qual.gz qual && mv *.fasta.gz fasta && mv *.xml.gz traceinfo"); 
chdir($ARACHNE_DIR);
#note: if you have trouble with the latex requirement, you can compile
#Assemblez with #define NO_CHECK_EXECS
system("Assemblez STOP=PartitionInput DATA=$PROJBASE/$link RUN=run");
chdir($dir);
mkdir("reads");
system("cp run/work/reads.fastb reads/reads.fastb");
system("cp run/work/reads.qualb reads/reads.qualb");
system("QueryLookupTable K=12 L=$lookup SEQS=reads/reads.fastb MF=250:1000:10000 PARSEABLE=True VISUAL=True > reads/reads.qltout");






  
