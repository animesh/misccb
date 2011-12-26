#!/usr/bin/perl -w

# acestatus.pl
# author: Cliff Wollam
# PURPOSE: Gets reads assembled, untrimmed length and trimmed length
#	   information for all contigs in ace file(s) passed.
# 991122 - cwollam - Changed so that it gets the more logical padded
#          and unpadded lengths of the contigs
# acestatus.pl - simple script that shows the major contigs and # of
#reads in each contig in xterm of a a specific acefile. So if                 
#       you
# are looking for a specific assembly/acefile you don't have to open
# the acefile of each assembly to find it.
# USAGE FROM THE COMMAND LINE:
# acestatus.pl ace_filename

use strict;

if($#ARGV >= 0) {
  my ($contig,$file,%contigs,$bsqual);
  foreach $file (@ARGV) {
    if(-f $file) {
      open(ACE,"<$file") or die "Could not open $file: $!\n";
      while(<ACE>) {
        if($_=~/^CO\s(\w+)\s(\d+)\s(\d+)/) {
          $contig=$1;
          $contigs{$1}->{'sz_untrimmed'}=$2;
          $contigs{$1}->{'reads'}=$3;
          $contigs{$1}->{'sz_trimmed'}=0;
#          while(defined $_ && $_!~/^BQ/) { $_=<ACE>; }
#          $_=<ACE>;
#          while(defined $_ && $_!~/^$/) {
#            chomp $_;
#            foreach $bsqual (split / /,$_) {
#              ++$contigs{$contig}->{'sz_trimmed'} if($bsqual);
#            }
#            $_=<ACE>;
#          }
          while(defined($_=<ACE>) && $_!~/^$/) {
            chomp $_;
            foreach my $base (split //,$_) {
              $contigs{$contig}->{'sz_trimmed'}++ if($base ne '*');
            }
          }
        }
      }
      close(ACE);
      print "Contig information for ace file $file\n";
      print "------------------------------------------------------------\n\n";
      foreach $contig (sort bycnum keys %contigs) {
#        printf "$contig.  %d reads; %d bp (untrimmed), %d bp (trimmed).\n",
        printf "$contig.  %d reads; %d bp (padded), %d bp (unpadded).\n",
             $contigs{$contig}->{'reads'},$contigs{$contig}->{'sz_untrimmed'},
             $contigs{$contig}->{'sz_trimmed'};
      }
      print "\n";
    }
  }
}
else {
  print "USAGE: acestatus.pl <list of ace filenames>\n";
}

sub bycnum {
  my ($anum,$bnum);
  $a=~/(\d+)/;
  $anum=$1;
  $b=~/(\d+)/;
  $bnum=$1;
  $anum <=> $bnum;
}
