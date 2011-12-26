#!/usr/bin/perl -w
use strict;

## Load agp scaffold information into a bank
## First construct the bank with contigs, and then post-process with this
## script

my $USAGE = "agp2amos.pl agp bnk | bank-transact -m - -b bnk\n";

my $scaff = shift @ARGV or die $USAGE;
my $bnk = shift @ARGV or die $USAGE;

my $map = "$bnk/CTG.map";

open SCAFF, "< $scaff" or die "Can't open $scaff ($!)\n";
open MAP, "< $map" or die "Can't open $map ($!)\n";

my %eid2iid;

while (<MAP>)
{
  if (/(\d+)\s+(\d+)\s+(\S+)/)
  {
    $eid2iid{$3} = $2;
  }
}

my $scaffid = "";
my $iid = 1;

sub endScaff
{
  print "}\n" if (defined $scaffid);
}


while (<SCAFF>)
{
  my @vals = split /\s+/, $_;

  if ($scaffid ne $vals[0])
  {
    endScaff();

    $scaffid = $vals[0];
    print "{SCF\n";
    print "eid:$scaffid\n";
    print "iid:$iid\n";
    $iid++;
  }

  if ($vals[4] eq "W")
  {
    ## contig record

    my $s = $vals[1];
    my $e = $vals[2];

    my $oo = $vals[8];

    my $ceid = $vals[5];

    my $len = $e - $s + 1;

    my $ciid = $eid2iid{$ceid};

    print "{TLE\n";

    if ($oo eq "+") { print "clr:0,$len\n"; } 
    else            { print "clr:$len,0\n"; }

    print "off:$s\n";
    print "src:$ciid\n";
    print "}\n";
  }
}

endScaff();


