#!/usr/local/bin/perl -w
# $Id: rptm-lint.pl,v 1.3 2003/03/04 23:24:33 schwartz Exp $

#
# Sanity check --- make sure that repeat masker talks
# about almost the entire sequence.  If it doesn't,
# it's probably truncated.
#

use 5.006_000;
use warnings;
use strict;

my ($sname, $rname);
my ($slen, $rlen) = (0,0);

# untaint pathnames, so `` and open are safe.
if ($ARGV[0] =~ m{([-_./a-zA-Z0-9]+)}) { $sname = $1; } else { die; }
if ($ARGV[1] =~ m{([-_./a-zA-Z0-9]+)}) { $rname = $1; } else { die; }

$slen = `seq_len $sname` or die;
chomp($slen);

open(F, '<', $rname) or die;
while ($_ = <F>) {
  my @F = split;
  if ($#F > 6) {
    if ($F[6] =~ /[0-9]+/) {
      $rlen = $F[6] if $F[6] > $rlen;
    }
  }
}
close(F);

if ($rlen > 0) {
  my $d = $rlen - $slen;
  $d = ($d < 0) ? -$d : $d;
  if (50_000 < $d) {
    die "inconsistency between repeatmasker and seq1: " .
	"rptmax=$rlen but seq1len=$slen\n";
  }
}

# if ($rlen == 0 && 10_000 < $slen) {
#   die "vacuous repeatmasker file rejected\n";
# }

