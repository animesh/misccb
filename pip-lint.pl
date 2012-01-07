#!/usr/local/bin/perl -w
# $Id: pip-lint.pl,v 1.35 2005/07/24 07:58:48 schwartz Exp $

use 5.006_000;
use warnings;
use strict;

use Getopt::Long;

$ENV{'PIPURL'} || die '$PIPURL undefined';

open(STDERR, ">&STDOUT") || die "Can't dup stdout";

my $usage = "usage --{seq1 seq2 exon rpts ulay nvce gxan}\n";
my ($seq1, $seq2, $exon, $rpts, $ulay, $nvce, $gxan);
my ($anno,$align);
my $reqhdr;

die $usage unless GetOptions(
	'seq1=s' => \$seq1,
	'seq2=s' => \$seq2,
	'exon=s' => \$exon,
	'rpts=s' => \$rpts,
	'ulay=s' => \$ulay,
	'nvce=s' => \$nvce,
	'gxan=s' => \$gxan,
	'anno=s' => \$anno,
	'align=s' => \$align,
	'reqhdr' => \$reqhdr,
);

die $usage unless $exon && $rpts && $ulay && $nvce && $gxan;
unless ($seq1 && $seq2) {
    $seq1 = $ARGV[0];
    $seq2 = $ARGV[1];
}
die $usage unless $seq1 && $seq2;

my $novice;
my $rpts_type = 0;

sub sayurl
{
  my $url = shift;
  my $txt = $url;
  $txt =~ s/^[^#]*//;

  my $base = $ENV{'PIPURL'};
  # $base =~ s|/$||;

  print "</pre>\n";
  print "For instructions on file format, see ";
  printf '<a href="%s">%s</a>', "$base/$url", "$txt";
  print "<br><pre>\n";
}

sub seqerr
{
    sayurl 'pip-instr.html#sequence';
    return -1;
}

sub check_hdr
{
    my $seq = shift;
    my $name = shift;
    if (system("seq_hashdr $seq") != 0) {
	printf "Missing FASTA header in $name seqeuence.\n";
	return &seqerr;
    }
    return 1;
}

sub check_seq
{
    my $seq = shift;
    my $name = shift;

    return 0 if (!$seq);

    my $n = `seq_len -a -m $seq`;
    if ($?) {
	system("<$seq grep -n -i '[^abcdghkmnrstvwxy0-9 \t]'|head|clip-lines");
        return &seqerr;
    } else {
        local @_ = split(' ', $n); # (total, upper, lower)

	# Make everything upper case if necessary.
        if (($novice || $_[1] == 0) && $_[2] > 0) {
            if (!rename($seq, "$seq.tmp")) {
		warn("rename: $seq, $seq.tmp: $!");
	        return &seqerr;
	    } else {
	        if (system("tr a-z A-Z <$seq.tmp >$seq") != 0) {
		    warn("tr: $?");
		    return &seqerr;
	        }
	    }
	    #print "\nThe $name sequence contains no upper-case letters ";
	    #print "(everything is masked out).\n";
            #return &seqerr;
	}

	# Uniform case means not masked, so check it out.
	if ($name eq "first") { # XXX - kludge!
            if ($novice || $_[1] == 0 || $_[2] == 0) {
		if ($rpts_type == 0 && system("rptm-lint $seq $rpts") != 0) {
    		    sayurl 'pip-instr.html#mask';
		    return -1;
	        }
	    }
        }
	return $_[0];
    }
}

#### main ####

if ($nvce) {
    $novice=`cat "$nvce"`;
    if ($novice =~ /0/) {
        $novice=0;
    } else {
        $novice=1;
    }
}

if ($rpts) {
   open(F, '<', $rpts) or die;
   $_ = <F> || "";
   $rpts_type = /^%:repeats/;
   close F;
}

sub nth {
  my $i = shift;
  $i++;
  return "first" if ($i == 1);
  return "second" if ($i == 2);
  return "third" if ($i == 3);
  return $i."th";
}

my @len;
$len[0] = check_seq $seq1, nth(0);
$len[1] = check_seq $seq2, nth(1);
foreach my $i (2..$#ARGV) { $len[$i] = check_seq $ARGV[$i], nth($i); }

print "The first sequence is empty.\n" if ($len[0] == 0);
print "The second sequence is empty.\n" if ($len[1] == 0);
exit 1 if ($len[0] <= 0 || $len[1] < 0);

my $err = 0;

if ($reqhdr) {
    foreach my $i (0..$#ARGV) {$err=1 if check_hdr($ARGV[$i], nth($i)) < 0;}
}

if ($exon) {
    if (system("gene_tag -$len[0] $exon >/dev/null") != 0 ||
	system("gene_tag  $exon >/dev/null") != 0) {
	    sayurl 'pip-instr.html#exons';
	    $err=1;
    }
}

if ($anno && (system("pip-annot $anno >/dev/null") != 0)) {
    sayurl 'pip-instr3.html';
    $err=1;
}

# Check conformance between length of seq and numbers in the mask.
if ($rpts && $rpts_type == 0 &&
	system("repeats_tag -$len[0] $rpts >/dev/null") != 0) {
    sayurl 'pip-instr.html#mask';
    $err=1;
}

if ($ulay && system("mkunderlays $ulay >/dev/null") != 0) {
    sayurl 'pip-instr2.html#color';
    $err=1;
}

if (`cat $gxan` =~ /1/) {
    if ((! -e $exon) || (-z $exon)) {
        print "The exons file is empty.\n";
        sayurl 'pip-instr.html#exons';
        $err=1;
    }
}

sub badlav {
    print "Bad lav format alignment file.\n";
    $err = 1;
}

if ($align) {
    open(F,"ualign") or die;
    my $a = <F>;
    unless (eof || $a =~ /^#:lav/) {
        badlav;
    }
    close(F);
}

exit $err;
