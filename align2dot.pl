#!/usr/local/bin/perl -w
# $Id: align2dot.pl,v 1.8 2002/07/23 18:04:38 schwartz Exp $

my @seq_a = (1, 1);
my @seq_b = (9, 9);

sub outline {
  my ($x0,$y0,$x1,$y1,$w,$c,$t) = @_;
  printf "%d %d %d %d %d %d %s\n",
	$x0+$X0-1,
        $y0+$Y0-1,
        $x1+$X0-1,
        $y1+$Y0-1,
        $w,
	$c,
	$linestyle{"l"};
}

sub out_header
{
	$need_header = 0;
	printf "/description (%s) def\n", $description;
	printf "/x0 %d def\n", $seq_a[0];
	printf "/x1 %d def\n", $seq_b[0];
	printf "/y0 %d def\n", $seq_a[1];
	printf "/y1 %d def\n", $seq_b[1];

	$X0 = $seq_a[0];

	print "% s {\n";
	for ($i = 0; $i <= $#seq_name; ++$i) {
		printf '%%   "%s" %s %s%s',
			$seq_name[$i], $seq_a[$i], $seq_b[$i], "\n";
	}
	print "% }\n";

       	printf "/x-axis-label [[(%s) /Courier 10 0]] def\n", $seq_hdr[0];
	printf "/y-axis-label [[(%s) /Courier 10 0]] def\n", $seq_hdr[1];
}

sub start_plot
{
	$need_plotdata = 0;
	print "plotdata\n";
}

#
# main
#

use Getopt::Std;
#$opt_a = 0;
$opt_h = 0;
$opt_w = 2;
getopts("haw:");

$want_align = !$opt_h;
$W = $opt_w;

$want_header = 1;
$need_header = 1;
$need_plotdata = 1;

$linestyle{"l"} = "Line";
$linestyle{"L"} = "LLine";
$linestyle{"R"} = "RLine";

$X0 = 1;
$Y0 = 1;

$seq_hdr[0] = '';
$seq_hdr[1] = '';

while (<>) {
	chop;
	if (/^d {/) {
		while (<>) {
			chop;
			last if /^}/;
			$description .= $_;
		}
	} elsif (/^s {/) {
		$n = 0;
		while (<>) {
			chop;
			last if /^}/;
			my @f = split;
			$f[0] =~ s/^\s*"//;
			$f[0] =~ s/"\s*$//;
			$seq_name[$n] = $f[0];
			$seq_a[$n] = $f[1];
			$seq_b[$n] = $f[2];
			++$n;
		}
	} elsif (/^h {/) {
		$n = 0;
		while (<>) {
			chop;
			last if /^}/;
			s/^\s*"//;
			s/"\s*$//;
			$seq_hdr[$n] = $_;
			++$n;
		}
	} elsif (/^a {/) {
		&out_header() if ($need_header && $want_header);
		last if !$want_align;
		&start_plot() if $need_plotdata;
		while (<>) {
			chop;
			last if /^}/;
			my @f = split;
			&outline($f[1],$f[2],$f[3],$f[4],$W,0,$f[0])
				if ($f[0] =~ /[lLR]/);
		}
	}
}

