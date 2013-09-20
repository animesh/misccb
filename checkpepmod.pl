#!/usr/bin/perl
use strict;
use warnings;
use Text::ParseWords;

my $f1=shift @ARGV;chomp $f1;
my $p1=shift @ARGV;chomp $p1;
my $p2=shift @ARGV;chomp $p2;
my $p3=shift @ARGV;chomp $p3;
my $thr=shift @ARGV;

open F1,$f1||die"\nUSAGE:\'perl program_name filename_2B_scanned\'\n\n";

my $c1=0;
my $c2;
my $c6;
my @mat;

while(my $l1=<F1>){
	chomp $l1;
	$l1 =~ s/\r//g;
	my @t1=parse_line(',',0,$l1);
	push(@mat,$l1);
	for($c2=0;$c2<$c1;$c2++){
		#$mat[$c2][$c1]=$t1[$c2];
		my @t2=parse_line(',',0,$mat[$c1]);
		my $lgt=length($t1[$p1]);
		my $poscol;
		my $posi;
		while($t2[$p1] =~ /$t1[$p1]/g){
			$posi=pos($t2[$p1]);
			$posi=($posi-($lgt+1));
			$poscol.=$posi;
		}
		print "$c1\t$c2\t$t1[$p1]\t$t1[$p2]\t$t1[$p3]\t$t2[$p1]\t$t2[$p2]\t$t2[$p3]\t$lgt\t$posi\t$poscol\n";
	}
	$c1++;
}


__END__

perl checkpepmod.pl /cygdrive/X/Elite/LARS/2013/september/CID_ETD_HCD\ sammenligning/MC3Rt.csv 3 8 9  2> /cygdrive/X/Elite/LARS/2013/september/CID_ETD_HCD\ sammenligning/0 | less
