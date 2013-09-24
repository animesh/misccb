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
my @mat;
my %cnt;
my %mcnt;

while(my $l1=<F1>){
	chomp $l1;
	$l1 =~ s/\r//g;
	my @t1=parse_line(',',0,$l1);
	if($t1[$p2]){
	push(@mat,$l1);
	for($c2=0;$c2<$c1;$c2++){
		my @t2=parse_line(',',0,$mat[$c2]);
		if($t1[$p1] ne $t2[$p1] and !$cnt{$t2[$p1]}){
			#$mat[$c2][$c1]=$t1[$c2];
			my $poscol;
			my $lgt=length($t2[$p1]);
			while($t1[$p1] =~ /$t2[$p1]/g){
				$poscol.=(pos( $t1[$p1] )-$lgt+1)."-";
				pos( $t1[$p1] ) = $-[0] + 1;
			}
			if($poscol){
				#print "$c1\t$c2\t$t1[$p1]\t$t1[$p3]\t$t2[$p1]\t$t2[$p3]\t$lgt\t$poscol\n";
				#$cnt{$t1[$p1]}++;
				$cnt{$t2[$p1]}++;
				$mcnt{$t1[$p1]}.="$t1[$p3],$t2[$p1]-($poscol)-$t2[$p3],";
			}
		}
		elsif($t1[$p1] eq $t2[$p1] ){$mcnt{$t1[$p1]}.="$t1[$p3],EQ-$t2[$p3],";}
		else{$mcnt{$t1[$p1]}.="$t1[$p3],"}
	}
	}
	$c1++;
	#print "$t1[$p1]\t$cnt{$t1[$p1]}\n";
}


foreach (keys %mcnt){
	print "$_,$mcnt{$_}\n";
}


__END__

perl checkpepmod.pl /cygdrive/X/Elite/LARS/2013/september/CID_ETD_HCD\ sammenligning/MC3Rt.csv 3 8 9  2> /cygdrive/X/Elite/LARS/2013/september/CID_ETD_HCD\ sammenligning/0 | less
