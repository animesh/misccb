use strict;
use Text::ParseWords;
open(F1,$ARGV[0]);
my $s1=$ARGV[1];
my $s2=$ARGV[2];
my $s3=$ARGV[3];
my $s4=$ARGV[4];
my $s5=$ARGV[5];
my $s6=$ARGV[6];
my $s7=$ARGV[7];
my $s8=$ARGV[8];
my %valn;
my %tmn;
my %pepn;
my %pen;
my %valt;
my %tmt;
my %pept;
my %pet;
my %valk;
my %tmk;
my %pepk;
my %pek;
my %valo;
my %tmo;
my %pepo;
my %peo;
my %tm;
my $lc;
my $qt=0.05;

while(my $l=<F1>){
	$lc++;
	if($lc>1){
		chomp $l;
		$l =~ s/\r//g;
		my @tmp1=split(/,/,$l);
		#my @tmp1=parse_line(',',0,$l); # take about 10 times more processing!
		my @tmp2=split(/;/,$tmp1[$s1]);
		for(my $c1=0;$c1<=$#tmp2;$c1++){
			if( $tmp1[$s3] =~/130603_Berit_1[0-2]_AID_YFP.raw/ and $tmp1[$s4] eq "Unambiguous" and  $tmp1[$s5] eq "" and $tmp1[$s6]<$qt){
			#if( $tmp1[$s3] =~/130603_Berit_1[0-2]_AID_YFP.raw/  and $c1==0){  # 5350 for MCR2RpepWGt.csv
				$valn{$tmp2[$c1]}+=($tmp1[$s2]);
				$pen{$tmp2[$c1]}+=log($tmp1[$s7]);
				$pepn{$tmp2[$c1]}.="$tmp1[$s8]($tmp1[$s5]-$tmp1[$s6]-$tmp1[$s4]);";
				$tmn{$tmp2[$c1]}++;
				#print "$tmp1[$s1],$tmp1[$s2],$tmp1[$s3],$tmp1[$s4],$tmp1[$s5],$tmp1[$s6],$tmp1[$s7],$tmp1[$s8]\n";
			}
			if( $tmp1[$s3] =~/130603_Berit_[7-9]_AID_YFP.raw/ and $tmp1[$s4] eq "Unambiguous" and  $tmp1[$s5] eq "" and $tmp1[$s6]<$qt){
				$valt{$tmp2[$c1]}+=($tmp1[$s2]);
				$pet{$tmp2[$c1]}+=log($tmp1[$s7]);
				$pept{$tmp2[$c1]}.="$tmp1[$s8]($tmp1[$s5]-$tmp1[$s6]-$tmp1[$s4]);";
				$tmt{$tmp2[$c1]}++;
				#print "$tmp1[$s1],$tmp1[$s2],$tmp1[$s3],$tmp1[$s4],$tmp1[$s5],$tmp1[$s6],$tmp1[$s7],$tmp1[$s8]\n";
			}
			if( $tmp1[$s3]  =~/130613_Berit_1[0-2]_AID_YFP_Stim_B002_T1.raw/ and $tmp1[$s4] eq "Unambiguous" and  $tmp1[$s5] eq "" and $tmp1[$s6]<$qt){
				$valk{$tmp2[$c1]}+=($tmp1[$s2]);
				$pek{$tmp2[$c1]}+=log($tmp1[$s7]);
				$pepk{$tmp2[$c1]}.="$tmp1[$s8]($tmp1[$s5]-$tmp1[$s6]-$tmp1[$s4]);";
				$tmk{$tmp2[$c1]}++;
				#print "$tmp1[$s1],$tmp1[$s2],$tmp1[$s3],$tmp1[$s4],$tmp1[$s5],$tmp1[$s6],$tmp1[$s7],$tmp1[$s8]\n";
			}
			if( $tmp1[$s3]  =~/130613_Berit_[7-9]_AID_YFP_Ustim_B002_T1.raw/ and $tmp1[$s4] eq "Unambiguous" and  $tmp1[$s5] eq "" and $tmp1[$s6]<$qt){
				$valo{$tmp2[$c1]}+=($tmp1[$s2]);
				$peo{$tmp2[$c1]}+=log($tmp1[$s7]);
				$pepo{$tmp2[$c1]}.="$tmp1[$s8]($tmp1[$s5]-$tmp1[$s6]-$tmp1[$s4]);";
				$tmo{$tmp2[$c1]}++;
				#print "$tmp1[$s1],$tmp1[$s2],$tmp1[$s3],$tmp1[$s4],$tmp1[$s5],$tmp1[$s6],$tmp1[$s7],$tmp1[$s8]\n";
			}
			$tm{$tmp2[$c1]}++;
		}
	}
}


print "UniprotID,TotalAreaStimB1,Peptides,Score,FDR,Count,TotalAreaUnStimB1,Peptides,Score,FDR,Count,TotalAreaStimB2,Peptides,Score,FDR,Count,TotalAreaUnStimB2,Peptides,Score,FDR,Count,TotalCount\n";
foreach (keys %tm){
	#if(!$tmn{$_} and !$tmk{$_} and $tmt{$_}){
		print "$_,$valn{$_},$pepn{$_},",-10*($pen{$_}),",",100*exp($pen{$_}),",$tmn{$_},$valt{$_},$pept{$_},",-10*($pet{$_}),",",100*exp($pet{$_}),",$tmt{$_},$valk{$_},$pepk{$_},",-10*($pek{$_}),",",100*exp($pek{$_}),",$tmk{$_},$valo{$_},$pepo{$_},",-10*($peo{$_}),",",100*exp($peo{$_}),",$tmo{$_},$tm{$_}\n";
	#}
}

__END__


perl scorecomp.pl /cygdrive/X/Qexactive/Linda/MCR7PepWG.csv 11 19 37 8 12 20 21 7 > /cygdrive/X/Qexactive/Linda/MCR7PepWGParse.csv