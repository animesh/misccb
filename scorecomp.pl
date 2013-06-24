use strict;
use Text::ParseWords;
open(F1,$ARGV[0]);
my $s1=$ARGV[1];
my $s2=$ARGV[2];
my $s3=$ARGV[3];
my $s4=$ARGV[4];
my %val1;
my %tm1;
my %val2;
my %tm2;
my %tm;
my $lc;

while(my $l=<F1>){
	$lc++;
	if($lc>1){
		chomp $l;
		$l =~ s/\r//g;
		my @tmp1=split(/,/,$l);
		#my @tmp1=parse_line(',',0,$l); # take about 10 times more processing!
		my @tmp2=split(/;/,$tmp1[$s1]);
		for(my $c1=0;$c1<=$#tmp2;$c1++){
			if($tmp1[$s2]>0 and ($tmp1[$s3] eq "130603_Berit_7_AID_YFP.raw" or $tmp1[$s3] eq "130603_Berit_8_AID_YFP.raw" or $tmp1[$s3] eq "130603_Berit_9_AID_YFP.raw")){
				$val1{$tmp2[$c1]}+=($tmp1[$s2]);
				$tm1{$tmp2[$c1]}++;
			}
			if($tmp1[$s2]>0 and ($tmp1[$s3] eq "130603_Berit_10_AID_YFP.raw" or $tmp1[$s3] eq "130603_Berit_11_AID_YFP.raw" or $tmp1[$s3] eq "130603_Berit_12_AID_YFP.raw")){
				$val2{$tmp2[$c1]}+=($tmp1[$s2]);
				$tm2{$tmp2[$c1]}++;
			}
			$tm{$tmp2[$c1]}++;
		}
	}
}

foreach (keys %tm){
	if($val1{$_}>0 && $val2{$_}>0){
		print "$_,",$val2{$_}/$val1{$_},",$val1{$_},$val2{$_},$tm1{$_},$tm2{$_},$tm{$_}\n";
	}
	elsif($val1{$_}eq"" && $val2{$_}){
		print "$_,",$val2{$_},",$val1{$_},$val2{$_},$tm1{$_},$tm2{$_},$tm{$_}\n";
	}
	elsif($val2{$_}eq"" && $val1{$_}){
		print "$_,",1/$val1{$_},",$val1{$_},$val2{$_},$tm1{$_},$tm2{$_},$tm{$_}\n";
	}
	elsif($val1{$_} && $val2{$_}){
		print "$_,",1,"$val2{$_},$tm1{$_},$tm2{$_},$tm{$_}\n";
	}
}


#system("perl -l /cygdrive/C/Users/animeshs/SkyDrive/Mayu/lib/ /cygdrive/C/Users/animeshs/SkyDrive/Mayu/Mayu.pl")



__END__


time perl scorecomp.pl /cygdrive/X/Qexactive/Berit_Sissel/130521_Berit_Aid_YFP/0130603_Berit_tek_rep_1_12/PepAreaWG.csv 8 16 34 > /cygdrive/X/Qexactive/Berit_Sissel/130521_Berit_Aid_YFP/0130603_Berit_tek_rep_1_12/PepAreaWGparsedratiochk2.csv
