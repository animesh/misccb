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
			if($tmp1[$s3] eq "130603_Berit_7_AID_YFP.raw" or $tmp1[$s3] eq "130603_Berit_8_AID_YFP.raw" or $tmp1[$s3] eq "130603_Berit_9_AID_YFP.raw"){
				$val1{$tmp2[$c1]}+=log($tmp1[$s2]);
				$tm1{$tmp2[$c1]}++;
			}
			if($tmp1[$s3] eq "130603_Berit_10_AID_YFP.raw" or $tmp1[$s3] eq "130603_Berit_11_AID_YFP.raw" or $tmp1[$s3] eq "130603_Berit_12_AID_YFP.raw"){
				$val2{$tmp2[$c1]}+=log($tmp1[$s2]);
				$tm2{$tmp2[$c1]}++;
			}
			$tm{$tmp2[$c1]}++;
		}
	}
}

foreach (keys %tm){print "$_,",$val2{$_}-$val1{$_},",$val1{$_},$val2{$_},$tm1{$_},$tm2{$_},$tm{$_}\n"}


#system("perl -l /cygdrive/C/Users/animeshs/SkyDrive/Mayu/lib/ /cygdrive/C/Users/animeshs/SkyDrive/Mayu/Mayu.pl")



__END__


time perl scorecomp.pl /cygdrive/X/Qexactive/Berit_Sissel/130521_Berit_Aid_YFP/0130603_Berit_tek_rep_1_12/Pep789101112WG.csv 8 16 32 > scoremix.csv
 

perl scorecomp.pl /cygdrive/X/Qexactive/Berit_Sissel/130521_Berit_Aid_YFP/0130603_Berit_tek_rep_1_12/Pep789101112WG.csv 8 15 16 32 | less
perl Mayu.pl -B example.csv -C tardecdb.fa -H 41 -N 4 -O 15 -v -PmFDR -runR

130603_Berit_8_AID_YFP.raw , 2759
130603_Berit_12_AID_YFP.raw , 1393
130603_Berit_11_AID_YFP.raw , 2026
130603_Berit_10_AID_YFP.raw , 1997
130603_Berit_7_AID_YFP.raw , 3034
130603_Berit_9_AID_YFP.raw , 2042


c=csvread('X:\Qexactive\Berit_Sissel\130521_Berit_Aid_YFP\0130603_Berit_tek_rep_1_12\scoremix.csv',0,1)
corr(c(:,1),c(:,4))
corr(c(:,1),c(:,5))
corr(c(:,1),c(:,6))
plot(c(:,2),c(:,3),'b.') % corr ~0.9
hist(c(:,6),[100])
