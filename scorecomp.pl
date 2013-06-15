open(F1,$ARGV[0]);
$s1=$ARGV[1];
$s2=$ARGV[2];
$s3=$ARGV[3];
$s4=$ARGV[4];

while($l=<F1>){
	@tmp1=split(/,/,$l);
	if($tmp1[$s1] and $tmp1[$s3]){
		$match{$tmp1[0]}.="$tmp1[$s1]\t$tmp1[$s2]\t$tmp1[$s3]\t$tmp1[$s4],";
		
	}
	else{next;}
 		
}

foreach (keys %match){if($match{$_}){print "$_ , $match{$_}\n"}}

system("perl -l /cygdrive/C/Users/animeshs/SkyDrive/Mayu/lib/ /cygdrive/C/Users/animeshs/SkyDrive/Mayu/Mayu.pl")


__END__

perl scorecomp.pl /cygdrive/X/Qexactive/Berit_Sissel/130521_Berit_Aid_YFP/0130603_Berit_tek_rep_1_12/score-mix.csv 7 11 15 19 | less
perl Mayu.pl -B example.csv -C tardecdb.fa -H 41 -N 4 -O 15 -v -PmFDR -runR