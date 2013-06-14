open(F1,$ARGV[0]);
open(F2,$ARGV[1]);
$mc=$ARGV[2];
$oc=$ARGV[3];

@list=<F1>;
@gop=<F2>;
for($c1=0;$c1<=$#list;$c1++){
	@tmp1=split(/,/,$list[$c1]);
	for($c2=0;$c2<=$#gop;$c2++){
		@tmp2=split(/,/,$gop[$c2]);
		chomp $tmp1[$mc];
		chomp $tmp2[$mc];
		$tmp1[$mc]=~s/\s+//g;
		$tmp2[$mc]=~s/\s+//g;
		if($tmp1[$mc] eq $tmp2[$mc]){
 			$match{$tmp1[$mc]}.="$tmp1[$oc]\t$tmp2[$oc],";
 			last;
 		}
 		else{next;}
 		
 	}
}

foreach (keys %match){if($match{$_}){print "$_ , $match{$_}\n"}}