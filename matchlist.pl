open(F1,$ARGV[0]);
open(F2,$ARGV[1]);
@list=<F1>;
@gop=<F2>;
for($c1=0;$c1<=$#list;$c1++){
	for($c2=0;$c2<=$#gop;$c2++){
		chomp $list[$c1];
		chomp $gop[$c2];
		$gop[$c2]=~s/\s+//g;
		$list[$c1]=~s/\s+//g;
		if($list[$c1] eq $gop[$c2]){
 			$match{$list[$c1]}++;
 			last;
 		}
 		else{next;}
 		
 	}
}
foreach (keys %match){if($match{$_}>0){print "$_\n"}}