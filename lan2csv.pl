$file2=shift @ARGV;
open(F2,$file2);
$fout="$file2.subid.csv";
open(FO,">$fout");
while($l=<F2>){
	$l=~s/^\s+//;
	$l=~s/\s+$//;
	@t=split(/\,/,$l);
	$line++;
	if($line==1){
			for($c=0;$c<$#t-2;$c++){
				$cp=$c+1;
				print FO"V$cp,";
			}
			print FO"SubID\n";
	}
	for($c=0;$c<=$#t-3;$c++){
		$out=@t[$c]+0;
		print FO"$out,";
	}
	$out=@t[$c+2]+0;
	print FO"C$out\n";
	print "$line SubID $out\n";
}
close F2;

