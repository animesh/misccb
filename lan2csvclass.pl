$file2=shift @ARGV;
open(F2,$file2);
while($l=<F2>){
$l=~s/^\s+//;
	$l=~s/\s+$//;
	@t=split(/\,/,$l);
	$line++;
	$subject=@t[$#t-1];
	$hashsub{$subject}++;
	$fout="$file2.s$subject.class.csv";
	open(FO,">>$fout");
	if($hashsub{$subject}==1){
			for($c=0;$c<$#t-2;$c++){
				$cp=$c+1;
				print FO"V$cp,";
			}
			print FO"CLASS\n";
	}
	for($c=0;$c<=$#t-3;$c++){
		$out=@t[$c]+0;
		print FO"$out,";
	}
	$out=@t[$c]+0;
	print FO"C$out\n";
	print "$line Class $out Subject $subject\n";
	close FO;
}
close F2;
