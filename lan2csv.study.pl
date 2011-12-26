$file2=shift @ARGV;
open(F2,$file2);
$fout="$file2.group.csv";
$foutr1="$file2.group.r1.csv";
$foutr2="$file2.group.r2.csv";
$foutr3="$file2.group.r3.csv";
$foutr="$file2.group.r";
open(FO,">$fout");
open(FOR1,">$foutr1");
open(FOR2,">$foutr2");
open(FOR3,">$foutr3");
open(FOR,">$foutr");
while($l=<F2>){
	$l=~s/^\s+//;
	$l=~s/\s+$//;
	@t=split(/\,/,$l);
	$line++;
	if($line==1){
			for($c=0;$c<=$#t-6;$c++){
				$cp=$c+1;
				print FO"V$cp,";
				print FOR1"V$cp,";
				print FOR2"V$cp,";
				print FOR3"V$cp,";
			}
			print FO"CLASS\n";
			print FOR1"CLASS\n";
			print FOR2"CLASS\n";
			print FOR3"CLASS\n";
	}
        $out0=@t[-6]+0;#svm_mat(:,11966) time
        $out1=@t[-5]+0;#svm_mat(:,11967) reps
        $out2=@t[-4]+0;#svm_mat(:,11968) ? 1 or 2
        $out3=@t[-3]+0;#svm_mat(:,11969) participants 9 or 10
        $out4=@t[-2]+0;#svm_mat(:,11970) code ?
        $out5=@t[-1]+0;#svm_mat(:,11971) code Turid?
	for($c=0;$c<=$#t-6;$c++){
		$out=@t[$c]+0;
		print FO"$out,";
		if($out0==1){print FOR1"$out,";$cnt{"$c-$out3-$out4"}++;}
		if($out0==2){print FOR2"$out,";$cnt{"$c-$out3-$out4"}++}
		if($out0==3){print FOR3"$out,";$cnt{"$c-$out3-$out4"}++}
	}
	$out=@t[$c]+0; #svm_mat(:,11966) 
	print FO"G$out2\n";
	if($out0==1){print FOR1"G$out2\n";print FOR"$c-$out1-$out2-$out3-$out4\t";}
	if($out0==2){print FOR2"G$out2\n";print FOR"$c-$out1-$out2-$out3-$out4\t";}
	if($out0==3){print FOR3"G$out2\n";print FOR"$c-$out1-$out2-$out3-$out4\t";}
	if($line%57==0){print FOR"\n";}
	print "$line $out\tT$out0\tR$out1\tC$out2\tP$out3\tID$out4\tIDT$out5\n";
}
close F2;

