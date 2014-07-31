$file2=shift @ARGV;
open(F2,$file2);
$fout="$file2.id.csv";
$foutr11="$file2.id.r1.t1.csv";
$foutr21="$file2.id.r2.t1.csv";
$foutr31="$file2.id.r3.t1.csv";
$foutr12="$file2.id.r1.t2.csv";
$foutr22="$file2.id.r2.t2.csv";
$foutr32="$file2.id.r3.t2.csv";
$foutr13="$file2.id.r1.t3.csv";
$foutr23="$file2.id.r2.t3.csv";
$foutr33="$file2.id.r3.t3.csv";
$foutr="$file2.id.r";
open(FO,">$fout");
open(FOR11,">$foutr11");
open(FOR21,">$foutr21");
open(FOR31,">$foutr31");
open(FOR12,">$foutr12");
open(FOR22,">$foutr22");
open(FOR32,">$foutr32");
open(FOR13,">$foutr13");
open(FOR23,">$foutr23");
open(FOR33,">$foutr33");
open(FOR,">$foutr");
while($l=<F2>){
	$l=~s/^\s+//;
	$l=~s/\s+$//;
	@t=split(/\,/,$l);
	$line++;
        $out0=@t[-6]+0;#svm_mat(:,11966) time
        $out1=@t[-5]+0;#svm_mat(:,11967) reps
        $out2=@t[-4]+0;#svm_mat(:,11968) ? 1 or 2
        $out3=@t[-3]+0;#svm_mat(:,11969) participants 9 or 10
        $out4=@t[-2]+0;#svm_mat(:,11970) code ?
        $out5=@t[-1]+0;#svm_mat(:,11971) code Turid?
        if($out0==1 && $out1==1){print FOR11"$line,$out0,$out1,$out2,$out3,$out4,$out5\n";}
        if($out0==2 && $out1==1){print FOR21"$line,$out0,$out1,$out2,$out3,$out4,$out5\n";}
        if($out0==3 && $out1==1){print FOR31"$line,$out0,$out1,$out2,$out3,$out4,$out5\n";}
        if($out0==1 && $out1==2){print FOR12"$line,$out0,$out1,$out2,$out3,$out4,$out5\n";}
        if($out0==2 && $out1==2){print FOR22"$line,$out0,$out1,$out2,$out3,$out4,$out5\n";}
        if($out0==3 && $out1==2){print FOR32"$line,$out0,$out1,$out2,$out3,$out4,$out5\n";}
 	if($out0==1 && $out1==3){print FOR13"$line,$out0,$out1,$out2,$out3,$out4,$out5\n";}
	if($out0==2 && $out1==3){print FOR23"$line,$out0,$out1,$out2,$out3,$out4,$out5\n";}
	if($out0==3 && $out1==3){print FOR33"$line,$out0,$out1,$out2,$out3,$out4,$out5\n";}
	print "$line $out\tT$out0\tR$out1\tC$out2\tP$out3\tID$out4\tIDT$out5\n";
}
close F2;

