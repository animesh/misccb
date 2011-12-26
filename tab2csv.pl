$file2=shift @ARGV;
open(F2,$file2);
$fout="$file2.csv";
open(FO,">$fout");
$vtotal=11965;
%$vtotal=4;
$stotal=15;
%$stotal=2;
while($l=<F2>){
        $l=~s/^\s+//;
        $l=~s/\s+$//;
        @t=split(/\t/,$l);
        @t=split(/\s+/,$l);
        $line++;
        if($line==1){
                        for($c=0;$c<$#t;$c++){
                                $cp=$c+1;
				$subj=($c%$stotal)+1;
				$voxel=int($c/$stotal)+1;
				%$voxel=int($cp/$vtotal-1)+1;
                                print FO $cp,"S",$subj,"V",$voxel,",";
                        }
                        print FO"CLASS\n";
        }
        for($c=0;$c<$#t;$c++){
                $out=@t[$c]+0;
                print FO"$out,";
        }
        $out=@t[$c]+0;
        print FO"$out\n";
        print "$line Class $out\n";
}
close F2;

