#!/usr/bin/perl

$ff=shift@ARGV;
$minorflen=shift@ARGV;
sub ORF {
   $seqs=shift;
   $snames=shift;
   $strand{"\+"} = "$seqs";
   $seqs=~tr/ATCG/TAGC/d;
   $rseqs=reverse($seqs);
   $strand{"\-"} = "$rseqs";
   foreach $direction (keys %strand) {$len=length($strand{$direction});
         #for ( $frame=1;$frame<=3;$frame++) {
	 #$seqf=substr($strand{$direction},$frame-1,$len-$frame+1);#}
	 #foreach (keys %seqf) {print "$_ $direction $len \n$seqf{$_}\n";}
      	 #foreach (keys %seqf) {
	 $seqf=$strand{$direction};
	 while ($seqf=~m/(ATG)/gi) {
         	push @starts,pos($seqf)-2;
	 	      			}
 	 while ($seqf=~m/(TAA|TGA|TAG)/gi) {
      		push @ends,pos($seqf)-2;
	 		}
	 for $s (@starts){
      	   for $e (@ends)  	{$les=$e-$s+3;
		if ($e%3==$s%3 and $e>$s and $les>=$minorflen){
		$rems=$s%3;
	   	if($rems == 0){$frame=3;}else{$frame=$rems;}
		$es=substr($seqf,$s-1,$les);
		if($direction eq "\+")
		{
		$ts=$s+$frame-1;$te=$e+$frame-1;
	   	print "$snames\tD-$direction F-$frame S-$ts E-$te  L-$les \n$es\n";
	 	}
		elsif($direction eq "\-")
		{
		$ts=$len-$s-$frame+2;$te=$len-$e-$frame+2;
	   	print "$snames\tD-$direction F-$frame S-$ts E-$te  L-$les \n$es\n";
	 	}
#						}
				}
			}
		undef @starts;undef @ends;
	}
	#}undef %seqf;
  }
}
open(F2,$ff);
while ($line = <F2>) {
        chomp ($line);
        if ($line =~ /^>/){
             push(@seqname,$line);
                if ($seq ne ""){
              push(@seq,$seq);
              $seq = "";
            }
      } else {$seq=$seq.$line;
      }
}
push(@seq,$seq);
for($c1=0;$c1<=$#seq;$c1++){
	        @temp2=split(/\s+/,@seqname[$c1]);$seqc=uc(@seq[$c1]);
	        $t2=@temp2[0];
		ORF($seqc,$t2);
		}
close F2;
