#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#    Code base of Animesh Sharma [ sharma.animesh@gmail.com ]

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
	 $seqf=$strand{$direction};
	 while ($seqf=~m/(ATG)/gi) {
         	push @starts,pos($seqf)-2;
	 	}
 	 while ($seqf=~m/(TAA|TGA|TAG)/gi){
      		push @ends,pos($seqf)-2;
	 	}
		@starts=sort {$a<=>$b} (@starts);
		@ends=sort {$a<=>$b} (@ends);
	for ($e1=0;$e1<=$#ends;$e1++){$e=@ends[$e1];$ss1=0;
	
 		for ($s1=$ss1;$s1<=$#starts;$s1++){$s=@starts[$s1];#print "$s1\t outer loop\n";
     	   	$les=$e-$s+3;
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

  		   	for ($ss1=$s1;$ss1<=$#starts;$ss1++){#print "in the loop";
		   	$sss=@starts[$ss1];
		   		if($e < $sss){last;#print "$s1\tin the loop for\n";
				}

			}
		   }
		}

	}
	undef @starts;undef @ends;
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
