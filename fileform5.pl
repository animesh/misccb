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
$file1=shift @ARGV;
chomp $file1;
open F1,$file1;

$ftr=shift @ARGV;
chomp $file1;

$file3=shift @ARGV;
chomp $file3;
open F3,$file3;

$file2=shift @ARGV;
chomp $file2;
open F2,$file2;
$row=0;

$all=27;
$aml=11;
sub REMHASH{
	undef %max;undef %min;undef %mean;undef %std;undef %max;undef %max;
}

while($l=<F1>){
@t=split(/\s+/,$l);
	for($c=0;$c<$ftr;$c++){
		$mat[$row][$c]=@t[$c];
	}
	$row++;
}
close F1;
#print "$all\t$c\n";
#$row=27;
for($c2=0;$c2<$ftr;$c2++){#$min{$c2}=0;$max{$c2}=0;
	for($c1=0;$c1<$all;$c1++){$mat[$c1][$c2]+=0;
		$meanoldall{$c2}+=($mat[$c1][$c2]);
		if($minoldall{$c2}>=$mat[$c1][$c2]){
			$minoldall{$c2}=$mat[$c1][$c2];
		}
		if($maxoldall{$c2}<=$mat[$c1][$c2]){
			$maxoldall{$c2}=$mat[$c1][$c2];
			#print "$mat[$c1][$c2]-$max\t";
		}
		#print "$mat[$c1][$c2]\t";
	}
	#print "\n";
	$meanoldall{$c2}=$meanoldall{$c2}/$all;

	for($c1=0;$c1<$all;$c1++){
		$stdoldall{$c2}+=(($meanoldall{$c2}-$mat[$c1][$c2])**2);
	}
	$stdoldall{$c2}=sqrt($stdoldall{$c2}/$all);
}

for($c2=0;$c2<$ftr;$c2++){
	#print "Mean-$mean{$c2}\tStd-$std{$c2}\tMin- $min{$c2}\tMax-$max{$c2}\n";
}
	REMHASH();

#print "$aml\t$c\n";
for($c2=0;$c2<$ftr;$c2++){#$min{$c2}=0;$max{$c2}=0;
	for($c1=$all;$c1<($aml+$all);$c1++){$mat[$c1][$c2]+=0;
		$meanoldaml{$c2}+=($mat[$c1][$c2]);
		if($minoldaml{$c2}>=$mat[$c1][$c2]){
			$minoldaml{$c2}=$mat[$c1][$c2];
		}
		if($maxoldaml{$c2}<=$mat[$c1][$c2]){
			$maxoldaml{$c2}=$mat[$c1][$c2];
			#print "$mat[$c1][$c2]-$max\t";
		}
		#print "$mat[$c1][$c2]\t";
	}
	#print "\n";
	$meanoldaml{$c2}=$meanoldaml{$c2}/$aml;

	for($c1=$all;$c1<($aml+$all);$c1++){
		$stdoldaml{$c2}+=(($meanoldaml{$c2}-$mat[$c1][$c2])**2);
	}
	$stdoldaml{$c2}=sqrt($stdoldaml{$c2}/$aml);
}
for($c2=0;$c2<$ftr;$c2++){
	#print "Mean-$mean{$c2}\tStd-$std{$c2}\tMin- $min{$c2}\tMax-$max{$c2}\n";
}
	REMHASH();
$c1=0;$c2=0;
for($c2=0;$c2<$ftr;$c2++){$min{$c2}=10000000000000000;$max{$c2}=-100000000000000000;
	for($c1=0;$c1<($aml+$all);$c1++){$mat[$c1][$c2]+=0;
		$mean{$c2}+=($mat[$c1][$c2]);
		if($min{$c2}>=$mat[$c1][$c2]){
			$min{$c2}=$mat[$c1][$c2];
		}
		if($max{$c2}<=$mat[$c1][$c2]){
			$max{$c2}=$mat[$c1][$c2];
			#print "$mat[$c1][$c2]-$max\t";
		}
		#print "$mat[$c1][$c2]\t";
	}
	#print "\n";
	$mean{$c2}=$mean{$c2}/($all+$aml);

	for($c1=0;$c1<($aml+$all);$c1++){
		$std{$c2}+=(($mean{$c2}-$mat[$c1][$c2])**2);
	}
	$std{$c2}=sqrt($std{$c2}/($all+$aml));
	#print "$min{$c2}\t$max{$c2}\n";
}



$row=0;
while($l=<F3>){
@t=split(/\s+/,$l);
	for($c=0;$c<$ftr;$c++){
		$mat[$row][$c]=@t[$c];
	}
	$row++;
}
close F3;
#$row=27;
#print "$all\t$c\n";
for($c2=0;$c2<$ftr;$c2++){#$min{$c2}=0;$max{$c2}=0;
	for($c1=0;$c1<$all;$c1++){$mat[$c1][$c2]+=0;
		$meannewall{$c2}+=($mat[$c1][$c2]);
		if($minnewall{$c2}>=$mat[$c1][$c2]){
			$minnewall{$c2}=$mat[$c1][$c2];
		}
		if($maxnewall{$c2}<=$mat[$c1][$c2]){
			$maxnewall{$c2}=$mat[$c1][$c2];
			#print "$mat[$c1][$c2]-$max\t";
		}
		#print "$mat[$c1][$c2]\t";
	}
	#print "\n";
	$meannewall{$c2}=$meannewall{$c2}/$all;

	for($c1=0;$c1<$all;$c1++){
		$stdnewall{$c2}+=(($meannewall{$c2}-$mat[$c1][$c2])**2);
	}
	$stdnewall{$c2}=sqrt($stdnewall{$c2}/$all);
}

for($c2=0;$c2<$ftr;$c2++){
	#print "Mean-$mean{$c2}\tStd-$std{$c2}\tMin- $min{$c2}\tMax-$max{$c2}\n";
}
	#REMHASH();

#print "$aml\t$c\n";

for($c2=0;$c2<$ftr;$c2++){#$min{$c2}=0;$max{$c2}=0;
	for($c1=$all;$c1<($aml+$all);$c1++){$mat[$c1][$c2]+=0;
		$meannewaml{$c2}+=($mat[$c1][$c2]);
		if($minnewaml{$c2}>=$mat[$c1][$c2]){
			$minnewaml{$c2}=$mat[$c1][$c2];
		}
		if($maxnewaml{$c2}<=$mat[$c1][$c2]){
			$maxnewaml{$c2}=$mat[$c1][$c2];
			#print "$mat[$c1][$c2]-$max\t";
		}
		#print "$mat[$c1][$c2]\t";
	}
	#print "\n";
	$meannewaml{$c2}=$meannewaml{$c2}/$aml;

	for($c1=$all;$c1<($aml+$all);$c1++){
		$stdnewaml{$c2}+=(($meannewaml{$c2}-$mat[$c1][$c2])**2);
	}
	$stdnewaml{$c2}=sqrt($stdnewaml{$c2}/$aml);
}

for($c2=0;$c2<$ftr;$c2++){
	#print "Mean-$mean{$c2}\tStd-$std{$c2}\tMin- $min{$c2}\tMax-$max{$c2}\n";
}
	#REMHASH();

for($c2=0;$c2<$ftr;$c2++){$minn{$c2}=10000000000000000;$maxn{$c2}=-100000000000000000;
	for($c1=0;$c1<($aml+$all);$c1++){$mat[$c1][$c2]+=0;
		$meann{$c2}+=($mat[$c1][$c2]);
		if($minn{$c2}>=$mat[$c1][$c2]){
			$minn{$c2}=$mat[$c1][$c2];
		}
		if($maxn{$c2}<=$mat[$c1][$c2]){
			$maxn{$c2}=$mat[$c1][$c2];
			#print "$mat[$c1][$c2]-$max\t";
		}
		#print "$mat[$c1][$c2]\t";
	}
	#print "\n";
	$meann{$c2}=$meann{$c2}/($all+$aml);

	for($c1=0;$c1<($aml+$all);$c1++){
		$stdn{$c2}+=(($meann{$c2}-$mat[$c1][$c2])**2);
	}
	$stdn{$c2}=sqrt($stdn{$c2}/($all+$aml));
}


$rown=0;
while($l=<F2>){
	$rule=0;$rulc=0;$rown++;
	@t=split(/\s+/,$l);#$len=(@t);
		for($c=2;$c<=($ftr*3+1);$c=$c+3){
			#print "$c\t@t[$c]\t";
			if($l=~/^Rule/ and @t[$c]!~/-/){
				if($rulc%1==0){$rule++;}
				if($rown%2==0){
				#print "$rown:Rule:$c:$rule:@t[$c]:$meannewaml{$rule-1}:$stdnewaml{$rule-1}:$meanoldaml{$rule-1}:$stdoldaml{$rule-1}\t";
				#$X1=(@t[$c])*($maxoldaml{$rule-1}-$minoldaml{$rule-1})+$minoldaml{$rule-1};
				#$X2=(@t[$c+2])*($maxoldaml{$rule-1}-$minoldaml{$rule-1});
				$X1=(@t[$c])*($max{$rule-1}-$min{$rule-1})+$min{$rule-1};
				$X2=(@t[$c+2])*($max{$rule-1}-$min{$rule-1});
				#print "$rown:$rule:$X1:@t[$c] [$max{$rule-1} - $min{$rule-1}] [$meann{$rule-1}:$mean{$rule-1}]:$X2:@t[$c+2] [$stdn{$rule-1}:$std{$rule-1}]\n";
				#print "$rown:$rule:$X1:@t[$c] [$maxoldaml{$rule-1} - $minoldaml{$rule-1}] [$meannewaml{$rule-1}:$meanoldaml{$rule-1}]:$X2:@t[$c+2] [$stdnewaml{$rule-1}:$stdoldaml{$rule-1}]\n";
				#print "$rown:$rule:$X1:@t[$c] [$meann{$rule-1}:$mean{$rule-1}]:$X2:@t[$c+2] [$stdn{$rule-1}:$std{$rule-1}]\n";
				print "$X1\t$X2\t";
				}
				else{
				#$X1=(@t[$c])*($maxoldall{$rule-1}-$minoldall{$rule-1})+$minoldall{$rule-1};
				#$X2=(@t[$c+2])*($maxoldall{$rule-1}-$minoldall{$rule-1});
				#print "$rown:$rule:$X1:@t[$c] [$maxoldall{$rule-1} - $minoldall{$rule-1}] [$meannewall{$rule-1}:$meanoldall{$rule-1}]:$X2:@t[$c+2] [$stdnewall{$rule-1}:$stdoldall{$rule-1}]\n";
				$X1=(@t[$c])*($max{$rule-1}-$min{$rule-1})+$min{$rule-1};
				$X2=(@t[$c+2])*($max{$rule-1}-$min{$rule-1});
				#print "$rown:$rule:$X1:@t[$c] [$max{$rule-1} - $min{$rule-1}] [$meann{$rule-1}:$mean{$rule-1}]:$X2:@t[$c+2] [$stdn{$rule-1}:$std{$rule-1}]\n";
				#print "$rown:$rule:$X1:@t[$c] [$meann{$rule-1}:$mean{$rule-1}]:$X2:@t[$c+2] [$stdn{$rule-1}:$std{$rule-1}]\n";
				print "$X1\t$X2\t";
				#print "$rown:Rule:$c:$rule:@t[$c]:$meannewall{$rule-1}:$stdnewall{$rule-1}:$meanoldall{$rule-1}:$stdoldall{$rule-1}\t";
				}
				#print "MeanOld-$meanold{$c2}\tStdOld-$std{$c2}\t";
				$rulc++;
			}
		}
	print "\n";
}
close F2;
#X1=X2(Max0-MinO)+Min
#Mean1=Mean2(Max0-MinO)+Min
#Std1=Std2(Max0-MinO)
#D:\users\Animesh\me_new\frbc\res>perl fileform3.pl top5_tr.txt 5 top5_train_n.txt top5.cen.out
#D:\users\Animesh\me_new\frbc\res>perl fileform5.pl top5_tr.txt 5 top5_train_n.txt top5.cen.out