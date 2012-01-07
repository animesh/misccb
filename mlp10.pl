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
#if((@ARGV)!=2){die "USAGE: progname feature_file_with_out_vector no_of_feature\n";}
$f=shift @ARGV;
$ftr=shift @ARGV;
READDATA();
$m=2;
$thres=0.003;
#$minval=shift @ARGV;
$sbcont=1000;
$clusd=2;
$features=$ftr;
$samples=$row;
$class=$c3-$ftr;
$iter=1000;
$eta=0.5;
$hlayers=1;
$hidnodez=10;
$pstatus=1;
$gamma=(-5);
@HL[0]=$features;
for($c1=1;$c1<=$hlayers;$c1++){
	@HL[$c1]=$hidnodez;
}
@HL[$hlayers+1]=$class;
INITIALIZE();

LEARNING();

#print "SSE after iteration-$iterno:$squareerror [$invsqerr]\t";
MISCLASSIFICATION($f);

sub READDATA {
	if($ftr != "") {
		open(F,$f);
		$foo=$f.".out";
		open(FO,">$foo");
		$c1=0;$rowno=0;
		while($l1=<F>){
			@t1=split(/\s+/,$l1);
				for($c2=0;$c2<$ftr;$c2++){
					$data[$c1][$c2]=@t1[$c2]+0;
					}
				for($c3=$ftr;$c3<=$#t1;$c3++){
					$c5=$c3-$ftr;			
					$target[$c1][$c5]=@t1[$c3]+0;
					if(@t1[$c3]==1){$label=$c3-$ftr+1;}
					print FO"$target[$c1][$c5]\t";
				}
					print FO"\n";
			$row=$c1+1;
			$labhash{$row}=$label;
			$clsno{$label}++;
			push(@lab,$label);
			$c1++;$label=0;
		}
		close F;
		close FO;
	}
	else{die "Number of Feature ($ftr) is vague";}
}


sub INITIALIZE{
	
	#hidden layers
	for($c1=0;$c1<=($hlayers+1);$c1++){
		$prevnodes=@HL[$c1-1];
		$nextnodes=@HL[$c1];
		for($c2=0;$c2<$prevnodes;$c2++){
			for($c3=0;$c3<$nextnodes;$c3++){
				if($c1==0){
					$weight[$c1][$c2][$c3] = 1/(1+exp(-$gamma));#$t++;
					$gamma[$c1][$c2][$c3] = $gamma;
				}
				elsif($c1==($hlayers+1)){
					$weight[$c1][$c2][$c3] = (rand(1)-0.5);
				}
				else{
					$weight[$c1][$c2][$c3] = (rand(1)-0.5);
				}
			}
		}
	}
}

sub LEARNING{$iterno=0;$invsqerr=$class*$samples;
	while(($iterno < $iter) and ( $invsqerr >= $thres )){
		$squareerror =0;
		for( $datarow = 0; $datarow < $samples; $datarow++){
			for($c1=0;$c1<=($hlayers+1);$c1++){
				$prevnodes=@HL[$c1-1];
				$nextnodes=@HL[$c1];
				for($c3=0;$c3<$nextnodes;$c3++){
					for($c2=0;$c2<$prevnodes;$c2++){
						if($c1==0){
							$output[$c1][$c3]+=$weight[$c1][$c2][$c3]*$data[$datarow][$c2];
							#$gamma[$c1][$c2][$c3] = $gamma;
						}
						elsif($c1==($hlayers+1)){
							$output[$c1][$c3]+=$weight[$c1][$c2][$c3]*$output[$c1][$c3-1];
						}
						else{
							$output[$c1][$c3]+=$weight[$c1][$c2][$c3]*$output[$c1][$c3-1];
						}
					}
					if($c1==($hlayers+1)){
						$squareerror+=($target[$datarow][$c1]-$output[$c1][$c3]);
						@error[$c3]=($target[$datarow][$c1]-$output[$c1][$c3]);
					}
				}
			}
			for($c1=($hlayers+1);$c1>=0;$c1--){
				$prevnodes=@HL[$c1-1];
				$nextnodes=@HL[$c1];
				for($c2=($prevnodes-1);$c2>=0;$c2++){
					for($c3=($nextnodes-1);$c3>=0;$c3++){
						$weight[$c1][$c2][$c3]+=($eta*@error[$c3]*$output[$c1][$c3-1]*$output[$c1][$c3]*(1-$output[$c1][$c3]));
					}
				}
			}
		}
#		if($iterno%($pstatus)==0){
#			print "SSE after iteration-$iterno:\t$squareerror [$invsqerr]\t";
#			MISCLASSIFICATION($f);
#			for($i=0; $i<$features; $i++){
#				print FG"@weightgamma[$i]\t"; 
#			}
#			print FG"\n"; 
#		}
	$invsqerr=$squareerror/($samples*$class);
	print "SSE after iteration-$iterno:\t$squareerror [$invsqerr]\t";
	$iterno++;
	}
}


sub MISCLASSIFICATION{
		$filename=shift;
		$fop=$filename.".tpre";
		open(FP,">$fop");
   $misclas=0;	
   for($r1=0; $r1 < $samples; $r1++){
	   FORWARD($r1);
        for($ims=0; $ims< $class; $ims++){
			if($output3[$ims]>$max){ 
				$max = $output3[$ims];
                $label = $ims;
			}				
			print FP"$output3[$ims]\t";
		}
		print FP"\n";
		if($target[$r1][$label]!=1){$misclas++;}
      $max =0; $label =0;
     }

  printf "MISCLASSIFICATION-$misclas\n";
}


#TEST();
sub TEST
{  
print "Test filename? - ";
$choice=<>;chomp $choice;
		$fot=$choice.".test";
		open(FA,">$foa");
open(FT,$choice);
		while($l1=<FT>){
			@t1=split(/\s+/,$l1);
				for($c2=0;$c2<$ftr;$c2++){
					$data[$c1][$c2]=@t1[$c2]+0;
					}
				for($c3=$ftr;$c3<=$#t1;$c3++){
					$c5=$c3-$ftr;			
					$target[$c1][$c5]=@t1[$c3]+0;
					if(@t1[$c3]==1){$label=$c3-$ftr+1;}
					print FA"$target[$c1][$c5]\t";
				}
					print FA"\n";
			$row=$c1+1;
			$labhash{$row}=$label;
			$clsno{$label}++;
			push(@lab,$label);
			$c1++;$label=0;
		}
		close FT;
		$samples=$row;
   print "Misclassification on test data - \n";
   MISCLASSIFICATION($choice);
}