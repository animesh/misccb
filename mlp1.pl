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
$thres=0.001;
#$minval=shift @ARGV;
$sbcont=1000;
$clusd=2;
$nDim=$ftr;
$nData=$row;
$nClass=$c3-$ftr;
$iter=1000;
$eta=0.5;
$hlayers=1;
$nHiddenUnits=10;
$pstatus=10;
for($c1=0;$c1<$hlayers;$c1++){
	@HL[$c1]=$nHiddenUnits;
}

INITIALIZE();

LEARNING();

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
	for($c1=0;$c1<$hlayers;$c1++){
		$nHiddenUnits=@HL[$c1];
		if($c1==0){
			for($c2=0;$c2<$nDim;$c2++){
				for($c3=0;$c3<$nHiddenUnits;$c3++){
					$weight12[$c2][$c3] = (rand(1)-0.5);#$t++;
					#print "$t\t$weight12[$c2][$c3]\t";
				}
			}
		}
		if(($hlayers-$c1)==1){
			for($c2=0;$c2<$nClass;$c2++){
				for($c3=0;$c3<$nHiddenUnits;$c3++){
					$weight23[$c2][$c3] = (rand(1)-0.5);#$t++;
					#print "$t\t$weight12[$c2][$c3]\t";
				}
			}
		}		
	}
}

sub LEARNING{$iterno=0;
	while($iterno <= $iter){
		$squareerror =0;
		for( $datarow = 0; $datarow < $nData; $datarow++){
			FORWARD($datarow);
			for($c1 = 0; $c1 < $nClass; $c1++){
			$squareerror+=(@output3[$c1]-$target[$datarow][$c1])**2;
			}
		}
		if($iterno%($pstatus)==0){
		   print "SSE after iteration-$iterno:\t$squareerror\t";
		   MISCLASSIFICATION();
		   $squareerror =0;
		} 

	   for( $datarow = 0; $datarow < $nData; $datarow++){
			FORWARD($datarow);
			for( $k = 0; $k < $nHiddenUnits; $k++){
				@delta2[$k] = 0;
			}
			for( $i = 0; $i < $nClass; $i++){
			   @error[$i]=@output3[$i]-$target[$datarow][$i];
			}
			for( $i = 0; $i < $nClass; $i++){
				for( $k = 0; $k < $nHiddenUnits; $k++){
					$weight23[$i][$k]-=($eta*@error[$i]*@output2[$k]*@output3[$i]*(1-@output3[$i]));
				}
			}
			for($k = 0; $k < $nHiddenUnits; $k++){
				for( $i = 0; $i < $nClass; $i++){ 
						  @delta2[$k]+=(@error[$i]*$weight23[$i][$k]*@output3[$i]*(1-@output3[$i]));
				}
			}

			for($i=0; $i<$nDim; $i++){
				for($j=0;$j<$nHiddenUnits;$j++){
					  $weight12[$i][$j]-=($eta*@delta2[$j]*$data[$datarow][$i]*(1-@output2[$j])*@output2[$j]); 
				}
			}
		 }
	$iterno++;
	}
}

sub FORWARD{
$dataNo=shift;
	for ($kf = 0; $kf < $nHiddenUnits; $kf++){
	      @output2[$kf] = CALOP1($kf,$dataNo);
	}
	for( $if = 0; $if < $nClass; $if++){
          @output3[$if] = CALOP2($if);
	}
}

sub CALOP1{
$hiddenUnitID=shift;
$datarow=shift;
$temp1=0;
   for($ic1=0;$ic1<$nDim;$ic1++){
      $temp1+=$weight12[$ic1][$hiddenUnitID]*$data[$datarow][$ic1];#/*atten[n];*/
   }
   $return1 = 1/(1 + exp(-($temp1)));
  return($return1);
}



sub CALOP2{
$classID=shift;$temp2=0;
    for( $kc2 = 0; $kc2 < $nHiddenUnits ; $kc2++){
      $temp2+=$weight23[$classID][$kc2]*@output2[$kc2];
	}
  $return2 = 1/(1 + exp(-$temp2));

  return($return2);
} 


sub MISCLASSIFICATION{
   $misclas=0;	
   for($r1=0; $r1 < $nData; $r1++){
	   FORWARD($r1);
        for($ims=0; $ims< $nClass; $ims++){
			if($output3[$ims]>$max){ 
				$max = $output3[$ims];
                $label = $ims;
			}
		}
		if($target[$r1][$label]!=1){$misclas++;}
      $max =0; $label =0;
     }

  printf "MISCLASSIFICATION-$misclas\n";
}

#%seen=();
#@lab = (grep{ !$seen{$_} ++} @lab);#undef @all;
#@lab = (sort {$a <=> $b} @lab);
#$elem=0;
#$totelem=0;
#for($a1=0;$a1<=$#lab;$a1++){
#	$elem=$clsno{@lab[$a1]};
#	for($c1=0;$c1<$elem;$c1++){
#		if($labhash{($c1+$totelem+1)} eq @lab[$a1]){
#			for($c2=0;$c2<$ftr;$c2++){
#				$mat[$c1][$c2]=$matold[($c1+$totelem)][$c2];
#			}
#		}
#	}
#	$totelem+=$clsno{@lab[$a1]};
#	$retfrm=MAIN();
#	$classnumber=$a1+1;
#
##Finding the Maxcol from Sample Belonginness
#		for($s1=0;$s1<$c1;$s1++){$max=0;$maxcol=$s2;
#			for($s2=0;$s2<$clusd;$s2++){
#				if($otprn[$s1][$s2]>$max){
#					$max=$otprn[$s1][$s2];$maxcol=$s2;
#				}
#			}
#			$maxcolhash{$maxcol}++;$maxcolsam{$s1}=$maxcol;	
#		}
#
##Finding the Mean,Variance and Sample Belonginness and storing in 3 dimensional array
#		foreach(keys %maxcolhash){
#			for($s2=0;$s2<$ftr;$s2++){
#				for($s1=0;$s1<$c1;$s1++){
#					if($_ ==$maxcolsam{$s1}){
#						$meanval+=$mat[$s1][$s2];$N++;
#					}
#				}
#			$meanval{$s2}=$meanval/$N;
#			$meanval=0;$N=0;
#			}
#			for($s2=0;$s2<$ftr;$s2++){
#				for($s1=0;$s1<$c1;$s1++){
#					if($_ ==$maxcolsam{$s1}){
#						$stddevval+=($mat[$s1][$s2]-$meanval{$s2})**2;$N++;
#						$stddevvalsb+=($mat[$s1][$s2]-$otprnm[$s2][$_])**2;
#					}
#				}
#			$stddevval{$s2}=sqrt($stddevval)/$N;
#			$stddevvalsb{$s2}=sqrt($stddevvalsb)/$N;
#			$stddevval=0;$N=0;$stddevvalsb=0;
#			#print "$meanval{$s2}\t";#$otprnm[$s2][$_]\t$stddevval{$s2}\t$stddevvalsb{$s2}\t";#$stddevval{$s2}\t
#			$trainvalmean[$a1][$_][$s2]=$meanval{$s2};
#			$trainvalstd[$a1][$_][$s2]=$stddevval{$s2};
#			$trainvalcen[$a1][$_][$s2]=$otprnm[$s2][$_];
#			$trainvalstdcen[$a1][$_][$s2]=$stddevvalsb{$s2};
#			print "$trainvalmean[$a1][$_][$s2]\t";#$trainvalcen[$a1][$_][$s2]\t$trainvalstd[$a1][$_][$s2]\t$trainvalstdcen[$a1][$_][$s2]\t";
#			}
#			print "$_\t$maxcolhash{$_}\n";
#		}
#		undef %maxcolhash;undef %maxcolsam;
#		undef %stddevvalsb;undef %stddevval;
#		undef %meanval;#undef %stddevval;
#
#}
#
##foreach(sort {$a<=>$b} @lab){print "$_\t$clsno{$_}\n";}
#sub MAIN{
#	$iterval=0;
#	$sbcont=$minval+1;
#	$gc=0;
#	$sb=0;
#	$ed=0;
#	$gcont=0;
#	$edcon=0;
#	#generate random matrix
#	if($iterval==0){
#		for($s1=0;$s1<$c1;$s1++){
#			for($s2=0;$s2<$clusd;$s2++){
#				$otpr[$s1][$s2]=rand(1);
#				$cnt+=$otpr[$s1][$s2];
#				}
#				@sum[$s1]=$cnt;
#				$cnt=0;
#
#		}
#
#		#normalise random matrix
#		for($s1=0;$s1<$c1;$s1++){
#			for($s2=0;$s2<$clusd;$s2++){
#				$otprn[$s1][$s2]=$otpr[$s1][$s2]/@sum[$s1];
#				$cnt+=$otprn[$s1][$s2];
#				#print "$otprn[$s1][$s2]\t";
#				}
#				#print "$otprn[$s1][$s2]\n";
#				$cnt=0;
#		}
#		$iterval++;
#	}
#
#	while($iterval<=$iter){
#
#		if($sbcont>=$minval){
#			$gcont=GC();
#			$edcont=ED();
#			$sbcont=SB();
#			print "	$minval-$iter: $gcont\t$edcont\t$sbcont \n";
#		}
#		else{
#			#print "	$minval-$iter: $gcont\t$edcont\t$sbcont \n";
#			#PREDI();
#			$iterval=$iter+1;
#		}
#			$iterval++;
#
#	}
#	return("$iterval");
#}
#	#generating centroid
#	sub GC {
#		$gc++;
#		for($s1=0;$s1<$clusd;$s1++){
#			for($s2=0;$s2<$ftr;$s2++){
#				for($s3=0;$s3<$c1;$s3++){
#					$val1+=($otprn[$s3][$s1]**$m)*$mat[$s3][$s2];
#					$val2+=($otprn[$s3][$s1]**$m);
#				}
#			if($val2!=0){
#				$otprnm[$s2][$s1]=$val1/$val2;
#				$val1=0;$val2=0;
#			}
#			else{$otprnm[$s2][$s1]=0;}
#			}
#		}
#		return $gc;
#	}
#		#calculating euclidean distance
#	sub ED{
#		$ed++;
#		for($s3=0;$s3<$c1;$s3++){	$val2=0;
#			for($s1=0;$s1<$clusd;$s1++){
#				for($s2=0;$s2<$ftr;$s2++){
#					$val1+=($otprnm[$s2][$s1]-$mat[$s3][$s2])**2;
#				}
#				$ed[$s3][$s1]=($val1);
#				$val2+=$ed[$s3][$s1];
#				$val1=0;
#			}
#			@eda[$s3]=$val2;$val2=0;
#		}
#		return $ed;
#	}				
#		#calculating each sample belongingness
#	sub SB{
#		$fp=$f.".prd";
#		open(FP,">$fp");
#		$sb++;
#		$val1=0;$val2=0;$val3=0;$val4=0;$val5=0;
#		for($s3=0;$s3<$c1;$s3++){$val1=0;$val2=0;
#			for($s1=0;$s1<$clusd;$s1++){#print "$s1\t";
#				for($s2=0;$s2<$clusd;$s2++){#print "$s1\t";
#					$otprnmn[$s3][$s1]=($ed[$s3][$s1]/$ed[$s3][$s2])**(2/($m-1));
#					$val1+=$otprnmn[$s3][$s1];
#				}
#				$val1=1/$val1;$val2+=$val1;
#				$val3=abs($otprn[$s3][$s1]-$val1);
#				$val4+=$val3;
#				$otprn[$s3][$s1]=$val1;
#				print FP"$val1\t";
#				$val1=0
#			}
#			print FP"\n";$val1=0;
#		}
#		$val5=$val4/(($s1)*$s3);
#		close FP;
#		return $val5;
#	}
#
#	sub PREDI{
#		$f1=$foo;
#		$f2=$fp;
#		open(F1,$f1);
#		open(F2,$f2);
#		undef @cmp1;undef @cmp2;$c1=0;$c2=0;$c3=0;
#		while($l1=<F1>){chomp $l1;
#		@t1=split(/\s+/,$l1);
#				for($c1=0;$c1<=$#t1;$c1++){
#					$h1{@t1[$c1]}=($c1+1);
#					if(@t1[$c1]>$m1){
#						$m1=@t1[$c1];
#						}
#				}
#				push(@cmp1,$h1{$m1});$m1=0;undef %h1;
#		}
#		close F1;
#		while($l2=<F2>){chomp $l2;
#		@t2=split(/\s+/,$l2);
#				for($c2=0;$c2<=$#t2;$c2++){
#					$h2{@t2[$c2]}=($c2+1);
#					if(@t2[$c2]>$m2){
#						$m2=@t2[$c2];
#						}
#				}
#				push(@cmp2,$h2{$m2});$m2=0;undef %h2;
#		}
#		close F2;		
#		print "Sample#\tActual\tObtained\n";
#		for($c2=0;$c2<=$#cmp2;$c2++){$c5=$c2+1;
#			$mct{@cmp1[$c2]}++;
#			if(@cmp1[$c2]!=@cmp2[$c2]){
#				$mc{@cmp1[$c2]}++;
#				print "$c5\t@cmp1[$c2]\t@cmp2[$c2]\n";
#				$c3++;
#			}
#		}
#
#		print "\nCLASS\tMISCLASSIFICATIONIFICATION: TOTAL MEMBERS\tPERCENT\n";
#
#		for($c10=1;$c10<=$c1;$c10++){
#			if($mct{$c10}==0 or $mc{$c10}==0){
#				$per=0;
#				print "$c10\t0\t\t : $mct{$c10}\t\t\t$per\n";
#			}
#			else{
#			$per=$mc{$c10}/$mct{$c10}*100;
#			print "$c10\t$mc{$c10}\t\t : $mct{$c10}\t\t\t$per\n";
#			}
#		}
#		$tper=($c3/$c5)*100;
#		print "\nTotal MISCLASSIFICATIONification\t-> $c3 : $c5\t\t$tper\n";
#	}