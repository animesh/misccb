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

if( @ARGV ne 1){die "\nUSAGE\t\"ProgName MultSeqFile\t\n\n\n";}
$file = shift @ARGV;
use Math::Complex;
$pi=pi;
$i=sqrt(-1);
open (F, $file) || die "can't open \"$file\": $!";
$seq="";while ($line = <F>) {
        chomp ($line);
        if ($line =~ /^>/){
             #@seqn=split(/\t/,$line);$snames=@seqn[0];$snames=~s/>//;1/1;
				$snames=$line;
                chomp $snames;
             push(@seqname,$snames);
                if ($seq ne ""){
              push(@seq,$seq);
              $seq = "";
            }
      } else {$seq=$seq.$line;
      }
}
push(@seq,$seq);close F;
for($c1=0;$c1<=$#seq;$c1++)
{
$seq=uc(@seq[$c1]);
$sname=@seqname[$c1];
#$foo=$sname."ft"."\.out";
#open FO,">$foo";
$N=length($seq);
	if($c1<12 and $c1>=0){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				$s=$t;$l=length($s);
				@t2=split(//,$s);
				for($c2=0;$c2<=$#t2;$c2++){
					for($c3=($c2+1);$c3<=$#t2;$c3++){
						$tag=@t2[$c2].':'.@t2[$c3];
						$aado1{$tag}.=":".($c3-$c2);
						$rtag=reverse($tag);
						$aado1{$rtag}.=":".($c3-$c2);
					}
				}
				$s="";
			}
		}
	}
	undef %aad;
	if($c1<=19 and $c1>=12){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				$s=$t;$l=length($s);
				@t2=split(//,$s);
				for($c2=0;$c2<=$#t2;$c2++){
					for($c3=($c2+1);$c3<=$#t2;$c3++){
						$tag=@t2[$c2].':'.@t2[$c3];
						$aado2{$tag}.=":".($c3-$c2);
						$rtag=reverse($tag);
						$aado2{$rtag}.=":".($c3-$c2);
					}
				}
				$s="";
			}
		}
	}	
	undef %aad;
	if($c1<=31 and $c1>=20){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				$s=$t;$l=length($s);
				@t2=split(//,$s);
				for($c2=0;$c2<=$#t2;$c2++){
					for($c3=($c2+1);$c3<=$#t2;$c3++){
						$tag=@t2[$c2].':'.@t2[$c3];
						$aado3{$tag}.=":".($c3-$c2);
						$rtag=reverse($tag);
						$aado3{$rtag}.=":".($c3-$c2);
					}
				}
				$s="";
			}
		}
	}	
	undef %aad;
	if($c1<=38 and $c1>=32){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				$s=$t;$l=length($s);
				@t2=split(//,$s);
				for($c2=0;$c2<=$#t2;$c2++){
					for($c3=($c2+1);$c3<=$#t2;$c3++){
						$tag=@t2[$c2].':'.@t2[$c3];
						$aado4{$tag}.=":".($c3-$c2);
						$rtag=reverse($tag);
						$aado4{$rtag}.=":".($c3-$c2);
					}
				}
				$s="";
			}
		}
	}	
	undef %aad;
	if($c1<=47 and $c1>=39){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				$s=$t;$l=length($s);
				@t2=split(//,$s);
				for($c2=0;$c2<=$#t2;$c2++){
					for($c3=($c2+1);$c3<=$#t2;$c3++){
						$tag=@t2[$c2].':'.@t2[$c3];
						$aado5{$tag}.=":".($c3-$c2);
						$rtag=reverse($tag);
						$aado5{$rtag}.=":".($c3-$c2);
					}
				}
				$s="";
			}
		}
	}	
	undef %aad;
	if($c1<=54 and $c1>=48){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				$s=$t;$l=length($s);
				@t2=split(//,$s);
				for($c2=0;$c2<=$#t2;$c2++){
					for($c3=($c2+1);$c3<=$#t2;$c3++){
						$tag=@t2[$c2].':'.@t2[$c3];
						$aado6{$tag}.=":".($c3-$c2);
						$rtag=reverse($tag);
						$aado6{$rtag}.=":".($c3-$c2);
					}
				}
				$s="";
			}
		}
	}	
	undef %aad;
	if($c1<=84 and $c1>=55){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				$s=$t;$l=length($s);
				@t2=split(//,$s);
				for($c2=0;$c2<=$#t2;$c2++){
					for($c3=($c2+1);$c3<=$#t2;$c3++){
						$tag=@t2[$c2].':'.@t2[$c3];
						$aadt1{$tag}.=":".($c3-$c2);
						$rtag=reverse($tag);
						$aadt1{$rtag}.=":".($c3-$c2);
					}
				}
				$s="";
			}
		}
	}	
	undef %aad;
	if($c1<=93 and $c1>=85){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				$s=$t;$l=length($s);
				@t2=split(//,$s);
				for($c2=0;$c2<=$#t2;$c2++){
					for($c3=($c2+1);$c3<=$#t2;$c3++){
						$tag=@t2[$c2].':'.@t2[$c3];
						$aadt2{$tag}.=":".($c3-$c2);
						$rtag=reverse($tag);
						$aadt2{$rtag}.=":".($c3-$c2);
					}
				}
				$s="";
			}
		}
	}	
	undef %aad;
	if($c1<=109 and $c1>=94){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				$s=$t;$l=length($s);
				@t2=split(//,$s);
				for($c2=0;$c2<=$#t2;$c2++){
					for($c3=($c2+1);$c3<=$#t2;$c3++){
						$tag=@t2[$c2].':'.@t2[$c3];
						$aadt3{$tag}.=":".($c3-$c2);
						$rtag=reverse($tag);
						$aadt3{$rtag}.=":".($c3-$c2);
					}
				}
				$s="";
			}
		}
	}	
	undef %aad;
	if($c1<=116 and $c1>=110){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				%aadt4=aadist($t);
			}
		}
	}	
	undef %aad;
	if($c1<=124 and $c1>=117){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				%aadt5=aadist($t);
			}
		}
	}	
	undef %aad;
	if($c1<=137 and $c1>=125){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				%aadt6=aadist($t);
			}
		}
	}	
	undef %aad;
	if($c1<=145 and $c1>=138){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				%aadt7=aadist($t);
			}
		}
	}	
	undef %aad;
	if($c1<=154 and $c1>=146){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				%aadt8=aadist($t);
			}
		}
	}	
	undef %aad;
	if($c1<=163 and $c1>=155){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				%aadt9=aadist($t);
			}
		}
	}	
	undef %aad;
	if($c1<=192 and $c1>=164){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				%aadth1=aadist($t);
			}
		}
	}	
	undef %aad;
	if($c1<=203 and $c1>=193){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				%aadth2=aadist($t);
			}
		}
	}	
	undef %aad;
	if($c1<=214 and $c1>=204){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				%aadth3=aadist($t);
			}
		}
	}	
	undef %aad;
	if($c1<=227 and $c1>=215){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				%aadth4=aadist($t);
			}
		}
	}	
	undef %aad;
	if($c1<=237 and $c1>=228){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				%aadth5=aadist($t);
			}
		}
	}	
	undef %aad;
	if($c1<=246 and $c1>=238){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				%aadth6=aadist($t);
			}
		}
	}	
	undef %aad;
	if($c1<=256 and $c1>=247){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				%aadth7=aadist($t);
			}
		}
	}	
	undef %aad;
	if($c1<=267 and $c1>=257){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				%aadth8=aadist($t);
			}
		}
	}	
	undef %aad;
	if($c1<=278 and $c1>=268){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				%aadth9=aadist($t);
			}
		}
	}	
	undef %aad;
	if($c1<=285 and $c1>=279){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				%aadf1=aadist($t);
			}
		}
	}	
	undef %aad;
	if($c1<=298 and $c1>=286){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				%aadf2=aadist($t);
			}
		}
	}	
	undef %aad;
	if($c1<=312 and $c1>=299){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				%aadf3=aadist($t);
			}
		}
	}	

}


sub aadist{
	$s=shift;$l=length($s);
	@t2=split(//,$s);
	for($c2=0;$c2<=$#t2;$c2++){
		for($c3=($c2+1);$c3<=$#t2;$c3++){
			$tag=@t2[$c2].':'.@t2[$c3];
			$aad{$tag}.=":".($c3-$c2);
			$rtag=reverse($tag);
			$aad{$rtag}.=":".($c3-$c2);
		}
	}
	$s="";
	return %aad;
}

foreach $q (sort keys %aado1){
	$t=$q;
	$t=~s/-/ /g;
	print "$t\t$aado2{$q}\n";
}

#%t2o = (
#      'ALA' => 'A',
#      'VAL' => 'V',
#      'LEU' => 'L',
#      'ILE' => 'I',
#      'PRO' => 'P',
#      'TRP' => 'W',
#      'PHE' => 'F',
#      'MET' => 'M',
#      'GLY' => 'G',
#      'SER' => 'S',
#      'THR' => 'T',
#      'TYR' => 'Y',
#      'CYS' => 'C',
#      'ASN' => 'N',
#      'GLN' => 'Q',
#      'LYS' => 'K',
#      'ARG' => 'R',
#      'HIS' => 'H',
#      'ASP' => 'D',
#      'GLU' => 'E',
#    );
#while($l=<F>){
#$s="";
#	if($l=~/^SEQRES/){
#		@t=split(//,$l);
#		for($c1=19;$c1<=67;$c1=$c1+4) {
#		$cs{@t[11]}.=$t2o{@t[$c1].@t[$c1+1].@t[$c1+2]};
#		$s.=$t2o{@t[$c1]};
#		}
#	}
#	$seq.=$s;$s="";
#
#if($l=~/^HELIX/){
#		@t=split(//,$l);
#		$str=@t[21].@t[22].@t[23].@t[24]+0;
#		$stp=@t[33].@t[34].@t[35].@t[36]+0;
#		$s=substr($cs{@t[19]},($str-1),($stp-$str+1));
#		@t2=split(//,$s);
#			for($c2=0;$c2<=$#t2;$c2++){
#				$s1=@t2[$c2]."-".@t[19]."-".($str+$c2);$s2="H";
#				$aap{$s1}=$s2;
#				#print "$str\t$stp\t$s1=>$aap{$s1}\n";
#			}
#		}
#@t2=split(//,$s);
#for($c2=0;$c2<=$#t2;$c2++){
#	for($c3=($c2+1);$c3<=$#t2;$c3++){
#		$tag=@t2[$c2].'-'.@t2[$c3];
#		$helix{$tag}.="-".($c3-$c2);
#		$rtag=reverse($tag);
#		$helix{$rtag}.="-".($c3-$c2);
#	}
#}
#$s="";
#
#if($l=~/^SHEET/){
#		@t=split(//,$l);
#		$str=@t[22].@t[23].@t[24].@t[25]+0;
#		$stp=@t[33].@t[34].@t[35].@t[36]+0;
#		$s=substr($cs{@t[21]},($str-1),($stp-$str+1));
#		@t2=split(//,$s);
#			for($c2=0;$c2<=$#t2;$c2++){
#				$s1=@t2[$c2]."-".@t[21]."-".($str+$c2);$s2="S";
#				$aap{$s1}=$s2;
#				#print "$str\t$stp\t$s1=>$aap{$s1}\n";
#			}
#		}
#@t2=split(//,$s);
#for($c2=0;$c2<=$#t2;$c2++){
#	for($c3=($c2+1);$c3<=$#t2;$c3++){
#		$tag=@t2[$c2].'-'.@t2[$c3];
#		$sheet{$tag}.="-".($c3-$c2);
#		$rtag=reverse($tag);
#		$sheet{$rtag}.="-".($c3-$c2);
#
#	}
#}
#$s="";
#
#if($l=~/^TURN/){
#		@t=split(//,$l);
#		$str=@t[20].@t[21].@t[22].@t[23]+0;
#		$stp=@t[31].@t[32].@t[33].@t[34]+0;
#		$s=substr($cs{@t[19]},($str-1),($stp-$str+1));
#		@t2=split(//,$s);
#			for($c2=0;$c2<=$#t2;$c2++){
#				$s1=@t2[$c2]."-".@t[19]."-".($str+$c2);$s2="T";
#				$aap{$s1}=$s2;
#				#print "$str\t$stp\t$s1=>$aap{$s1}\n";
#			}
#		}
#@t2=split(//,$s);
#for($c2=0;$c2<=$#t2;$c2++){
#	for($c3=($c2+1);$c3<=$#t2;$c3++){
#		$tag=@t2[$c2].'-'.@t2[$c3];
#		$turn{$tag}.="-".($c3-$c2);
#		$rtag=reverse($tag);
#		$turn{$rtag}.="-".($c3-$c2);
#	}
#}
#}
#close F;
##$l=length($seq);print "$l\n$seq\n";$c=0;
#$c=0;print "HELIX\n";
#foreach (keys %helix){$c++;
#	print "$c\t$_ => $helix{$_}\n";
#}
#
#$c=0;print "SHEET\n";
#foreach (keys %sheet){$c++;
#	print "$c\t$_\t$sheet{$_}\n";
#}
#
#$c=0;print "TURN\n";
#foreach (keys %turn){$c++;
#	print "$c\t$_\t$turn{$_}\n";
#	}

#$c1=0;$c2=0;
#foreach $w1 (keys %helix){$c1++;$c2=0;
#	foreach $w2 (keys %sheet){$c2++;
#		if($w1 eq $w2){
#		$val=SNR($helix{$w1},$sheet{$w2});
#		$key="$w1\t$helix{$w1}\t$sheet{$w2}";
#		$sum{$key}=$val;
##		print "$w1\t$\"c1:$c2\"\t\"$helix{$w1}:$sheet{$w2}\"\t$val\n";
#		}
#	}
##}
#foreach $q (sort {$sum{$b} <=> $sum{$a}} keys %sum){
#	$t=$q;
#	$t=~s/-/ /g;
#	print "$t\t$sum{$q}\n";
#	}
#foreach $q (sort {$aad{$b} <=> $aad{$a}} keys %aad){

sub SNR{
	$val1=shift;
	$val2=shift;
	
	@t1=split(/\-/,$val1);
	$length=@t1;$N=$length-1;#print "$N\n";
	for($c2=1;$c2<=$#t1;$c2++){
		$temp3+=@t1[$c2];
	}
	$temp3=$temp3/$N;
	for($c2=1;$c2<=$#t1;$c2++){
		$temp5+=($temp3-@t1[$c2])**2;
	}
	$temp5=sqrt($temp5/$N);

	@t1=split(/\-/,$val2);
	$length=@t1;$N=$length-1;#print "$N\n";
	for($c2=1;$c2<=$#t1;$c2++){
		$temp2+=@t1[$c2];
	}
	$temp2=$temp2/$N;
	for($c2=1;$c2<=$#t1;$c2++){
		$temp4+=($temp2-@t1[$c2])**2;
	}
	$temp4=sqrt($temp4/$N);

	if($temp4 eq $temp5){$temp1=1;}
	else{
		$temp1=abs(($temp2-$temp3)/($temp4+$temp5));
		}
	$temp2=0;$temp3=0;$temp4=0;$temp5=0;$temp6=0;$temp7=0;
	return $temp1;
}