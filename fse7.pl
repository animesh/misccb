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
$cl=13;
$N=length($seq);
	if($c1<=12 and $c1>=0){
		$N1+=$N;
		#print "$N1\t$N\n";
		for($c=0;$c<=$N;$c=($c+(($cl-1)/2))){
		$t=substr($seq,$c,$cl);$l=length($t);
			if($l>=(($cl-1)/2)){
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
				$s=$t;$l=length($s);
				@t2=split(//,$s);
				for($c2=0;$c2<=$#t2;$c2++){
					for($c3=($c2+1);$c3<=$#t2;$c3++){
						$tag=@t2[$c2].':'.@t2[$c3];
						$aadt4{$tag}.=":".($c3-$c2);
						$rtag=reverse($tag);
						$aadt4{$rtag}.=":".($c3-$c2);
					}
				}
				$s="";
			}
		}
	}	
	undef %aad;
	if($c1<=124 and $c1>=117){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				$s=$t;$l=length($s);
				@t2=split(//,$s);
				for($c2=0;$c2<=$#t2;$c2++){
					for($c3=($c2+1);$c3<=$#t2;$c3++){
						$tag=@t2[$c2].':'.@t2[$c3];
						$aadt5{$tag}.=":".($c3-$c2);
						$rtag=reverse($tag);
						$aadt5{$rtag}.=":".($c3-$c2);
					}
				}
				$s="";
			}
		}
	}	
	undef %aad;
	if($c1<=137 and $c1>=125){
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
						$aadt6{$rtag}.=":".($c3-$c2);
					}
				}
				$s="";
			}
		}
	}	
	undef %aad;
	if($c1<=145 and $c1>=138){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				$s=$t;$l=length($s);
				@t2=split(//,$s);
				for($c2=0;$c2<=$#t2;$c2++){
					for($c3=($c2+1);$c3<=$#t2;$c3++){
						$tag=@t2[$c2].':'.@t2[$c3];
						$aadt7{$tag}.=":".($c3-$c2);
						$rtag=reverse($tag);
						$aadt7{$rtag}.=":".($c3-$c2);
					}
				}
				$s="";
			}
		}
	}	
	undef %aad;
	if($c1<=154 and $c1>=146){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				$s=$t;$l=length($s);
				@t2=split(//,$s);
				for($c2=0;$c2<=$#t2;$c2++){
					for($c3=($c2+1);$c3<=$#t2;$c3++){
						$tag=@t2[$c2].':'.@t2[$c3];
						$aadt8{$tag}.=":".($c3-$c2);
						$rtag=reverse($tag);
						$aadt8{$rtag}.=":".($c3-$c2);
					}
				}
				$s="";
			}
		}
	}	
	undef %aad;
	if($c1<=163 and $c1>=155){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				$s=$t;$l=length($s);
				@t2=split(//,$s);
				for($c2=0;$c2<=$#t2;$c2++){
					for($c3=($c2+1);$c3<=$#t2;$c3++){
						$tag=@t2[$c2].':'.@t2[$c3];
						$aadt9{$tag}.=":".($c3-$c2);
						$rtag=reverse($tag);
						$aadt9{$rtag}.=":".($c3-$c2);
					}
				}
				$s="";
			}
		}
	}	
	undef %aad;
	if($c1<=192 and $c1>=164){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				$s=$t;$l=length($s);
				@t2=split(//,$s);
				for($c2=0;$c2<=$#t2;$c2++){
					for($c3=($c2+1);$c3<=$#t2;$c3++){
						$tag=@t2[$c2].':'.@t2[$c3];
						$aadth1{$tag}.=":".($c3-$c2);
						$rtag=reverse($tag);
						$aadth1{$rtag}.=":".($c3-$c2);
					}
				}
				$s="";
			}
		}
	}	
	undef %aad;
	if($c1<=203 and $c1>=193){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				$s=$t;$l=length($s);
				@t2=split(//,$s);
				for($c2=0;$c2<=$#t2;$c2++){
					for($c3=($c2+1);$c3<=$#t2;$c3++){
						$tag=@t2[$c2].':'.@t2[$c3];
						$aadth2{$tag}.=":".($c3-$c2);
						$rtag=reverse($tag);
						$aadth2{$rtag}.=":".($c3-$c2);
					}
				}
				$s="";
			}
		}
	}	
	undef %aad;
	if($c1<=214 and $c1>=204){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				$s=$t;$l=length($s);
				@t2=split(//,$s);
				for($c2=0;$c2<=$#t2;$c2++){
					for($c3=($c2+1);$c3<=$#t2;$c3++){
						$tag=@t2[$c2].':'.@t2[$c3];
						$aadth3{$tag}.=":".($c3-$c2);
						$rtag=reverse($tag);
						$aadth3{$rtag}.=":".($c3-$c2);
					}
				}
				$s="";
			}
		}
	}	
	undef %aad;
	if($c1<=227 and $c1>=215){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				$s=$t;$l=length($s);
				@t2=split(//,$s);
				for($c2=0;$c2<=$#t2;$c2++){
					for($c3=($c2+1);$c3<=$#t2;$c3++){
						$tag=@t2[$c2].':'.@t2[$c3];
						$aadth4{$tag}.=":".($c3-$c2);
						$rtag=reverse($tag);
						$aadth4{$rtag}.=":".($c3-$c2);
					}
				}
				$s="";
			}
		}
	}	
	undef %aad;
	if($c1<=237 and $c1>=228){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				$s=$t;$l=length($s);
				@t2=split(//,$s);
				for($c2=0;$c2<=$#t2;$c2++){
					for($c3=($c2+1);$c3<=$#t2;$c3++){
						$tag=@t2[$c2].':'.@t2[$c3];
						$aadth5{$tag}.=":".($c3-$c2);
						$rtag=reverse($tag);
						$aadth5{$rtag}.=":".($c3-$c2);
					}
				}
				$s="";
			}
		}
	}	
	undef %aad;
	if($c1<=246 and $c1>=238){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				$s=$t;$l=length($s);
				@t2=split(//,$s);
				for($c2=0;$c2<=$#t2;$c2++){
					for($c3=($c2+1);$c3<=$#t2;$c3++){
						$tag=@t2[$c2].':'.@t2[$c3];
						$aadth6{$tag}.=":".($c3-$c2);
						$rtag=reverse($tag);
						$aadth6{$rtag}.=":".($c3-$c2);
					}
				}
				$s="";
			}
		}
	}	
	undef %aad;
	if($c1<=256 and $c1>=247){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				$s=$t;$l=length($s);
				@t2=split(//,$s);
				for($c2=0;$c2<=$#t2;$c2++){
					for($c3=($c2+1);$c3<=$#t2;$c3++){
						$tag=@t2[$c2].':'.@t2[$c3];
						$aadth7{$tag}.=":".($c3-$c2);
						$rtag=reverse($tag);
						$aadth7{$rtag}.=":".($c3-$c2);
					}
				}
				$s="";
			}
		}
	}	
	undef %aad;
	if($c1<=267 and $c1>=257){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				$s=$t;$l=length($s);
				@t2=split(//,$s);
				for($c2=0;$c2<=$#t2;$c2++){
					for($c3=($c2+1);$c3<=$#t2;$c3++){
						$tag=@t2[$c2].':'.@t2[$c3];
						$aadth8{$tag}.=":".($c3-$c2);
						$rtag=reverse($tag);
						$aadth8{$rtag}.=":".($c3-$c2);
					}
				}
				$s="";
			}
		}
	}	
	undef %aad;
	if($c1<=278 and $c1>=268){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				$s=$t;$l=length($s);
				@t2=split(//,$s);
				for($c2=0;$c2<=$#t2;$c2++){
					for($c3=($c2+1);$c3<=$#t2;$c3++){
						$tag=@t2[$c2].':'.@t2[$c3];
						$aadth9{$tag}.=":".($c3-$c2);
						$rtag=reverse($tag);
						$aadth9{$rtag}.=":".($c3-$c2);
					}
				}
				$s="";
			}
		}
	}	
	undef %aad;
	if($c1<=285 and $c1>=279){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				$s=$t;$l=length($s);
				@t2=split(//,$s);
				for($c2=0;$c2<=$#t2;$c2++){
					for($c3=($c2+1);$c3<=$#t2;$c3++){
						$tag=@t2[$c2].':'.@t2[$c3];
						$aadf1{$tag}.=":".($c3-$c2);
						$rtag=reverse($tag);
						$aadf1{$rtag}.=":".($c3-$c2);
					}
				}
				$s="";
			}
		}
	}	
	undef %aad;
	if($c1<=298 and $c1>=286){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				$s=$t;$l=length($s);
				@t2=split(//,$s);
				for($c2=0;$c2<=$#t2;$c2++){
					for($c3=($c2+1);$c3<=$#t2;$c3++){
						$tag=@t2[$c2].':'.@t2[$c3];
						$aadf2{$tag}.=":".($c3-$c2);
						$rtag=reverse($tag);
						$aadf2{$rtag}.=":".($c3-$c2);
					}
				}
				$s="";
			}
		}
	}	
	undef %aad;
	if($c1<=312 and $c1>=299){
		for($c=0;$c<=$N;$c=($c+25)){
		$t=substr($seq,$c,50);$l=length($t);
			if($l>=25){
				$s=$t;$l=length($s);
				@t2=split(//,$s);
				for($c2=0;$c2<=$#t2;$c2++){
					for($c3=($c2+1);$c3<=$#t2;$c3++){
						$tag=@t2[$c2].':'.@t2[$c3];
						$aadf3{$tag}.=":".($c3-$c2);
						$rtag=reverse($tag);
						$aadf3{$rtag}.=":".($c3-$c2);
					}
				}
				$s="";
			}
		}
	}	

}

$s="ACDEFGHIKLMNPQRSTVWY";
aadist($s);
sub aadist{
	$s=shift;$l=length($s);
	@t2=split(//,$s);
	for($c2=0;$c2<=$#t2;$c2++){
		for($c3=($c2);$c3<=$#t2;$c3++){
			$tag=@t2[$c2].':'.@t2[$c3];
			push(@aad,$tag);
		}
	}
	$s="";
#	return %aad;
}
for($c=0;$c<=$#aad;$c++){
	$q=@aad[$c];
	$t=$q;
	$t=~s/\:/ /g;
	#$aado1{$q}=~s/\:/  /g;
	@fre=split(/\:/,$aado1{$q});
	for($c11=0;$c11<=$#fre;$c11++){
		$freqc{@fre[$c11]}++;
	}
	#print "$t\t$aado1{$q}\t";
	for($c12=1;$c12<=$cl;$c12++){
	#	print "$c12-$freqc{$c12}  ";
		$norm1=$freqc{$c12}/$N1;
		print "$norm1  ";
	}
	#print "\n";
	undef %freqc;
}
print "1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0\n";  
