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
$f=shift @ARGV;
open(F,$f);


$file1=$f."_ft.dat";
open(F1,">$file1");

use Math::Complex;
$pi=pi;
$i=sqrt(-1);
$c1=0;$rowno=0;
while($l1=<F>){
	chomp $l1;
	@t1=split(/\s+/,$l1);
	for($c2=0;$c2<=$#t1;$c2++){
		if($c1==0 or $c2==0){$mat[$c2][$c1]=@t1[$c2]+0;}
		else{$mat[$c1][$c2]=(@t1[$c2])+0;}
		}
	
	$c1++;
}
close F;

for($c6=0;$c6<$c1;$c6++){
	for($c5=0;$c5<$c2;$c5++){
		$subsum=0;
		$u=$c5;
		for($c7=0;$c7<$c2;$c7++){
			$val=($mat[$c6][$c7]+0)*((-1)**($c7+$c6));		
			$N=$c2;
			$x=$c7;
			$subsum+=($val*exp(-(2*$pi*$i*($u*$x)/$N)));
		}
		$subsuma=(1/$N)*(($subsum));
		$matt[$c6][$c5]=$subsuma;
	}
}
for($c9=0;$c9<$c2;$c9++){
	for($c10=0;$c10<$c1;$c10++){
		$subsum=0;
		$u=$c10;
		for($c7=0;$c7<$c1;$c7++){
			$val=($matt[$c7][$c9]);
			$N=$c1;
			$x=$c7;
			$subsum+=($val*exp(-(2*$pi*$i*($u*$x)/$N)));
		}
		$subsuma=(1/$N)*(($subsum));
		$mattt[$c10][$c9]=$subsuma;
		$subsumm = int($subsuma);
		print F1"$mattt[$c10][$c9]\t";

	}
	print F1"\n";
}
