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
$file1=$f."_all.dat";
$file2=$f."_aml.dat";
open(F,$f);
#open(F1,">$file1");
#open(F2,">$file2");
use Math::Complex;
$pi=pi;
$i=sqrt(-1);
$c1=0;
while($l1=<F>){
	chomp $l1;
	@t1=split(/\t/,$l1);
	for($c2=0;$c2<=$#t1;$c2++){
		if($c1==0 or $c2==0){$mat[$c2][$c1]="@t1[$c2]";}
		else{$mat[$c2][$c1]=(@t1[$c2])+0;}#print "@t1[$c2]  ";
		}
	
	$c1++;
}
for($c6=1;$c6<($c2);$c6++){
	for($c5=1;$c5<$c1;$c5++){$subsum=0;
		$u=$c5-1;
		#print "$mat[0][$c6]\t$mat[$c5][0]\t$val\t";
		for($c7=1;$c7<$c1;$c7++){
			$val=$mat[$c6][$c7]+0;		
			$N=$c1-1;
			$x=$c7-1;
			$subsum+=($val*exp(-(2*$pi*$i*($u*$x)/$N)));
			#print "$val\t2PI I $x $u $N\t";
		}
		#print "\n";
		#$subsumtotal+=(((1/$le)**2)*(abs($subsum)**2));
		$subsuma=(1/$N)*(abs($subsum));
		#print "$mat[0][$c6]\t$mat[$c5][0]\t$val\t$subsum\t$subsuma\n";
		print "$subsuma\t";
	}
	print "\n";
}