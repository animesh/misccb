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
$f1=shift @ARGV;
$f2=shift@ARGV;
open(F1,$f1);
open(F2,$f2);

while($l1=<F1>){chomp $l1;
@t1=split(/\s+/,$l1);
		for($c1=0;$c1<=$#t1;$c1++){
			$h1{@t1[$c1]}=($c1+1);
			if(@t1[$c1]>$m1){
				$m1=@t1[$c1];
				}
		}
		push(@cmp1,$h1{$m1});$m1=0;undef %h1;
}

while($l2=<F2>){chomp $l2;
@t2=split(/\s+/,$l2);
		for($c2=0;$c2<=$#t2;$c2++){
			$h2{@t2[$c2]}=($c2+1);
			if(@t2[$c2]>$m2){
				$m2=@t2[$c2];
				}
		}
		push(@cmp2,$h2{$m2});$m2=0;undef %h2;
}
		
print "Sample#\tActual\tObtained\n";
for($c2=0;$c2<=$#cmp2;$c2++){$c5=$c2+1;
	$mct{@cmp1[$c2]}++;
	if(@cmp1[$c2]!=@cmp2[$c2]){
		$mc{@cmp1[$c2]}++;
		print "$c5\t@cmp1[$c2]\t@cmp2[$c2]\n";
		$c3++;
	}
	else{
		#$mc{@cmp1[$c2]}++;
		print "$c5\t@cmp1[$c2]\t@cmp2[$c2]\n";
	}
}

print "\nCLASS\tMISCLASSIFICATION: TOTAL MEMBERS\tPERCENT\n";

foreach  (sort {$a <=> $b} keys %mc) {
	#print "$mc{@cmp1[$c2]}-$mct{@cmp1[$c2]}\t$per\n";
	#$per=$mc{@cmp1[$c2]}/$mct{@cmp1[$c2]}*100;
	$per=$mc{$_}/$mct{$_}*100;
	print "$_\t$mc{$_}\t\t : $mct{$_}\t\t\t$per\n";
}
$tper=($c3/$c5)*100;
print "\nTotal Misclassification\t-> $c3 : $c5\t\t$tper\n";
