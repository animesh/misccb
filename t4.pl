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

$f1=shift @ARGV;$f2=shift @ARGV;
open(F1,$f1);open(F2,$f2);
while($l1=<F1>){
@t1=split(/\t/,$l1);
chomp @t1[1];
@t1[1]=~s/\s+|\[|\]//g;
#print "@t1[1]\n";
push(@c1,"@t1[1]");
}
while($l2=<F2>){
@t2=split(/\:|\,/,$l2);
if(@t2[1]=~/^c/){
	@t2[1]=~s/c//g;#reverse(@t2[1]);
	@t3=split(/\-/,@t2[1]);
	#print "@t3[1]-@t3[0]\n";
	push (@c2,"@t3[1]-@t3[0]");
	}
else{
	#print "@t2[1]\n";
	push(@c2,"@t2[1]");
	}
	
}
$le=@c1;
foreach $q1 (@c2){
	foreach $q2 (@c1){
	if($q1 == $q2){
#		print "$q1\t$q2\n";
		}
	else{
		$n++;
		}
	}
	if($n == $le){
	print "$q1\n";
	}
$n=0;
}
close F1; close F2;
