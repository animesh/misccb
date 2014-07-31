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
$file = shift @ARGV;
open(F,$file)||die "no such file $file";
$atnom=0;
while($l=<F>){
	if($l=~/^ATOM/){
		@t=split(//,$l);
		$name=@t[17].@t[18].@t[19].@t[12].@t[13].@t[14].@t[15].@t[16];
			$x=@t[30].@t[31].@t[32].@t[33].@t[34].@t[35].@t[36].@t[37]+0;
			$y=@t[38].@t[39].@t[40].@t[41].@t[42].@t[43].@t[44].@t[45]+0;
			$z=@t[46].@t[47].@t[48].@t[49].@t[50].@t[51].@t[52].@t[53]+0;
			$xcor{$atnom}=$x;
			$ycor{$atnom}=$y;
			$zcor{$atnom}=$z;
			$s2=$x."-".$y."-".$z;
			$aac{$atnom}=$name;
		$atnom++;

	}
}
close F;
for ($c1=0;$c1<$atnom;$c1++) {
	for ($c2=0;$c2<$atnom;$c2++) {
		#if($c1!=$c2){
			$distmat[$c1][$c2]=ED($xcor{$c1},$ycor{$c1},$zcor{$c1},$xcor{$c2},$ycor{$c2},$zcor{$c2});
			#print "$distmat[$c1][$c2]\t";
		#}
		if($distmat[$c1][$c2]<=5){
			$adjmat[$c1][$c2]=1;
			print "C-$distmat[$c1][$c2]\t";
		}
		else{
			$adjmat[$c1][$c2]=0;
			print "$distmat[$c1][$c2]\t";
		}
	}
	print "\n";
}

sub ED{
	my $xval1=shift;
	my $yval1=shift;
	my $zval1=shift;
	my $xval2=shift;
	my $yval2=shift;
	my $zval2=shift;
	$dis=sqrt(($xval1-$xval2)**2+($yval1-$yval2)**2+($zval1-$zval2)**2);
	return $dis;
}

# Version 5 of Coordinate extract written on 10/15/2005 under the guidance of Rajeev Mishra
