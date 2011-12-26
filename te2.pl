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
$f1=shift @ARGV;chomp $f1;
open(F2,$f1);
$max=0;
while($l2=<F2>){
	$c1++;
	chomp $l2;
	@t=split(/\t/,$l2);
	if(@t[0]==0){
		print "@t[0]\t$c1\t$l2\n";
	}
	if($max<@t[0]){$max=@t[0];$maxline{$max}=$c1;}
}	
print "$max\t$maxline{$max}\n";