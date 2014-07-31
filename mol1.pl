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

$fs1= shift @ARGV;
$fs2= shift @ARGV;
open F2,$fs2;
while($l1 = <F2>){
	chomp $l1;
	if($l1 ne ""){push(@list,$l1);}
	}
close F2;
foreach  (@list) { print " $_ \n";
}undef @list;$l1="";
open F1,$fs1;$f2=$fs1.".aout";
open F2,">$f2";undef @temp;
while($l1 = <F1>){
	push (@temp,$l1);
	print F2 @temp;
	}
close F1;close F2;
 