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
@w=qw/1 2 3 4 5 6 7 8 9 10 14 15 19 22/;
$z=2;
$len=@w;
for($i=0;$i<$len;$i++)
{
$k=@w[$i]+$z;
$cont=0;
for($j=$i;$j<$len;$j++)
{
if(@w[$j]<=$k)
{
$cont++;
$i=$k;
}
#print "$k\t$cont\n"; 
}
print "$i\t$cont\n";  
}
#print "@w\n$len";

