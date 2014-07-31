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
$t1=times();
print "Hello, World...\n";
@country=qw/singapore thailand china hongkong malaysia taiwan phillipines/;$length=@country;
#foreach  (@country) { print "$_ \n";}print "@country[int(rand($length))-1]\n";
$contrand=@country[int(rand($length))-1];
foreach  (1..100000) {$temp=($_)*($_)*($_);
print "$temp\n"
}
$t2=times();
print "\n$t1\t$t2\n";
$ttq=($t2-$t1)/60;$ttr=($t2-$t1)%60;$ttq=int($ttq);
print "Total time taken by the script : $ttq Min $ttr Sec\n\n";
