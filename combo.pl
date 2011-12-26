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
print

if($choice eq 'R')
{
print "enter collected output file name\n";
$filee=<>;
system "more *sure>$filee";
system "more *maybe>>$filee";
open F1,$filee;
while($line=<F1>)
{
system "cat $line";
}
}
if($choice eq 'M')
{

open F1,"genescanmulout.txt";
while($line=<F1>)
{
$line =~ s/ //g;
$line =~ s/\*//g;
print "$line";
}

