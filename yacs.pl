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

open(FILE,"c:\windows\yacs.log");
@array=<FILE>;
while($line=<FILE>
{
chomp$line;
if($line ne "")
{
$seq=$seq.$line;
}
else
{
$seq=$seq.$line;
}
}
$length=length(@array);
print $length;
print $array[23];
print $seq;

