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
open(FILEMAT,"./matrix/BLOSUM62");
while($line=<FILEMAT>)
{
	if($line=~/^#/){
		print "\n";}
	else{
		$line=~s/"  "/" "/g;
		$line=~s/"\n"/" "/;
		$line2=$line2.$line;;
		#print @blos;		
		}
		@blos=split(" ",$line2);
}
foreach $ccc (@blos) {
print "$ccc";
}