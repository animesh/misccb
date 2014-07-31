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
$i=0;
while($line=<FILEMAT>)
{
	if($line=~/^#/)
		{
		#print "\n";
		}
	else{
		$line=~s/"  "/" "/g;
		$line=~s/\*/x/g;
		$line=~s/"\n"/" "/g;
		@b=split(" ",$line);
		#print "@b[0]\n";
		for($j=0;$j<$#b;$j++)
		{$blos[$i][$j]=@b[$j];}
		#print "@b\n";		
		}$i++;
		
}
$blos[0][0]=0;
		#for($j=2;$j<26;$j++)
		#{$blos[0][$j]=$blos[$j][0];}

for($cc=0;$cc<26;$cc++)
{for($ccc=0;$ccc<26;$ccc++)
	{print "$blos[$cc][$ccc] ";
	}print "\n";
	}
