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
			$flag=0;
		}
	else{
		$line=~s/"  "/" "/g;
		$line=~s/\*/\-/g;
		$line=~s/"\n"/" "/g;
		@b=split(" ",$line);
		if($i eq 6 and @b[0] eq "A")
		{unshift(@b,0);}
		for($j=0;$j<=$#b;$j++)
		{$blos[$i][$j]=@b[$j];}
		#print "@b\n";		
		}
$i++;
}
for($cc=6;$cc<31;$cc++)
{for($ccc=0;$ccc<25;$ccc++)
	{print "$blos[$cc][$ccc] ";
	}print "\n";
	}
	print "$blos[6][1]";