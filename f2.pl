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
open (MYHANDLE,"hexmer.txt")
or die "can't open file \n";
while ($lin = <MYHANDLE>){
push (@x,$lin);}
# print @x;

	print "enter the filename \n";
	$filename =<> ;
	open (MYHANDLE,$filename)
	or die "can't open $filename :$!\n";
	while ($line = <MYHANDLE>){
	chomp ($line);
	    if ($line=~ /^>/) {
	    $line =~ s/>//;
	    $seqname = $line ;
	    } else {
	    $seq = $seq.$line ;
	    push (@seq,$seq);
	       	     }
		   }
	   # print "@seq \n";

for ($ii ; $ii <=$#x ; $ii++){

  ($hs = $seq=~s/$nii/$nii/g) ;
# print " hex $nii occures $hs times \n";

		     	      }
print " hex $nii occures $hs times \n";
# print "@hexnu \n";




