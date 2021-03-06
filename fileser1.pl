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
if( @ARGV ne 2){die "\nUSAGE\t\"ProgName Actual FFN Predicted FFN\n\n\n";}
$file1 = shift @ARGV;$file2 = shift @ARGV;
$f3=$file1."_".$file2.".out";
open (F1, $file1) || die "can't open \"$file1\": $!";
open (F2, $file2) || die "can't open \"$file2\": $!";
open (F3, ">$f3") || die "can't open \"$f3\": $!";
while ($line = <F1>) {
		chomp $line;
		push(@seqo,$line);	
}
push(@seqo,$seq);
close F1;
while ($line = <F2>) {
			chomp $line;
			push(@seqn,$line);
}
push(@seqn,$seq);
close F2;

for($c1=0;$c1<=$#seqo;$c1++){
	$seqoo=@seqo[$c1];
	for($c2=0;$c2<=$#seqn;$c2++){
		$seqnn=$seqn[$c2];
		if($seqnn eq $seqoo){
			$cc1=$c1+1;$cc2=$c2+1;
			print F3"$cc1\t$cc2\n";			
			print "$cc1\t$cc2\n";
			last;
		}
	}
}
#perl fileser1.pl dub_4200_train.txt  mixdub_4200_train.txt