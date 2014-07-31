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
$fposmut=shift @ARGV;
open(FO,"$fposmut");
$c=0;
while($l=<FO>){
	chomp $l;
	push(@fo,$l);
	@t=split(/\t/,$l);
	$posm{$c}=@t[1];
	#print "$c\t$posm{$c}\n";
	$c++;
}
foreach $uu (sort {$posm{$b} <=> $posm{$a}} keys %posm) {
	print "@fo[$uu]\n";
}
close FO;
