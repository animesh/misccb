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
$file1=shift @ARGV;
$file2=shift @ARGV;
	open(F1,$file1);
	open(F2,$file2);

	while($l=<F1>){
	chomp $l;
	#@t=split(/\s+/,$l);
	@t=split(/\_/,$l);
	@t=split(/\s+/,@t[0]);
	push(@class,@t[4]);
	}		
	while($l=<F2>){
	chomp $l;$c++;
	$seq{$c}=$l;
	}		




for($c3=0;$c3<=$#class;$c3++){
	#print "$seq{@class[$c3]}\n"
	print "$seq{@class[$c3]}\n"
}
