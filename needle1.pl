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
$f=shift @ARGV;
open(F,$f);
while($l=<F>){
	chomp $l;
	$c++;
	if($l!~/^\#/ && $l ne ""){
		@t=split(/\s+/,$l);
		#print "$c\t@t[2]\n";
		@t[2]=~s/\s+//g;
		if($l=~/^\s+/){
			$symbol=@t[1];
			#print "$c\t$symbol\n";
			if($symbol ne ""){
				@s=split(//,$symbol);
			}
		}
		if($symbol ne ""){
			@res=split(//,@t[2]);
		}
		for($a=0;$a<=$#s;$a++){
			if(@s[$a] ne "|"){
				print "$c\t@s[$a]\n";
			}
		}
	}
}

