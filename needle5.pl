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
	if($l=~/^\# 1: /){
		@t=split(/\s+/,$l);
		$n1=@t[2];
	}
	if($l=~/^\# 2: /){
		@t=split(/\s+/,$l);
		$n2=@t[2];
	}
	if($l!~/^\#/ && $l ne ""){
		@t=split(/\s+/,$l);
		#print "$c\t@t[2]\n";
		@t[2]=~s/\s+//g;
		if(@t[0] ne ""){
			$seq{@t[0]}.=@t[2];
		}

		if($l=~/^\s+/){
			$symbol=@t[1];
			#print "$c\t$symbol\n";
			if($symbol ne ""){
				$seq{"sym"}.=$symbol;
			}
		}
#		if($symbol ne ""){
#			@res=split(//,@t[2]);
#		}
#		for($a=0;$a<=$#s;$a++){
#			if(@s[$a] ne "|"){
#				print "$c\t@s[$a]\n";
#			}
#		}
	}
}
foreach  (keys %seq) {
	$keyn=$_;
	if($keyn ne "sym" and $n1 eq $keyn){
		@seq1=split(//,$seq{$_});
	}
	elsif($keyn ne "sym" and $n2 eq $keyn){
		@seq2=split(//,$seq{$_});
	}
	else{
		$len=length($seq{$_});
		@sym=split(//,$seq{$_});
	}
	#print "$n1\t$n2\t$_\t$keyn\n"
}
#print "POS\t$n1-SYM-$n2\n";
for($a=0;$a<$len;$a++){
	$pos=$a+1;
	if(@sym[$a] ne "|"){
		print "@seq1[$a]-@sym[$a]-@seq2[$a]\n";
	}
}
