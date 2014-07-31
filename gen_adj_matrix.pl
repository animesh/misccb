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
if( @ARGV ne 1){die "\nUSAGE\t\"ProgName Connection_File\t\n\n\n";}
$file = shift @ARGV;
open (F, $file) || die "can't open \"$file\": $!";
while ($line = <F>) {$r++;
	if($r eq 1){next;}
	chomp $line;
	@t=split(/\t/,$line);
		$s1=@t[0];
		$s2=@t[1];
		push(@all,$s1);push(@all,$s2);
		$s3=$s1."_".$s2;$s5=$s2."_".$s1;$s6=$s2."_".$s2;$s7=$s1."_".$s1;
		$m{$s3}++;$m{$s5}++;$m{$s6}++;$m{$s7}++;
}
close F;
%seen=();
@allu = (grep{ !$seen{$_} ++} @all);undef @all;
@allu = (sort {$a <=> $b} @allu);
for($c2=0;$c2<=$#allu;$c2++){
	$tag1=$allu[$c2];
	$pro{$tag1}=$c2;
	for($c3=0;$c3<=$#allu;$c3++){
		$tag2=@allu[$c3];
		$tag3=$tag1."_".$tag2;
		if($m{$tag3}>0){$mat[$c2][$c3]=1;}
		elsif($m{$tag3} eq ""){$mat[$c2][$c3]=0;}
		else{$mat[$c2][$c3]=0;}
		print "$mat[$c2][$c3]\t";
	}
	print "\n";
}
