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
if( @ARGV ne 1){die "\nUSAGE\t\"ProgName MultSeqFile\t\n\n\n";}
$file = shift @ARGV;
open (F, $file) || die "can't open \"$file\": $!";
while ($line = <F>) {$r++;
	if($r eq 1){next;}
	chomp $line;
	@t=split(/\t/,$line);
	#$s1=@t[1];$s1=~s/\[//g;$s2=@t[3];$s2=~s/\]//g;$s1+=0;$s2+=0;
	#if((@t[2] eq "physical") and (@t[5] eq "two hybrid")){
		$s1=@t[1];
		$s2=@t[4];
		push(@all,$s1);push(@all,$s2);
		$s3=$s1."_".$s2;$s5=$s2."_".$s1;
		$m{$s3}++;$m{$s5}++;
	#}
		#print "$s1\t$s2\t$s3\n";
}
#foreach $w (sort {$m{$b} <=> $m{$a}} keys %m){$c+=$m{$w};$cc=$m{$w};print "$w\t$cc\t$c\n";}
%seen=();
@allu = grep{ !$seen{$_} ++} @all;undef @all;
#@allu=uniq(@all);
for($c2=0;$c2<=$#allu;$c2++){
	$tag1=$allu[$c2];
	$pro{$tag1}=$c2;
	for($c3=($c2+1);$c3<=$#allu;$c3++){
		$tag1=$allu[$c2];$tag2=@allu[$c3];$tag3=$tag1."_".$tag2;
		if($m{$tag3}>0){$mat[$c1][$c2]=1;}
		else{$mat[$c1][$c2]=0;}
		#print "$tag1-$tag2:$mat[$c1][$c2]\t";
		print "$mat[$c1][$c2]\t";
	}
	print "\n";
}

#foreach  (@allu) {	print "$_\n";}}

#foreach $w (sort {$pro{$a} <=> $pro{$b}} keys %pro){$c+=$pro{$w};$cc=$pro{$w};print "$w\t$cc\t$c\n";}

for($c2=0;$c2<=$#allu;$c2++){
	$tag1=$allu[$c2];
	$pro{$tag1}=$c2;
	for($c3=($c2+1);$c3<=$#allu;$c3++){
		$tag2=@allu[$c3];
		$tag3=$tag1."_".$tag2;
		if($m{$tag3}>0){$mat[$c1][$c2]=1;}
		else{$mat[$c1][$c2]=0;}
		#print "$tag1-$tag2:$mat[$c1][$c2]\t";
		#print "$mat[$c1][$c2]\t";
	}
	#print "\n";
}