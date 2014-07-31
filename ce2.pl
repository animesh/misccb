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
%t2o = (
      'ALA' => 'A',
      'VAL' => 'V',
      'LEU' => 'L',
      'ILE' => 'I',
      'PRO' => 'P',
      'TRP' => 'W',
      'PHE' => 'F',
      'MET' => 'M',
      'GLY' => 'G',
      'SER' => 'S',
      'THR' => 'T',
      'TYR' => 'Y',
      'CYS' => 'C',
      'ASN' => 'N',
      'GLN' => 'Q',
      'LYS' => 'K',
      'ARG' => 'R',
      'HIS' => 'H',
      'ASP' => 'D',
      'GLU' => 'E',
    );
$file = shift @ARGV;
open(F,$file)||die "no such file";
while($l=<F>){
$s="";
	if($l=~/^SEQRES/){
		@t=split(/\s+/,$l);
		for($c1=4;$c1<=16;$c1++) {
			$cs{@t[2]}.=$t2o{@t[$c1]};
			$s.=$t2o{@t[$c1]};
		}
	}
$seq.=$s;$s="";
	if($l=~/^HELIX/){
		@t=split(/\s+/,$l);
		$s=substr($cs{@t[4]},(@t[5]-1),(@t[8]-@t[5]+1));
		@t2=split(//,$s);
			for($c2=0;$c2<=$#t2;$c2++){
			$s1=@t2[$c2]."-".@t[4]."-".($t[5]+$c2);$s2="H";
			$aap{$s1}=$s2;
			#print "$s1=>$aap{$s1}\n";
			}
		}
@t2=split(//,$s);
for($c2=0;$c2<=$#t2;$c2++){
	for($c3=($c2+1);$c3<=$#t2;$c3++){
		$tag=@t2[$c2].'-'.@t2[$c3];
		$helix{$tag}.="-".($c3-$c2);
		$rtag=reverse($tag);
		$helix{$rtag}.="-".($c3-$c2);
	}
}
$s="";

if($l=~/^SHEET/){
	@t=split(/\s+/,$l);
	$s=substr($cs{@t[5]},(@t[6]-1),(@t[9]-@t[6]+1));
	#print "S=>$s\n";
	}
@t2=split(//,$s);
for($c2=0;$c2<=$#t2;$c2++){
	for($c3=($c2+1);$c3<=$#t2;$c3++){
		$tag=@t2[$c2].'-'.@t2[$c3];
		$sheet{$tag}.="-".($c3-$c2);
		$rtag=reverse($tag);
		$sheet{$rtag}.="-".($c3-$c2);

	}
}
$s="";

if($l=~/^TURN/){
	@t=split(/\s+/,$l);
	$s=substr($cs{@t[4]},(@t[5]-1),(@t[8]-@t[5]+1));
	#print "T=>$s\n";
	}
@t2=split(//,$s);
for($c2=0;$c2<=$#t2;$c2++){
	for($c3=($c2+1);$c3<=$#t2;$c3++){
		$tag=@t2[$c2].'-'.@t2[$c3];
		$turn{$tag}.="-".($c3-$c2);
		$rtag=reverse($tag);
		$turn{$rtag}.="-".($c3-$c2);
		}
	}
if($l=~/^ATOM/){
	@t=split(/\s+/,$l);
	if(@t[0]=~/ATOM/ and @t[2]=~/CA/){
		$s1=$t2o{@t[3]}."-".@t[4]."-".@t[5];
		$s2=@t[6]."-".@t[7]."-".@t[8];
		$aac{$s1}=$s2;
		print "$s1=>$aac{$s1}=>$aap{$s1}\n";
		}
	}
}
close F;
##$l=length($seq);print "$l\n$seq\n";$c=0;
#$c=0;print "HELIX\n";
#foreach (keys %helix){$c++;
#	print "$c\t$_ => $helix{$_}\n";
#}
#
#$c=0;print "SHEET\n";
#foreach (keys %sheet){$c++;
#	print "$c\t$_\t$sheet{$_}\n";
#}
#
#$c=0;print "TURN\n";
#foreach (keys %turn){$c++;
#	print "$c\t$_\t$turn{$_}\n";
#	}
#
#$c1=0;$c2=0;
#foreach $w1 (keys %helix){$c1++;$c2=0;
#	foreach $w2 (keys %sheet){$c2++;
#		if($w1 eq $w2){
#		$val=SNR($helix{$w1},$sheet{$w2});
#		$key="$w1\t$helix{$w1}\t$sheet{$w2}";
#		$sum{$key}=$val;
##		print "$w1\t$\"c1:$c2\"\t\"$helix{$w1}:$sheet{$w2}\"\t$val\n";
#		}
#	}
#}
$c1=0;$c2=0;
foreach $w1 (keys %aac){$c1++;$c2=0;
	foreach $w2 (keys %aac){$c2++;
		if($w1 eq $w2){
		$val=ED($aac{$w1},$aac{$w2});
		$key="$w1\t$aap{$w1}\t$aap{$w2}";
		$sum{$key}=$val;
#		print "$w1\t$\"c1:$c2\"\t\"$helix{$w1}:$sheet{$w2}\"\t$val\n";
		}
	}
}

foreach $q (sort {$sum{$b} <=> $sum{$a}} keys %sum){
	$t=$q;
	$t=~s/-/ /g;
	#print "$t\t$sum{$q}\n";
	}
sub ED{
	$val1=shift;
	$val2=shift;
	
	@t1=split(/\-/,$val1);
	@t2=split(/\-/,$val1);

	$dis=sqrt((@t1[0]-@t2[0])**2+(@t1[1]-@t2[1])**2+(@t1[2]-@t2[2])**2);
	return $dis;
	}
}

sub SNR{
	$val1=shift;
	$val2=shift;
	
	@t1=split(/\-/,$val1);
	$length=@t1;$N=$length-1;#print "$N\n";
	for($c2=1;$c2<=$#t1;$c2++){
		$temp3+=@t1[$c2];
	}
	$temp3=$temp3/$N;
	for($c2=1;$c2<=$#t1;$c2++){
		$temp5+=($temp3-@t1[$c2])**2;
	}
	$temp5=sqrt($temp5/$N);

	@t1=split(/\-/,$val2);
	$length=@t1;$N=$length-1;#print "$N\n";
	for($c2=1;$c2<=$#t1;$c2++){
		$temp2+=@t1[$c2];
	}
	$temp2=$temp2/$N;
	for($c2=1;$c2<=$#t1;$c2++){
		$temp4+=($temp2-@t1[$c2])**2;
	}
	$temp4=sqrt($temp4/$N);

	if($temp4 eq $temp5){$temp1=1;}
	else{
		$temp1=abs(($temp2-$temp3)/($temp4+$temp5));
		}
	$temp2=0;$temp3=0;$temp4=0;$temp5=0;$temp6=0;$temp7=0;
	return $temp1;
}