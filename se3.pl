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
open(F,"1rab.pdb");
while($l=<F>){
$s="";
if($l=~/^SEQRES/){
	@t=split(/\s+/,$l);
	for($c1=4;$c1<=16;$c1++) {$cs{@t[2]}.=$t2o{@t[$c1]};}
	}
	$seq.=$s;
if($l=~/^HELIX/){
	@t=split(/\s+/,$l);
	$s=substr($cs{@t[4]},(@t[5]-1),(@t[8]-@t[5]+1));
	#print "H=>$s\n";
	}
@t2=split(//,$s);
for($c2=0;$c2<=$#t2;$c2++){
	for($c3=($c2+1);$c3<=$#t2;$c3++){
		$tag=@t2[$c2].'-'.@t2[$c3];
		$name=@t[2];
		$name{$tag}.="-".($c3-$c2);
	}
}
#foreach (keys %name){$c++;print "$c\t$_\t$name{$_}\n";}$c=0;
if($l=~/^SHEET/){
	@t=split(/\s+/,$l);
	$s=substr($cs{@t[5]},(@t[6]-1),(@t[9]-@t[6]+1));
	#print "S=>$s\n";
	}
@t2=split(//,$s);
for($c2=0;$c2<=$#t2;$c2++){
	for($c3=($c2+1);$c3<=$#t2;$c3++){
		$tag=@t2[$c2].'-'.@t2[$c3];
		$name=@t[2];
		$name{$tag}.="-".($c3-$c2);
	}
}
if($l=~/^TURN/){
	@t=split(/\s+/,$l);
	$s=substr($cs{@t[4]},(@t[5]-1),(@t[8]-@t[5]+1));
	#print "T=>$s\n";
	}
@t2=split(//,$s);
for($c2=0;$c2<=$#t2;$c2++){
	for($c3=($c2+1);$c3<=$#t2;$c3++){
		$tag=@t2[$c2].'-'.@t2[$c3];
		$name=@t[2];
		$name{$tag}.="-".($c3-$c2);
	}
}
}
close F;
$l=length($seq);print "$l\n$seq\n";
foreach (keys %name){$c++;print "$c\t$_\t$name{$_}\n";}$c=0;
