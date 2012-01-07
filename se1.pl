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
	for($c1=4;$c1<=16;$c1++) {$s.=$t2o{@t[$c1]};}
	#foreach (@t){$c++;print "$c\t$_\n";}$c=0;
	}
	$seq.=$s;
if($l=~/^HELIX/){
	@t=split(/\s+/,$l);
	#for($c1=4;$c1<=16;$c1++) {$s.=$t2o{@t[$c1]};}
	foreach (@t){$c++;print "$c\t$_\n";}$c=0;
	}
}
#$l=length($seq);print "$l\n$seq\n"
