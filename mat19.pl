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
open(FILEMAT,"BLOSUM62");
$i=0;
while($line=<FILEMAT>)
{
	if($line=~/^#/)
		{
			 $flag=0;
		}
	else{
		$line=~s/"  "/" "/g;
		$line=~s/\*/\-/g;
		$line=~s/"\n"/" "/g;
		@b=split(" ",$line);
		if($i eq 6 and @b[0] eq "A")
		{unshift(@b,0);}
		for($j=0;$j<=$#b;$j++)
		{$blos[$i][$j]=@b[$j];}
		#print "@b\n";
		}
$i++;
}
print "\nEnter the Protein Sequence 1\t";
$sequence1=uc(<>);
chomp $sequence1;
@seq1=split(//,$sequence1);
print "\nEnter the Protein Sequence 2\t";
$sequence2=uc(<>);
chomp $sequence2;
@seq2=split(//,$sequence2);
print "\nenter the gap penalty(default=(-2))\t";
$gap=<>;
$flag=0;
unshift(@seq1,0);
unshift(@seq2,0);
$len1=@seq1;$len2=@seq2;
for($c=1;$c<=$len1;$c++)
{$l=$c-1;$comp[0][$c]=@seq1[$l];
$compf[0][$c]=@seq1[$l];}
for($cc=1;$cc<=$len2;$cc++)
{$l=$cc-1;$comp[$cc][0]=@seq2[$l];
$compf[$cc][0]=@seq2[$l];}
$comp[0][0]=0;$comp[1][1]=0;$compf[0][0]=" ";$compf[1][1]=0;
for ($gg=2; $gg<=$len1;$gg++)
 { $co1 = $gap+$comp[1][($gg-1)];
 $comp[1][$gg]=$co1;
 $compf[1][$gg]=9; }
 for ($q=2;$q<=$len2;$q++)
 {$co1 = $gap+$comp[($q-1)][1] ;
 $comp[$q][1]=$co1;
 $compf[$q][1]=4;}
for ($a=2,$x=1; $x<=$len1;$a++,$x++){
 for ($b=2,$y=1; $y<=$len2;$b++,$y++){
	for ($c1=7;$c1<=31;$c1++) {
	  if(@seq1[$x] eq $blos[$c1][0])
	  {	for ($c2=1;$c2<=25;$c2++) {
			if(@seq2[$y] eq $blos[6][$c2])
		{$co1=$blos[$c1][$c2]+$comp[($b-1)][($a-1)];}}
	  }}
  $co2=$gap+$comp[($b-1)][$a];
  $co3=$gap+$comp[($b)][($a-1)];
  $max=$co1;
  $comp[$b][$a]=$co1;$flag=1;$compf[$b][$a]=$flag;
 if($co2>$max){$comp[$b][$a]=$co2;$flag=4;$compf[$b][$a]=$flag;$max=$co2;if($co2 eq $co3)
  {$flag=13;$compf[$b][$a]=$flag;}}
 if($co3>$max){$comp[$b][$a]=$co3;$flag=9;$compf[$b][$a]=$flag;$max=$co3;if($co1 eq $co3)
  {$flag=10;$compf[$b][$a]=$flag;}}if($co1 eq
$co2){$flag=5;$compf[$b][$a]=$flag;}
 if($co3 eq $co1){if($co3 eq
$co2){$comp[$b][$a]=$co3;$flag=14;$compf[$b][$a]=$flag;}}
  }
}
 for ($a=$len2,$y=($len2-1); $y>0;$y--,$a--){
 for ($b=$len1,$x=($len1-1);$x>0;$x--,$b--){
  if(@seq1[$x] =~ /@seq2[$y]/)
  {$co1=$match+$comp[($b-1)][($a-1)];$idxm[$a][$b]="|";}
  elsif(@seq1[$x] eq "a" and @seq2[$y] eq "g")
  {$co1=$pmatch+$comp[($b-1)][($a-1)]; $idxm[$a][$b]="+";}
   elsif(@seq1[$x] eq "g" and @seq2[$y] eq "a")
   {$co1=$pmatch+$comp[($b-1)][($a-1)];$idxm[$a][$b]="+"; }
   elsif(@seq1[$x] eq "c" and @seq2[$y] eq "t")
  {$co1=$pmatch+$comp[($b-1)][($a-1)]; $idxm[$a][$b]="+";}
  elsif(@seq1[$x] eq "t" and @seq2[$y] eq "c")
  {$co1=$pmatch+$comp[($b-1)][($a-1)];$idxm[$a][$b]="+"; }
  else
  {$co1=$mismatch+$comp[($b-1)][($a-1)];$idxm[$a][$b]=" "; }
  }}
 $row=$len2;
 $col=$len1;
 while (($row ne "0") and ($col ne "0"))
    { if ($compf[$row][$col] eq "1") { $rowop=$rowop.$compf[$row][0];
$colop=$colop.$compf[0][$col];
                             $matop=$matop.$idxm[$row][$col];
$row--,$col--; }
           elsif ( $compf[$row][$col] eq "9") {$rowop=$rowop."-";
$colop=$colop.$compf[0][$col];

$matop=$matop.$idxm[$row][$col]; $col--;  }
           elsif( $compf[$row][$col] eq "4")  {
$rowop=$rowop.$compf[$row][0]; $colop=$colop."-";

$matop=$matop.$idxm[$row][$col]; $row--; }
            else {$rowop=$rowop.$compf[$row][0];$colop=$colop."-";
$matop=$matop.$idxm[$row][$col];
                                             $row--;   }}
$rrow=reverse($rowop);$rcol=reverse($colop);$rmat=reverse($matop);$row=$len2;$col=$len1;
print "the sequence alignment matrix is \n";
for($s=0;$s<=$row;$s++)
{ for ($t=0; $t<=$col; $t++)
{  print "$comp[$s][$t]\t";
  #print "$compf[$s][$t] \t";
  }print "\n";
  }
print "\nThe optimal sequence alignment is ::=\>\n";
print "\t $rrow \n";
#print "\t $rmat \n" ;
print "\t $rcol \n";
print "the score is $comp[$row][$col]\n";