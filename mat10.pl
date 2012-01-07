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

#use PDL::Core;

#use PDL::Matrix;

#$m = PDL::Matrix->pdl([[1,2,3],[4,5,6]]);

#$m = mpdl [[1,2,3],[4,5,6]];

#print $m;

#@rt=([2,3,4],[3,4,5]);

#push(@rt,6);

#$rt[2][1]=7;

#$rt[2][0]=6;

#print $rt[2][0];

#print $rt[2][1];

#print @rt;

$flag=0;

$match=2;

$gap=(-1);

$mismatch=0;

@seq1= qw/a c t g/;

unshift(@seq1,0);

@seq2=qw/c t g/;

unshift(@seq2,0);

$len1=@seq1;

$len2=@seq2;

for($c=1;$c<=$len1;$c++)

{

$l=$c-1;

$comp[0][$c]=@seq1[$l];

$compf[0][$c]=@seq1[$l];

}

for($cc=1;$cc<=$len2;$cc++)

{

$l=$cc-1;

$comp[$cc][0]=@seq2[$l];

$compf[$cc][0]=@seq2[$l];

}

#print "@seq1\t@seq2";

#print $comp[0][2];

#print "$len1\t$len2\n";

$comp[0][0]=0;

$comp[1][1]=0;

 $compf[0][0]=0;

$compf[1][1]=0;

 for ($gg=2; $gg<=$len1;$gg++)

 {

 $co1 = $gap+$comp[1][($gg-1)];

 $comp[1][$gg]=$co1;

 $compf[1][$gg]=9;

#print $comp[1][$gg] ;

 }

 for ($q=2;$q<=$len2;$q++)

 {

 $co1 = $gap+$comp[($q-1)][1] ;

 $comp[$q][1]=$co1;

 $compf[$q][1]=4;

  }
for ($a=2,$x=1; $x<$len1;$a++,$x++){

 for ($b=2,$y=1; $y<$len2;$b++,$y++){

#print "@seq1[$x] \t @seq2[$y]\n";

 #print "$b\t$a\t$comp[$b][$a]\n";

#  if($a==1)

# {$co1 = $gap+$comp[($b-1)][($a)]}

#   if($b==1)

# {$co1 = $gap+$comp[($b)][($a-1)]}

  if(@seq1[$x] =~ /@seq2[$y]/)

        {$co1=$match+$comp[($b-1)][($a-1)];

         }

  else

 {$co1=$mismatch+$comp[($b-1)][($a-1)];}

  $co2=$gap+$comp[($b-1)][$a];

  $co3=$gap+$comp[($b)][($a-1)];

  $max=$co1;

  $comp[$b][$a]=$co1;$flag=1;$compf[$b][$a]=$flag;

  if($co2>$max){$comp[$b][$a]=$co2;$flag=4;$compf[$b][$a]=$flag;$max=$co2;if($co2 eq $co3)

  {$flag=13;$compf[$b][$a]=$flag;}}

  if($co3>$max){$comp[$b][$a]=$co3;$flag=9;$compf[$b][$a]=$flag;$max=$co3;if($co1 eq $co3)

  {$flag=10;$compf[$b][$a]=$flag;}}if($co1 eq $co2){$flag=5;$compf[$b][$a]=$flag;}

 if($co3 eq $co1){if($co3 eq $co2){$comp[$b][$a]=$co3;$flag=14;$compf[$b][$a]=$flag;}}

 # if($co1=$max){if($co1=$co2){$flag=5;$compf[$b][$a]=$flag;}$flag=1;$compf[$b][$a]=$flag;

 # if($co3=$max){if($co2=$co3){if($co1=$co2){$flag=14;$compf[$b][$a]=$flag;}}if($co3=$co1){

  #$flag=10;$compf[$b][$a]=$flag;}}}

 # print "$max\n";

   #$flag=0;

  }

}

$comb=$len1*$len2;
#print "$comp[4][5]\n";

 for ($t=$len1; $t>=0 ; $t--){

 {for ($s=$len2; $s>=0;$s--)
 
 #if($compf[$s][$t]==9)

 #{print "\*\t@seq1[$t-1]\n";last;}

 if($compf[$s][$t]==1)

 {print "@seq2[$s-1]\t@seq1[$t-1]\n";last;
}
 #if($compf[$s][$t]==9)

 #{print "\*\t@seq1[$t-1]\n";last;}
 }}