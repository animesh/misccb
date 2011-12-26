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

print "Enter first sequence  : "; $seq1 = <STDIN>; chop($seq1);
print "Enter second sequence : "; $seq2 = <STDIN>; chop($seq2);
$c = length($seq1); $r = length($seq2);

for ($i=0;$i<=$r;$i++) {
   for ($j=0;$j<=$c;$j++) {
   $p[$i][$j] = 0;  # score of best alignment of two substrings 
   $a[$i][$j] = 0;  # cost of match or mismatch
   }
}

for ($i=0;$i<=$r;$i++) {
   $p[$i][0] = $i*(-2); 
   $b[$i][0] = "up"; # up means trace back upwards
}

for ($j=0;$j<=$c;$j++) {
   $p[0][$j] = $j*(-2); 
   $b[0][$j] = "le";  # means trace back leftwards
}

for ($i=1;$i<=$r;$i++) {
   for ($j=1;$j<=$c;$j++) {
         if ( substr($seq1,$j-1,1) eq substr($seq2,$i-1,1) ) {
            $a[$i][$j] = 1;
         } else { 
            $a[$i][$j] = -1
         } 
         $vl=$p[$i][$j-1]-2; # right/left 
         $vd=$p[$i-1][$j-1]+$a[$i][$j];  # diagonal 
         $vt=$p[$i-1][$j]-2; # up/down
         if ($vt >= $vd && $vt >= $vl) {
            $p[$i][$j] = $vt; 
            $b[$i][$j] = "up"; 
         } elsif ($vd >= $vt && $vd >= $vl) {
            $p[$i][$j] = $vd; 
            $b[$i][$j] = "dg"; 
         } else {
            $p[$i][$j] = $vl; 
            $b[$i][$j] = "le"; 
         }
   } 
}

print "Show Matrix? "; $ans = <STDIN>; chop($ans);
print_matrix() if $ans eq "y";   

$i=$r; $j=$c;
$ans1 = ""; $ans2 = ""; 
while ($i >= 1 || $j >= 1) { 
   if ($b[$i][$j] eq "up") { 
      $ans1 = $ans1 . "-"; 
      $ans2 = $ans2 . substr($seq2,$i-1,1); 
      $i--;
   } elsif ($b[$i][$j] eq "le") { 
      $ans1 = $ans1 . substr($seq1,$j-1,1); 
      $ans2 = $ans2 . "-"; 
      $j--;
   } else { 
      $ans1 = $ans1 . substr($seq1,$j-1,1); 
      $ans2 = $ans2 . substr($seq2,$i-1,1); 
      $i--;
      $j--;
   }  
}

$ans1 = reverse($ans1); 
$ans2 = reverse($ans2); 
print $ans1, "\n", $ans2, "\n";
print "Similarity is ", $p[$r][$c], "\n";

exit;

sub print_matrix {
  # print first two rows 
   print "\t"; 
   for ($j=0;$j<=$c;$j++) { 
      print substr(" " . $seq1,$j,1), "\t"; 
   }
   print "\n";

   for ($i=0;$i<=$r;$i++) {
      print substr(" " . $seq2,$i,1), "\t"; 
      for ($j=0;$j<=$c;$j++) {
         print $p[$i][$j], " ", $b[$i][$j], "\t"; 
      } 
   print "\n";
   }
}
