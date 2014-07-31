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
open (FILENAME,"seq1.txt") ||die "can't open $filename: $!";
 while ($line = <FILENAME>) {
 chomp ($line);
        if ($line =~ /^>/){
             $line =~ s/>//;
            push(@seqname,$line);
             $cc++;
                   if ($seq ne ""){
                        push(@seq,$seq);
                        $seq = "";
                                         }
        } else {
                  $seq=$seq.$line;
                  }
}
push(@seq,$seq);
$tots2=@seq;
@nt=qw/a t g c/;
foreach $x1 (@nt) { foreach $x2 (@nt) {  foreach $x3 (@nt) {
$aaco = $x1.$x2.$x3 ;
push (@aacomb,$aaco);
 } } }
for($x11=0;$x11<$tots2;$x11++){
$testseq=lc(@seq[$x11]);
@elem=split(//,$testseq);
$t1=@elem;
for($x5=0,$x6=1,$x7=2;$x7<$t1;$x5=$x5+3,$x6=$x6+3,$x7=$x7+3)
{$x8=@elem[$x5].@elem[$x6].@elem[$x7];
push(@seqcod,$x8);
}
$cont1=0;
foreach $x12 (@aacomb){
        foreach $x13 (@seqcod){
                     if($x12 eq $x13){$cont1++;$treehash{$x12}=$cont1;}
        }$cont1=0;
}$cont1=0;
print "@seqname[$x11]\n@seq[$x11]\n";
$cont3=0;
while( ($k,$v) = each %treehash ){ print "$k => $v\n";}
foreach $x16 (keys %treehash){delete $treehash{$x16};}
@seqcod=0;
}
