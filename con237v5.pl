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
@base=qw/A T C G/;
open(F,"237UAS1000.txt")||die "can't open";
$seq = "";
while ($line = <F>) {
        chomp ($line);
        if ($line =~ /^>/){
             @seqn=split(/\t/,$line);
             push(@seqname,@seqn[0]);
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
$sequence=join(//,@seq);
foreach $b1 (@base){foreach $b2 (@base){foreach $b3 (@base){foreach $b4 (@base){foreach $b5(@base){foreach $b6 (@base){$comb=$b1.$b2.$b3.$b4.$b5.$b6;push(@combos,$comb);}}}}}}
print "Hexamer\tHexamer Count\n";
{foreach $cc (@combos)
   {    $cont = $sequence =~ s/$cc/$cc/g;
        print "$cc\t$cont";
        print "\n";
   }
}
