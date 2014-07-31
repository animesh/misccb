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
@base=qw/a t c g/;
open(F,"testy.txt")||die "can't open";
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
foreach $b1 (@base){foreach $b2 (@base){foreach $b3 (@base){foreach $b4 (@base){foreach $b5(@base){foreach $b6 (@base){$comb=$b1.$b2.$b3.$b4.$b5.$b6;push(@combos,$comb);}}}}}}
#print "Sequence Name\tHexamer\tHexamer Count\tHexamer Position\n";
for($s=0;$s<=$#seq;$s++)
{print "@seqname[$s]\t@seq[$s]\n"; 
	foreach $cc (@combos)
   {    
	$ts=lc(@seq[$s]);
	$cont = $ts =~ s/$cc/$cc/g;
		if($cont eq ""){next;}
	   else{#print "\n";#$cont1++;$treehash{$cc}=$cont;
		$treehash{$cc}=$cont;
		#print "@seqname[$s]\t $cc \t $cont \t";
			while(@seq[$s] =~ /$cc/g)
			{
			$posi=pos(@seq[$s]);
			$post{$cc}=$posi;
			#print "$cc\t$cont\t$posi\t";
			}
	}}
foreach $dine (keys %treehash) {
	print "$dine\t$treehash{$dine}\t$post{$dine}\n";
}
#while( ($k,$v) = each %treehash ){ print "$k => $v\n";}
foreach $x16 (keys %treehash){delete $treehash{$x16};}
#while( ($kk,$vv) = each %post ){ print "$kk => $vv\n";}
foreach $x26 (keys %post){delete $post{$x26};}
#@seqcod=0;
#print "\n";
}