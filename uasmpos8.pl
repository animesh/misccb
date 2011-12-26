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
$tots2=@seq;
for($x11=0;$x11<$tots2;$x11++){
$testseq=lc(@seq[$x11]);
@elem=split(//,$testseq);
$t1=@elem;
	for($x5=0,$x6=1,$x7=2,$x8=3,$x9=4,$x10=5;$x10<$t1;$x5=$x5+1,$x6=$x6+1,$x7=$x7+1,$x8=$x8+1,$x9=$x9+1,$x10=$x10+1)
	{#print $x21;
	$x21=@elem[$x5].@elem[$x6].@elem[$x7].@elem[$x8].@elem[$x9].@elem[$x10];
	push(@seqcod,$x21);#print "$x21\n";
	}
$cont1=0;
foreach $x12 (@combos){
        foreach $x13 (@seqcod){
                     if($x12 eq $x13){#print "$x12\n";
		     		$cont1++;$treehash{$x12}=$cont1;
					while($testseq =~ /$x12/g)
						{
						$posi=pos($testseq);
						push (@temp,$posi);
						}
						#print "@temp\t";
						$posit{$x12}=[@temp];
						undef@temp;
		}
        	}
			$cont1=0;
	}
$cont1=0;
print "@seqname[$x11]\n";
$cont3=0;
while( ($k,$v) = each %treehash )
	{ print "$k => $v\t";
	#while($testseq =~ /$k/g)
     #   {
      #  $posi=pos($testseq);
       # push (@temp,$posi);
		#}
		#print "@temp\t";
		#$posit{$k}=@temp;
        #undef@temp;
	print "\n";
	}
while( ($kk,$vv) = each %posit )
	{ print "$kk => $vv\t";print "\n";
	}
foreach $x16 (keys %treehash){delete $treehash{$x16};}
foreach $x30 (keys %posit){delete $posit{$x30};}
@seqcod=0;
}
