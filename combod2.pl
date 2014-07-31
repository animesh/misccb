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
open(F,"ys.txt");
$out="co.txt";
while ($line = <F>) {
        chomp ($line);
        if ($line =~ /^>/){
             @seqn=split(/\t/,$line);
		$snames=@seqn[0];
		chomp $snames;
             push(@seqname,$snames);
                if ($seq ne ""){
              push(@seq,$seq);
              $seq = "";
            }
      } else {$seq=$seq.$line;
      }
}close F;
push(@seq,$seq);
$tots2=@seqname;
$plan[0][0]=0;$cont=0;
$colo=4;
while($colo<10){
for($x11=0;$x11<$tots2;$x11++){$sname=@seqname[$x11];
chomp $sname;
$sname =~ s/\>//g;
$d9=$x11+1;$plan[0][$d9]=$sname;$dist[0][$d9]=$sname;
chomp $testseq;
$testseq=lc(@seq[$x11]);
$len=length($testseq);
#print "$sname\n$testseq\n";
for($co2=0;$co2<=($len-$colo);$co2++)
	{$subs=substr($testseq,$co2,$colo);
	chomp $subs;
	push(@fran,$subs);
	}
%seen=();
@combos = grep{ !$seen{$_} ++} @fran;
undef %treehash;
undef %posito;
}
$colo++;
}
for($c1=0;$c1<=$#combos;$c1++){$r1=@combos[$c1];for($c2=0;$c2<=$#combos;$c2++){
	$r2=@combos[$c2];
if($r1 ne $r2){$cont++;
for($x11=0;$x11<$tots2;$x11++){$sname=@seqname[$x11];
		chomp $sname;
		$sname =~ s/\>//g;
		@tr=split(/ /,$sname);
		$sname=@tr[0];$plan[0][($x11+1)]=$sname;
		print "$sname\n";
		$testseq=lc(@seq[$x11]);
		$len1=length($r1);
		$len2=length($r2);
		$cor1=$testseq =~ s/$r1/$r1/g;
		$cor2=$testseq =~ s/$r2/$r2/g;
		while($testseq =~ /$cor1/g)
						{
						$posi=pos($testseq);
						$posi=($posi-($length+1));
						push(@temp1,$posi);
						}
		while($testseq =~ /$cor2/g)
						{
						$posi=pos($testseq);
						$posi=($posi-($length+1));
						push(@temp2,$posi);
						}
		for($tt1=0;$tt1<$#temp1;$tt1++){for($tt1=0;$tt1<$#temp1;$tt1++){
		$distance=@temp1[$tt1]-@temp2[$tt2];
		if( $distance <= 0 ){$distance=$distance*(-1);}
						if(($cor1 > 0 and $cor2 > 0) and ($cor1 < 4 and $cor2 < 4) and $distance < 50 and $distance > ($len1+$len2)){$comm++;
$plan[$cont][(0)]="$r1 and $r2";
$plan[$cont][($x11+1)]=$distance;
print "$r1\-$r2\*$cor1\-$cor2\t$distance\n";	
				}#else{$plan[$cont][($x11+1)]="$cor1 and $cor2";}
			}$desc{"$r1 and $r2"}=$comm;$comm=0;	#else{$cont;}
}}
	}else{#$plan[$cont][($x11+1)]="none";
		last;}
}
print "\n";
}
open (FO,">$out");
$contis=$cont+1;
for($d2=0;$d2<=$tots2;$d2++){
print FO"$plan[0][$d2]\t";}
print FO"\n";
foreach $eu (sort {$desc{$b} <=> $desc{$a}} keys %desc){
for($d3=1;$d3<=($contis);$d3++){for($d2=0;$d2<=$tots2;$d2++){
if($plan[$d3][0] eq $eu) {print FO"$plan[$d3][$d2]\t";}
}
if($plan[$d3][0] eq $eu) {print FO"$desc{$eu}\n";}
}
}
close FO;
open FG,"anir.txt";
for($d22=1;$d22<=$tots2;$d22++){$n1=$plan[0][$d22];
				for($d2=1;$d2<=$tots2;$d2++){$n2=$plan[0][$d2];print "seq1=>$n1\tseq2=>$n2\n";
					if($n1 eq $n2){$rmatix[$d22][$d2]=0;$rmatixn[$d22][$d2]=0;
					}
					else{for($d3=1;$d3<=$tots3;$d3++){
							$nnn+=(($plan[$d3][$d22]-$mean{$n1})*($plan[$d3][$d2]-$mean{$n2})/($sd{$n1}*$sd{$n2}));
$nnnn=((($plan[$d3][$d22]-$mean{$n1})*($plan[$d3][$d2]-$mean{$n2})/($sd{$n1}*$sd{$n2}))/($tots3));
							}$nnn=($nnn/($d3-1));$rmatix[$d22][$d2]=$nnn;$rmatix[$d2][$d22]=$nnn;
							$rmatixn[$d22][$d2]=((1-$nnn)/2);$rmatixn[$d2][$d22]=((1-$nnn)/2);
							$nnn=0;
						}
				}
			}
print FG"$tots2\n";
for($d4=1;$d4<=$tots2;$d4++){print FG"@seqname[($d4-1)]\t";
for($d5=1;$d5<=$tots2;$d5++){
print FG"$rmatixn[$d4][$d5]\t";}
print FG"\n";
}close FG;