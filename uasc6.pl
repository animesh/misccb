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
#programme for motif generation FASTER ALGO;
@base=qw/a t c g/;
#print "\nenter the multiple seq. containing fasta file name\t:";
$file="testy.txt";
#$file=<>;
#chomp $file;
open(F,$file)||die "can't open";
#print "\nenter the output filename for position matrix\t:";
$dout="outd.txt";
#$dout=<>;
#chomp $dout;
open (FD,">$dout");
#print "\nenter the output filename for -number of occurence- matrix\t:";
$out="outp.txt";
#$out=<>;
#chomp $out;
open (FO,">$out");
#print "\nenter the length of motif\t:";
#$colo=<>;
#chomp $colo;
$colo=6;
$seq = "";
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
}
push(@seq,$seq);$tots2=@seqname;
$plan[0][0]=0;
$dist[0][0]=0;
for($x11=0;$x11<$tots2;$x11++){$sname=@seqname[$x11];
chomp $sname;
$sname =~ s/\>//g;
$d9=$x11+1;$plan[0][$d9]=$sname;$dist[0][$d9]=$sname;
chomp $testseq;
$testseq=lc(@seq[$x11]);
$len=length($testseq);
print "$sname\n$testseq\n";
for($co2=0;$co2<=($len-$colo);$co2++)
	{$subs=substr($testseq,$co2,$colo);
	chomp $subs;
	push(@fran,$subs);
	$co2n=$co2+1;
	push (@{$posito{$subs}},$co2n);
	}
%seen=();
@uniq = grep{ !$seen{$_} ++} @fran;
$tots3=@uniq;
#foreach  (@uniq) {print "$_\t";}
foreach $cc (@uniq)
   {$cont = $testseq =~ s/$cc/$cc/g;
	if($cont ne ""){$treehash{$cc}=$cont;#print "$cc\t$cont\n";
					}
	}
for($d6=1;$d6<=$tots3;$d6++)
	{$d7=$d6-1;$plan[$d6][0]=@uniq[$d7];$dist[$d6][0]=@uniq[$d7];
	}
for($d4=1;$d4<=$tots3;$d4++)
		{foreach $d5 (keys %treehash) {$d7=$x11+1;
			if($plan[$d4][0] eq $d5){$plan[$d4][$d7]=@{$posito{$d5}};$dist[$d4][$d7]="@{$posito{$d5}}";#print @{$posito{$d5}};
			}
			}
		}
foreach $x16 (keys %treehash){delete $treehash{$x16};}
#print "\n";
#foreach $u (keys %posito){print "$u : @{$posito{$u}}\n";}
undef %posito;
#foreach $x30 (keys %posito){delete @{$posito{$x30}};}
}
$percent=20;
$threshold=($percent/100)*($tots2+1);
$comp=0;
for($d3=0;$d3<=$tots3;$d3++){
for($d2=0;$d2<=$tots2;$d2++){#print "$plan[$d3][$d2]\t";#$dist[$d3][$d2]
                if($plan[$d3+1][$d2+1] ne ""){$comp++ ;
                                         }
        }
        if($comp >= $threshold){$thresh{$plan[$d3+1][0]}=$comp}#print "\n";
        $comp=0;
}
print FO"\t";
print FD"\t";
for($d2=1;$d2<=$tots2;$d2++){print FD"$dist[0][$d2]\t";}print FD"\n";
for($d2=1;$d2<=$tots2;$d2++){print FO"$plan[0][$d2]\t";}print FO"\n";
foreach $uu (sort {$thresh{$b} cmp $thresh{$a}} keys %thresh){
$tots3=@uniq;
$tots2=@seqname;
print FO"$uu\t";
print FD"$uu\t";
for($d3=1;$d3<=$tots3;$d3++){
for($d2=1;$d2<=$tots2;$d2++){
if($plan[$d3][0] eq $uu) {print FO"$plan[$d3][$d2]\t";}
}if($plan[$d3][0] eq $uu) {print FO"\n";}
}
for($d3=1;$d3<=$tots3;$d3++){
for($d2=1;$d2<=$tots2;$d2++){
if($plan[$d3][0] eq $uu) {print FD"$dist[$d3][$d2]\t";}
}if($plan[$d3][0] eq $uu) {print FD"\n";}
}
#print "$uu : $thresh{$uu}\n";
}
#foreach $uuu (keys %stresh){print "$uuu : $stresh{$uuu}\n";}
#print "$threshold\n";