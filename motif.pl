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
open (F,"ys.txt");
open (FO,"qq.txt");
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
push(@seq,$seq);$tots2=@seqname;close F;
print FO"SeqName\tPosition\n";
#$motif=tgastcw;
#@to=split(//,$motif);
#for($c=0;$c<=$#to;$c++)
#{if(@to[$c] eq ( a or g or t or c ){$l=$l.@to[$c];}
#elsif(@to[$c] eq s){$l1=$l.g;$l2=$l.c;}
#elsif(@to[$c] eq w){$l3=$l.a;$l4=$l.t;}
#elsif(@to[$c] eq y){$l.=t;$l.=c;}
#elsif(@to[$c] eq p){$l.=g;$l.=a;}
#}
#push(
#$motif1=$motif;
#$motif1 =~ s/s/g/g;push(@gcnp,$motif1);
#$motif1=$motif;
#$motif1 =~ s/s/g/g;push(@gcnp,$motif1);
@gcnp=qw/tgagtca tgagtct tgactca tgactct/;
for($c=0;$c<$tots2;$c++)
{
#tgastcw
#for($x5=0,$x6=1,$x7=2,$x8=3,$x9=4,$x10=5,$x11=6;$x10<$t1;$x5=$x5+1,$x6=$x6+1,$x7=$x7+1,$x8=$x8+1,$x9=$x9+1,$x10=$x10+1,$x11=$x11+1)
#	{#print $x21;
#	$x21=@elem[$x5].@elem[$x6].@elem[$x7].@elem[$x8].@elem[$x9].@elem[$x10].@elem[$x11];
#	push(@seqcod,$x21);#print "$x21\n";
#	}
$testseq=lc(@seq[$c]);
foreach $tt (@gcnp) {$length=length($tt);
while($testseq =~ /$tt/g)
						{
						$posi=pos($testseq);
						$posi=($posi-($length+1));
						push(@temp,$posi);
						}
	}
print FO"@seqname[$c]\t@temp\n";
print "@seqname[$c]\t@temp\n";
undef @temp;
}
