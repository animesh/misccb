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
@base=qw/a t c g/;$bc=@base;
$file=shift @ARGV;
open(F,$file)||die "can't open";
$colo=shift @ARGV;$colo++;$jump=1;
$total=$bc**$colo;#print $total;
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
push(@seq,$seq);$seq = "";$tots2=@seqname;close F;
$plan[0][0]=0;
$dist[0][0]=0;
while($colo<=4){
	for($x11=0;$x11<$tots2;$x11++){$sname=@seqname[$x11];
		chomp $sname;
		$sname =~ s/\>//g;
		$d9=$x11+1;$plan[0][$d9]=$sname;$dist[0][$d9]=$sname;
		$seq=lc(@seq[$x11]);
		$len=length($seq);
		for($co2=0;$co2<=($len-$colo);$co2++)
			{$subs=substr($seq,$co2,$colo);
			chomp $subs;
			push(@fran,$subs);
			$co2n=$co2+1;
			push (@{$posito{$subs}},$co2n);
			}
		%seen=();
		@uniq = grep{ !$seen{$_} ++} @fran;undef @fran;
		$tots3=@uniq;print ">$sname\t$seq\t";
		print ">$sname\t @uniq\t$seq\n";
		for($c1=0;$c1<=$#uniq;$c1++){$cc=@uniq[$c1];$p1=@{$posito{$cc}};
			$subs1=substr($cc,($jump-1),($colo-1));
			$subs2=substr($cc,$jump,($colo-1));
			$subc=substr($cc,$jump,($colo-2));
			$subs="$subs1.$subs2";#print $subs;
			$mash{$subs}+=$p1/$total;
			for($c2=0;$c2<=$#base;$c2++){$bb1=@base[$c2];$strnew=$subs1.$bb1;
						$subs1=substr($strnew,($jump-1),($colo-1));
						$subs2=substr($strnew,$jump,($colo-1));
						$ct1=$seq =~ s/$strnew/$strnew/g;
						$strnew1="$subs1.$subs2";
						#print "$c2\t$strnew\t$ct1\t$subs1\t$subs2\n";
						if($ct1 == 0){$mash{$strnew1}=1/$total;}
						}
			#print "$cc\t$subs1\t$subs2\t$subc\t$p1\t";
			}#print "\n";
		undef @uniq;undef %posito;
	}$colo++;
}$colo=0;
foreach $k (keys %mash) {print "$k => $mash{$k}\n";}
