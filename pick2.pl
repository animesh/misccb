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
if( @ARGV ne 1){die "\nUSAGE\t\"ProgName MultSeqFile\t\n\n\n";}
$file = shift @ARGV;
open (F, $file) || die "can't open \"$file\": $!";$c=0;
$seq="";while ($line = <F>) {
        chomp ($line);
        if ($line =~ /^>/){$c++;print "reading\t seq no.$c\n";
             @seqn=split(/\t/,$line);$snames=$line;
                #$snames=@seqn[0];$snames=~s/>//;
                chomp $snames;
             push(@seqname,$snames);
                if ($seq ne ""){
              push(@seq,$seq);
              $seq = "";
            }
      } else {$seq=$seq.$line;
      }
}$c=0;
push(@seq,$seq);close F;#$div=int ($seql/$part);
$step=10000;$cc=1;$minseq=150;
$foo=$file."\.pick.out";
open F1,">$foo";$ss1=@seq;
for($c1=0;$c1<$ss1;$c1++)
{$fooo=$c1+1;
#$cc=$cc*$step;
#print "analysing \t seq no.$fooo\t";
$seq=uc(@seq[$c1]);
$sname=@seqname[$c1];
$N=length($seq);@t1[1]=~s/c//g;$c2=@t1[1]+0;
@t1=split(/:|' '|\-/,$sname);
#foreach $w (@t1){print "$w\t";}print "$sname\n";
	if(($c2>(($cc-1)*$step)) and ($c2<=($cc*$step)) and ($N>$minseq))
	{print F1"$sname\t$N\t$cc\n$seq\n";$cc++;
	}
}
