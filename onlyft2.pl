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
use Math::Complex;
$pi=pi;
$i=sqrt(-1);
open (F, $file) || die "can't open \"$file\": $!";
$seq="";while ($line = <F>) {
        chomp ($line);
        if ($line =~ /^>/){
             @seqn=split(/\t/,$line);
                $snames=@seqn[0];$snames=~s/>//;1/1;
                chomp $snames;
             push(@seqname,$snames);
                if ($seq ne ""){
              push(@seq,$seq);
              $seq = "";
            }
      } else {$seq=$seq.$line;
      }
}
push(@seq,$seq);close F;
@base=qw/A T G C/;$ss1=@seq;#print "$ss1\t$ss2\n";
for($c1=0;$c1<$ss1;$c1++)
{
$seq=uc(@seq[$c1]);
$sname=@seqname[$c1];
$foo=$sname."ft"."\.out";
open FO,">$foo";
$N=length($seq);
FT(1,$N);
	sub FT {
	$st=shift;
	$sp=shift;
	$le=$sp-$st+1;
	$subs=substr($seq,($st-1),$le);
	$sfo=$sname."b".$st."e".$sp."out";
	open(SFO,">$sfo");
	@subssplit=split(//,$subs);
		for($k=1;$k<=($le/2);$k++)
		{
		 for($c6=0;$c6<=$#base;$c6++)
		 {
			$bvar=@base[$c6];
  			for($c7=0;$c7<=$#subssplit;$c7++)
			{$wsvar=@subssplit[$c7];
				if ($bvar eq $wsvar)
				{
				$subsum+=exp(2*$pi*$i*($k/$le)*($c7+1));#print"$wsvar\t$bvar\n";
				}
				else{$subsum+=0;}
			}
			$subsumtotal+=(((1/$le)**2)*(abs($subsum)**2));
			$subsum=0;
		 }push(@ssts,$subsumtotal);$substs+=$subsumtotal;$subsumtotal=0;
		}$substss=2*($substs/$le);$substs=0;
		
		for($c8=0;$c8<=$#ssts;$c8++)
		{
		
		$subptnr1=@ssts[$c8]/$substss;
		$subptnr2=$subptnr1/($sp*$substss);
		$subptnr3=$subptnr2*2;
		1/1;$pp=($c8+1)/$le;
		print SFO"$pp\t$subptnr1\t$subptnr2\t$subptnr3\n";
		}
		print "S(f) to Frequency written to file $sfo\n";
		undef @ssts;close SFO;
	}
}
