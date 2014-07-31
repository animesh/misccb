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


#!  usr/bin/perl
print"\n::\tenter the multiple sequence file\t::";$file=<>;chomp($file);
#print"\n::\tenter the periodicity chek no.\t::";$f=<>;chomp($f);
#print"\n::\tenter the window length\t::";$ws=<>;chomp($ws);
$ws=300;
#$starttime=times();
use Math::Complex;
$pi=pi;
$i=sqrt(-1);
$f=1/3;
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
$fo=$sname."ft"."\.out";
open FO,">$fo";
$N=length($seq);
$len=($N-$ws+1);
	for($c2=1;$c2<=$len;$c2++)
	{
	$subseq=substr($seq,($c2-1),($ws));
	@wssplit=split(//,$subseq);#print "$seq1\n";
	#foreach $temp (@window){print "$temp\n";}
		for($c3=0;$c3<=$#base;$c3++)
		{
		$bvar=@base[$c3];
  			for($c4=0;$c4<=$#wssplit;$c4++)
			{$wsvar=@wssplit[$c4];
				if ($bvar eq $wsvar)
                      		{
                        	$sum+=exp(2*$pi*$i*$f*($c4+1));#print "$wsvar\t$bvar\n";
				}
				else{$sum+=0;}
			}
			$sumtotal+=(((1/$ws)**2)*(abs($sum)**2));$sum=0;
		}
		$sbar=(1/$ws)*(1+(1/$ws)-$sumtotal);
		$ptnr=$sumtotal/$sbar;
		$sumtotal=0;$ll=$c2+$ws;
		if($ptnr ge 4.0)
                   	{
			print "$c2 to $ll of length $ws is $ptnr\n";
   			}
                else
                      	{
			print "ELSE$c2 to $ll of length $ws is $ptnr\n";
			}
	} # End else
}