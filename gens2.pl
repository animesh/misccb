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
$winsize=$ws;
$starttime=times();
use Math::Complex;
$i=sqrt(-1);
$pi=pi;
$f=1/3;
 for($j=1;$j<=300;$j++)
 {
 $e[$j]= exp(($i)*2*$pi*$f*$j );

 }
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
 #print"LENGTH OF THE SEQUENCE :$N is less then the windows length\n\n";
 #print"$sname\t$seq\t$fo\t$N\n\n";
           $winsq=(1/$winsize+(1/$winsize**2));
           $wincube= ($winsize**3) ;
           $flag=0;  $i=0;
           for($c2=1;$c2<=($N-$ws+1);$c2++)
             {$subseq=substr($seq,($c2-1),($winsize));
	     @window=split(//,$subseq);#print "$seq1\n";
	     #foreach $temp (@window){print "$temp\n";}
		for($c3=0;$c3<=$#base;$c3++)
			{
			$var=@base[$c3];
                	$sq=$sq+($n{$var}**2);
                	$s1 = 0;
                	$j=0;
   			foreach $base(@window)
                	{
		        $j++;
                	if ($base eq $var)
                      {
                        $s1=$s1+$e[$j];

                      } }
                     $s2=($s2+(abs$s1)**2); }
                    # $s2=$s2/(
                      #to calculate $sbar
                                $sq=$sq/$wincube;
                               $sbar=($winsq-$sq);
                                $sq=$sbar*($wincube)/2  ;
                              #to calculate signal to noise ratio
                              $s2=$s2/$sq;
                              $p=($s2/$sbar);

                             $s[$i]=$k;#print"$s[$i]\n";
                             $x[$i]=$p;#print"$x[$i]\n";
                             $i++;

                       }
                    $num_of_entries=$i;
                   # print "no of entries: $i";
                   for($i=0;$i<$num_of_entries;$i++)
                   {


                    if($x[$i]>4)
                      {
                         if($flag==0)
                           {
                               print "\n\n\nSTART: $s[$i]\n";
                               @start=(@start,$s[$i]);
                               $flag=1;
                           }
                       }
                      else
                      {
                          if($flag==1)
                          {
                              if($x[$i]<4)
                                   {
                                         print "\t\tEND: $s[$i-1]\n" ;
                                         @end=(@end,$s[$i-1]);
                                        $flag=0;
                                    }
                           }
                       } # End else

                     $i++;
                     }#end for
                       my $endtime=times();
                         $progtime=$endtime-$starttime;

                         print"time:$progtime\n";
}
