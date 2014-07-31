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
$file1=shift @ARGV;
$file2=shift @ARGV;
$co=shift @ARGV;$colo=$co+1;$jump=1;
$total=$bc**$colo;#print $total;
(@sq1)=openfile($file1);print "sn\t\tsq @sq1\n";
(@sq2)=openfile($file2);print "sn\t\tsq @sq2\n";
%mash1=createhash(@sq1);
undef @sq1;undef %mash;
%mash2=createhash(@sq2);
undef @sq2;undef %mash;
%mash11=formathash(\%mash1);
foreach $k (keys %mash11) {print "$k => $mash11{$k}\n";}

sub formathash{%mash=$$_;
foreach $k (keys %mash) {
	for($b2=0;$b2<=$#base;$b2++)
	{$su2=substr($k,0,$co);
	$su22=@base[$b2];
	$su222=$su2.$su22;print "$su222\n";
		if($mash{$su222} ne ""){
		$mash{$su222}=$mash{$k};
		print "$k => $mash{$k}\n";
		}
		else {
		$mash{$su222}=1;
		#print "$k => $mash2{$k}\n";
		}
	}
}return %mash;
}
sub openfile {
$file=shift;undef @seqname;undef @seq;
$seq="";
open(F,$file)||die "can't open";
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
      }undef @seqname;
}push(@seq,$seq);return (@seq);close F;
}

sub createhash{
	@seq=@_;
	for($x11=0;$x11<=$#seq;$x11++){
		$seq=lc(@seq[$x11]);chomp $seq;
		$len=length($seq);
		for($co2=0;$co2<=($len-$colo);$co2++)
			{$subs=substr($seq,$co2,$colo);
			#push(@fran,$subs);
			$mash{$subs}+=1;
			}
	}
#print "@seq\n";foreach $w (@fran) {print "$w\t";}print "\n";undef @fran;
return %mash;undef %mash;
}#foreach $k (keys %mash) {print "$k => $mash{$k}\n";}
