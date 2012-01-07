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
if(@ARGV != 4){print "usage:\tprogname c nc mm-model test\n";die;}
@base=qw/a t c g/;$bc=@base;
$file1=shift @ARGV;
$file2=shift @ARGV;
$co=shift @ARGV;$colo=$co+1;$jump=1;
$file3=shift @ARGV;
$total=$bc**$colo;#print $total;

(@sq1)=openfile($file1);undef @seqname;#print "sn\t\tsq @sq1\n";
(@sq2)=openfile($file2);undef @seqname;#print "sn\t\tsq @sq2\n";

%mash1=createhash(@sq1);
undef @sq1;undef %mash;
%mash2=createhash(@sq2);
undef @sq2;undef %mash;
#%mash11=formathash(\%mash1);

foreach $k (keys %mash1) {
	for($b2=0;$b2<=$#base;$b2++)
	{$su2=substr($k,0,$co);
	$su22=@base[$b2];
	$su222=$su2.$su22;
		if($mash1{$su222} eq ""){
		$mash1{$su222}=1;#print "$k => $mash1{$su222}\n";#print "$k => $mash2{$k}\n";
		}
	}
}
foreach $k (values %mash1) {$cash1+=$k;}
foreach $k (keys %mash1) {$mash1{$k}=$mash1{$k}/$cash1;1/1;}

foreach $k (keys %mash2) {
	for($b2=0;$b2<=$#base;$b2++)
	{$su2=substr($k,0,$co);
	$su22=@base[$b2];
	$su222=$su2.$su22;
		if($mash2{$su222} eq ""){
		$mash2{$su222}=1;#print "$k => $mash1{$su222}\n";#print "$k => $mash2{$k}\n";
		}
	}
}
foreach $k (values %mash2) {$cash2+=$k;}
foreach $k (keys %mash2) {$mash2{$k}=$mash2{$k}/$cash2;1/1;}

(@sq3)=openfile($file3);
for($fot=0;$fot<=$#sq3;$fot++){
$seq=lc(@sq3[$fot]);$seqname=@seqname[$fot];$len=length($seq);
		for($cot=0;$cot<=($len-$colo);$cot++)
			{$subs=substr($seq,$cot,$colo);
			if(($mash2{$subs} ne "") and ($mash1{$subs} ne ""))
				{$p=$mash1{$subs}/$mash2{$subs};1/1;
				$prob+=log($p);}
			elsif(($mash2{$subs} eq "") and ($mash1{$subs} eq ""))
				{$p=$cash2/$cash1;1/1;
				$prob+=log($p);}
			elsif($mash2{$subs} eq "")
				{$p=$mash1{$subs}/(1/$cash2);print "$p\n";
				$prob+=log($p);}
			elsif($mash1{$subs} eq "")
				{$p=(1/$cash1)/$mash2{$subs};
				$prob+=log($p);}
			}
print "$seqname\n$seq\n$prob\n";
$prob=0;
}
undef @sq3;
#print "$cash1\t$cash2\n";
#print "\n\ncoding\n\n";
#foreach $k (keys %mash1) {$v1{$k}=$mash1{$k}/$cash1;1/1;print "$k => $v1\n";}
#print "\n\nnon-coding\n\n";
#foreach $k (keys %mash2) {$v2=$mash2{$k}/$cash1;1/1;print "$k => $v2\n";}
sub openfile {
$file=shift;undef @seqname;undef @seq;
$seq="";
open(F,$file)||die "can't open";
while ($line = <F>) {
        chomp ($line);
        if ($line =~ /^>/){
             @seqn=split(/\t/,$line);
		#$snames=@seqn[0];
		$snames=$line;
		chomp $snames;
             push(@seqname,$snames);
                if ($seq ne ""){
              push(@seq,$seq);
              $seq = "";
            }
      } else {$seq=$seq.$line;
      }
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
