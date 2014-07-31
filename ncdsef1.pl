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
$file=shift @ARGV;undef @seqname;undef @seq;
$seq="";
open(F,$file)||die "can't open";
$file2=shift @ARGV;
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
}push(@seq,$seq);close F;
close F;
open(F,$file2)||die "can't open";
while ($l=<F>)
{
        if($l=~/^ORIGIN/)
        {        while($ll=<F>)
                {
                $ll=~s/[0-9]//g;$ll=~s/\s+//g;chomp $ll;$line.=$ll;
                }
        }
    }
    $line=($line);$line=~s/\///g;1/1;$seql=length($line);
for($fot=0;$fot<=$#seq;$fot++){
$seq=@seq[$fot];$seqname=@seqname[$fot];$len=length($seq);chomp $seq;
@t1=split(/\s+|\[|\]|\_|\-/,$seqname);
$st=@t1[1]+@t1[6]-1;$sp=@t1[1]+@t1[9]-1+3;$length=$sp-$st+1;
#$c2=0;foreach $w (@t1){$c2++;print "$c2 \t $w\n";}
$str = uc(substr($line,($st-1),($length)));
$t11=@t1[0];$t11=~s/\>|\s+//g;
if($t11 eq "cIntergenic"){
$st=@t1[2]-@t1[9]+1-3;$sp=@t1[2]-@t1[6]+1;$length=$sp-$st+1;
$str = uc(substr($line,($st-1),($length)));
$str = reverse ($str);
$str =~ tr/ATCG/TAGC/d;
print "@t1[0]\[$st-$sp]\t$t1[4]\t$length\t$file\n$str\n$seq\n";
}
else{
print "@t1[0]\[$st-$sp]\t$t1[4]\t$length\t$file\n$str\n$seq\n";
}
}
