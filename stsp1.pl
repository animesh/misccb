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

$file=shift @ARGV;undef @seqname;undef @seq;
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
}push(@seq,$seq);close F;
for($fot=0;$fot<=$#seq;$fot++){
$seq=@seq[$fot];$seqname=@seqname[$fot];$len=length($seq);chomp $seq;
$st=substr($seq,0,3);
$m{$st}+=1;
$sp=substr($seq,($len-3),3);
$n{$sp}+=1;
}
print "Start CODONS in file $file\n";
foreach $w (keys %m){print "$w\t=>\t$m{$w}\n";$t+=$m{$w};}print "Total\t=>\t$t\n\n";
print "Stop CODONS in file $file\n";
foreach $w (keys %n){print "$w\t=>\t$n{$w}\n";$u+=$n{$w};}print "Total\t=>\t$u\n";
