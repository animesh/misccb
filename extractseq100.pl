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
use strict;
use lib "/home/fimm/ii/ash022/bioperl/";
use Bio::SeqIO;


my $main_file_pattern=shift @ARGV;chomp $main_file_pattern;
my $start=0;
my $frag=1000;
open(F,$main_file_pattern)||die "can't open";
my $seq;
my @seq;
my $fot;
my $line;
my @seqname;
my $snames;
while ($line = <F>) {
        chomp ($line);
        if ($line =~ /^>/){
		$snames=$line;
		chomp $snames;
             push(@seqname,$snames);
                if ($seq ne ""){
              push(@seq,$seq);
              $seq = "";
            }
      } else {$seq=$seq.$line;
      }
}push(@seq,$seq);
$seq="";
close F;
my $c1;
my $con1;
my $con2;
for($c1=$start;$c1<=$#seq;$c1+=$frag){
	$con1++;	
	my $c2;
	my $fo=$main_file_pattern.".$c1.out";
	open(FRA,">$fo")||die "can't open";
	for($c2=$c1;$c2<($c1+$frag);$c2++){
		$con2++;
		print FRA"@seqname[$c2]\n@seq[$c2]\n";

	}
	print "$con1\t$con2\n";
	close FRA;
}
print "$con1\t$con2\n";

