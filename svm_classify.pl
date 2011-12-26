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

#
# svm_classify.pl - svm_classify implementation using perl module
#

# $Id: svm_classify.pl,v 1.5 2001/08/29 15:00:15 taku-ku Exp $;

use TinySVM;
use Getopt::Std;
getopts("v");

my $model = $ARGV[$#ARGV];
my $test  = $ARGV[$#ARGV-1];
die "not enough parameters\n" if (! $model || ! $test);

my $m = new TinySVM::Model();
$m->read($model);

my($all,$ok);
open(F,$test) || die "$! $test\n";
while(<F>) {
    chomp;
    if  (/^\s*([-\+]?1\.?0*)\s+/) {
	$all++;
	my($l,$v) = split(/\s+/,$_,2);
	my($r) = $m->classify($v);
	printf("%g %g\n",$l,$r) if ($opt_v);
	$ok++ if ($r * $l > 0);
    } else {
	printf("%g\n",$m->classify($_));
    }
}
close(F);

printf("Accuracy = %10.10f%% (%d/%d)\n",100.0*$ok/$all,$ok,$all) if ($all);
