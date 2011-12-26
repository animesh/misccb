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
# svm_learn.pl - svm_learn implementation using perl module
#

# $Id: svm_learn.pl,v 1.7 2001/08/29 15:12:29 taku-ku Exp $;
use TinySVM;

$model = $ARGV[$#ARGV];
$train = $ARGV[$#ARGV-1];
$param = join(" ",@ARGV[0..$#ARGV-2]);
die "not enough parameters\n" if (! $model || ! $train);

$e = new TinySVM::Example();
$e->read($train);
$m = $e->learn($param);

print "VC = ", $m->estimateVC(), "\n";
print "Margin = ", $m->estimateMargin(), "\n";
print "VCnum = ", $m->getSVnum(), "\n";
$m->write($model);
