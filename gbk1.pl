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
# Extract the annotation and sequence sections from the first
#   record of a GenBank library

use strict;
use warnings;
use BeginPerlBioinfo;     # see Chapter 6 about this module

# Declare and initialize variables
my $annotation = '';
my $dna = '';
my $record = '';
my $filename = 'record.gb';
my $save_input_separator = $/;

# Open GenBank library file
unless (open(GBFILE, $filename)) {
    print "Cannot open GenBank file \"$filename\"\n\n";
    exit;
}

# Set input separator to "//\n" and read in a record to a scalar
$/ = "//\n";

$record = <GBFILE>;

# reset input separator 
$/ = $save_input_separator;

# Now separate the annotation from the sequence data
($annotation, $dna) = ($record =~ /^(LOCUS.*ORIGIN\s*\n)(.*)\/\/\n/s);

# Print the two pieces, which should give us the same as the
#  original GenBank file, minus the // at the end
print $annotation, $dna;

exit;