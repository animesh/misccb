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
#  - main program to test parse_features

use BeginPerlBioinfo;     # see Chapter 6 about this module

# Declare and initialize variables
my $fh;
my $record;
my $dna;
my $annotation;
my %fields;
my @features;
my $library = 'library.gb';

# Get the fields from the first GenBank record in a library
$fh = open_file($library);

$record = get_next_record($fh);

($annotation, $dna) = get_annotation_and_dna($record);

%fields = parse_annotation($annotation);

# Extract the features from the FEATURES table
@features = parse_features($fields{'FEATURES'});

# Print out the features
foreach my $feature (@features) {

     # extract the name of the feature (or "feature key")
     my($featurename) = ($feature =~ /^ {5}(\S+)/);

     print "******** $featurename *********\n";
     print $feature;
}

exit;

############################################################################ 
# Subroutine
############################################################################ 

# parse_features
#
#  extract the features from the FEATURES field of a GenBank record

sub parse_features {

     my($features) = @_;   # entire FEATURES field in a scalar variable

     # Declare and initialize variables
     my(@features) = ();   # used to store the individual features

     # Extract the features
     while( $features =~ /^ {5}\S.*\n(^ {21}\S.*\n)*/gm ) {
         my $feature = $&;
     push(@features, $feature);
     }

     return @features;
}
