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
$record;
$dna;
$annotation;
%fields;
@features;
$filename;
$filename= shift @ARGV; chomp $filename;
print "$filename\n";
(open(F,"$filename"))||die "Cannot open file $filename\n";
#while ($ll=<F>) {
#	if($ll=~/^ORIGIN/){
#			while ($l=<F>) {
#			$l=~s/[0-9]//g;
#			$l=~s/\s+//g;
#			chomp $l;
#			print $l
#			$line.=$l;
#			}
#	}
#	$line=~s/\///g;1/1;$seql=length($line);
#}close F;$line = uc($line);
print "$line\n$seql\n";
(open(F,"$filename"))||die "Cannot open file $filename\n";$fh=F;
#open(F,$filename)||die "can't open";
$record = get_next_record($fh);
($annotation, $dna) = get_annotation_and_dna($record);
%fields = parse_annotation($annotation);
@features = parse_features($fields{'FEATURES'});

foreach $feature (@features) {
     ($featurename) = ($feature =~ /^ {5}(\S+)/);
     print "$featurename\n";
     print $feature;
}

sub parse_features {

     ($features) = @_;
     (@features) = ();
     while( $features =~ /^ {5}\S.*\n(^ {21}\S.*\n)*/gm ) {
         $feature = $&;
     push(@features, $feature);
     }

     return @features;
}
sub get_next_record {

    ($fh) = shift;
    ($offset);
    ($record) = '';
    ($save_input_separator) = $/;

    $/ = "//\n";

    $record = <$fh>;

    $/ = $save_input_separator;

    return $record;
}
sub get_annotation_and_dna {

    ($record) = @_;

    ($annotation) = '';
    ($dna) = '';
    ($annotation, $dna) = ($record =~ /^(LOCUS.*ORIGIN\s*\n)(.*)\/\/\n/s);
    $dna =~ s/[\s\/\d]//g;

    return($annotation, $dna)
}
sub parse_annotation {

    ($annotation) = @_; 
    (%results) = (  );

    while( $annotation =~ /^[A-Z].*\n(^\s.*\n)*/gm ) {
        $value = $&;
        ($key = $value) =~ s/^([A-Z]+).*/$1/s;
        $results{$key} = $value;
    }
    return %results;
}
#/product="hypothetical protein Rv0004"
#/product="fadE24"