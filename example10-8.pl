#!/usr/bin/perl
# Example 10-8 - make a DBM index of a GenBank library,
#     and demonstrate its use interactively

use strict;
use warnings;
use BeginPerlBioinfo;     # see Chapter 6 about this module

# Declare and initialize variables
my $fh;
my $record;
my $dna;
my $annotation;
my %fields;
my %dbm;
my $answer;
my $offset;
my $library = 'library.gb';

# open DBM file, creating if necessary
unless(dbmopen(%dbm, 'GB', 0644)) {
    print "Cannot open DBM file GB with mode 0644\n";
    exit;
}

# Parse GenBank library, saving accession number and offset in DBM file
$fh = open_file($library);

$offset = tell($fh);

while ( $record = get_next_record($fh) ) {

    # Get accession field for this record.
    ($annotation, $dna) = get_annotation_and_dna($record);

    %fields = parse_annotation($annotation);

    my $accession = $fields{'ACCESSION'};

    # extract just the accession number from the accession field
    # -remove any trailing spaces
    $accession =~ s/^ACCESSION\s*//;

    $accession =~ s/\s*$//;

    # store the key/value of  accession/offset
    $dbm{$accession} = $offset;

    # get offset for next record
    $offset = tell($fh);
}

# Now interactively query the DBM database with accession numbers
#  to see associated records

print "Here are the available accession numbers:\n";

print join ( "\n", keys %dbm ), "\n";

print "Enter accession number (or quit): ";

while( $answer = <STDIN> ) {
    chomp $answer;
    if($answer =~ /^\s*q/) {
        last;
    }
    $offset = $dbm{$answer};

    if (defined $offset) {
        seek($fh, $offset, 0);
        $record = get_next_record($fh);
        print $record;
    }else{
        print "Do not have an entry for accession number $answer\n";
    }

    print "\nEnter accession number (or quit): ";
}

dbmclose(%dbm);

close($fh);

exit;
