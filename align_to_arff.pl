#!/usr/bin/perl
#align_to_arff.pl version 1.0
#This script converts sequence alignment data into an ARFF (attribute-relation file format) file, which describes a list
#of instances sharing a set of attributes. The ARFF format was developed by the Machine Learning Project at the Department
#of Computer Science of The University of Waikato.
#
#The Weka machine learning software uses the ARFF format
#http://www.cs.waikato.ac.nz/~ml/weka/
#
#To construct an ARFF file from sequence alignment data using this script, first create a file containing the
#aligned sequences in FASTA format. Next, modify the titles of the sequences so that they end with the text
#CLASS: X, where X is some category that the sequence falls into. For example, the sequences might correspond to enzymes
#with levels of activity that are high or low. The resulting input file might look like the following:
#
#>sequence1 CLASS: high
#MSEGRR-TS
#
#>some other sequence CLASS: high
#MSQARH-TS
#
#>another sequence CLASS: low
#MSEGRHTTS
#
#When executing the script you specify which positions in the alignment are to be included in the relation.
#
#If you specified that positions 4,5,6, and 7 are to be included in the ARFF file, the following would be
#generated:
#--------------------------------------------------------------------------------------
#% Title: multiple sequence alignment relation
#% Creator: align_to_arff.pl on Mon Dec  8 20:59:17 2003 (GM time)
#@relation Untitled
#
#@attribute position_4 { A, G }
#@attribute position_5 { R }
#@attribute position_6 { H, R }
#@attribute position_7 { _, T }
#@attribute class { high, low }
#
#@data
#%
#% 3 instances
#%
#G, R, R, -, high
#A, R, H, -, high
#G, R, H, T, low
#--------------------------------------------------------------------------------------
#
#There are four command line options:
#-i input file
#-o output file
#-c the columns of the alignment to use when constructing the relation.
#-t the title of the relation.
#
#example usage:
#perl align_to_arff.pl -i my_sequences.txt -o my_sequences.arff -c 4,5,20 -t relation between active site and activity
#
#Written by Paul Stothard, Genome Canada Bioinformatics Help Desk.
#
#stothard@ualberta.ca

use strict;
use warnings;

#These simple modules were written for this script.
use ARFF::Sequence;
use ARFF::Attribute;
use ARFF::Data;
use ARFF::Relation;

#Command line processing.
use Getopt::Long;

my $inputFile;
my $outputFile;
my $relationTitle;
my $columns;

Getopt::Long::Configure ('bundling');
GetOptions ('i|input_file=s' => \$inputFile,
	    'o|output_file=s' => \$outputFile,
	    't|relation_title=s' => \$relationTitle,
	    'c|columns=s' => \$columns);

if(!defined($inputFile)) {
    die ("Usage: align_to_arff.pl -i <input file> -o <output file> -t <relation title> -c <columns to use>\n");
}

if(!defined($outputFile)) {
    die ("Usage: genome_search.pl -i <input file> -o <output file> -t <relation title> -c <columns to use>\n\n");
}

if(!defined($relationTitle)) {
    $relationTitle = "Untitled";
}

if(!defined($columns)) {
    die ("Usage: genome_search.pl -i <input file> -o <output file> -t <relation title> -c <columns to use>\n\n");
}

#create a Sequence object for each sequence in the input file
my @arrayOfSequences = ();
my $sequenceTitle;
my $sequenceClass;

open (DNAFILE, $inputFile) or die( "Cannot open file : $!" );

$/ = ">";

while (my $sequenceEntry = <DNAFILE>) {

    if ($sequenceEntry eq ">"){
	next;
    }
    
    #sequence title CLASS: high
    if ($sequenceEntry =~ m/^([^\n]+)class\:\s*([^\n]+)\n/i) {
	$sequenceTitle = $1;
	$sequenceClass = $2;
    }
    else {
	die ("A sequence title could not properly be read for this entry:\n $sequenceEntry.");
    }

    #remove title
    $sequenceEntry =~ s/^[^\n]+//;
    #remove white space and >
    $sequenceEntry =~ s/[\s\>]//g;

    #convert white-space in class to underscore
    $sequenceClass =~ s/\s+/\-/g;

    #create sequenceObject
    my $sequenceObject = new ARFF::Sequence;
    $sequenceObject->setClass($sequenceClass);
    $sequenceObject->setSequence($sequenceEntry);

    push (@arrayOfSequences, $sequenceObject);
}

close (DNAFILE) or die( "Cannot close file : $!");

#Do a quick check of the sequences to confirm that they contain the same number of characters
my $length;
foreach(@arrayOfSequences) {
    if (defined($length)) {
	if ($length != $_->getLength()) {
	    die ("The sequences in the alignment must all contain the same number of characters");
	}
    }
    else {
	$length = $_->getLength();
    }
}

#convert the column text into an array of digits and confirm that each position is acceptable.
my @arrayOfPositions = ();
while ($columns =~ m/(\d+)/g) {
    if (($1 < 1) || ($1 > $length)) {
	die ("The position values for the entered sequences must be between 1 and $length");
    }
    
    push (@arrayOfPositions, $1);
}

#create a Relation object
my $relation = new ARFF::Relation;
$relation->setTitle($relationTitle);

#for each position, create an Attribute object and add it to the relation.
foreach(@arrayOfPositions) {
    my $attribute = new ARFF::Attribute;
    my $position = $_;
    $attribute->setTitle("position_" . $position);
    
    foreach(@arrayOfSequences) {
	$attribute->addValue($_->getResidue($position));
    }
    $relation->addAttribute($attribute);
}

#create an Attribute object for the sequence classes.
my $attribute = new ARFF::Attribute;
$attribute->setTitle("class");
foreach(@arrayOfSequences) {
    $attribute->addValue($_->getClass());
}
$relation->addAttribute($attribute);

#now create the data entry for each sequence
foreach(@arrayOfSequences) {
    my $data = new ARFF::Data;
    my $sequence = $_;
    foreach(@arrayOfPositions) {
	$data->addValue($sequence->getResidue($_));
    }
    #add the class as the last entry
    $data->addValue($sequence->getClass());
    $relation->addData($data);
}

#now write the relation to file
open(OUTFILE, ">" . $outputFile) or die ("Cannot open file for output: $!");
print(OUTFILE $relation->toString());
close(OUTFILE) or die ("Cannot close file for output: $!");

