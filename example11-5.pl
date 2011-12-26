#!/usr/bin/perl
# Example 11-5   Extract sequence chains from PDB file

use strict;
use warnings;
use BeginPerlBioinfo;     # see Chapter 6 about this module

# Read in PDB file:  Warning - some files are very large!
my @file = get_file_data('pdb/c1/pdb1c1f.ent');

# Parse the record types of the PDB file
my %recordtypes = parsePDBrecordtypes(@file);

# Extract the amino acid sequences of all chains in the protein
my @chains = extractSEQRES( $recordtypes{'SEQRES'} );

# Translate the 3-character codes to 1-character codes, and print
foreach my $chain (@chains) {
    print "$chain\n";
    print iub3to1($chain), "\n";
}

exit;

################################################################################
# Subroutines for Example 11-5
################################################################################

# parsePDBrecordtypes
#
#-given an array of a PDB file, return a hash with
#    keys   = record type names
#    values = scalar containing lines for that record type 

sub parsePDBrecordtypes {

    my @file = @_;

    use strict;
    use warnings;
    
    my %recordtypes = (  );
    
    foreach my $line (@file) {
    
        # Get the record type name which begins at the
        # start of the line and ends at the first space

        # The pattern (\S+) is returned and saved in $recordtype
        my($recordtype) = ($line =~ /^(\S+)/);
    
        # .= fails if a key is undefined, so we have to
        # test for definition and use either .= or = depending
        if(defined $recordtypes{$recordtype} ) {
            $recordtypes{$recordtype} .= $line;
        }else{
            $recordtypes{$recordtype} = $line;
        }
    }
    
    return %recordtypes;
}

# extractSEQRES
#
#-given an scalar containing SEQRES lines,
#    return an array containing the chains of the sequence

sub extractSEQRES {

    use strict;
    use warnings;

    my($seqres) = @_;

    my $lastchain = '';
    my $sequence = '';
    my @results = (  );
    # make array of lines

    my @record = split ( /\n/, $seqres);
    
    foreach my $line (@record) {
        # Chain is in column 12, residues start in column 20
        my ($thischain) = substr($line, 11, 1);
        my($residues)  = substr($line, 19, 52); # add space at end
    
        # Check if a new chain, or continuation of previous chain
        if("$lastchain" eq "") {
            $sequence = $residues;
        }elsif("$thischain" eq "$lastchain") {
            $sequence .= $residues;
    
        # Finish gathering previous chain (unless first record)
        }elsif ( $sequence ) {
            push(@results, $sequence);
            $sequence = $residues;
        }
        $lastchain = $thischain;
    }

    # save last chain
    push(@results, $sequence);
    
    return @results;
}

# iub3to1
#
#-change string of 3-character IUB amino acid codes (whitespace separated)
#    into a string of 1-character amino acid codes

sub iub3to1 {

    my($input) = @_;
    
    my %three2one = (
      'ALA' => 'A',
      'VAL' => 'V',
      'LEU' => 'L',
      'ILE' => 'I',
      'PRO' => 'P',
      'TRP' => 'W',
      'PHE' => 'F',
      'MET' => 'M',
      'GLY' => 'G',
      'SER' => 'S',
      'THR' => 'T',
      'TYR' => 'Y',
      'CYS' => 'C',
      'ASN' => 'N',
      'GLN' => 'Q',
      'LYS' => 'K',
      'ARG' => 'R',
      'HIS' => 'H',
      'ASP' => 'D',
      'GLU' => 'E',
    );

    # clean up the input
    $input =~ s/\n/ /g;

    my $seq = '';
    
    # This use of split separates on any contiguous whitespace
    my @code3 = split(' ', $input);

    foreach my $code (@code3) {
        # A little error checking
        if(not defined $three2one{$code}) {
            print "Code $code not defined\n";
            next;
        }
        $seq .= $three2one{$code};
    }
    return $seq;
}
