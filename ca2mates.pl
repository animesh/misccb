#!/usr/local/bin/perl

use strict;
use AMOS::AmosLib;
use TIGR::Foundation;

my $version = '$Revision: 1.6 $ ';

my $helptext = qq~
.USAGE.
  ca2mates [-a .asm file] prefix

.DESCRIPTION.
  Extracts BAMBUS .mates information from a .frg file.  If a .asm file
  is provided as well, it uses the library sizes specified in this file.

  if prefix ends in .frg program assumes that it represents the .frg file
  otherwise the program attaches the .frg extension when looking for the
  input file <prefix.frg>.

.OPTIONS.
  -a .asm file

.KEYWORDS.
  converter, celera assembler, mates
 ~;

my $base = new TIGR::Foundation;

if (! defined $base){
    print STDERR "Nasty error, hide!\n";
    exit(1);
}


$base->setHelpInfo($helptext);
$version =~ s/\$//g;
$base->setVersionInfo($version);

my $asmfile;
$base->TIGR_GetOptions("a=s" => \$asmfile);

my $infile;
my $ids = $ARGV[1];
$infile = $ARGV[0];

if (! defined $infile){
    $base->bail("You must provide a file");
}

#my $asmfile = $infile . ".asm";
my $frgfile;
if ($infile =~ /\.frg$/){
    $frgfile = $infile;
} else {
    $frgfile = $infile . ".frg";
}

my $outfile = $infile . ".mates";

my %sequences;


if (! -e $frgfile) {
    $base->bail("Cannot find $frgfile");
}

if (defined $asmfile){
    open(ASM, $asmfile) ||
	$base->bail("Cannot open $asmfile: $!");
}
open(OUT, ">$outfile") || 
    $base->bail("Cannot open $outfile: $!");

my $foundMDI = 0;

if (defined $asmfile){
    print STDERR "Reading $asmfile\n";
    
    while ( my $record = getRecord(\*ASM)){
	my ($type, $fields, $recs) = parseRecord($record);
	
	if ($type eq "MDI"){
	    my $id = getCAId($$fields{"ref"});
	    my $mean = $$fields{"mea"};
	    my $stdev = $$fields{"std"};
	    
	    my $min = $mean - 3 * $stdev;
	    if ($min < 0) {$min = 0;}
	    
	    my $max = $mean + 3 * $stdev;
	    
	    print STDERR "library\t$id\t$min\t$max\n";
	    print OUT "library\t$id\t$min\t$max\n";
	    
	    $foundMDI = 1;
	} else {
	    if ($foundMDI){
		print STDERR "Last record was an $type\n";
		last;
	    }
	}
    }
	
    close(ASM);
}

print STDERR "done\n";
print STDERR "Reading $frgfile\n";

open(FRG, "$frgfile") ||
    $base->bail("Cannot open $frgfile : $!");

while ( my $record = getRecord(\*FRG)){
    my ($type, $fields, $recs) = parseRecord($record);

    if ($type eq "DST"){
	if (! defined $asmfile){
	    my $id = getCAId($$fields{"acc"});
	    my $mean = $$fields{"mea"};
	    my $stdev = $$fields{"std"};
	    my $min = $mean - 3 * $stdev;
	    if ($min < 0) {$min = 0;}
	    
	    my $max = $mean + 3 * $stdev;
	    print OUT "library\t$id\t$min\t$max\n";
	}
    }elsif ($type eq "FRG"){
	my $id = getCAId($$fields{"acc"});
	my $name = getCAId($$fields{"src"});
	
	my @name = split('\n', $name);
	$name = join('', @name);

	$sequences{$id} = $name;
    } elsif ($type eq "LKG"){
        my $seqA;
        my $seqB; 
        if (defined $ids) {
	  $seqA = $$fields{"fg1"};
	  $seqB = $$fields{"fg2"};
        } else {
	  $seqA = $sequences{$$fields{"fg1"}};
	  $seqB = $sequences{$$fields{"fg2"}};
        }
	
	print OUT "$seqA\t$seqB\t$$fields{dst}\n";
    }
}

close(FRG);
close(OUT);

print STDERR "done\n";

exit(0);
