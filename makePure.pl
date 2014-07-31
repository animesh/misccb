use strict;
use warnings;

# Usage:
# makePure.pl scaffolds_to_exclude.txt GenomeScaffolds.txt > GenomeScaffoldsPure.txt

my %scafs;

open (INEXCL, "< $ARGV[0]") or die ("Cannot open file $ARGV[0]: $!");
open (INSCAF, "< $ARGV[1]") or die ("Cannot open file $ARGV[1]: $!");


while (<INEXCL>){
	chomp;
	$scafs{$_}++
}

while (<INSCAF>){
	my ($name) = split;
	print $_ if not defined $scafs{$name};
}
