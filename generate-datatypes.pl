#!/usr/bin/perl -w
#
# Generate datatypes.
#
# $Id: generate-datatypes.pl,v 1.6 2006/10/13 22:08:39 senger Exp $
# Contact: Martin Senger <martin.senger@gmail.com>
# -----------------------------------------------------------

# some command-line options
use Getopt::Std;
use vars qw/ $opt_h $opt_d $opt_v $opt_s /;
getopt;

# usage
if ($opt_h) {
    print STDOUT <<'END_OF_USAGE';
Generate datatypes.
Usage: [-vds] [data-type-name] [data-type-name...]

    It also needs to get a location of a local cache (and potentially
    a BioMoby registry endpoint, and an output directory). It takes
    it from the 'moby-service.cfg' configuration file.

    If no data type given it generates all of them.

    -s ... show generated code on STDOUT
           (no file is created, disabled when no data type name given)
    -v ... verbose
    -d ... debug
    -h ... help
END_OF_USAGE
    exit (0);
}
# -----------------------------------------------------------

use FindBin qw( $Bin );
use lib "$Bin/../Perl";   # assuming: Perl/MOSES/...
                          #           scripts/generate-datatypes.pl
use strict;

use MOSES::MOBY::Base;
use MOSES::MOBY::Generators::GenTypes;

$LOG->level ('INFO') if $opt_v;
$LOG->level ('DEBUG') if $opt_d;

sub say { print @_, "\n"; }

my $generator = new MOSES::MOBY::Generators::GenTypes;
if (@ARGV) {
    say 'Generating ' . (@ARGV+0) . '+ data types.';
    if ($opt_s) {
	my $code = '';
	$generator->generate (datatype_names => [@ARGV],
			      with_docs      => 1,
			      outcode        => \$code);
	say $code;
    } else {
	$generator->generate (datatype_names => [@ARGV]);
    }
} else {
    say 'Generating all data types.';
    $generator->generate;
}
say 'Done.';


__END__
