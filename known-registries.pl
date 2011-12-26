#!/usr/bin/perl -w
#
# List of known BioMoby registries.
#
# $Id: known-registries.pl,v 1.2 2006/10/13 22:08:39 senger Exp $
# Contact: Martin Senger <martin.senger@gmail.com>
# -----------------------------------------------------------

use FindBin qw( $Bin );
use lib "$Bin/../Perl";   # assuming: Perl/MOSES/...
                          #           scripts/install.pl
use MOSES::MOBY::Cache::Registries;
use Data::Dumper;
use strict;

sub say { print @_, "\n"; }

say join (", ", MOSES::MOBY::Cache::Registries->list);
say (Data::Dumper->Dump ( [ MOSES::MOBY::Cache::Registries->all ], ['Registries']));


__END__

