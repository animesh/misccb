#!/usr/bin/perl
# h2.pl                   krishna_bhakt@BHAKTI-YOGA
#                         21 Aug 2006

use warnings;
use strict;
$|=1;
use Data::Dumper;

use File::Path qw(mkpath);
use File::Basename;
use Env qw(HOME);

use Getopt::Std;
my %opts;
getopts('d', \%opts); 
my $DEBUG = $opts{d} || 1;
print "hi";
#for(){}



print "rr";
__END__

=head1 NAME

h2.pl

=head1 SYNOPSIS
   

=head1 DESCRIPTION
Stub documentation for h2.pl, 
created by template.el.

It looks like the author of this script was negligent
enough to leave the stub unedited.


=head1 AUTHOR

, E<lt>krishna_bhakt@BHAKTI-YOGAE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2006 by 

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.2 or,
at your option, any later version of Perl 5 you may have available.

=head1 BUGS

None reported... yet.

=cut
