#!/usr/bin/perl
# h4.pl     krishna_bhakt@BHAKTI-YOGA     2006/08/21 12:18:00

use warnings;
use strict;
$|=1;
use Data::Dumper;
#use Text::Capitalize 0.3 qw(scramble_case);
while(<>){                                 
    print scramble_case($_);                
}                                          
sub scramble_case {
    return rand($_);
}




__END__

=head1 NAME

h4.pl


=head1 SYNOPSIS



=head1 DESCRIPTION

Stub documentation for h4.pl, 
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
