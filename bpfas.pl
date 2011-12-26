#!/usr/bin/perl
# bpfas.pl     krishna_bhakt@BHAKTI-YOGA     2006/09/03 09:41:15

use warnings;
use strict;
$|=1;
use Data::Dumper;

use Bio::Seq;
use Bio::SeqIO;
my $foo=shift @ARGV;

my $input_seqs = Bio::SeqIO->new ( '-format' => 'Fasta' , 
                                '-file'   => $foo
                              );

while ( my $s = $input_seqs->next_seq() )

   {
    #print( ">Sequence\n", $s->seq(), "\n" );
    #print( "\nLength:", $s->length(), "\n\n" );
    my $r = $s->revcom();
    print( ">Rev. Complement\n", $r->seq(), "\n" );
   }





__END__

=head1 NAME

bpfas.pl

=head1 SYNOPSIS



=head1 DESCRIPTION

Stub documentation for bpfas.pl, 
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
