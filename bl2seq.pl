#!/usr/bin/perl
# bl2seq.pl     krishna_bhakt@BHAKTI-YOGA     2006/09/05 06:14:58

use warnings;
use strict;
$|=1;
use Data::Dumper;

use Bio::Tools::Run::StandAloneBlast;
use Bio::SeqIO;


my $foo1=shift @ARGV;
my $foo2=shift @ARGV;

my $seq1 = Bio::SeqIO->new ( '-format' => 'Fasta' , 
			     '-file'   => $foo1 );

my $seq2 = Bio::SeqIO->new ( '-format' => 'Fasta' , 
			     '-file'   => $foo2 );

my $factory = Bio::Tools::Run::StandAloneBlast->new(p => 'blastn',
                                                    e => '1');
#my $seq1 = Bio::PrimarySeq->new(-id => 'test1',
#                               -seq => 'AGATCAGTAGATGATAGGGGTAGA');
#my $seq2 = Bio::PrimarySeq->new(-id => 'test1',
#                               -seq => 'AGATCAGTAGATGATAGGGGTAGA');

#my $report = $factory->bl2seq($seq1,$seq2); # get back a Bio::SearchIO report
my $report = $factory->bl2seq($foo1,$foo2); # get back a Bio::SearchIO report


my $result = $report->next_result;

#my $blast_report = $factory->blastall($query);
#my $result = $blast_report->next_result;

#while( my $hit = $result->next_hit()) { 
#  print "\thit name: ", $hit->name(), " significance: ", $hit->significance(), "\n";}



while( my $hit = $result->next_hit()) {    
  print "\thit name: ", $hit->name(),"\n";    
  while( my $hsp = $hit->next_hsp()) { 	
    #print "E: ", $hsp->evalue(), "  frac_identical: ",	$hsp->frac_identical(), "\n";   # }}
   		   my $strand=$hsp->strand;
    #foreach (keys %stran){print "$_=>stran{$_}";}
    if($hsp->score>1000){
	print join("\t",
		   #$hsp->P,
		   #$hsp->percent,
           $hsp->score,
           $hsp->bits,
           #$hsp->percent,
           #$hsp->P,
           #$hsp->match,
           #$hsp->positive,
           $hsp->length,
           #$hsp->querySeq,
           #$hsp->sbjctSeq,
           #$hsp->homologySeq,
           $hsp->query->start,
           $hsp->query->end,
           $hsp->sbjct->start,
           $hsp->sbjct->end,
           $hsp->sbjct->seq_id,
           #$hsp->sbjct->overlaps($exon),
		   "start" , $hsp->query->start(), 
		   "end",$hsp->query->end(),
		   $hsp->score,
		   "Dir", $strand), "\n";
}}}




__END__

=head1 NAME

bl2seq.pl

=head1 SYNOPSIS



=head1 DESCRIPTION

Stub documentation for bl2seq.pl, 
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
