#!/usr/bin/perl
 use Bio::SeqIO;
    $in  = Bio::SeqIO->new(-file => "duffy.fasta" , '-format' => 'fasta');
    $out = Bio::SeqIO->new(-file => ">duffy.embl" , '-format' => 'EMBL');
    # note: we quote -format to keep older perl's from complaining.
    while ( my $seq = $in->next_seq() )
     {
        $out->write_seq($seq);
     }