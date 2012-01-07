#!/usr/bin/perl
 use Bio::SeqIO;
    print "enter the name of the fasta file\n";
    $chu=<STDIN>;
    chomp$file;
    $in  = Bio::SeqIO->new(-file => $chu , '-format' => 'fasta');
    $out = Bio::SeqIO->new(-file => ">duffy.embl" , '-format' => 'EMBL');
    # note: we quote -format to keep older perl's from complaining.
    while ( my $lun = $in->next_seq() )
     {
        $out->write_seq($lun);
     }