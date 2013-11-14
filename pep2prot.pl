use strict;
use warnings;
my %seqh;
my $seqc;
my $f1=shift @ARGV;
my $f2=shift @ARGV;
open(F1,$f1);
while(my $l1=<F1>){
	chomp $l1;
        $l1=~s/\r//g;
        if($l1=~/^>/){my @st=split(/\|/,$l1);$seqc=$st[1];}
        else{$l1=~s/[0-9]|\s+//g;$seqh{$seqc}.=uc($l1);}
}
close F1;

open(F2,$f2);
while(my $l2=<F2>){
	chomp $l2;
        $l2=~s/\r//g;
        my @temp=split(/\;/,$l2);
        print "$temp[1],$temp[0],";
        foreach my $seq (keys %seqh){
        	if($seqh{$seq}=~/$temp[1]/){print "$seq,"}
        }
        print "\n";
}
close F2;

__END__

perl pep2prot.pl /cygdrive/x/FastaDB/uniprot-human-oct-13.fasta /cygdrive/x/Qexactive/Mirta/QExactive/Bcell_Project/20131113_Bcell_Uracil_peptideList.txt > /cygdrive/x/Qexactive/Mirta/QExactive/Bcell_Project/20131113_Bcell_Uracil_peptideList.match.csv
