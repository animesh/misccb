#!/usr/bin/perl
use strict;
use warnings;

my $f1=shift @ARGV;chomp $f1;
my $thr=shift @ARGV;
my $seq;
my $seqn;

open F1,$f1||die"\nUSAGE:	\'perl program_name filename_2B_scanned\'\n\n";

while(my $l1=<F1>){
	chomp $l1;
	if($l1!~/^>/){
		$seq.=$l1;		
	}
	else{$seqn.=$l1;}
}
$seqn=~s/\>//g;
$seqn=~s/sp\|//;
my $lgt=length($seq);
for(my $c2=0;$c2<$thr;$c2++){
	my $protstr=substr($seq,$c2,$lgt); #split(/\,/,$l1);
	print ">sp|S$c2$seqn\n$protstr\n";
}


__END__

perl protcleave.pl /cygdrive/X/FastaDB/UNG1.fasta 36 2>0 > /cygdrive/X/FastaDB/UNG1c36UNG2c45.fasta
perl protcleave.pl /cygdrive/X/FastaDB/UNG.fasta 45 2>0 >> /cygdrive/X/FastaDB/UNG1c36UNG2c45.fasta
wc /cygdrive/X/FastaDB/UNG1c36UNG2c45rest.fasta
