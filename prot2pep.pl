use strict;
use warnings;
use Text::ParseWords;

my %seqh;
my %sfull;
my $seqc;
my $f1=shift @ARGV;
my $pid=shift @ARGV;
#my $pid=5;
my $col=25;
my %pephsh;
my %pepcnt;

open(F1,$f1);
while(my $l1=<F1>){
	chomp $l1;
        $l1=~s/\r//g;
        if($l1=~/^>/){my @st=split(/\|/,$l1);$seqc=$st[1];$sfull{$seqc}=$l1;}
        else{$l1=~s/[0-9]|\s+//g;$seqh{$seqc}.=uc($l1);}
}
close F1;

foreach my $seq (keys %seqh){
	my $lgt=length($seqh{$seq});	
	for(my $c1=0;$c1<=$lgt-$pid;$c1++){
		my $st=substr($seqh{$seq},$c1,$pid);
		if($st=~/^[KR]/ and $st=~/[KR]$/){
			$pephsh{$st}.="$seq;";
			$pepcnt{$st}++;
			#print "$seq\t$sfull{$seq}\t$seqh{$seq}\t$lgt\t$st\n";
		}
	}
}

foreach my $pep (keys %pepcnt){
	print "$pep\t$pepcnt{$pep}\t$pephsh{$pep}\n";
}

__END__

perl pep2prot.pl /cygdrive/l/Qexactive/Mirta/QExactive/Bcell_Project/combined.fasta  6 > /cygdrive/l/Qexactive/Mirta/QExactive/Bcell_Project/combinedpeplist5.txt




