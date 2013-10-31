use strict;
use warnings;
my $file=shift @ARGV;
open F1, "$file" or die "Can't open $file : $!";
my $thr=shift @ARGV;


my %seqh;
my $seqc;
my $cl=3;
my %c2a = (
	'TTT' => 'F','TTC' => 'F','TTA' => 'L','TTG' => 'L',
	'TCT' => 'S','TCC' => 'S','TCA' => 'S','TCG' => 'S',
	'TAT' => 'T','TAC' => 'T','TAA' => 'stop','TAG' => 'stop',
	'TGT' => 'C','TGC' => 'C','TGA' => 'stop','TGG' => 'W',

	'CTT' => 'L','CTC' => 'L','CTA' => 'L','CTG' => 'L',
	'CCT' => 'P','CCC' => 'P','CCA' => 'P','CCG' => 'P',
	'CAT' => 'H','CAC' => 'H','CAA' => 'Q','CAG' => 'Q',
	'CGT' => 'R','CGC' => 'R','CGA' => 'R','CGG' => 'R',

	'ATT' => 'I','ATC' => 'I','ATA' => 'I','ATG' => 'M',
	'ACT' => 'T','ACC' => 'T','ACA' => 'T','ACG' => 'T',
	'AAT' => 'N','AAC' => 'N','AAA' => 'K','AAG' => 'K',
	'AGT' => 'S','AGC' => 'S','AGA' => 'R','AGG' => 'R',

	'GTT' => 'V','GTC' => 'V','GTA' => 'V','GTG' => 'V',
	'GCT' => 'A','GCC' => 'A','GCA' => 'A','GCG' => 'A',
	'GAT' => 'D','GAC' => 'D','GAA' => 'E','GAG' => 'E',
	'GGT' => 'G','GGC' => 'G','GGA' => 'G','GGG' => 'G',
);

sub translate{
	my $se=shift;
        my $lt=length($se);
	my $ct=int($lt/$cl);
	my $rr=$lt%$cl;
	my $sa="";
	my %cu;
	my $cp;
	for (my $c2=0;$c2<$ct;$c2++) {
		my $sp=$c2*$cl;
		my $aa=substr($se,$sp,$cl);
		$sa.=$c2a{$aa};
		$cu{$aa}++;
		if($c2a{$aa} eq "stop"){$cp.="$sp-";}
	}
	return($sa,$cp,$lt,$rr,%cu);
}


while(my $l1=<F1>){
	chomp $l1;
        $l1=~s/\r//g;
        if($l1=~/^>/){$seqc=$l1;}
        else{$l1=~s/[0-9]|\s+//g;$seqh{$seqc}.=uc($l1);}
}

foreach (keys %seqh){
        my $seqn=$_;
        my $seq=$seqh{$_};
        my ($seqt,$scp,$lgt,$rem,%cut)=translate($seq);
        print "$seqn\t$lgt\t$rem\t$scp\t",$cut{"AAA"},"\t",$cut{"AAG"},"\n";
}

__END__



perl codonusage.pl /cygdrive/x/Elite/gaute/geir.txt 


http://cardioserve.nantes.inserm.fr/mad/madgene/batch.php
http://cardioserve.nantes.inserm.fr/madtools/madgene/batch.php
http://www.ncbi.nlm.nih.gov/sites/batchentrez
ftp://ftp.ncbi.nlm.nih.gov/refseq/H_sapiens/RefSeqGene/
http://genome.ucsc.edu/cgi-bin/hgTables?hgsid=$cl51570679&clade=mammal&org=Human&db=hg19&hgta_group=genes&hgta_track=refGene&hgta_table=0&hgta_regionType=genome&position=chr21%$clA$cl$cl%2C0$cl1%2C597-$cl$cl%2C041%2C570&hgta_outputType=sequence&hgta_outFileName=

email: sharma.animesh@gmail.com
