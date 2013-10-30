use strict;
use warnings;
my $file=shift @ARGV;
open F1, "$file" or die "Can't open $file : $!";
my $thr=shift @ARGV;


my %seqh;
my $seqc;
my %cb;
my $tc;

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
	my @t=split(//,$se);
	my $seqaa="";
	my %cu;
	for (my $c2=0;$c2<=$#t-3;$c2=$c2+3) {
		$seqaa.=$c2a{$t[$c2].$t[$c2+1].$t[$c2+2]};
		$cb{$t[$c2].$t[$c2+1].$t[$c2+2]}++;
		$cu{$t[$c2].$t[$c2+1].$t[$c2+2]}++;
		$tc++;
	}
	return $seqaa,%cu;
}


while(my $l1=<F1>){
        $l1=~s/\r|\n|$//g;
        if($l1=~/^>/){$seqc=$l1;}
        else{$seqh{$seqc}.=uc($l1);}
}

foreach (keys %seqh){
        my $seqn=$_;
        my $seq=$seqh{$_};
        my $lgt=length($seq);
        my ($seqt,%cut)=translate($seq);
        if($cut{"AAA"}/($cut{"AAG"}+1)>$thr){print "$seqn\t$lgt\t",$cut{"AAA"},"-",$cut{"AAG"},"\n$seqt\n";}
}

__END__

my $chk;
foreach (keys %cb){
	$chk+=$cb{$_};
	my $freq=$cb{$_}/$tc;
        print "$_\t$c2a{$_}\t$freq\t$cb{$_}\t$chk\t$tc\n",;
}





__END__


sed 's/,/\n/g' /cygdrive/x/Elite/Aida/RNAproc.txt > /cygdrive/x/Elite/Aida/RNAprocsed.txt
sed 's/;/\n/g' /cygdrive/x/Elite/Aida/RNAprocRS.txt > /cygdrive/x/Elite/Aida/RNAprocRSsed.txt
 
$ perl codonusage.pl /cygdrive/x/Elite/Aida/SSwCLREP/sequence.fasta | wc
     64     128     466



ftp://ftp.ensembl.org/pub/release-73/fasta/homo_sapiens/cdna/Homo_sapiens.GRCh37.73.cdna.all.fa.gz
http://david.abcc.ncifcrf.gov/helps/knowledgebase/DAVID_gene.html#gene
http://www.ncbi.nlm.nih.gov/refseq/rsg/browse/
http://cardioserve.nantes.inserm.fr/mad/madgene/batch.php
http://cardioserve.nantes.inserm.fr/madtools/madgene/batch.php
http://www.ncbi.nlm.nih.gov/sites/batchentrez
ftp://ftp.ncbi.nlm.nih.gov/refseq/H_sapiens/RefSeqGene/
http://genome.ucsc.edu/cgi-bin/hgTables?hgsid=351570679&clade=mammal&org=Human&db=hg19&hgta_group=genes&hgta_track=refGene&hgta_table=0&hgta_regionType=genome&position=chr21%3A33%2C031%2C597-33%2C041%2C570&hgta_outputType=sequence&hgta_outFileName=

email: sharma.animesh@gmail.com
