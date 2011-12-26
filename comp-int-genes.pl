#!/usr/bin/perl
use strict;

#      137     92587    803536 bacfugu.int.txt
#      316   1391983  11922444 bacmedaka.int.txt
#      407   3160850  27018593 bacstickle.int.txt
#      551  31344373 279259172 bactetraodon.int.txt
#      333   1795019  16028863 baczf.int.txt
#     1744  37784812 335032608 total


my $fugufile="bacfugu.int.txt";
my $medakafile="bacmedaka.int.txt";
my $sticklefile="bacstickle.int.txt";
my $tetrafile="bactetraodon.int.txt";
my $zffile="baczf.int.txt";

openfile($fugufile);
openfile($medakafile);
openfile($sticklefile);
openfile($tetrafile);
openfile($zffile);

my %bactag;
my %baccont;
my %genecont;
my %transcont;

sub openfile {
	my $file=shift;
	#my $afile="a".$file;
	my $line;
	open(F,$file);
	#my @{$file};
	while($line=<F>){
		my %uniqgene;
		my %uniqtrans;
		chomp $line;
		my @afile=split(/\s+/,$line);
		my @dist=split(/\-/,$afile[5]);
		my $len=abs($dist[1]-$dist[0]);
		$bactag{$afile[1]}++;
		my $c;
		for ($c=0;$c<=$#afile;$c++) {
			if($afile[$c] eq "XREF"){
				$genecont{$afile[1]}.="$afile[$c+1],";
				$uniqgene{$afile[$c+1]}++;
			}
			if($afile[$c]=~/[a-z|A-Z|0-9]*\_[a-z|A-Z|0-9]*/){
				$transcont{$afile[1]}.="$afile[$c],";
				$uniqtrans{$afile[$c]}++;
			}
		}
		$baccont{$afile[1]}.="$file-$len-$afile[2]-$afile[4]-$afile[5]-$afile[6]-$afile[-3]-$afile[-2]-";
		foreach(keys %uniqgene){$baccont{$afile[1]}.="$_:$uniqgene{$_}-";}
		foreach(keys %uniqtrans){$baccont{$afile[1]}.="$_:$uniqtrans{$_}-";}
		$baccont{$afile[1]}.="\t";
		#print "$file\t$afile[1]\n";
	}
}

foreach (sort {$bactag{$b}<=>$bactag{$a}} keys %bactag){
	print "$_\t$bactag{$_}\t$baccont{$_}\t\n";#\t$genecont{$_}\t$transcont{$_}\n";
}
