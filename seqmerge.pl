use strict;
#use warnings;
sub genseq{
	my $len=shift;
	my $c;
	my $seq;
	my @b=qw/A T G C/;
	my @b=qw/A C A C/;
	my $sn=">Seq$len";
	while($c<$len){
		$seq.=@b[int(rand(4))];
		$c++; 
	}
	print "$sn\n$seq\n";
	return($sn,$seq);
}
sub splitseq{
	my $seq=shift;
	my $len=shift;
	my $cov=shift;
	my $cnt=0;
	my $slen=length($seq);
	my @seqscoll;
	for(my $c=0;$c<$cov*$slen/$len;$c++){
		my $ss=substr($seq,($c)%($slen),$len);
			$cnt++;
			push(@seqscoll,$ss);
			print "$cnt\t$c\t$ss\n";
			print FO"";
	}
	return(\@seqscoll);
}
my $fo="@ARGV[0].@ARGV[1],@ARGV[2].cg";
open(FO,">fo");
my ($sn,$seq)=genseq(@ARGV[0]);
my $seqs=splitseq($seq,@ARGV[1],@ARGV[2]);
my @seqsc=@$seqs;
for(my $c1=0;$c1<=$#seqsc;$c1++){
	for(my $c2=$c1+1;$c2<=$#seqsc;$c2++){
		if($seqsc[$c1]=~/$seqsc[$c2]/){
			print "$c1\t$seqsc[$c1]\t$c2\t$seqsc[$c2]\n";
		}
	}
}

