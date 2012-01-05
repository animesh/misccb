use strict;
use warnings;

#Simulate circular genome of length $seqlen
sub genseq{
	my $seqlen=shift;
	my $c=0;
	my $seq;
	my @b=qw/A T G C/;
	#my @b=qw/A C A C/;
	my $sn=">Seq$seqlen";
	while($c<$seqlen){
		$seq.=@b[int(rand(4))];
		$c++; 
	}
	print "$sn\n$seq\n";
	return($sn,$seq);
}

#Split simulated sequence in $len k-mers reads with $ovl overlap reflecting sum of read length as a multiple of $cov coverage and length of simulated genome $seqlen
sub splitseq{
	my $seq=shift;
	my $len=shift;
	my $cov=shift;
	my $ovl=shift;
	my $cnt=0;
	my $slen=length($seq);
	my @seqscoll;
	for(my $c=0;$c<$cov*$slen*($len-$ovl)/$len;$c+=($len-$ovl)){
		my $ss=substr($seq,($c)%($slen),$len);
		if($c%$slen+$len>$slen){$ss.=substr($seq,0,($c+$len)%$slen)}
		$cnt++;
		push(@seqscoll,$ss);
		#print "$cnt\t$c-$len-$slen\t$ss\n";
	}
	return(\@seqscoll);
}

sub breadth{
           my $reads = shift;
           my @read = @$reads;
           while(scalar(@read) > 0){
                 my @t;
                 foreach my $r (@read){
                       print $r."\n";
                       my ($con,$r)=get_con($r);
                       map { &proc_con($_);} @$con;
                       push @t, @$r;
                 }
                 @read = @t;
            }
     }


#Calling routing to simulate genome
my $fg="Genome.L$ARGV[0].K$ARGV[1].C$ARGV[2].O$ARGV[3].fa";
open(FG,">$fg");
my ($sn,$seq)=genseq($ARGV[0]);
print FG"$sn\n$seq\n";
close FG;

#Calling routine to generate reads from the simulated genome
my $seqs=splitseq($seq,$ARGV[1],$ARGV[2],$ARGV[3]);

#Comparing reads against each other to find perfect match (very restricted version of read alignment ;)
my @seqsc=@$seqs;
my $fo="Genome.L$ARGV[0].K$ARGV[1].C$ARGV[2].O$ARGV[3].cg";
open(FO,">$fo");
for(my $c1=0;$c1<=$#seqsc;$c1++){
	if($c1<$#seqsc){print FO"$seqsc[$c1]\t$seqsc[$c1+1]\t";}
	if($c1>0){print FO"$seqsc[$c1-1]\t";}
	for(my $c2=$c1+1;$c2<$#seqsc;$c2++){
		if(($seqsc[$c1] eq $seqsc[$c2]) and ($seqsc[$c1-1] ne $seqsc[$c2-1]) and ($seqsc[$c1+1] ne $seqsc[$c2+1])){
			print "$c1\t$seqsc[$c1]\t$c2\t$seqsc[$c2]\n";
			print FO"\t$seqsc[$c2+1]\t$seqsc[$c2-1]\t";
		}
	}
	print FO"\n";
}
close FO;
print "Genome written into file $fg , Connectivity into $fo\n";
