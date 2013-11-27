use strict;
use Text::ParseWords;
my %vh;
my %nh;
my %ch;

sub createhash{
	my $f1 = shift;
	open (F1, $f1) || die "can't open \"$f1\": $!";
	my $lc;
	while (my $line = <F1>) {
		$lc++;
		$line =~ s/\r//g;
		chomp $line;
		if($lc==1){$vh{$f1}="$line";}
		else{
			$line =~ s/\'/-/g;
			my @tmp=parse_line(',',0,$line);
			if ($tmp[0] ne ""){
				my @name=split(/\;/,$tmp[0]);
				foreach (@name) {
					$nh{$_}++;$ch{"$_-$f1"}++;
					if(abs($tmp[1])>0){
						if($tmp[1]>0 and $tmp[1]<1){$vh{"$_-$f1"}=-1/$tmp[1];}
						else{$vh{"$_-$f1"}=$tmp[1];}
					}
				}
			}
		}
	}
	close F1;
	return $f1;
}

for(my $c=0;$c<=$#ARGV;$c++){
	my $fn=createhash($ARGV[$c]);
	print "Processed file,$fn,$ARGV[$c]\n";
}


my $lc;
foreach my $ncc (keys %nh){
	$lc++;
	print "$lc,$ncc,";
	for(my $c=0;$c<=$#ARGV;$c++){
		my $name="$ncc-$ARGV[$c]";
		print "$vh{$name},$ch{$name},";
	}
	print "$nh{$ncc}\n";

}

__END__

perl mrna-prot-con.pl /cygdrive/c/Users/animeshs/SkyDrive/kamerge/MaxQuantOld.csv /cygdrive/c/Users/animeshs/SkyDrive/kamerge/MaxQuantNew.csv /cygdrive/c/Users/animeshs/SkyDrive/kamerge/1SILAC_LR5_8226_20130416.csv /cygdrive/c/Users/animeshs/SkyDrive/kamerge/list_updown_mrna5.csv /cygdrive/c/Users/animeshs/SkyDrive/kamerge/2013070370GQJPMXGF.fasta > /cygdrive/c/Users/animeshs/SkyDrive/kamerge/merge.csv
