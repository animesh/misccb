use strict;
use Text::ParseWords;
my $id=0;
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
			my @tmp=parse_line(',',0,$line);
			if ($tmp[$id] ne ""){
				$nh{$tmp[$id]}++;
				$ch{"$tmp[$id]-$f1"}++;
				$vh{"$tmp[$id]-$f1"}=$line;
			}
		}
	}
	close F1;
	return $f1;
}

for(my $c=0;$c<=$#ARGV;$c++){
	my $fn=createhash($ARGV[$c]);
}


my $lc;

print "ID,Total,";
for(my $c=0;$c<=$#ARGV;$c++){
	print "$vh{$ARGV[$c]},InFile,";
}
print "Total\n";

foreach my $ncc (keys %nh){
	$lc++;
	print "$ncc,$nh{$ncc},";
	for(my $c=0;$c<=$#ARGV;$c++){
		my $name="$ncc-$ARGV[$c]";
		print "$vh{$name},$ch{$name},";
	}
	print "$nh{$ncc}\n";
}

__END__

perl filecomb.pl /cygdrive/l/Tony/SILACvelosGN.csv /cygdrive/l/Tony/SILACqexGN.csv /cygdrive/l/Tony/IPgn.csv > /cygdrive/l/Tony/combo.csv

