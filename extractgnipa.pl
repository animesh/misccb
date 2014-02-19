use strict;
use Text::ParseWords;
my $id=0;
my %bh;
my %nh;
my %bhn;

my $f1 = shift;
my $bp = shift;
my $gn = shift;

open (F1, $f1) || die "can't open \"$f1\": $!";
my $lc;
while (my $line = <F1>) {
	$lc++;
	$line =~ s/\r//g;
	chomp $line;
	my @tmp1=parse_line('\t',0,$line);
	my @tmp2=parse_line(',',0,$tmp1[$gn]);
	$bhn{$tmp1[$bp]}++;
	for(my $c=0;$c<=$#tmp2;$c++){
		$tmp2[$c] =~ s/\s+//g;
		if ($tmp2[$c] ne ""){
			$nh{$tmp2[$c]}++;
			$bh{$tmp1[$bp]}.="$tmp2[$c]\n";
		}
	}
}
close F1;


foreach my $ncc (keys %bh){
	my $fon=$ncc;
	$fon=~s/\s+|\-|\,|\'//g;
	my $fon="$f1.$fon.gn.txt";
	open (FO, ">$fon") || die "can't open \"$fon\": $!";
	print "$ncc,$bhn{$ncc}\n";
	print FO"$bh{$ncc}";
	close FO;
}

__END__

perl extractgnipa.pl /cygdrive/l/Qexactive/Berit_Sissel/B005/Bodil/stim-all-S1.txt 0 4

