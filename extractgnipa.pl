use strict;
use Text::ParseWords;
my %bh;
my %bhn;

my $f1 = shift;
my $bp = shift;
my $gn = shift;

open (F1, $f1) || die "can't open \"$f1\": $!";
my $lc;
while (my $line = <F1>) {
	$lc++;
	$line =~ s/\r//g;
	$line =~ s/\'//g;
	chomp $line;
	my @tmp1=parse_line('\t',0,$line);
	my @tmp2=split(/\,/,$tmp1[$gn]);
	$bhn{$tmp1[$bp]}++;
	for(my $c=0;$c<=$#tmp2;$c++){
		$tmp2[$c] =~ s/\s+//g;
		#if ($tmp2[$c]=~/^[0-9]/){
		if ($tmp2[$c] ne ""){
			$bh{$tmp1[$bp]}.="$tmp2[$c]\n";
		}
	}
}
close F1;


foreach my $ncc (keys %bh){
	my $fon=$ncc;
	$fon=~s/\s+|\-|\,|\'//g;
	my $fon="$f1.$fon.$bp.$gn.csv";
	open (FO, ">$fon") || die "can't open \"$fon\": $!";
	print "$ncc,$bhn{$ncc}\n";
	print FO"$bh{$ncc}";
	close FO;
}

__END__

cd /cygdrive/l/Qexactive/Berit_Sissel/B005/
for j in /cygdrive/l/Qexactive/Berit_Sissel/B005/Bodil/*-all-*.txt ; do echo $j ; perl extractgnipa.pl $j 0 4;   done
for i in /cygdrive/l/Qexactive/Berit_Sissel/B005/Bodil/*-all-*.0.4.csv; do echo $i; sort $i | uniq -ic | awk '{print $2","$1}' > $i.sort ; done
for j in *sort ; do for i in *cutoff1c.csv ; do echo $i $j ; perl /cygdrive/c/Users/animeshs/misccb/matchlist.pl $j $i > $j.$i.ml.csv ; done ; done
