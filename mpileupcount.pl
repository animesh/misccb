use strict;
use warnings;
use Text::ParseWords;

my @files=<*mpileup.txt>;
my %id;
my %idc;
my %ids;
my %cc;
my $cnt;

print "Seq-Pos-Nuc\t";
for($cnt=0;$cnt<=$#files;$cnt++){
    my $f1=$files[$cnt];
    print "$f1\t";
    my @tmp;
    my @name;
    my %pg;
    my $lcnt;
    open (F1, $f1) || die "can't open \"$f1\": $!";
    while (my $line = <F1>) {
        chomp $line;
        $line =~ s/\r|\`|\"|\'/ /g;
        $lcnt++;
    	@tmp=parse_line('\t',0,$line);
    	my $idi="$f1-$tmp[0]-$tmp[1]-$tmp[2]";
	$id{$idi}.="$tmp[3]";
	$idc{$idi}++;
	$cc{"$tmp[0]-$tmp[1]-$tmp[2]"}+=$tmp[3];
	$ids{"$tmp[0]-$tmp[1]-$tmp[2]"}++;
    }
    close F1;
}
print "Depth\tCount\n";


foreach my $g  (keys %ids){
	print "$g\t";
	for($cnt=0;$cnt<=$#files;$cnt++){
		my $f1="$files[$cnt]-$g";
		print "$id{$f1}\t";
	}
	print "$cc{$g}\t$ids{$g}\n";
}


__END__

perl /home/animeshs/misccb/mpileupcount.pl
