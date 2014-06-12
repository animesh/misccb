use strict;
use warnings;
use Text::ParseWords;

my $fpat = shift @ARGV;

my $idi = shift @ARGV;
my $i1 = shift @ARGV;

my @files=<*$fpat>;
my %mrna;
my %nc;


print "ID-$idi-Feature-$i1\t";
foreach my $f1 (@files){
    my @tmp;
    my @name;
    my %pg;
    my $lcnt;
    my @fn=split(/\./,$f1);
    print "$fn[0]\t";
    open (F1, $f1) || die "can't open \"$f1\": $!";
    while (my $line = <F1>) {
        chomp $line;
        $line =~ s/\r|\`|\"|\'/ /g;
        $lcnt++;
    	@tmp=parse_line('\s+',0,$line);
	my $key="$fn[0]_$tmp[$idi]";
	if($tmp[$idi]){$mrna{$key}.="+$tmp[$i1]";}
	$nc{$tmp[$idi]}++;
	#print "$lcnt\t$key\t$mrna{$key}\t$i1-$tmp[$i1]\t$idi-$tmp[$idi]\t$nc{$idi}\n";
    }
    close F1;
}
print "TotalDetect\n";

foreach my $g  (keys %nc){
        print "$g\t";
        foreach  my $f (@files){
		my @fn=split(/\./,$f);
        	my $key="$fn[0]_$g";
        	print "$mrna{$key}\t";
        }
        print "$nc{$g}\n";
}


__END__

perl /home/animeshs/misccb/filecombo.pl  cnt.txt 2 1 > /home/animeshs/misccb/test.txt
