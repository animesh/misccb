use strict;
use Text::ParseWords;

my $path = shift @ARGV;
my $pat = shift @ARGV;
my $i1 = shift @ARGV;
my @files=<$path/*$pat>;
my %mrna;
my %nc;

print "FileColumn$i1\t";
foreach my $f1 (@files){
    my @tmp;
    my @name;
    my %pg;
    my $lcnt;
    my $fn=$f1;
    $fn=~s/$path|$pat|\///g;
    print "$fn\t";
    open (F1, $f1) || die "can't open \"$f1\": $!";
    while (my $line = <F1>) {
	$lcnt++;
	if($pat=~/csv/){@tmp=parse_line(',',0,$line);}
	if($pat=~/txt/){@tmp=parse_line('\t',0,$line);}
        if ($lcnt>1){
	    @name=split(/\;/,$tmp[0]);
	    foreach (@name) {
		my $key=$_.$f1;
		if($tmp[$i1]=~/[0-9]/){my $htl=$tmp[$i1]/($tmp[$i1]+1);$mrna{$key}.="$htl ";}
#		if($tmp[$i1]=~/[0-9]/){my $htl=$tmp[$i1]/($tmp[$i1]+1);$mrna{$key}.="$tmp[$i1] ";}
		elsif($tmp[$i1] eq ""){$mrna{$key}.="NA ";}
		else{$mrna{$key}.="$tmp[$i1] ";} 		
		$nc{$_}++;
	    }
        }
    }
    close F1;
}
print "\n";

foreach my $g  (keys %nc){
    print "$g\t";
    foreach  my $f (@files){
	my $key=$g.$f;
	print "$mrna{$key}\t";
    }
    print "\n";
}


__END__

perl mqpevol.pl /cygdrive/m/RAW/AidaX MQ.csv 19 

perl mqpevol.pl /cygdrive/m/RAW/AidaX/IH1_121125_AIDA/txt proteinGroups.txt 19
