use strict;
use Text::ParseWords;

my $path = shift @ARGV;
my $pat = shift @ARGV;
my $i1 = shift @ARGV;
my $i2 = shift @ARGV;

my @files=<$path/*$pat>;
my %mrna;
my %nc;

print "FileColumn$i2\t";
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
            @name=split(/\;/,$tmp[$i1]);
            foreach (@name) {
                my $key="$_;$f1";
                if($tmp[$i2]=~/[0-9]/){my $htl="$tmp[$i2]";$mrna{$key}.="$htl ";}
                elsif($tmp[$i2] eq ""){$mrna{$key}.="NA ";}
                else{$mrna{$key}.="$tmp[$i2] ";}                
                $nc{"$_"}++;
            }
        }
    }
    close F1;
}
print "ExperimentsGeneDetectedIn\n";

foreach my $g  (keys %nc){
    my $ocg;
    print "$g\t";
    foreach  my $f (@files){
        my $key="$g;$f";
        print "$mrna{$key}\t";
        if($mrna{$key}){$ocg++;}
    }
    print "$ocg\n";
}

__END__	

perl pdpevol.pl /cygdrive/X/Qexactive Area_pep.csv 1 8
