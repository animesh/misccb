use strict;

my $f1 = shift @ARGV;
my $f2 = shift @ARGV;
my @tmp;
my @name;
my %pg;
my %mrna;
my %nc;
my $lcnt;


open (F1, $f1) || die "can't open \"$f1\": $!";
while (my $line = <F1>) {
    $lcnt++;
	@tmp=split(/\t/,$line);
        if ($lcnt>1 && $tmp[19]){
 		@name=split(/\;/,$tmp[0]);
 		foreach (@name) { if(!/^(REV|CON)/){$pg{$_}="$tmp[19]"; $nc{$_}++;}} 	
		#print "$name[1]\t$tmp[19]\n";
        }
}
close F1;
$lcnt=0;

open (F2, $f2) || die "can't open \"$f2\": $!";
while (my $line = <F2>) {
    $lcnt++;
	$line=~s/\"//g;
        @tmp=split(/\,/,$line);
        if ($lcnt>1 && $tmp[0]){
 		@name=split(/\;/,$tmp[0]);
 		foreach (@name) { $mrna{$_}="$tmp[9]"; $nc{$_}++;} 	
		#print "$tmp[0]\t$tmp[9]\n";
        }
}
close F2;

my %cpgn;
my $cp;
my $cm;

foreach my $pgn (keys %pg){
    $cp++;
    $cm=0;
    foreach my $mn (keys %mrna){
	    $cm++;
		if($pgn eq $mn){
		    my $ratios;
		    if($mrna{$mn}){$ratios=$pg{$pgn}/$mrna{$mn};}
			print "MATCH\t$pgn,$nc{$mn}\t$pg{$pgn}\t$mrna{$mn}\t$ratios\n";
			$cpgn{$pgn}++;
			#if($nc{$mn}!=2){print "$nc{$mn}\t$nc{$mn}\n";}
		}

    }
    if(!$cpgn{$pgn}){print "MQ\t$pgn\t$nc{$pgn}\t$pg{$pgn}\n"}
}

foreach  (keys %mrna){
    if(!$cpgn{$_}){print "PD\t$_$nc{$_}\t\t$mrna{$_}\n"}
}


print "TOTAL\tMQ\t$cp\tPD\t$cm\n";


__END__

perl pdmqcomp.pl /cygdrive/m/RAW/AidaX/volin/txt/proteinGroups.txt /cygdrive/m/RAW/AidaX/VOLIN_121125_AIDA_PD.csv | grep "^MATCH"

sed 's/,/ /g' /cygdrive/m/RAW/AidaX/VOLIN_121125_AIDA_PD.csv | awk '{print $1}' | wc

awk '{print $1}' /cygdrive/m/RAW/AidaX/volin/txt/proteinGroups.txt | sed 's/;/ /g' | wc


