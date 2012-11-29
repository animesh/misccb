use strict;

my $f1 = shift @ARGV;
my $f2 = shift @ARGV;
my @tmp;
my @name;
my %pg;
my %mrna;
my %nc;

open (F1, $f1) || die "can't open \"$f1\": $!";
while (my $line = <F1>) {
	@tmp=split(/\t/,$line);
        if ($tmp[19]){
 		@name=split(/\;/,$tmp[0]);
 		foreach (@name) { $pg{$_}.="$tmp[19]|"; $nc{$_}++;} 	
		#print "$name[1]\t$tmp[19]\n";
        }
}
close F1;

open (F2, $f2) || die "can't open \"$f2\": $!";
while (my $line = <F2>) {
	$line=~s/\"//g;
        @tmp=split(/\,/,$line);
        if ($tmp[0]){
 		@name=split(/\;/,$tmp[0]);
 		foreach (@name) { $mrna{$_}.="$tmp[9]|"; $nc{$_}++;} 	
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
			print "MATCH\t$pgn,$nc{$mn},$pg{$pgn},$mrna{$mn}\n";
			$cpgn{$pgn}++;
			#if($nc{$mn}!=2){print "$nc{$mn}\t$nc{$mn}\n";}
		}

    }
    if(!$cpgn{$pgn}){print "MQ\t$pgn,$pg{$pgn}\n"}
}

foreach  (keys %mrna){
    if(!$cpgn{$_}){print "PD\t$_,$mrna{$_}\n"}
}


print "MQ\t$cp\tPD\t$cm\n";


__END__
# perl mrna-prot-con.pl proteinGroups.txt  mrna5.csv

sed 's/,/ /g' /cygdrive/c/Users/animeshs/AidaX/VOLIN_121125_AIDA.csv | awk '{print $1}' | wc
 awk '{print $1}' /cygdrive/c/Users/animeshs/AidaX/volin/txt/proteinGroups.txt | sed 's/;/ /g' | wc
