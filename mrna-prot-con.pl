use strict;

my $f1 = shift @ARGV;
my $f2 = shift @ARGV;
my @tmp;
my @name;
my %pg;
my %mrna;

open (F1, $f1) || die "can't open \"$f1\": $!";
while (my $line = <F1>) {
	@tmp=split(/\t/,$line);
        if ($tmp[0] =~ /^[0-9]/){
 		@name=split(/\;/,$tmp[12]);
 		foreach (@name) { $pg{$_}=$tmp[48];} 	
		#print "$name[1]\t$tmp[48]\n";
        }
}
close F1;

open (F2, $f2) || die "can't open \"$f2\": $!";
while (my $line = <F2>) {
	$line=~s/\"//g;
        if ($line =~ /^[0-9]/){
                @tmp=split(/\|/,$line);
                $mrna{$tmp[1]}=$tmp[9];
		#print "$tmp[1]\t$tmp[9]\n";
        }
}
close F2;

foreach my $pgn (keys %pg){
	foreach my $mn (keys %mrna){
		if($pgn eq $mn){
			print "$pgn,$mn,$pg{$pgn},$mrna{$mn}\n"
		}
	}
}

__END__
for(my $cnt=0;$cnt<=$#tmp;$cnt++){
            my $val=$tmp[$cnt];
            #print "$val\n";
}       


