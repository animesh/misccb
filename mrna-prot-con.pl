use strict;

my $f1 = shift @ARGV;
my $f2 = shift @ARGV;
my $f3 = shift @ARGV;
my $f4 = shift @ARGV;
my @tmp;
my @name;
my %pg1;
my %pg2;
my %pg3;
my %pg4;
my %nc;

open (F1, $f1) || die "can't open \"$f1\": $!";
while (my $line = <F1>) {
	chomp $line;
	$line =~ s/\r//g;
	@tmp=split(/,/,$line);
        if ($tmp[49] =~ /[0-9]/){
 		@name=split(/\;/,$tmp[14]);
 		foreach (@name) { $pg1{$_}="$tmp[49]"; $nc{$_}++;	
		#print "$name[$_]\t$tmp[48]\n";
		}
    }
}
close F1;


open (F2, $f2) || die "can't open \"$f2\": $!";
while (my $line = <F2>) {
	chomp $line;
	$line =~ s/\r//g;
	@tmp=split(/,/,$line);
        if ($tmp[20]  =~ /[0-9]/){
 		@name=split(/\;/,$tmp[0]);
 		foreach (@name) { $pg2{$_}="$tmp[20]"; $nc{$_}++; 	
		#print "$_:$pg2{$_}.=$tmp[20]\n";
		}
        }
}
close F2;


open (F3, $f3) || die "can't open \"$f3\": $!";
while (my $line = <F3>) {
	chomp $line;
	$line =~ s/\r//g;
	@tmp=split(/,/,$line);
        if ($tmp[8] =~ /[0-9]/){
 		@name=split(/\;/,$tmp[0]);
 		foreach (@name) { $pg3{$_}="$tmp[8]"; $nc{$_}++; 	
		#print "$_\t$tmp[8]\n";
		}
     }
}
close F3;

open (F4, $f4) || die "can't open \"$f4\": $!";
while (my $line = <F4>) {
	chomp $line;
	$line =~ s/\r//g;
	@tmp=split(/,/,$line);
        if ($tmp[4]  =~ /[0-9]/){
 		@name=split(/\;/,$tmp[0]);
 		foreach (@name) { $pg4{$_}="$tmp[4]"; $nc{$_}++;	
		#print "$_\t$tmp[4]\n";
		}
    }
}
close F4;


print "Uniprot ID,#Detected,$f1,$f2,$f3,$f4\n";
foreach my $ncc (keys %nc){
		if($pg1{$ncc} and $pg2{$ncc} and $pg3{$ncc} and $pg4{$ncc}){
			print "$ncc,$nc{$ncc},$pg1{$ncc},$pg2{$ncc},$pg3{$ncc},$pg4{$ncc}\n";
		}
}

__END__

perl mrna-prot-con.pl /cygdrive/c/Users/animeshs/SkyDrive/kamerge/MaxQuantOld.csv /cygdrive/c/Users/animeshs/SkyDrive/kamerge/MaxQuantNew.csv /cygdrive/c/Users/animeshs/SkyDrive/kamerge/1SILAC_LR5_8226_20130416.csv /cygdrive/c/Users/animeshs/SkyDrive/kamerge/list_updown_mrna5.csv > /cygdrive/c/Users/animeshs/SkyDrive/kamerge/merge.csv

