use strict;
use warnings;
my $file=shift @ARGV;
open F, "$file" or die "Can't open $file : $!";

while(my $l=<F>){
	chomp $l;
        $l=~s/\r//g;
        my @t=split(/,/,$l);
        print $t[0];
}


__END__

perl genloc.pl testmod.csv

email: sharma.animesh@gmail.com
