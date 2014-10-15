use strict;
use Text::ParseWords;
my %vh;
my %nh;
my $cntt;

my $f1 = shift @ARGV;
my $id = shift @ARGV;
my $cat = shift @ARGV;
if(!$cat){die "can't open \"$f1\": $! usage:perl category-counting.pl file-name id-column-number category--column-number";}
open(F1,$f1);
while (my $line = <F1>) {
	$line =~ s/\r//g;
	chomp $line;
		my @tmp=parse_line('\t',0,$line);
		$tmp[$cat]=~s/\s+//g;
		$tmp[$id]=~s/\s+//g;
		if($tmp[$cat] eq ""){
					$nh{"NA"}++;
					$vh{"NA"}.="$tmp[$id];";
		}
		else{
			my @tmpp=split(/\;/,$tmp[$cat]);
			for($cntt=0;$cntt<=$#tmpp;$cntt++){
					my ($name)=uc($tmpp[$cntt]);
					$nh{$name}++;
					$vh{$name}.="$tmp[$id];";
			}
		}
	#print "@tmp[0],@tmp[9],@tmpp[0]\n";
}
close F1;

print "Category\tID(s)\tCount\n";
foreach my $ncc (keys %nh){
		print "$ncc\t$vh{$ncc}\t$nh{$ncc}\n";
}

__END__

perl category-counting.pl /cygdrive/l/Qexactive/Linda/uniprot-tw283.txt 0 9 > t.txt
 
 