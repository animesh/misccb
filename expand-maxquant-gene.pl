use strict;
use Text::ParseWords;
use Scalar::Util qw(looks_like_number);
my %vh;
my %nh;
my %ch;
my $gn=7;
my $cnt;
my $cntt;

my $f1 = shift @ARGV;
open (F1, $f1) || die "can't open \"$f1\": $!";
my $lc;
while (my $line = <F1>) {
	$lc++;
	$line =~ s/\r//g;
	chomp $line;
	if($lc==1){$vh{"header"}="$line";}
	else{
		$line =~ s/\`|\"|\'/ /g;
		my @tmp=parse_line('\t',0,$line);
		my @tmpp=split(/\||;|\s+/,$tmp[$gn]);
		for($cntt=0;$cntt<=$#tmpp;$cntt++){
			if($tmpp[$cntt] =~ m/(\w+_\w+)/ and $tmpp[$cntt] !~ /\:/){
				my ($name)=$tmpp[$cntt];
				$nh{$name}++;
				for($cnt=0;$cnt<=$#tmp;$cnt++) {
					$tmp[$cnt] =~ s/^\s+|\s+$//;
					if (looks_like_number($tmp[$cnt])){
						if($vh{"$name-$cnt"}<$tmp[$cnt]){
							$vh{"$name-$cnt"}=$tmp[$cnt];
						}
					}
					else{
						$tmp[$cnt]=~s/,/-/g;
						$vh{"$name-$cnt"}.="$tmp[$cnt] ";
					}
				}
			}
		}
	}
}
close F1;

$lc=0;
foreach my $ncc (keys %nh){
	if($nh{$ncc}>0){
		$lc++;
		my $name=$ncc;
		$name=~s/\_[A-Za-z]+//g;
		$vh{"header"}=~s/\t/\,/g;
		if($lc==1){print "Gene,FullID,",$vh{"header"},",Count,Number\n";}
		print "$name,$ncc,";
		for(my $c=0;$c<$cnt;$c++){
			#my $name="$ncc-$ARGV[$c]";
			print $vh{"$ncc-$c"},",";
		}
		print "$nh{$ncc},$lc\n";
	}
}

__END__

$ perl remove-duplicate-gene.pl /cygdrive/x/Qexactive/Berit_Sissel/B005/MCR22ProteinsB5.csv | sed 's/\,/ /g' | awk '{print $3}' | sort | uniq -c
   4105 149
    101 298

