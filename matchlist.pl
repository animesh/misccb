use warnings;
use strict;
open(F1,$ARGV[0]);
open(F2,$ARGV[1]);
my $mc=$ARGV[2];
my $oc=$ARGV[3];
if(!$ARGV[2]){$mc=0;}
if(!$ARGV[3]){$oc=$mc;}
my @list=<F1>;
my @gop=<F2>;
@list = sort { uc($a) cmp uc($b) } @list;
@gop = sort { uc($a) cmp uc($b) } @gop;
my $cnt=0;
my %match;
for(my $c1=0;$c1<=$#list;$c1++){
	my @tmp1=split(/,/,$list[$c1]);
	for(my $c2=$cnt;$c2<=$#gop;$c2++){
		my @tmp2=split(/,/,$gop[$c2]);
		#$tmp1[$mc]=~s/\s+|\r//g; #take twice the time
		$tmp1[$mc]=~s/\s+//g;
		$tmp2[$oc]=~s/\s+//g;
		if(uc($tmp1[$mc]) eq uc($tmp2[$oc])){
			chomp $list[$c1];
			chomp $gop[$c2];
			$match{$tmp1[$mc]}.="$tmp2[$oc], $list[$c1], $gop[$c2],";
 			delete $list[$c1]; # saves a sec
 			delete $gop[$c2];
 			$cnt=$c2+1;
 			last;
 		}
 		else{next;}
 		
 	}
}
foreach (keys %match){if($match{$_}){print "$_, $match{$_}\n"}}