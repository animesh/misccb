#!/usr/bin/perl
if((@ARGV)!=1){die "USAGE: progname_name OFS-status-file\n";}
$f=shift @ARGV;
open(F,$f);
while($l=<F>){
	chomp $l;
	$l=~s/^\s+//g;
	$l=~s/\s+/\t/g;
	@t1=split(/\t/,$l);
	if((@t1)==6){
		$iter{(@t1[0]+0)}=@t1[1]+0;
		$iterval=(@t1[0]+0);
		#print "@t1[0]=>$iter{@t1[0]}=>\t$l\n";
	}
	if((@t1)==2){
		$gene{(@t1[0]+0)}=@t1[1]+0;
		#$matrixofs[(@t1[1]+0)][$iterval]=@t1[2]+0;
		#print "@t1[0]=>$gene{@t1[0]}=>\t$l\n";
	}
}
close F;

#foreach  $i1 (%iter) {
#	foreach $g1 (%gene) {
#		print "$matrixofs[$g1][$i1],";
#	}
#	print "\n";
#}

$foo=$f.".out";
open(FO,">$foo");
foreach $g1 (sort { $gene{$b} <=> $gene{$a}} keys %gene) {
	print FO"$g1\t$gene{$g1}\n";
}
close FO;
