use strict;
use Text::ParseWords;
open(F1,$ARGV[0]);
my %nm;
my %id;
my %md;
my $lv=1;
my $lc=0;
my $unid=4;

while(my $l=<F1>){
	$lc++;
	if($lc>$lv){
		$l =~ s/\r|\n|\'//g;
		my @tmp=parse_line(',',0,$l);
		my @tmp1=split(/\;/,$tmp[$unid]);
		for(my $c1=0;$c1<=$#tmp1;$c1++){
			if($tmp1[$c1]){
				$nm{$tmp1[$c1]}.="$tmp[0]\t";
				$md{$tmp[0]}.="$tmp[5]\t";
				$id{$tmp1[$c1]}++;
			}
		}
	}
	#else{$hdr{$lc}=$l}
}
close F1;

my %seqh;
my $seqc;
open(F2,$ARGV[1]);
while(my $l1=<F2>){
	chomp $l1;
        $l1=~s/\r//g;
        if($l1=~/^>/){$seqc=$l1;}
        else{$l1=~s/[0-9]|\s+//g;$seqh{$seqc}.=uc($l1);}
}
close F2;

foreach (keys %seqh){
        my $seqn=$_;
        my @t=split(/\|/,$seqn);
        my @t1=split(/\t/,$nm{$t[1]});
        my $seq=$seqh{$_};
        foreach my $pep (@t1){
        #if($md{$nm{$t[1]}}){
        	print "$t[1]\t$nm{$t[1]}\t$id{$t[1]}\t$md{$pep}\n";
        #}
        }
}


__END__

foreach (keys %hdr){
	print "HDR-$_,Ratio [$s2/$s1],FoundIn,$hdr{$_}";
}



__END__

perl pepcomp.pl /cygdrive/x/Elite/LARS/2013/oktober/TNRpeps.csv | sort | uniq > list.txt
