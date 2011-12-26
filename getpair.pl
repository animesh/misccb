#!/usr/bin/perl
# getorder.pl     sharma.animesh@gmail.com     2009/03/09 10:01:28
#>codbac-190o01.fb140_b1.SCF length=577 sp3=clipped
$file=shift @ARGV;
chomp $file;
$filec=$file.".ca.fna";
$filem=$file.".m.fna";
open(F,$file);
open(FC,">$filec");
open(FM,">$filem");
while(<F>){
        if($_=~/^>/){
        $cnt++;
    my @tmp=split(/\-|\./,$_);
    my $namestr=$tmp[1].".".substr($tmp[2],0,1);
    my $namesubstr=($tmp[1]);
    $hitname{$namesubstr}++;
        $hitpos{$namestr}++;
        #print "$namestr\t$namesubstr\t$hitpos{$namestr}\n";    
        print FC">$namestr\n";    
        }
	else{print FC"$_";}
}

foreach my $w (keys %hitname) {
         my $rname=$w.".r";
         my $fname=$w.".f";
	#if($hitname{$w}>2){print "$w\t$hitpos{$fname}\t$hitpos{$rname}\t$hitname{$w}\n";}
        $cnt2++;
                if($hitpos{$rname} and $hitpos{$fname}){
                        $cseq++;
                        print "$w\t$fname\t$rname\t-\t$hitpos{$fname}\t$hitpos{$rname}\n";
                        print FM"$rname\t$fname\n";
                }
}

print "$cseq Total Pair from $cnt2 Elements in $cnt strings\n";

__END__

getpair.pl (END)

