#>FTL1_1_1_0_507
$file=shift @ARGV;chomp $file;open(F,$file);
while(<F>){
	chomp;
	if($_=~/^>/){
		$_=~s/\>//g;
		@tmp=split(/\s+/,$_);
		$n=@tmp[0];
		$ncom{$n}=$_;
		$name=$n;
		$name=~s/R//g;
		$name=~s/F//g;
		$p{$name}++;
	}
	else{$seq{$ncom{$n}}=$_;}
}
open(FO1,">$file.1.fna");
open(FO2,">$file.2.fna");
open(FO3,">$file.3.fna");
open(FO4,">$file.4.fna");
open(FO5,">$file.5.fna");
open(FO6,">$file.6.fna");
open(FO7,">$file.7.fna");
open(FO8,">$file.8.fna");
open(FO9,">$file.9.fna");
open(FO10,">$file.10.fna");
open(FO,">$file.rest.fna");
foreach $v (keys %p){
	$rname="R$v";
	$fname="F$v";
	$snf=$ncom{$fname};
        $snr=$ncom{$rname};
	if($p{$v}==2){
		$cnt++;
                if($cnt%10==1){
                        print FO1">$snf\n$seq{$snf}\n";
                        print FO1">$snr\n$seq{$snr}\n";
                } 
                if($cnt%10==2){
                        print FO2">$snf\n$seq{$snf}\n";
                        print FO2">$snr\n$seq{$snr}\n";
                } 
                if($cnt%10==3){
                        print FO3">$snf\n$seq{$snf}\n";
                        print FO3">$snr\n$seq{$snr}\n";
                } 
                if($cnt%10==4){
                        print FO4">$snf\n$seq{$snf}\n";
                        print FO4">$snr\n$seq{$snr}\n";
                }
                if($cnt%10==5){
                        print FO5">$snf\n$seq{$snf}\n";
                        print FO5">$snr\n$seq{$snr}\n";
                } 
                if($cnt%10==6){
                        print FO6">$snf\n$seq{$snf}\n";
                        print FO6">$snr\n$seq{$snr}\n";
                } 
                if($cnt%10==7){
                        print FO7">$snf\n$seq{$snf}\n";
                        print FO7">$snr\n$seq{$snr}\n";
                } 
                if($cnt%10==8){
                        print FO8">$snf\n$seq{$snf}\n";
                        print FO8">$snr\n$seq{$snr}\n";
                }
                if($cnt%10==9){
                        print FO9">$snf\n$seq{$snf}\n";
                        print FO9">$snr\n$seq{$snr}\n";
                } 
                if($cnt%10==0){
                        print FO10">$snf\n$seq{$snf}\n";
                        print FO10">$snr\n$seq{$snr}\n";
                } 
	}
	else{
		if($seq{$snf} ne ""){print FO">$ncom{$fname}\n$seq{$snf}\n";}
		else{print FO">$ncom{$rname}\n$seq{$snr}\n";}
	}
}
