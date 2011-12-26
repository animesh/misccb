#!/usr/bin/perl

system("ls -1 *.gbk > tempfile1");
open(FT1,"tempfile1");
open(FT,">temp.txt");

my $cnt=0;
while(my $tfl1=<FT1>){
	$cnt++;
	chomp $tfl1;
	#print "Converting $tfl1...\t";
	$file=$tfl1;
	$tfl1=~s/AANW01|\.|gbk|[A-Z]//g;
    if(($tfl1+0)!=$cnt){
		print "$file\t$cnt\t$tfl1\n";
    }
}
close FT1;



