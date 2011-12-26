#!/usr/bin/perl

system("ls -1 *.1.asn1 > tempfile1");
open(FT1,"tempfile1");
open(FT,">temp.txt");

my $cnt=0;
while(my $tfl1=<FT1>){
	$cnt++;
	chomp $tfl1;
	print "Converting $tfl1...\t";
    if($tfl1=~/asn1$/){
	my @temp=split(/\./,$tfl1);
	my $foo=@temp[0].".gb";
	system("asn2gb.Win32 -i $tfl1 -o $foo -f b");
    }
	print "Done\n";
}
close FT1;



