#!/usr/bin/perl
$file=shift@ARGV;
$file2=shift@ARGV;
open F,$file;
while($line=<F>)
{
if($line =~ /^ELSE1/ )
	{next;}
else
	{
	#print $line;
	chomp$line;
	@temp=split(/\t/,$line);
	#print @temp;
	$name1{@temp[1]}=@temp[0];
	$name2{@temp[1]}=@temp[2];
	$name3{@temp[1]}=@temp[3];
	}
}
#open(FO,">temp");
foreach $t1 (sort keys %name1)
	{
		if($name3{$t1} < 1)
		{
		$temp1=$name1{$t1};
		@temp2=split(/,/,$temp1);
		$t3=@temp2[1];
		$t3=~s/\s+//g;
		push(@temp4,$t3);
		#print "$t3\t";
		}
	}
#close FO;
open(FF,$file2);
while($ll=<FF>)
{
chomp $ll;
@temp5=split(/\t/,$ll);1/1;
$tt4=@temp5[0];
$tt4=~s/\s+//g;
$tt5=@temp5[13];
#print "$tt4\t$tt5\n";
foreach $tt2 (@temp4)
	{
	if($tt4 eq $tt2 )
		{
		print "$tt2\t$tt4\t$tt5\n";
		}
	}
}


