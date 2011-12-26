#!/usr/bin/perl
$file=shift@ARGV;
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
foreach $t1 (sort keys %name1)
	{
	print "$name1{$t1}\t$t1\t$name2{$t1}\t$name3{$t1}\n";
	}
