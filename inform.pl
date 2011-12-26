#!/usr/bin/perl
#system("ls -1 vol.*.txt | wc > $size");
$size = `ls -1 vol.*.txt | wc`;
split(/\s+/,$size);$size=$_[1];
if($size<1){die"Size NULL\n";}
print "$size\n";
open(FO,">volall.out");
open(FOF,">ftr.out");

for($c=0;$c<$size;$c++){
	$cnt=$c+1;
	$file="vol.".$cnt.".txt";
	print "Processing $c\t$file\n"; 
	open(F,"$file");
	while($line=<F>){
		chomp $line;
		@tmp=split(/\,/,$line);
		$row++;
		$col=@tmp;
		print "Row $row has $col Columns @tmp[-1]\n";
		print FOF"Row $row has $col Columns @tmp[-1]\n";
		for($cc=0;$cc<($col-1);$cc++){
			print FO"@tmp[$cc]\t";
		}
		if(@tmp[-1]==1){print FO"1\t0\n";}
		elsif(@tmp[-1]==0){print FO"0\t1\n";}
	}
	$row=0;
}

