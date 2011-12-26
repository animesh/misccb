$input=shift @ARGV;
chomp $input;
open(F,$input);
$fo1=$input."_conc.csv";
$fo2=$input."_class.csv";
print "writing to file $fo1 and $fo2\n";
open(FOTP,"> $fo1")or die $!;
open(FOTPC,">$fo2")or die $!;

while($lval=<F>){
		chomp $lval;
        @t=split(/,/,$lval);
        $line1++;
		$out="@t[0].txt";
        print "$line1\t$out\t@t\t";
		#if($line1==1){
			open_file($out,$lval);
		#}
}
close F;
foreach  (keys %filevalhash) {
	@temp=split(/\_/,$_);
	print FOTP "$_,$filevalhash{$_}\n";
	print FOTPC "CLASS,$fileclasshash{\"@temp[0]_@temp[1]\"}\n";
}

sub open_file {
	$file=shift;
	$extftrarr=shift;
    @tex=split(/,/,$extftrarr);
	open(FOF,$file);
	while($l=<FOF>){
		chomp $l;
		$l=~s/\t/ /g;
		$l=~s/\s+/ /g;
		@tlist=split(/\s+/,$l);
		#$check=1;
		for($c=0;$c<$#tlist;$c++){
			$out=@tlist[$c]+0;
			$cp=$c+1;
			$ftrname="FC$cp";
			$ftrval=$out;
			for($cv=1;$cv<=$#tex;$cv++){
				if(@tex[$cv] eq $ftrname){
					#print "$check\t$c\t$cv\t$ftrname\t$ftrval\t";
					$filevalhash{"@tex[0]_$ftrname"}.="$ftrval,";
					#$check=$cv+1;
				}
			}
		}
		$out=@tlist[$c]+0;
		$classname="C$out";
		$fileclasshash{"@tex[0]"}.="$classname,";
		#print "$classname\n";
	}
	close FOF;
    print "Select\t$extftrarr\tfrom file $file\n";
}
