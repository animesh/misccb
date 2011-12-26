$file1 = shift @ARGV;
open (F, $file1) || die "can't open \"$file1\": $!";
$seq="";
while ($line = <F>) {
	if ($line =~ /^>/){
		$c++;
		chomp $line;
		push(@seqname1,$line);	
		if ($seq ne ""){
			push(@seq1,$seq);
              		$seq = "";
            	}
      	}
	 else {$seq=$seq.$line;
      	}
}
push(@seq1,$seq);
close F;
$per=10;
@base=qw/A T G C/;
for($c1=0;$c1<=$#seq1;$c1++){
	$snc=$seqname1[$c1];
	@tmp=split(/\s+|\>/,$snc);
	if($c1==5||$c1==7){
		open(FT,">$file1.$c1.fna");
		$len=length($seq1[$c1]);
		print FT"$seqname1[$c1]\n$seq1[$c1]\n";
	}
}	
close FT;

