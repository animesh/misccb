while(<>){
	chomp;
	push(@t,$_);
}
for($c=0;$c<=$#t;$c++){
	@t2=split(/\s+/,@t[$c]);
	if(@t2[-2]==1){
		print "@t[$c-3]\n@t[$c-2]\n@t[$c-1]\n@t[$c]\n";
		#print "@t2[-2]\n";
	}
}
	
