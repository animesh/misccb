@files1 = <IC_[0-9].txt.ru.class.arff.lrclass.txt>;
@files2 = <IC_[0-9][0-9].txt.ru.class.arff.lrclass.txt>;
@files=(@files1,@files2);
for($c=0;$c<=$#files;$c++) {
        $file=@files[$c];
        #print "Processing file # $c $file,"; 
	@fname=split(/\./,$file);
	print "@fname[0],";
	open(F,$file);
	while(<F>){
		chomp;
		@t1=split(/\s+/);
		for($cc=0;$cc<=$#t1;$cc++) {
			if(@t1[$cc]=~/^FC/){
				push(@tall,@t1[$cc]);
			}
		}
	}
	close F;
	@utall = grep !$seen{$_}++, @tall;
	print join(",",@utall),"\n";
	undef @tall;
	undef @utall;
}
