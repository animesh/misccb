open(F,"IC_1.txt");
open(FO,">IC_1.csv");

while($l=<F>){
	@t=split(/\s+/,$l);
	$line++;
	if($line==1){
			for($c=0;$c<$#t;$c++){
				$cp=$c+1;
				print FO"FC$cp,";
			}
			print FO"CLASS\n";
	}
	for($c=0;$c<$#t;$c++){
		$out=@t[$c]+0;
		print FO"$out,";
	}
	$out=@t[$c]+0;
	print FO"$out\n";
	print "Processes line $line with FCol $c\n";
}

#system("java weka.core.converters.CSVLoader class_all_pl.csv > class_all_pl.arff");
#system("java weka.classifiers.functions.LinearRegression -t class_all_pl.arff -x 5");
