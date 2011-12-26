open(F,"class_all.txt");
open(FO,">class_all_pl.csv");

print FO"MeanF1,MeanF2,MeanF3,MeanF4,MeanF5,MeanF6,MeanF7,MeanF8,StdDev1,StdDev2,StdDev3,StdDev4,StdDev5,StdDev6,StdDev7,StdDev8,Other1,Other2,Other3,Other4,Other5,Other6,Other7,Other8,Output\n";

@out_vec=qw/14 9 7 11 1/;
	$outp=0;

while($l=<F>){
	@t=split(/\s+/,$l);
	for($c=1;$c<$#t;$c++){
		$out=@t[$c]+0;
		if($c!=25){
			print FO"$out,";
		}
	}
	$out=@out_vec[$outp]+0;
	print FO"$out\n";
	$outp++;
}

#system("java weka.core.converters.CSVLoader class_all_pl.csv > class_all_pl.arff");
system("java weka.classifiers.functions.LinearRegression -t class_all_pl.arff -x 5");
