push(@files,"IC_FC_toML.csv");
system("export CLASSPATH=/work/ash022");
foreach $file (@files) {
	$c++;
	print "Processing file # $c $file\n"; 
	system("java weka.core.converters.CSVLoader $file > $file.class.arff");
        system("java -Xmx3000m  weka.classifiers.meta.ClassificationViaRegression -t $file.class.arff -x 10 > $file.class.arff.10fold.lr.txt ");
        system("java -Xmx3000m  weka.classifiers.functions.SMO -t $file.class.arff -x 10 > $file.class.arff.10fold.svm.txt ");
	system("java -Xmx3000m  weka.classifiers.meta.ClassificationViaRegression -t $file.class.arff -x 72 > $file.class.arff.72fold.lr.txt ");
        system("java -Xmx3000m  weka.classifiers.functions.SMO -t $file.class.arff -x 72 > $file.class.arff.72fold.svm.txt ");
}

__END__
