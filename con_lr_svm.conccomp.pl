push(@files,"P1.v21764.data.txt.P1.t360.class.txt.conc.ru.arff");
system("export CLASSPATH=/work/ash022");
foreach $file (@files) {
	$c++;
	print "Processing file # $c $file\n"; 
        system("java -Xmx3000m  weka.classifiers.meta.ClassificationViaRegression -t $file -x 10 > $file.class.arff.10fold.lr.txt ");
        system("java -Xmx3000m  weka.classifiers.functions.SMO -t $file -x 10 > $file.class.arff.10fold.svm.txt ");
}

__END__
