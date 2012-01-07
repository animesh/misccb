	system("export CLASSPATH=/work/ash022");
	$file="selcomp.arff";
	print "Processing file $file\n"; 
        system("java -Xmx1500m  weka.classifiers.functions.MultilayerPerceptron -t $file  -x 10 > $file.nn.10f.txt ");

