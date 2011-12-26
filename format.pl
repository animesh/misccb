while(<>){
	chomp;
	$c++;
	@t1=split(/\s+/);
	if($c%4==1){
		@t2=split(/\./,@t1[0]);
		print "@t2[0],";
	}
	print "@t1[1],@t1[2],";
	if($c%4==0){print "\n";}
}




__END__

 1612  grep -E "a = C0|b = C1" IC_?.svmbin.txt >> resultsvm.txt
 1613  grep -E "a = C0|b = C1" IC_*.svmbin.txt >> resultsvm.txt
 1614  less resultsvm.txt
 1615  ls *.pl
 1616  mkdir svmbin
 1617  mv *.svmbin.txt svmbin/.
 1618  cd svmbin/
 1619  l
 1620  ls
 1621  cp ../resultsvm.txt .
 1622  less resultsvm.txt
 1623  cp ../lr/*.pl .
 1624  ls *.pl
 1625  less format.pl
 1626  perl format.pl resultsvm.txt
 1627  perl format.pl resultsvm.txt > resultsvm.csv
 1628  pwd
 1629  less IC_65.txt.ru.class.arff.svmbin.txt
 1630  grep "Correctly Classified Instances" *.svmbin.txt
 1631  less IC_65.txt.ru.class.arff.svmbin.txt
 1632  qstat|grep ash022


 1266  md5sum IC_??.txt.ru.class.arff.lrclass.txt | awk '{print $1}' | sort | uniq
 1267  md5sum IC_??.txt.ru.class.arff.lrclass.txt | awk '{print $1}' | sort | uniq | wc
 1268  wc IC_??.txt.ru.class.arff.lrclass.txt | wc
 1269  cp IC_??.txt.ru.class.arff.lrclass.txt lr/.
 1270  cd lr
 1271  wc IC_??.txt.ru.class.arff.lrclass.txt 
 1272  wc IC_??.txt.ru.class.arff.lrclass.txt | less
 1273  ls
 1274  grep -E "a = C0|b = C1" IC_?.txt.ru.class.arff.lrclass.txt 
 1275  grep -E "a = C0|b = C1" IC_?.txt.ru.class.arff.lrclass.txt >> result.txt
 1276  grep -E "a = C0|b = C1" IC_??.txt.ru.class.arff.lrclass.txt >> result.txt

