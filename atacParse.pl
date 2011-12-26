$file=shift;
open(F,$file);
while($l=<F>){
	chomp $l;
	@tmp=split(/\s+/,$l);
	if(@tmp[1] eq "c"){
		$lenmat1=@tmp[6];
		$lenmat2=@tmp[10];
		if($lenmat1>$lenmat2){$lenmat=$lenmat2}
		else{$lenmat=$lenmat1}
		$matlen{"@tmp[4]-@tmp[8]"}=$lenmat;
		$mat{"@tmp[4]-@tmp[8]"}="$lenmat1-$lenmat2";
		$pos{"@tmp[4]-@tmp[8]"}="@tmp[5]-@tmp[9]";
		$totlen+=$lenmat;
		#print "$lenmat\t@tmp[6]-@tmp[10] @tmp[4]-@tmp[8] @tmp[5]-@tmp[9]\n";
	}
}

for $key ( sort {$matlen{$b}<=>$matlen{$a}} keys %matlen) {
	$acclen+=$matlen{$key};
	$cnt++;
	$per=100*$matlen{$key}/$totlen;
	print "$cnt\t$key\t$matlen{$key}\t$acclen\t$totlen\t$per\t$mat{$key}\t$pos{$key}\n";
}
