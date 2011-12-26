open(F,"seq2.needle");
while($line=<F>){
		if($line=~/^# Length: /){my @t=split(/\:/,$line);$length=@t[1]; print $length;    }
	    }
