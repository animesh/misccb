while(<>){
        chomp;
        @tmp=split(/\s+/,);
                         @tmp[2] =~ s/\'//g;
                         @tmp[4] =~ s/\'//g;
        #if(@tmp[0] eq "C" and @tmp[5] < 10 and $fh{100000*@tmp[1]+@tmp[2]}<2 and $fh{100000*@tmp[3]+@tmp[4]}<2 ){
        if(@tmp[0]=~/[0-9]+/){
                        $tc++;
                        $cl{@tmp[0]}=@tmp[2];
                #print "@tmp[0] @tmp[1] @tmp[2] @tmp[3] @tmp[4]\n";
        }
        if(@tmp[0] eq "C"){
                        if(@tmp[2]==5){$fpe{@tmp[1]}++;}
                        if(@tmp[2]==3){$tpe{@tmp[1]}++;}
                        if(@tmp[4]==5){$fpe{@tmp[3]}++;}
                        if(@tmp[4]==3){$tpe{@tmp[3]}++;}
        }
	if(@tmp[0] eq "C" and @tmp[5] < 10 and ($cig{@tmp[1]}<1 and $cig{@tmp[3]}<1) ){
                        $cig{@tmp[1]}++;
                        $cig{@tmp[3]}++;
                print "RE @tmp[1] @tmp[2] @tmp[3] @tmp[4]\n";
        }


}
for($c1=1;$c1<=$tc;$c1++){
        if( ($fpe{$c1}<1 || $tpe{$c1}<1) && $cl{$c1}<500){
                print "RC $c1\n";
        }
}

#editcg.pl (END)


