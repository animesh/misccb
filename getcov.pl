$file=shift @ARGV; chomp $file; open(FC,$file);
$fw=$file.".depth.txt";
open(FO,">$fw");
$min=Inf;$max=-Inf;
while($l=<FC>){
$line++;
@tmp=split(/\t/,$l);
if($l=~/^F/ and @tmp[6] ne "" and @tmp[5] ne ""){
chomp $l;
#$l=~s/\s+//g;
if(@tmp[5]>@tmp[6]){$swap=@tmp[6];@tmp[6]=@tmp[5];@tmp[5]=$swap;print "S-$swap";}
if($max<@tmp[6]){$max=@tmp[6];}
if($min>@tmp[5]){$min=@tmp[5];}
for($c=@tmp[5];$c<=@tmp[6];$c++){$pos{$c}++;}
#print "R-@tmp[0] S-@tmp[5] E-@tmp[6] S-$swap C-$c D-$pos{$c}\n";
#@tmp[0]=~s/^>|\s+$//g;
}
}
print "$line\t$min\t$max\n";
for($c=$min;$c<=$max;$c++){
        if($pos{$c}){print FO"$c\t$pos{$c}\n";}
        else{print FO"$c\t0\n";}
}


