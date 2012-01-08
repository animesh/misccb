#source http://stackoverflow.com/a/6026745/1137129
$str1=$ARGV[0];
$str2=$ARGV[1];
print overlap($str1,$str2),"\n";
sub overlap{
    my $c = 0;
    my $ovl = "";
    my $s1 = shift;
    my $s2 = shift;
    @s1=split(//,$s1);
    @s2=split(//,$s1);
    while ($s1[$c] and $s2[$c]){
	if($s1[$c] eq $s2[$c]){
		$ovl.=$s1[$c];
        	$c++;
	}
    }
    return ($ovl,$c);
}
