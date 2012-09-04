$sn = rec(@ARGV[0]);
print "$sn\n";
sub rec
{
    print "$n\n";
    $n = shift;
    if($n <= 1) {exit;}
    else{$p = $n * rec($n - 1);}
    return $p;
}
