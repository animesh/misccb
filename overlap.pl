#source http://www.perlmonks.org/?abspart=1;displaytype=displaycode;node_id=733924;part=1
$str1=$ARGV[0];
$str2=$ARGV[1];
print overlap($str1,$str2),"\n";
sub overlap{
    my $s1 = shift;
    my $s2 = shift;
    my $ovl = $s1 ^ $s2;
    $ovl =~ s/\0/X/g;
    return ($ovl);
}
