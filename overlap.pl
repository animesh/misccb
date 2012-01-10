#source http://en.wikibooks.org/wiki/Algorithm_implementation/Strings/Longest_common_substring#Perl
$str1=$ARGV[0];
$str2=$ARGV[1];
@reads=<>;
for($c1=0;$c1<=$#reads;$c1++){
	$str1=$reads[$c1];
	$str1=~s/\s+|\n//g;
	for($c2=$c1+1;$c2<=$#reads;$c2++){
		$str2=$reads[$c2];
		$str2=~s/\s+|\n//g;
		if($str2 ne $str1 and $str1 ne "" and $str2 ne ""){
			print overlap($str1,$str2),"\n";
		}
	}
}
#print overlap($str1,$str2),"\n";
sub overlap{
  my ($str1, $str2) = @_; 
  my $l_length = 0; # length of longest common substring
  my $len1 = length $str1; 
  my $len2 = length $str2; 
  my @char1 = (undef, split(//, $str1)); # $str1 as array of chars, indexed from 1
  my @char2 = (undef, split(//, $str2)); # $str2 as array of chars, indexed from 1
  my @lc_suffix; # "longest common suffix" table
  my @substrings; # list of common substrings of length $l_length
  for my $n1 ( 1 .. $len1 ) { 
    for my $n2 ( 1 .. $len2 ) { 
      if ($char1[$n1] eq $char2[$n2]) {
        $lc_suffix[$n1-1][$n2-1] ||= 0;
        $lc_suffix[$n1][$n2] = $lc_suffix[$n1-1][$n2-1] + 1;
        if ($lc_suffix[$n1][$n2] > $l_length) {
          $l_length = $lc_suffix[$n1][$n2];
          @substrings = ();
        }
        if ($lc_suffix[$n1][$n2] == $l_length) {
          push @substrings, substr($str1, ($n1-$l_length), $l_length);
        }
      }
    }
  }   
 
  return @substrings;
}
__END__
 2076  perl -e '@b=qw/A T G C/;print "while($l<1000){print @b[int(rand(4))];$l++;}'
 2073  perl -ne 'if ($p) { for($c=0;$c<length;$c++){print substr($_,$c,10);print "\n"}; $p = 0 } $p++ if />/' rangen.txt  > rangen.txt.kmer
 2075  time perl overlap.pl rangen.txt.kmer

perl -e '@b=qw/A T G C/;while($l<100){print @b[int(rand(4))];$l++;}' > rangen.txt
perl -ne 'for($c=0;$c<length;$c++){print substr($_,$c,10);print "\n"};' rangen.txt > rangen.txt.kmer 

