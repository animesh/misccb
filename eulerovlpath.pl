use strict;
use warnings;
use Data::Dumper;
use Carp;

my @reads=<>;
my %ovlidx;
for(my $c1=0;$c1<=$#reads;$c1++){
	my $str1=$reads[$c1];
	chomp $str1;
	my $len1=length($str1);
	$str1=~s/\s+//g;
	for(my $c2=$c1+1;$c2<=$#reads;$c2++){
		my $str2=$reads[$c2];
		chomp $str2;
		my $len2=length($str2);
		$str2=~s/\s+//g;
		if($str2 ne $str1 and $str1 ne "" and $str2 ne "" and $len1 == $len2){
			my @ovl=overlap($str1,$str2);
			my $lenovl=length($ovl[0]);
			if($lenovl and ($len2-1) and $lenovl == ($len2-1)){
				#print "$str1\t$len1\t$str2\t$len2\t@ovl\t$lenovl\n";
				push(@{$ovlidx{$ovl[0]}},substr($ovl[0],1,$lenovl-1).substr($str2,$lenovl,1));				
				push(@{$ovlidx{substr($str1,0,$lenovl-1).substr($ovl[0],$lenovl-1,1)}},$ovl[0]);
				#print "$ovl[0]\t@{$ovlidx{$ovl[0]}}\t",substr($str1,0,$lenovl-1).substr($ovl[0],$lenovl-1,1),"\t$ovl[0]\n";				
			}
			#print "$str1\t$len1\t$str2\t$len2\t@ovl\t$lenovl\n";
		}
	}
}

my @path=eulerPath(%ovlidx);
#source http://en.wikibooks.org/wiki/Algorithm_implementation/Strings/Longest_common_substring#Perl
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

#source http://stackoverflow.com/questions/4031325/finding-eulerian-path-in-perl

sub eulerPath {

    my %graph = @_;

    # count the number of vertices with odd degree
    my @odd = ();
    foreach my $vert ( sort keys %graph ) {
        my @edg = @{ $graph{$vert} };

        my $size = scalar(@edg);
        if ( $size % 2 != 0 ) {
            push @odd, $vert;
        }
    }

    push @odd, ( keys %graph )[0];

    if ( scalar(@odd) > 3 ) {
        return "None";

    }

    my @stack = ( $odd[0] );
    my @path  = ();

    while (@stack) {
        my $v = $stack[-1];
        #suggestion http://stackoverflow.com/a/4031608
        if ( @{$graph{$v}} ) {
                my $u = ( @{ $graph{$v} } )[0];
                push @stack, $u;
              # Find index of vertice v in graph{$u}

            my @graphu = @{ $graph{$u} };  # This is line 54.
            my ($index) = grep $graphu[$_] eq $v, 0 .. $#graphu;
            #suggestion http://stackoverflow.com/a/4031608
            splice @{ $graph{$u} }, $index, 1;
            splice @{ $graph{$v} }, 0, 1;

        }
        else {

            push @path, pop(@stack);
        }

    }

    print Dumper \@path;

    return @path;
}


__END__
 2076  perl -e '@b=qw/A T G C/;print "while($l<1000){print @b[int(rand(4))];$l++;}'
 2073  perl -ne 'if ($p) { for($c=0;$c<length;$c++){print substr($_,$c,10);print "\n"}; $p = 0 } $p++ if />/' rangen.txt  > rangen.txt.kmer
 2075  time perl overlap.pl rangen.txt.kmer

perl -e '@b=qw/A T G C/;while($l<100){print @b[int(rand(4))];$l++;}' > rangen.txt
perl -ne 'for($c=0;$c<length;$c++){print substr($_,$c,10);print "\n"};' rangen.txt > rangen.txt.kmer 

perl -e '@b=qw/A T G C/;print ">RandGenomeL1000\n";while($l<1000){print @b[int(rand(4))];$l++;}' > rangen.txt
perl -ne 'if ($p) { for($c=0;$c<length;$c++){print substr($_,$c,10);print "\n"}; $p = 0 } $p++ if />/' rangen.txt  > rangen.txt.kmer
perl overlap.pl rangen.txt.kmer
perl -ne 'for($c=0;$c<length;$c++){print substr($_,$c,10);print "\n"};' rangen.txt
perl eulerovlpath.pl rangen.txt.kmer 

