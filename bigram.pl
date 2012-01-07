#!/usr/bin/perl -w


##############################
#
# This script creates a 2d hash with bigram statistics and then prints it out
#
#############################


while (<>) {
  $_ =index.html $1 . $_;            # prepend extra one from last line
  while (s/(.)(.)/$2/) {   # as long as 2 non-newlines
    $bigram{$1}{$2}++;
  }
  /(.)/;                   # save first character of $_ in $1
}
    
foreach $first (sort keys %bigram) {     # %bigram is a hash of references to hashes
  print $first;
  foreach $second (sort keys %{$bigram{$first}}) {            #dereference with %
    print "\t$second $bigram{$first}{$second}\n";
  }
}
