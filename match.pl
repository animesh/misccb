my $seq = "MVQRWLYSTNAKDIAVLYFMLAIFSGMAGTAMSLIIRLELAAPG";
my $pep = "MAGTAM";
while($seq =~/$pep/g) {
    print "Found a match from ".($-[0]+1)." to ".($+[0])."\n";
                      }

