#!/usr/bin/perl
print "What is the filename containing the sequences? "; #prints a question that can prompt input
$name = <STDIN>;                                         #reads the value input by the user into $name
chomp($name);                                            #removes the endline character, if it exists
print "The sequence filename is $name \n";
#
open (FILENAME, $name) ||
       die "can't open $name: $!";
while ($line = <FILENAME>) {
	chomp ($line);	
	if ($line =~ /^>/){
	    $line =~ s/>//;
	    $seqname=$line;
	} else {
            $seq=$seq.$line;
        }
        }
#
     print "the sequence name is $seqname\n";
     print "Sequence read from file is:\n$seq \n";

