#!/usr/bin/perl
$name = <STDIN>;
chomp($name);
print "The sequence filename is $name \n";
open (FILENAME, $name) ||
       die "can't open $name: $!";
$seq = "";
while ($line = <FILENAME>) {
	chomp ($line);	
	if ($line =~ /^>/){
	    $line =~ s/>//;
	    push(@seqname,$line);
	    if ($seq ne ""){
	      push(@seq,$seq);
	      $seq = "";
	    }
      } else {
            $seq=$seq.$line;
      }
}
push(@seq,$seq);
$c=length(@seqname);
for($w=0;$w<$c;$w++)
{
foreach $element(@seqname)
{
keys(%seqn)=@seqn{$w}=$element;
}
$w++;
foreach $valu(@seq)
{
 values(%seqn)=@seqn{$w}=$valu;
}
}
while(($k,$v) = each(%seqn))
{
print "$k  $v  \n";
}