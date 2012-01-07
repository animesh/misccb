#!/usr/bin/perl
open f1,"AC109365.tot";
while($line=<f1>)
{
if($line=~/Query/)
{print "$line\t";}
if($line=~/Sbjct/)
{print "$line\n";}
}
