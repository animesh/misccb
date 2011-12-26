#!/usr/bin/perl
$x=shift;
$y=shift;
$z=shift;
$x>0 and $y>0 or die "argument must be 
two and positive 
$!";
print "\nhypotenuse is:", sqrt($x**2+$y**2);
