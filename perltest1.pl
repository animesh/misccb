
#!/usr/bin/perl
print "What is the name of your file? ";
$name = <STDIN>;
chomp($name);
print "The file name is $name \n";
open (FILEHANDLE,$name);
while($line=<FILEHANDLE>) {
print"$line";
}
close (FILEHANDLE)