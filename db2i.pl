#!/usr/bin/perl
##################

# First you need to load the DBI module:
use DBI;

# Next open a connection session with the database:
$dbhost = 'localhost';
print "\nenter the dbusername:>";
$dbusername = <STDIN>;
print "\nenter the password for dbuser $dbusername:>";
$dbpassword =<STDIN>;
$dbh = DBI->connect("DBI:mysql:database=db;host=$dbhost","$dbusername","$dbpassword");
# To insert something to the database, use the do function:
print  "enter the dept no. :> \n";
$dno=<STDIN>;
print "enter the dname :>\n";
$dname=<STDIN>;
$sql = "INSERT INTO dept(dno,dname) VALUES ($dno,$dname)";
$dbh->do($sql);
# Close connection with the server:
$dbh->disconnect();
exit;
