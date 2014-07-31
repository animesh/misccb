#!/usr/bin/perl
##################

# First you need to load the DBI module:
use DBI;

# Next open a connection session with the database:
$dbhost = 'localhost';
$dbusername = 'login';
$dbpassword = 'password';
$dbh = DBI->connect("DBI:mysql:database=mydatabase;host=$dbhost","$dbusername","$dbpassword");

# To insert something to the database, use the do function:
$sql = "INSERT INTO mydatabase (user_name, user_password, user_email) VALUES ('newaccount','password','test@warpalace.com')";
$dbh->do($sql);

# Use the do for other features as well such as updating and deleting:
$sql = "UPDATE mydatabase SET user_password='newpassword' WHERE user_name='roupen'";
$dbh->do($sql);
$dbh->do("DELETE FROM mydatabase WHERE username='helix'");

# When you are trying to retreive information from the database,
# use the prepare and execute functions:
$sql = "SELECT * FROM mydatabase WHERE username='roupen'";
$results = $dbh->prepare($sql);
$results->execute();

# After you have the information sorted from the database,
# you need to handle the information into variables so you
# can access them and use the results:
$ref = $results->fetchrow_hashref();
$hisUsername = $ref->{'user_name'};
$hisPassword = $ref->{'user_password'};
$hisEmail = $ref->{'user_email'};

# If you expect to have more than one result, you can
# make a loop to go through the rows of the database
# entries matched:
$sql = "SELECT * FROM mydatabase";
$results = $dbh->prepare($sql);
$results->execute();
$counter = 0;
while ($ref = $results->fetchrow_hashref()) {
    $counter++;
    print "This is the $counter row in the database\n";
    print "This row holds the information for: $ref->{'user_name'}\n";
}

# Close connection with the server:
$dbh->disconnect();
exit;
