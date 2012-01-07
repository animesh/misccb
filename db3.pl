use strict;
use DBI;
use XML::XPath;

my $dbh = DBI->connect('dbi:mysql:mydb', 'mysqlu','mysqlp') || die $DBI::errstr;
my $xp = XML::XPath->new(filename => 'dump.xml');

my $sth = $dbh->prepare(qq{INSERT INTO users_table VALUES (?,?,?)});

# loop through the records
foreach my $row ($xp->findnodes('/DBI/RESULTSET/ROW')) {

    # extract from the XML
    my $email = $row->find('email')->string_value;
    my $user = $row->find('user')->string_value;
    my $name = $row->find('name')->string_value;
    # insert into the db (using placeholders)
    $sth->execute($user,$name,$email) || die $DBI::errstr;
}

$dbh->disconnect;
print "Success!!! \n";
