#!/usr/bin/perl -w

# Modified from "Relational Modeling of
# Biological Data: Trees and Graphs", page 2:
# http://www.oreillynet.com/pub/a/network/2002/11/27/bioconf.html?page=2

use strict;
use warnings;

use DBI;
use Getopt::Long qw( :config no_ignore_case );

#-----------------------------------------------------------------------

sub usage {
  print("Usage:\n");
  printf( "\t%s\t-h dbhost [-P dbport] \\\n"
      . "\t%s\t-u dbuser [-p dbpass] \\\n"
      . "\t%2\$s\t-d dbname\n",
    $0, ' ' x length($0) );
  print("\n");
  printf( "\t%s\t-?\n", $0 );
  print("\n");
  print("Arguments:\n");
  print("\t-h/--host dbhost\tDatabase server host name\n");
  print("\t-P/--port dbport\tDatabase server port (optional)\n");
  print("\t-u/--user dbuser\tDatabase user name\n");
  print("\t-p/--pass dbpass\tUser password (optional)\n");
  print("\t-d/--name dbname\tDatabase name\n");
  print("\t-?/--help\t\tDisplays this information\n");
}

#-----------------------------------------------------------------------

my ( $dbhost, $dbport );
my ( $dbuser, $dbpass );
my $dbname;

$dbport = '3306';

if (
  !GetOptions(
    'dbhost|host|h=s' => \$dbhost,
    'dbport|port|P=i' => \$dbport,
    'dbuser|user|u=s' => \$dbuser,
    'dbpass|pass|p=s' => \$dbpass,
    'dbname|name|d=s' => \$dbname,
    'help|?'          => sub { usage(); exit } )
  || !defined($dbhost)
  || !defined($dbuser)
  || !defined($dbname) )
{
  usage();
  exit;
}

my $dsn = sprintf( "DBI:mysql:database=%s;host=%s;port=%s",
  $dbname, $dbhost, $dbport );

my $dbh =
  DBI->connect( $dsn, $dbuser, $dbpass,
  { 'RaiseError' => 1, 'PrintError' => 1 } );

$dbh->do('TRUNCATE TABLE closure');
$dbh->do('ALTER TABLE closure DISABLE KEYS');

$dbh->do(
  q(
INSERT INTO closure
  (child_term_id, parent_term_id, distance, subparent_term_id)
SELECT  term_id, term_id, 0, NULL
FROM    term
) );

$dbh->do(
  q(
INSERT INTO closure
  (child_term_id, parent_term_id, distance, subparent_term_id)
SELECT  child_term_id, parent_term_id, 1, child_term_id
FROM    relation r
) );

my $select_sth = $dbh->prepare(
  q(
SELECT DISTINCT
        child.child_term_id,
        parent.parent_term_id,
        child.distance + 1,
        parent.child_term_id
FROM    closure child,
        closure parent
WHERE   parent.child_term_id = child.parent_term_id
  AND   child.distance  = ?
  AND   parent.distance = 1
) );

my $insert_sth = $dbh->prepare(
  q(
REPLACE INTO closure
  (child_term_id, parent_term_id, distance, subparent_term_id)
VALUES (?, ?, ?, ?)
) );

my ($oldsize) =
  $dbh->selectrow_array('SELECT COUNT(1) FROM closure');

my $newsize;
my $distance = 0;

while ( !defined($newsize) || $newsize > $oldsize ) {
  $oldsize = $newsize || $oldsize;
  $newsize = $oldsize;

  $dbh->do('LOCK TABLES closure AS child READ, closure AS parent READ');

  $select_sth->execute( ++$distance );

  printf( "Distance = %d\n", $distance );

  $dbh->do('LOCK TABLE closure WRITE');
  while ( my @data = $select_sth->fetchrow_array() ) {
    $insert_sth->execute(@data);
    $newsize++;
  }
  $dbh->do('UNLOCK TABLES');
}

$dbh->do('ALTER TABLE closure ENABLE KEYS');
$dbh->do('OPTIMIZE TABLE closure');

$dbh->disconnect();

# $Id: compute_closure.pl,v 1.2 2009/05/11 14:06:40 ak4 Exp $
