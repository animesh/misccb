#!/usr/local/bin/perl

use strict;

use Getopt::Long;
use Bio::EnsEMBL::DBSQL::Obj;
use Bio::EnsEMBL::DBLoader;

# global defaults
my $host = 'localhost';
my $dbuser = 'root';
my $dbname = 'arne_freeze05_tim';
my $dbpass = undef;
my $do_gene = 0;
my $usefile = 0;
my $module = 'Bio::EnsEMBL::DBSQL::Obj';
my $port   = '410000';

&GetOptions( 
	     'dbuser:s'  => \$dbuser,
	     'dbpass:s'  => \$dbpass,
	     'host:s'    => \$host,
	     'dbname:s'  => \$dbname,
	     'usefile'   => \$usefile,
	     'port:n'    => \$port
	     );
my @clone;

if( $usefile == 1 ) {
    while( <> ) {
	my ($en) = split;
	push(@clone,$en);
    }
} else {
    @clone = @ARGV;
}

my $locator = "$module/host=$host;port=$port;dbname=$dbname;user=$dbuser;pass=$dbpass";
print STDERR "Using $locator for todb\n";
my $db =  Bio::EnsEMBL::DBLoader->new($locator);


foreach my $clone_id ( @clone) {
    print STDERR "Deleting $clone_id\n";
    my $clone = $db->get_Clone($clone_id);

    if( $do_gene == 1 ) {
	my @genes = $clone->get_all_Genes();

	foreach my $gene ( @genes ) {
	    print STDERR "   Deleting gene ".$gene->id."\n";
	    $db->delete_Gene($gene->id());
	}
    }

    # deprecated method!
    # $db->delete_Clone($clone_id);
    $clone->delete;
    
}

	

