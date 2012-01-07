#!/usr/local/ensembl/bin/perl -w

use strict;
use DBI;
use Getopt::Long;
use Bio::EnsEMBL::Compara::DBSQL::DBAdaptor;
use Bio::EnsEMBL::Pipeline::DBSQL::DBAdaptor;
use Bio::EnsEMBL::Pipeline::Analysis;
use Bio::EnsEMBL::Pipeline::Rule;
use Bio::EnsEMBL::Compara::GenomeDB;
use Bio::EnsEMBL::DBLoader;


# ok this is a hack, but I'm going to pretend I've got an object here
# by creating a blessed hash ref and passing it around like an object
# this is to avoid using global variables in functions, and to consolidate
# the globals into a nice '$self' package
my $self = bless {};

$self->{'compara_conf'} = {};
$self->{'compara_conf'}->{'-user'} = 'ensro';
$self->{'compara_conf'}->{'-port'} = 3306;

$self->{'speciesList'} = ();
$self->{'removeXedSeqs'} = undef;
$self->{'outputFasta'} = undef;
$self->{'noSplitSeqLines'} = undef;

my $conf_file;
my ($help, $host, $user, $pass, $dbname, $port, $adaptor);

GetOptions('help'     => \$help,
           'conf=s'   => \$conf_file,
           'dbhost=s' => \$host,
           'dbport=i' => \$port,
           'dbuser=s' => \$user,
           'dbpass=s' => \$pass,
           'dbname=s' => \$dbname,
           'fasta=s'  => \$self->{'outputFasta'},
           'noX=i'    => \$self->{'removeXedSeqs'},
           'nosplit'  => \$self->{'noSplitSeqLines'},
          );

if ($help) { usage(); }

parse_conf($self, $conf_file);

if($host)   { $self->{'compara_conf'}->{'-host'}   = $host; }
if($port)   { $self->{'compara_conf'}->{'-port'}   = $port; }
if($dbname) { $self->{'compara_conf'}->{'-dbname'} = $dbname; }
if($user)   { $self->{'compara_conf'}->{'-user'}   = $user; }
if($pass)   { $self->{'compara_conf'}->{'-pass'}   = $pass; }


unless(defined($self->{'compara_conf'}->{'-host'})
       and defined($self->{'compara_conf'}->{'-user'})
       and defined($self->{'compara_conf'}->{'-dbname'}))
{
  print "\nERROR : must specify host, user, and database to connect to compara\n\n";
  usage(); 
}

$self->{'comparaDBA'}  = new Bio::EnsEMBL::Compara::DBSQL::DBAdaptor(%{$self->{'compara_conf'}});
$self->{'pipelineDBA'} = new Bio::EnsEMBL::Pipeline::DBSQL::DBAdaptor(-DBCONN => $self->{'comparaDBA'}->dbc);

while(convert_sequence($self)) {};

exit(0);


#######################
#
# subroutines
#
#######################

sub usage {
  print "comparaDumpAllPeptides.pl [options]\n";
  print "  -help                  : print this help\n";
  print "  -conf <path>           : config file describing compara, templates, and external genome databases\n";
  print "  -dbhost <machine>      : compara mysql database host <machine>\n";
  print "  -dbport <port#>        : compara mysql port number\n";
  print "  -dbname <name>         : compara mysql database <name>\n";
  print "  -dbuser <name>         : compara mysql connection user <name>\n";
  print "  -dbpass <pass>         : compara mysql connection password\n";
  print "  -fasta <path>          : file where fasta dump happens\n";
  print "  -noX <num>             : don't dump if <num> 'X's in a row in sequence\n";
  print "  -nosplit               : don't split sequence lines into readable format\n";
  print "comparaDumpAllPeptides.pl v1.1\n";
  
  exit(1);  
}


sub parse_conf {
  my $self      = shift;
  my $conf_file = shift;

  if($conf_file and (-e $conf_file)) {
    #read configuration file from disk
    my @conf_list = @{do $conf_file};

    foreach my $confPtr (@conf_list) {
      #print("HANDLE type " . $confPtr->{TYPE} . "\n");
      if($confPtr->{TYPE} eq 'COMPARA') {
        $self->{'compara_conf'} = $confPtr;
      }
    }
  }
}


sub convert_sequence {
  my $self = shift;

  my $db = $self->{'comparaDBA'};

  my $sql_tmp = "SELECT member.member_id, member.sequence FROM member WHERE member.sequence IS NOT NULL";
  my $sql = $db->dbc->add_limit_clause($sql_tmp,5000);

  my $sth = $db->prepare($sql);
#   my $sth = $db->prepare("SELECT member.member_id, member.sequence " .
#                          " FROM member" .
#                          " WHERE member.sequence IS NOT NULL".
#                          " LIMIT 5000 ");
  $sth->execute();

  my ($member_id, $sequence);
  my $seqCount=0;
  $sth->bind_columns( undef, \$member_id, \$sequence );

  while( $sth->fetch() ) {
    $seqCount++;
    my $seq_length = length($sequence);

    my $sth_seq = $db->prepare("INSERT INTO sequence (sequence, length) VALUES (?,?)");
    $sth_seq->execute($sequence, $seq_length);
    my $sequence_id = $sth_seq->{'mysql_insertid'};
    $sth_seq->finish;

    my $sth_mem = $db->prepare("UPDATE member SET sequence_id=?, sequence=NULL WHERE member_id=?");
    $sth_mem->execute($sequence_id, $member_id);
    $sth_mem->finish;

    print("converted sequence for member_id=$member_id\n");
  }
  $sth->finish();
  return $seqCount;
}
