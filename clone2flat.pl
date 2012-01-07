#!/usr/local/bin/perl 

=head1 NAME - clone2flat.pl

 provides flat file formats from EnsEMBL databases

=head1 SYNOPSIS - 

    clone2embl dJ271M21

    clone2embl -format gff dJ271M21
   
    clone2embl -dbtype ace dJ271M21

    clone2embl -dbtype rdb -host mysql.server.somewhere dJ271M21

    clone2embl -dbtype timdb AL035541           # dump as accession
    clone2embl -dbtype timdb dJ718J7            # dump as accession
    clone2embl -dbtype timdb -noacc dJ718J7     # dump as clone

=head1 OPTIONS

    -verbose   print to STDERR on each clone to dump

    -module    Module name to load (Defaults to Bio::EnsEMBL::DBSQL::Obj)

    -usetimdb  Overrides Module name for using Flat file Sanger system
    
    -oldtimdb  Test of old way of calling TimDB object
    
    -species   Species other than human (e.g. 'mouse' - if not set, only loads human)

    -freeze    Loads only clones in current frozen set (number)

    -nogene    Allows clones to be read if dna has been updated successfully
               and searches completed, but before genes have been built and clone unlocked

    -nosecure

    -dbtype    Database type (valid types are timdb, ace)

    -host      host name for database (gets put as host= in locator)
			      
    -dbname    For RDBs, what name to connect to (dbname= in locator)

    -dbuser    For RDBs, what username to connect as (dbuser= in locator)

    -dbpass    For RDBs, what password to use (dbpass= in locator)

    -nodna     dont write dna part of embl file (for testing)

    -format    [gff/ace/pep/fasta] dump in gff/ace/peptide/fasta format, not EMBL

    -pepformat What format output to dump to. NB gene2flat is a better
               script now for dumping translations.

    -noacc     [only timdb] by default, regardless of specifing the
               accession for a sanger clone or its clonename, it will
               dump as its accession.  Use -noacc to dump by clonename

    -test      use test database rather than live [only timdb]
               clones in testdb are listed with a T below

    -getall    all clones from the database [no applicable to timdb]

    -usefile   read in on stdin a list of clones, one clone per line

    -start     start point in list of clones (useful with -getall)

    -end       end point in list of clones (useful with -getall)

    -outfile   write output into file instead of to STDOUT

    -help      displays this documentation with PERLDOC

=head1 EXAMPLE CLONES

    dJ271M21/AL031983   T  single contig, mainly forward strand genes, but one reverse

    dJ718J7                single contig, single reverse strand gene with partial transcripts

    C361A3/AL031717     T  unfinished sanger, 3 contigs, 2 ordered by gene predictions

    C367G8/Z97634       T  unfinished sanger, 2 contigs, not ordered by gene predictions

    AP000228               External finished clone

    AC005776               External finished clone

    AC010144               External unfinished clone

=cut

# to find more things in TimDB use:
#~humpub/scripts.devel/set_dbm.pl -f ~/th/unfinished_ana/unfinished_clone -l 10

use strict;

use Bio::EnsEMBL::DBLoader;
use Bio::EnsEMBL::EMBL_Dump;

# we run time load tim db now
#use Bio::EnsEMBL::TimDB::Obj;

use Bio::SeqIO;

use Getopt::Long;

# signal handler
$SIG{INT}=sub {my $sig=shift;die "exited after SIG$sig";};

# global defaults

my $module    = 'Bio::EnsEMBL::DBSQL::Obj';

my $dbtype    = '';
my $format    = 'embl';
my $nodna     = 0;
my $help;
my $noacc     = 0;
my $aceseq;
my $fromfile  = 0;
my $getall    = 0;
my $pepformat = 'Fasta';
my $test=0;
my $part=0;
my $verbose   = 0;
my $cstart    = 0;
my $cend      = undef;
my $outfile;
my $oldstyle = 0;
my $usetimdb = 0;
my $checkdna;
# test
my $oldtimdb=0;
my $species='';
my $freeze=0;
my $nogene=0;
my $nosecure=0;

# defaults for msql (rdb) access
my $host      = 'localhost';
my $dbname    = 'ensembl_freeze17';
my $dbuser    = 'ensro';
my $dbpass    = undef;
my $port      = '410000';

# this doesn't have genes (finished)
#my $clone  = 'dJ1156N12';
# this does have genes (finished)
#my $clone  = 'dJ271M21';
# this does have genes (unfinished)
# my $clone = '217N14';

&GetOptions( 'module:s'  => \$module,
	     'usetimdb'  => \$usetimdb,
	     'dbtype:s'  => \$dbtype,
	     'dbuser:s'  => \$dbuser,
	     'dbpass:s'  => \$dbpass,
	     'host:s'    => \$host,
	     'port:n'    => \$port,
	     'dbname:s'  => \$dbname,
	     'format:s'  => \$format,
	     'nodna'     => \$nodna,
	     'h|help'    => \$help,
	     'noacc'     => \$noacc,
	     'aceseq:s'  => \$aceseq,
	     'pepform:s' => \$pepformat,
	     # usefile to be consistent with other scripts
	     'usefile'   => \$fromfile,
	     'getall'    => \$getall,
	     'test'      => \$test,
	     'part'      => \$part,
	     'checkdna:s'=> \$checkdna,
	     'verbose'   => \$verbose,
	     'start:i'   => \$cstart,
	     'end:i'     => \$cend,
	     'outfile:s' => \$outfile,
	     'oldstyle'  => \$oldstyle,
	     'species:s' => \$species,
	     'oldtimdb'  => \$oldtimdb,
	     'freeze:n'  => \$freeze,
	     'nogene'    => \$nogene,
	     'nosecure'  => \$nosecure,
	     ) or exec('perldoc', $0);

if ($help){
    exec('perldoc', $0);
}

# set module if dbtype set and recognised
if($dbtype eq 'timdb'){
    $usetimdb=1;
}elsif($dbtype eq 'ace'){
    $module='Bio::EnsEMBL::AceDB::Obj';
}elsif($dbtype){
    die "-dbtype $dbtype not recognised";
}

my $db;

my @clones;

if( $fromfile == 1 ) {
    while( <> ) {
	my ($cloneid) = split;
	push(@clones,$cloneid);
    }
} else {
    @clones = @ARGV;
}


if ( $usetimdb == 1 ) {

    # run time loading of TimDB so sites with no
    # timdb dependencies can loaded. Yes - this is not pretty.
    # Yes - we will get rid of it as soon as we can.

    my $requirestr = "Bio::EnsEMBL::TimDB::Obj.pm";
    $requirestr =~ s/::/\//g;

    eval {
	require $requirestr;
    };
    if( $@ ){
	print STDERR "Unable to load TimDB, due to\n $@\n";
    }

    # don't specify clone list if want to do getall, so whole db loads
    my $raclones;
    if(!$getall){
	$raclones=\@clones;
    }

    # clones required are passed to speed things up - cuts down on parsing of flat files
    if(!$oldtimdb){
	$db = Bio::EnsEMBL::TimDB::Obj->new(-clones => $raclones,
					    -noacc => $noacc,
					    -test => $test,
					    -part => $part,
					    -species => $species,
					    -freeze => $freeze,
					    -nogene => $nogene,
					    -nosecure => $nosecure,
					    );
    }else{
	$db = Bio::EnsEMBL::TimDB::Obj->new($raclones,$noacc,$test,$part);
    }
} else {
    
    my $locator = "$module/host=$host;port=$port;dbname=$dbname;user=$dbuser;pass=$dbpass";
    $db = Bio::EnsEMBL::DBLoader->new($locator);

}

if ( $getall == 1 ) {
    @clones = $db->get_all_Clone_id();
    print STDERR scalar(@clones)." clones found in DB\n";
}

if( defined $cend ) {
    print STDERR "splicing $cstart to $cend\n";

    my @temp = splice(@clones,$cstart,($cend-$cstart));
    @clones = @temp;
}

# set output file
my $OUT;
if($outfile){
    open(OUT,">$outfile") || die "cannot write to $outfile";
    $OUT=\*OUT;
}else{
    $OUT=\*STDOUT;
}


#
# Main loop over clone ids...
#
#
#

foreach my $clone_id ( @clones ) {

    if( $verbose >= 1 ) {
	print STDERR "Dumping $clone_id\n";
    }


    #
    # wrap in eval to protect from exceptions being
    # thrown. One problem here is that if the exception
    # is thrown during the feature table dumping, then it
    # has still dumped part of the entry. Bad news. Need to "exercise" the
    # objects first.
    #
    # I think we just have to assumme that if we get an object that
    # it conforms to the interface. 
    #

    eval {
	my $clone = $db->get_Clone($clone_id);
	my $as    = $clone->virtualcontig;
#	$as->skip_SeqFeature('similarity',1);
	# choose output mode
	
	# test clone check call
	if($checkdna){
	    if($clone->compare_dna($checkdna)){
		print STDERR "WARN: DNA of $checkdna DIFFERENT to that in $clone_id\n";
	    }else{
		print STDERR "DNA of $checkdna identical to that in $clone_id\n";
	    }
	}

	my $debug = 0;
	if($debug){
	    # debug tests
	    my $id=$clone->id;
	    print "DNA date [$id]: ".$clone->seq_date."\n";
	    print "create date [$id]: ".$clone->created."\n";
	    print "modify date [$id]: ".$clone->modified."\n";
	    print "version [$id]: ".$clone->version."\n";

	    # try to load genes
	    $clone->get_all_Genes;

	    # debug tests by contig
	    foreach my $contig ($clone->get_all_Contigs){
		my $id=$contig->id;
		print "DNA date [$id]: ".$contig->seq_date."\n";
	    }
	    exit 0;
	}
        print(STDERR "Format is $format\n");
	if( $format =~ /gff/ ) {
	    foreach my $contig ( $clone->get_all_Contigs )  {
		my @seqfeatures = $contig->as_seqfeatures();
		foreach my $sf ( @seqfeatures ) {
		    print $OUT $sf->gff_string, "\n";
		}
	    }
	} elsif ( $format =~ /fasta/ ) {
	    my $seqout = Bio::SeqIO->new( '-format' => 'Fasta' , -fh => $OUT);
	    
	    foreach my $contig ( $clone->get_all_Contigs ) {
		$seqout->write_seq($contig->seq());
	    }
	} elsif ( $format =~ /embl/ ) {
	    print(STDERR "Dumping embl\n");
	    &Bio::EnsEMBL::EMBL_Dump::add_ensembl_comments($as);
	    print(STDERR "making new seq\n");
	    my $emblout = Bio::SeqIO->new( '-format' => 'EMBL', -fh => $OUT);
	    &Bio::EnsEMBL::EMBL_Dump::ensembl_annseq_output($emblout);
	    if( $nodna == 1 ) {
		$emblout->_show_dna(0);
	    }
	    
	    $emblout->write_seq($as);
	} elsif ( $format =~ /genbank/ ) {
	    &Bio::EnsEMBL::EMBL_Dump::add_ensembl_comments($as);
	    my $gbout = Bio::SeqIO->new( '-format' => 'GenBank', -fh => $OUT);
	    &Bio::EnsEMBL::EMBL_Dump::ensembl_annseq_output($gbout);
	   
	    # genbank format - the ID line is wrong. Fall back to locus
	    # $gbout->_id_generation_func(undef);
	    # $gbout->_ac_generation_func(undef);
	    # $as->accession($clone->id());
	    # $as->division("PRI");
	    
	    if( $nodna == 1 ) {
		$gbout->_show_dna(0);
	    }
	    
	    $gbout->write_seq($as);
	} elsif ( $format =~ /pep/ ) {
	    my $seqout = Bio::SeqIO->new ( '-format' => $pepformat , -fh => $OUT ) ;
	    my $cid = $clone->id();
	    
	    foreach my $gene ( $clone->get_all_Genes() ) {
		my $geneid = $gene->id();
		foreach my $trans ( $gene->each_Transcript ) {
		    my $tseq = $trans->translate();
		    if( $tseq->seq =~ /\*/ ) {	
			print STDERR $trans-id," got stop codons. Not dumping. on $cid\n";
		    }
		    $tseq->desc("Clone:$cid Gene:$geneid");
		    $seqout->write_seq($tseq);
		}
	    }
	} elsif ( $format =~ /ace/ ) {
	    foreach my $contig ( $clone->get_all_Contigs() ) {
		$contig->write_acedb($OUT,$aceseq);
	    }
	}
    };
    if( $@ ) {
	print STDERR "Dumping Error! Cannot dump $clone_id\n$@\n";
    }
}


