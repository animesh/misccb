#!/usr/local/bin/perl

=head1 NAME

gene2flat

=head1 SYNOPSIS
 
  gene2flat ENSG00000012

=head1 DESCRIPTION

gene2flat produces a number of flat file outputs of the genes,
in particular the protein translation

=head1 OPTIONS

    -dbtype    Database type (only used for TimDB)

    -dbhost    host name for database (gets put as host= in locator)

    -dbname    For RDBs, what name to connect to (dbname= in locator)

    -dbuser    For RDBs, what username to connect as (dbuser= in locator)

    -dbpass    For RDBs, what password to use (dbpass= in locator)

    -module    Module name to load (Defaults to Bio::EnsEMBL::DBSQL::Obj)

    -format    [pep/dump/transcript] dump in peptides/info/dna format

    -noacc     [only timdb] by default, regardless of specifing the
               accession for a sanger clone or its clonename, it will
               dump as its accession.  Use -noacc to dump by clonename

    -test      use test database rather than live [only timdb]
               clones in testdb are listed with a T below

    -getall    all clones from the database [no applicable to timdb]

    -usefile   read in on stdin a list of clones, one clone per line

    -verbose   print to STDERR on each gene to dump

    -help      displays this documentation with PERLDOC

    -chunk     chunk size to array (only use if know what this means)

=cut

use strict;

use Bio::EnsEMBL::DBSQL::Obj;
use Bio::EnsEMBL::DBLoader;
use Bio::EnsEMBL::TimDB::Obj;
use Bio::SeqIO;

use Getopt::Long;

my $dbtype = 'rdb';
my $host   = 'localhost';
my $port   = '410000';
my $dbname = 'ensembl';
my $dbuser = 'ensro';
my $dbpass = undef;
my $module = 'Bio::EnsEMBL::DBSQL::Obj';

my $format  = 'transcript';
my $usefile = 0;
my $getall  = 0;
my $verbose = 0;
my $noacc   = 0;
my $test    = 0;
my $webdir = undef;
my $logerror = undef;
my $help;
my $chunk   = 1;

&GetOptions( 
	     'dbtype:s'   => \$dbtype,
	     'host:s'     => \$host,
	     'port:n'     => \$port,
	     'dbname:s'   => \$dbname,
	     'dbuser:s'   => \$dbuser,
	     'dbpass:s'   => \$dbpass,
	     'module:s'   => \$module,
	     'chunk:i'    => \$chunk,
	     'usefile'    => \$usefile,
	     'format:s'   => \$format,
	     'getall'     => \$getall,
	     'verbose'    => \$verbose,
	     'webdir:s'     => \$webdir,
	     'test'       => \$test,
	     'noacc'      => \$noacc,
	     'logerror:s' => \$logerror,
	     'h|help'     => \$help
	     );
my $db;

if ($help) {
    exec('perldoc', $0);
}

if ( $dbtype =~ 'timdb' ) {
    $db = Bio::EnsEMBL::TimDB::Obj->new('',$noacc,$test);
} else {
    my $locator = "$module/host=$host;port=$port;dbname=$dbname;user=$dbuser;pass=$dbpass";
    $db =  Bio::EnsEMBL::DBLoader->new($locator);
}

my @gene_id;

if( $usefile ) {
    while( <> ) {
	my ($g) = split;
	push(@gene_id,$g);
    }
} elsif ( $getall == 1 ) {
    @gene_id = $db->get_all_Gene_id();
} else {
    @gene_id = @ARGV;
}

my $seqio;

if( $format eq 'pep' || $format eq 'transcript') {
    $seqio = Bio::SeqIO->new('-format' => 'Fasta' , -fh => \*STDOUT ) ;
}

if( $format eq 'id' ) {
    foreach my $id ( @gene_id ) {
	print "$id\n";
    }
    exit(0);
}

if ($format eq 'webdump') {
    mkdir($webdir, 0777) or die "Can't create '$webdir' : $!";
}

$db->DESTROY;
$db=undef;

while ( @gene_id > 0 ) {
    my @chunk_list = splice(@gene_id,0,$chunk);

    if( $verbose ) {
	print STDERR "Fetching @chunk_list\n";
    }

    if ( $dbtype =~ 'timdb' ) {
	$db = Bio::EnsEMBL::TimDB::Obj->new('',$noacc,$test);
    } else {
	my $locator = "$module/host=$host;port=$port;dbname=$dbname;user=$dbuser;pass=$dbpass";
	$db =  Bio::EnsEMBL::DBLoader->new($locator);
	$db->static_golden_path_type('UCSC');
    }
    my @genes = $db->gene_Obj->get_array_supporting('none',@chunk_list);
    my $stgp = $db->get_StaticGoldenPathAdaptor();
    
    foreach my $gene ( @genes ) {
	my $gene_id = $gene->id();
	if( $format eq 'pep' ) {
	    foreach my $trans ( $gene->each_Transcript ) {
		# get out first exon. Tag it to clone and gene on this basis
		my @exon = $trans->each_Exon;
		my $fe = $exon[0];
		my $tseq = $trans->translate();
		if ( $tseq->seq =~ /\*/ ) {
		    print STDERR "translation has stop codons. Skipping! (in clone". $fe->clone_id .")\n";
		    next;
		}
		my ($chr,$bp) = $stgp->get_Gene_chr_bp($gene_id);
		$tseq->desc("Gene:$gene_id Clone:".$fe->clone_id . " Contig:" . $fe->contig_id . " Chr:".$chr." basepair:".$bp);
		$seqio->write_seq($tseq);
	    }
	} elsif ( $format eq 'dump' ) {
	    foreach my $trans ( $gene->each_Transcript ) {
		print "Transcript ",$trans->id,"\n";
		foreach my $exon ( $trans->each_Exon ) {
		    print "  Exon ",$exon->id," ",$exon->contig_id,":",$exon->start,"-",$exon->end,".",$exon->strand,"\n";
		    my $seq = $exon->seq();
		    my $str = $seq->seq();
		    print "    Start phase ",$exon->phase,"[",substr($str,0,10),"] End phase ",$exon->end_phase," [",substr($str,-10),"]\n";
		}
	    }
	    
	} 
	elsif ($format eq 'transcript') {
	    foreach my $trans ( $gene->each_Transcript ) {
		my $seq = $trans->dna_seq();
		$seq->id($trans->id);
		my @exon = $trans->each_Exon;
		my $fe = $exon[0];
		my ($chr,$bp) = $stgp->get_Gene_chr_bp($gene_id);
		$seq->desc("Gene:$gene_id Clone:".$fe->clone_id . " Contig:" . $fe->contig_id . " Chr:".$chr." basepair:".$bp);
		$seqio->write_seq($seq);
	    }
	}
	elsif ($format eq 'webdump') {
	    foreach my $trans ( $gene->each_Transcript ) {
		eval {
		    my $trans_file = $webdir.$trans->id.".trans";
		    open (TRANS,">$trans_file");
		    my $seqiot = Bio::SeqIO->new('-format' => 'Fasta' , -fh => \*TRANS );
		    my $seq = $trans->dna_seq();
		    print STDERR "dumping transcript",$trans->id,"\n";
		    $seq->display_id($trans->id);
		    my @exon = $trans->each_Exon;
		    my $fe = $exon[0];
		    my ($chr,$bp) = $stgp->get_Gene_chr_bp($gene_id);
		    $seq->desc("Gene:$gene_id Clone:".$fe->clone_id . " Contig:" . $fe->contig_id . " Chr:".$chr." basepair:".$bp);
		    $seqiot->write_seq($seq);
		    $seqiot=undef;
		    close (TRANS);
		    
		    my $pep_file =  $webdir.$trans->id.".pep";
		    open (PEP,">$pep_file");
		    my $seqiop = Bio::SeqIO->new('-format' => 'Fasta' , -fh => \*PEP) ;
		    my $tseq = $trans->translate();
		    if ( $tseq->seq =~ /\*/ ) {
			print STDERR "Skipping peptide dumping of ".$gene->id.", translation has stop codons. (in clone ". $fe->clone_id .")\n\n";
			next;
		    }
		    my ($chr,$bp) = $stgp->get_Gene_chr_bp($gene_id);
		    $tseq->desc("Gene:$gene_id Clone:".$fe->clone_id . " Contig:" . $fe->contig_id . " Chr:".$chr." basepair:".$bp);
		    $seqiop->write_seq($tseq);
		    $seqiop=undef;
		    close(PEP);
		};
		if ($@) {
		    print STDERR "Cannot dump gene $gene_id, transcript ".$trans->id.", exception: $@\n";
		}
	    }
	}
	else {
	    die "No valid format!";
	}
    }
    $db->DESTROY;
    $db=undef;
}





