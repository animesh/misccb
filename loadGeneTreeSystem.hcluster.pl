#!/usr/local/ensembl/bin/perl -w

use strict;
use DBI;
use Getopt::Long;
use Bio::EnsEMBL::Compara::DBSQL::DBAdaptor;
use Bio::EnsEMBL::Compara::GenomeDB;
use Bio::EnsEMBL::Hive;
use Bio::EnsEMBL::DBLoader;
use Bio::EnsEMBL::Registry;

my $conf_file;
my %analysis_template;
my @speciesList = ();
my %hive_params;
my %dnds_params;
my %sitewise_dnds_params;
my %genetree_params;

my %compara_conf = ();
#$compara_conf{'-user'} = 'ensadmin';
$compara_conf{'-port'} = 3306;

my ($help, $host, $user, $pass, $dbname, $port);
my $verbose;

GetOptions('help'     => \$help,
           'conf=s'   => \$conf_file,
           'dbhost=s' => \$host,
           'dbport=i' => \$port,
           'dbuser=s' => \$user,
           'dbpass=s' => \$pass,
           'dbname=s' => \$dbname,
           'v' => \$verbose,
          );

if ($help) { usage(); }

Bio::EnsEMBL::Registry->no_version_check(1);

parse_conf($conf_file);

if($host)   { $compara_conf{'-host'}   = $host; }
if($port)   { $compara_conf{'-port'}   = $port; }
if($dbname) { $compara_conf{'-dbname'} = $dbname; }
if($user)   { $compara_conf{'-user'}   = $user; }
if($pass)   { $compara_conf{'-pass'}   = $pass; }


unless(defined($compara_conf{'-host'}) and defined($compara_conf{'-user'}) and defined($compara_conf{'-dbname'})) {
  print "\nERROR : must specify host, user, and database to connect to compara\n\n";
  usage();
}

if(%analysis_template and (not(-d $analysis_template{'fasta_dir'}))) {
  die("\nERROR!!\n  ". $analysis_template{'fasta_dir'} . " fasta_dir doesn't exist, can't configure\n");
}

# ok this is a hack, but I'm going to pretend I've got an object here
# by creating a blessed hash ref and passing it around like an object
# this is to avoid using global variables in functions, and to consolidate
# the globals into a nice '$self' package
my $self = bless {};

$self->{'comparaDBA'}   = new Bio::EnsEMBL::Compara::DBSQL::DBAdaptor(%compara_conf);
$self->{gdba}   = $self->{'comparaDBA'}->get_GenomeDBAdaptor();
$self->{'hiveDBA'}      = new Bio::EnsEMBL::Hive::DBSQL::DBAdaptor(-DBCONN => $self->{'comparaDBA'}->dbc);

if(%hive_params) {
  if(defined($hive_params{'hive_output_dir'})) {
    die("\nERROR!! hive_output_dir doesn't exist, can't configure\n  ", $hive_params{'hive_output_dir'} , "\n")
      if(($hive_params{'hive_output_dir'} ne "") and !(-d $hive_params{'hive_output_dir'}));
    $self->{'comparaDBA'}->get_MetaContainer->delete_key('hive_output_dir');
    $self->{'comparaDBA'}->get_MetaContainer->store_key_value('hive_output_dir', $hive_params{'hive_output_dir'});
  }
}

$self->build_GeneTreeSystem();

exit(0);

#######################
#
# subroutines
#
#######################

sub usage {
  print "loadGeneTreeSystem.pl [options]\n";
  print "  -help                  : print this help\n";
  print "  -conf <path>           : config file describing compara, templates\n";
  print "loadGeneTreeSystem.pl v1.3\n";

  exit(1);
}

sub parse_conf {
  my($conf_file) = shift;

  if($conf_file and (-e $conf_file)) {
    #read configuration file from disk
    my @conf_list = @{do $conf_file};

    foreach my $confPtr (@conf_list) {
      print("HANDLE type " . $confPtr->{TYPE} . "\n") if($verbose);
      if($confPtr->{TYPE} eq 'COMPARA') {
        %compara_conf = %{$confPtr};
      }
      if(($confPtr->{TYPE} eq 'BLAST_TEMPLATE') or ($confPtr->{TYPE} eq 'BLASTP_TEMPLATE')) {
        %analysis_template = %{$confPtr};
      }
      if($confPtr->{TYPE} eq 'HIVE') {
        %hive_params = %{$confPtr};
      }
      if($confPtr->{TYPE} eq 'dNdS') {
        %dnds_params = %{$confPtr};
      }
      if($confPtr->{TYPE} eq 'sitewise_dNdS') {
        %sitewise_dnds_params = %{$confPtr};
      }
      if($confPtr->{TYPE} eq 'SPECIES') {
        push @speciesList, $confPtr;
      }
      if($confPtr->{TYPE} eq 'GENE_TREE') {
        %genetree_params = %{$confPtr};
      }
    }
  }
}
#
# need to make sure analysis 'SubmitGenome' is in database
# this is a generic analysis of type 'genome_db_id'
# the input_id for this analysis will be a genome_db_id
# the full information to access the genome will be in the compara database
# also creates 'GenomeLoadMembers' analysis and
# 'GenomeDumpFasta' analysis in the 'genome_db_id' chain

sub build_GeneTreeSystem
{
  #yes this should be done with a config file and a loop, but...
  my $self = shift;

  my $dataflowRuleDBA = $self->{'hiveDBA'}->get_DataflowRuleAdaptor;
  my $ctrlRuleDBA = $self->{'hiveDBA'}->get_AnalysisCtrlRuleAdaptor;
  my $analysisStatsDBA = $self->{'hiveDBA'}->get_AnalysisStatsAdaptor;
  my $analysisDBA = $self->{'hiveDBA'}->get_AnalysisAdaptor;
  my $stats;

  #
  # SubmitGenome
  #
  my $submit_analysis = Bio::EnsEMBL::Analysis->new(
      -db_version      => '1',
      -logic_name      => 'SubmitGenome',
      -input_id_type   => 'genome_db_id',
      -module          => 'Bio::EnsEMBL::Hive::RunnableDB::Dummy'
    );
  $analysisDBA->store($submit_analysis);
  $stats = $analysisStatsDBA->fetch_by_analysis_id($submit_analysis->dbID);
  $stats->batch_size(100);
  $stats->hive_capacity(-1);
  $stats->update();

  return $submit_analysis
    unless($analysis_template{fasta_dir});

  #
  # GenomeLoadMembers
  #
  my $load_genome = Bio::EnsEMBL::Analysis->new(
      -db_version      => '1',
      -logic_name      => 'GenomeLoadMembers',
      -module          => 'Bio::EnsEMBL::Compara::RunnableDB::GenomeLoadMembers'
    );
  $analysisDBA->store($load_genome);
  $stats = $analysisStatsDBA->fetch_by_analysis_id($load_genome->dbID);
  $stats->batch_size(1);
  $stats->hive_capacity(-1); #unlimited
  $stats->update();

  $dataflowRuleDBA->create_rule($submit_analysis, $load_genome);

  #
  # LoadUniProt
  #
  my $loadUniProt = Bio::EnsEMBL::Analysis->new(
        -db_version      => '1',
        -logic_name      => 'LoadUniProt',
        -module          => 'Bio::EnsEMBL::Compara::RunnableDB::LoadUniProt',
      );
  $analysisDBA->store($loadUniProt);
  $stats = $analysisStatsDBA->fetch_by_analysis_id($loadUniProt->dbID);
  $stats->batch_size(1);
  $stats->hive_capacity(-1);
  $stats->status('LOADING');
  $stats->update();


  #
  # BlastSubsetStaging
  #
  my $blastSubsetStaging = Bio::EnsEMBL::Analysis->new(
      -db_version      => '1',
      -logic_name      => 'BlastSubsetStaging',
      -module          => 'Bio::EnsEMBL::Hive::RunnableDB::Dummy'
    );
  $analysisDBA->store($blastSubsetStaging);
  $stats = $analysisStatsDBA->fetch_by_analysis_id($blastSubsetStaging->dbID);
  $stats->batch_size(100);
  $stats->hive_capacity(-1);
  $stats->update();

  $dataflowRuleDBA->create_rule($load_genome, $blastSubsetStaging);
  $dataflowRuleDBA->create_rule($loadUniProt, $blastSubsetStaging, 2);

  #
  # GenomeSubmitPep
  #
  my $submitpep_analysis = Bio::EnsEMBL::Analysis->new(
      -db_version      => '1',
      -logic_name      => 'GenomeSubmitPep',
      -input_id_type   => 'genome_db_id',
      -module          => 'Bio::EnsEMBL::Compara::RunnableDB::GenomeSubmitPep'
    );
  $analysisDBA->store($submitpep_analysis);
  $stats = $analysisStatsDBA->fetch_by_analysis_id($submitpep_analysis->dbID);
  $stats->batch_size(1);
  $stats->hive_capacity(3);
  $stats->update();

  $dataflowRuleDBA->create_rule($blastSubsetStaging, $submitpep_analysis);

  # GenomeDumpFasta does not use the normal eval method of parameter loading so this is why
  # our hash lookie-like structure is not a real evallable hash
  my @dump_fasta_params = ("fasta_dir => $analysis_template{fasta_dir}");
  my $blast_hive_capacity = $hive_params{'blast_hive_capacity'};
  push(@dump_fasta_params, "blast_hive_capacity => ${blast_hive_capacity}") if defined $blast_hive_capacity;
  my $dump_fasta_params_str = join(',', @dump_fasta_params);

  #
  # GenomeDumpFasta
  #
  my $dumpfasta_analysis = Bio::EnsEMBL::Analysis->new(
      -db_version      => '1',
      -logic_name      => 'GenomeDumpFasta',
      -input_id_type   => 'genome_db_id',
      -module          => 'Bio::EnsEMBL::Compara::RunnableDB::GenomeDumpFasta',
      -parameters      => $dump_fasta_params_str,
    );
  $analysisDBA->store($dumpfasta_analysis);
  $stats = $analysisStatsDBA->fetch_by_analysis_id($dumpfasta_analysis->dbID);
  $stats->batch_size(1);
  $stats->hive_capacity(-1);
  $stats->update();

  $dataflowRuleDBA->create_rule($blastSubsetStaging, $dumpfasta_analysis);

  #
  # CreateBlastRules
  #
#  my $parameters = "{phylumBlast=>0, selfBlast=>1,cr_analysis_logic_name=>'BuildHomology'}";
#  my $parameters = "{phylumBlast=>0, selfBlast=>1,cr_analysis_logic_name=>'UpdatePAFIds'}";
  my $parameters = "{phylumBlast=>0, selfBlast=>1,cr_analysis_logic_name=>'CreateHclusterPrepareJobs'}";
  my $blastrules_analysis = Bio::EnsEMBL::Analysis->new(
      -db_version      => '1',
      -logic_name      => 'CreateBlastRules',
      -input_id_type   => 'genome_db_id',
      -module          => 'Bio::EnsEMBL::Compara::RunnableDB::CreateBlastRules',
      -parameters      => $parameters
    );
  $analysisDBA->store($blastrules_analysis);
  $stats = $analysisStatsDBA->fetch_by_analysis_id($blastrules_analysis->dbID);
  $stats->batch_size(1);
  $stats->hive_capacity(1);
  $stats->status('BLOCKED');
  $stats->update();

  $ctrlRuleDBA->create_rule($load_genome, $blastrules_analysis);
  $ctrlRuleDBA->create_rule($blastSubsetStaging, $blastrules_analysis);
  $ctrlRuleDBA->create_rule($submitpep_analysis, $blastrules_analysis);
  $ctrlRuleDBA->create_rule($dumpfasta_analysis, $blastrules_analysis);

  $dataflowRuleDBA->create_rule($dumpfasta_analysis, $blastrules_analysis);

  #
  # blast_template
  #
  # create an unlinked analysis called blast_template
  # it will not have rules so it will never execute
  # used to store module,parameters... to be used as template for
  # the dynamic creation of the analyses like blast_1_NCBI34

  #Have to generate new parameters & not use analysis_template as
  #that now has keys which are not prefixed by a -
  #Since hash order is unpredictable this causes problems with rearrange
  my %new_params = map { $_ => $analysis_template{$_} } grep {$_ =~ /^-/} keys(%analysis_template);

  my $blast_template = new Bio::EnsEMBL::Analysis(%new_params);
  $blast_template->logic_name("blast_template");
  my $blast_template_analysis_data_id =
    $self->{'hiveDBA'}->get_AnalysisDataAdaptor->store_if_needed($blast_template->parameters);
  $parameters = undef;
  if (defined $blast_template_analysis_data_id) {
    $parameters = "{'blast_template_analysis_data_id'=>'$blast_template_analysis_data_id'}";
    $blast_template->parameters($parameters);
  }
  eval { $analysisDBA->store($blast_template); };

  #
  # Create peptide_align_feature per-species tables
  #
  foreach my $speciesPtr (@speciesList) {
    my $gdb_id = $speciesPtr->{'genome_db_id'};
    my $gdb = $self->{gdba}->fetch_by_dbID($gdb_id)
        || die( "Cannot fetch_by_dbID genome_db $gdb_id" );
    my $species_name = lc($gdb->name);
    $species_name =~ s/\ /\_/g;
    my $tbl_name = "peptide_align_feature"."_"."$species_name"."_"."$gdb_id";
    my $sql = "CREATE TABLE IF NOT EXISTS $tbl_name like peptide_align_feature";

    #print("$sql\n");
    my $sth = $self->{'comparaDBA'}->dbc->prepare($sql);
    $sth->execute();
  }

  # CreateHclusterPrepareJobs
  #
  # FIXME
  $parameters = $genetree_params{'cluster_params'};
  $parameters =~ s/\A{//;
  $parameters =~ s/}\Z//;
  $parameters = '{' . $parameters . ",fasta_dir=>'" . $analysis_template{fasta_dir} . "'}";

  my $CreateHclusterPrepareJobs = Bio::EnsEMBL::Analysis->new(
      -db_version      => '1',
      -logic_name      => 'CreateHclusterPrepareJobs',
      -module          => 'Bio::EnsEMBL::Compara::RunnableDB::CreateHclusterPrepareJobs',
      -parameters      => $parameters
  );
  $analysisDBA->store($CreateHclusterPrepareJobs);

  if(defined($self->{'hiveDBA'})) {
    my $stats = $analysisStatsDBA->fetch_by_analysis_id($CreateHclusterPrepareJobs->dbID);
    $stats->batch_size(1);
    $stats->hive_capacity(-1);
    $stats->status('BLOCKED');
    $stats->update();
#     $ctrlRuleDBA->create_rule($orthotree,$CreateHclusterPrepareJobs);
#     $ctrlRuleDBA->create_rule($njtree_phyml,$CreateHclusterPrepareJobs);
#     $ctrlRuleDBA->create_rule($muscle,$CreateHclusterPrepareJobs);
#     $ctrlRuleDBA->create_rule($BreakPAFCluster,$CreateHclusterPrepareJobs);
  }

  #
  # HclusterPrepare
  #
  $parameters = $genetree_params{'cluster_params'};
  $parameters =~ s/\A{//;
  $parameters =~ s/}\Z//;
  $parameters = '{' . $parameters . ",fasta_dir=>'" . $analysis_template{fasta_dir} . "'}";
  my $hclusterprepare = Bio::EnsEMBL::Analysis->new(
      -logic_name      => 'HclusterPrepare',
      -module          => 'Bio::EnsEMBL::Compara::RunnableDB::HclusterPrepare',
      -parameters      => $parameters
    );
  $analysisDBA->store($hclusterprepare);
  $stats = $hclusterprepare->stats;
  $stats->batch_size(10);
  $stats->hive_capacity(4);
  $stats->status('BLOCKED');
  $stats->update();

  #
  # HclusterRun
  #
  $parameters = $genetree_params{'cluster_params'};
  $parameters =~ s/\A{//;
  $parameters =~ s/}\Z//;
  $parameters = '{' . $parameters . ",fasta_dir=>'" . $analysis_template{fasta_dir} . "'}";
  my $hclusterrun = Bio::EnsEMBL::Analysis->new(
      -logic_name      => 'HclusterRun',
      -module          => 'Bio::EnsEMBL::Compara::RunnableDB::HclusterRun',
      -parameters      => $parameters
    );

  if(exists $genetree_params{hcluster_sg}) {
  	$hclusterrun->program_file($genetree_params{hcluster_sg});
  }

  $analysisDBA->store($hclusterrun);
  $stats = $hclusterrun->stats;
  $stats->batch_size(1);
  $stats->hive_capacity(-1);
  $stats->status('BLOCKED');
  $stats->update();

  #
  # Clusterset_staging
  #
  my $clusterset_staging = Bio::EnsEMBL::Analysis->new(
      -logic_name      => 'Clusterset_staging',
      -module          => 'Bio::EnsEMBL::Hive::RunnableDB::Dummy',
    );
  $analysisDBA->store($clusterset_staging);
  $stats = $clusterset_staging->stats;
  $stats->batch_size(-1);
  $stats->hive_capacity(1);
  $stats->update();

  #
  # Mcoffee
  #
  $parameters = "{'method'=>'cmcoffee'";
  if (defined $genetree_params{'max_gene_count'}) {
    $parameters .= ",'max_gene_count'=>".$genetree_params{'max_gene_count'};
  }
  if (defined $genetree_params{'honeycomb_dir'}) {
    $parameters .= ",'honeycomb_dir'=>'".$genetree_params{'honeycomb_dir'}."'";
  }
  if(defined $genetree_params{mafft}) {
  	$parameters .= qq|, 'mafft' => '$genetree_params{mafft}'|;
  }
  $parameters .= "}";

  my $mcoffee_exe = $genetree_params{'mcoffee'} || '/nfs/acari/gj1/bin/t_coffee';

  my $mcoffee = Bio::EnsEMBL::Analysis->new(
      -logic_name      => 'MCoffee',
      -program_file    => $mcoffee_exe,
      -module          => 'Bio::EnsEMBL::Compara::RunnableDB::MCoffee'
    );

  #If params exceed 254 then use the analysis_data table.
  if(length($parameters) > 254) {
  	my $ad_dba =  $self->{'hiveDBA'}->get_AnalysisDataAdaptor();
   	my $adi = $ad_dba->store_if_needed($parameters);
   	$parameters = "{'analysis_data_id'=>${adi}}";
  }

  $mcoffee->parameters($parameters);

  $analysisDBA->store($mcoffee);
  $stats = $mcoffee->stats;
  $stats->batch_size(1);
  $stats->hive_capacity(-1);
  $stats->update();

#   #
#   # Muscle
#   #
#   $parameters = "{'options'=>'-maxhours 5'";
#   if (defined $genetree_params{'max_gene_count'}) {
#     $parameters .= ",'max_gene_count'=>".$genetree_params{'max_gene_count'};
#   }
#   if (defined $genetree_params{'honeycomb_dir'}) {
#     $parameters .= ",'honeycomb_dir'=>'".$genetree_params{'honeycomb_dir'}."'";
#   }
#   $parameters .= "}";

#   my $muscle_exe = $genetree_params{'muscle'} || '/usr/local/ensembl/bin/muscle';

#   my $muscle = Bio::EnsEMBL::Analysis->new(
#       -logic_name      => 'Muscle',
#       -program_file    => $muscle_exe,
#       -module          => 'Bio::EnsEMBL::Compara::RunnableDB::Muscle',
#       -parameters      => $parameters
#     );
#   $analysisDBA->store($muscle);
#   $stats = $muscle->stats;
#   $stats->batch_size(1);
#   $stats->hive_capacity(-1);
#   $stats->update();

  #
  # NJTREE_PHYML
  #
  $parameters = "{cdna=>1,bootstrap=>1";
  if (defined $genetree_params{'max_gene_count'}) {
    $parameters .= ",max_gene_count=>".$genetree_params{'max_gene_count'};
  }
  if ($genetree_params{'species_tree_file'}){
    $parameters .= ",'species_tree_file'=>'". $genetree_params{'species_tree_file'}."'";
  } else {
    warn("No species_tree_file => 'myfile' has been set in your config file. "
         ."This parameter can not be set for njtree. EXIT 3\n");
    exit(3);
  }
  if (defined $genetree_params{'honeycomb_dir'}) {
    $parameters .= ",'honeycomb_dir'=>'".$genetree_params{'honeycomb_dir'}."'";
  }

  $parameters .= ", 'use_genomedb_id'=>1" if defined $genetree_params{use_genomedb_id};

  $parameters .= "}";
  my $njtree_phyml_analysis_data_id = $self->{'hiveDBA'}->get_AnalysisDataAdaptor->store_if_needed($parameters);
  if (defined $njtree_phyml_analysis_data_id) {
    $parameters = "{'njtree_phyml_analysis_data_id'=>'$njtree_phyml_analysis_data_id'}";
  }
  my $tree_best_program = $genetree_params{'treebest'} || '/nfs/acari/avilella/src/treesoft/trunk/treebest/treebest';
  my $njtree_phyml = Bio::EnsEMBL::Analysis->new(
      -logic_name      => 'NJTREE_PHYML',
      -program_file    => $tree_best_program,
      -module          => 'Bio::EnsEMBL::Compara::RunnableDB::NJTREE_PHYML',
      -parameters      => $parameters
    );
  $analysisDBA->store($njtree_phyml);
  $stats = $njtree_phyml->stats;
  $stats->batch_size(1);
  my $njtree_hive_capacity = $hive_params{'njtree_hive_capacity'};
  $njtree_hive_capacity = 400 unless defined $njtree_hive_capacity;
  $stats->hive_capacity($njtree_hive_capacity);
  $stats->update();

  #
  # Change $dnds_params{'method_link_type'} to {'method_link_types'}
  #
  if( defined( $dnds_params{'method_link_type'} ) ){
    warn('[WARN] dNdS => method_link_type is deprecated. '
         .'Use method_link_types instead');
    $dnds_params{'method_link_types'}
      ||= '['. $dnds_params{'method_link_type'} . ']';
  }

  #
  # OrthoTree
  #
  my $with_options_orthotree = 0;
  my $ortho_params = '';
  if (defined $genetree_params{'honeycomb_dir'}) {
    $ortho_params = "'honeycomb_dir'=>'".$genetree_params{'honeycomb_dir'}."'";
    $with_options_orthotree = 1;
  }
  if (defined $dnds_params{'species_sets'}) {
    $ortho_params .= ',species_sets=>' . $dnds_params{'species_sets'};
    if( defined $dnds_params{'method_link_types'} ){
      $ortho_params .= ',method_link_types=>'
          . $dnds_params{'method_link_types'};
    }
    $with_options_orthotree = 1;
  }
  if(defined $genetree_params{'species_tree_file'}) {
    my $tree_file = $genetree_params{'species_tree_file'};
    $ortho_params .= ",'species_tree_file'=>'${tree_file}'";
    $with_options_orthotree = 1;
  }

  $ortho_params .= ", 'use_genomedb_id'=>1" if defined $genetree_params{use_genomedb_id};

  #EDIT Originally created a anon hash which caused problems with OrthoTree when using eval
  if($with_options_orthotree) {
    $parameters =~ s/\A{//;
    $parameters =~ s/}\Z//;
    $parameters = '{' . $parameters . ',' .  $ortho_params . '}'
  }

  my $analysis_data_id = $self->{'hiveDBA'}->get_AnalysisDataAdaptor->store_if_needed($parameters);
  if (defined $analysis_data_id) {
    $parameters = "{'analysis_data_id'=>'$analysis_data_id'}";
  }

  my $orthotree = Bio::EnsEMBL::Analysis->new(
      -logic_name      => 'OrthoTree',
      -module          => 'Bio::EnsEMBL::Compara::RunnableDB::OrthoTree',
      -parameters      => $parameters
      );
  $analysisDBA->store($orthotree);
  $stats = $orthotree->stats;
  $stats->batch_size(1);
  my $ortho_tree_hive_capacity = $hive_params{'ortho_tree_hive_capacity'};
  $ortho_tree_hive_capacity = 200 unless defined $ortho_tree_hive_capacity;
  $stats->hive_capacity($ortho_tree_hive_capacity);

  $stats->update();

  #
  # QuickTreeBreak
  #
  $parameters = "{max_gene_count=>".$genetree_params{'max_gene_count'};
  if($genetree_params{sreformat}) {
  	$parameters .= qq|, sreformat_exe=>'$genetree_params{sreformat}'|;
  }
  $parameters .= '}';

  my $quicktreebreak = Bio::EnsEMBL::Analysis->new(
      -logic_name      => 'QuickTreeBreak',
      -module          => 'Bio::EnsEMBL::Compara::RunnableDB::QuickTreeBreak',
      -parameters      => $parameters
      );
  $quicktreebreak->program_file($genetree_params{quicktree}) if $genetree_params{quicktree};
  $analysisDBA->store($quicktreebreak);
  $stats = $quicktreebreak->stats;
  $stats->batch_size(1);
  my $quicktreebreak_hive_capacity = 10;
  $stats->hive_capacity($quicktreebreak_hive_capacity);

  $stats->update();


  # turn these two on if you need dnds from the old homology system
  #
  # CreateHomology_dNdSJob
  #
  my $CreateHomology_dNdSJob = Bio::EnsEMBL::Analysis->new(
      -db_version      => '1',
      -logic_name      => 'CreateHomology_dNdSJob',
      -module          => 'Bio::EnsEMBL::Compara::RunnableDB::CreateHomology_dNdSJobs',
  );
  $analysisDBA->store($CreateHomology_dNdSJob);

  if(defined($self->{'hiveDBA'})) {
    my $stats = $analysisStatsDBA->fetch_by_analysis_id($CreateHomology_dNdSJob->dbID);
    $stats->batch_size(1);
    $stats->hive_capacity(-1);
    $stats->status('BLOCKED');
    $stats->update();
    $ctrlRuleDBA->create_rule($orthotree,$CreateHomology_dNdSJob);
    $ctrlRuleDBA->create_rule($njtree_phyml,$CreateHomology_dNdSJob);
    $ctrlRuleDBA->create_rule($mcoffee,$CreateHomology_dNdSJob);
#    $ctrlRuleDBA->create_rule($BreakPAFCluster,$CreateHomology_dNdSJob);
  }
  if (defined $dnds_params{'species_sets'}) {
    $self->{'hiveDBA'}->get_AnalysisJobAdaptor->CreateNewJob
        (
         -input_id       => ( '{species_sets=>'
                              . $dnds_params{'species_sets'}
#                               . ',method_link_types=>'
#                               . $dnds_params{'method_link_types'}
                              . '}' ),
         -analysis       => $CreateHomology_dNdSJob,
        );
  }

  #
  # Homology_dNdS
  #
  my $homology_dNdS = Bio::EnsEMBL::Analysis->new(
      -db_version      => '1',
      -logic_name      => 'Homology_dNdS',
      -module          => 'Bio::EnsEMBL::Compara::RunnableDB::Homology_dNdS'
  );
  $self->store_codeml_parameters(\%dnds_params);
  if (defined $dnds_params{'dNdS_analysis_data_id'}) {
    $homology_dNdS->parameters('{dNdS_analysis_data_id=>' . $dnds_params{'dNdS_analysis_data_id'} . '}');
  }
  $analysisDBA->store($homology_dNdS);
  if(defined($self->{'hiveDBA'})) {
    my $stats = $analysisStatsDBA->fetch_by_analysis_id($homology_dNdS->dbID);
    $stats->batch_size(10);
    my $homology_dnds_hive_capacity = $hive_params{homology_dnds_hive_capacity};
  	$homology_dnds_hive_capacity = 200 unless defined $homology_dnds_hive_capacity;
  	$stats->hive_capacity($homology_dnds_hive_capacity);
    $stats->status('BLOCKED');
    $stats->update();
    $ctrlRuleDBA->create_rule($CreateHomology_dNdSJob,$homology_dNdS);
  }

  #
  # Threshold_on_dS
  #
  my $threshold_on_dS = Bio::EnsEMBL::Analysis->new(
      -db_version      => '1',
      -logic_name      => 'Threshold_on_dS',
      -module          => 'Bio::EnsEMBL::Compara::RunnableDB::Threshold_on_dS'
  );
  $analysisDBA->store($threshold_on_dS);

  if(defined($self->{'hiveDBA'})) {
    my $stats = $analysisStatsDBA->fetch_by_analysis_id($threshold_on_dS->dbID);
    $stats->batch_size(1);
    $stats->hive_capacity(-1);
    $stats->status('BLOCKED');
    $stats->update();
    $ctrlRuleDBA->create_rule($homology_dNdS,$threshold_on_dS);
  }
  if (defined $dnds_params{'species_sets'}) {
    $self->{'hiveDBA'}->get_AnalysisJobAdaptor->CreateNewJob
        (
         -input_id       => ( '{species_sets=>'
                              . $dnds_params{'species_sets'}
#                               . ',method_link_types=>'
#                               . $dnds_params{'method_link_types'}
                              . '}' ),
         -analysis       => $threshold_on_dS,
        );
  }

  #
  # Sitewise_dNdS
  #

  if (defined $sitewise_dnds_params{'saturated'}) {
    $parameters = '';
    my $with_options_sitewise_dnds = 0;
    if (defined $genetree_params{'honeycomb_dir'}) {
      $parameters = "'honeycomb_dir'=>'".$genetree_params{'honeycomb_dir'}."',";
      $with_options_sitewise_dnds = 1;
    }
    if (defined $sitewise_dnds_params{'saturated'}) {
      $parameters .= "'saturated'=>" . $sitewise_dnds_params{'saturated'};
      $with_options_sitewise_dnds = 1;
    }
    if (defined $sitewise_dnds_params{gblocks}) {
    	$parameters .= q{'gblocks_exe'=>} . $sitewise_dnds_params{'gblocks'};
    	$with_options_sitewise_dnds = 1;
    }
    $parameters = '{' . $parameters .'}' if (1==$with_options_sitewise_dnds);

    my $Sitewise_dNdS = Bio::EnsEMBL::Analysis->new
      (
       -db_version      => '1',
       -logic_name      => 'Sitewise_dNdS',
       -module          => 'Bio::EnsEMBL::Compara::RunnableDB::Sitewise_dNdS',
       -program_file    => $sitewise_dnds_params{'program_file'} || ''
      );

    #If params exceed 254 then use the analysis_data table.
    if(length($parameters) > 254) {
    	my $ad_dba =  $self->{'hiveDBA'}->get_AnalysisDataAdaptor();
    	my $adi = $ad_dba->store_if_needed($parameters);
    	$parameters = "{'analysis_data_id'=>${adi}}";
    }
    $Sitewise_dNdS->parameters($parameters);

    $analysisDBA->store($Sitewise_dNdS);

    if(defined($self->{'hiveDBA'})) {
      my $stats = $analysisStatsDBA->fetch_by_analysis_id($Sitewise_dNdS->dbID);
      $stats->batch_size(1);
      $stats->hive_capacity(600);
      $stats->status('BLOCKED');
      $stats->update();
      $ctrlRuleDBA->create_rule($orthotree,$Sitewise_dNdS);
    }

    # When a Sitewise_dNdS job is saturated, we reincorporate the
    # subtrees in the analysis to rerun them again
    $dataflowRuleDBA->create_rule($orthotree, $Sitewise_dNdS, 1);
    # Saturated jobs create new jobs of the same kind
    $dataflowRuleDBA->create_rule($Sitewise_dNdS, $Sitewise_dNdS, 2);
  }
  #
  # build graph of control and dataflow rules
  #

  # $ctrlRuleDBA->create_rule($updatepafids_analysis, $paf_cluster);
  $ctrlRuleDBA->create_rule($blastrules_analysis, $CreateHclusterPrepareJobs);
  $ctrlRuleDBA->create_rule($CreateHclusterPrepareJobs, $hclusterprepare);
  $ctrlRuleDBA->create_rule($hclusterprepare, $hclusterrun);
#   $dataflowRuleDBA->create_rule($hclusterprepare, $hclusterrun, 1);
#   $dataflowRuleDBA->create_rule($paf_cluster, $clusterset_staging, 1);
#   $dataflowRuleDBA->create_rule($paf_cluster, $mcoffee, 2);
#   $dataflowRuleDBA->create_rule($paf_cluster, $BreakPAFCluster, 3);

  $dataflowRuleDBA->create_rule($hclusterrun, $clusterset_staging, 1);
  $dataflowRuleDBA->create_rule($hclusterrun, $mcoffee, 2);
#  $dataflowRuleDBA->create_rule($hclusterrun, $BreakPAFCluster, 3);

  $dataflowRuleDBA->create_rule($mcoffee, $njtree_phyml, 1);
#  $dataflowRuleDBA->create_rule($mcoffee, $BreakPAFCluster, 2);

  # Failing small seqnum jobs create new Jackknife jobs of the same kind
  $dataflowRuleDBA->create_rule($njtree_phyml, $njtree_phyml, 2);
  $dataflowRuleDBA->create_rule($njtree_phyml, $quicktreebreak, 3);
  $dataflowRuleDBA->create_rule($orthotree, $quicktreebreak, 2);
  $dataflowRuleDBA->create_rule($quicktreebreak, $mcoffee, 1);
  $dataflowRuleDBA->create_rule($njtree_phyml, $orthotree, 1);

#  $dataflowRuleDBA->create_rule($njtree_phyml, $BreakPAFCluster, 2);

#  $dataflowRuleDBA->create_rule($BreakPAFCluster, $mcoffee, 2);
#  $dataflowRuleDBA->create_rule($BreakPAFCluster, $BreakPAFCluster, 3);

  #
  # create initial job
  #

  Bio::EnsEMBL::Hive::DBSQL::AnalysisJobAdaptor->CreateNewJob
    (
     -input_id       => 1,
     -analysis       => $CreateHclusterPrepareJobs,
    );

   Bio::EnsEMBL::Hive::DBSQL::AnalysisJobAdaptor->CreateNewJob
       (
        -input_id       => 1,
 #       -analysis       => $paf_cluster,
        -analysis       => $hclusterrun,
       );
  return 1;
}

sub store_codeml_parameters
{
  my $self = shift;
  my $dNdS_Conf = shift;

  my $options_hash_ref = $dNdS_Conf->{'codeml_parameters'};
  return unless($options_hash_ref);

  my @keys = keys %{$options_hash_ref};
  my $options_string = "{\n";
  foreach my $key (@keys) {
    $options_string .= "'$key'=>'" . $options_hash_ref->{$key} . "',\n";
  }
  $options_string .= "}";

  $dNdS_Conf->{'dNdS_analysis_data_id'} =
         $self->{'hiveDBA'}->get_AnalysisDataAdaptor->store_if_needed($options_string);

  $dNdS_Conf->{'codeml_parameters'} = undef;
}


