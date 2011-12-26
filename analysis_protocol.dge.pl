#!/usr/bin/perl -W

##############################################################
 # These materials (including  without limitation all articles,
 #  text, images, logos, software, and designs) are copyright 
 # © 2006,2007,2008 Helicos BioSciences Corporation.  All rights 
 # reserved.
 #
 # This program is free software; you can redistribute it and/or
 # modify it under the terms of the GNU General Public License
 # as published by the Free Software Foundation version 2
 # of the License
 # This program is distributed in the hope that it will be useful,
 # but WITHOUT ANY WARRANTY; without even the implied warranty of
 # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 # GNU General Public License for more details.
 #
 # You should have received a copy of the GNU General Public License
 # along with this program; if not, write to the Free Software
 # Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  
 # 02110-1301, USA.
#################################################################

# ================================================================================
# FILE:          analysis_protocol.dge.pl
# DESCRIPTION:   
# AUTHORS:       Steven Roels
# ================================================================================

use 5.008;

use strict;
use vars qw( $REVISION $VERSION) ;
use lib qw(/gpfs2/bioinf/perl/external/lib/perl5/site_perl/5.8.5); #required for MLDBM

use Getopt::Long;
use IO::File;
use Fcntl;
use File::Path qw(mkpath rmtree);
use File::Copy;
use File::Basename;
use File::Temp;
use Data::Dumper;
use Cwd qw(chdir getcwd abs_path);
use MLDBM qw(DB_File Storable);     # DB_File and Storable
use Proc::Background;

use Helicos::Util qw(execute_command min max);
use Helicos::Util::ValueTypes qw(is_non_negative_integer is_positive_integer is_number);
use Helicos::Exception::Classes;
use Helicos::MetaData::Classes;

use Helicos::SequenceAnalysis::Pipeline::Environment;
use Helicos::SequenceAnalysis::Pipeline::Util qw(output_header get_next_fasta_record get_channel_blocks timestamped_string formatted_datetime timing_string);
use Helicos::SequenceAnalysis::Pipeline::FilterProfile;
use Helicos::SequenceAnalysis::Pipeline::GrowthProfile;
use Helicos::SequenceAnalysis::Pipeline::ErrorProfile;
use Helicos::SequenceAnalysis::Pipeline::YieldProfile;
use Helicos::SequenceAnalysis::Pipeline::Tools::IndexDP::Job;

$REVISION = "";
$VERSION = "1.0.0";

$| = 1;

my $help = 0;
my $verbose = 0;
my $debug = 0;

my $launch_dir = getcwd();

my $HELICOS_ANALYSIS_HOME = Helicos::SequenceAnalysis::Pipeline::Environment->get_value("HELICOS_ANALYSIS_HOME");
my $HELICOS_ANALYSIS_WORKSPACE_ROOT = Helicos::SequenceAnalysis::Pipeline::Environment->get_value("HELICOS_ANALYSIS_WORKSPACE_ROOT");

#============================
# Hardcoded Defaults

my $base_addition_order_id          = 'BASE_ADDITION_ORDER_REFERENCE';
my $schema_dir                      = join("/",${HELICOS_ANALYSIS_HOME},'/config/descriptors/schema');
my $indexdp_template_repository     = join("/",${HELICOS_ANALYSIS_HOME},'/config/tools/indexDP');
my $indexDP_configuration_directory = join("/",${HELICOS_ANALYSIS_HOME},'config/tools/indexDP');
my $countDGE_score_probability_file = join("/",${HELICOS_ANALYSIS_HOME},'config/tools/countDGE/score.def');

my $max_retry=0;
my $use_sge=0;
my $max_processes=1;

my %params = ( sms_file                         => undef,
	       config_dbfile                    => undef,
	       assignment_file                  => undef,
	       use_sge                          => undef,
	       max_processes                    => undef,
	       max_retry                        => undef );

my %OPTIONS = (
	       'verbose=i'                => \$verbose,
	       'debug=i'                  => \$debug,
	       'help'                     => \$help,
	      );

foreach my $paramkey (keys %params) {
    $OPTIONS{join("",$paramkey,"=s")}=\$params{$paramkey};
}

unless (&GetOptions(%OPTIONS)) {
    die "FATAL: invalid options or option values\n\n",&_usage();
}

if ($help) { print &_usage(); exit; }

#$params{max_processes}=$ENV{MAX_DGE_PROCESSES};

foreach my $paramkey (qw(sms_file config_dbfile assignment_file)) {
    unless (defined($params{$paramkey})) {
	Helicos::Exception::Simple->throw("parameter <$paramkey> is required\n");
    }
}

if (defined($params{use_sge})) {
    unless ($params{use_sge}=~/^(0|1)$/) { Helicos::Exception::Simple->throw("invalid value <$params{use_sge}> for parameter <use_sge>\n"); }
    $use_sge=$params{use_sge};
}

if (defined($params{max_processes})) {
    unless ($params{max_processes}=~/^\d+$/ and $params{max_processes}>0) {
	Helicos::Exception::Simple->throw("invalid value <$params{max_processes}> for parameter <max_processes>\n");
    }
    $max_processes=$params{max_processes};
}

if (defined($params{max_retry})) {
    unless ($params{max_retry}=~/^\d+$/ and $params{max_retry}>=0) {
	Helicos::Exception::Simple->throw("invalid value <$params{max_retry}> for parameter <max_retry>\n");
    }
    $max_retry=$params{max_retry};
}

-e $params{sms_file} or Helicos::Exception::Simple->throw("specified SMS file <$params{sms_file}> not found\n");
-e $params{config_dbfile} or Helicos::Exception::Simple->throw("specified configuration MLDBM file <$params{config_dbfile}> not found\n");
-e $params{assignment_file} or Helicos::Exception::Simple->throw("specified DGE assignment file <$params{assignment_file}> not found\n");


my $analysis;
my $environment;
($analysis,$environment) = &read_configuration_file($params{config_dbfile});

my $run = $analysis->run;
my $instrument = $run->instrument;
my $instrument_name = $instrument->name;
my $run_name = $run->name;
my $analysis_name = $analysis->name;
my $protocol = $analysis->protocol;
my $step;
my $number_of_cameras = scalar(@{$instrument->cameras});
my @positions;
my @qc_positions;
my @flow_cell_ids = @{$protocol->flow_cell_ids};
my @cameras = (0..($number_of_cameras-1));
my $flow_cell_count = $instrument->number_of_flow_cells;

{
    my($program_name)=fileparse($0);
    my $precedure = $protocol->procedure;
    my @steps = grep {$_->program->name eq $program_name} @{$precedure->steps};
    @steps==1 or Helicos::Exception::Simple->throw("unable to find procedure step matching program name <$0>\n");
    $step = $steps[0];
}

{
    my $pos=$protocol->positions;
    if (defined($pos)) {@positions=@$pos;}
    
    my $qcpos=$analysis->run->qc_positions;
    if (defined($qcpos)) {@qc_positions=@$qcpos;}
}

my $basedir = join("/",$HELICOS_ANALYSIS_WORKSPACE_ROOT,$instrument_name,$run_name,$analysis_name);
my $ref_dir = join("/",$basedir,'reference_data');
my $reports_dir = join("/",$basedir,'reports');
my $log_dir = join("/",$basedir,'logs');
my $analysis_dir = join("/",$basedir,'analysis');

my $lengthtool_dir = join("/",$analysis_dir,'length_tool');
my $lengthtoollite_dir = join("/",$analysis_dir,'length_tool_lite');
my $errortool_dir = join("/",$analysis_dir,'error_tool');
my $extractsms_dir = join("/",$analysis_dir,'extract_sms');
my $filtersms_dir = join("/",$analysis_dir,'filter_sms');
my $indexdp_dir = join("/",$analysis_dir,'indexDP');
my $mergesms_dir = join("/",$analysis_dir,'merge_sms');
my $countdge_dir = join("/",$analysis_dir,'count_dge');

my $log_file = join("/",$log_dir,'analysis_protocol_dge.log');


chdir($basedir);

my $log_fh = IO::File->new(">$log_file");
$log_fh or Helicos::Exception::IO->throw("unable to open log file <$log_file> for writing\n");
$log_fh->autoflush(1);

### SET UP WORKSPACE ###

print $log_fh &timestamped_string("Setting up workspace.\n");

{   
    my @dirs = ($filtersms_dir,$indexdp_dir,$lengthtool_dir,$errortool_dir,$ref_dir,$mergesms_dir,$countdge_dir,$lengthtoollite_dir);
    if (@positions) { push @dirs, $extractsms_dir; }
    foreach my $path (@dirs)
    {
	unless(mkpath($path)) {
	    Helicos::Exception::Simple->throw("unable to create directory <$path>\n");
	}
    }
}


### GET ANALYSIS PROTOCOL PARAMETERS ###

my $hpdp_config_file;
my %analysis_protocol_parameters;
my $maximum_read_length;

my $indexdb_seed_size;
my $indexdb_number_errors;
my $indexdb_weight;

{
    foreach my $parameter_name ( 'minimum_read_length','maximum_orphan_object_quality_score','maximum_read_length_to_quad_ratio','maximum_bao_dinucleotide_frequency',
				 'maximum_A_plus_T_frequency','exclude_control_frame_positive_strands',
				 'junk_filter_bithpdp_percent_error','junk_filter_minimum_normalized_alignment_score',
				 'trim_T',
				 'alignment_template_family','alignment_template_family_percent_error','alignment_mode',
				 'alignment_percent_error','alignment_maximum_seed_hit_rate',
				 'alignment_minimum_normalized_score','alignment_minimum_suboptimal_normalized_score',
				 'alignment_maximum_suboptimal_normalized_score_delta','alignment_maximum_suboptimal_records',
				 'alignment_maximum_read_block_size') {
	$analysis_protocol_parameters{$parameter_name} = $step->parameter_by_name($parameter_name)->value;
    }
    
    my $hpdp_config_file_name;
    if ($analysis_protocol_parameters{alignment_mode} eq 'Global-Local') {
	$hpdp_config_file_name='hpdp_GL_noHP_config';
    }
    elsif ($analysis_protocol_parameters{alignment_mode} eq 'Local-Local') {
	$hpdp_config_file_name='hpdp_LL_noHP_config';
    }
    elsif ($analysis_protocol_parameters{alignment_mode} eq 'Smith-Waterman') {
	$hpdp_config_file_name='hpdp_SW_noHP_config';
    }
    else {
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{alignment_mode}> for parameter <alignment_mode>\n");
    }

    $hpdp_config_file = join("/",${HELICOS_ANALYSIS_HOME},"config/tools/HPDP/${hpdp_config_file_name}");

    #TO DO: clean up and expand sanity checks on values
    unless ($analysis_protocol_parameters{minimum_read_length}=~/^\d+$/ and $analysis_protocol_parameters{minimum_read_length}>=16) {
        Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{minimum_read_length}> for parameter <minimum_read_length>\n");
    }
    ($analysis_protocol_parameters{maximum_orphan_object_quality_score}=~/^\d+$/) or die;
    #($analysis_protocol_parameters{minimum_normalized_alignment_score}=~/^(\d+(\.\d*)?|(\d+)?\.\d+)$/ and $minimum_normalized_alignment_score<=5) or die;

    {
	my $flow_order_string = $run->flow_order->flow_order_string;
	$maximum_read_length = int($analysis_protocol_parameters{maximum_read_length_to_quad_ratio} * (length($flow_order_string)/4));
    }

<<<<<<< .working
=======
    unless (is_non_negative_integer($analysis_protocol_parameters{maximum_orphan_object_quality_score})) {
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{maximum_orphan_object_quality_score}> for parameter <maximum_orphan_object_quality_score>\n");
    }
    
    unless (is_boolean($analysis_protocol_parameters{trim_leading_T})) {
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{trim_leading_T}> for parameter <trim_leading_T>\n");
    }

    unless (is_number($analysis_protocol_parameters{trim_leading_T_frequency}) and 
	    $analysis_protocol_parameters{trim_leading_T_frequency}>=0 and 
	    $analysis_protocol_parameters{trim_leading_T_frequency}<=1) {
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{trim_leading_T_frequency}> for parameter <trim_leading_T_frequency>\n");
    }

    unless (is_number($analysis_protocol_parameters{maximum_bao_dinucleotide_frequency}) and 
	    $analysis_protocol_parameters{maximum_bao_dinucleotide_frequency}>=0 and 
	    $analysis_protocol_parameters{maximum_bao_dinucleotide_frequency}<=1) {
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{maximum_bao_dinucleotide_frequency}> for parameter <maximum_bao_dinucleotide_frequency>\n");
    }
    
    unless (is_number($analysis_protocol_parameters{maximum_A_plus_T_frequency}) and 
	    $analysis_protocol_parameters{maximum_A_plus_T_frequency}>=0 and 
	    $analysis_protocol_parameters{maximum_A_plus_T_frequency}<=1) {
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{maximum_A_plus_T_frequency}> for parameter <maximum_A_plus_T_frequency>\n");
    }

    unless (is_number($analysis_protocol_parameters{junk_filter_bithpdp_percent_error}) and 
	    $analysis_protocol_parameters{junk_filter_bithpdp_percent_error}>=0 and 
	    $analysis_protocol_parameters{junk_filter_bithpdp_percent_error}<=100) {
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{junk_filter_bithpdp_percent_error}> for parameter <junk_filter_bithpdp_percent_error>\n");
    }

    #TODO: warning - assumes standard score matrix
    unless (is_number($analysis_protocol_parameters{junk_filter_minimum_normalized_alignment_score}) and 
	    $analysis_protocol_parameters{junk_filter_minimum_normalized_alignment_score}>=0 and 
	    $analysis_protocol_parameters{junk_filter_minimum_normalized_alignment_score}<=5) {
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{junk_filter_minimum_normalized_alignment_score}> for parameter <junk_filter_minimum_normalized_alignment_score>\n");
    }
    
    unless (is_number($analysis_protocol_parameters{alignment_percent_error}) and 
	    $analysis_protocol_parameters{alignment_percent_error}>=0 and 
	    $analysis_protocol_parameters{alignment_percent_error}<=100) {
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{alignment_percent_error}> for parameter <alignment_percent_error>\n");
    }

    #TODO: warning - assumes standard score matrix
    unless (is_number($analysis_protocol_parameters{alignment_minimum_normalized_score}) and
	    $analysis_protocol_parameters{alignment_minimum_normalized_score}>=0 and
	    $analysis_protocol_parameters{alignment_minimum_normalized_score}<=5) {
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{alignment_minimum_normalized_score}> for parameter <alignment_minimum_normalized_score>\n");
    }

    unless (is_number($analysis_protocol_parameters{alignment_minimum_suboptimal_normalized_score}) and
	    $analysis_protocol_parameters{alignment_minimum_suboptimal_normalized_score}>=0 and
	    $analysis_protocol_parameters{alignment_minimum_suboptimal_normalized_score}<=5) {
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{alignment_minimum_suboptimal_normalized_score}> for parameter <alignment_minimum_suboptimal_normalized_score>\n");
    }
    
    unless (is_number($analysis_protocol_parameters{counting_alignment_minimum_normalized_score}) and
	    $analysis_protocol_parameters{counting_alignment_minimum_normalized_score}>=0 and
	    $analysis_protocol_parameters{counting_alignment_minimum_normalized_score}<=5) {
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{counting_alignment_minimum_normalized_score}> for parameter <counting_alignment_minimum_normalized_score>\n");
    }
    
    unless (is_number($analysis_protocol_parameters{alignment_maximum_seed_hit_rate}) and 
	    $analysis_protocol_parameters{alignment_maximum_seed_hit_rate}>=1) {
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{alignment_maximum_seed_hit_rate}> for parameter <alignment_maximum_seed_hit_rate>\n");
    }

    unless ($analysis_protocol_parameters{counting_alignment_minimum_normalized_score}>=$analysis_protocol_parameters{alignment_minimum_normalized_score}) {
	Helicos::Exception::Simple->throw("invalid combination of parameters: counting_alignment_minimum_normalized_score <$analysis_protocol_parameters{counting_alignment_minimum_normalized_score}> must be greater than or equal to alignment_minimum_normalized_score <$analysis_protocol_parameters{alignment_minimum_normalized_score}>\n");
    }

    unless ($analysis_protocol_parameters{alignment_minimum_normalized_score}>=$analysis_protocol_parameters{alignment_minimum_suboptimal_normalized_score}) {
	Helicos::Exception::Simple->throw("invalid combination of parameters: alignment_minimum_normalized_score <$analysis_protocol_parameters{alignment_minimum_normalized_score}> must be greater than or equal to alignment_minimum_suboptimal_normalized_score <$analysis_protocol_parameters{alignment_minimum_suboptimal_normalized_score}>\n");
    }
    
    unless (is_number($analysis_protocol_parameters{alignment_maximum_suboptimal_normalized_score_delta}) and 
	    $analysis_protocol_parameters{alignment_maximum_suboptimal_normalized_score_delta}>=0 and 
	    $analysis_protocol_parameters{alignment_maximum_suboptimal_normalized_score_delta}<=5) {
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{alignment_maximum_suboptimal_normalized_score_delta}> for parameter <alignment_maximum_suboptimal_normalized_score_delta>\n");
    }
    
    unless (is_non_negative_integer($analysis_protocol_parameters{alignment_maximum_suboptimal_records})) {
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{alignment_maximum_suboptimal_records}> for parameter <alignment_maximum_suboptimal_records>\n");
    }
    
    unless (is_positive_integer($analysis_protocol_parameters{alignment_maximum_read_block_size})) {
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{alignment_maximum_read_block_size}> for parameter <alignment_maximum_read_block_size>\n");
    }
  
    if ($analysis_protocol_parameters{trim_leading_T} and $analysis_protocol_parameters{trim_leading_T_frequency}<0.5) {
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{trim_leading_T_frequency}> for parameter <trim_leading_T_frequency>\n");
    }

    unless (is_positive_integer($analysis_protocol_parameters{qc_top_expressors_set_size})) {
	#TODO: set logical max
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{qc_top_expressors_set_size}> for parameter <qc_top_expressors_set_size>\n");
    }

    unless (is_positive_integer($analysis_protocol_parameters{qc_top_expressors_maximum_reference_length})) {
	#TODO: set logical range
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{qc_top_expressors_maximum_reference_length}> for parameter <qc_top_expressors_maximum_reference_length>\n");
    }

    unless (is_positive_integer($analysis_protocol_parameters{lengthtool_target_sample_size})) {
	#TODO: set logical max
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{lengthtool_target_sample_size}> for parameter <lengthtool_target_sample_size>\n");
    }
    
    unless (is_positive_integer($analysis_protocol_parameters{lengthtool_maximum_sample_size})) {
	#TODO: set logical max
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{lengthtool_maximum_sample_size}> for parameter <lengthtool_maximum_sample_size>\n");
    }
    
    unless (is_positive_integer($analysis_protocol_parameters{errortool_target_base_sample_size})) {
	#TODO: set logical max
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{errortool_target_base_sample_size}> for parameter <errortool_target_base_sample_size>\n");
    }
    
    unless (is_positive_integer($analysis_protocol_parameters{errortool_maximum_sample_size})) {
	#TODO: set logical max
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{errortool_maximum_sample_size}> for parameter <errortool_maximum_sample_size>\n");
    }
    
    unless (is_boolean($analysis_protocol_parameters{qc_alignment_uniqueness_option})) {
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{qc_alignment_uniqueness_option}> for parameter <qc_alignment_uniqueness_option>\n");
    }

    unless (is_positive_integer($analysis_protocol_parameters{termination_loss_minimum_cycles})) {
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{termination_loss_minimum_cycles}> for parameter <termination_loss_minimum_cycles>\n");
    }

    #TODO: parameter termination_loss_reference_position_range

>>>>>>> .merge-right.r8
    {
	#20:16:2
	unless ($analysis_protocol_parameters{alignment_template_family}=~/^(\d+):(\d+):(\d+)$/) {
	    Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{alignment_template_family}> for parameter <alignment_template_family>\n");
	}
	$indexdb_seed_size = $1;
	$indexdb_number_errors = $3;
	$indexdb_weight = $2;
    }
}

### BUILD REFERENCE FILES ###

print $log_fh &timestamped_string("Building reference files.\n");

my %reference_files;

my %references;
my %artifact_reference_ids;
my @ordered_ref_ids;

{
    #generate common base addition order artifact sequence
    my $bao_record;
    {
	my $flow_order_string = $run->flow_order->flow_order_string;
	my $nuc_challenge_frame_string = $flow_order_string;
	$nuc_challenge_frame_string=~tr/ACGT//cd;
	$bao_record=">$base_addition_order_id\n$nuc_challenge_frame_string\n";
	$artifact_reference_ids{$base_addition_order_id}++;
    }

    my $reference_set = $protocol->reference_sequence_set;
    my $reference_set_id = $reference_set->id;

    my $prefix="references";
	
    my %ids;
    my @exclude_records;

    %reference_files = ( main    => join("/",$ref_dir,"${prefix}.with_artifacts.fasta"),
			 exclude => join("/",$ref_dir,"${prefix}.exclude.fasta") );
    
    my %handles;
    foreach my $file_type (sort keys %reference_files) {
	my $filepath=$reference_files{$file_type};
	my $fh = IO::File->new(">$filepath");
	$fh or Helicos::Exception::IO->throw("unable to open file <$filepath> for writing\n");
	$handles{$file_type}=$fh;
    }

    my @components = @{$reference_set->components};
    
    foreach my $component (@components) {
	my %file_ids;
	my $reported_number_of_sequences = $component->number_of_sequences;
	my $filepath = $component->uri->path;
	-f $filepath or Helicos::Exception::Simple->throw("reference sequence file <$filepath> not found\n");
	defined($component->pass) and Helicos::Exception::Simple->throw("pass attribute for reference sequence for protocol <",$protocol->name,"> not valid\n");
	
	my $ref_class = $component->reference_class;

	my($filename)=fileparse($filepath);
	
	#make copy of original file?
	
	my $fh=IO::File->new($filepath);
	$fh or Helicos::Exception::IO->throw("unable to open file <$filepath> for reading\n");
	
	my $ofh=$handles{main};
	
	my $rec;
	
	while (defined($rec=&get_next_fasta_record($fh))) {

	    if (exists($ids{$rec->{id}})) {
	        Helicos::Exception::Simple->throw("reference sequence with id <$rec->{id}> not unique in reference file(s) for reference set <$reference_set_id>\n");
	    }
	    
	    if ($rec->{id} eq $base_addition_order_id) {
		Helicos::Exception::Simple->throw("reference sequence id in reference file <$filepath> matched base addition order id <$base_addition_order_id>\n");
	    }

	    $ids{$rec->{id}}++;
	    
	    $file_ids{$rec->{id}}++;
	    
	    if ($ref_class eq 'artifact') {
		push @exclude_records,$rec->{raw};
		$artifact_reference_ids{$rec->{id}}++;
		next;
	    }

	    print $ofh $rec->{raw};
	}
	
	close($fh) or die;
	
	my $observed_number_of_sequences = scalar(keys %file_ids);
	unless($observed_number_of_sequences == $reported_number_of_sequences) {
	    Helicos::Exception::Simple->throw("reported number <$reported_number_of_sequences> or reference sequences does not match observed <$observed_number_of_sequences>\n");
	}
    }

    push @exclude_records,$bao_record;

    #append artifacts to main reference file
    {
	my $ofh = $handles{main};
	print $ofh join("",@exclude_records);
    }
    
    #make exclude file
    {
	my $efh = $handles{exclude};
	print $efh join("",@exclude_records);
    }
    
    foreach my $file_type (sort keys %reference_files) {
	my $filepath=$reference_files{$file_type};
	my $fh = $handles{$file_type};
	close($fh) or Helicos::Exception::IO->throw("error on close of file <$filepath>\n");
    }

    #now get index for all sequences in main reference file

    {
	my $i=-1;
	my $fh = IO::File->new($reference_files{main});
	$fh or Helicos::Exception::IO->throw("unable to open file <$reference_files{main}> for reading\n");
	my $rec;
	while (defined($rec=&get_next_fasta_record($fh))) {
	    $i++;
	    my $length=length($rec->{sequence});
	    push @ordered_ref_ids, $rec->{id};
	    my $artifact = exists($artifact_reference_ids{$rec->{id}}) ? 1 : 0;
	    $references{$rec->{id}}={index=>$i,length=>$length,artifact=>$artifact,assign_index=>undef};
	}
    }
}

### BUILD ASSIGNMENT FILE ###

my $ref_assign_file=join("/",$ref_dir,"references.assign.txt");;

print $log_fh &timestamped_string("Constructing internal reference assignment file.\n");

my %assign_idx_to_reference;

{
    my %used_assign_idx;
    my %assign_file_ref_ids;
    my %assign_idx_to_name;
    my %assign_name_to_idx;

    my $fh = IO::File->new($params{assignment_file});
    $fh or die;
    
    my $max_assign_idx=-1;

    while (<$fh>) {
	chomp;
	next if /^\#/;
	my @f=split(/\t/,$_,-1);
	@f==3 or die;
	next if /^RefID/;
	my($ref_id,$assign_idx,$assign_name)=@f;

	$max_assign_idx = $assign_idx if ($assign_idx > $max_assign_idx);

	exists($assign_file_ref_ids{$ref_id}) and die;
	$assign_file_ref_ids{$ref_id}++;

	if ( exists($assign_idx_to_name{$assign_idx})
	     or
	     exists($assign_name_to_idx{$assign_name}) ) {
	    ($assign_idx_to_name{$assign_idx} eq $assign_name and $assign_name_to_idx{$assign_name} eq $assign_idx ) or die("assign_idx:$assign_idx assign_name:$assign_name");
	}
	else {
	    $assign_idx_to_name{$assign_idx}=$assign_name;
	    $assign_name_to_idx{$assign_name}=$assign_idx;
	}

	unless (exists($references{$ref_id})) {
	    print $log_fh "WARNING: reference sequence id <$ref_id> from assignment file not found in reference fasta file - skipping\n";
	}

	$used_assign_idx{$assign_idx}++;

	defined($references{$ref_id}->{assign_index}) and die;  #uniq assignment assumed

	$references{$ref_id}->{assign_index}=$assign_idx;

	$assign_idx_to_reference{$assign_idx}->{$ref_id}++;
    }

    close($fh) or die;

    my @unassigned;
    foreach my $ref_id (sort keys %references) {
	unless (defined($references{$ref_id}->{assign_index}) or $references{$ref_id}->{artifact}) {
	    push @unassigned, $ref_id;
	}
    }

    if (@unassigned) {
	foreach my $ref_id (@unassigned) {
	    warn "ERROR: reference <$ref_id> was not found in reference assignment file.";	    
	}
	Helicos::Exception::Simple->throw("one or more references unaccounted for in reference assignment file\n");
    }

    $fh = IO::File->new(">$ref_assign_file");
    $fh or die;

    print $fh "# $reference_files{main}\n";
    print $fh join("\t",qw(SeqID SeqName SeqLen TranscriptID TranscriptName)),"\n";

    foreach my $ref_id (@ordered_ref_ids) {
	my $tmp = $references{$ref_id};
	my $idx = $tmp->{index};
	my $len = $tmp->{length};
	my $artifact = $tmp->{artifact};
	next if $artifact;
	my $assign_idx = $tmp->{assign_index};
	my $assign_name = $assign_idx_to_name{$assign_idx};
	print $fh join("\t",$idx,$ref_id,$len,$assign_idx,$assign_name),"\n";
    }

    foreach my $ref_id (@ordered_ref_ids) {
	my $tmp = $references{$ref_id};
	my $idx = $tmp->{index};
	my $len = $tmp->{length};
	my $artifact = $tmp->{artifact};
	next unless $artifact;
	$max_assign_idx++;
	my $assign_idx = $max_assign_idx;
	my $assign_name = $ref_id;
	exists($assign_name_to_idx{$ref_id}) and die;
	print $fh join("\t",$idx,$ref_id,$len,$assign_idx,$assign_name),"\n";
    }   

    close($fh) or die;
}

my $raw_sms_file;
my $extract_sms_file;
my $filtered_sms_file;

{
    my($sms_filename)=fileparse($params{sms_file});
    my $tmp=$sms_filename;
    $tmp=~s/\.sms$//i;
    $raw_sms_file = join("/",$basedir,"data",$sms_filename);
    $extract_sms_file = join("/",$basedir,"data","${tmp}.extract.sms") if @positions;
    $filtered_sms_file = join("/",$basedir,"data","${tmp}.filtered.sms");
}


### COPY INPUT SMS FILE TO DATA DIRECTORY ###

{
    print $log_fh &timestamped_string("Copying input SMS file to results data directory.\n");
    
    my $cmd = "cp -p $params{sms_file} $raw_sms_file";

    my $stats = &execute_command("$cmd 2> /dev/null");
}

### RUN EXTRACTSMS ###

if (@positions) {

    print $log_fh &timestamped_string("Extracting strands for specified positions from original SMS file.\n");
    
    my $stdout = join("/",$log_dir,"extract_sms.out");
    my $stderr = join("/",$log_dir,"extract_sms.err");
    
    chdir($extractsms_dir);
    {
	my $program_name = 'extractSMS';
	my @args;
	push @args, "--input_file","$raw_sms_file";
	push @args, "--output_file","${extract_sms_file}";
	push @args, "--positions",join(",",@positions);
	
	my $cmd = join(" ",$program_name,@args);

	print $log_fh &timestamped_string("Excecuting command: $cmd\n");

	my $stats = &execute_command("$cmd > $stdout 2> $stderr");

	print $log_fh &timestamped_string(join("","--- timing: ",&timing_string($stats),"\n"));
    }

    chdir($basedir);
}

### RUN FILTERSMS ###

print $log_fh &timestamped_string("Filtering strands in SMS file.\n");

{
    my $stdout = join("/",$log_dir,"filter_sms.out");
    my $stderr = join("/",$log_dir,"filter_sms.err");
    
    my $infile = defined($extract_sms_file) ? $extract_sms_file : $raw_sms_file;

    my $dinuc_file = join("/",$filtersms_dir,"dinuc_filter.txt");
    
    {
	my $dinuc_fh = IO::File->new("> $dinuc_file");
	$dinuc_fh or Helicos::Exception::IO->throw("unable to open file <$dinuc_file> for writing\n");

	my $dinuc_template = join("/",${HELICOS_ANALYSIS_HOME},"config/tools/filterSMS/dinuc.txt");
	my $dinuc_template_fh  = IO::File->new($dinuc_template);
	$dinuc_template_fh or die;
	my $i=0;
	while (<$dinuc_template_fh>) {
	    $i++;
	    chomp;
	    my @f = split(/\t/,$_,-1);
	    @f==18 or die;
	    if ($i>1) {
		my($filter_name) = $f[0];
		if ($filter_name eq 'BAO') { $f[-1]=$analysis_protocol_parameters{maximum_bao_dinucleotide_frequency}; }
		elsif ($filter_name eq 'AT') { $f[-1]=$analysis_protocol_parameters{maximum_A_plus_T_frequency}; }
		else { next; }
		$_=join("\t",@f);
	    }
	    print $dinuc_fh "$_\n";
	}
	close($dinuc_template_fh) or Helicos::Exception::IO->throw("error on close of file <$dinuc_template>\n");
	close($dinuc_fh) or Helicos::Exception::IO->throw("error on close of file <$dinuc_file>\n");
    }
    
    
    chdir($filtersms_dir);
    {
	my $program_name = 'filterSMS';
	my @args;
	push @args, "--input_file","$infile";
	push @args, "--output_file","$filtered_sms_file";
	push @args, "--minlen","$analysis_protocol_parameters{minimum_read_length}";
	push @args, "--maxlen","$maximum_read_length";
	push @args, "--orphan","$analysis_protocol_parameters{maximum_orphan_object_quality_score}";
	push @args, "--dinuc","$dinuc_file";
	push @args, "--align","$reference_files{exclude}";
	push @args, "--config_file","$hpdp_config_file";
	push @args, "--percent_error","$analysis_protocol_parameters{junk_filter_bithpdp_percent_error}";
	push @args, "--minscore","$analysis_protocol_parameters{junk_filter_minimum_normalized_alignment_score}";
	push @args, "--trim","T/H/0.75";
	
	my $cmd = join(" ",$program_name,@args);

	print $log_fh &timestamped_string("Excecuting command: $cmd\n");

	my $stats = &execute_command("$cmd > $stdout 2> $stderr");

	print $log_fh &timestamped_string(join("","--- timing: ",&timing_string($stats),"\n"));
    }
    chdir($basedir);
}

### PARSE FILTERED STRAND COUNTS ###

my %filtered_strand_counts;

{    
    print $log_fh &timestamped_string("Parsing filtered strand counts from filterSMS stats file.\n");

    my $file  = join("/",$filtersms_dir,"filter_stats.txt");
    my $counts_ref = &parse_filter_stats_file($file);
    %filtered_strand_counts=%{$counts_ref};
}


#TODO: split sms file by channel

### RUN INDEXDP ###

#TODO: capture STDOUT STDERR from children

my @jobs;

print $log_fh &timestamped_string("Starting alignment phase.\n");


{
    chdir($indexdp_dir);

    #define all jobs

    my $job_id=0;

    my $align_sms_file = $filtered_sms_file;
    
    {
	my $program_name = 'alignment_wrapper.dge.pl';
	my @common_args;
	push @common_args, "--sms_file=$align_sms_file";
	push @common_args, "--reference_file=$reference_files{main}";	
	push @common_args, "--seed_size=$indexdb_seed_size";
	push @common_args, "--num_errors=$indexdb_number_errors";
	push @common_args, "--weight=$indexdb_weight";
	push @common_args, "--config_file=$hpdp_config_file";
	push @common_args, "--template_repository=$indexdp_template_repository";
	push @common_args, "--max_hit_duplication=$analysis_protocol_parameters{alignment_maximum_seed_hit_rate}";
	push @common_args, "--percent_error=$analysis_protocol_parameters{alignment_percent_error}";
	push @common_args, "--min_norm_score=$analysis_protocol_parameters{alignment_minimum_suboptimal_normalized_score}";
	push @common_args, "--aligned_files_threshold=$analysis_protocol_parameters{alignment_minimum_normalized_score}";
	push @common_args, "--max_diff=$analysis_protocol_parameters{alignment_maximum_suboptimal_normalized_score_delta}";
	push @common_args, "--max_align_num=$analysis_protocol_parameters{alignment_maximum_suboptimal_records}";

	foreach my $flow_cell_id (1..$flow_cell_count) {

	    my @channels=@{$protocol->channels_for_flow_cell($flow_cell_id)};

	    my @fargs;
	    push @fargs, "--flow_cell=$flow_cell_id";
	    
	    foreach my $channel_number (@channels) {

		my @cargs;
		push @cargs, "--channel=$channel_number";
		
		unless (exists($filtered_strand_counts{$flow_cell_id}) and exists($filtered_strand_counts{$flow_cell_id}->{$channel_number})) {
		    Helicos::Exception::Simple->throw("missing filter data for flowcell <$flow_cell_id> channel <$channel_number>\n");
		}
		
		my $strand_count = $filtered_strand_counts{$flow_cell_id}->{$channel_number};
		
		my $number_of_blocks = int($strand_count/$analysis_protocol_parameters{alignment_maximum_read_block_size}) + 1;

		push @cargs, "--num_blocks=$number_of_blocks";

		foreach my $block_id (1..$number_of_blocks) {

		    my $block_index = $block_id - 1;

		    my @bargs;

		    my $output_directory = join("/",$indexdp_dir,"flow_cell_${flow_cell_id}","channel_${channel_number}","block_${block_id}");
		    my $prefix = join("_","fc${flow_cell_id}","channel${channel_number}","block${block_id}");

		    push @bargs, "--block_index=$block_index";
		    push @bargs, "--output_directory=$output_directory";
		    push @bargs, "--out_prefix=${prefix}";

		    my $cmd = join(" ",$program_name,@common_args,@fargs,@cargs,@bargs);

		    $job_id++;

		    my $job = Helicos::SequenceAnalysis::Pipeline::Tools::IndexDP::Job->new( id             => $job_id,
											     flow_cell_id   => $flow_cell_id,
											     channel_number => $channel_number,
											     block          => $block_id,
											     command        => $cmd );

		    push @jobs, $job;
		}
	    }
	}
    }

    print $log_fh &timestamped_string(join("","Total jobs defined <",scalar(@jobs),">.\n"));
    
    # launch and monitor jobs until all are complete or a fatal error occurs

    warn Dumper(\@jobs);

    if ($use_sge) {
	die;
    }
    else {
	# just attempt to paralellize using multiple cores on local machine

	my @remaining_jobs = @jobs;
	
	my %kids;
	my $j=0;
	while ( @remaining_jobs or scalar(keys %kids) )
	{
	    sleep 5;
	    $j++;
	    #warn "LOOP: $j : number of jobs <",scalar(@remaining_jobs),"> ; number of kids <",scalar(keys %kids),">\n";
	    
	    # check for and reap children
	    my @reaped_jobs = &reap(kids=>\%kids,wait=>0);
	    foreach my $job (@reaped_jobs) {
		my $jid = $job->id;
		my $status=$job->status;
		if ($status) {
		    my $attempts = $job->attempt;
		    if ($attempts<$max_retry+1) {
			#TODO: warn
			unshift @remaining_jobs, $job;
			next;
		    }
		    my $flow_cell_id=$job->flow_cell_id;
		    my $channel_number=$job->channel_number;
		    my $block=$job->block;
		    Helicos::Exception::Simple->throw("background job <$jid> for flow cell <$flow_cell_id> channel <$channel_number> block <$block> failed and maximum retries exceeded.\n");
		}
		print $log_fh &timestamped_string("Reaped job <$jid> : status <$status>\n");
	    }
	    
	    # spawn new process if possible
	    
	    while ( @remaining_jobs and scalar(keys %kids)<$max_processes )
	    {
		my $job = shift(@remaining_jobs);
		my $jid = $job->id;
		my $flow_cell_id=$job->flow_cell_id;
		my $channel_number=$job->channel_number;
		my $block=$job->block;
		my $cmd = $job->command;
		
		my $active_process_number = scalar(keys %kids) + 1;

		print $log_fh &timestamped_string("Launching alignment job <$jid> for flow cell <$flow_cell_id> channel <$channel_number> block <$block>: active process <$active_process_number> of <$max_processes> allowed.\n");
		print $log_fh &timestamped_string("Excecuting background command: $cmd\n");
		
		#TODO: avoid write conflicts by children

		open(OLDSTDOUT,">&STDOUT") or die;
		open(STDOUT,">&",$log_fh) or die;

		my $proc = Proc::Background->new($cmd);

		open(STDOUT,">&OLDSTDOUT") or die;
		
		unless (defined($proc)) {
		    Helicos::Exception::Simple->throw("unable to launch background job <$jid> for flow cell <$flow_cell_id> channel <$channel_number> block <$block>.\n");
		}
		
		$job->start();
		$kids{$jid} = {job=>$job,process=>$proc};
	    }
	}
    }   
}

print $log_fh &timestamped_string("Completed alignment phase.\n");


### MERGE SMS OUTPUT FILES ###

print $log_fh &timestamped_string("Merging output from alignment phase.\n");

{
    my %reads_files = map {$_=>[];} qw(repetitive aligned unaligned);
    
    foreach my $job (@jobs) {
	my $jid = $job->id;
	my $flow_cell_id=$job->flow_cell_id;
	my $channel_number=$job->channel_number;
	my $block_id=$job->block;
	my $output_directory = join("/",$indexdp_dir,"flow_cell_${flow_cell_id}","channel_${channel_number}","block_${block_id}");
	my $prefix = join("_","fc${flow_cell_id}","channel${channel_number}","block${block_id}");
 	my $rep_file = join("/",$output_directory,"${prefix}_indexDP_duplicates.sms");
	my $aln_file = join("/",$output_directory,"${prefix}_indexDP_aligned.sms");
	my $unaln_file = join("/",$output_directory,"${prefix}_indexDP_nonAligned.sms");
	push @{$reads_files{repetitive}}, $rep_file if (-e $rep_file);
	push @{$reads_files{aligned}}, $aln_file if (-e $aln_file);
	push @{$reads_files{unaligned}}, $unaln_file if (-e $unaln_file);
    }
 
    chdir($mergesms_dir);

    foreach my $read_class (sort keys %reads_files)
    {
	my @files = @{$reads_files{$read_class}};
	my $out_file = join("/",$mergesms_dir,"reads.${read_class}.sms");
	
	my $program_name = 'mergeSMS';
	my @args;
	foreach my $file (@files) {
	    push @args, "--input_file","$file";
	}
	push @args, "--output_file","${out_file}";
	
	my $stdout = join("/",$log_dir,"merge_sms.${read_class}.out");
	my $stderr = join("/",$log_dir,"merge_sms.${read_class}.err");
	
	my $cmd = join(" ",$program_name,@args);

	print $log_fh &timestamped_string("Excecuting command: $cmd\n");

	my $stats = &execute_command("$cmd > $stdout 2> $stderr");
	
	print $log_fh &timestamped_string(join("","--- timing: ",&timing_string($stats),"\n"));
    }

    chdir($basedir);
}

### RUN COUNTDGE ###

{
    my @summary_alignment_files;
    
    foreach my $job (@jobs) {
	my $jid = $job->id;
	my $flow_cell_id=$job->flow_cell_id;
	my $channel_number=$job->channel_number;
	my $block_id=$job->block;
	my $output_directory = join("/",$indexdp_dir,"flow_cell_${flow_cell_id}","channel_${channel_number}","block_${block_id}");
	my $summary_alignment_file = join("/",$output_directory,"summary_alignments.bin");
	push @summary_alignment_files, $summary_alignment_file;
    }
 
    chdir($countdge_dir);

    my $out_file_prefix = join("/",$countdge_dir,"dge");
    
    my $program_name = 'countDGE';
    my @args;
    foreach my $file (@summary_alignment_files) {
	push @args, "--align_file","$file";
    }
    push @args, "--ref_file","$ref_assign_file";
    push @args, "--out_prefix","${out_file_prefix}";  #docs say output file, but used as a prefix

    push @args, "--min_score","$analysis_protocol_parameters{alignment_minimum_normalized_score}";
    push @args, "--min_len","$analysis_protocol_parameters{minimum_read_length}";
    push @args, "--max_diff","$analysis_protocol_parameters{alignment_maximum_suboptimal_normalized_score_delta}";
    push @args, "--score_file","$countDGE_score_probability_file";


    my $stdout = join("/",$log_dir,"count_dge.out");
    my $stderr = join("/",$log_dir,"count_dge.err");
    
    my $cmd = join(" ",$program_name,@args);
    
    print $log_fh &timestamped_string("Excecuting command: $cmd\n");
    
    my $stats = &execute_command("$cmd > $stdout 2> $stderr");
    
    print $log_fh &timestamped_string(join("","--- timing: ",&timing_string($stats),"\n"));

    chdir($basedir);
}

### PERFORM QC ASSESSMENT ###

{
    #TODO: expand to work from spike-ins as well
    
    #get references corresponding to the top 10 most abundant genes
    my $counts_file = join("/",$countdge_dir,"dge.count");
    
    print $log_fh &timestamped_string("Parsing top hit information from counts file <$counts_file>\n");
    my($top_references,$assigned_read_fraction) = &get_qc_reference_set(counts_file=>$counts_file,number=>10);

    #set sample size to yield approx 10000 aligned reads
    my $sample_size = &min(100000,int(10000/$assigned_read_fraction));

    my $prefix="references";
    my $qc_reference_file = join("/",$ref_dir,"${prefix}.qc_set.fasta");

    my %top_refs;
    map {$top_refs{$_}++} @$top_references;
 
    my $fh = IO::File->new($reference_files{main});
    $fh or die;
 
    my $ofh = IO::File->new(">$qc_reference_file");
    $ofh or die;

    my $rec;
    while (defined($rec=&get_next_fasta_record($fh))) {
	next unless (exists($top_refs{$rec->{id}}));
	print $ofh $rec->{raw};
    }

    close($fh) or die;
    close($ofh) or die;

    my $qc_reads_file = join("/",$mergesms_dir,"reads.aligned.sms");
    
    ### RUN LENGTHTOOLLITE ###

    print $log_fh &timestamped_string("Running lengthTool tool.\n");
    
    {
	my $raw_sms_file_to_process = defined($extract_sms_file) ? $extract_sms_file : $raw_sms_file;

	chdir($lengthtoollite_dir);
	{
	    my $program_name = 'lengthToolLite';
	    my @common_args;

	    foreach my $file ($raw_sms_file_to_process,$filtered_sms_file,$qc_reads_file) {
		push @common_args, "--in_file","$file";
	    }
	    
	    foreach my $flow_cell_id (1..$flow_cell_count) {
		my @channels=@{$protocol->channels_for_flow_cell($flow_cell_id)};
		if (@channels) {
		    my @args;
		    push @args, "--flow_cells","$flow_cell_id";
		    push @args, "--channels",join(",",@channels);
		    
		    my $stdout = join("/",$log_dir,"length_tool_lite.fc${flow_cell_id}.out");
		    my $stderr = join("/",$log_dir,"length_tool_lite.fc${flow_cell_id}.err");
		    
		    my $cmd = join(" ",$program_name,@common_args,@args);
		    
		    print $log_fh &timestamped_string("Excecuting command: $cmd\n");
		    
		    my $stats = &execute_command("$cmd > $stdout 2> $stderr");
		    
		    print $log_fh &timestamped_string(join("","--- timing: ",&timing_string($stats),"\n"));
		}		  
	    }
	}
	chdir($basedir);
    }

    ### RUN LENGTHTOOL ###

    print $log_fh &timestamped_string("Running lengthTool tool.\n");
    
    {
	#my $exclude_reference_file = $reference_files{$reference_set_id}->{exclude};
	
	chdir($lengthtool_dir);
	{
	    my $program_name = 'lengthTool';
	    my @common_args;
	    push @common_args, "--read_file","$qc_reads_file";
	    push @common_args, "--reference_file","$qc_reference_file";
	    #push @common_args, "--excluded_references_file","$exclude_reference_file";
	    push @common_args, "--sample_size","$sample_size";
	    push @common_args, "--term_cycles","20";
	    push @common_args, "--uniqueness_option","1";
	    push @common_args, "--percent_error","30";
	    push @common_args, "--config_file","$hpdp_config_file";
	    push @common_args, "--min_normalized_score","$analysis_protocol_parameters{alignment_minimum_normalized_score}";
	    
	    foreach my $flow_cell_id (1..$flow_cell_count) {
		my @channels=@{$protocol->channels_for_flow_cell($flow_cell_id)};
		if (@channels) {
		    my @args;
		    push @args, "--flow_cells","$flow_cell_id";
		    push @args, "--channels",join(",",@channels);
		    
		    my $stdout = join("/",$log_dir,"length_tool.fc${flow_cell_id}.out");
		    my $stderr = join("/",$log_dir,"length_tool.fc${flow_cell_id}.err");
		    
		    my $cmd = join(" ",$program_name,@common_args,@args);
		    
		    print $log_fh &timestamped_string("Excecuting command: $cmd\n");
		    
		    my $stats = &execute_command("$cmd > $stdout 2> $stderr");
		    
		    print $log_fh &timestamped_string(join("","--- timing: ",&timing_string($stats),"\n"));
		    
		    #now rename the newly created growth.pass1 output file to have a flow-cell-specific name (otherwise we overwrite fc1 output - if generated - with fc2 output)
		    
		    my $growth_file = join("/",$lengthtool_dir,"growth.pass1");
		    my $growth_file_renamed = join("/",$lengthtool_dir,"growth.fc${flow_cell_id}.pass1");
		    -f $growth_file or Helicos::Exception::Simple->throw("expected file <$growth_file> does not exist\n");
		    &execute_command("mv",$growth_file,$growth_file_renamed);
		}
	    }
	}
	chdir($basedir);
    }

    
    ### RUN ERRORTOOL ###
    print $log_fh &timestamped_string("Running errorTool tool.\n");
    
    {
	my($x_dim,$y_dim,$bin_size)=(1392,1040,100);
	
	my $sample_size_by_nuc = &min(100000,($sample_size * 4));
	my $sample_size_by_cycle = &min(100000,($sample_size * 10));
	
	foreach my $dname (qw(by_cycle_summary)) {
	    my $tmpname = "${errortool_dir}/${dname}";
	    # unless(mkpath("${tmpname}")) {
	    # 	 Helicos::Exception::Simple->throw("unable to create directory <$tmpname>\n");
	    # }
	}
	
	chdir($errortool_dir);
	
	my $program_name = 'errorTool';
	my @common_args;
	push @common_args, "--read_file","$qc_reads_file";
	push @common_args, "--reference_file","$qc_reference_file";
	#push @common_args, "--excluded_references_file","$exclude_reference_file";
	push @common_args, "--uniqueness_option","1";
	push @common_args, "--percent_error","30";
	push @common_args, "--config_file","$hpdp_config_file";
	push @common_args, "--first_read_number","1";
	push @common_args, "--last_read_number","1";
	push @common_args, "--min_normalized_score","$analysis_protocol_parameters{alignment_minimum_normalized_score}";
	
	foreach my $analysis_type (qw(general by_nuc by_detailed_substitutions)) {     #by_cycle
	    
	    my @targs;
	    push @targs, "--analysis_type","$analysis_type";
	    if ($analysis_type eq 'general') {
		push @targs, "--sample_size","$sample_size";
		push @targs, "--by_camera";          #needed?
	    #push @targs, "--summary_output";
	    }
	    elsif ($analysis_type eq 'by_nuc') {
		push @targs, "--sample_size","$sample_size_by_nuc";
		push @targs, "--by_camera";
		push @targs, "--summary_output";
	    }
	    elsif ($analysis_type eq 'by_detailed_substitutions') {
		push @targs, "--sample_size","$sample_size_by_nuc";
		push @targs, "--by_camera";
		#push @targs, "--summary_output"; #not implemented
	    }
	    elsif ($analysis_type eq 'by_cycle') {
		chdir("${errortool_dir}/by_cycle_summary");
		push @targs, "--sample_size","$sample_size_by_cycle";
	    }
	    else {die;}
	    
	    foreach my $flow_cell_id (1..$flow_cell_count) {
		my @channels=@{$protocol->channels_for_flow_cell($flow_cell_id)};
		if (@channels) {
		    my @fargs;
		    push @fargs, "--flow_cells","$flow_cell_id";
		    push @fargs, "--channels",join(",",@channels);
		    
		    my $stdout = join("/",$log_dir,"error_tool.${analysis_type}.fc${flow_cell_id}.out");
		    my $stderr = join("/",$log_dir,"error_tool.${analysis_type}.fc${flow_cell_id}.err");
		    
		    my $cmd = join(" ",$program_name,@common_args,@targs,@fargs);
		    
		    print $log_fh &timestamped_string("Excecuting command: $cmd\n");
		    
		    my $stats = &execute_command("$cmd > $stdout 2> $stderr");
		    
		    print $log_fh &timestamped_string(join("","--- timing: ",&timing_string($stats),"\n"));
		    
		    if ($analysis_type eq 'by_detailed_substitutions') {
			#now rename detailed_substitutions file to distinguish results			
			my $ofile = 'by_detailed_substitutions.txt';
			-f $ofile or die;
			my $nfile=join(".",'by_detailed_substitutions',"fc${flow_cell_id}","passALL",'txt');
			my $source = join("/",$errortool_dir,$ofile);
			my $dest = join("/",$errortool_dir,$nfile);
			&execute_command("mv",$source,$dest);
		    }
		}
	    }
	    
	    if ($analysis_type eq 'by_cycle') {
		chdir($errortool_dir);
	    }
	}
	chdir($basedir);
    }
    
}

print $log_fh &timestamped_string("Protocol complete.\n");

close($log_fh) or die;

exit;

######################################################################

sub reap
{
    #check for completed jobs
    my @args = @_;
    @args % 2 == 0 or Helicos::Exception::Simple->throw("invalid number of arguments\n");
    my %args = @args;
    
    my $kids;
    my $wait = 0;
    
    foreach my $key (keys %args) {
	my $val=$args{$key};
	if ($key eq 'kids') { $kids=$val; }
	elsif ($key eq 'wait') {
	    die unless ($val=~/^\d+$/ and ($val==0 or $val==1));
	    $wait=$val;
	}
	else {die;}
    }
    
    my @job_ids = keys %{$kids};
    
    my @reaped_jobs;
    
    foreach my $job_id (@job_ids)
    {
	my $kid = $kids->{$job_id};
	
	my $job = $kid->{job};
	my $process = $kid->{process};

	next if ( $process->alive() and ! $wait);
	
	my $flow_cell_id = $job->flow_cell_id;
	my $channel_number = $job->channel_number;
	my $block = $job->block;

	my $status = $process->wait();

	$job->stop($status);

	push @reaped_jobs,$job;
	delete($kids->{$job_id});
    }

    return @reaped_jobs;
}

sub read_configuration_file
{
    my($file)=@_;
    my %conf;
    my $db = tie %conf, 'MLDBM', $file, O_RDONLY or Helicos::Exception::IO->throw("unable to open configuration file <$file>: $!\n");
    my $environment=$conf{environment};
    my $analysis=$conf{analysis};
    undef $db;
    untie(%conf);
    return($analysis,$environment);
}

sub parse_filter_stats_file
{
    my($file)=@_;

    my %counts;
    
    my %map = ( 'In'      => 'in',
		'Length'  => 'length',
		'Ctrl'    => 'control',
		'Qual'    => 'quality',
		'Out'     => 'out');
    
    my $fh = IO::File->new($file);
    $fh or Helicos::Exception::IO->throw("unable to open file <$file> for reading\n");

    my @in_hdr;
    
    while (<$fh>) {
	chomp;
	next if /\#/;
	my @f = split(/\t/,$_,-1);
	@f>=6 or die;
	if (/^Flow/) {
	    @in_hdr=@f;
	    unless ($in_hdr[0] eq 'Flowcell' and
		    $in_hdr[1] eq 'Channel' and
		    $in_hdr[2] eq 'Position' and
		    $in_hdr[3] eq 'Camera') {
		Helicos::Exception::Simple->throw("bad format for file <$file>: header: ",join(" /// ",@in_hdr),"\n");;
	    }
	    next;
	}
	@f==@in_hdr or die;

	my($fc,$ch) = @f[0,1];
	my $out_count = $f[-1];

	grep {!/^\d+$/} ($fc,$ch,$out_count) and die;
	
	(exists($counts{$fc}) and exists($counts{$fc}->{$ch})) and die;
	$counts{$fc}->{$ch}=$out_count;
    }
    close($fh) or die;

    return(\%counts);
} 

sub get_qc_reference_set
{
    my(@args)=@_;
    @args % 2 == 0 or Helicos::Exception::IO->throw("invalid number of arguments\n");
    my %args=@args;
    
    my $file;
    my $number;

    foreach my $key (sort keys %args) {
	my $val=$args{$key};
	if ($key eq 'counts_file') {$file=$val;}
	elsif ($key eq 'number') {$number=$val;}
	else {die;}
    }

    (defined($file) and -e $file) or die "FATAL: no file <$file>";
    &is_positive_integer($number) or die;

    my $fh = IO::File->new($file);
    $fh or Helicos::Exception::IO->throw("unable to open file <$file> for reading\n");

    my $total=0;
    my @data;

    while (<$fh>) {
	chomp;
	next if /\#/;
	my @f = split(/\t/,$_,-1);
	@f==8 or die;
	$f[7] eq "" or die;  #black trailing column
	my($assign_idx,$assign_name,$min,$max,$frac,$rmc,$avg_score) = @f;
	&is_non_negative_integer($assign_idx) or die;
	(&is_number($rmc) and $rmc>=0) or die;
	$total+=$rmc;
	push @data, [$assign_idx,$rmc];
    }
    close($fh) or die;

    @data = sort {$b->[1]<=>$a->[1] or $a->[0]<=>$b->[0];} @data;
    
    my @top;
    if ($number>scalar(@data)) { @top = @data; }
    else { @top = @data[0..$number-1]; }

    my @top_idx = map {$_->[0];} @top;

    my $top_total=0;
    map {$top_total+=$_->[1]} @top;
    
    #warn Dumper(\%references,\%assign_idx_to_reference);
    #warn Dumper(\@top);

    my %top_refs;
    foreach my $top (@top) {
	my($assign_idx,$rmc)=@$top;
	defined($assign_idx_to_reference{$assign_idx}) or die $assign_idx;
	my @refs = sort keys %{$assign_idx_to_reference{$assign_idx}};

	my $frac_total = ($top_total>0) ? sprintf("%.6f",$rmc/$top_total) : "";
	
	map {$top_refs{$_}++} @refs;
    }

    my @top_refs = sort keys %top_refs;

    return(\@top_refs,$top_total/$total);
} 

sub _usage
{
    my $usage;
    my @lines;
    push @lines,"Synopsis:";
    push @lines,"     analysis_protocol.dge.pl --sms_file=path_to_strand_file --config_dbfile=path_to_config_file [options]";
    push @lines,"     analysis_protocol.dge.pl --help";
    push @lines,"";
    push @lines,"Parameters/Options:";
    push @lines,"     --sms_file=arg              Path to input SMS strand data file";
    push @lines,"     --config_dbfile=arg         Path to MLDBM run/analysis configuration file (generated by analysis_controller.pl)";
    push @lines,"";
    push @lines,"     --help                      Print this help message";

    $usage = join("",join("\n",@lines),"\n");
    return $usage;
}




__END__





#==========================================================================================
# GENERATE SUMMARY REPORTS

print $log_fh &timestamped_string("Generating summary reports.\n");

my %report_dirs;

my %filter_profiles = (map {$_=>undef} (@flow_cell_ids));
my %growth_profiles = (map {$_=>undef} (@flow_cell_ids));
my %error_profiles = (map {$_=>undef} (@flow_cell_ids));
my %yield_profiles = (map {$_=>undef} (@flow_cell_ids));

{
    foreach my $flow_cell_id (@flow_cell_ids) {
	my $dir = join("/",$reports_dir,"flow_cell_$flow_cell_id");
	unless(mkpath($dir)) {
	    Helicos::Exception::Simple->throw("unable to create directory <$dir>\n");
	}
	$report_dirs{$flow_cell_id}=$dir;
    }
}

# parse strand filter information

{
    foreach my $flow_cell_id (@flow_cell_ids) {
	my @channel_numbers=@{$protocol->channels_for_flow_cell($flow_cell_id)};
	my $file  = join("/",$filtersms_dir,"filter_stats.txt");
	-e $file or die;
	
	my $filter_profile = Helicos::SequenceAnalysis::Pipeline::FilterProfile->new(flow_cell_id=>$flow_cell_id,
										     channel_numbers=>\@channel_numbers);
	
	$filter_profile->parse_filter_stats_file($file);
	
	$filter_profiles{$flow_cell_id}=$filter_profile;
    }
}

# parse all growth information

{
    foreach my $flow_cell_id (@flow_cell_ids) {
	my @channel_numbers=@{$protocol->channels_for_flow_cell($flow_cell_id)};
	my @termloss_files;
	foreach my $channel_number (@channel_numbers) {
	    foreach my $camera_id (@cameras) {
		my $file  = join("/",$lengthtool_dir,"termloss.fc${flow_cell_id}.ch${channel_number}.pass1");
		-e $file or die;
		push @termloss_files,$file;
	    }
	}	
	
	my $growth_table=join("/",$lengthtool_dir,"growth.fc${flow_cell_id}.pass1");
	-e $growth_table or die;
	
	my $growth_profile = Helicos::SequenceAnalysis::Pipeline::GrowthProfile->new(flow_cell_id=>$flow_cell_id,
										     channel_numbers=>\@channel_numbers,
										     pass=>1);
    
	$growth_profile->parse_growth_table($growth_table);
	$growth_profile->compile_from_termloss_tables(@termloss_files);
	
	$growth_profiles{$flow_cell_id}=$growth_profile;
    }
}

# parse all error information

{
    foreach my $flow_cell_id (@flow_cell_ids) {
	my @channel_numbers=@{$protocol->channels_for_flow_cell($flow_cell_id)};
	my @files;
	foreach my $channel_number (@channel_numbers) {
	    foreach my $camera_id (@cameras) {
		my $file  = join("/",$errortool_dir,"by_nuc_error_fl${flow_cell_id}_ch${channel_number}_passALL_camera${camera_id}");
		-e $file or die;
		push @files,$file;
	    }
	}	
	
	my $error_profile = Helicos::SequenceAnalysis::Pipeline::ErrorProfile->new(flow_cell_id=>$flow_cell_id,
										   channel_numbers=>\@channel_numbers,
										   analysis=>$analysis,
										   pass=>1);
    
	$error_profile->compile_from_by_nuc_tables(@files);
	
	$error_profiles{$flow_cell_id}=$error_profile;
    }
}
 
# parse count information

{
    foreach my $flow_cell_id (@flow_cell_ids) {
	my @channel_numbers=@{$protocol->channels_for_flow_cell($flow_cell_id)};
	my $file  = join("/",$palmer_dir,"palmer.hits.fc${flow_cell_id}.csv");
	-e $file or die;
		
	my $yield_profile = Helicos::SequenceAnalysis::Pipeline::YieldProfile->new(flow_cell_id=>$flow_cell_id,
										   channel_numbers=>\@channel_numbers,
										   ordered_reference_ids=>$ordered_reference_ids{$reference_set_id},
										   analysis=>$analysis,
										   filter_profile=>$filter_profiles{$flow_cell_id});
	
	$yield_profile->parse_single_pass_palmer_file($file);
	
	$yield_profiles{$flow_cell_id}=$yield_profile;
    }
}


# growth summary
    
{
    foreach my $flow_cell_id (@flow_cell_ids) {
	my $growth_profile = $growth_profiles{$flow_cell_id};
	
	{
	    my $outfile_name = "growth.fc${flow_cell_id}.txt";
	    my $outfile = join("/",$report_dirs{$flow_cell_id},$outfile_name);
	    $growth_profile->generate_growth_report(file=>$outfile);
	}
    }
}

# termloss summaries

{
    foreach my $flow_cell_id (@flow_cell_ids) {
	my $growth_profile = $growth_profiles{$flow_cell_id};

	{
	    my $outfile_name = "termloss.summary.fc${flow_cell_id}.txt";
	    my $outfile = join("/",$report_dirs{$flow_cell_id},$outfile_name);
	    $growth_profile->generate_termloss_report(file=>$outfile,type=>'summary');
	}
	
	{
	    my $outfile_name = "termloss.detail.fc${flow_cell_id}.txt";
	    my $outfile = join("/",$report_dirs{$flow_cell_id},$outfile_name);
	    $growth_profile->generate_termloss_report(file=>$outfile,type=>'detail',min_reference_position=>5,max_reference_position=>15);
	}	
    }
}

# strand filter summary report
#    report total strands and loss due to attrition during initial filtering

{
    foreach my $flow_cell_id (@flow_cell_ids) {
	my $filter_profile = $filter_profiles{$flow_cell_id};
	
	{
	    my $outfile_name = "strand_filter_summary.fc${flow_cell_id}.txt";
	    my $outfile = join("/",$report_dirs{$flow_cell_id},$outfile_name);
	    $filter_profile->generate_filter_report(file=>$outfile);
	}
    }
}

# yield summary reports
#    report total strands and loss due to attrition during initial filtering

{
    foreach my $flow_cell_id (@flow_cell_ids) {
	my $yield_profile = $yield_profiles{$flow_cell_id};

	{
	    my $outfile_name = "yield.fc${flow_cell_id}.txt";
	    my $outfile = join("/",$report_dirs{$flow_cell_id},$outfile_name);
	    $yield_profile->generate_report(file=>$outfile,type=>'yield');	    
	}
	
	{
	    my $outfile_name = "counts.usable_strands.fc${flow_cell_id}.txt";
	    my $outfile = join("/",$report_dirs{$flow_cell_id},$outfile_name);
	    $yield_profile->generate_report(file=>$outfile,type=>'usable');	    
	}

	{
	    my $outfile_name = "relative_abundance.usable_strands.fc${flow_cell_id}.txt";
	    my $outfile = join("/",$report_dirs{$flow_cell_id},$outfile_name);
	    $yield_profile->generate_report(file=>$outfile,type=>'relative_abundance');	    
	}
    }
}

## generate error reports

{
    my %context_param = $has_hp_refs ? () : (context=>'non-HP');

    # general
    foreach my $flow_cell_id (@flow_cell_ids) {
	my $error_profile = $error_profiles{$flow_cell_id};

	{
	    my $outfile_name = "errors.fc${flow_cell_id}.txt";
	    my $outfile = join("/",$report_dirs{$flow_cell_id},$outfile_name);
	    $error_profile->generate_report(file=>$outfile,type=>'general',by_camera=>0,%context_param);
	}

	{
	    my $outfile_name = "errors.by_camera.fc${flow_cell_id}.txt";
	    my $outfile = join("/",$report_dirs{$flow_cell_id},$outfile_name);
	    $error_profile->generate_report(file=>$outfile,type=>'general',by_camera=>1,%context_param);
	}
    }

    # deletion/insertion
    foreach my $flow_cell_id (@flow_cell_ids) {
	my $error_profile = $error_profiles{$flow_cell_id};

	foreach my $error_type (qw(deletion insertion))
	{
	    {
		my $outfile_name = "${error_type}s.fc${flow_cell_id}.txt";
		my $outfile = join("/",$report_dirs{$flow_cell_id},$outfile_name);
		$error_profile->generate_report(file=>$outfile,type=>$error_type,by_camera=>0,%context_param);
	    }
	    
	    {
		my $outfile_name = "${error_type}s.by_camera.fc${flow_cell_id}.txt";
		my $outfile = join("/",$report_dirs{$flow_cell_id},$outfile_name);
		$error_profile->generate_report(file=>$outfile,type=>$error_type,by_camera=>1,%context_param);
	    }
	}
	
    }
}


__END__

   #  cmd="alignment_wrapper.dge.pl --sms_file=$READ_FILE --reference_file=${REF_FILE} --seed_size=20 --num_errors=1 --weight=16 --config_file=${CONFIGFILE} --template_repository=${REPOS} --flow_cell=1 --channel=${CHANNEL} --num_blocks=3 --block_index=${i} --max_hit_duplication=20 --out_prefix=out_prefix --percent_error=10 --min_norm_score=4.0 --aligned_files_threshold=4.5 --max_diff=1 --max_align_num=100 --output_directory=output/channel${CHANNEL}/block${BLOCK}"
