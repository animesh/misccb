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
# FILE:          analysis_protocol.oligo.pl
# DESCRIPTION:   
# AUTHORS:       Steven Roels
# ================================================================================

use 5.008;

use strict;
use vars qw( $REVISION $VERSION) ;

use Getopt::Long;
use IO::File;
use Fcntl;
use File::Path qw(mkpath rmtree);
use File::Copy;
use File::Basename;
use Data::Dumper;
use Cwd qw(chdir getcwd abs_path);
use MLDBM qw(DB_File Storable);     # DB_File and Storable

use Helicos::Util qw(execute_command min max);
use Helicos::Util::ValueTypes qw(is_non_negative_integer is_positive_integer is_number is_boolean);
use Helicos::Exception::Classes;
use Helicos::MetaData::Classes;

use Helicos::SequenceAnalysis::Pipeline::Environment;
use Helicos::SequenceAnalysis::Pipeline::Util qw(output_header get_next_fasta_record get_channel_blocks timestamped_string formatted_datetime timing_string);
use Helicos::SequenceAnalysis::Pipeline::FilterProfile;
use Helicos::SequenceAnalysis::Pipeline::GrowthProfile;
use Helicos::SequenceAnalysis::Pipeline::ErrorProfile;
use Helicos::SequenceAnalysis::Pipeline::YieldProfile;

$REVISION = "";
$VERSION = "1.0.6";

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

my $prefix_mode_hpdp_config_file    = join("/",${HELICOS_ANALYSIS_HOME},"config/tools/HPDP/hpdp_LJ_noHP_config");

my $schema_dir                      = join("/",${HELICOS_ANALYSIS_HOME},'/config/descriptors/schema');

my $has_hp_refs=0;

my %params = (sms_file                         => undef,
	      config_dbfile                    => undef);

my %OPTIONS = (
	       'verbose=i'                => \$verbose,
	       'debug=i'                  => \$debug,
	       'help'                     => \$help,
	      );

foreach my $paramkey (qw(sms_file config_dbfile)) {
    $OPTIONS{join("",$paramkey,"=s")}=\$params{$paramkey};
}

unless (&GetOptions(%OPTIONS)) {
    die "FATAL: invalid options or option values\n\n",&_usage();
}

if ($help) { print &_usage(); exit; }

unless (defined($params{config_dbfile})) {
    Helicos::Exception::Simple->throw("config_dbfile is required\n");
}


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
my $palmer_dir = join("/",$analysis_dir,'palmer');

my $log_file = join("/",$log_dir,'analysis_protocol_oligo.log');

chdir($basedir);

my $log_fh = IO::File->new(">$log_file");
$log_fh or Helicos::Exception::IO->throw("unable to open log file <$log_file> for writing\n");
$log_fh->autoflush(1);

### SET UP WORKSPACE ###

print $log_fh &timestamped_string("Setting up workspace.\n");

{   
    my @dirs = ($ref_dir,$filtersms_dir,$palmer_dir,$lengthtool_dir,$errortool_dir,$lengthtoollite_dir);
    if (@positions) { push @dirs, $extractsms_dir; }
    foreach my $path (@dirs)
    {
	unless(mkpath($path)) {
	    Helicos::Exception::Simple->throw("unable to create directory <$path>\n");
	}
    }
}

### GET ANALYSIS PROTOCOL PARAMETERS ###

my %analysis_protocol_parameters;

my $hpdp_config_file;
my $termination_loss_reference_position_min;
my $termination_loss_reference_position_max;

{
    foreach my $parameter_name ( 'usable_strand_length','usable_strand_maximum_errors',
				 'minimum_read_length',
				 'exclude_control_frame_positive_strands','maximum_orphan_object_quality_score',
				 'trim_leading_T','trim_leading_T_frequency',
				 'maximum_bao_dinucleotide_frequency',
				 'alignment_mode','lengthtool_sample_size','errortool_base_sample_size',
				 'alignment_uniqueness_option','alignment_percent_error','alignment_minimum_normalized_score',
				 'termination_loss_minimum_cycles','termination_loss_reference_position_range',
				 'fill_and_lock','fill_and_lock_trim_references'  ) {
	$analysis_protocol_parameters{$parameter_name} = $step->parameter_by_name($parameter_name)->value;
    }
    
    my $hpdp_config_file_name;
    if ($analysis_protocol_parameters{alignment_mode} eq 'Left-Justified-Global-Local') {
	$hpdp_config_file_name='hpdp_LJ_noHP_config';
    }
    elsif ($analysis_protocol_parameters{alignment_mode} eq 'Global-Local') {
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
    
    unless (is_positive_integer($analysis_protocol_parameters{usable_strand_length}) and $analysis_protocol_parameters{usable_strand_length}>=10) {
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{usable_strand_length}> for parameter <usable_strand_length>\n");
    }
    
    unless (is_non_negative_integer($analysis_protocol_parameters{usable_strand_maximum_errors}) and $analysis_protocol_parameters{usable_strand_maximum_errors}<=3) {
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{usable_strand_maximum_errors}> for parameter <usable_strand_maximum_errors>\n");
    }

    unless (is_non_negative_integer($analysis_protocol_parameters{minimum_read_length}) and $analysis_protocol_parameters{minimum_read_length}>=6) {
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{minimum_read_length}> for parameter <minimum_read_length>\n");
    }

    unless (is_boolean($analysis_protocol_parameters{exclude_control_frame_positive_strands})) {
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{exclude_control_frame_positive_strands}> for parameter <exclude_control_frame_positive_strands>\n");
    }

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

    unless (is_positive_integer($analysis_protocol_parameters{lengthtool_sample_size})) {
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{lengthtool_sample_size}> for parameter <lengthtool_sample_size>\n");
    }

    unless (is_positive_integer($analysis_protocol_parameters{errortool_base_sample_size})) {
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{errortool_base_sample_size}> for parameter <errortool_base_sample_size>\n");
    }
    
    unless (is_boolean($analysis_protocol_parameters{alignment_uniqueness_option})) {
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{alignment_uniqueness_option}> for parameter <alignment_uniqueness_option>\n");
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

    unless (is_positive_integer($analysis_protocol_parameters{termination_loss_minimum_cycles})) {
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{termination_loss_minimum_cycles}> for parameter <termination_loss_minimum_cycles>\n");
    }

    {
	unless ($analysis_protocol_parameters{termination_loss_reference_position_range}=~/^(\d+):(\d+)$/) {
	    Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{termination_loss_reference_position_range}> for parameter <termination_loss_reference_position_range>\n");
	}
	($termination_loss_reference_position_min,$termination_loss_reference_position_max)=($1,$2);
	
	unless (is_positive_integer($termination_loss_reference_position_min) and
		is_positive_integer($termination_loss_reference_position_max) and
		$termination_loss_reference_position_max>=$termination_loss_reference_position_min) {
	    Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{termination_loss_reference_position_range}> for parameter <termination_loss_reference_position_range>\n");
	}
    }

    unless (is_boolean($analysis_protocol_parameters{fill_and_lock})) {
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{fill_and_lock}> for parameter <fill_and_lock>\n");
    }

    unless (is_boolean($analysis_protocol_parameters{fill_and_lock_trim_references})) {
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{fill_and_lock_trim_references}> for parameter <fill_and_lock_trim_references>\n");
    }
    
    if ($analysis_protocol_parameters{trim_leading_T} and $analysis_protocol_parameters{trim_leading_T_frequency}<0.5) {
	Helicos::Exception::Simple->throw("invalid value <$analysis_protocol_parameters{trim_leading_T_frequency}> for parameter <trim_leading_T_frequency>\n");
    }

    if ($analysis_protocol_parameters{trim_leading_T} and ! $analysis_protocol_parameters{fill_and_lock}) {
	Helicos::Exception::Simple->throw("invalid combination of parameters: trim_leading_T <$analysis_protocol_parameters{trim_leading_T}> and fill_and_lock <$analysis_protocol_parameters{fill_and_lock}>\n");
    }

    if ($analysis_protocol_parameters{alignment_mode} eq 'Left-Justified-Global-Local' and
	($analysis_protocol_parameters{fill_and_lock_trim_references} or $analysis_protocol_parameters{trim_leading_T}) ) {
	Helicos::Exception::Simple->throw("invalid combination of parameters: alignment_mode <$analysis_protocol_parameters{alignment_mode}>, fill_and_lock_trim_references <$analysis_protocol_parameters{fill_and_lock_trim_references}>, trim_leading_T <$analysis_protocol_parameters{trim_leading_T}>\n");
    }
}

### BUILD REFERENCE FILES ###

print $log_fh &timestamped_string("Building reference files.\n");

my %reference_files;
my @ordered_ref_ids;

{
    my %ids;
    
    #generate common base addition order artifact sequence
    my $bao_record;
    {
	my $flow_order_string = $run->flow_order->flow_order_string;
	my $nuc_challenge_frame_string = $flow_order_string;
	$nuc_challenge_frame_string=~tr/ACGT//cd;
	$bao_record=">$base_addition_order_id\n$nuc_challenge_frame_string\n";
    }

    my $reference_set = $protocol->reference_sequence_set;
    my $reference_set_id = $reference_set->id;
    
    my $prefix="references";
    
    my @exclude_records;
    
    %reference_files = ( main    => join("/",$ref_dir,"${prefix}.with_artifacts.fasta"),
			 exclude => join("/",$ref_dir,"${prefix}.exclude.fasta"),
			 palmer  => join("/",$ref_dir,"${prefix}.palmer.fasta") );
    
    if ($analysis_protocol_parameters{fill_and_lock}) {
	$reference_files{trimmed} = join("/",$ref_dir,"${prefix}.trimmed_with_artifacts.fasta");
    }
    
    my %handles;
    foreach my $file_type (sort keys %reference_files) {
	my $filepath=$reference_files{$file_type};
	my $fh = IO::File->new(">$filepath");
	$fh or Helicos::Exception::IO->throw("unable to open file <$filepath> for writing\n");
	$handles{$file_type}=$fh;
    }
    
    push @exclude_records,$bao_record;
    
    my @components = @{$reference_set->components};
    
    my $component_copy_dir = join("/",$ref_dir,'original_components');
    
    unless(mkpath($component_copy_dir)) {
	Helicos::Exception::Simple->throw("unable to create directory <$component_copy_dir>\n");
    }

    foreach my $component (@components) {
	my %file_ids;
	my $reported_number_of_sequences = $component->number_of_sequences;
	my $filepath = $component->uri->path;
	-f $filepath or Helicos::Exception::Simple->throw("reference sequence file <$filepath> not found\n");
	defined($component->pass) and Helicos::Exception::Simple->throw("pass attribute for reference sequence for protocol <",$protocol->name,"> not valid\n");
	
	my $ref_class = $component->reference_class;
	
	my($filename)=fileparse($filepath);
	
	# copy original component fasta files
	# this assumes fasta files have unique names

	my $component_file;

	{
	    print &timestamped_string("Copying original reference set component file <$filepath> to results directory <$component_copy_dir>.\n");
	    $component_file=join("/",$component_copy_dir,$filename);
	    -e $component_file and die;
	    copy($filepath,$component_file) or Helicos::Exception::Simple->throw("copy of original reference set component file <$filepath> to <$component_file> failed.\n");
	}
	
	my $fh=IO::File->new($component_file);
	$fh or Helicos::Exception::IO->throw("unable to open file <$component_file> for reading\n");
	
	my $ofh=$handles{main};
	my $palmer_ofh=$handles{palmer};
	
	my $rec;
	
	while (defined($rec=&get_next_fasta_record($fh))) {
	    warn(Dumper($rec)) if $debug;

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
		next;
	    }

	    push @ordered_ref_ids,$rec->{id};

	    my $hdr;
	    if (defined($rec->{definition})) {
		$hdr = join("",">",$rec->{id}," $rec->{definition}");
	    }
	    else { $hdr = join("",">",$rec->{id}); }
	    
	    if (length($rec->{sequence}) < $analysis_protocol_parameters{usable_strand_length}) {die;}
	    
	    {
		my $tmp = substr($rec->{sequence},0,$analysis_protocol_parameters{usable_strand_length});
		if ($tmp=~/(AA|CC|GG|TT)/) {
		    $has_hp_refs=1;
		}
	    }

	    my $palmer_seq = substr($rec->{sequence},0,$analysis_protocol_parameters{usable_strand_length});
	    
	    if ($analysis_protocol_parameters{fill_and_lock}) {
		#make a version of the reference sequence in which any leading T's, plus the first non-T are removed
		# we are assuming this has not yet been done !
		my $tmp = $rec->{sequence};
		$tmp=~s/^T*[^T]//;
		#TODO: sanity checks on resulting sequence
		my $fh = $handles{trimmed};
		print $fh join("\n",$hdr,$tmp),"\n";

		#TODO: long enough??
		$palmer_seq = substr($tmp,0,$analysis_protocol_parameters{usable_strand_length});
	    }
		
	    print $ofh $rec->{raw};
	    print $palmer_ofh join("\n",$hdr,$palmer_seq),"\n";
	}
	
	close($fh) or die;
	
	my $observed_number_of_sequences = scalar(keys %file_ids);
	unless($observed_number_of_sequences == $reported_number_of_sequences) {
	    Helicos::Exception::Simple->throw("reported number <$reported_number_of_sequences> or reference sequences does not match observed <$observed_number_of_sequences>\n");
	}
    }
    
    #append artifacts to main reference file
    {
	my $ofh = $handles{main};
	print $ofh join("",@exclude_records);
    }

    #append artifacts to trimmed reference file (if any)

    if ($analysis_protocol_parameters{fill_and_lock}) {
	my $ofh = $handles{trimmed};
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
}  

#if ($has_hp_refs) { print $log_fh &timestamped_string("-- homopolymer context in reference(s) identified - including HP context in error reporting.\n"); }

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
	push @args, "--input_file","$params{sms_file}";
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

print $log_fh &timestamped_string("Filtering strands in original SMS file.\n");

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
	push @args, "--orphan","$analysis_protocol_parameters{maximum_orphan_object_quality_score}";

	push @args, "--dinuc","$dinuc_file";

	if ($analysis_protocol_parameters{trim_leading_T}) {
	    push @args, "--trim","T/H/$analysis_protocol_parameters{trim_leading_T_frequency}";
	}
	
	if (! $analysis_protocol_parameters{exclude_control_frame_positive_strands}) {
	    push @args, "--noctrl";
	}
	
	my $cmd = join(" ",$program_name,@args);
	
	print $log_fh &timestamped_string("Excecuting command: $cmd\n");

	my $stats = &execute_command("$cmd > $stdout 2> $stderr");

	print $log_fh &timestamped_string(join("","--- timing: ",&timing_string($stats),"\n"));
    }
    chdir($basedir);
}

### RUN PALMER ###

print $log_fh &timestamped_string("Running palmer tool.\n");

{
    my $reference_file_palmer = $reference_files{palmer};

    chdir($palmer_dir);
    {
	my $program_name = 'palmer';
	my @common_args;
	push @common_args, "--read_file","$filtered_sms_file";
	push @common_args, "--reference_file_pass1","$reference_file_palmer";
	push @common_args, "--max_errors_pass1","$analysis_protocol_parameters{usable_strand_maximum_errors}";
	push @common_args, "--discard_length","6";
	
	foreach my $flow_cell_id (1..$flow_cell_count) {
	    my @channels=@{$protocol->channels_for_flow_cell($flow_cell_id)};
	    if (@channels) {
		my @args;

		my $stdout = "palmer.fc${flow_cell_id}.stdout.txt";
		my $stderr = join("/",$log_dir,"palmer.fc${flow_cell_id}.err");
		my $hits_file_name = "palmer.hits.fc${flow_cell_id}.csv";
		my $spikes_file_name = "palmer.spikes.fc${flow_cell_id}.sms";
		my $discards_file_name = "palmer.discards.fc${flow_cell_id}.sms";
		
		push @args, "--flow_cells","$flow_cell_id";
		push @args, "--channels",join(",",@channels);
		push @args, "--hits","$hits_file_name";
		#push @args, "--discards","$discards_file_name";
		
		my $cmd = join(" ",$program_name,@common_args,@args);

		print $log_fh &timestamped_string("Excecuting command: $cmd\n");

		my $stats = &execute_command("$cmd > $stdout 2> $stderr");
		
		print $log_fh &timestamped_string(join("","--- timing: ",&timing_string($stats),"\n"));
	    }
	}
    }
    chdir($basedir);
}

### RUN LENGTHTOOLLITE ###

print $log_fh &timestamped_string("Running lengthToolLite tool.\n");

{
    my $raw_sms_file_to_process = defined($extract_sms_file) ? $extract_sms_file : $raw_sms_file;

    chdir($lengthtoollite_dir);
	
    {
	my $program_name = 'lengthToolLite';
	my @common_args;
	
	foreach my $file ($raw_sms_file_to_process,$filtered_sms_file) {
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
    my $reference_file = $analysis_protocol_parameters{fill_and_lock_trim_references} ? $reference_files{trimmed} : $reference_files{main};
    my $exclude_reference_file = $reference_files{exclude};

    chdir($lengthtool_dir);

    {
	my $program_name = 'lengthTool';
	my @common_args;
	push @common_args, "--read_file","$filtered_sms_file";
	push @common_args, "--reference_file","$reference_file";
	push @common_args, "--excluded_references_file","$exclude_reference_file";
	push @common_args, "--sample_size","$analysis_protocol_parameters{lengthtool_sample_size}";
	push @common_args, "--term_cycles","$analysis_protocol_parameters{termination_loss_minimum_cycles}";
	push @common_args, "--uniqueness_option","$analysis_protocol_parameters{alignment_uniqueness_option}";
	push @common_args, "--percent_error","$analysis_protocol_parameters{alignment_percent_error}";
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
    chdir($errortool_dir);

    my($x_dim,$y_dim,$bin_size)=(1392,1040,100);

    my $sample_size_by_nuc = $analysis_protocol_parameters{errortool_base_sample_size} * 4;
    my $sample_size_by_cycle = $analysis_protocol_parameters{errortool_base_sample_size} * 10;
    my $sample_size_by_imageXY = $analysis_protocol_parameters{errortool_base_sample_size} * 10;
    my $sample_size_by_reference_position = $analysis_protocol_parameters{errortool_base_sample_size} * 10;
    my $sample_size_by_cycle_by_position = 1000;
    my $sample_size_by_cycle_by_qc_position = $sample_size_by_cycle;

    my @analysis_types = qw(general by_nuc by_detailed_substitutions by_imageXY by_reference_position by_cycle);
    my @subdirs = qw(by_cycle_summary by_cycle_by_position by_cycle_by_position_sample);
    
    if ($analysis_protocol_parameters{fill_and_lock}) {
	push @analysis_types, "by_nuc_prefix";
	push @subdirs, "prefix_analysis"  #--prefix_analysis
    }

    foreach my $dname (@subdirs) {
	my $tmpname = "${errortool_dir}/${dname}";
	unless(mkpath("${tmpname}")) {
	    Helicos::Exception::Simple->throw("unable to create directory <$tmpname>\n");
	}
    }
    
    my $exclude_reference_file = $reference_files{exclude};
    
    my $program_name = 'errorTool';
    my @common_args;
    push @common_args, "--read_file","$filtered_sms_file";
    push @common_args, "--excluded_references_file","$exclude_reference_file";
    push @common_args, "--uniqueness_option","$analysis_protocol_parameters{alignment_uniqueness_option}";
    push @common_args, "--percent_error","$analysis_protocol_parameters{alignment_percent_error}";
    push @common_args, "--first_read_number","1";
    push @common_args, "--last_read_number","1";
    push @common_args, "--min_normalized_score","$analysis_protocol_parameters{alignment_minimum_normalized_score}";
    push @common_args, "--fixed_random_seed","1";  #really just useful when sample size is uniform

    foreach my $analysis_type (@analysis_types) {

	my @targs;

	if ($analysis_type eq 'by_nuc_prefix') {
	    push @targs, "--analysis_type","by_nuc";
	    push @targs, "--reference_file","$reference_files{trimmed}";
	    push @targs, "--config_file","$prefix_mode_hpdp_config_file";
	}
	else {
	    my $reference_file = $analysis_protocol_parameters{fill_and_lock_trim_references} ? $reference_files{trimmed} : $reference_files{main};
	    push @targs, "--analysis_type","$analysis_type";
	    push @targs, "--reference_file","$reference_file";
	    push @targs, "--config_file","$hpdp_config_file";
	}


	if ($analysis_type eq 'general') {
	    push @targs, "--sample_size","$analysis_protocol_parameters{errortool_base_sample_size}";
	    push @targs, "--by_camera";          #needed?
	}
	elsif ($analysis_type eq 'by_nuc') {
	    push @targs, "--sample_size","$sample_size_by_nuc";
	    push @targs, "--by_camera";
	    push @targs, "--summary_output";
	}
	elsif ($analysis_type eq 'by_nuc_prefix') {
	    chdir("${errortool_dir}/prefix_analysis");
	    push @targs, "--sample_size","$sample_size_by_nuc";
	    push @targs, "--by_camera";
	    push @targs, "--summary_output";
	}
	elsif ($analysis_type eq 'by_detailed_substitutions') {
	    push @targs, "--sample_size","$sample_size_by_nuc";
	    push @targs, "--by_camera";
	    #push @targs, "--summary_output"; #not implemented
	}
	elsif ($analysis_type eq 'by_imageXY') {
	    push @targs, "--sample_size","$sample_size_by_imageXY";
	    push @targs, "--by_camera";
	    push @targs, "--X_dim","$x_dim";
	    push @targs, "--Y_dim","$y_dim";
	    push @targs, "--bin_side","$bin_size";
	}
	elsif ($analysis_type eq 'by_reference_position') {
	    push @targs, "--sample_size","$sample_size_by_reference_position";
	    push @targs, "--by_reference_accounting";
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
	
	chdir($errortool_dir);    
    }

    {
	my $analysis_type = 'by_cycle';

	my $sample_size_by_cycle_by_position = 1000;
	my $sample_size_by_cycle_by_qc_position = $sample_size_by_cycle;
	
	foreach my $subtype (qw(by_position by_position_sample)) {
	    
	    my $wdir = "${errortool_dir}/by_cycle_${subtype}";
	    my $sample_size_to_use;

	    if ($subtype eq 'by_position') {
		$sample_size_to_use = $sample_size_by_cycle_by_position;
	    }
	    elsif ($subtype eq 'by_position_sample') {
		next unless @qc_positions;
		$sample_size_to_use = $sample_size_by_cycle_by_qc_position;
	    }
	    
	    chdir("${wdir}") or die;
	    
	    my @targs;
	    my $reference_file = $analysis_protocol_parameters{fill_and_lock_trim_references} ? $reference_files{trimmed} : $reference_files{main};
	    push @targs, "--reference_file","$reference_file";
	    push @targs, "--config_file","$hpdp_config_file";
	    push @targs, "--analysis_type","$analysis_type";
	    push @targs, "--sample_size","$sample_size_to_use";

	    foreach my $flow_cell_id (1..$flow_cell_count) {
		my @channels=@{$protocol->channels_for_flow_cell($flow_cell_id)};		
		if (@channels) {
		    my @fargs;
		    push @fargs, "--flow_cells","$flow_cell_id";

		    my @ppc_channel_sets = &get_channel_sets_by_ppc_for_flow_cell($flow_cell_id);
		    
		    foreach my $ppc_channel_set (@ppc_channel_sets) {
			my $number_of_positions = $ppc_channel_set->[0];
			my @set_channels = @{$ppc_channel_set->[1]};

			my @cargs;
			push @cargs, "--channels",join(",",@set_channels);

			my @by_cycle_positions;

			my $pos_file_tag;
			if ($subtype eq 'by_position')
			{
			    @by_cycle_positions = (1..$number_of_positions);
			    $pos_file_tag="all";
			}
			elsif ($subtype eq 'by_position_sample') {
			    @by_cycle_positions = grep {$_<=$number_of_positions} @qc_positions;
			    $pos_file_tag="qc";
			}
			else {die;}
			
			if (@positions) {
			    my %p = map {$_=>1} @positions;
			    my @found = grep {exists($p{$_}) ? 1 : 0;} @by_cycle_positions;
			    @by_cycle_positions = @found;
			}
			
			next unless @by_cycle_positions;

			my $positions_file = "positions.fc${flow_cell_id}.ppc${number_of_positions}.${pos_file_tag}.txt";
			
			push @cargs, "--by_position","$positions_file";

			{
			    my $tmpfh=IO::File->new(">$positions_file");
			    $tmpfh or die;
			    print $tmpfh join("\n",@by_cycle_positions),"\n";
			    close($tmpfh) or die;
			}

			my $stdout = join("/",$log_dir,"error_tool.${analysis_type}.${subtype}.fc${flow_cell_id}.ppc${number_of_positions}.out");
			my $stderr = join("/",$log_dir,"error_tool.${analysis_type}.${subtype}.fc${flow_cell_id}.ppc${number_of_positions}.err");
			    
			my $cmd = join(" ",$program_name,@common_args,@targs,@fargs,@cargs);
			
			print $log_fh &timestamped_string("Excecuting command: $cmd\n");
			
			my $stats = &execute_command("$cmd > $stdout 2> $stderr");
			
			print $log_fh &timestamped_string(join("","--- timing: ",&timing_string($stats),"\n"));
		    }
		}
	    }
	}
	chdir($errortool_dir);
    }

    chdir($basedir);
}


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
										   ordered_reference_ids=>[@ordered_ref_ids],
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
	    $growth_profile->generate_termloss_report(file=>$outfile,
						      type=>'summary',
						      min_reference_position=>$termination_loss_reference_position_min,
						      max_reference_position=>$termination_loss_reference_position_max);
	}
	
	{
	    my $outfile_name = "termloss.detail.fc${flow_cell_id}.txt";
	    my $outfile = join("/",$report_dirs{$flow_cell_id},$outfile_name);
	    my $min=$analysis_protocol_parameters{minimum_read_length};
	    my $max=max(15,$termination_loss_reference_position_max+5);
	    $growth_profile->generate_termloss_report(file=>$outfile,
						      type=>'detail',
						      min_reference_position=>$min,
						      max_reference_position=>$max);
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
    my %context_param = ();  #$has_hp_refs ? () : (context=>'non-HP');

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

print $log_fh &timestamped_string("Protocol complete.\n");

close($log_fh) or die;

exit;

######################################################################

sub get_channel_sets_by_ppc_for_flow_cell
{
    my($flow_cell_id) = @_;
    my %protocol_channels = map {$_=>1}  @{$protocol->channels_for_flow_cell($flow_cell_id)};
    my @channel_sets;
    my @ppc_sets = @{$run->imaging_config->positions_per_channel_config->positions_per_channel_sets};
    foreach my $ppc_set (@ppc_sets) {
	my $number_of_positions = $ppc_set->number_of_positions;
	my @channel_sets_tmp = @{$ppc_set->channel_sets};
	foreach my $channel_set (@channel_sets_tmp) {
	    my $fc = $channel_set->flow_cell;
	    my @channels =@{$channel_set->channels};
	    next unless ($flow_cell_id eq $fc);
	    @channels = grep {exists($protocol_channels{$_}) ? 1 : 0;} @channels;
	    next unless @channels;
	    push @channel_sets, [$number_of_positions,\@channels];
	}
    }
    return @channel_sets;
}

sub get_max_positions_per_channel
{
    my @ppc_sets = @{$run->imaging_config->positions_per_channel_config->positions_per_channel_sets};
    my $max_ppc;
    foreach my $ppc_set (@ppc_sets) {
	my $number_of_positions = $ppc_set->number_of_positions;
	next if (defined($max_ppc) and $max_ppc>$number_of_positions);
	$max_ppc=$number_of_positions;
    }
    return $max_ppc;
}

sub get_sample_positions
{
    #my($flow_cell_id)=@_;
    my @sample_positions;
    my $max_ppc = &get_max_positions_per_channel();
    @sample_positions = (grep {$_<=$max_ppc} @qc_positions);
    return @sample_positions;
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

sub _usage
{
    my $usage;
    my @lines;
    push @lines,"Synopsis:";
    push @lines,"     analysis_protocol_tsms.pl --sms_file=path_to_strand_file --config_dbfile=path_to_config_file [options]";
    push @lines,"     analysis_protocol_tsms.pl --help";
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

