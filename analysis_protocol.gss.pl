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
# FILE:          analysis_protocol.gss.pl
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
use File::Path;
use File::Copy;
use File::Basename;
use Data::Dumper;
use Cwd qw(chdir getcwd);
use MLDBM qw(DB_File Storable);     # DB_File and Storable

use Helicos::Util qw(execute_command);
use Helicos::Exception::Classes;
use Helicos::MetaData::Classes;

use Helicos::SequenceAnalysis::Pipeline::Environment;
use Helicos::SequenceAnalysis::Pipeline::Util qw(output_header get_next_fasta_record get_channel_blocks timestamped_string formatted_datetime timing_string);
use Helicos::SequenceAnalysis::Pipeline::FilterProfile;
use Helicos::SequenceAnalysis::Pipeline::GrowthProfile;
use Helicos::SequenceAnalysis::Pipeline::ErrorProfile;
use Helicos::SequenceAnalysis::Pipeline::YieldProfile;

$REVISION = "";
$VERSION = "1.0.0";

$| = 1;

my $help = 0;
my $verbose = 0;
my $debug = 0;

my $HELICOS_ANALYSIS_HOME = Helicos::SequenceAnalysis::Pipeline::Environment->get_value("HELICOS_ANALYSIS_HOME");
my $HELICOS_ANALYSIS_WORKSPACE_ROOT = Helicos::SequenceAnalysis::Pipeline::Environment->get_value("HELICOS_ANALYSIS_WORKSPACE_ROOT");

#============================
# Hardcoded Defaults

my $base_addition_order_id = 'BASE_ADDITION_ORDER_REFERENCE';
my $term_loss_min_length=6;
my $term_loss_max_length=11;

my $schema_dir = "${HELICOS_ANALYSIS_HOME}/config/descriptors/schema";

#TODO: parse from frame group config
my %pass_name_to_number = ('SampleBarcodePass'=>1,'TranscriptBarcodePass'=>2);
my %pass_number_to_name = reverse(%pass_name_to_number);
my @pass_names=sort keys %pass_name_to_number;

my $pass1_name = $pass_number_to_name{1};
my $pass2_name = $pass_number_to_name{2};

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

my %environment;
my $analysis;

{
    my %conf;
    my $db = tie %conf, 'MLDBM', $params{config_dbfile}, O_RDONLY or die $!;
    %environment=%{$conf{environment}};
    $analysis=$conf{analysis};
    undef $db;
    untie(%conf);
}

my $run = $analysis->run;
my $instrument = $run->instrument;
my $instrument_name = $instrument->name;
my $run_name = $run->name;
my $analysis_name = $analysis->name;

my @qc_positions;
{
    my $qcpos=$analysis->run->qc_positions;
    if (defined($qcpos)) {@qc_positions=@$qcpos;}
}
my @positions;

my $basedir = join("/",$HELICOS_ANALYSIS_WORKSPACE_ROOT,$instrument_name,$run_name,$analysis_name);
my $ref_dir = join("/",$basedir,'reference_data');
my $reports_dir = join("/",$basedir,'reports');
my $log_dir = join("/",$basedir,'logs');
my $log_file = join("/",$log_dir,'analysis_protocol_gss.log');

my $number_of_cameras = scalar(@{$instrument->cameras});

my $launch_dir = getcwd();

chdir($basedir);

my $log_fh = IO::File->new(">$log_file");
$log_fh or Helicos::Exception::IO->throw("unable to open log file <$log_file> for writing\n");
$log_fh->autoflush(1);

### SET UP WORKSPACE ###

{
    unless(mkpath($ref_dir)) {
	Helicos::Exception::Simple->throw("unable to create directory <$ref_dir>\n");
    }
}

my $protocol;
my $step;

{
    my($program_name)=fileparse($0);
    my @protocols = grep {$_->name eq 'GSS_standard'} @{$analysis->protocols};
    @protocols==1 or Helicos::Exception::Simple->throw("unable to find protocol matching name <GSS_standard>\n");
    $protocol = $protocols[0];
    my $precedure = $protocol->procedure;
    my @steps = grep {$_->program->name eq $program_name} @{$precedure->steps};
    @steps==1 or Helicos::Exception::Simple->throw("unable to find procedure step matching program name <$0>\n");
    $step = $steps[0];
}

{
    my $pos=$protocol->positions;
    if (defined($pos)) {
	@positions=@$pos;
    }
}

my @flow_cell_ids = @{$protocol->flow_cell_ids};
my @cameras = (0..($number_of_cameras-1));

my $flow_cell_count = $instrument->number_of_flow_cells;
my $base_sample_size = $step->parameter_by_name('errortool_base_sample_size')->value;
my $uniqueness_option = $step->parameter_by_name('errortool_uniqueness_option')->value;
my $hpdp_config_file = join("/",${HELICOS_ANALYSIS_HOME},'config/tools/HPDP/hpdp_LJ_noHP_config');
my $bithpdp_percent_error = $step->parameter_by_name('errortool_bithpdp_percent_error')->value;
my $use_spikes_for_error = $step->parameter_by_name('use_spikes_for_error_assessment')->value;
my $sample_barcode_default_length = $step->parameter_by_name('sample_barcode_default_length')->value;
my $sample_barcode_max_errors = $step->parameter_by_name('sample_barcode_max_errors')->value;
my $transcript_barcode_default_length = $step->parameter_by_name('transcript_barcode_default_length')->value;
my $transcript_barcode_max_errors = $step->parameter_by_name('transcript_barcode_max_errors')->value;

my %barcode_lengths = (SampleBarcodePass => $sample_barcode_default_length,
		       TranscriptBarcodePass => $transcript_barcode_default_length);

my %barcode_errors = (SampleBarcodePass => $sample_barcode_max_errors,
		      TranscriptBarcodePass => $transcript_barcode_max_errors);

my $minimum_read_length = $step->parameter_by_name('minimum_read_length')->value;
my $maximum_orphan_object_quality_score = $step->parameter_by_name('maximum_orphan_object_quality_score')->value;
my $minimum_normalized_alignment_score = $step->parameter_by_name('minimum_normalized_alignment_score')->value;

#TO DO: clean up and expand sanity checks on values
#($minimum_read_length=~/^\d+$/ and $minimum_read_length>=6) or die;
($minimum_read_length=~/^\d+$/) or die;
($maximum_orphan_object_quality_score=~/^\d+$/) or die;
($minimum_normalized_alignment_score=~/^(\d+(\.\d*)?|(\d+)?\.\d+)$/ and $minimum_normalized_alignment_score<=5) or die;


### BUILD REFERENCE FILES ###

#TODO: build separate BAO reference for the two passes (e.g. if the flow order string changes and we still want LJ alignments to be OK)

# We need to construct 2 sets of reference files for each pass:
#       palmer reference (just the barcode portion of each reference sequence)
#       full reference (original reference sequence - needed for alignment-based analysis

my $reference_set_id;
my %reference_files;
my %ordered_reference_ids;

{
    foreach my $pass_name (@pass_names) {
	my $pass = $pass_name_to_number{$pass_name};
	my $prefix="reference_set_pass${pass}";
	
	$reference_files{$pass} = { main    => join("/",$ref_dir,"${prefix}.with_artifacts.fasta"),
				    exclude => join("/",$ref_dir,"${prefix}.exclude.fasta"),
				    palmer  => join("/",$ref_dir,"${prefix}.palmer.fasta") };
    }

    my @reference_sequence_sets = @{$protocol->reference_sequence_sets};    
    
    @reference_sequence_sets==1 or Helicos::Exception::Simple->throw("can only have a single reference set\n");

    my $reference_sequence_set = $reference_sequence_sets[0];
    $reference_set_id=$reference_sequence_set->id;

    #generate common base addition order artifact sequence
    my $bao_record;
    {
	my $flow_order_string = $run->flow_order->flow_order_string;
	my $nuc_challenge_frame_string = $flow_order_string;
	$nuc_challenge_frame_string=~tr/ACGT//cd;
	$bao_record=">$base_addition_order_id\n$nuc_challenge_frame_string\n";
    }

    my %pass2components = map {$_=>[]} (1,2);

    {
	my @components = @{$reference_sequence_set->components};
	@components==2 or Helicos::Exception::Simple->throw("unexpected number of components\n");
	foreach my $component (@components) {
	    my $pass_name = $component->pass;
	    defined($pass_name) or Helicos::Exception::Simple->throw("pass attribute required for protocol <",$protocol->name,">\n");
	    
	    my $pass_num = $pass_name_to_number{$pass_name};
	    push @{$pass2components{$pass_num}},$component;
	}
    }

    foreach my $pass (1,2) {
	my %ids;
	my %handles;
	foreach my $file_type (sort keys %{$reference_files{$pass}}) {
	    my $filepath=$reference_files{$pass}->{$file_type};
	    my $fh = IO::File->new(">$filepath");
	    $fh or Helicos::Exception::IO->throw("unable to open file <$filepath> for writing\n");
	    $handles{$file_type}=$fh;
	}

	#TODO: get alt barcode length from fasta header
	my $barcode_length = $barcode_lengths{$pass_number_to_name{$pass}};
	$barcode_length or die;
	
	my @ordered_ref_ids;
	my @exclude_records;
	push @exclude_records,$bao_record;
	
	my @components = @{$pass2components{$pass}};

	foreach my $component (@components) {
	    my %file_ids;
	    my $reported_number_of_sequences = $component->number_of_sequences;
	    my $filepath = $component->uri->path;
	    -f $filepath or Helicos::Exception::Simple->throw("reference sequence file <$filepath> not found\n");
	    
	    my($filename)=fileparse($filepath);
	    
	    #make copy of original file?
	    #my $copy = join("/",$ref_dir,$filename);
	    #copy($filepath,$copy) or Helicos::Exception::Simple->throw("copy of reference file <$filepath> to <$copy> failed\n");

	    my $fh=IO::File->new($filepath);
	    $fh or Helicos::Exception::IO->throw("unable to open file <$filepath> for reading\n");
	    
	    my $ofh=$handles{main};
	    my $palmer_ofh=$handles{palmer};
	    
	    my $rec;
	    
	    while (defined($rec=&get_next_fasta_record($fh))) {
		warn(Dumper($rec)) if $debug;
		exists($ids{$rec->{id}}) and Helicos::Exception::Simple->throw("reference sequence with id <$rec->{id}> not unique in reference file(s) for reference set\n");
		$rec->{id} eq $base_addition_order_id and Helicos::Exception::Simple->throw("reference sequence id in reference file <$filepath> matched base addition order id <$base_addition_order_id>\n");
		$ids{$rec->{id}}++;
		$file_ids{$rec->{id}}++;
		push @ordered_ref_ids,$rec->{id};
		my $hdr;
		if (defined($rec->{definition})) {
		    $hdr = join("",">",$rec->{id}," $rec->{definition}");
		}
		else { $hdr = join("",">",$rec->{id}); }
		
		if (length($rec->{sequence}) < $barcode_length) {die;}
		
		print $ofh $rec->{raw};
		print $palmer_ofh join("\n",$hdr,substr($rec->{sequence},0,$barcode_length)),"\n";
	    }
	    
	    close($fh) or die;
	    
	    my $observed_number_of_sequences = scalar(keys %file_ids);
	    unless($observed_number_of_sequences == $reported_number_of_sequences) {
		Helicos::Exception::Simple->throw("reported number <$reported_number_of_sequences> or reference sequences does not match observed <$observed_number_of_sequences>\n");
	    }
	}
	
	$ordered_reference_ids{$pass}=\@ordered_ref_ids;

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
	
	foreach my $file_type (sort keys %{$reference_files{$pass}}) {
	    my $filepath=$reference_files{$pass}->{$file_type};
	    my $fh = $handles{$file_type};
	    close($fh) or Helicos::Exception::IO->throw("error on close of file <$filepath>\n");
	}
    }    
}

warn Dumper(\%barcode_lengths,\%barcode_errors,\%reference_files,\%ordered_reference_ids) if $debug;

my $extract_sms_file;
my $filtered_sms_file;
{
    my($tmp)=fileparse($params{sms_file});
    $tmp=~s/\.sms$//i;
    $extract_sms_file = join("/",$basedir,"data","${tmp}.extract.sms") if @positions;
    $filtered_sms_file = join("/",$basedir,"data","${tmp}.filtered.sms");
}

my $palmer_dir = "${basedir}/analysis/palmer";
my $lengthtool_dir = "${basedir}/analysis/length_tool";
my $errortool_dir = "${basedir}/analysis/error_tool";
my $extractsms_dir = "${basedir}/analysis/extract_sms";
my $filtersms_dir = "${basedir}/analysis/filter_sms";

### RUN EXTRACTSMS ###

if (@positions) {

    print $log_fh &timestamped_string("Extracting strands for specified positions from original SMS file.\n");

    unless(mkpath($extractsms_dir)) {
	Helicos::Exception::Simple->throw("unable to create directory <$extractsms_dir>\n");
    }
    
    my $stdout = join("/",$log_dir,"extract_sms.out");
    my $stderr = join("/",$log_dir,"extract_sms.err");
    
    chdir($extractsms_dir);
    {
	my $program_name = 'extractSMS';
	my @common_args;
	push @common_args, "--input_file","$params{sms_file}";
	push @common_args, "--output_file","${extract_sms_file}";
	push @common_args, "--positions",join(",",@positions);
	
	my $cmd = join(" ",$program_name,@common_args);
	
	print $log_fh &timestamped_string("Excecuting command: $cmd\n");

	my $stats = &execute_command("$cmd > $stdout 2> $stderr");

	print $log_fh &timestamped_string(join("","--- timing: ",&timing_string($stats),"\n"));
    }
    chdir($basedir);
}

### RUN FILTERSMS ###

print $log_fh &timestamped_string("Filtering strands in original SMS file.\n");

{
    unless(mkpath($filtersms_dir)) {
	Helicos::Exception::Simple->throw("unable to create directory <$filtersms_dir>\n");
    }
    
    my $stdout = join("/",$log_dir,"filter_sms.out");
    my $stderr = join("/",$log_dir,"filter_sms.err");

    my $infile = defined($extract_sms_file) ? $extract_sms_file : $params{sms_file};

    chdir($filtersms_dir);
    {
	my $program_name = 'filterSMS';
	my @common_args;
	push @common_args, "--input_file","$infile";
	push @common_args, "--output_file","$filtered_sms_file";
	#TODO: filterSMS can not yet handle either/both logic nor report counts for passes separately
	push @common_args, "--minlen","$minimum_read_length";
	push @common_args, "--orphan","$maximum_orphan_object_quality_score";
	#push @common_args, "--dinuc","";
	
	my $cmd = join(" ",$program_name,@common_args);

	print $log_fh &timestamped_string("Excecuting command: $cmd\n");

	my $stats = &execute_command("$cmd > $stdout 2> $stderr");

	print $log_fh &timestamped_string(join("","--- timing: ",&timing_string($stats),"\n"));
    }
    chdir($basedir);
}


### RUN PALMER ###

print $log_fh &timestamped_string("Running palmer tool.\n");

{
    unless(mkpath($palmer_dir)) {
	Helicos::Exception::Simple->throw("unable to create directory <$palmer_dir>\n");
    }

    my $ref1 = $reference_files{1}->{palmer};
    my $ref2 = $reference_files{2}->{palmer};

    my $err1 = $barcode_errors{$pass1_name};
    my $err2 = $barcode_errors{$pass2_name};

    chdir($palmer_dir);
    {
	my $program_name = 'palmer';
	my @common_args;
	push @common_args, "--read_file","$filtered_sms_file";
	push @common_args, "--reference_file_pass1","$ref1";
	push @common_args, "--reference_file_pass2","$ref2";
	push @common_args, "--max_errors_pass1","$err1";
	push @common_args, "--max_errors_pass2","$err2";
	push @common_args, "--discard_length","6";

	foreach my $flow_cell_id (@flow_cell_ids) {
	    my @channels=@{$protocol->channels_for_flow_cell($flow_cell_id)};
	    if (@channels) {
		my @args;

		my $stdout = "palmer.fc${flow_cell_id}.stdout.txt";
		my $stderr = join("/",$log_dir,"palmer.fc${flow_cell_id}.err");
		my $hits_file_name = "palmer.hits.fc${flow_cell_id}.csv";
		my $spikes_file_name = "palmer.spikes.fc${flow_cell_id}.sms";
		my $discards_file_name = "palmer.discards.fc${flow_cell_id}.sms";
		my $hits_pass1_file_name = "palmer.hits.pass1.fc${flow_cell_id}.csv";
		my $hits_pass2_file_name = "palmer.hits.pass2.fc${flow_cell_id}.csv";
		
		push @args, "--flow_cells","$flow_cell_id";
		push @args, "--channels",join(",",@channels);
		push @args, "--hits","$hits_file_name";
		push @args, "--spikes","$spikes_file_name" if $use_spikes_for_error;
		push @args, "--discards","$discards_file_name";
		push @args, "--hits_pass1","$hits_pass1_file_name";
		push @args, "--hits_pass2","$hits_pass2_file_name";

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
    unless(mkpath($lengthtool_dir)) {
	Helicos::Exception::Simple->throw("unable to create directory <$lengthtool_dir>\n");
    }

    my %refs;
    my %erefs;
    $refs{1} = $reference_files{1}->{main};
    $refs{2} = $reference_files{2}->{main};
    $erefs{1} = $reference_files{1}->{exclude};
    $erefs{2} = $reference_files{2}->{exclude};
    
    chdir($lengthtool_dir);
    {
	my $program_name = 'lengthTool';
	my @common_args;
	push @common_args, "--read_file","$filtered_sms_file";
	push @common_args, "--sample_size","$base_sample_size";
	push @common_args, "--term_cycles","20";
	push @common_args, "--uniqueness_option","$uniqueness_option";
	push @common_args, "--percent_error","$bithpdp_percent_error";
	push @common_args, "--config_file","$hpdp_config_file";
	push @common_args, "--min_normalized_score","$minimum_normalized_alignment_score";

	foreach my $flow_cell_id (1..$flow_cell_count) {
	    my @channels=@{$protocol->channels_for_flow_cell($flow_cell_id)};
	    if (@channels) {
		my @fargs;
		push @fargs, "--flow_cells","$flow_cell_id";
		push @fargs, "--channels",join(",",@channels);
		    
		foreach my $pass (1,2) {
		    my @pargs;
		    push @pargs, "--excluded_references_file","$erefs{$pass}";
		    push @pargs, "--passes","$pass";
		    push @pargs, "--reference_file","$refs{$pass}";

		    my $stdout = join("/",$log_dir,"length_tool.fc${flow_cell_id}.pass${pass}.out");
		    my $stderr = join("/",$log_dir,"length_tool.fc${flow_cell_id}.pass${pass}.err");
		    
		    my $cmd = join(" ",$program_name,@common_args,@fargs,@pargs);

		    print $log_fh &timestamped_string("Excecuting command: $cmd\n");
		
		    my $stats = &execute_command("$cmd > $stdout 2> $stderr");
		
		    print $log_fh &timestamped_string(join("","--- timing: ",&timing_string($stats),"\n"));

		    #now rename the newly created growth.pass* output file to have a flow-cell-specific name
		    #(otherwise we overwrite fc1 output - if generated - with fc2 output)
		    
		    my $growth_file = join("/",$lengthtool_dir,"growth.pass${pass}");
		    my $growth_file_renamed = join("/",$lengthtool_dir,"growth.fc${flow_cell_id}.pass${pass}");
		    -f $growth_file or Helicos::Exception::Simple->throw("expected file <$growth_file> does not exist\n");
		    &execute_command("mv",$growth_file,$growth_file_renamed);
		}
	    }
	}
    }
    chdir($basedir);
}

### RUN ERRORTOOL ###
print $log_fh &timestamped_string("Running errorTool tool.\n");

{
    # Currently we need to work around a limitation in errorTool that prevents using single_pass_accounting when specifying only one pass (pass 2 in particular)
    # to process.
    # The problem is that we need to process the passes separately (for GSS) because we can specify only a single reference sequence file.
    # Omitting the separate pass accounting flag results in output files that are named *_passAll_*.  Processing the second pass would overwrite these files.
    # Options:
    #      (1) [preferred and used here]. After processing each pass, rename the output files to match *_pass1_* or *_pass2_* as appropriate.
    #      (2) Use a combined WBC/GBC reference file.


    my($x_dim,$y_dim,$bin_size)=(1392,1040,100);

    my $sample_size_by_nuc = $base_sample_size * 4;
    my $sample_size_by_cycle = $base_sample_size * 10;
    my $sample_size_by_imageXY = $base_sample_size * 10;
    my $sample_size_by_reference_position = $base_sample_size * 10;
    my $sample_size_by_cycle_by_position = 1000;
    my $sample_size_by_cycle_by_qc_position = $sample_size_by_cycle;
    
    unless(mkpath($errortool_dir)) {
	Helicos::Exception::Simple->throw("unable to create directory <$errortool_dir>\n");
    }

    foreach my $dname (qw(by_cycle_summary by_cycle_by_position by_cycle_by_position_sample)) {
	my $tmpname = "${errortool_dir}/${dname}";
	unless(mkpath("${tmpname}")) {
	    Helicos::Exception::Simple->throw("unable to create directory <$tmpname>\n");
	}
    }

    my %refs;
    my %erefs;
    $refs{1} = $reference_files{1}->{main};
    $refs{2} = $reference_files{2}->{main};
    $erefs{1} = $reference_files{1}->{exclude};
    $erefs{2} = $reference_files{2}->{exclude};
    
    chdir($errortool_dir);

    my $program_name = 'errorTool';
    my @common_args;
    push @common_args, "--read_file","$filtered_sms_file";
    push @common_args, "--uniqueness_option","$uniqueness_option";
    push @common_args, "--percent_error","$bithpdp_percent_error";
    push @common_args, "--config_file","$hpdp_config_file";
    push @common_args, "--min_normalized_score","$minimum_normalized_alignment_score";
    #push @common_args, "--separate_pass_accounting";
  
    foreach my $analysis_type (qw(general by_nuc by_detailed_substitutions by_imageXY by_reference_position)) {
	my @targs;
	push @targs, "--analysis_type","$analysis_type";
	if ($analysis_type eq 'general') {
	    push @targs, "--sample_size","$base_sample_size";
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
	    #push @targs, "--by_camera";
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

		my @channel_blocks = &get_channel_blocks(@channels);

		foreach my $pass (1,2) {
		    my @pargs;
		    push @pargs, "--reference_file","$refs{$pass}";
		    push @pargs, "--excluded_references_file","$erefs{$pass}";
		    push @pargs, "--first_read_number","$pass";
		    push @pargs, "--last_read_number","$pass";

		    my $stdout = join("/",$log_dir,"error_tool.${analysis_type}.fc${flow_cell_id}.pass${pass}.out");
		    my $stderr = join("/",$log_dir,"error_tool.${analysis_type}.fc${flow_cell_id}.pass${pass}.err");
			
		    my $cmd = join(" ",$program_name,@common_args,@targs,@fargs,@pargs);
		    
		    print $log_fh &timestamped_string("Excecuting command: $cmd\n");
		    
		    my $stats = &execute_command("$cmd > $stdout 2> $stderr");
		    
		    print $log_fh &timestamped_string(join("","--- timing: ",&timing_string($stats),"\n"));
		    
		    #now rename all the newly created output files
		    
		    opendir(DIR,$errortool_dir) or die;
		    my @files = grep {/passALL/} readdir(DIR);
		    closedir(DIR) or die;
		    foreach my $ofile (@files) {
			my $nfile = $ofile;
			$nfile=~s/passALL/pass${pass}/;
			my $source = join("/",$errortool_dir,$ofile);
			my $dest = join("/",$errortool_dir,$nfile);
			&execute_command("mv",$source,$dest);
		    }
		    
		    if ($analysis_type eq 'by_detailed_substitutions') {
			#now rename detailed_substitutions file to distinguish results			
			my $ofile = 'by_detailed_substitutions.txt';
			-f $ofile or die;
			my $nfile=join(".",'by_detailed_substitutions',"fc${flow_cell_id}","pass${pass}",'txt');
			my $source = join("/",$errortool_dir,$ofile);
			my $dest = join("/",$errortool_dir,$nfile);
			&execute_command("mv",$source,$dest);
		    }
		}
	    }
	}

	if ($analysis_type eq 'by_cycle') {
	    chdir($errortool_dir);
	}
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

			foreach my $pass (1,2) {
			    my @pargs;
			    push @pargs, "--reference_file","$refs{$pass}";
			    push @pargs, "--excluded_references_file","$erefs{$pass}";
			    push @pargs, "--first_read_number","$pass";
			    push @pargs, "--last_read_number","$pass";
			    
			    my $stdout = join("/",$log_dir,"error_tool.${analysis_type}.${subtype}.fc${flow_cell_id}.ppc${number_of_positions}.pass${pass}.out");
			    my $stderr = join("/",$log_dir,"error_tool.${analysis_type}.${subtype}.fc${flow_cell_id}.ppc${number_of_positions}.pass${pass}.err");
			    
			    my $cmd = join(" ",$program_name,@common_args,@targs,@fargs,@cargs,@pargs);
			
			    print $log_fh &timestamped_string("Excecuting command: $cmd\n");
			    
			    my $stats = &execute_command("$cmd > $stdout 2> $stderr");
			    
			    print $log_fh &timestamped_string(join("","--- timing: ",&timing_string($stats),"\n"));

			    #now rename all the newly created output files
			    
			    opendir(DIR,$wdir) or die;
			    my @files = grep {/passALL/} readdir(DIR);
			    closedir(DIR) or die;
			    foreach my $ofile (@files) {
				my $nfile = $ofile;
				$nfile=~s/passALL/pass${pass}/;
				my $source = join("/",$wdir,$ofile);
				my $dest = join("/",$wdir,$nfile);
				&execute_command("mv",$source,$dest);
			    }
			}
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
    foreach my $pass (1,2) {
	foreach my $flow_cell_id (@flow_cell_ids) {
	    my @channel_numbers=@{$protocol->channels_for_flow_cell($flow_cell_id)};
	    my @termloss_files;
	    foreach my $channel_number (@channel_numbers) {
		foreach my $camera_id (@cameras) {
		    my $file  = join("/",$lengthtool_dir,"termloss.fc${flow_cell_id}.ch${channel_number}.pass${pass}");
		    -e $file or die;
		    push @termloss_files,$file;
		}
	    }	
	    
	    my $growth_table=join("/",$lengthtool_dir,"growth.fc${flow_cell_id}.pass${pass}");
	    -e $growth_table or die;
	    
	    my $growth_profile = Helicos::SequenceAnalysis::Pipeline::GrowthProfile->new(flow_cell_id=>$flow_cell_id,
											 channel_numbers=>\@channel_numbers,
											 pass=>$pass);
	    
	    $growth_profile->parse_growth_table($growth_table);
	    $growth_profile->compile_from_termloss_tables(@termloss_files);
	    
	    $growth_profiles{$flow_cell_id}->{$pass}=$growth_profile;
	}
    }
}

# parse count information

{
    foreach my $flow_cell_id (@flow_cell_ids) {
	my @channel_numbers=@{$protocol->channels_for_flow_cell($flow_cell_id)};
	my $file  = join("/",$palmer_dir,"palmer.hits.fc${flow_cell_id}.csv");
	my $file_pass1  = join("/",$palmer_dir,"palmer.hits.pass1.fc${flow_cell_id}.csv");
	my $file_pass2  = join("/",$palmer_dir,"palmer.hits.pass2.fc${flow_cell_id}.csv");
	-e $file or die;
	-e $file_pass1 or die;
	-e $file_pass2 or die;

	my $yield_profile = Helicos::SequenceAnalysis::Pipeline::YieldProfile->new(flow_cell_id=>$flow_cell_id,
										   channel_numbers=>\@channel_numbers,
										   ordered_reference_ids=>$ordered_reference_ids{1},
										   ordered_reference_ids_pass2=>$ordered_reference_ids{2},
										   analysis=>$analysis,
										   filter_profile=>$filter_profiles{$flow_cell_id});
	
	$yield_profile->parse_two_pass_palmer_file_set(joint=>$file,pass1=>$file_pass1,pass2=>$file_pass2);
	#$yield_profile->parse_two_pass_palmer_file_set(joint=>$file);
	
	$yield_profiles{$flow_cell_id}=$yield_profile;
    }
}

# parse all error information

{
    foreach my $pass (1,2) {
	foreach my $flow_cell_id (@flow_cell_ids) {
	    my @channel_numbers=@{$protocol->channels_for_flow_cell($flow_cell_id)};
	    my @files;
	    foreach my $channel_number (@channel_numbers) {
		foreach my $camera_id (@cameras) {
		    my $file  = join("/",$errortool_dir,"by_nuc_error_fl${flow_cell_id}_ch${channel_number}_pass${pass}_camera${camera_id}");
		    -e $file or die;
		    push @files,$file;
		}
	    }	
	    
	    my $error_profile = Helicos::SequenceAnalysis::Pipeline::ErrorProfile->new(flow_cell_id=>$flow_cell_id,
										       channel_numbers=>\@channel_numbers,
										       analysis=>$analysis,
										       pass=>$pass);
	    
	    $error_profile->compile_from_by_nuc_tables(@files);
	    
	    $error_profiles{$flow_cell_id}->{$pass}=$error_profile;
	}
    }
}
 


# growth summary
    
{
    foreach my $pass (1,2) {
	foreach my $flow_cell_id (@flow_cell_ids) {
	    my $growth_profile = $growth_profiles{$flow_cell_id}->{$pass};
	    
	    {
		my $outfile_name = "growth.fc${flow_cell_id}.pass${pass}.txt";
		my $outfile = join("/",$report_dirs{$flow_cell_id},$outfile_name);
		$growth_profile->generate_growth_report(file=>$outfile);
	    }
	}
    }
}

# termloss summaries

{
    foreach my $pass (1,2) {
	foreach my $flow_cell_id (@flow_cell_ids) {
	    my $growth_profile = $growth_profiles{$flow_cell_id}->{$pass};
	    
	    {
		my $outfile_name = "termloss.summary.fc${flow_cell_id}.pass${pass}.txt";
		my $outfile = join("/",$report_dirs{$flow_cell_id},$outfile_name);
		$growth_profile->generate_termloss_report(file=>$outfile,type=>'summary');
	    }
	    
	    {
		my $outfile_name = "termloss.detail.fc${flow_cell_id}.pass${pass}.txt";
		my $outfile = join("/",$report_dirs{$flow_cell_id},$outfile_name);
		$growth_profile->generate_termloss_report(file=>$outfile,type=>'detail',min_reference_position=>5,max_reference_position=>15);
	    }	
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
	
	#next;

	foreach my $pass (1,2) {
	    {
		my $outfile_name = "counts.usable_strands.fc${flow_cell_id}.pass${pass}.txt";
		my $outfile = join("/",$report_dirs{$flow_cell_id},$outfile_name);
		$yield_profile->generate_report(file=>$outfile,type=>'usable',pass=>$pass);	    
	    }
	    
	    {
		my $outfile_name = "relative_abundance.usable_strands.fc${flow_cell_id}.pass${pass}.txt";
		my $outfile = join("/",$report_dirs{$flow_cell_id},$outfile_name);
		$yield_profile->generate_report(file=>$outfile,type=>'relative_abundance',pass=>$pass);	    
	    }
	}

	my @channel_numbers=@{$protocol->channels_for_flow_cell($flow_cell_id)};
	foreach my $channel_number (@channel_numbers) {
	    my $outfile_name = "counts_matrix.fc${flow_cell_id}.channel${channel_number}.txt";
	    my $outfile = join("/",$report_dirs{$flow_cell_id},$outfile_name);
	    $yield_profile->generate_report(file=>$outfile,type=>'matrix',channel=>$channel_number);
	}
    }
}

## generate error reports

{
    foreach my $pass (1,2) {
	# general
	foreach my $flow_cell_id (@flow_cell_ids) {
	    my $error_profile = $error_profiles{$flow_cell_id}->{$pass};
	    
	    {
		my $outfile_name = "errors.fc${flow_cell_id}.pass${pass}.txt";
		my $outfile = join("/",$report_dirs{$flow_cell_id},$outfile_name);
		$error_profile->generate_report(file=>$outfile,type=>'general',by_camera=>0,context=>'non-HP');
	    }
	    
	    {
		my $outfile_name = "errors.by_camera.fc${flow_cell_id}.pass${pass}.txt";
		my $outfile = join("/",$report_dirs{$flow_cell_id},$outfile_name);
		$error_profile->generate_report(file=>$outfile,type=>'general',by_camera=>1,context=>'non-HP');
	    }
	}
	
	# deletion/insertion
	foreach my $flow_cell_id (@flow_cell_ids) {
	    my $error_profile = $error_profiles{$flow_cell_id}->{$pass};
	    
	    foreach my $error_type (qw(deletion insertion))
	    {
		{
		    my $outfile_name = "${error_type}s.fc${flow_cell_id}.pass${pass}.txt";
		    my $outfile = join("/",$report_dirs{$flow_cell_id},$outfile_name);
		    $error_profile->generate_report(file=>$outfile,type=>$error_type,by_camera=>0,context=>'non-HP');
		}
		
		{
		    my $outfile_name = "${error_type}s.by_camera.fc${flow_cell_id}.pass${pass}.txt";
		    my $outfile = join("/",$report_dirs{$flow_cell_id},$outfile_name);
		    $error_profile->generate_report(file=>$outfile,type=>$error_type,by_camera=>1,context=>'non-HP');
		}
	    }
	    
	}
    }
}


exit;

#================================================

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
    if ($max_ppc<=35) {
	@sample_positions = (1..$max_ppc);
    }
    else {
	@sample_positions = (grep {$_<=$max_ppc} @qc_positions);
    }
    return @sample_positions;
}

sub _usage
{
    my $usage;
    my @lines;
    push @lines,"Synopsis:";
    push @lines,"     analysis_protocol_gss.pl --sms_file=path_to_strand_file --config_dbfile=path_to_config_file [options]";
    push @lines,"     analysis_protocol_gss.pl --help";
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

    