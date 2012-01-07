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
# FILE:          analysis_controller.pl
# DESCRIPTION:   Master script for launching pre-defined sequence analysis protocols. 
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
use File::Spec;
use Cwd qw(chdir getcwd abs_path);
use Data::Dumper;

use Helicos::Util qw(execute_command);
use Helicos::Exception::Classes;
use Helicos::MetaData::Loader;
use Helicos::MetaData::Classes;
use Helicos::SRF::XML;

use Helicos::SequenceAnalysis::Pipeline::Environment;
use Helicos::SequenceAnalysis::Pipeline::Util qw(output_header timestamped_string formatted_datetime timing_string);


# use MLDBMS for serializing analysis configuration and environment for logging and for use by called scripts

use MLDBM qw(DB_File Storable);     # DB_File and Storable
    
#TODO: consider setting umask

$REVISION = "";
$VERSION = "";

$| = 1;

#$SIG{__DIE__} = \&prepare_to_die;

#sub prepare_to_die {
#    my $txt = "\n\nFATAL: Analysis failed: $0 encountered a problem and could not continue !\n\n";
#    print $txt;
#    warn $txt;
#}

my $help = 0;
my $verbose = 0;
my $debug = 0;

#my @default_qc_image_positions=qw(1 25 45 60 75 100 200 300 301 400 500 600 601 700 800 900 901 1000 1100 1200 1201 1300 1400 1500); #old values
#my @default_qc_image_positions = qw(1 25 45 60 75 76 175 275 276 375 475 476 575 576 675 775 875 975 1075 1175 1176 1275 1375 1475);
my @default_qc_image_positions = qw(1 25 45 60 75 76 100 175 200 275 276 300 301 375 400 475 476 575 576 675 775 875 876 975 1075 1175 1176 1275 1375 1475);

my $tmpdir = File::Spec->tmpdir();

my $HELICOS_ANALYSIS_HOME = Helicos::SequenceAnalysis::Pipeline::Environment->get_value("HELICOS_ANALYSIS_HOME");
my $HELICOS_ANALYSIS_WORKSPACE_ROOT = Helicos::SequenceAnalysis::Pipeline::Environment->get_value("HELICOS_ANALYSIS_WORKSPACE_ROOT");

my $schema_dir = join("/",$HELICOS_ANALYSIS_HOME,'config/descriptors/schema');

my $instrument_xsd_uri = URI->new(join("","file:","${schema_dir}","/",'heliscope.xsd'));
my $run_xsd_uri = URI->new(join("","file:","${schema_dir}","/",'heliscope_run.xsd'));
my $analysis_xsd_uri = URI->new(join("","file:","${schema_dir}","/",'heliscope_sequence_analysis.xsd'));

my $sms_file;
my $clipping_file;
my @qc_positions;
my @positions;
my $protocol;
my $input_strand_data_format;

my %params = ( strand_data              => undef,
	       instrument_xml           => undef,
	       run_xml                  => undef,
	       analysis_xml             => undef,
	       clipping_file            => undef,
	       instrument_name          => undef,
	       instrument_serial_number => undef,
	       qc_positions             => undef,
	       positions                => undef,
	       dge_assignment_file      => undef,
	       max_processes            => undef,
	       max_retry                => undef );

my %OPTIONS = (
	       'verbose=i'                => \$verbose,
	       'debug=i'                  => \$debug,
	       'help'                     => \$help,
	      );

foreach my $paramkey (qw(strand_data instrument_xml run_xml analysis_xml instrument_name instrument_serial_number clipping_file qc_positions positions dge_assignment_file max_processes max_retry)) {
    $OPTIONS{join("",$paramkey,"=s")}=\$params{$paramkey};
}

unless (&GetOptions(%OPTIONS)) {
    die "FATAL: invalid options or option values\n\n",&_usage();
}

if ($help) { print &_usage(); exit; }



#---------------
# validate script options/parameters
#---------------

unless (defined($params{strand_data})) {
    Helicos::Exception::Argument->throw("required parameter <strand_data> not specified\n");
}

unless (defined($params{analysis_xml})) {
    Helicos::Exception::Argument->throw("required parameter <analysis_xml> not specified\n");
}

unless (-f $params{strand_data}) {
    Helicos::Exception::Argument->throw("strand data file <$params{strand_data}> does not exist or is not a regular file\n");
}

if (defined($params{dge_assignment_file}) and ! -e $params{dge_assignment_file}) {
    Helicos::Exception::Argument->throw("specified DGE assignment file <$params{dge_assignment_file}> does not exist\n");
}

# auto determine strand data file format
if ($params{strand_data}=~/\.(yac|sms)$/) {
    #SMS format
    #no action required
    $input_strand_data_format='SMS';
}
elsif ($params{strand_data}=~/\.(ssr|srf)$/) {
    #SRF format
    #need to convert to SMS format and extract embeded XML for building instrument and run xml files    
    $input_strand_data_format='SRF';
}
else {
    #not obvious
    #TODO: die because we don't yet have code to peek in the file
    Helicos::Exception::Simple->throw("unable to infer strand data file type from filename <$params{strand_data}>\n");
}

if ($input_strand_data_format eq 'SRF') {
    if (grep {defined($params{$_});} qw(instrument_xml run_xml clipping_file)) {
	Helicos::Exception::Argument->throw("invalid combination of parameters - <instrument_xml> and <run_xml> parameters are not compatible with an SRF-format strand file\n");
    }
}
elsif ($input_strand_data_format eq 'SMS') {
    if (grep {!defined($params{$_});} qw(instrument_xml run_xml)) {
	Helicos::Exception::Argument->throw("parameters <instrument_xml> and <run_xml> parameters are required with an SMS-format strand file\n");
    }
    $sms_file=$params{strand_data};
}
else {
    Helicos::Exception::Simple->throw("unknown strand data file format\n");
}

if ($input_strand_data_format eq 'SRF') {
    unless  (defined($params{instrument_name})) {
	if (defined($params{instrument_serial_number})) {
	    $params{instrument_name}=join("_",'Heliscope',$params{instrument_serial_number});
	}
	else {
	    Helicos::Exception::Argument->throw("required parameter <instrument_name OR instrument_serial_number> not specified\n");
	}
    }
}
else {
    if (defined($params{instrument_name}) or defined($params{instrument_serial_number})) {
	Helicos::Exception::Argument->throw("invalid parameter(s) <instrument_name AND/OR instrument_serial_number> not valid\n");
    }
}

if (defined($params{qc_positions})) {
    my $str = $params{qc_positions};
    my @pos = split(/,/,$str,-1);
    grep {!(/^\d+$/ and $_>0);} @pos and die;
    @pos or die;
    my %tmp = map {$_=>1} @pos;
    @qc_positions= sort {$a<=>$b} keys %tmp;
}
else {
    @qc_positions=@default_qc_image_positions;
}

if (defined($params{positions})) {
    my $str = $params{positions};
    my @pos = split(/,/,$str,-1);
    grep {!(/^\d+$/ and $_>0);} @pos and die;
    @pos or die;
    my %tmp = map {$_=>1} @pos;
    @positions= sort {$a<=>$b} keys %tmp;
}


#---------------
# extract embedded metadata if input strand file in SRF format, and use to create instrument and run xml files
#---------------

if ($input_strand_data_format eq 'SRF') {

    ### Run srf2sms only to extract Helicos XML ###

    print &timestamped_string("Extracting embedded Helicos XML metadata from SRF file <$params{strand_data}>.\n");

    my $xml_file = abs_path(join("/",$tmpdir,"helicos.$$.srf2sms.extract_xml.xml"));
    if (-e $xml_file) {
	Helicos::Exception::Simple->throw("temporary file <$xml_file> exists\n");
    }
    
    my $stdout = abs_path(join("/",$tmpdir,"helicos.$$.srf2sms.extract_xml.log"));
    my $stderr = abs_path(join("/",$tmpdir,"helicos.$$.srf2sms.extract_xml.err"));
    
    {
	my $program_name = 'srf2sms';
	my @common_args;
	push @common_args, "--srf_file","$params{strand_data}";
	push @common_args, "--xml_file","$xml_file";
	
	my $cmd = join(" ",$program_name,@common_args);
	
	print &timestamped_string("Excecuting command: $cmd\n");

	my $stats = &execute_command("$cmd > $stdout 2> $stderr");

	print &timestamped_string(join("","--- timing: ",&timing_string($stats),"\n"));
    }

    print &timestamped_string("Parsing embedded Helicos XML metadata from temporary file <$xml_file>.\n");

    my $srf_xml;
    
    {
	my %srf_xml_opts = (xml=>URI->new(join("","file:",$xml_file)),
			    instrument_name=>$params{instrument_name});
	
	if (defined($params{instrument_serial_number})) {
	    $srf_xml_opts{instrument_serial_number}=$params{instrument_serial_number};
	}
	
        $srf_xml = Helicos::SRF::XML->new(%srf_xml_opts);
    }
    
    my $instrument_xml = abs_path(join("/",$tmpdir,"helicos.pid$$.instrument.xml"));
    my $run_xml = abs_path(join("/",$tmpdir,"helicos.pid$$.run.xml"));
    
    my $fh;
    
    print &timestamped_string("Generating instrument XML and writing to temporary file <$instrument_xml>.\n");

    $fh = IO::File->new(">$instrument_xml");
    $fh or Helicos::Exception::IO->throw("error opening file <$instrument_xml> for writing\n");;
    
    print $fh $srf_xml->run->instrument->to_xml;
    
    close($fh) or Helicos::Exception::IO->throw("error on close for file <$instrument_xml>\n");

    print &timestamped_string("Generating run XML and writing to temporary file <$run_xml>.\n");

    $fh = IO::File->new(">$run_xml");
    $fh or Helicos::Exception::IO->throw("error opening file <$run_xml> for writing\n");;
    
    print $fh $srf_xml->run->to_xml;
    
    close($fh) or Helicos::Exception::IO->throw("error on close for file <$run_xml>\n");
    
    $params{instrument_xml}=$instrument_xml;
    $params{run_xml}=$run_xml;
}


#--------------------------
# parse configuration files
#--------------------------

print &timestamped_string("Starting parse of primary configuration flies.\n");

my $analysis = &_parse_configuration_files(map {$params{$_};} qw(instrument_xml run_xml analysis_xml));

print &timestamped_string("Completed parse of primary configuration flies.\n");

print Dumper($analysis) if $debug;


#-----------------------------
# verify single protocol
#-----------------------------

{
    my @protocols = @{$analysis->protocols};

    unless (@protocols==1) {
	Helicos::Exception::Simple->throw("current implementation requires that only a single protocol be specified\n");
    }
    
    $protocol = $protocols[0];
}

#---------------------------
# set positions if specified
#---------------------------

#ultimately we want this in the analysis XML itself

if (@positions) {
    $protocol->set_positions(\@positions);
}

#---------------------------
# set qc_positions
#---------------------------

#ultimately we want this in the run XML itself

$analysis->run->set_qc_positions(\@qc_positions);

#---------------------------
# handle protocol-specific parameters
#---------------------------

#ultimately we want this in the analysis XML itself

{
    my $protocol_name = $protocol->name;
    
    if ($protocol_name eq 'DGE' and ! defined($params{dge_assignment_file})) {
	Helicos::Exception::Simple->throw("parameter <dge_assignment_file> is required for protocol <$protocol_name>\n");
    }

    if ($protocol_name ne 'DGE' and defined($params{dge_assignment_file})) {
	Helicos::Exception::Simple->throw("parameter <dge_assignment_file> is not valid for protocol <$protocol_name>\n");
    }
}

#-----------------------------
# set up workspace environment
#-----------------------------


my $instrument_name = $analysis->run->instrument->name;
my $run_name = $analysis->run->name;
my $analysis_name = $analysis->name;

my $basedir = join("/",$HELICOS_ANALYSIS_WORKSPACE_ROOT,$instrument_name,$run_name,$analysis_name);
my $log_dir = join("/",$basedir,'logs');

print &timestamped_string("Checking for analysis base directory <$basedir>.\n");

# create the base analysis directory if it doesn't already exist

if (-e $basedir) {
    if (-d $basedir) {
	#check to make sure it is empty
	# TODO: check that permission are OK as well
	opendir(BASEDIR,$basedir) or die;
	my @content = grep {! /^\.\.?$/} readdir(BASEDIR);
	if (@content) {
	    Helicos::Exception::Simple->throw("base directory <$basedir> already exists and is not empty\n");
	}
    }
    else {
	Helicos::Exception::Simple->throw("unable to create base directory <$basedir> - file with that name exists\n");
    }
    print &timestamped_string("Analysis base directory <$basedir> exists and is empty - ready\n");
}
else {
    unless(mkpath($basedir)) {
	Helicos::Exception::Simple->throw("unable to create base directory <$basedir>\n");
    }
    print &timestamped_string("Analysis base directory <$basedir> created.\n");
}

# create directories for configuration and logging information

print &timestamped_string("Creating directories for storing strand data, configuration information, log output, raw analysis results, and summary reports.\n");

{
    my $dir;

    foreach my $subdir (qw(data config logs analysis reports)) {
    
	my $dir = join("/",$basedir,$subdir);
	unless(mkpath($dir)) {
	    Helicos::Exception::Simple->throw("unable to create directory <$dir>\n");
	}
    }
}

print &timestamped_string("Completed creating output subdirectories.\n");

# copy configuration information
{
    print &timestamped_string("Copying configuration XML to local output config directory.\n");
    
    my @xml_config_files;
    push @xml_config_files,(map {$params{$_};} qw(instrument_xml run_xml analysis_xml));
    push @xml_config_files,(map {$_->path} ($instrument_xsd_uri,$run_xsd_uri,$analysis_xsd_uri));
    
    #TODO: grab embedded/linked xml as well
    #push @xml_config_files, map {$_->} @{$analysis->protocols}
    
    foreach my $xml_file (@xml_config_files) {
	my $dir = join("/",$basedir,'config');
	copy($xml_file,$dir) or Helicos::Exception::Simple->throw("copy of file <$xml_file> to config directory <$dir> failed.\n");
    }
    
    print &timestamped_string("Completed copying configuration XML to local output config directory.\n");
}


#----------------------------------------------------------
# Process input SRF file if SRF is input strand format
#----------------------------------------------------------

if ($input_strand_data_format eq 'SRF') {
    
    ### RUN SRF2SMS ###

    print &timestamped_string("Converting strand data from SRF file <$params{strand_data}> to SMS format\n");
   
    my $sms_fn=join(".",$analysis->run->instrument->name,$analysis->run->name,"sms");
    my $clipping_fn=join(".",$analysis->run->instrument->name,$analysis->run->name,"clipping");
    $sms_file = abs_path(join("/",$basedir,"data",$sms_fn));
    $clipping_file=abs_path(join("/",$basedir,"data",$clipping_fn));
    
    if (-e $sms_file) {
	Helicos::Exception::Simple->throw("file <$sms_file> exists\n");
    }
    
    my $stdout = join("/",$log_dir,"srf2sms.stdout.txt");
    my $stderr = join("/",$log_dir,"srf2sms.err");

    {
	my $program_name = 'srf2sms';
	my @common_args;
	push @common_args, "--srf_file","$params{strand_data}";
	push @common_args, "--sms_file","$sms_file";
	push @common_args, "--clipping_roi_file","$clipping_file";
	
	my $cmd = join(" ",$program_name,@common_args);
	
	print &timestamped_string("Excecuting command: $cmd\n");

	my $stats = &execute_command("$cmd > $stdout 2> $stderr");
	
	print &timestamped_string(join("","--- timing: ",&timing_string($stats),"\n"));
    }

    $analysis->run->parse_clipping_file($clipping_file);
}
else {
    #process supplied clipping file (if any)
    if (defined($params{clipping_file})) {
	$analysis->run->parse_clipping_file($params{clipping_file});
    }
}

{
    #get full path for sms_file (in case it is a relative path as provided)
    my $abs_path = abs_path($sms_file);
    $sms_file=$abs_path;
}

#---------------------------------
# store configuration information
#
#    must wait until now - when we have set the registration clipping information in the analysis object
#---------------------------------

my $dbfile;

{
    my $dir = join("/",$basedir,'config');
    $dbfile= join("/",${dir},'analysis_config.mldbm');
    print &timestamped_string("Storing analysis configuration object and environment in dbfile <$dbfile>\n");
    my %conf;
    my $db = tie %conf, 'MLDBM', $dbfile, O_CREAT|O_RDWR, 0640 or die $!;
    $conf{environment} = \%ENV;              # Stores the whole environment
    $conf{analysis} = $analysis;             # Stores the whole analysis object
    undef $db;
    untie(%conf);
}




#------------------------------------------------------------------------------------
# execute analysis protocol on strand data as specified in the analysis configuration
#------------------------------------------------------------------------------------

{
    my $name = $protocol->name;
    print &timestamped_string("Launching analysis protocol <$name>\n");
    
    unless ($name eq 'tSMS_validation' or $name eq 'GSS_standard' or $name eq 'DGE') {
	Helicos::Exception::Simple->throw("unsupported protocol <$name>\n");
    }

    my $precedure = $protocol->procedure;
    my @steps = @{$precedure->steps};
    foreach my $step (@steps) {
	my $program_name = $step->program->name;
	my @args;
	push @args, "--sms_file=$sms_file";
	push @args, "--config_dbfile=$dbfile";

	if ($name eq 'DGE') {
	    push @args, "--assignment_file=$params{dge_assignment_file}";
	    if (defined($params{max_processes})) {
		push @args, "--max_processes=$params{max_processes}";
	    }
	    if (defined($params{max_retry})) {
		push @args, "--max_retry=$params{max_retry}";
	    }
	}
	
	my $cmd = join(" ",$program_name,@args);
	
	print &timestamped_string("Excecuting command: $cmd\n");

	my $stats = &execute_command("$cmd");
	
	print &timestamped_string(join("","--- timing: ",&timing_string($stats),"\n"));
    }
   
    print &timestamped_string("Completed analysis protocol <$name>\n");
}


print &timestamped_string("Analysis completed successfully.\n");

exit;

##############

sub _parse_configuration_files
{
    my(@args)=@_;
    my($instrument_xml,$run_xml,$analysis_xml)=@args;
    
    my $instrument_xml_uri = URI->new("file:$instrument_xml");
    my $run_xml_uri = URI->new("file:$run_xml");
    my $analysis_xml_uri = URI->new("file:$analysis_xml");

    my $loader = Helicos::MetaData::Loader->new(instrument_xml=>$instrument_xml_uri,
						instrument_xsd=>$instrument_xsd_uri,
						run_xml=>$run_xml_uri,
						run_xsd=>$run_xsd_uri,
						analysis_xml=>$analysis_xml_uri,
						analysis_xsd=>$analysis_xsd_uri);
    
    my $analysis=$loader->analysis;
    return $analysis;
}

sub _usage
{
    my $usage;
    my @lines;
    push @lines,"Synopsis:";
    push @lines,"     analysis_controller.pl --strand_file=path_to_strand_file --analysis_xml=path_to_analysis_xml_file [options]";
    push @lines,"     analysis_controller.pl --help";
    push @lines,"";
    push @lines,"Parameters/Options:";
    push @lines,"     --strand_file=arg              Path to input SRF or SMS strand data file";
    push @lines,"     --analysis_xml=arg             Path to analysis XML configuration file";
    push @lines,"     --instrument_xml=arg           Path to instrument XML configuration file [SMS input only]";
    push @lines,"     --run_xml=arg                  Path to Heliscope run XML configuration file [SMS input only]";
    push @lines,"     --clipping_file=arg            Path to image stack registration data file [SMS input only] [OPTIONAL]";
    push @lines,"     --positions=arg                Comma-delimited list of positions to process [OPTIONAL]";
    push @lines,"     --qc_positions=arg             Comma-delimited list of QC positions for the run [OPTIONAL]";
    push @lines,"     --instrument_name=arg          Only required when using external SRF files as input (which lack this information); Used in creating output path.";
    push @lines,"     --dge_assignment_file=arg      Path to DGE reference sassignment file [REQUIRED for DGE analysis]";
    push @lines,"     --max_processes=arg            Number of parallel alignment jobs [OPTIONAL: only valid for DGE analysis DEFAULT: 1]";
    push @lines,"     --max_retry=arg                Number of times to retry alignment jobs in the event of an error [OPTIONAL: only valid for DGE analysis DEFAULT: 0]";
    push @lines,"";
    push @lines,"     --help                         Print this help message";
    push @lines,"";
    push @lines,"Example:";
    push @lines,"";
    push @lines,"   These are typically long-running processes, so it is recommended that stdout and stderr be captured and that the";
    push @lines,"   job be launched in the background";
    push @lines,"";
    push @lines,"   analysis_controller.pl \\";
    push @lines,"          --strand_data=2008-03-18T15_57_09.srf \\";
    push @lines,"          --analysis_xml=EA1.20080318.analysis.xml \\";
    push @lines,"          --instrument_name=EA1 \\";
    push @lines,"          > pipeline_analysis.log 2>&1 &";

    $usage = join("",join("\n",@lines),"\n");
    return $usage;
}

__END__

