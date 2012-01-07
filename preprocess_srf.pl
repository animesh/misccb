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
# FILE:          preprocess_srf.pl
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

#TODO: consider setting umask
#We assume version 2 SRF XML in that we don't take an instrument_serial_number parameter

$REVISION = "";
$VERSION = "";

$| = 1;

my $help = 0;
my $verbose = 0;
my $debug = 0;

my $HELICOS_ANALYSIS_HOME = Helicos::SequenceAnalysis::Pipeline::Environment->get_value("HELICOS_ANALYSIS_HOME");
my $HELICOS_ANALYSIS_WORKSPACE_ROOT = Helicos::SequenceAnalysis::Pipeline::Environment->get_value("HELICOS_ANALYSIS_WORKSPACE_ROOT");

my $sms_file;
my $clipping_file;
my $launch_dir = getcwd();
my $working_directory;
my $output_directory;
my $srf2sms="srf2sms";

my %params = (srf_file                 => undef,
	      instrument_name          => undef,
	      output_prefix            => undef,
	      output_directory         => undef,
	      srf2sms                  => undef );

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


#---------------
# validate script options/parameters
#---------------

unless (defined($params{instrument_name})) {
    Helicos::Exception::Argument->throw("required parameter <instrument_name> not specified\n");
}

unless (defined($params{srf_file})) {
    Helicos::Exception::Argument->throw("required parameter <strand_data> not specified\n");
}

unless (-f $params{srf_file}) {
    Helicos::Exception::Argument->throw("SRF data file <$params{srf_file}> does not exist or is not a regular file\n");
}

if (defined($params{srf2sms})) {
    $srf2sms = $params{srf2sms};
}

if (defined($params{output_directory})) {
    $output_directory = $params{output_directory};
    if (-e $output_directory) {
	-d $output_directory or Helicos::Exception::Simple->throw("file corresponding to specified output directory <$output_directory> exist and is not a directory\n");
    }
    else {
	eval { mkpath($output_directory); };
	if ($@) {
	    Helicos::Exception::Simple->throw("unable to create output directory <$output_directory>\n");
	}
    }
    $output_directory = abs_path($output_directory);
}
else {
    $output_directory = getcwd();
}

if (defined($params{output_prefix})) {
    $params{output_prefix}=~/^[A-Za-z0-9]([A-Za-z0-9_\-\.])*$/ or Helicos::Exception::Simple->throw("invalid ouput prifix <$params{output_prefix}>\n");
}
else {
    my($basename) = fileparse($params{srf_file});
    $params{output_prefix}=$basename;
    $params{output_prefix}=~s/\.srf//;
}

{
    my $wdir;
    my $tmpdir_template = 'helicos.preprocess_srf.XXXXX';
    eval { 
	$wdir = File::Temp->newdir($tmpdir_template,
				   CLEANUP => 0,
				   TMPDIR=>1);
    };
    if ($@) {
	Helicos::Exception::Simple->throw("unable to create working directory: $@\n");
    }
    $working_directory = $wdir->dirname;
}

my %temp_files;

#---------------
# RUN SRF2SMS
#---------------

{
    print &timestamped_string("Processing SRF file <$params{srf_file}>.\n");

    $temp_files{srf_xml} = abs_path(join("/",$working_directory,join("",$params{output_prefix},".srf.xml")));
    $temp_files{sms} = abs_path(join("/",$working_directory,join("",$params{output_prefix},".sms")));
    $temp_files{clipping} = abs_path(join("/",$working_directory,join("",$params{output_prefix},".clipping")));
    $temp_files{instrument_xml} = abs_path(join("/",$working_directory,join("",$params{output_prefix},".instrument.xml")));
    $temp_files{run_xml} = abs_path(join("/",$working_directory,join("",$params{output_prefix},".run.xml")));

    my $stdout = abs_path(join("/",$working_directory,"srf2sms.log"));
    my $stderr = abs_path(join("/",$working_directory,"srf2sms.err"));
    
    {
	my $program_name = $srf2sms;
	my @args;
	push @args, "--srf_file","$params{srf_file}";
	push @args, "--xml_file","$temp_files{srf_xml}";
	push @args, "--sms_file","$temp_files{sms}";
	push @args, "--clipping_roi_file","$temp_files{clipping}";

	my $cmd = join(" ",$program_name,@args);
	
	print &timestamped_string("Excecuting command: $cmd\n");

	my $stats = &execute_command("$cmd > $stdout 2> $stderr");

	print &timestamped_string(join("","--- timing: ",&timing_string($stats),"\n"));
    }
}

#--------------
# Parse SRF XML and generate instrument XML and run XML files for analysis pipeline
#--------------

{
    print &timestamped_string("Parsing embedded Helicos XML metadata from temporary file <$temp_files{srf_xml}>.\n");

    my $srf_xml = Helicos::SRF::XML->new(xml             => URI->new(join("","file:",$temp_files{srf_xml})),
					 instrument_name => $params{instrument_name});

    
    my $fh;
    
    print &timestamped_string("Generating instrument XML and writing to temporary file <$temp_files{instrument_xml}>.\n");

    $fh = IO::File->new(">$temp_files{instrument_xml}");
    $fh or Helicos::Exception::IO->throw("error opening file <$temp_files{instrument_xml}> for writing\n");;
    
    print $fh $srf_xml->run->instrument->to_xml;
    
    close($fh) or Helicos::Exception::IO->throw("error on close for file <$temp_files{instrument_xml}>\n");

    print &timestamped_string("Generating run XML and writing to temporary file <$temp_files{run_xml}>.\n");

    $fh = IO::File->new(">$temp_files{run_xml}");
    $fh or Helicos::Exception::IO->throw("error opening file <$temp_files{run_xml}> for writing\n");;
    
    print $fh $srf_xml->run->to_xml;
    
    close($fh) or Helicos::Exception::IO->throw("error on close for file <$temp_files{run_xml}>\n");
}


#--------------
# Copy output to final ouput directory
#--------------

print &timestamped_string("Copying output files to final ouput directory <$output_directory>.\n");

{
    my @files;
    foreach my $key (sort keys %temp_files) {
	push @files, $temp_files{$key};
    }
    
    foreach my $file ( @files ) {
	my($filename) = fileparse($file);
	my $outfile = join("/",$params{output_directory},$filename);
	
	my $cmd = "cp -p $file $outfile";
	
	print &timestamped_string("Excecuting command: $cmd\n");
	
	my $stats = &execute_command("$cmd > /dev/null 2> /dev/null");
    }
}

#---------------
# CLEANUP 
#---------------

chdir($launch_dir);

print &timestamped_string("Removing working directory <$working_directory>.\n");

eval { rmtree($working_directory); };
if ($@) {
    Helicos::Exception::Simple->throw("unable to remove working directory <$working_directory>\n");
}

### FINISH ###
    
print &timestamped_string("Preprocessing of SRF file completed successfully.\n");

close(STDOUT) or Helicos::Exception::Simple->throw("close of STDOUT failed\n");

exit;


##############

sub _usage
{
    my $usage;
    my @lines;
    push @lines,"Synopsis:";
    push @lines,"     preprocess_srf.pl --srf_file=path_to_srf_file --instrument_name=instrument_name [options]";
    push @lines,"     preprocess_srf.pl --help";
    push @lines,"";
    push @lines,"Parameters/Options:";
    push @lines,"     --output_prefix=arg            Prefix for output files [DEFAULT: basename of SRF file]";
    push @lines,"     --output_directory=arg         Path to final output dir [DEFAULT: current directory]";
    push @lines,"     --srf2sms=arg                  Name or full path of srf2sms executable (can be used to handle an old SRF file by specifying an appropriate executable)";
    push @lines,"";
    push @lines,"     --help                         Print this help message";
    push @lines,"";
    push @lines,"Example:";
    push @lines,"";
    push @lines,"   preprocess_srf.pl \\";
    push @lines,"          --srf_file=2008-03-18T15_57_09.srf \\";
    push @lines,"          --instrument_name=CTS1 \\";
    push @lines,"          --output_prefix=run2008-03-18 \\";
    push @lines,"          --output_directory=/helicos/analysis/run2008-03-18/data \\";
    push @lines,"          > preprocess.log 2>&1 &";

    $usage = join("",join("\n",@lines),"\n");
    return $usage;
}

__END__
