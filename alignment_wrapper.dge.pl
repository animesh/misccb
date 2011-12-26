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
# FILE:          alignment_wrapper.dge.pl
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
use File::Temp;
use Data::Dumper;
use Cwd qw(chdir getcwd abs_path);

use Helicos::Util qw(execute_command);
use Helicos::Exception::Classes;

use Helicos::SequenceAnalysis::Pipeline::Util qw(output_header timestamped_string formatted_datetime timing_string);

$REVISION = "";
$VERSION = "1.0.0";

$| = 1;

my $help = 0;
my $verbose = 0;
my $debug = 0;

my $working_directory;
my $launch_dir = getcwd();

#============================
# Hardcoded Defaults
#--read_file_type sms
#--pass 1
#--binary_output
#--read_step 2
#===========================

my %params = (sms_file                         => undef,
	      reference_file                   => undef,
	      seed_size                        => undef,
	      weight                           => undef,
	      num_errors                       => undef,
	      config_file                      => undef,
	      template_repository              => undef,
	      working_directory                => undef,     # local working directory
	      output_directory                 => undef,     # directory (typically on shared volume) to which results are copied
	      out_prefix                       => undef,
	      flow_cell                        => undef,
	      channel                          => undef,
	      num_blocks                       => undef,
	      block_index                      => undef,
	      max_hit_duplication              => undef,
	      percent_error                    => undef,
	      min_norm_score                   => undef,     # suboptimal threshold
	      aligned_files_threshold          => undef,     # "aligned" read threshold AND summAlign param "min_score"
	      max_diff                         => undef,     # summAlign parameter
	      max_align_num                    => undef );   # summAlign parameter

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


### VERIFY PARAMETERS ###

{ 
    my @required = grep {$_ !~ /^(working_directory)$/} keys %params;
    foreach my $param_name (@required) {
	defined($params{$param_name}) or Helicos::Exception::Simple->throw("required parameter <$param_name> not specified\n");
    }

    if ( -e $params{output_directory} ) { Helicos::Exception::Simple->throw("output directory <$params{output_directory}> exists\n"); }

    foreach my $infile (map {$params{$_}} qw(sms_file reference_file config_file)) {
	-e $infile or Helicos::Exception::Simple->throw("input file <$infile> not found\n");
	-f $infile or Helicos::Exception::Simple->throw("input file <$infile> not a regular file\n");
    }

    unless (-d $params{template_repository}) {
	Helicos::Exception::Simple->throw("template repository <$params{template_repository}> does not exist or is not a directory\n");
    }

    $params{template_repository} = abs_path($params{template_repository});
    
    foreach my $int_param ( qw(seed_size weight num_errors max_align_num) ) {
	unless ($params{$int_param}=~/^\d+$/) { Helicos::Exception::Simple->throw("invalid value <$params{$int_param}> for parameter <$int_param>\n"); }
    }

    my $template_family_file = join("/",$params{template_repository},"templateFamily_$params{seed_size}_$params{weight}_$params{num_errors}");
    -e $template_family_file or Helicos::Exception::Simple->throw("template family file <$template_family_file> not found\n");
    -f $template_family_file or Helicos::Exception::Simple->throw("template family file <$template_family_file> not a regular file\n");

    unless ($params{percent_error}=~/^\d+$/ and $params{percent_error}>=0 and $params{percent_error}<=100) {
	Helicos::Exception::Simple->throw("invalid value <$params{percent_error}> for parameter <percent_error>\n");
    }

    foreach my $float_param ( qw(min_norm_score aligned_files_threshold max_diff) ) {
	unless ($params{$float_param}=~/^\d+(\.\d*)?$/ or $params{$float_param}=~/^\d*\.\d+$/) {
	    Helicos::Exception::Simple->throw("invalid value <$params{$float_param}> for parameter <$float_param>\n");
	}
    }

    unless ($params{min_norm_score}<=$params{aligned_files_threshold}) {
	Helicos::Exception::Simple->throw("invalid values for score parameters: aligned_files_threshold <$params{aligned_files_threshold}> larger than min_norm_score <$params{min_norm_score}>\n");
    }

    $params{num_blocks}>0 or Helicos::Exception::Simple->throw("invalid value <$params{num_blocks}> for parameter <num_blocks>\n");
    $params{max_align_num}>0 or Helicos::Exception::Simple->throw("invalid value <$params{max_align_num}> for parameter <max_align_num>\n");
    
    unless ($params{block_index}<$params{num_blocks}) {
	Helicos::Exception::Simple->throw("invalid values for block parameters: block_index <$params{block_index}> not less than num_blocks <$params{num_blocks}>\n");
    }
}


### SET UP WORKSPACE ###

if (defined($params{working_directory})) {
    $working_directory = $params{working_directory};
    -e $working_directory and Helicos::Exception::Simple->throw("specified working directory <$working_directory> exist\n");
    eval { mkpath($working_directory); };
    if ($@) {
	Helicos::Exception::Simple->throw("unable to create working directory <$working_directory>\n");
    }
    $working_directory = abs_path($working_directory);
}
else {
    my $ndir;
    my $tmpdir_template = 'helicos.alignment_wrapper_dge.XXXXX';
    eval { 
	$ndir = File::Temp->newdir($tmpdir_template,
				   CLEANUP => 0,
				   TMPDIR=>1);
    };
    if ($@) {
	Helicos::Exception::Simple->throw("unable to create working directory: $@\n");
    }
    $working_directory = $ndir->dirname;
}

unless(mkpath($params{output_directory})) {
    Helicos::Exception::Simple->throw("unable to create output directory <$params{output_directory}>\n");
}
$params{output_directory} = abs_path($params{output_directory});

chdir($working_directory);

### OPEN LOG FILE ###

# my $log_file = join("/",$working_directory,'alignment_wrapper.log');

# my $log_fh = IO::File->new(">$log_file");
# $log_fh or Helicos::Exception::IO->throw("unable to open log file <$log_file> for writing\n");
# $log_fh->autoflush(1);

#TODO: want to send all error output to log file as well as stderr


### COPY INPUT FILES TO LOCAL WORKSPACE ###

my %working_input_files;

print STDOUT &timestamped_string("Copying input files to working directory.\n");

foreach my $input_file_param ( qw(sms_file reference_file config_file) ) {
    
    my $input_file = $params{$input_file_param};
    my($filename) = fileparse($input_file);
    $working_input_files{$input_file_param} = join("/",$working_directory,$filename);
    
    my $cmd = "cp -p $input_file $working_input_files{$input_file_param}";
    
    print STDOUT &timestamped_string("Excecuting command: $cmd\n");
    
    my $stats = &execute_command("$cmd > /dev/null 2> /dev/null");

}

### RUN INDEXDP ###

print STDOUT &timestamped_string("Runnnig indexDP.\n");

my $indexDP_logfile = join("/",$working_directory,"indexDP.out");
my $indexDP_errfile = join("/",$working_directory,"indexDP.err");


{
    my $program_name = 'indexDP';
    my @args;
    push @args, "--read_file_type","sms";
    push @args, "--reads_file","$working_input_files{sms_file}";
    push @args, "--reference_file","$working_input_files{reference_file}";
    push @args, "--seed_size","$params{seed_size}";
    push @args, "--num_errors","$params{num_errors}";
    push @args, "--weight","$params{weight}";
    push @args, "--config_file","$working_input_files{config_file}";
    push @args, "--template_repository","$params{template_repository}";
    push @args, "--flow_cell","$params{flow_cell}";
    push @args, "--channel","$params{channel}";
    push @args, "--pass","1";
    push @args, "--num_blocks","$params{num_blocks}";
    push @args, "--block_index","$params{block_index}";
    push @args, "--max_hit_duplication","$params{max_hit_duplication}";
    push @args, "--out_prefix","$params{out_prefix}";
    push @args, "--percent_error","$params{percent_error}";
    push @args, "--min_norm_score","$params{min_norm_score}";
    push @args, "--aligned_files_threshold","$params{aligned_files_threshold}";
    push @args, "--read_step","2";
    push @args, "--binary_output";

    my $cmd = join(" ",$program_name,@args);
    
    print STDOUT &timestamped_string("Excecuting command: $cmd\n");
    
    my $stats = &execute_command("$cmd > $indexDP_logfile 2> $indexDP_errfile");
    
    print STDOUT &timestamped_string(join("","--- timing: ",&timing_string($stats),"\n"));
}

### VERIFY INDEXDP OUTPUT ###

print STDOUT &timestamped_string("Verifying indexDP output.\n");

my $binary_alignment_file;
my @sms_files;

{
    foreach my $suffix ( qw(duplicates.sms aligned.sms nonAligned.sms binary.bin) ) {
	my $filename = join("_",$params{out_prefix},'indexDP',$suffix);
	my $file = join("/",$working_directory,$filename);
	unless (-e $file) {
	    if ($suffix=~/\.sms$/) {
		warn("WARNING: indexDP output file <$file> not found - skipping\n");
		next;
	    }
	    Helicos::Exception::Simple->throw("indexDP output file <$file> not found\n");
	}
	if ($suffix eq "binary.bin") {
	    $binary_alignment_file = $file;
	}
	elsif ($suffix=~/\.sms$/) {
	    push @sms_files, $file;
	}
    }
}

### RUN SUMMALIGN ###

print STDOUT &timestamped_string("Runnnig summAlign.\n");

my $summary_alignment_file = join("/",$working_directory,"summary_alignments.bin");
my $summAlign_logfile = join("/",$working_directory,"summAlign.out");
my $summAlign_errfile = join("/",$working_directory,"summAlign.err");

{
    my $program_name = 'summAlign';
    my @args;
    push @args, "--align_file","$binary_alignment_file";
    push @args, "--out_file","$summary_alignment_file";
    push @args, "--min_score","$params{aligned_files_threshold}";
    push @args, "--max_diff","$params{max_diff}";
    push @args, "--max_align_num","$params{max_align_num}";

    my $cmd = join(" ",$program_name,@args);
    
    print STDOUT &timestamped_string("Excecuting command: $cmd\n");
    
    my $stats = &execute_command("$cmd > $summAlign_logfile 2> $summAlign_errfile");
    
    print STDOUT &timestamped_string(join("","--- timing: ",&timing_string($stats),"\n"));
}


### COPY OUTPUT FILES TO FINAL OUTPUT DIRECTORY ###

print STDOUT &timestamped_string("Copying output files to final ouput directory <$params{output_directory}>.\n");

{
    my @files;
    push @files, $summary_alignment_file, $binary_alignment_file;
    push @files, @sms_files;

    foreach my $file ( @files ) {
	my($filename) = fileparse($file);

	my $outfile = join("/",$params{output_directory},$filename);
	
	my $cmd = "cp -p $file $outfile";
	
	print STDOUT &timestamped_string("Excecuting command: $cmd\n");
	
	my $stats = &execute_command("$cmd > /dev/null 2> /dev/null");
    }
}


### CLEANUP ###

chdir($launch_dir);

print STDOUT &timestamped_string("Removing working directory <$working_directory>.\n");


eval { rmtree($working_directory); };
if ($@) {
    #TODO: may want to just warn to avoid crash when someone is viewing an output file (e.g. tail -f log)
    my $e = Helicos::Exception::Simple->new("unable to remove working directory <$working_directory>\n");
    warn $e->to_string();
}

### FINISH ###
    
print STDOUT &timestamped_string("Alignment task complete.\n");

#close(STDOUT) or Helicos::Exception::Simple->throw("close of STDOUT failed\n");

exit;



######################################################################


sub _usage
{
    my $usage;
    my @lines;
    push @lines,"Synopsis:";
    push @lines,"     alignment_wrapper.dge.pl [parameters/options]";
    push @lines,"     alignment_wrapper.dge.pl --help";
    push @lines,"";
    push @lines,"Parameters/Options:";
    push @lines,"     --sms_file=arg                 path to input SMS strand data file";
    push @lines,"     --reference_file=arg           path to reference sequence file in fasta format";
    push @lines,"     --seed_size=arg                indexDP seed size";
    push @lines,"     --weight=arg                   indexDP weight parameter";
    push @lines,"     --num_errors=arg               indexDP number of errors parameter";
    push @lines,"     --config_file=arg              indexDP alignment mode configuration file";
    push @lines,"     --template_repository=arg      path to directory containing indexDP template family files";
    push @lines,'     --working_directory=arg        temporary workspace directory [default: $ENV{TMPDIR} or /tmp]';
    push @lines,"     --output_directory=arg         final output directory - results are copied here";
    push @lines,"     --out_prefix=arg               indexDP output prefix parameter";
    push @lines,"     --flow_cell=arg                indexDP flow cell parameter";
    push @lines,"     --channel=arg                  indexDP channel parameter";
    push @lines,"     --num_blocks=arg               indexDP number of blocks parameter";
    push @lines,"     --block_index=arg              indexDP block index file";
    push @lines,"     --max_hit_duplication=arg      indexDP maximum hit duplication parameter";
    push @lines,"     --percent_error=arg            indexDP percent_error parameter";
    push @lines,"     --min_norm_score=arg           indexDP min_norm_score parameter";
    push @lines,"     --aligned_files_threshold=arg  indexDP aligned_files_threshold parameter AND summAlign min_score parameter";
    push @lines,"     --max_diff=arg                 summAlign max_diff parameter";
    push @lines,"     --max_align_num=arg            summAlign max_align_num parameter";
    push @lines,"";
    push @lines,"     --verbose=arg                  verbocity level [default:0]";
    push @lines,"     --debug=arg                    debug flag [0|1] [default:0]";
    push @lines,"";
    push @lines,"     --help                         print this help message";

    $usage = join("",join("\n",@lines),"\n");
    return $usage;
}



__END__

# $ENV{TMPDIR} (unless taint is on) and /tmp

