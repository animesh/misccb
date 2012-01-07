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
# FILE:          script.pl
# DESCRIPTION:   
# AUTHORS:       Steven Roels
# ================================================================================

use 5.008;

use strict;
use vars qw( $REVISION $VERSION %exp_params) ;

use Getopt::Long;
use IO::File;
use Data::Dumper;

use File::Path qw(mkpath);
#use File::Copy;
use File::Basename qw(basename);
use Data::Dumper;
use Cwd qw(chdir getcwd abs_path);

use Helicos::Util::StrandTemplateFile;
use Helicos::Util qw(execute_command);

$REVISION = "";
$VERSION = "";

$| = 1;

my $temp2sms = "temp2sms";

my $help = 0;
my $verbose = 0;
my $debug = 0;

my $run_name;
my $image_analysis_root;
my $positions_per_channel;
my $instrument_name;
my $flow_order_string;
my $max_orphan_object_filter;
my $output_file_version;
my $exclude_filtered_strands;
my $pass2_start;
my $number_of_channels;
my $number_of_cameras=4;
my $template_file_version;
my $flow_cell_1_name;
my $flow_cell_2_name;
my $template_file_name = "hc_with_template.txt";
my $sample_fraction;
my $validate = 1; #run db_with_template_preprocessor.pl script
my $exclude_control_positive = 1;  #do this by default for now because control frame positive strands are automatically excluded by temp2sms

my $start_dir = &getcwd();
my $working_dir = &abs_path(join("/",$start_dir,"working"));  #working directory for intermediate files

my $fc1_channel_string;
my $fc2_channel_string;

my $all_channels = 1;

my %channels;

my $original_flow_order_string;

my %OPTIONS = ( 'verbose=i'                => \$verbose,
		'debug=i'                  => \$debug,
		'help'                     => \$help,
		'run_name=s'               => \$run_name,
		'instrument_name=s'        => \$instrument_name,
		'positions_per_channel=s'  => \$positions_per_channel,
		'image_analysis_root=s'    => \$image_analysis_root,
		'template_file_version=i'  => \$template_file_version,
		'flow_order_string=s'      => \$flow_order_string,
		'exclude_filtered_strands=i' => \$exclude_filtered_strands,
		'max_orphan_object_filter=i' => \$max_orphan_object_filter,
		'output_file_version=i'    => \$output_file_version,
		'pass2_start=i'            => \$pass2_start,
		'flow_cell_1_name=s'       => \$flow_cell_1_name,
		'flow_cell_2_name=s'       => \$flow_cell_2_name,
		'template_file_name=s'     => \$template_file_name,
		'sample_fraction=s'        => \$sample_fraction,
		'validate=i'               => \$validate,
		'flow_cell_1_channels=s'   => \$fc1_channel_string,
		'flow_cell_2_channels=s'   => \$fc2_channel_string );

unless (&GetOptions(%OPTIONS)) {
    die "FATAL: invalid options or option values\n\n",&_usage();
}

if ($help) { print &_usage(); exit; }

unless(defined($image_analysis_root)) {
    $image_analysis_root='.';
}

defined($output_file_version) or $output_file_version = $template_file_version;

defined($positions_per_channel) or die;
defined($run_name) or die;
defined($flow_order_string) or die;

if (defined($pass2_start)) {
    $pass2_start=~/^\d+$/ or die;
}

if (defined($sample_fraction)) {
    ($sample_fraction>0 and $sample_fraction<1) or die;
}

$validate=~/^(0|1)$/ or die;

if (defined($fc1_channel_string) or defined($fc2_channel_string)) {
    $all_channels = 0;

    my %str = ();
    if (defined($fc1_channel_string)) {$str{1}=$fc1_channel_string;}
    if (defined($fc2_channel_string)) {$str{2}=$fc2_channel_string;}
    
    my @fc = sort {$a<=>$b} keys %str;

    foreach my $fc (@fc) {
	my $str=$str{$fc};
	my @tokens = split(/,/,$str,-1);
	@tokens or die "FATAL: invalid channel argument <$str> for flow cell <$fc>\n";
	foreach my $token (@tokens) {
	    my @ch = split(/\-/,$token,-1);
	    (@ch==1 or @ch==2) or die "FATAL: invalid channel argument <$str> for flow cell <$fc>\n";
	    if (@ch==2) {
		$ch[1]>$ch[0] or die "FATAL: invalid channel argument <$str> for flow cell <$fc>\n";
		@ch=($ch[0]..$ch[1]);
	    }
	    foreach my $ch (@ch) {
		(exists($channels{$fc}) and exists($channels{$fc}->{$ch})) and die "FATAL: invalid channel argument <$str> for flow cell <$fc>: duplicate channel <$ch>\n";
		$channels{$fc}->{$ch}++;
	    }
	}
    }
}

$template_file_version=~/^(2|3)$/ or die;
($template_file_version==2 and defined($max_orphan_object_filter)) and die;
if ($template_file_version==2) { (defined($exclude_filtered_strands) and $exclude_filtered_strands=~/^(0|1)$/) or die; }
$output_file_version=~/^(2|3)$/ or die;

($template_file_version==2 and $output_file_version==3) and die;

if ($template_file_version>2 and ! defined($max_orphan_object_filter)) {
    $max_orphan_object_filter = 1000;
}

if ($instrument_name=~/^(Gazerbeam|Flash|CTS\d+)$/) {
    $number_of_cameras=4;
    $number_of_channels=25;
}
elsif ($instrument_name=~/^(Alpha\d+|Optics\d+)$/) {    
    $number_of_cameras=1;
    if ($instrument_name=~/^Alpha(3|4)$/) {    
	$number_of_channels=5;
    }
    else {
	$number_of_channels=1;
    }
}
else {die;}

{
    my @ppc = split(/,/,$positions_per_channel,-1);
    grep {! /^\d+$/} @ppc and die;
    if (@ppc>1) {
	@ppc == $number_of_channels or die;
	$positions_per_channel=\@ppc;
    }
}

$original_flow_order_string=$flow_order_string;

my $fos_format;
if ($flow_order_string=~/U/) {
    $fos_format='old';
    $flow_order_string=~tr/TRXU/ttxT/;
}
else {
    $fos_format='new';
}

$flow_order_string=~/^[txACTG]+$/ or die;

$image_analysis_root=~s/\/\/+/\//;
unless ($image_analysis_root eq "/") {
    $image_analysis_root=~s/\/$//;
}

unless ($image_analysis_root ne "" and -d $image_analysis_root) {
    die("FATAL: image analysis root <$image_analysis_root> not a directory\n");
}

if (-e $working_dir) {
    die("FATAL: working dir <$working_dir> exists");
}

mkpath($working_dir) or die;
mkpath("${working_dir}/preprocess") or die;
mkpath("${working_dir}/tmpfiles") or die;

my $cmd = "find ${image_analysis_root} -name ${template_file_name}";

my @template_files=`$cmd`;
my $status=$?;
if ($status) {die;}

chomp (@template_files);

my %files;

foreach my $file (@template_files) {
    $file=~s/\/\/+/\//;
    next if $file=~/^$/;
    my $flow_cell=1;
    my $channel=1;

    my $base=$image_analysis_root;
    if ($base eq "/") {$base="";}
    
    $file=~/^${base}\/(.*)/ or die;
    
    my $relpath=$1;
    if (defined($flow_cell_1_name)) {
	if ($relpath=~/^(|.*?\/)${flow_cell_1_name}\//i) {
	    $flow_cell=1;
	}
	elsif ($relpath=~/^(|.*?\/)${flow_cell_2_name}\//i) {
	    $flow_cell=2;
	}
	else {die;}
    }

    if ($relpath=~/^(|.*?\/)channel_?(\d+)/i) {
	$channel=$2;
    }

    warn "FILE: $file : flowcell <$flow_cell> channel <$channel>\n";

    if (exists($files{$flow_cell}) and exists($files{$flow_cell}->{$channel})) {
	die;
    }
    
    next unless ($all_channels or (exists($channels{$flow_cell}) and exists($channels{$flow_cell}->{$channel})));
    
    $files{$flow_cell}->{$channel}=$file;
}

print Dumper(\%files);

if (! $all_channels) {
    my %missing;
    foreach my $flow_cell (sort {$a<=>$b} keys %channels) {
	foreach my $channel (sort {$a<=>$b} keys %{$channels{$flow_cell}}) {
	    unless (exists($files{$flow_cell}) and exists($files{$flow_cell}->{$channel})) {
		$missing{$flow_cell}->{$channel}++;
		warn("ERROR: no template file found corresponding to requested flowcell <$flow_cell> and channel <$channel>\n");
	    }
	}
    }
    if (scalar(keys %missing)>0) {
	die("FATAL: missing template data for requested flow cell and channel combination(s)\n");
    }
}

if ($validate) {
    my %pfiles;
    my @pfiles;

    chdir("${working_dir}/preprocess");

    foreach my $flow_cell (1,2) {
	next unless exists($files{$flow_cell});
	my $fcpath=abs_path("./flow_cell_${flow_cell}");
	mkpath($fcpath) or die;
	foreach my $channel (sort {$a<=>$b} keys %{$files{$flow_cell}}) {
	    my $chpath=abs_path("./flow_cell_${flow_cell}/channel${channel}");
	    mkpath($chpath) or die;
	    my $file=$files{$flow_cell}->{$channel};
	    my($filename) = basename($file);
	    my $copy=join("/",$chpath,$filename);
	    system("cp ${file} ${copy}") and die;
	    $pfiles{$flow_cell}->{$channel}=$copy;
	    push @pfiles,$copy;
	}
    }	    
	
    my $fos_arg = $original_flow_order_string;

    if ($fos_format eq 'new') {
	#must use old-style flow order string for db_with_template_preprocessor.pl
	$fos_arg=~tr/txT/TXU/;
    }

    my $prg = "db_with_template_preprocessor.pl";
    my @cargs;
    push @cargs, "--flow_order_string=${fos_arg}";
    push @cargs, "--replace=1";
    push @cargs, "--suffix=.original";
    push @cargs, @pfiles;

    my $cmd = join(" ",$prg,@cargs);

    print("Excecuting command: $cmd\n");

    my $stats = &execute_command("$cmd > preprocess.log 2> preprocess.err");

    %files=%pfiles;

    chdir($start_dir);
}

chdir("${working_dir}/tmpfiles");

my %tmpfiles;

foreach my $flow_cell (1,2) {
    next unless exists($files{$flow_cell});
    foreach my $channel (sort {$a<=>$b} keys %{$files{$flow_cell}}) {
	my $file=$files{$flow_cell}->{$channel};
	my $fh=IO::File->new($file);
	$fh or die;
	my %tmpfh = map {$_=>undef;} (0..$number_of_cameras-1);
	foreach my $camera (0..$number_of_cameras-1) {
	    my $tmpfile=join(".","flow_cell_${flow_cell}","channel_${channel}","camera_${camera}","txt");
	    $tmpfile=abs_path($tmpfile);
	    $tmpfiles{$flow_cell}->{$channel}->{$camera}=$tmpfile;
	    $tmpfh{$camera}=IO::File->new(">$tmpfile");
	    $tmpfh{$camera} or die;
	}

	print "Processing strand file <$file>\n";
	
	my $stf = Helicos::Util::StrandTemplateFile->new(positions_per_channel=>$positions_per_channel,number_of_channels=>$number_of_channels,number_of_cameras=>$number_of_cameras);

	my ($rand_val);
	if (defined($sample_fraction)) {$rand_val=int(10000*(1/$sample_fraction));}

	while (<$fh>) {
	    chomp;
	    /^(.*)  ((\d+) .* (\d+))$/ or die;
	    my($fs,$meta)=($1,$2);
	    my @meta=split(/ /,$meta,-1);;
	    (@meta==4 or @meta==5) or die;
	    my($filter,$x,$y,$pos)=@meta[0,1,2,-1];

	    my($ichannel,$position,$camera)=$stf->parse_pseudoposition($pos);

	    $ichannel-- if $instrument_name eq "Gazerbeam";

	    $ichannel eq $channel or die;

	    ($template_file_version==2 and $filter !~ /^(0|1)$/) and die;

	    if ($template_file_version==2 and $exclude_filtered_strands and $filter==0) {
		next;
	    }

	    if ($template_file_version==3 and $filter>$max_orphan_object_filter) {
		next;
	    }

	    if ($output_file_version==2 and $template_file_version==3) {
		my $qual=0;
		if ($filter<=$max_orphan_object_filter) {
		    $qual=1;
		}
		$meta[0]=$qual;
	    }

	    if ($exclude_control_positive) {
		next if $fs=~/x/i;
	    }

	    my $ofh = $tmpfh{$camera};
	    
	    $meta[-1]=$position;

	    $meta = join(" ",@meta);

	    my $rec=join("  ",$fs,$meta);
	    
	    if (defined($sample_fraction)) {
		my $n = rand($rand_val)/10000;
		if ($n<=1) {
		    print $ofh "$rec\n";
		}
	    }
	    else {
		print $ofh "$rec\n";
	    }
	}
	
	close($fh) or die;
	foreach my $tmpfh (values(%tmpfh)) {
	    close($tmpfh) or die;
	}
    }
}

chdir($start_dir);

my $cnf = "temp2sms.config";
my $cnffh = IO::File->new(">$cnf");
$cnffh or die;

print $cnffh join("\t","Flowcell","Channel","Camera","File"),"\n";

foreach my $flow_cell (1,2) {
    next unless exists($tmpfiles{$flow_cell});
    foreach my $channel (sort {$a<=>$b} keys %{$tmpfiles{$flow_cell}}) {
	foreach my $camera (sort {$a<=>$b} keys %{$tmpfiles{$flow_cell}->{$channel}}) {
	    my $tmpfile=$tmpfiles{$flow_cell}->{$channel}->{$camera};
	    print $cnffh join("\t",$flow_cell,$channel,$camera,$tmpfile),"\n";
	}
    }
}

{
    #my $cmd = "$temp2sms --input $cnf --output ${run_name}.sms --flow $flow_order_string --filter0";
    my $cmd = "$temp2sms --input $cnf --output ${run_name}.sms --flow $flow_order_string";
    if (defined($pass2_start)) {
	$cmd .= " --split $pass2_start";
    }
    my $status = system($cmd);
    $status and die;
}


exit;

##############

sub _usage
{
    my $usage;
    my @lines;
    push @lines,"Usage:\n";
    push @lines,"  $0 [-verbose=i] [-debug=i]\n";
    push @lines,"  $0 -help\n";
    $usage = join('',@lines);
    return $usage;
}
