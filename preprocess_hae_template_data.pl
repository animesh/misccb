#!/usr/bin/perl -W

# --------------------------------------------------------------------------------
# These materials (including without limitation all articles, text, images, logos, 
# software, and designs) are copyright © 2007 Helicos BioSciences Corporation. 
#
# All rights reserved. 
#
# The materials included herein may be used, downloaded, and printed for personal, 
# noncommercial use only, provided that any material downloaded or printed 
# remains intact and that all copies include the following notice in a clearly 
# visible position:
#
#     "Copyright © 2007 Helicos BioSciences Corporation. All rights reserved." 
#
# These materials may not otherwise be copied or redistributed for commercial or 
# noncommercial purposes or for compensation of any kind without prior written 
# permission from Helicos BioSciences Corporation. 
# --------------------------------------------------------------------------------

# ================================================================================
# FILE:          preprocess_hae_template_data.pl
# DESCRIPTION:   This script is used to extract a sample of files from a compressed archive
#                of strand data in template file format generated vie the HAE pipeline and convert
#                the strand data to standard per-channel db_with_template format.
# AUTHORS:       Steven Roels
# ================================================================================

use strict;
use IO::File;
use Getopt::Long;
use Cwd qw(getcwd chdir abs_path);
use Data::Dumper;
use File::Path qw(mkpath);
use File::Basename;

use Helicos::Util::StrandTemplateFile;
use Helicos::MetaData::Classes;

my $help;

$|=1;

my %instrument=(flow_cells=>2,
		channels_per_flow_cell=>25);

my $basedir;
my $outdir;
my $positions_per_channel;
my $sample_size;
my $position_string;
my $fc1_channel_string;
my $fc2_channel_string;

my $all_positions=1;
my $all_channels=1;

my @fc1_channels;
my @fc2_channels;

my %channels;
my %positions;

my $max_observed_position;

my $number_of_cameras=4;

my $prefix;                      #filename prefix (e.g. run name)
my $flow_cell_1_barcode;
my $flow_cell_2_barcode;

my $imaging_config_file;

my $clipping_roi_filename = 'clipping_roi.txt';

my $max_real_imaging_position = 1500;

my $strict = 0;

my %area_per_position_by_camera;

my $dbwt_pattern = '^([ACGTUXRN\-]( [ACGTUXRN\-])*)  ((\d+) \S.*\S)$';

my %OPTIONS = ( 'input_dir=s'              => \$basedir,
		'output_dir=s'             => \$outdir,
		'positions_per_channel=i'  => \$positions_per_channel,
		'position_sample_size=i'   => \$sample_size,
		'positions=s'              => \$position_string,
		'flow_cell_1_channels=s'   => \$fc1_channel_string,
		'flow_cell_2_channels=s'   => \$fc2_channel_string,
		'flow_cell_1_name=s'       => \$flow_cell_1_barcode,
		'flow_cell_2_name=s'       => \$flow_cell_2_barcode,
		'prefix=s'                 => \$prefix,
		'imaging_config_file=s'    => \$imaging_config_file,
		'strict=i'                 => \$strict,
		'help'                     => \$help );

unless (&GetOptions(%OPTIONS)) {
    die "FATAL: invalid options or option values\n\n",&usage();
}

if ($help) { exit 1; }

print "Verifying parameters...\n";

my $start_dir = getcwd();

{
    my @missing_parameters;
    {
	push @missing_parameters, 'input_dir' unless (defined($basedir));
	push @missing_parameters, 'output_dir' unless (defined($outdir));
	push @missing_parameters, 'flow_cell_1_name' unless (defined($flow_cell_1_barcode));
	push @missing_parameters, 'flow_cell_2_name' unless (defined($flow_cell_2_barcode));
	push @missing_parameters, 'imaging_config_file' unless (defined($imaging_config_file));
    }

    if (@missing_parameters) {
	die "FATAL: missing required parameter(s): <",join(",",@missing_parameters),">\n";
    }

    if (defined($positions_per_channel)) {
	($positions_per_channel=~/^\d+$/ and $positions_per_channel>0) or die "FATAL: invalid positions_per_channel value <$positions_per_channel>\n";
	if ($positions_per_channel>$max_real_imaging_position) {
	    die "FATAL: specified positions per channel <$positions_per_channel> is larger than maximum expected true imaging position value <$max_real_imaging_position>\n";
	}
    }
    else {
	my $txt = "positions_per_channel not specified - willl make best guess based on the data but this is not recommended";
	print "-- $txt\n";
	warn "WARNING: $txt\n";
    }
    
    unless (defined($prefix)) {
	my $txt = "strand filename prefix not specified - willl make best guess based on the data but this is not recommended";
	print "-- $txt\n";
	warn "WARNING: $txt\n";
    }
}

$strict=~/^(0|1)$/ or die "FATAL: invalid value for parameter \"strict\" <$strict> - must be [0|1]\n";

($flow_cell_1_barcode ne "" and $flow_cell_1_barcode=~/^\S+$/) or die "FATAL: invalid flow cell 1 name <$flow_cell_1_barcode>\n";
($flow_cell_2_barcode ne "" and $flow_cell_2_barcode=~/^\S+$/) or die "FATAL: invalid flow cell 2 name <$flow_cell_2_barcode>\n";

($basedir,$outdir,$imaging_config_file) = map {abs_path($_);} ($basedir,$outdir,$imaging_config_file);

unless (-d $basedir) { die "FATAL: input_dir <$basedir> does not exist or is not a directory\n"; }
$basedir = abs_path($basedir);

{
    my $tmp = abs_path($outdir);
    unless (defined($tmp) and $tmp ne "") {
	die "FATAL: output_dir <$outdir> not valid - directory or parent directory must exist\n";
    }
    $outdir = $tmp;
}

{
    my $tmp = abs_path($imaging_config_file);
    unless (defined($tmp) and $tmp ne "") {
	die "FATAL: imaging config file <$imaging_config_file> not found\n";
    }
    $imaging_config_file = $tmp;
    unless (-f $imaging_config_file) {
	die "FATAL: imaging config file <$imaging_config_file> not a plain file\n";
    }
}

if (defined($prefix)) {
    ($prefix ne "" and $prefix=~/^\S+$/) or die "FATAL: invalid prefix <$prefix>\n";
}

if (defined($position_string) and defined($sample_size)) {
    die "FATAL: invalid conbination of parameters - positions and position_sample_size are mutually exclusive\n";
}

if (defined($sample_size)) {
    unless ($sample_size=~/^\d+$/ and $sample_size>0) { die "FATAL: invalid position_sample_size argument <$sample_size>\n"; }
    defined($positions_per_channel) or die "FATAL: positions_per_channel parameter is required if position_sample_size parameter is used\n";
}

if (defined($position_string)) {
    $all_positions=0;
    my @tokens = split(/,/,$position_string,-1);
    @tokens or die "FATAL: invalid positions argument <$position_string>\n";
    foreach my $token (@tokens) {
	my @pos = split(/\-/,$token,-1);
	(@pos==1 or @pos==2) or die "FATAL: invalid position argument <$position_string>\n";
	grep {!/^\d+$/} @pos and die "FATAL: invalid position argument <$position_string>\n";
	if (@pos==2) {
	    $pos[1]>$pos[0] or die "FATAL: invalid position argument <$position_string>\n";
	    @pos=($pos[0]..$pos[1]);
	}
	foreach my $pos (@pos) {
	    exists($positions{$pos}) and die "FATAL: invalid position argument <$position_string>: duplicate position <$pos>\n";
	    $positions{$pos}++;
	}
    }
}
elsif (defined($sample_size)) {
    $all_positions=0;

    my(@corepos) = qw(1 25 45 60 75 76 175 275 276 375 475 476 575 576 675 775 875 975 1075 1175 1176 1275 1375 1475);
    my(@extrapos) = qw(10 15 100 125 150 200 225 250 300);

    my @pos = grep {$_<=$positions_per_channel} @corepos;
    if (scalar(@pos)>$sample_size) {
	@pos=@pos[0..$sample_size-1];
    }
    else {
	my @tmp=@extrapos;
	while (@tmp and scalar(@pos)<$sample_size) {
	    last if $tmp[0]>$positions_per_channel;
	    push @pos,shift @tmp;
	}
    }
    if (@pos<$sample_size) {
	die("FATAL: position sample size <$sample_size> too large with current implementation - try a value less than or equal to ",scalar(@pos),"\n");
    }
    map {$positions{$_}++;} @pos;
}

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

#verify input and output directories

print "Verifying input/output directories...\n";

unless (-d $basedir) {
    die "FATAL: input dir <$basedir> does not exist or is not a directory\n";
}

if (-e $outdir) {
    if (-d $outdir) {
	opendir(DIR,$outdir) or die "FATAL: unable to open directory <$outdir> for reading\n";
	my @files=grep {!/^\.\.?$/} readdir DIR;
	if (@files) {
	    die "FATAL: output directory <$outdir> exists and is not empty\n";
	}
    }
    else {die "FATAL: file $outdir exists (and is not a directory)\n";}
}
else {
    mkpath($outdir) or die "FATAL: unable to create directory <$outdir>\n";
}

#process imaging config file

print "Processing imaging config file <$imaging_config_file>\n";

{
    my $script;
    {
	my $fh=IO::File->new($imaging_config_file);
	$fh or die "FATAL: unable to open file <$imaging_config_file> for reading\n";
	local $/=undef;
	$script=<$fh>;
    }
    my %imaging_config;
    eval($script);  #sets %imaging_config
    if ($@) {
	die("$0: FATAL: eval failed\n",$@);
    }
    
    my $ncam = $imaging_config{cameras};
    $number_of_cameras==$ncam or die;
    my @cpos = @{$imaging_config{camera_position}};
    my @object_rois = @{$imaging_config{object_rois}};
    my @fov = @{$imaging_config{fov}};
    grep {$_ != 0} @fov[0,1] and die;
    $number_of_cameras==@cpos or die;
    my($width,$height)=($fov[2]+1,$fov[3]+1);
    my $fov = Helicos::MetaData::FieldOfView->new(width=>$width,height=>$height);
    foreach my $camera_id (0..${number_of_cameras}-1) {
	my $pos = $cpos[$camera_id];
	my @object_roi = @{$object_rois[$camera_id]};
	my $camera= Helicos::MetaData::Camera->new(id=>$camera_id,position=>$pos,field_of_view=>$fov);
	my $roi = Helicos::MetaData::ImageRegionOfInterest::Rectangular->new(x0=>$object_roi[0],
									     y0=>$object_roi[1],
									     x1=>$object_roi[2],
									     y1=>$object_roi[3],
									     camera=>$camera);
	$area_per_position_by_camera{$camera_id}=$roi->area;
    }
}


my %flow_cell_to_barcode = (1=>$flow_cell_1_barcode,
			    2=>$flow_cell_2_barcode);

my %barcode_to_flowcell = reverse(%flow_cell_to_barcode);


print "Dataset:\n";

{
    my $pos_str;
    if ($all_positions) {
	$pos_str = "all";
    }
    else {
	my @pos=sort {$a<=>$b} keys %positions;
	my @intervals = &_integer_intervals(@pos);
	my @out;
	foreach my $interval (@intervals) {
	    my($min,$max)=@$interval;
	    my $txt = ($max==$min) ? $min : join("..",$min,$max);
	    push @out,$txt;
	}
	$pos_str = join(",",@out);
    }
    my @fc = sort {$a<=>$b} keys %channels; 
    foreach my $flow_cell (@fc) {
	my $ch_str;
	my @channels = sort {$a<=>$b} keys %{$channels{$flow_cell}};
	my @intervals = &_integer_intervals(@channels);
	my @out;
	foreach my $interval (@intervals) {
	    my($min,$max)=@$interval;
	    my $txt = ($max==$min) ? $min : join("..",$min,$max);
	    push @out,$txt;
	}
	$ch_str = join(",",@out);
	print "--  Flowcell <$flow_cell> : Channels <$ch_str> : Positions <$pos_str>\n";
    }
}


opendir(BASEDIR,$basedir) or die;
my @archives = map {join("/",$basedir,$_);} grep {/tar/} grep {!/^\./} readdir(BASEDIR);

print "Archives:\n";
foreach my $archive (@archives) {
    print "-- $archive\n";
}

my %all_files;
my %files;
my %files_by_archive;
my %clipping_files_by_archive;

my $format;

my $prefix_pattern;
if (defined($prefix)) {$prefix_pattern=$prefix; map { s/\-/\\-/g; s/\./\\./g; } ($prefix_pattern);}
my $suffix_pattern = '-(\d+)-(\d+)-(\d+).txt';
my $base_pattern = join("","($flow_cell_to_barcode{1}|$flow_cell_to_barcode{2})",$suffix_pattern);
map { s/\-/\\-/g; s/\./\\./g; } ($suffix_pattern,$base_pattern);

print "Expected file pattern: prefix <",(defined($prefix) ? $prefix : 'undef'),"> base pattern <$base_pattern>\n";

my %observed_prefixes;

print "Scanning archives at: $basedir\n";

foreach my $archive (@archives) {
    my $name = $archive;

    print "-- scanning archive: $archive\n";

    $name=~s/^.*\///;
    unless ($name=~/^(.*)\.tar\.(gz|bz2)$/) {
	die "FATAL: archive name <$name> does not match expected pattern";
    }
    $name=$1;
    unless (defined($format)) {$format=$2;}
    $format eq $2 or die;
    my $fh;
    if ($format eq 'gz') {
	$fh = IO::File->new("gunzip -c $archive | tar -tf - |");
    }
    else {
	$fh = IO::File->new("bunzip2 -c $archive | tar -tf - |");
    }    
    $fh or die;

    while (<$fh>) {
	chomp;
	my $rel_path=$_;

	next if $rel_path=~/\/$/; #ignore directories

	#print "---- file: $rel_path\n";

	my $filename=$rel_path;
	$filename=~s/^.*\///;
	
	if ($filename=~/^clipping\-offsets\-\d*\.txt$/) {   #strands-IA2/clipping-offsets-18186.txt
	    $clipping_files_by_archive{$archive}->{$rel_path}++;
	    next;
	}
	
	unless ($filename=~/${suffix_pattern}$/o) {
	    die "FATAL: file <$rel_path> in archive file <$archive>: name does not match expected suffix pattern\n";
	}

	unless ($filename=~/^(.*)${base_pattern}$/o) {
	    if (defined($prefix) and $filename=~/^${prefix_pattern}/o) {
		die "FATAL: file <$rel_path> in archive file <$archive>: name matches prefix but does not match expected base pattern\n" ;
	    }
	    warn "WARNING: file <$rel_path> in archive file <$archive>: name does not match expected base pattern - ignoring\n";
	    next;
	}
	    
	my($pre,$flowcell_barcode,$channel,$position,$camera)=($1,$2,$3,$4,$5);

	$observed_prefixes{$pre}++;

	if (defined($prefix) and $pre ne $prefix) {
	    die "FATAL: file <$rel_path> in archive file <$archive>: name matches base pattern but prefix <$pre> does not match expected prefix <$prefix>\n";
	}

	if (! exists($barcode_to_flowcell{$flowcell_barcode})) {
	    if (defined($prefix) and $pre eq $prefix) {
		die "FATAL: file <$rel_path> in archive file <$archive>: name matches prefix <$prefix> but not barcode patterns\n";
	    }
	    warn "WARNING: file <$rel_path> in archive file <$archive>: name <$pre> does not match expected barcodes - ignoring\n";
	    next;
	}

	if (defined($positions_per_channel) and $position>$positions_per_channel) {
	    unless ($position>$max_real_imaging_position) {
		die "FATAL: file <$rel_path> in archive file <$archive>: position <$position> is larger than specified positions per channel <$positions_per_channel> but does not appear to be a focus stack position (position>$max_real_imaging_position)\n";
	    }
	}

	if ($position>$max_real_imaging_position) {
	    #assume this is a focus stack position
	    warn "WARNING: file <$rel_path> in archive file <$archive>: position value <$position> suggests this is a focus stack - ignoring\n";
	    next;
	}

	my $flow_cell = $barcode_to_flowcell{$flowcell_barcode};

	if (exists($all_files{$flow_cell}->{$channel}->{$position}->{$camera})) {
	    die "FATAL: multiple files for flowcell <$flow_cell>, channel <$channel>, position <$position>, camera <$camera>\n";
	}

	$all_files{$flow_cell}->{$channel}->{$position}->{$camera} = {archive=>$archive,path=>$rel_path};
	
	if (! $all_channels) {
	    #print "---- file: $rel_path flowcell <$flow_cell>, channel <$channel>, position <$position>, camera <$camera> \n";
	    next unless (exists($channels{$flow_cell}) and exists($channels{$flow_cell}->{$channel}));
	}

	next unless ($all_positions or exists($positions{$position}));

	#print "---- file: $rel_path PASSED\n";
	
	$files{$flow_cell}->{$channel}->{$position}->{$camera} = {archive=>$archive,path=>$rel_path};
	$files_by_archive{$archive}->{$rel_path}++;
    }

    close($fh) or die;
}

print "Completed scanning archives\n";

#print Dumper(\%all_files,\%files,\%files_by_archive,\%clipping_files_by_archive,\%observed_prefixes);

#verify observed file set

print "Verifying archive content..\n";

{
    my %all_observed_positions;
 
    my @observed_flow_cells = sort {$a<=>$b} keys %all_files;
    my %observed_flow_cells = map {$_=>1} @observed_flow_cells;

    foreach my $flow_cell (@observed_flow_cells) {
	my @observed_channels = sort {$a<=>$b} keys %{$all_files{$flow_cell}};
	foreach my $channel (@observed_channels) {
	    my @observed_positions = sort {$a<=>$b} keys %{$all_files{$flow_cell}->{$channel}};
	    my %observed_positions = map {$_=>1} @observed_positions;
	    map {$all_observed_positions{$_}++;} @observed_positions;
	}
    }

    my @all_observed_positions = sort {$a<=>$b} grep {$_<=1400;} keys %all_observed_positions;
    $max_observed_position = $all_observed_positions[-1];

    if (defined($positions_per_channel)) {
	unless ($positions_per_channel == $max_observed_position) {
	    if ($positions_per_channel < $max_observed_position) {
		die "FATAL: max observed position <$max_observed_position> is larger than the supplied positions per channel value <$positions_per_channel>\n";
	    }
	    else {
		warn "WARNING: max observed position <$max_observed_position> is less than the supplied positions per channel value <$positions_per_channel>\n";
	    }
	}
    }

    if (! defined($positions_per_channel)) {
	my $txt = "positions_per_channel not specified - using maximum position encountered in archives <$max_observed_position>";
	print "-- $txt\n";
	warn "WARNING: $txt\n";
    }

    my $max_position_to_check = defined($positions_per_channel) ? $positions_per_channel : $max_observed_position;


    my @missing_flow_cells = grep {! exists($observed_flow_cells{$_});} (1,2);

    if (@missing_flow_cells) {
	if (! $all_channels) {
	    my @missing_requested = grep {exists($channels{$_});} @missing_flow_cells;
	    if (@missing_requested) {
		die "FATAL: no files found in archives for requested flowcells <",join(",",@missing_requested),">\n";
	    }	    
	}
	warn "WARNING: no files found in archives for flowcells <",join(",",@missing_flow_cells),">\n";
    }
    
    # first check all combinations (just warn if missing)
    
    foreach my $flow_cell (@observed_flow_cells) {
	my @observed_channels = sort {$a<=>$b} keys %{$all_files{$flow_cell}};
	my %observed_channels = map {$_=>1} @observed_channels;
	my @missing_channels = grep {! exists($observed_channels{$_});} (1..25);
	if (@missing_channels) {
	    warn "WARNING: no files found for channels <",join(",",@missing_channels),"> for flow cell <$flow_cell>\n";
	}
	foreach my $channel (@observed_channels) {
	    my @observed_positions = sort {$a<=>$b} keys %{$all_files{$flow_cell}->{$channel}};
	    my %observed_positions = map {$_=>1} @observed_positions;	    
	    my @missing_positions = grep {! exists($observed_positions{$_});} (1..$max_position_to_check);
	    if (@missing_positions) {
		warn "WARNING: no files found for positions <",join(",",@missing_positions),"> for flow cell <$flow_cell> channel <$channel>\n";
	    }
	    foreach my $position (@observed_positions) {
		my @observed_cameras = sort {$a<=>$b} keys %{$all_files{$flow_cell}->{$channel}->{$position}};
		my %observed_cameras = map {$_=>1} @observed_cameras;	    
		my @missing_cameras = grep {! exists($observed_cameras{$_});} (0..3);
		if (@missing_cameras) {
		    warn "WARNING: no files found for cameras <",join(",",@missing_cameras),"> for flow cell <$flow_cell> channel <$channel> position <$position>\n";
		}
	    }
	}
    }


    #now just verify we have all data requested (may be subset)

    foreach my $flow_cell (@observed_flow_cells) {
	my @observed_channels = sort {$a<=>$b} keys %{$all_files{$flow_cell}};
	my %observed_channels = map {$_=>1} @observed_channels;
	my @channel_set = $all_channels ? (1..25) : (sort {$a<=>$b} keys %channels);
	my @missing_channels = grep {! exists($observed_channels{$_});} @channel_set;
	if (@missing_channels) {
	    my $txt = join("","no files found for requested channels <",join(",",@missing_channels),"> for flow cell <$flow_cell>\n");
	    if ($strict) {die "FATAL: $txt";}
	    warn "WARNING: $txt";
	}
	foreach my $channel (@observed_channels) {
	    my @observed_positions = sort {$a<=>$b} keys %{$all_files{$flow_cell}->{$channel}};
	    my %observed_positions = map {$_=>1} @observed_positions;
	    my @positions_set = $all_positions ? (1..$max_position_to_check) : (sort {$a<=>$b} %positions);
	    my @missing_positions = grep {! exists($observed_positions{$_});} @positions_set;
	    if (@missing_positions) {
		if (! $all_positions) {
		    my $txt = join("","no files found for requested positions <",join(",",@missing_positions),"> for flow cell <$flow_cell> channel <$channel>\n");
		    if ($strict) {die "FATAL: $txt";}
		    warn "WARNING: $txt";
		}
	    }
	    foreach my $position (@observed_positions) {
		my @observed_cameras = sort {$a<=>$b} keys %{$all_files{$flow_cell}->{$channel}->{$position}};
		my %observed_cameras = map {$_=>1} @observed_cameras;	    
		my @missing_cameras = grep {! exists($observed_cameras{$_});} (0..3);
		if (@missing_cameras) {
		    warn "WARNING: no files found for requested cameras <",join(",",@missing_cameras),"> for flow cell <$flow_cell> channel <$channel> position <$position>\n";
		}
	    }
	}
    }
}

print "Completed verifying archive content\n";


chdir($outdir);

my $tmp_dir = join("/",$outdir,"_tmp");
my $tmp_clipping_dir = join("/",$tmp_dir,"clipping");
my $tmp_strand_dir = join("/",$tmp_dir,"strands");

mkpath($tmp_dir) or die "FATAL: unable to make temporary directory <$tmp_dir>\n";
mkpath($tmp_clipping_dir) or die "FATAL: unable to make temporary directory <$tmp_clipping_dir>\n";
mkpath($tmp_strand_dir) or die "FATAL: unable to make temporary directory <$tmp_strand_dir>\n";

chdir($tmp_clipping_dir);

print "Extracting archive clipping file content\n";

foreach my $archive (sort keys %clipping_files_by_archive) {

    print "-- extracting clipping file content from archive: $archive\n";

    my @files=sort keys %{$clipping_files_by_archive{$archive}};
    
    my $tmp_file_list = "${tmp_dir}/filelist.clipping.txt";

    my $fh=IO::File->new(">$tmp_file_list");
    $fh or die "FATAL: unable to open file <$tmp_file_list> for writing\n";
    print $fh join("\n",@files),"\n";
    close($fh) or die "FATAL: error on close of file <$tmp_file_list>\n";
    
    my $cmd;
    if ($format eq 'gz') {
	$cmd = "tar -T $tmp_file_list -zxf $archive";
    }
    else {
	$cmd = "tar -T $tmp_file_list -jxf $archive";
    }

    print "---- executing command: $cmd\n";

    my $status = system($cmd);
    $status and die "FATAL: command <$cmd> failed: <$status>\n";

    unlink($tmp_file_list) or die "FATAL: unable to remove file <$tmp_file_list>\n";
}

print "Completed extracting archive clipping content\n";

chdir($tmp_strand_dir);

print "Extracting archive strand content\n";

foreach my $archive (sort keys %files_by_archive) {

    print "-- extracting strand file content from archive: $archive\n";

    my @files=sort keys %{$files_by_archive{$archive}};
    
    my $tmp_file_list = "${tmp_dir}/filelist.strands.txt";

    my $fh=IO::File->new(">$tmp_file_list");
    $fh or die "FATAL: unable to open file <$tmp_file_list> for writing\n";
    print $fh join("\n",@files),"\n";
    close($fh) or die "FATAL: error on close of file <$tmp_file_list>\n";
    
    my $cmd;
    if ($format eq 'gz') {
	$cmd = "tar -T $tmp_file_list -zxf $archive";
    }
    else {
	$cmd = "tar -T $tmp_file_list -jxf $archive";
    }

    print "---- executing command: $cmd\n";

    my $status = system($cmd);
    $status and die "FATAL: command <$cmd> failed: <$status>\n";

    unlink($tmp_file_list) or die "FATAL: unable to remove file <$tmp_file_list>\n";
}

print "Completed extracting archive strand content\n";


chdir($tmp_clipping_dir);

print "Processing clipping data...\n";

my %clipping;

foreach my $archive (sort keys %clipping_files_by_archive) {
    my @files=sort keys %{$clipping_files_by_archive{$archive}};
    foreach my $file (@files) {
	my $ifh = IO::File->new($file);
	$ifh or die;
	while (<$ifh>) {
	    chomp;
	    my @f = split(/ /,$_,-1);
	    @f==5 or die;
	    my($name,$x0,$y0,$dx,$dy) = @f;

	    $name=~/${base_pattern}$/o or die "FATAL: filename <$name> in archive file <$file>: name does not match expected base pattern\n" ;
	    my($flowcell_barcode,$channel,$position,$camera)=($1,$2,$3,$4);

	    exists($barcode_to_flowcell{$flowcell_barcode}) or die;
	    my $fc=$barcode_to_flowcell{$flowcell_barcode};

	    foreach ($x0,$y0) {
		if ($_<0.1) {$_=0.1;}
	    }

	    if ( grep {!/^\-?(\d+(\.\d+)?|\.\d+)$/} ($x0,$y0) or
		 grep {!/^(\d+(\.\d+)?|\.\d+)$/} ($dx,$dy) ) {
		# may get nonsense values if a flow cell fails for example
		#only throw exception if this flowcell/channel/position was requested
		my $txt = "invalid clipping value(s) <$x0,$y0,$dx,$dy> for name <$name>: flowcell <$fc> channel <$channel> position <$position> camera <$camera>";
		if ( ($all_channels or exists($channels{$fc})) and ($all_positions or exists($positions{$position})) ) {
		    die "FATAL: $txt\n";
		}
		warn "WARNING: $txt\n";
	    }
	    
	    my $area = $dx * $dy;
	    
	    if (exists($clipping{$fc}) and 
		exists($clipping{$fc}->{$channel}) and 
		exists($clipping{$fc}->{$channel}->{$position}) and 
		exists($clipping{$fc}->{$channel}->{$position}->{$camera})) {
		my $key_current = join("\001",@{$clipping{$fc}->{$channel}->{$position}->{$camera}});
		my $key_this = join("\001",$x0,$y0,$dx,$dy,$area);
		if ($key_current eq $key_this) {
		    warn("WARNING: duplicate clipping row for: FC:${fc} CH:${channel} POS:${position} CAMERA:${camera}\n");
		    next;
		}
		else {
		    die("FATAL: inconsistent duplicate clipping rows for: FC:${fc} CH:${channel} POS:${position} CAMERA:${camera}\n");
		}
	    }
	    
	    $clipping{$fc}->{$channel}->{$position}->{$camera}=[$x0,$y0,$dx,$dy,$area];
	}
	close($ifh) or die;
    }
}

chdir($outdir);

print "Writing clipping data...\n";

{
    my $file = join("/",$outdir,$clipping_roi_filename);
    my $fh=IO::File->new(">$file");
    $fh or die "FATAL: unable to open file <$file> for writing\n";
    
    print $fh join("\t","FlowCell","Channel","Position","Camera","x0","y0","dx","dy","Area"),"\n";
    
    foreach my $fc (sort {$a<=>$b} keys %clipping) {
	foreach my $ch (sort {$a<=>$b} keys %{$clipping{$fc}}) {
	    foreach my $pos (sort {$a<=>$b} keys %{$clipping{$fc}->{$ch}}) {
		foreach my $cam (sort {$a<=>$b} keys %{$clipping{$fc}->{$ch}->{$pos}}) {
		    print $fh join("\t",$fc,$ch,$pos,$cam,@{$clipping{$fc}->{$ch}->{$pos}->{$cam}}),"\n";
		}	
	    }
	}
    }
    
    close($fh) or die "FATAL: error on close of file <$file>\n";
}

print "Logging strand file information...\n";

{
    my $strand_data_file_log = join("/",$outdir,'strand_data_files.txt');
    my $fh=IO::File->new(">$strand_data_file_log");
    $fh or die "FATAL: unable to open file <$strand_data_file_log> for writing\n";
    
    print $fh join("\t","FlowCell","Channel","Position","Camera","File"),"\n";
    
    foreach my $fc (sort {$a<=>$b} keys %files) {
	foreach my $ch (sort {$a<=>$b} keys %{$files{$fc}}) {
	    foreach my $pos (sort {$a<=>$b} keys %{$files{$fc}->{$ch}}) {
		foreach my $cam (sort {$a<=>$b} keys %{$files{$fc}->{$ch}->{$pos}}) {
		    print $fh join("\t",$fc,$ch,$pos,$cam,$files{$fc}->{$ch}->{$pos}->{$cam}->{path}),"\n";
		}
	    }
	}	
    }

    close($fh) or die "FATAL: error on close of file <$strand_data_file_log>\n";
}


my $stf;
{
    my $ppc = defined($positions_per_channel) ? $positions_per_channel : $max_observed_position;
    $stf=Helicos::Util::StrandTemplateFile->new(positions_per_channel=>$ppc,
						number_of_channels=>$instrument{channels_per_flow_cell},
						number_of_cameras=>$number_of_cameras);
}


print "Reformatting raw HAE strand data...\n";

chdir($tmp_strand_dir);

my %corrupt_files;

{
    my @flow_cells=sort {$a<=>$b} keys %files;
    foreach my $flow_cell (@flow_cells) {
	my @channels = sort {$a<=>$b} keys %{$files{$flow_cell}};
	foreach my $channel (@channels) {
	    
	    print "-- starting flowcell <$flow_cell> channel <$channel>\n";
	    
	    my $odir = "$outdir/flow_cell_${flow_cell}/channel${channel}";
	    mkpath($odir) or die "FATAL: unable to create output directory <$odir>\n";
	    
	    my $outfile = join("/",$odir,"hc_with_template.txt");
	    my $ofh = IO::File->new(">$outfile");
	    $ofh or die "FATAL: unable to open file <$outfile> for reading\n";
	    
	    my $outfile_strand_count = join("/",$odir,"hc_total_strands.txt");
	    my $ofh_strand_count = IO::File->new(">$outfile_strand_count");
	    $ofh_strand_count or die "FATAL: unable to open file <$outfile_strand_count> for reading\n";
	    
	    my $outfile_area = join("/",$odir,"hc_total_area.txt");
	    my $ofh_area = IO::File->new(">$outfile_area");
	    $ofh_area or die "FATAL: unable to open file <$outfile_area> for reading\n";
	    
	    my $total_strands=0;
	    my $total_area=0;

	    my @positions = sort {$a<=>$b} keys %{$files{$flow_cell}->{$channel}};
	    
	    foreach my $position (@positions) {

		my $tmp = $files{$flow_cell}->{$channel}->{$position};
		
		foreach my $camera (0..$number_of_cameras-1) {
		    my $fc_name = $flow_cell_to_barcode{$flow_cell};
		    my $tempfile;
		    
		    if (exists($tmp->{$camera})) {
			$tempfile = $tmp->{$camera}->{path};
		    }
		    
		    next unless defined($tempfile);
		    
		    $total_area+=$area_per_position_by_camera{$camera};

		    if (-z $tempfile) {
			warn "WARNING: file $tempfile is zero length - assuming failure to satisfy registration threshold\n";
			next;
		    }
		    
		    my $fh = IO::File->new($tempfile);
		    $fh or die "FATAL: unable to open file <$tempfile> for reading\n";
		    
		    #prescan

		    my $corrupt=0;

		    my %frame_string_lengths;
		    while (<$fh>) {
			chomp;
			unless (/${dbwt_pattern}/o) {
			    my $txt;
			    if (eof($fh) and !/\n/) {
				$txt = "strand file <$tempfile> appears to be truncated - last line is incomplete";
			    }
			    else {
			        $txt = "strand file <$tempfile> appears to be corrupt - failure to match general template format at line <$_>";
			    }
			    $corrupt=1;
			    if ($strict) { die "FATAL: $txt\n"; }
			    warn "ERROR: $txt - skipping\n";
			    while (<$fh>) {}  #avoid error on close of handle
			    last;
			}
			my($fs,$meta)=($1,$2);
			$fs=~tr/A-Z\-//cd;
			my $len=length($fs);
			$frame_string_lengths{$len}++;
		    }
		    close($fh) or die;

		    if ($corrupt) {
			my $key = join("\001",$flow_cell,$channel,$position,$camera,$tempfile);
			exists($corrupt_files{$key}) and die;
			$corrupt_files{$key}++;
			next;
		    }

		    my @lengths=sort {$b<=>$a} keys %frame_string_lengths;
		    if (@lengths>1) {
			warn "ERROR: file <$tempfile>: non-uniform frame string lengths\n";		    
		    }
		    
		    my $max_len = $lengths[0];
		    
		    $fh = IO::File->new($tempfile);
		    $fh or die $tempfile;
		    
		    my $dbpos = $stf->create_pseudoposition(channel=>$channel,position=>$position,camera=>$camera);
		    
		    my $strands=0;
		    
		    while (<$fh>) {
			chomp;
			/${dbwt_pattern}/o or die;	    
			my($fs,$meta)=($1,$3);
			my $q = $4;
			my @fs = split(/ /,$fs,-1);
			my @meta = split(/ /,$meta,-1);
			my $pos = pop(@meta);
			my($x,$y) = @meta[1,2];
			$pos eq $position or die;
			push @meta, $dbpos;

			foreach (@fs) {
			    tr/N/T/;
			}
			
			#hack to deal with a bug in HAE strand output where trailing null (no incorporation) frames are sometimes omitted 
			if (@fs<$max_len) {
			    @lengths>1 or die;
			    my $pad_len = $max_len-scalar(@fs);
			    push @fs, ("-") x $pad_len;
			}
			
			$strands++;
			
			print $ofh join(" ",@fs), "  ", join(" ",@meta), "\n";
		    }
		    close($fh) or die;
		    
		    $total_strands+=$strands;
		}
		
	    }
	    
	    print $ofh_strand_count "$total_strands\n";
	    
	    print $ofh_area "$total_area\n";
	    
	    close($ofh) or die;
	    
	    close($ofh_strand_count) or die;
	    close($ofh_area) or die;
	}
    }
}

if (scalar(keys %corrupt_files)) {
    my @tmp = sort {$a->[0]<=>$a->[0] or $a->[1]<=>$a->[1] or $a->[2]<=>$a->[2] or $a->[3]<=>$a->[3]} map {[split(/\001/,$_)]} keys %corrupt_files;
    foreach my $tmp (@tmp) {
	my($flow_cell,$channel,$position,$camera,$file)=@$tmp;
	warn "ERROR: corrupt strand template file <$file> for flowcell <$flow_cell> channel <$channel> position <$position> camera <$camera> was skipped\n";
    }
}

chdir($start_dir);

print "Done\n";

exit;


sub _integer_intervals {
    my @vals = @_;
    my @intervals;
    my @tmp = shift @vals;
    foreach my $val (@vals) {
	if ($val==$tmp[-1]+1) {
	    push @tmp,$val;
	}
	else {
	    push @intervals,[$tmp[0],$tmp[-1]];
	    @tmp=($val);
	}
    }
    push @intervals,[$tmp[0],$tmp[-1]];
    return @intervals;
}

sub usage
{
    print "Sorry. No help\n";
    exit 1;
}


__END__

