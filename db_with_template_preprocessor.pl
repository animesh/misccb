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
# FILE:          db_with_template_preprocessor.pl
# DESCRIPTION:   
# AUTHORS:       Steven Roels
# ================================================================================

# Release Notes:
#  >> 2007-06-20: Created [Steven Roels]


# Note: the variable "position" refers to the imaging position value as reported in db_with_template format files.
# This value is actually an integer that uniquely identifies an image stack for a flow cell.  That is, it is based on channel, true position, and camera.

# Given that these position values are not unique across flow cells, this script should be used to process sets of files for different flow cells separately.

# Known bug:
#
#   Position with consesus frame strings like:          TXCUAGCUAGC--.       (frames that have no incorporations preceding one or more missing terminal frames)
#                            are treated like:          TXCUAGCUAGC...
#
#   This is largely cosmetic given that the positions will be "recovered" and written correctly in output template files.  But the logs will suggest more
#   "missing" data than is the case.



use strict;

use IO::File;
use Data::Dumper;
use Getopt::Long;
use Helicos::MetaData::Classes;
use Helicos::Util::StrandTemplateFile;

$|=1;

warn "PROGRAM=$0\n";
warn "COMMAND=$0 @ARGV\n";

# Template file versions:
#     1:  strand count value always "1"
#     2:  strand count column (0|1); 0 = failed orphan object filter, 1 = passed orphan object filter
#     3:  strand count column indicates minimum orphan object filter passed

my $help;
my $verbose=0;
my $debug=0;

my $image_width = 1392;
my $image_height = 1040;

my $output_file;                                      # output file
my $log_file = "db_with_template_preprocessor.log";   # log file
my $flow_order_string;
my $template_file_version=3;
my $exclude_failed_orphan_filter_strands;             # only relevant to template version 2 (exclude "O" strands)
my $max_strand_orphan_filter;                         # only relevant to template version 3 (exclude strands with quality value > max_strand_orphan_filter)
my $flow_order_string_map;                            # used to replace incorrect frame string characters
my $exclude_out_of_bounds_strands = 0;                # exclude strands with negative coordinates or x/y coordinates outside the width/height of an image
                                                      # (may be needed to address expectations of other code)

my $replace=0;                                        # 0: output file wil be named <original>.<suffix>;  1: original will be renamed <original>.<suffix> and new file <original>
my $suffix;                                           # suffix to use in naming output files (or input file backups if repolace is set to 1)
my $repair=1;                                         # if set to 0, simply excludes strands not consistent with the flow order string rather than trying to "repair" them
my $filter_by_roi=0;
my $roi_config_file;
my $clipping_file;
my $number_of_cameras;
my $min_frame;                                        # if set, trim and ignore frames before frame specified
my $max_frame;                                        # if set, trim and ignore frames beyond frame specified
my %object_rois;
my %strand_rois;
my $clipping_coordinate_reference="template_object_roi";
my $strand_coordinate_reference="clipping_roi";         # clipping_roi|template_object_roi
my $flow_cell;
my $positions_per_channel_string;
my $number_of_channels=25;
my $positions_per_channel;

my $dbwt_pattern = '^([ACGTUXR\-]( [ACGTUXR\-])*)  ((\d+) \S.*\S)$';


#TODO: eliptical and rectangular ROI filtering
#TODO: map characters
#TODO: table of counts of strands filtered at each step (need positions_per_channel)

my %valid_template_versions=map {$_=>1} (1..3);
my @allowed_values = qw(A C G U T R X -);
my %allowed_values=map {$_=>1} @allowed_values;

my %OPTIONS = ( 'verbose=i' => \$verbose,
		'debug=i' => \$debug,
		'output_file=s'  => \$output_file,
		'log_file=s'     => \$log_file,
		'flow_order_string=s' => \$flow_order_string,
		'template_file_version=s' => \$template_file_version,
		'exclude_failed_orphan_filter_strands=i' => \$exclude_failed_orphan_filter_strands,
		'max_strand_orphan_filter=i' => \$max_strand_orphan_filter,
		'exclude_out_of_bounds_strands=i' => \$exclude_out_of_bounds_strands,
		'flow_order_string_map=s' => \$flow_order_string_map,
		'replace=i' => \$replace,
		'suffix=s' => \$suffix,
		'roi_config_file=s' => \$roi_config_file,
		'clipping_roi_file=s' => \$clipping_file,
		'min_frame=i'   => \$min_frame,
		'max_frame=i'   => \$max_frame,
		'strand_coordinate_reference=s' => \$strand_coordinate_reference,
		'flow_cell=i'  => \$flow_cell,
		'positions_per_channel=s' => \$positions_per_channel_string,
		'number_of_channels=i' => \$number_of_channels,
		'help'          => \$help );

unless (&GetOptions(%OPTIONS)) {
    die "FATAL: invalid options or option values\n\n",&_usage();
}

if ($help) {
    print &usage();
    exit;
}

my(@files)=@ARGV;

#verify parameters
{
    $debug=~/^(0|1)$/ or die;
    $verbose=~/^(0|1)$/ or die;
    defined($flow_order_string) or die;
    exists($valid_template_versions{$template_file_version}) or die;
    if (defined($max_strand_orphan_filter)) {
	$template_file_version==3 or die;
	$max_strand_orphan_filter=~/^(0|1|2)$/ or die;
    }
    if ($template_file_version==2 and ! defined($exclude_failed_orphan_filter_strands)) {$exclude_failed_orphan_filter_strands=1;}
    if (defined($exclude_failed_orphan_filter_strands)) {
	$template_file_version==2 or die;
	$exclude_failed_orphan_filter_strands=~/^(0|1)$/ or die;
    }
    
    if (defined($output_file) and defined($suffix)) {die;}
    if (defined($flow_order_string_map)) {
	length($flow_order_string_map)==length($flow_order_string) or die;	
    }
    if (defined($suffix) and defined($output_file)) {die;}
    if ($replace and ! defined($suffix)) {die;}
    if ($replace and defined($output_file)) {die;}
    if (defined($suffix) and $suffix!~/^[A-Za-z\.\-\_]+$/) {die;}
    if (defined($min_frame)) {
	$min_frame=~/^\d+$/ or die;
	$min_frame<length($flow_order_string) or die;
    }
    if (defined($max_frame)) {
	$max_frame=~/^\d+$/ or die;
	$max_frame<length($flow_order_string) or die;
    }
    if (defined($min_frame) and defined($max_frame)) {
	$min_frame<=$max_frame or die;
    }
    if (defined($flow_cell)) {
	$flow_cell=~/^\d+$/ or die;
    }
    if (defined($clipping_file)) {
	(defined($flow_cell) and defined($strand_coordinate_reference) and defined($positions_per_channel_string) and defined($roi_config_file)) or die;
    }
    $strand_coordinate_reference=~/^(clipping_roi|template_object_roi)$/ or die;
}

if (defined($positions_per_channel_string))
{
    my @ppc = split(/,/,$positions_per_channel_string,-1);
    @ppc or die;
    #warn "PPC:",join(" /// ",@ppc),"\n";
    grep {! /^\d+$/} @ppc and die;
    if (@ppc>1) {
	@ppc == $number_of_channels or die;
	$positions_per_channel=\@ppc;
    }
    else {$positions_per_channel=$ppc[0];}
}

my @flow_order = split("",$flow_order_string);
grep {! exists($allowed_values{$_});} @flow_order and die;
my $flow_order_string_length = length($flow_order_string);

my %flow_order_map;
my @flow_order_map_indeces;

if (defined($flow_order_string_map)) {
    my @flow_order_map = split("",$flow_order_string_map);
    grep {! exists($allowed_values{$_});} @flow_order_map and die;
    for (my $i=0; $i<@flow_order_map;$i++) {
	next if $flow_order_map[$i] eq "-";
	$flow_order[$i] =~ /[CUAG]/ and die;
	$flow_order_map[$i] eq $flow_order[$i] and die;
	$flow_order_map{$i}=$flow_order_map[$i];
    }
    @flow_order_map_indeces=sort {$a<=>$b} keys %flow_order_map;
}

if (defined($roi_config_file)) {
    my $script;
    {
	my $fh=IO::File->new($roi_config_file);
	local $/=undef;
	$script=<$fh>;
    }
    my %imaging_config;
    eval($script);  #sets %imaging_config
    if ($@) {
	die("$0: FATAL: eval failed\n",$@);
    }

    $number_of_cameras = $imaging_config{cameras};
    my @cpos = @{$imaging_config{camera_position}};
    my @object_rois = @{$imaging_config{object_rois}};
    my @strand_rois = @{$imaging_config{strand_rois}};
    my @fov = @{$imaging_config{fov}};
    grep {$_ != 0} @fov[0,1] and die;
    $number_of_cameras==@cpos or die;
    my($width,$height)=($fov[2]+1,$fov[3]+1);
    my $fov = Helicos::MetaData::FieldOfView->new(width=>$width,height=>$height);
    foreach my $camera_id (0..${number_of_cameras}-1) {
	my $pos = $cpos[$camera_id];
	my @object_roi = @{$object_rois[$camera_id]};
	my @strand_roi = @{$strand_rois[$camera_id]};
	my $camera= Helicos::MetaData::Camera->new(id=>$camera_id,position=>$pos,field_of_view=>$fov);
	my $strand_roi = Helicos::MetaData::ImageRegionOfInterest::Rectangular->new(x0=>$strand_roi[0],y0=>$strand_roi[1],x1=>$strand_roi[2],y1=>$strand_roi[3],camera=>$camera);
	my $object_roi = Helicos::MetaData::ImageRegionOfInterest::Rectangular->new(x0=>$object_roi[0],y0=>$object_roi[1],x1=>$object_roi[2],y1=>$object_roi[3],camera=>$camera);
	$strand_rois{$camera_id}=$strand_roi;
	$object_rois{$camera_id}=$object_roi;
	unless ( ($strand_roi->x0 <= $object_roi->x0 and $strand_roi->x1 >= $object_roi->x1)
		 and
		 ($strand_roi->y0 <= $object_roi->y0 and $strand_roi->y1 >= $object_roi->y1) ) {
	    $filter_by_roi=1;
	}
    }
}

my $stf;
if (defined($clipping_file)) {
    $stf=Helicos::Util::StrandTemplateFile->new(positions_per_channel=>$positions_per_channel,number_of_channels=>$number_of_channels,number_of_cameras=>$number_of_cameras);
}

my %filter;

if ($filter_by_roi and defined($clipping_file)) {
   
    my $fh = IO::File->new($clipping_file);
    $fh or die;

    #"FlowCell","Channel","Position","Camera","x0","y0","dx","dy","Area","init_x0","init_y0","init_dx","init_dy","init_Area"

    while (<$fh>) {
	chomp;
	my @f = split(/\t/,$_,-1);
	(@f==9 or @f==14) or die;
	(@f==14) or die;
	my($fc,$channel,$position,$camera,$x0,$y0,$dx,$dy,$area,$init_x0,$init_y0) = @f[0..10];
	next unless $fc eq $flow_cell;

	#define filter by adjusting clipping coordinates to strand coordinate system
	my($x_min,$y_min,$x_max,$y_max)=($x0,$y0,$x0+$dx-1,$y0+$dy-1);
	if ($strand_coordinate_reference eq 'clipping_roi') {
	    #make min/max x and y relative to initial clipping region
	    map {$_=$_-$init_x0;} ($x_min,$x_max);
	    map {$_=$_-$init_y0;} ($y_min,$y_max);
	}
	elsif ($strand_coordinate_reference eq 'template_object_roi') {
	    #dont need to do anything (clipping and strand coordinates are both template-relative
	}
	else {die;}
	$filter{$channel}->{$position}->{$camera}=[$x_min,$x_max,$y_min,$y_max];
	#$clipping{$channel}->{$position}->{$camera}=[$x0,$y0,$x0+$dx-1,$dy,$area];
    }
    close($fh) or die;
}

my $logfh=IO::File->new(">$log_file");
$logfh or die;
$logfh->autoflush(1);


# start processing

foreach my $original_file (@files) {
    
    my $infile = $original_file;
    my $meta_count;

    my %observed_positions;                   #all positions in file (clean + failed + recovered_usable + recovered_unusabele)
    my %failed_positions;                     #rejected positions
    my %repaired_positions;
    my %recovered_usable_positions;           #short frame strings; could be mapped to expected flow order; no missing essential information
    my %recovered_but_unusable_positions;     #short frame strings; could be mapped to expected flow order; missing essential information

    my %valid_positions;                      # clean + recovered_usable
  
    my %suspect_position_to_frame_string;     # map files for suspect positions (frame string here is as reconciled (if possible) with reported flow_order_string)
    my %frame_string_to_suspect_position;     #

    my %frame_counts_by_position;
    my %frame_values_by_position;
    
    my %flow_order_string_to_corrupt_frame_string_for_position;

    
    # initial scan of the file - record frame string information per position and mark as failed those positions whose strands have non-uniform length and/or
    # concompatible frame string values
    
    {	
	my $fh=IO::File->new($infile);
	$fh or die;
	
	while (<$fh>)
	{
	    chomp;
	    
	    my $strand_string;
	    my $meta_string;
	    my $filter_value;
	    
	    unless (/${dbwt_pattern}/o) {
		my $txt;
		if (eof($fh) and !/\n/) {
		    $txt = "strand file <$infile> appears to be truncated - last line is incomplete";
		}
		else {
		    $txt = "strand file <$infile> appears to be corrupt - failure to match general template format at line <$_>";
		}
		die "FATAL: $txt\n";
	    }
	    
	    $strand_string=$1;
	    $meta_string=$3;
	    $filter_value=$4;
	    
	    if ($template_file_version==1 and $filter_value!=0) { die;}
	    
	    if ($template_file_version==2 and $filter_value>1) { die; }
	    
	    my @f=split(/ /,$strand_string,-1);
	    my @m=split(/ /,$meta_string,-1);
	    
	    if (grep {!exists($allowed_values{$_});} @f) { die("FATAL: invalid value in frame string <$strand_string> in file <$infile>\n"); }
	    
	    my $frame_count=scalar(@f);
	    
	    unless (defined($meta_count)) {
		$meta_count=scalar(@m);
		($meta_count=~/^(4|5|6)$/) or die;
	    }
	    
	    $meta_count==scalar(@m) or die("FATAL: inconsistent number of metadata fields in file <$infile>\n");
	    
	    my($x_pos,$y_pos,$position) = @m[1,2,-1];
	    
	    $position=~/^\d+$/ or die("FATAL: bad position <$position> in meta string $meta_string in file <$infile>\n");
	    
	    next if exists($failed_positions{$position});
	    
	    unless (exists($observed_positions{$position})) {
		$observed_positions{$position}++;
		$frame_counts_by_position{$position} = $frame_count;
		$frame_values_by_position{$position}=[];
		map {$frame_values_by_position{$position}->[$_]="-";} (0..$frame_count-1);
	    }
	    
	    if ($frame_counts_by_position{$position} != $frame_count) {
		$failed_positions{$position}='non-uniform_length';
		next;
	    }
	    
	    for (my $i=0;$i<@f;$i++)
	    {
		my $char=$f[$i];
		my $seen=$frame_values_by_position{$position}->[$i];
		next if ($char eq '-' or $char eq $seen);
		if ($seen eq '-') {$frame_values_by_position{$position}->[$i]=$char; next;}
		#inconsistent
		$failed_positions{$position}='inconsistent_frame_strings';
		next;
	    }
	}
	
	close($fh) or die;
	
	foreach my $position (keys %failed_positions) {
	    delete($frame_counts_by_position{$position});
	    delete($frame_values_by_position{$position});
	}
    }
    
    # At this point, remaining (not failed) positions are those with strands that have frames strings that are consistent with those of other strands for the same position.
    # Now validate observered frame strings (for internally consistent positions) against reported flow order string.
    
    {
	my @positions = sort {$a<=>$b} grep {! exists($failed_positions{$_})} keys %observed_positions;
	
	foreach my $position (@positions) {
	    
	    my @fstr=@{$frame_values_by_position{$position}};
	    
	    my $fos=join("",@fstr);
	    
	    if ($fos eq $flow_order_string){
		$valid_positions{$position}++;
	    }
	    else {
		if (length($fos)==$flow_order_string_length) {
		    #should be just one or more frames where no objects were observed
		    my $match=1;
		    for (my $i=0;$i<@flow_order;$i++) {
			unless ($fstr[$i] eq '-' or $fstr[$i] eq $flow_order[$i]) {
			    $match=0;
			    last;
			}
		    }
		    if ($match) {
			$valid_positions{$position}++;
		    }
		    else {
			warn "ERROR: file <$infile>: frame order string for position <$position> does not match expected:\n  O: $fos\n  E: $flow_order_string\n";
			$failed_positions{$position}='flow_order_string_mismatch';
		    }
		}
		elsif (length($fos)>$flow_order_string_length) {
		    # extra frames - possibly processing of muliple images for the same "frame"
		    warn "ERROR: file <$infile>: frame order string for position <$position> is longer than expected:\n  O: $fos\n  E: $flow_order_string\n";
		    $failed_positions{$position}='long';
		}
		else {
		    #missing data
		    #attempt to identify which information is missing
		    
		    my $flow_order_to_frame_string = &get_flow_order_to_consensus_frame_string_map(\@flow_order,\@fstr);
		    
		    if (defined($flow_order_to_frame_string)) {
			$repaired_positions{$position}++;
			$flow_order_string_to_corrupt_frame_string_for_position{$position}=$flow_order_to_frame_string;
		    }
		    else {
			warn "ERROR: file <$infile>: frame string for position <$position> is shorter than expected and is not reconcilable:\n  O: $fos\n  E: $flow_order_string\n";
			$failed_positions{$position}='short_non-recoverable';
		    }
		}
	    }
	    
	    if (exists($failed_positions{$position})) {
		$suspect_position_to_frame_string{$position}=$fos;
		$frame_string_to_suspect_position{$fos}->{$position}++;
	    }
	}
    }
    
    # now go through the "repaired" positions (if any) and classify them as usable or not based on whether missing information corresponds to any nuc challenge frames
    # that are followed by nuc challenge frames for which incorporations were observed
    
    # in theory, we could recover individual strands, but for now, just pass or fail all strands for each position
    
    {
	my @positions = sort {$a<=>$b} keys %repaired_positions;
	foreach my $position (@positions) {
	    my $failed=0;
	    my $term=0;
	    my @psuedo_frame_string;
	    my @map = @{$flow_order_string_to_corrupt_frame_string_for_position{$position}};
	    foreach my $idx (0..$#map) {
		my $val;
		if (defined($map[$idx])) {
		    $val=$flow_order[$idx];
		}
		else {
		    my $flow_char = $flow_order[$idx];
		    if ($flow_char=~/^[CUAG]$/) {
			$failed=1;
		    }
		    $val=lc($flow_char);
		}
		push @psuedo_frame_string,$val;
	    }
	    
	    my $pseudo_frame_string = join("",@psuedo_frame_string);
	    
	    if ($pseudo_frame_string=~/^[CUAGTRX\-trx]*[cuagtrx]+$/) {
		#assume run terminated prematurely
		$term=1;
		$failed=0;
	    }
	    
	    $pseudo_frame_string=~tr/a-z/./;

	    $suspect_position_to_frame_string{$position}=$pseudo_frame_string;
	    $frame_string_to_suspect_position{$pseudo_frame_string}->{$position}++;

	    if ($failed) {
		$recovered_but_unusable_positions{$position}='recovered_but_not_usable;missing_required_information';
		#print $logfh sprintf("%-54s  %s\n","Corrupt (non-recoverable) position <$position>:",$pseudo_frame_string);
       	    }
	    elsif ($term) {
		$recovered_usable_positions{$position}='recovered;presumed_run_termination';
		#print $logfh sprintf("%-54s  %s\n","Corrupt (recoverable; terminated) position <$position>:",$pseudo_frame_string);
	    }
	    else {
		$recovered_usable_positions{$position}='recovered';
		#print $logfh sprintf("%-54s  %s\n","Corrupt (recoverable) position <$position>:",$pseudo_frame_string);
	    }
	}
    }

    # log issues encountered
    #    foreach failed - report failed (and consensus flow order if defined)
    #    foreach recovered - report pseudo frame string

    {
	print $logfh "File: $infile\n";

	my $num_positions=scalar(keys %observed_positions);
	my $num_rejected_positions=scalar(keys %failed_positions) + scalar(keys %recovered_but_unusable_positions);
	my $num_corrupt_but_usable_positions=scalar(keys %recovered_usable_positions);

	print $logfh "\n";
	print $logfh "  number of template positions: ",$num_positions,"\n";
	print $logfh "  number of rejected template positions: ",$num_rejected_positions,"\n";
	print $logfh "  number of corrupt but usable template positions: ",$num_corrupt_but_usable_positions,"\n";
	print $logfh "\n";

	print $logfh "Failed or suspect positions:\n";
	print $logfh join("\t",'position','status','annotation','consensus_frame_string'),"\n";

	foreach my $position (sort {$a<=>$b} ((keys %failed_positions),(keys %recovered_usable_positions),(keys %recovered_but_unusable_positions))) {
	    my $fs = exists($suspect_position_to_frame_string{$position}) ? $suspect_position_to_frame_string{$position} : '<non-uniform>';
	    my $status;
	    my $annotation;
	    if (exists($failed_positions{$position})) {$annotation=$failed_positions{$position};}
	    elsif (exists($recovered_but_unusable_positions{$position})) {$annotation=$recovered_but_unusable_positions{$position};}
	    elsif (exists($recovered_usable_positions{$position})) {$annotation=$recovered_usable_positions{$position};}
	    else {die;}

	    $status = exists($recovered_usable_positions{$position}) ? 'usable' : 'rejected';
	    
	    print $logfh join("\t",$position,$status,$annotation,$fs),"\n";
	}
	print $logfh "\n";

	
	print $logfh "Suspect frame strings:\n";
	print $logfh join("\t",'consensus_frame_string','number_of_positions','positions'),"\n";
	
	{
	    my %frames_strings;
	    my @tmp = keys %suspect_position_to_frame_string;
	    foreach my $position (@tmp) {
		my $fs = $suspect_position_to_frame_string{$position};
		$frames_strings{$fs}->{$position}++;		
	    }
	    
	    @tmp = sort {$b->[2]<=>$a->[2] or $a->[1]->[0]<=>$b->[1]->[0]} map {[$_,[sort {$a<=>$b} keys %{$frames_strings{$_}}],scalar(keys %{$frames_strings{$_}})]} keys %frames_strings;
	    
	    foreach my $ref (@tmp) {
		my($fs,$posref,$numpos)=@$ref;
		my @pos=@$posref;
		my $block_str = &get_position_block_string(@pos);
		print $logfh join("\t",$fs,$numpos,$block_str),"\n";
	    }
	}
	print $logfh "\n";
    }

    # defined "valid" set of positions (clean + usable recovered positions)

    map {$valid_positions{$_}++} (grep {! exists($failed_positions{$_}) and ! exists($recovered_but_unusable_positions{$_});} (keys %observed_positions));
    
    #set up outfile, backing up the infile as needed
    
    my $outfh;
    
    {	    
	if ($replace) {
	    my $backup_file = join("",$infile,$suffix);
	    system("mv $infile $backup_file") and die;
	    my $output_file=$infile;
	    $infile=$backup_file;
	    $outfh=IO::File->new(">$output_file");
	    $infile=$backup_file;
	}
	else {
	    if (defined($output_file)) {
		$outfh=IO::File->new(">$output_file");
	    }
	    elsif (defined($suffix)) {
		my $output_file=join("",$infile,$suffix);
		$outfh=IO::File->new(">$output_file");
	    }	
	    else {
		$outfh=*STDOUT;
	    }
	}
	$outfh or die;	
	$outfh->autoflush(1);
    }
    
    # now go through file and generate filtered derivative
    
    {
	my $fh=IO::File->new($infile);
	$fh or die;
	
	while (<$fh>) {
	    chomp;
	    
	    my $strand_string;
	    my $meta_string;
	    my $filter_value;
	    
	    m/^(.*\S)  ((\d+) (\S.*))$/g or die;
	    $strand_string=$1;
	    $meta_string=$2;
	    $filter_value=$3;
	    
	    my @f=split(/ /,$strand_string,-1);
	    my @m=split(/ /,$meta_string,-1);
	    
	    my($x_pos,$y_pos,$position) = @m[1,2,-1];
	    
	    next unless (exists($valid_positions{$position}));
	    
	    if ($filter_by_roi) {
		next unless &is_in_roi($x_pos,$y_pos,$position);
	    }
	    
	    if ($exclude_out_of_bounds_strands) {
		next if ($x_pos<0 or $x_pos>$image_width-1);
		next if ($y_pos<0 or $y_pos>$image_height-1);
	    }
	    
	    if ($template_file_version==2 and $exclude_failed_orphan_filter_strands and $filter_value==0) {
		next;
	    }
	    
	    if ($template_file_version==3 and defined($max_strand_orphan_filter) and $max_strand_orphan_filter<$filter_value) {
		next;
	    }
	    
	    if (exists($flow_order_string_to_corrupt_frame_string_for_position{$position})) {
		my @out;
		my @map = @{$flow_order_string_to_corrupt_frame_string_for_position{$position}};
		foreach my $idx (0..$#map) {
		    my $val;
		    if (defined($map[$idx])) {
			$val=$f[$map[$idx]];
		    }
		    else {
			$val="-";
		    }
		    push @out,$val;
		}
		@f=@out;
	    }
	    
	    if (@flow_order_map_indeces) {
		foreach my $i (@flow_order_map_indeces) {
		    if ($f[$i] ne '-') { $f[$i] = $flow_order_map{$i}; }
		}
	    }
	    
	    if (defined($min_frame) or defined($max_frame)) {
		my $low=defined($min_frame) ? $min_frame : 0;
		my $high=defined($max_frame) ? $max_frame : $#f;
		@f=@f[$low..$high];
	    }
	    
	    print $outfh join("  ",join(" ",@f),$meta_string),"\n";	
	}
	
	close($fh) or die;
	close($outfh) or die;
    }
}

exit;


######################################################################################

sub is_in_roi
{
    my($x,$y,$pseudo_position)=@_;
    my $in = 0;

    if (defined($clipping_file)) {
	my($channel,$position,$camera)=$stf->parse_pseudoposition($pseudo_position);
	(exists($filter{$channel}) and exists($filter{$channel}->{$position}) and exists($filter{$channel}->{$position}->{$camera})) or die;
	my $tmp=$filter{$channel}->{$position}->{$camera};	
	if (($x>=$tmp->[0] and $x<=$tmp->[1])
	    and
	    ($y>=$tmp->[2] and $y<=$tmp->[3]))
	{
	    $in=1;
	}
	warn "COMPARE: $x,$y,$pseudo_position /// $channel,$position,$camera ///  X:$tmp->[0]..$tmp->[1] Y:$tmp->[2]..$tmp->[3]  RESULT: $in\n" if rand(1)<0.001;
    }
    else {
	my $camera = ($pseudo_position-1) % $number_of_cameras;
	my $o_roi=$object_rois{$camera};
	my $s_roi=$strand_rois{$camera};
	#map to image coordinates
	my($ix,$iy)=($x+$o_roi->x0,$y+$o_roi->y0);
	
	#check to see if strand roi includes this coordinate
	if ( ($ix>=$s_roi->x0 and $ix<=$s_roi->x1)
	     and
	     ($iy>=$s_roi->y0 and $iy<=$s_roi->y1) ) {
	    $in=1;
	}
    }
    return $in;
}

sub get_position_block_string
{
    my(@positions)=@_;
    @positions or die;
    my @blocks;
    my $block;
    for (my $i=0;$i<@positions;$i++) {
	my $p=$positions[$i];
	unless (defined($block)) {$block=[$p,$p]; next;}
	if ($p==$block->[1]+1) {
	    $block->[1]=$p;
	}
	else {
	    push @blocks,$block;
	    $block=[$p,$p];
	}
    }
    push @blocks,$block;
	
    my $block_str = join(",", map {$_->[1]==$_->[0] ? $_->[0] : join("-",@$_);} @blocks);
    return $block_str;
}

sub get_flow_order_to_consensus_frame_string_map
{
    my($flow_order_ref,$frame_str_ref)=@_;
    
    my @fstr=@$frame_str_ref;
    my @flow_order = @$flow_order_ref;

    my @framestr_to_floworder = (undef) x scalar(@fstr);
    my @floworder_to_framestr = (undef) x scalar(@flow_order);
    
    my @missing_indeces;
    
    # start by assigning the indeces of all known (non-"-") values in the frame string to corresponding indeces in the flow order string
    
    eval {
	for (my $i=0,my $j=0; $i<@fstr; $i++,$j++) {
	    my $char = $fstr[$i];
	    next if ($char eq '-');
	    
	    while ($char ne $flow_order[$j]) {
		$j++;
		die if $j>$#flow_order;        #sanity check
	    }		
	    $char eq $flow_order[$j] or die;         #sanity check
	    
	    $framestr_to_floworder[$i]=$j;
	    $floworder_to_framestr[$j]=$i;
	}
    };
    if ($@) {
	return undef;
    }
    
    # Now examine blocks in the flow order string with no current mapping to frames string (either unknown "-" or missing)
    # Assign columns with unknown "-" values, with a preference for control "X" frames, then red "R" frames, then template "T" frames
    # X frames are the most likely, and this represents the most conservative approach as missing data will then be preferentially assigned to nuc
    # challenge frames
    
    my @unmapped_flow_string_indeces = grep {! defined($floworder_to_framestr[$_])} (0..$#floworder_to_framestr);
    
    @unmapped_flow_string_indeces or die;
    
    my @unmapped_flow_string_index_blocks;
    
    {
	my @tmp = @unmapped_flow_string_indeces;
	my $tmp = shift @tmp;
	my $block = [$tmp,$tmp];
	while (@tmp) {
	    $tmp= shift @tmp;
	    if ($tmp==$block->[1]+1) {
		$block->[1]=$tmp;
		next;
	    }
	    push @unmapped_flow_string_index_blocks,$block;
	    $block = [$tmp,$tmp];
	}
	push @unmapped_flow_string_index_blocks,$block;
    }
    
    if ($debug) {
	my @astr;
	foreach my $i (0..$#floworder_to_framestr) {
	    my $z = defined($floworder_to_framestr[$i]) ? $fstr[$floworder_to_framestr[$i]] : ".";
	    push @astr,$z;
	}
	warn "  ",join("",@flow_order),"\n";
	warn "  ",join("",@astr),"\n";		
	
	warn "  unmappped_flow_string_indeces: ",join(",",@unmapped_flow_string_indeces),"\n";
	warn "  unmappped_flow_string_index blocks: ",(join(",",map {"[".join(",",@$_)."]"} @unmapped_flow_string_index_blocks)),"\n";
    }
    
    foreach my $block (@unmapped_flow_string_index_blocks) {
	
	my($flow_low,$flow_high)=@$block;
	
	if ($debug) {
	    warn "    processing block with flow order string index span [$flow_low..$flow_high]\n";
	    warn "        prev_match: ",$flow_low-1," => ",$floworder_to_framestr[$flow_low-1],"\n";
	    warn "        next_match: ",$flow_high+1," => ",$floworder_to_framestr[$flow_high+1],"\n";
	}
	
	# determine the index positions of characters ("-") in the frame string that correspond to this interval in the flow order string
	
	my @unmapped_frame_str_indeces;
	{
	    my($prev,$next)=(0,$#fstr);
	    if ($flow_high+1<=$#flow_order) {
		#get the next matching frame string index
		$next=$floworder_to_framestr[$flow_high+1];
	    }
	    if ($flow_low-1>=0) {
		#get the previous matching frame string index
		$prev=$floworder_to_framestr[$flow_low-1];
	    }
	    
	    $debug and warn "   prev: $prev  next: $next\n";
	    if ($next-$prev>1) {@unmapped_frame_str_indeces=($prev+1..$next-1);}
	}
	
	$debug and warn "   unmapped_frame_str_indeces: ",join(",",@unmapped_frame_str_indeces),"\n";
	
	my $num_missing_values = ($flow_high-$flow_low+1) - scalar(@unmapped_frame_str_indeces);
	
	#assign as missing (start with nuc challenges, then "T", "R", and finally "X" frames)
	
	my $remaining = $num_missing_values;
	
	my @to_assign = ($flow_low..$flow_high);
	my @nuc_frames;
	my @R_frames;
	my @T_frames;
	my @X_frames;
	
	$debug and warn Dumper(\@unmapped_frame_str_indeces,\@to_assign);
	
	foreach my $idx (@to_assign) {
	    my $flowchar = $flow_order[$idx];
	    if ($flowchar eq 'X') {push @X_frames,$idx;}
	    elsif ($flowchar eq 'R') {push @R_frames,$idx;}
	    elsif ($flowchar eq 'T') {push @T_frames,$idx;}
	    else {push @nuc_frames,$idx;}
	}
	
	while ($remaining) {
	    $remaining--;
	    if (@nuc_frames) { pop @nuc_frames; }
	    elsif (@T_frames) { pop @T_frames; }
	    elsif (@R_frames) { pop @R_frames; }
	    elsif (@X_frames) { pop @X_frames; }
	    else {die;}
	}
	
	@to_assign = sort {$a<=>$b} (@nuc_frames,@R_frames,@T_frames,@X_frames);
	
	@to_assign==@unmapped_frame_str_indeces or die;
	
	for (my $k=0; $k<@to_assign; $k++) {
	    $framestr_to_floworder[$unmapped_frame_str_indeces[$k]]=$to_assign[$k];
	    $floworder_to_framestr[$to_assign[$k]]=$unmapped_frame_str_indeces[$k];
	}
    }
    
    return(\@floworder_to_framestr);
    
}

sub usage
{
    my @lines;
    push @lines, "db_with_template_preprocessor.pl [options] <db_with_template_file>";
    push @lines, "";
    push @lines, " options:";
    push @lines, "      --output_file=<outputfile>                           output file (STDOUT if not specified)";
    push @lines, "      --template_file_version=<version>                    template file version/format [default=2]";
    push @lines, "      --exclude_failed_orphan_filter_strands=<0|1>         [default=1]";
    push @lines, "      --max_strand_orphan_filter=<maxstrandfiltervalue>    maximum strand filter value to include (from StrandCount column) [default=undef]";
    push @lines, "      --flow_order_string=<flow_order_string>              expected experiment flow order string (e.g. : \"TXCUAGCUAGCUAGCUAG\")";
    return join("\n",@lines) . "\n";
}


__END__
	
