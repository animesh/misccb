#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#    Code base of Animesh Sharma [ sharma.animesh@gmail.com ]

#!/usr/bin/perl -w
#
# $Id: wav2mp3.pl,v 1.5 2000/11/26 19:52:33 bob Exp $
#

=pod

=head1 wav2mp3.pl

=head2 Overview

This script demonstrates use of the SWIG-generated interface to 
J. A. Bezemer's gramofile program, which is available from 
http://panic.et.tudelft.nl/~costar/gramofile/. 

SWIG (Software Wrapper Interface Generator) is written by Dave 
Beazley and is available from www.swig.org. It facilitates the
creation of wrapper functions around C code, to permit this code 
to be called from Perl, Python, Tcl, Guile.

gramofile can be used to convert large wav files generated 
from e.g. vinyl LPs into a number of constituent wav files 
(i.e. the songs on the LP or cassette).

This program allowed me to record many sides of vinyl, and then 
run the conversion to mp3 in batch-mode (almost). Back to the 
future anyone... A scriptable program is better than an interactive
one when the parameters change little.

This code splits the file, and then filters the signal by applying
some of the filters available from gramofile. These split and processed
wav files are post-processed with sox, which adjusts the volume of the
file to the maximum possible withour clipping. The wav files are then
converted to mp3 format by using lame.

sox is available from http://home.sprynet.com/~cbagwell/sox.html
lame is available from www.sulaco.org

=head2 Pragmatism and Heuristics

The raw wav files were recorded utilising the standard curses interface to 
gramofile.

I have a set of XML files which described the information on each of the
records I was processing. I used these files to provide the number of tracks
on each side of vinyl. I found that occasionally the track splitting algorithm
undersplit the wav files. I knew (from the XML files) the number of tracks 
that should be located; so if the number produced by the splitting algorithm
was fewer than the number expected I decremented the inter-track silence factor
and re-computed until I arrived at the expected value.

I made no correction for over-splitting because this happened infrequently. (I
suppose that this could be rectified by increasing the inter-track silence 
factor generally, and then reducing it, as described above). 

Infrequently (e.g. with live albums) it's difficult to accurately split the
wav files into their constituent tracks. In this case the .tracks files created
by the track splitting process were hand edited. This program was then re-run
on the wav file with the flag $use_tracksplit switched off.

=cut

# Ease debugging!

use strict;
use diagnostics;

# CPAN modules

use File::Basename;
use Getopt::Long;
use XML::Simple;
use Data::Dumper;
use IO::File;

# The module we're interested in.

use Gramofile;

# switch this off to use hand-edited .tracks file from Gramofile

my $use_tracksplit=1;

# switch this off if you have no XML files describing the songs on the record

my $use_xml=1;

=pod

=head2 Gramofile::tracksplit_main($wav_file, $make_use_rms, 
       $make_graphs, $blocklen, $global_silence_factor, 
       $local_silence_threshold, $min_silence_blocks, $min_track_blocks, 
       $extra_blocks_start, $extra_blocks_end);

=cut

# default parameters for tracksplitting 

my $silence_blocks = 20; 
my $make_use_rms = 1;
my $make_graphs = 0;
my $blocklen = 4410;
my $global_silence_factor = 150;
my $local_silence_threshold = 5;
my $min_silence_blocks = $silence_blocks; 
my $min_track_blocks = 50;
my $extra_blocks_start = 3;
my $extra_blocks_end = 6;

my @filter_list;
GetOptions (
    'make_use_rms=i' => \$make_use_rms,
    'make_graphs=i' => \$make_graphs,
    'blocklen=i' => \$blocklen,
    'global_silence_factor=i' => \$global_silence_factor,
    'local_silence_threshold=i' => \$local_silence_threshold,
    'min_silence_blocks=i' => \$min_silence_blocks,
    'min_track_blocks=i' => \$min_track_blocks,
    'extra_blocks_start=i' => \$extra_blocks_start,
    'extra_blocks_end=i' => \$extra_blocks_end,
    'use_tracksplit=i' => \$use_tracksplit,
    'use_xml=i' => \$use_xml,
    'filter=s@' => \@filter_list,
);

$silence_blocks=$min_silence_blocks;

=pod

=head2    Gramofile::signproc_main($wav_file, 
          join ('/', $tmp_dir,"out".$file), $filter_num, $filter_ptr);

=cut

# filters available from gramofile

my %filters_id = (
   simple_median_filter  => 1,
   simple_mean_filter    => 2,
   cond_median_filter    => 3,
   double_median_filter  => 4,
   cond_median2_filter   => 5,
   rms_filter            => 6,
   copyonly_filter       => 7,
   experiment_filter     => 8,
);
push @filter_list, "cond_median2_filter" unless (@filter_list);
 
my @filters;
#    print "Passed Filter is $filt\n";
foreach my $filt (@filter_list) {
#    print "Passed Filter is $filt\n";
    push @filters, $filters_id{$filt};
}
my $filter_num = scalar(@filters);
#print "There are $filter_num filters\n";
#foreach my $filtnum (@filters) {
#    print "Filter Number is $filtnum\n";
#}

# Prepare vector of integers to pass information about selected filters
# to the SWIG wrapper, and thence to gramofile

my $filter_ptr = Gramofile::ptrcreate("int",0,$filter_num);
for (my $i=0; $i<$filter_num;$i++) {
    Gramofile::ptrset($filter_ptr, $filters[$i], $i, "int");
}

if ($use_xml) {
   die "wav2mp3 wav_dir mp3_dir xml_dir [tmp_dir]" 
   unless (($#ARGV == 2) or ($#ARGV == 3));
} else {
   die "wav2mp3 wav_dir mp3_dir [tmp_dir]" 
   unless (($#ARGV == 1) or ($#ARGV == 2));
}

my $wav_dir = shift @ARGV;
my $mp3_dir = shift @ARGV;
my $xml_dir = shift @ARGV if ($use_xml);
my $tmp_dir= @ARGV ? shift @ARGV : $wav_dir;

opendir(WAVDIR, $wav_dir) || die "can't opendir $wav_dir: $!";

# all of my input files matched this regexp i.e. they were called 
# somethingnew.wav

foreach my $file (grep { /new\.wav$/ } readdir(WAVDIR)) {
    $min_silence_blocks=$silence_blocks;
    my $xml_count;
    if ($use_xml) {
        my ($xml_name, $side) = ($file =~ /^(.*?)_(\w)_new\.wav/);
        my $xml_file = $xml_dir . "/" . $xml_name . ".xml";
        print "XML Name is $xml_name, SIDE is $side, XML_FILE is $xml_file\n";
        my $xml_ref = XMLin($xml_file, keyattr => "MYSIDE", forcearray => ["TRACK","SIDE"]);
#        print Dumper($xml_ref);
        my $xml_side = $xml_ref->{ALBUM}->{SIDE}->{uc $side}->{TRACK};
        $xml_side = $xml_ref->{SIDE}->{uc $side}->{TRACK} unless ($xml_side);
        print Dumper($xml_side);
        $xml_count = scalar @$xml_side;
        print "XML count is $xml_count\n";
    }
    print "WAV_DIR is $wav_dir, MP3_DIR is $mp3_dir, TMP_DIR is $tmp_dir, FILE : $file\n";
    my $wav_file = join ('/', $wav_dir, $file);
    if ($use_tracksplit) {
        Gramofile::tracksplit_main($wav_file, $make_use_rms, 
        $make_graphs, $blocklen, $global_silence_factor, 
        $local_silence_threshold, $min_silence_blocks, $min_track_blocks, 
        $extra_blocks_start, $extra_blocks_end);
    
        if ($use_xml) {
            my $track_count = get_tracks($wav_file, $min_silence_blocks);
            while ($track_count < $xml_count) {
                $min_silence_blocks--;
                die "MIN_SILENCE_BLOCKS zeroed" unless ($min_silence_blocks);
                Gramofile::tracksplit_main($wav_file, $make_use_rms, 
                $make_graphs, $blocklen, $global_silence_factor, 
                $local_silence_threshold, $min_silence_blocks, $min_track_blocks, 
                $extra_blocks_start, $extra_blocks_end);
                $track_count = get_tracks($wav_file, $min_silence_blocks);
            }
        }
    }
    Gramofile::signproc_main($wav_file, 
    join ('/', $tmp_dir,"out".$file), $filter_num, $filter_ptr);
    opendir(TMPDIR, $tmp_dir) || die "can't opendir $tmp_dir: $!";

# my input files ended with the string new.wav, so the gramofile
# program splits them into whatnew01.wav, whatnew02.wav etc.

    my ($root,$stem) = $file =~ /^(.*)new((\d\d)?\.wav)$/;
    foreach my $split_wav (grep { /^out$root/ } readdir (TMPDIR)) {
        print "split_wav is ",$split_wav,"\n";
        my ($root,$stem) = $split_wav =~ /^out(.*)new((\d\d)?\.wav)$/;
        $stem = "01" . $stem unless ($stem =~ /^\d\d/);
        my $in_file = join ('/', $tmp_dir,$split_wav);
        (my $out_file = $mp3_dir . "/" . $root . $stem) =~ s/wav$/mp3/;
        print "INFILE : $in_file, OUTFILE : $out_file, ROOT : $root, STEM : $stem\n";
        encode_wav($in_file, $out_file);
        unlink $in_file;
    }
    closedir TMPDIR;
}
closedir WAVDIR;
Gramofile::ptrfree($filter_ptr);         # Destroy the pointer

sub encode_wav {
    my $in_file = shift;
    my $out_file = shift;
    print "in_file is $in_file, out_file is $out_file\n";
    my $tmp_file = "/tmp/$$.wav";
    eval { 
        my $soxval = `sox $in_file -e stat -v 2>&1`;
        chomp($soxval);
        print "SOXVAL is $soxval for $in_file\n";
        my @sox_args = ("sox","-v","$soxval","$in_file","$tmp_file");
        system(@sox_args) == 0 or die "system @sox_args failed: $?"; 
        my @lame_args = ("lame","-h","$tmp_file","$out_file");
        system(@lame_args) == 0 or die "system @lame_args failed: $?"; 
    }; warn $@ if $@;
}

sub get_tracks {
    my $wav_file = shift;
    my $min_silence_blocks = shift;
    my $track_file = $wav_file . ".tracks";
    my $track_fh = new IO::File $track_file, "r" or die "Can't open $track_file, $!";
    my @track_data = <$track_fh>;
    my ($num_tracks) = grep {s/^Number_of_tracks=//} @track_data;
    chomp $num_tracks;
    print "NUM tracks is $num_tracks for min_silence_blocks=$min_silence_blocks\n";
    return $num_tracks;
}
