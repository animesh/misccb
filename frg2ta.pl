#!/usr/local/bin/perl

use lib "/local/asmg/lib";

###########################################################################
# Author: Mihai Pop 
#
# Description:
#
# Converts Celera Assembler input to TIGR Assembler input.
#
# Test and Debug Features:
#   Debug level 0 - All errors
#   Debug level 1 - Basic Application progress messages and warnings
#   Debug level 9 - Full Application progress messages (Verbose)
###########################################################################


use strict;
use Fcntl;
use TIGR::Foundation;
use TIGR::AsmLib;
use IO::File;
use File::Basename;

my %deleted;    # list of deleted IDs
my %accname;    # correspondence between accessions and seqnames
my %meandist;   # library sizes
my %stdevdist;

my $MINSEQSIZE = 64;
my $version = "Version 1.2 (Build " . (qw/$Revision: 1.1 $/ )[1] . ")";
my $HELPTEXT = qq~
    frg2ta [options] PREFIX.frg
    converts CA input PREFIX.frg into PREFIX.seq, PREFIX.qual similar to
    inputs for TA

   OPTIONS
     -check          Prints out those seqnames that are shorter than MINSEQ
     -minseq         sets MINSEQ.  Default is 64
     -o prefix       Output prefix
     -filter         filters the .frg file for sequences shorter than MINSEQ
     -noqual         doesnt create .qual file
     -qualidx        creates an index for the qual file [needed by cutAsm]
     -mates          creates a .mates file indicating which sequences are mates
     -nonames        Uses Ids rather than trying to figure out seqnames
     -h              Prints help
     -V              Prints version information
    ~;

my $tf = new TIGR::Foundation;
if (! defined $tf) {
    print STDERR "the sky is falling, run away!\n";
    exit(1);
}

MAIN:
{
    $tf->setHelpInfo($HELPTEXT);
    $tf->setVersionInfo($version);

    my %opts = ( 'minseq' => \$MINSEQSIZE);
    my $frag_fh;
    my $opref;

    my $err = $tf->TIGR_GetOptions(\%opts,
                                   'check',
                                   'minseq=i',
                                   'o=s',
                                   'filter',
                                   'noqual',
                                   'qualidx',
                                   'mates',
                                   'nonames');

    if ($err == 0){
        $tf->bail("Command line parsing failed.  See -h option");
    }

    if (isReadableFile($ARGV[0])) {
        $frag_fh = new IO::File "$ARGV[0]" or
          $tf->bail("Cannot open \"$ARGV[0]\": $!\n");
    } else {
        $tf->bail("You must specify a .frg file!");
    }

    $ARGV[0] =~ /(\S+)\.frg/;
    if (! defined $1) {
        $tf->bail("\"$ARGV[0]\" not recognized, must have extension .frg");
    }
    if (defined $opts{'o'}) {
        $opref = $opts{'o'};
    } else {
        $opref = $1;
    }

    if (defined $opts{'filter'}) {
        $tf->logLocal("Filtering $ARGV[0] for sequences < $MINSEQSIZE...", 9);
        if ($opref eq $1) {
            $tf->bail("You must choose a different output name with -o\n");
        }
        open(NEWFRG, ">$opref.frg") or
          $tf->bail("Cannot open \"$opref.frg\": $!\n");
    } elsif (! defined $opts{'check'}) {
        open (SEQ, ">$opref.seq") or
          $tf->bail("Cannot open \"$opref.seq\": $!\n");
        if (! defined $opts{'noqual'}){
            open (QUAL, ">$opref.qual") or
              $tf->bail("Cannot open \"$opref.qual\": $!\n");
            if (defined $opts{'qualidx'}) {
                open (QUALIDX, ">$opref.qual.qlx") or
                  $tf->bail("Cannot open \"$opref.qual.qlx\": $!\n");
            }
        }
        if (defined $opts{'mates'}){
            open (MATES, ">$opref.mates") or
              $tf->bail("Cannot open \"$opref.mates\": $!\n");
        }
    } else {
        $tf->logLocal("Checking $ARGV[0] for sequences < $MINSEQSIZE...", 9);
    }

    my $record = "";
    while ($record = getCARecord($frag_fh)) {
        my ($rec, $fields, $recs) = parseCARecord($record);

        if ($rec eq "ADT") {
            processADT($record, \%opts);
        } elsif ($rec eq "MDI") {
            processMDI($record, $fields, \%opts);
        } elsif ($rec eq "FRG") {
            processFRG($record, $fields, \%opts);
        } elsif ($rec eq "LKG") {
            processLKG($record, $fields, \%opts);
        } elsif ($rec eq "DST") {
            processDST($record, $fields, \%opts);
        } else {
            if (defined $opts{'filter'}) {
                print NEWFRG $record;
	    }
            $tf->logLocal("$rec ($$fields{acc}) is not currently parsed " .
                          "and has $#$recs sub-records", 1);
        }

    }

    if (defined $opts{'filter'}) { close(NEWFRG); }

    if (! defined $opts{'check'}) {
        close (SEQ);
        if (! defined $opts{'noqual'}) { close (QUAL); }
        if (defined $opts{'qualidx'}) { close(QUALIDX); }
        if (defined $opts{'mates'}) { close (MATES); }
    }
    $frag_fh->close();

    # The end
    exit(0);
}

######################################################################
# process an ADT record
######################################################################
sub processADT {
    my $record = shift;
    my $rh_opts = shift;

    if (defined $$rh_opts{'filter'}){
        print NEWFRG $record;
    }
}

######################################################################
# process an MDI record
######################################################################
sub processMDI {
    my $record = shift;
    my $fields = shift;
    my $rh_opts = shift;
    $tf->logLocal("Processing MDI record...", 9);

    if (defined $$rh_opts{'filter'}){
        print NEWFRG $record;
    }

    $tf->logLocal("Distance id: $$fields{ref} revised to " .
    "mean: $$fields{mea}, stddev: $$fields{std}, min: $$fields{min}, " .
    "max: $$fields{max}", 1);
}

######################################################################
# process a FRG record
######################################################################
sub processFRG {
    my $record = shift;
    my $fields = shift;
    my $rh_opts = shift;

    my $sequence = $$fields{seq};
    my $qualities = $$fields{qlt};
    my $clear = $$fields{clr};
    my $seqname = $$fields{src};
    my $id = getCAId($$fields{acc});

    $tf->logLocal("Processing FRG record for $id...", 9);

    my @lines = split('\n', $sequence);
    $sequence = join('', @lines);
    @lines = split('\n', $qualities);
    $qualities = join('', @lines);
    @lines = split('\n', $seqname);
    $seqname = $lines[0];
    if ($seqname eq "" || $seqname =~ /^\s*$/) {
        $seqname = "$id";
    }
    if (defined $$rh_opts{'nonames'}) {
        $accname{$id} = $id;
        $seqname = $id;
    } else {
        $accname{$id} = $seqname;
    }
    $clear =~ /(\d+),(\d+)/;

    if (defined $$rh_opts{'filter'}) {
        if ($2 - $1 < $MINSEQSIZE){
            $deleted{$id} = 1;
            $tf->logLocal("Clear range for $seqname (" . ($2 - $1) .
                          ") < $MINSEQSIZE", 1);
            print "Clear range for $seqname (", $2 - $1, ") < $MINSEQSIZE\n";
        } else {
            print NEWFRG $record;
        }
    } elsif ($$rh_opts{'check'}) {
        if ($2 - $1 < $MINSEQSIZE) {
            $tf->logLocal("Clear range for $seqname (" . ($2 - $1) .
                          ") < $MINSEQSIZE", 1);
            print "Clear range for $seqname (", $2 - $1, ") < $MINSEQSIZE\n";
        }
    } else {
        my $cll = $1 + 1;
        my $clr = $2;

        my $quals = sprintf("%02d", ord(substr($qualities, 0, 1)) - ord('0'));
        for (my $c = 1; $c < length($qualities); $c++){
            $quals .= sprintf(" %02d", ord(substr($qualities, $c, 1)) -
                                       ord('0'));
        }

        my($pos,$pos1,$pos2,$seqname_len);
        printFastaSequence(\*SEQ, "$seqname 0 0 0 $cll $clr", uc($sequence));
        if (! defined $$rh_opts{'noqual'}){
            if (defined $$rh_opts{'qualidx'}) {
                $seqname_len = length($seqname) + 1;
                $pos = tell QUAL;
                $pos1 = $pos + $seqname_len;
                printFastaQual(\*QUAL, "$seqname", $quals);
                $pos = tell QUAL;
                $pos2 = $pos - 1;
                print QUALIDX "$seqname,$pos1,$pos2\n";
            } else {
                printFastaQual(\*QUAL, "$seqname", $quals);
            }
        }
    }
}

######################################################################
# process a LKG record
######################################################################
sub processLKG {
    my $record = shift;
    my $fields = shift;
    my $rh_opts = shift;
    $tf->logLocal("Processing LKG record...", 9);

    if (defined $$rh_opts{'mates'}){
        print MATES "$accname{$$fields{fg1}} $accname{$$fields{fg2}} " .
                    "$meandist{$$fields{dst}} $stdevdist{$$fields{dst}}\n";
    }
    if (defined $$rh_opts{'filter'}) {
        unless (exists $deleted{$$fields{fg1}} or
                exists $deleted{$$fields{fg2}}) {
            print NEWFRG $record;
        }
    }
}

######################################################################
# process a DST record
######################################################################
sub processDST {
    my $record = shift;
    my $fields = shift;
    my $rh_opts = shift;
    $tf->logLocal("Processing DST record...", 9);

    if (defined $$rh_opts{'mates'}){
        $meandist{$$fields{acc}} = $$fields{mea};
        $stdevdist{$$fields{acc}} = $$fields{std};
    }

    if (defined $$rh_opts{'filter'}) {
        print NEWFRG $record;
    }
}
