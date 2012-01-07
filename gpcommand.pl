#!/usr/bin/perl -w

##############################################################################
#
#  Program:  gp_command.pl
#  Creator:  John Pearson
#  Created:  2006-05-30
#
#  This perl script is supposed to be the first script invoked by the end 
#  user after installing GenePool and copying all the necessary .CEL files 
#  into a working directory. This script will read a config file as
#  outlined in the POD within this script.
#
#  $Id: gpcommand.pl,v 1.1 2006/12/11 22:38:42 smitropa Exp $
#
##############################################################################

use strict;
use warnings;
use IO::File;
use Getopt::Long;
use Data::Dumper;
use POSIX;

###########################################################################
#
# "Cry havoc, and let slip the dogs of war ..."
#

our $LOGFH = undef;
our $VERBOSE = 0;

MAIN: {

    # Setup defaults for important variables.

    my $config_file   = 'genepool.ini';
    my $log_file      = '';
    my $gpextract     = '';
    my $gpanalyze     = '';
    my $annotate      = '';
    my $snpinfo       = '';
    my $sliding       = '';
    my $byrank        = '';
    my $help          = 0;

    # Use GetOptions module to parse commandline options

    my $results = GetOptions (
           'c|config_file=s'      => \$config_file,   # -c
           'l|log_file=s'         => \$log_file,      # -l
           'x|gpextract'          => \$gpextract,     # -x
           'z|gpanalyze'          => \$gpanalyze,     # -z
           'v|verbose+'           => \$VERBOSE,       # -v
           'help|?'               => \$help,          # -?
           );

    die "gpcommand options:\n\n",
        " -c | --config_file  INI-stype configuration file\n",
        " -l | --log_file     name of log file\n",
        " -x | --gpextract    run gpextract\n",
        " -z | --gpanalyze    run gpanalyze\n",
        " -v | --verbose      print progress and diagnostic messages\n\n",
        "For more help see the gpcommand man page.\n" if $help;

    die "A configuration file must be specified\n" unless ($config_file);

    die "Sliding window cannot be done without -i option\n" 
        if ($sliding and ! $snpinfo);

    print "\ngpcommand  [" . localtime() ."]\n",
          "   config_file   $config_file\n",
          "   logfile       $log_file\n",
          "   gpextract     $gpextract\n",
          "   gpanalyze     $gpanalyze\n",
          "   verbose       $VERBOSE\n\n" if ($VERBOSE);

    # Open log file if one was specified

    if ($log_file) {
        $LOGFH = IO::File->new( $log_file, 'a' );
        die "Cannot open log file $log_file for appending: $!" 
            unless (defined $LOGFH);
        logprint( "\n[", localtime().'', "]\n" );  # print timestamp
    }

    # Process the configuration file

    logprint( "Processing configuration file: $config_file\n" );
    my $ini = MyINI->new( $config_file );
    check_ini_file( $ini );
    logprint( 'Processing experiment: ',
              $ini->section('EXPERIMENT')->{'NAME'}, "\n" );

    # Run gpextract

    if ($gpextract) {
        die "WARNING: gpextract binary does not appear to be available\n"
            unless defined is_binary_available( qw( gpextract -h ) );
        do_gpextracts( $ini );
    }

    # Run gpanalyze

    # Hash of analysis variations that will be run
    my %analyses = ( 'Silhouette-Euclidean'    => ['-p','0','-d','0'],
                     'Silhouette-Manhattan'    => ['-p','0','-d','1'],
                     'Silhouette-ModManhattan' => ['-p','0','-d','2'],
                     'Consistency-Unweighted'  => ['-p','1','-c','0'],
                     'Consistency-Weighted'    => ['-p','1','-c','1'] );

    if ($gpanalyze) {
        die "WARNING: gpanalyze binary does not appear to be available\n"
            unless defined is_binary_available( qw( gpanalyze -? ) );
        do_gpanalyzes( $ini, \%analyses );
    }
    
    # Annotate a gpanalyze output file

    if ($annotate) {
        do_annotates( $ini, $annotate, $snpinfo, $sliding, $byrank );
    }
}


sub check_ini_file {
    my $ini = shift;

    die "Config file does not contain an [EXPERIMENT] section.\n"
        unless $ini->section( 'EXPERIMENT' );
    die "Config file must contain at least one [PLATFORMx] section.\n"
        unless $ini->section( 'PLATFORM1' );

    die "Config file [EXPERIMENT] section must contain a NAME= line.\n"
        unless $ini->section( 'EXPERIMENT' )->{'NAME'};

    foreach my $platform ( grep { /PLATFORM/} $ini->sections ) {
        die "Config file [$platform] section must contain a CHIPTYPE= line.\n"
            unless $ini->section( $platform )->{'CHIPTYPE'};
    }
}


sub do_gpextracts {
    my $ini = shift;

    logprint( "Running gpextract commands (-x | --gpextract option)\n" );

    my $experiment = $ini->section('EXPERIMENT')->{'NAME'};

    #my $experiment_filename = $experiment . '_Experiment.txt';
    my $experiment_filename = 'Experiment.txt';
    my $expfh = IO::File->new( $experiment_filename, 'w' );
    die "Cannot open file $experiment_filename for writing: $!" 
        unless (defined $expfh);

    my %valid_chips = ( 'Affy_10K'      => 1,
                        'Affy_100K'     => 1,
                        'Affy_500K'     => 1,
                        'Illumina_300K' => 1,
                        'Illumina_550K' => 1 );

    # At the moment the C code can't ignore comment lines so these
    # lines will have to remain commented until that feature's added
    #
    #print $expfh "# File:         $experiment_filename\n", 
    #             "# Created by:   $0\n", 
    #             '# From config:  ', $ini->filename, "\n", 
    #             '# Created on:   ', localtime().'', "\n", 
    #             "#\n",
    #             "# This file is used by gpanalyze\n\n";

    foreach my $sect ( $ini->sections ) {
        next unless $sect =~ 'PLATFORM';
        # Get the var-val lines for this config file section
        my $rh_var_vals = $ini->section( $sect );

        my @cases    = get_chips( $ini, $sect, 'CASE' );
        my @controls = get_chips( $ini, $sect, 'CONTROL' );
        my $enzyme   = '';

        # Affymetrix and Illumina are invoked with different parameters
        # so we need to take this next switch block and wrap the whole
        # invoking block of code inside it.

        my $vendor = $rh_var_vals->{'VENDOR'};
        if ($vendor =~ /^Affymetrix$/i) {
            $enzyme = $rh_var_vals->{'ENZYME'};
            # Execute gpextract commandlines
            foreach my $case (@cases) {
                my @gpargs = ( 'gpextract',
                               '-c', '0',
                               '-e', $enzyme,
                               '-l', $rh_var_vals->{$case},
                               '-f', $rh_var_vals->{'CDFFILENAME'},
                               '-s', 'Case',
                               '-V' );
                my $case_string = join(' ',@gpargs);
                logprint( "  executing: $case_string\n" );
                system(@gpargs) == 0
                    or die "Running $case_string failed: $?";
            }
            foreach my $control (@controls) {
                my @gpargs = ( 'gpextract',
                               '-c', '0',
                               '-e', $enzyme,
                               '-l', $rh_var_vals->{$control},
                               '-f', $rh_var_vals->{'CDFFILENAME'},
                               '-s', 'Control',
                               '-V' );
                my $control_string = join(' ',@gpargs);
                logprint( "  executing: $control_string\n" );
                system(@gpargs) == 0
                    or die "Running $control_string failed: $?";
            }
        }
        elsif ($vendor =~ /^Illumina$/i) {
            # Execute gpextract commandlines
            foreach my $case (@cases) {
                my @gpargs = ( 'gpextract',
                               '-c', '1',
                               '-i', $rh_var_vals->{$case},
                               '-s', 'Case',
                               '-V' );
                my $case_string = join(' ',@gpargs);
                logprint( "  executing: $case_string\n" );
                system(@gpargs) == 0
                    or die "Running $case_string failed: $?";
            }
            foreach my $control (@controls) {
                my @gpargs = ( 'gpextract',
                               '-c', '1',
                               '-i', $rh_var_vals->{$control},
                               '-s', 'Control',
                               '-V' );
                my $control_string = join(' ',@gpargs);
                logprint( "  executing: $control_string\n" );
                system(@gpargs) == 0
                    or die "Running $control_string failed: $?";
            }
        }
        else {
            # big problem!
            die "Vendor [$vendor] is not valid.\n"
        }

        my $cases_filename    = "${experiment}_Cases${enzyme}Files.txt";
        my $controls_filename = "${experiment}_Controls${enzyme}Files.txt";

        # Print line to Experiment file
        print $expfh join( "\t",
                           $cases_filename,
                           scalar(@cases),
                           $controls_filename,
                           scalar(@controls),
                           $enzyme."SnpNames.txt\n" );

        # Create Cases file
        my $casfh = IO::File->new( $cases_filename, 'w' );
        die "Cannot open file $cases_filename for writing: $!" 
            unless (defined $casfh);
        logprint( "  creating Cases file: $cases_filename\n" );

        # At the moment the C code can't ignore comment lines so these
        # lines will have to remain commented until that feature's added
        #
        #print $casfh "# File:         $cases_filename\n", 
        #             "# Created by:   $0\n", 
        #             '# From config:  ', $ini->filename, "\n", 
        #             '# Created on:   ', localtime().'', "\n", 
        #             "#\n",
        #             "# This file is used by gpanalyze\n\n";

        print $casfh $rh_var_vals->{$_}.".gpb\n" foreach (@cases);
        $casfh->close;

        # Create Controls file
        my $confh = IO::File->new( $controls_filename, 'w' );
        die "Cannot open file $controls_filename for writing: $!" 
            unless (defined $confh);
        logprint( "  creating Controls file: $controls_filename\n" );

        # At the moment the C code can't ignore comment lines so these
        # lines will have to remain commented until that feature's added
        #
        #print $confh "# File:         $controls_filename\n", 
        #             "# Created by:   $0\n", 
        #             '# From config:  ', $ini->filename, "\n", 
        #             '# Created on:   ', localtime().'', "\n", 
        #             "#\n",
        #             "# This file is used by gpanalyze\n\n";

        print $confh $rh_var_vals->{$_}.".gpb\n" foreach (@controls);
        $confh->close;

    }

    $expfh->close;
}


sub do_gpanalyzes {
    my $ini = shift;
    my $rh_analyses = shift;
    
    logprint( "Running gpanalyze commands (-z | --gpanalyze option)\n" );

    my $experiment = $ini->section('EXPERIMENT')->{'NAME'};
    #my $experiment_filename = $experiment . '_Experiment.txt';
    my $experiment_filename = 'Experiment.txt';

    # Execute gpanalyze commandlines
    foreach my $command ( keys %{ $rh_analyses } ) {
        my @gpargs = ( 'gpanalyze',
                       @{$rh_analyses->{$command}},
                       '-i', $experiment,
#                       '-F', $experiment_filename,
                       '-o', analysis_filename($experiment,$command) );
        my $command_string = join(' ',@gpargs);
        logprint( "  executing: $command_string\n" );
        system(@gpargs) == 0
            or die "Running $command_string failed: $?";
    }
}


# Predictable name for analysis output files based on experiment.
sub analysis_filename {
    my $expt = shift;
    my $cmd  = shift;
    return $expt . '_' . $cmd . '_Output.txt';
}


# Output text to the logfile filehandle if we actually opened one
sub logprint {
    if ($LOGFH) {
        print $LOGFH $_ foreach @_;
    }
}


# Try to execute binaries to check if they are accessible
sub is_binary_available {
    my @args = @_;
    my $cmdline = join(' ',@args);
    { 
        no warnings;
        `$cmdline`;
    }
    return undef unless WIFEXITED($?);
    return WEXITSTATUS($?);
}


# For a given platform and type (case/control), retrieve the chip names
sub get_chips {
    my $ini     = shift;
    my $section = shift;
    my $type    = shift;

    die "get_chips() only works on [PLATFORMx] sections"
        unless $section =~ 'PLATFORM';

    # Get the var-val lines for this config file section
    my $rh_var_vals = $ini->section( $section );

    # Modified Schwartzian transforms to get chip names
    my @samples = map { $_->[1] } 
                  sort { $a->[0] <=> $b->[0] }
                  map { my $c = $_; $_ =~ s/$type//; [ $_, $c ] }
                  grep { /$type/ }
                  keys %{ $rh_var_vals };

    return @samples;
}


###########################################################################
#
#  Module:   MyINI
#  Creator:  John V Pearson
#  Created:  2006-06-09
#
#  This internal module implements a basic system for reading INI-style 
#  config files.
#
###########################################################################

package MyINI;

use Data::Dumper;

sub new {
    my $invocant = shift;
    my $infile   = shift;

    my $self = { 'filename'             => $infile,
                 'sections'             => {},
                 'section_order'        => [],
                 'current_section_name' => undef,
               };
    my $class = ref($invocant) || $invocant;
    bless $self, $class;

    # Hash of legal values for lines in the INI-style config file
    my %legal_lines = ( 'EXPERIMENT'  =>    [ 'NAME',
                                              'DESCRIPTION' ],
                        'PLATFORM\d+' =>    [ 'VENDOR',
                                              'CHIPTYPE',
                                              'ENZYME',
                                              'CDFFILENAME',
                                              'CASE\d+',
                                              'CONTROL\d+', ],
                        '#_No_Section_#' => [ ],
                      );
    $self->{'legal_lines'} = \%legal_lines;

    # We always have a #_No_Section_# section to catch any parameter
    # lines that appear before the first section.
    $self->add_section( '#_No_Section_#' );
    $self->current_section_name( '#_No_Section_#' );

    $self->read_config_file;

    return $self;
}


sub filename {
    my $self = shift;
    return $self->{'filename'} = shift if @_;
    return $self->{'filename'};
}


sub current_section_name {
    my $self = shift;
    return $self->{'current_section_name'} = shift if @_;
    return $self->{'current_section_name'};
}


sub current_section {
    my $self = shift;
    return $self->{'sections'}->{ $self->current_section_name };
}


sub sections {
    my $self = shift;
    return @{ $self->{'section_order'} };
}


sub check_section_is_valid {
    my $self    = shift;
    my $section = shift;
 
    foreach my $sect (keys %{$self->{'legal_lines'}}) {
        return $sect if ($section =~ /$sect/);
    }
    return undef;
}


sub add_section {
    my $self = shift;
    my $section = shift;

    die "[$section] does not appear to be a valid section name.\n"
        unless $self->check_section_is_valid( $section );

    die "Section [$section] appears twice in config file.\n"
        if exists $self->{'sections'}->{$section};

    # Add new section to sections hash and order array
    my $new_section = {};
    $self->{'sections'}->{$section} = $new_section;
    push @{ $self->{'section_order'} }, $section;

    # Return reference to new section array;
    return $new_section;
}


sub section {
    my $self = shift;
    my $section = shift;

    # If section exists, return ref to storage hash else return undef
    if (exists $self->{'sections'}->{$section}) {
        return $self->{'sections'}->{$section};
    }
    else {
        return undef;
    }
}


sub read_config_file {
    my $self = shift;

    my $infile = $self->filename;
    my $fh = IO::File->new( $infile, 'r');
    die "Cannot open file configuration file [$infile]: $!\n"
        unless (defined $fh);

    while(my $line = $fh->getline) {
        chomp $line;
        $line =~ s/^\s*//;         # strip leading spaces
        $line =~ s/\s*$//;         # strip trailing spaces
        next unless $line;         # skip blank lines
        next if $line =~ /^[;#]/;  # skip comments

        #print "Processing line - $line\n";

        # Check for new section
        if ($line =~ /^\[(\w+)\]$/) {
            my $section = uc($1);
            $self->add_section( $section );
            $self->current_section_name( $section );
        }
        elsif ($line =~ /^(\w+)\s*=\s*(.*)$/) {
            $self->add_var_val_pair( $1, $2 );
        }
        else {
            die "A line from the config file is not recognized: [$line]\n";
        }
    }
    return 1;
}


sub check_variable_is_valid {
    my $self = shift;
    my $var  = shift;
 
    # Get name of matching section in legal_lines structure
    my $sect = $self->check_section_is_valid( $self->current_section_name );

    my $ra_lines = $self->{'legal_lines'}->{$sect};

    foreach my $valid_var (@{$ra_lines}) {
        #print "checking vars: $var =~ $valid_var\n";
        return $valid_var if ($var =~ /$valid_var/);
    }

    die 'In section [', $self->current_section_name,
        "] of the config file, the line \"$var=...\" is not valid.\n";
}


sub add_var_val_pair {
    my $self = shift;
    my $var  = uc( shift );
    my $val  = shift;
 
    # Check that $var is legal in the current section
    return undef unless $self->check_variable_is_valid( $var );

    $self->current_section->{ $var } = $val;

    return 1;
}
