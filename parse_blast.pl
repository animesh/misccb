#!/usr/bin/perl -w

#---------------------------------------------------------------------------
# PROGRAM : parse_blast.pl
# PURPOSE : To demonstrate parsing features of the Bio::Tools::Blast.pm module.
# AUTHOR  : Steve Chervitz (sac@bioperl.org)
# CREATED : 3 Feb 1998
# REVISION: $Id: parse_blast.pl,v 1.9 2002/01/11 08:05:38 sac Exp $
# WEBSITE : http://bio.perl.org/Projects/Blast/
# USAGE   : parse_blast.pl -h
# EXAMPLES: parse_blast.pl -eg
#
# INSTALLATION: 
#    Set the require ".../blast_config.pl" to point to the proper location
#    of the blast_config.pl file. See blast_config.pl for additional steps.
#
# COMMENTS:
#
# Sample BLAST output files can be found in examples/blast/out/ of the 
# distribution. This script can process Blast report files specified 
# on the command line or supplied via a STDIN stream.
# 
# This demo script does not exercise all of the functionality of the Blast 
# object. See parse_blast2.pl and parse_positions.pl script for some other
# manipulations and the documentation in the Bio::Tools::Blast.pm, 
# accessible from the above website or by running Blast.pm through pod2html.
#
# TODO:
#  * Create an example that shows how to parse with HTML-formatted
#    reports. The new Blast.pm module no longer parses such reports
#    directly.
#
# MODIFIED:
#  sac, 11 Mar 1999: Merged parse_stream.pl with parse.pl to create
#                    parse_blast.pl. Replaces parse_stream.pl and parse.pl.
#  sac,  4 Sep 1998: Added example of using -filt_func option.
#  sac, 16 Jun 1998: Added installation comment, require statement comments.
#                    Minor alteration of seq_inds() calls.
#  sac, 15 Jul 1998: Segregated code into parse2.pl which was formerly in 
#                    parse.pl but commented out.
#---------------------------------------------------------------------------

# Using blast_config.pl in the examples/blast distribution directory:
require "blast_config.pl"; 
# Proper path to blast_config.pl after you install it in your system:
#require "/share/www-data/html/perlOOP/bioperl/bin/blast/blast_config.pl";

# Using vars from blast_config to prevent warning messages under -w.
use vars qw($ID $VERSION $DESC $MONITOR %blastParam @objects
	    $opt_in $opt_table $opt_compress $opt_filt_func);

$ID      = 'parse_blast.pl';
$VERSION = 0.05;
$DESC    = "Demonstrates parsing Blast reports using Bio::Tools::Blast.pm";

@errs = ();
#$opt_filt_func =
#    sub { $hit=shift;
#	   $hit->frac_aligned_hit >= 0.8; };
#

#-----------------
sub parse_usage {
#-----------------
    &blast_usage;
    &blast_parse_params;
    &blast_general_params;
}

#------------
sub examples {
#------------
# THESE NEED TO BE UPDATED TO INCLUDE MORE STREAM PARSING EXAMPLES
<<"QQ_EG_QQ";
(Run these in the examples/blast/ directory of the distribution.)

  ./$ID out/blastx.2 
  ./$ID out/blastp.2.gz -signif 1e-15 -table 1
  ./$ID out/blastp.2.gz -signif 1e-15 -table 1 -exponent -desc
  ./$ID out/blastp.2.gz -signif 1e-15 -table 2
  ./$ID out/blastp.2.wu -check_all -filt_func '\$hit->gaps == 0' -table 2
  ./$ID out/blastp.205.gz -signif 1e-1 -nostats
  ./$ID out/blastp.2.gz -noaligns -signif 1e-5
  ./$ID -signif 1e-5 -table 1 < out/tblastn.2 > parsed.out
  ./$ID out/blastx.2.email.gz -table 1 -signif 1e-4  
  ./$ID out/blastn.2* -table 1 -best -nostats > parsed.out2
  ./$ID out/tblastn.206.out.gz -table 2 -signif 0.1 
  ./$ID out/blastp.1.gz   # should issue some warnings.

STREAM PARSING:

  gzip -cd out/blastp.2* | ./$ID -signif 1e-5 -table 2  > blast.table2
  cat ./out/blastx* | ./$ID -table 1 > blast.table1
  print_blasts.pl ./out | ./$ID -best -noshare
    
  The '-noshare' argument is necessary because the out/ directory
  contains a mixed bag of Blast reports (version 1, 2, blastp, tblastn,
  gapped, ungapped, etc.). Most of the time, -noshare is unnecessary
  since all reports have the same program, version, gapping, etc.

  The "print_blasts.pl dir" syntax or the parse_multi.pl script are 
  recommended when working with large numbers of Blast reports (thousands).
  The Blasts reports located in "dir" can be compressed or not. 

  WARNING:
  Parsing large numbers of Blast reports can lead to serious
  memory usage problems. See documentation in parse_multi.pl and
  Blast.pm for more information (including a workaround).

QQ_EG_QQ
}

##### MAIN #####

&init_blast(\&parse_usage);

if(!@ARGV and $opt_in) {  push @ARGV, $opt_in; }

&set_blast_params();


my ($blast_obj);

if(@ARGV) {
    # Building object(s) from files specified on command line.
    # Each file should contain one report.
    # Note that we don't really need to capture the $blast_object 
    # created by create_blast() since we can always access it via
    # the global $blastObj defined in blast_config.pl.
    # However, doing so makes things more obvious.
    $MONITOR && print STDERR "\nParsing Blast report file(s).\n";
    $count = 0;
    while($_ = shift) {
	# Load the file into the Blast parameters.
	next unless -f and -s;
	$blastParam{-file} = $_;

	eval { 
	    # Create the Blast object with the specified parameters.
	    # Using functions provided by blast_config.pl
	    # which also supplies $blastObj.
	    $blast_obj = &create_blast;  
	    $opt_table ? &print_table($blast_obj) : &show_results($blast_obj);

	    $opt_compress && $blast_obj->compress_file; 
	    $blast_obj->destroy();  # important when crunching lots of reports.
	    $count++;
	};
	if($@) {
	    my $er = "\nFILE: $blastParam{-file}\n$@\n";
	    push @errs, $er;
	}
	print STDERR ".", $count % 50 ? '' : "\n";
    }
} else {
    # @ARGV is empty. Build Blast objects from STDIN stream. 
    # May contain one or more reports.
    print STDERR "\nParsing Blast stream from STDIN.\n";
    if($opt_table) {
      # Process each Blast as you go.
      $blastParam{-exec_func} = \&print_table;
      # Alternatively, try this:
      #? $blastParam{-exec_func} = \&display_hit_info();
    } else {
      # Save all the Blast objects.
      $blastParam{-save_array} = \@objects;  
    }

    eval { &parse_stream;  };

    if($@) {
      die "\n*** TROUBLE:\n$@\n";
      
    } elsif(@objects) {
      &display;
    }
}

if(@errs) {
    printf STDERR "\n*** %d Blast reports produced fatal errors:\n", scalar(@errs);
    foreach(@errs) { print STDERR $_; }
}

&wrap_up_blast;



