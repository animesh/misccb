#
# Author: Jarrod Chapmon, Isaac Ho
#
# Copyright 2011 The Regents of the University of California.
# All rights reserved.

# The United States Government has rights in this work pursuant
# to contracts DE-AC03-76SF00098, W-7405-ENG-36 and/or
# W-7405-ENG-48 between the United States Department of Energy
# and the University of California.

# Redistribution and use in source and binary forms are permitted
# provided that: (1) source distributions retain this entire
# copyright notice and comment, and (2) distributions including
# binaries display the following acknowledgement:  "This product
# includes software developed by the University of California,
# JGI-PSF and its contributors" in the documentation or other
# materials provided with the distribution and in all advertising
# materials mentioning features or use of this software.  Neither the
# name of the University nor the names of its contributors may be
# used to endorse or promote products derived from this software
# without specific prior written permission.

# THIS SOFTWARE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE.

#!/usr/bin/env perl
#
# meraculous.pl - drives the meraculous genome assembler
#
# Written by Isaac Ho
# Some functions written by Craig Furman, Sara Ting, Harris Shapiro


=pod
Overall logging and error handling strategy:
----------------------------------------------

As much as possible, errors are logged to a JGI_Log object to ensure consistancy.
Exceptions to this occur when creating these logging objects - in which case an error is 
sent via print_jgi_msg

At startup, a temporary log directory is created in the current directory, since there is a fair amount of
error prone work to be done before the main assembly dir. is known.

Once everything is good to go, the log dir is moved to the main assembly directory, so that anything logged at
startup is still there. 

If the temp, dir exists at startup, its removed since it was probably from a previous failed run.

Errors found at startup are logged, and any die statements are caught via the startup eval block.
Currently, we just exit, which causes the an END block to be run. It just dumps the error log so that 
the user knows what went wrong. See END below.

After startup,i.e during the main stage loop, errors are detected both through eval/die statements and return codes.
Some functions die and others return JGI_FAILURE. (This is temporary, we should go to full exception handling at some point).

Once again if an error is caught, the error log is dumped before exiting (at the end of main, not during the END blocK)

=cut

###################################################################
# Module Inclusions                                               #
###################################################################

#
# Enforce good programming style.
#
use strict;
use threads;
#
# Turn on lots of warnings.
#
use warnings;

#
# Include system modules
#
use POSIX qw (ceil time difftime);
use File::Basename;    # for splitting filnamees
use Getopt::Long;      #for processing command line options.
use Data::Dumper;      # for dumping tables to a file
use Cwd;
use FileHandle;

#
# Use the relevant JGI modules.
# Included by default are the ones that pretty much every script
# will need to use.
#

use JGI_Constants;
use JGI_Utility;
#use JGI_Job_Set;
use JGI_Generic_Config;
#use JGI_Job_Control;
use Log::Log4perl qw(:easy);




###################################################################
# Constant Definitions                                            #
###################################################################

# Insert any global constant definitions in this section.

my $SCRIPT_ID = "meraculous.pl";

# A string to identify this script's version.
my $VERSION_ID = "Version 2.10.8  10/15/2006";

# A string to identify the author of this script.
my $AUTHOR_ID = "Isaac, Serge, Craig, Sara";

my @stages = ();

my $g_restarting = 0;
my $g_resuming = 0;
my $g_stage = "";

# max RAM to reserve for each malign process ( in MB )
my $MALIGN_MEM = 2000;

# number of output files to produce for each mercounter process
my $MERCOUNTER_OUT_FILES = 10;

# should we keep all outputs around, or allow clean up of some intermediates?
my $bKeepFullOutputs = 0;

use constant LOG_PREFIX => 'log/jazz_log';

# the filename in each applicable stage that contains that dir's subdirectory structure
use constant DIR_STRUCTURE_FILENAME => 'directory_structure';

# User config file parameter constants
use constant {
  USER_PARAM_depth               => 'depth',
  USER_PARAM_genome_size         => 'genome_size',
#  USER_PARAM_mismatch_penalty    => 'mismatch_penalty',
#  USER_PARAM_genus               => 'genus',
#  USER_PARAM_species             => 'species',
#  USER_PARAM_strain              => 'strain',
  USER_PARAM_use_cluster         => 'use_cluster',
  USER_PARAM_jgi_fasta           => 'jgi_fasta',
  USER_PARAM_lib                 => 'lib',
  USER_PARAM_fasta               => 'fasta',
  USER_PARAM_email_to            => 'email_to',
#  USER_PARAM_microbial_community => 'microbial_community',

  USER_PARAM_unhash_thresh                 => 'unhash_thresh',
  USER_PARAM_min_trim_length               => 'min_trim_length',
#  USER_PARAM_run_data_cleaning             => 'run_data_cleaning',
#  USER_PARAM_lib_ids_to_ignore_for_pairing => 'lib_ids_to_ignore_for_pairing',
#  USER_PARAM_edgequality                   => 'edgequality',
#  USER_PARAM_great_match                   => 'great_match',
#  USER_PARAM_strictness1                   => 'strictness1',
#  USER_PARAM_strictness2                   => 'strictness2',
#  USER_PARAM_no_bonus                      => 'no_bonus',
#  USER_PARAM_max_bad_cover                 => 'max_bad_cover',
#  USER_PARAM_microbial_community           => 'microbial_community',
#  USER_PARAM_chisel_contigs                => 'chisel_contigs',  

  USER_PARAM_reads_per_brd                 => 'reads_per_brd',
#  USER_PARAM_sun_jazz_version              => 'sun_jazz_version',
  USER_PARAM_release_root                  => 'release_root',
  USER_PARAM_jazz_root                     => 'jazz_root',
  USER_PARAM_use_cluster_graphy            => 'use_cluster_graphy',
  USER_PARAM_use_cluster_new_three         => 'use_cluster_new_three',
  USER_PARAM_use_cluster_gapmalign         => 'use_cluster_gapmalign',
  USER_PARAM_use_cluster_regraphy          => 'use_cluster_regraphy',
  USER_PARAM_use_cluster_mercounter        => 'use_cluster_mercounter',
  USER_PARAM_use_cluster_brd               => 'use_cluster_brd',
  USER_PARAM_use_cluster_malign            => 'use_cluster_malign',
  USER_PARAM_use_cluster_assemble_clusters  => 'use_cluster_assemble_clusters',
#  USER_PARAM_graphy_threads                => 'graphy_threads',
  USER_PARAM_trim_quality_high             => 'trim_quality_high',
  USER_PARAM_trim_quality_low              => 'trim_quality_low',
  USER_PARAM_cluster_queue                 => 'cluster_queue',
  USER_PARAM_use_touchfiles                => 'use_touchfiles',
#  USER_PARAM_save_intermediate_gbis        => 'save_intermediate_gbis',
#  USER_PARAM_allow_unsafe_gapmalign        => 'allow_unsafe_gapmalign',
#  USER_PARAM_write_ace_file                => 'write_ace_file',
#  USER_PARAM_allow_sloppy_consensus        => 'allow_sloppy_consensus',
#  USER_PARAM_allow_complex_gap_closing     => 'allow_complex_gap_closing',
 USER_PARAM_load_removed_reads            => 'load_removed_reads',
  USER_PARAM_create_phd_ball               => 'create_phd_ball',
  USER_PARAM_mercounter_groups             => 'mercounter_groups',
  USER_PARAM_malign_column_mode            => 'malign_column_mode',
  USER_PARAM_ringer_cutoff                 => 'ringer_cutoff',
  USER_PARAM_ringer_cutoff_degree          => 'ringer_cutoff_degree',
  USER_PARAM_meraculous_mer_size           => 'meraculous_mer_size',
  USER_PARAM_meraculous_min_depth_cutoff   => 'meraculous_min_depth_cutoff',
  USER_PARAM_lib_seq                => 'lib_seq',
  USER_PARAM_num_prefix_blocks                => 'num_prefix_blocks',
  USER_PARAM_num_sub_blocks_per_lib      => 'num_sub_blocks_per_lib',
  USER_PARAM_stages_script               => 'stages_script',
  USER_PARAM_min_mercount_to_report           => 'min_mercount_to_report',
  USER_PARAM_min_contig_size_for_scaffold_links   =>  'min_contig_size_for_scaffold_links',
  USER_PARAM_meraculous_num_linking_pairs_cutoff => 'meraculous_num_linking_pairs_cutoff',
  USER_PARAM_cluster_num_procs => 'cluster_num_procs',
  USER_PARAM_cluster_max_retries => 'cluster_max_retries',
  USER_PARAM_local_num_procs => 'local_num_procs',
  USER_PARAM_num_procs_mercount => 'num_procs_mercount',
  USER_PARAM_num_procs_mergraph => 'num_procs_mergraph',
  USER_PARAM_num_procs_ufx => 'num_procs_ufx',
  USER_PARAM_num_procs_contigs => 'num_procs_contigs',
  USER_PARAM_num_procs_blast_ono => 'num_procs_blast_ono',
  
  USER_PARAM_local_max_retries => 'local_max_retries',
  USER_PARAM_blast_thoroughly_for_ono => 'blast_thoroughly_for_ono',
  USER_PARAM_external_contigs_file => 'external_contigs_file',
  USER_PARAM_min_qual_to_keep => 'min_qual_to_keep',
  USER_PARAM_cluster_ram_request => 'cluster_ram_request',
  USER_PARAM_cluster_slots_per_job => 'cluster_slots_per_job'
};

# local config file parameter constants
use constant {
  LOCAL_PARAM_assembly_dir => 'assembly_dir',
  LOCAL_PARAM_perl         => 'perl',
  LOCAL_PARAM_label        => 'label',
  LOCAL_PARAM_library_file => 'library_file',
  LOCAL_PARAM_autocalibrated_min_depth_cutoff => 'autocalibrated_min_depth_cutoff',
	LOCAL_PARAM_local_resume_checkpoint => 'local_resume_checkpoint'
};

use constant {
  PATH_MAKE_CLUSTERS => 'PHRAPATTACK/make_clusters.4.01.linux.64bit',
  PATH_FASTA_INDEX => 'scripts/fasta_index.pl',
  PATH_PA_SETUP => 'PHRAPATTACK/PAsetup.pl',
  PATH_FETCH_FASTA_CLUSTERS => 'PHRAPATTACK/fetchFastaClusters.pl',
  PATH_SCREEN_HDH => 'scripts/screen_hdh.pl',
  PATH_SCREEN_LIST => 'scripts/screen_list.pl',
  PATH_RENAME_READS => 'scripts/renameReads.pl',
  PATH_RINGER => 'PHRAPATTACK/ringer3.pl',
  PATH_MERACULOUS => 'MERACULOUS/meraculous5.pl',
  PATH_FASTA_LENGTHS => 'scripts/fastaLengths.pl',
  PATH_HISTOGRAM => 'scripts/histogram2.pl',
  PATH_N50 => 'scripts/N50.pl',
  PATH_RANDOM_LIST2 => 'scripts/randomList2.pl',
  PATH_NOUT => 'scripts/Nout.pl',
  PATH_BOWTIE2HACKEDBVP => 'scripts/bowtie2HackedBvp.pl',
  PATH_BOWTIE => 'bowtie',
  PATH_BOWTIE_BUILD => 'bowtie-build',
  PATH_BLASTVIEW3 => 'scripts/blastView3.pl',
  PATH_BLASTVIEW3ILLUMINA => 'scripts/blastView3Illumina.pl',
  PATH_BV3SWITCHEROO => 'scripts/bv3Switcheroo.pl',
  PATH_FASTA2SCARF => 'scripts/fasta2Scarf.pl',
  PATH_SCARF2FASTA => 'scripts/scarf2Fasta.pl',
  PATH_FASTALENGTHS => 'scripts/fastaLengths.pl',

  PATH_MERCOUNTERS => 'MERACULOUS/merCounterS',
  PATH_SORT_SMASH => 'MERACULOUS/sortSmash.pl',
  PATH_BLASTMAPANALYZER => 'MERACULOUS/blastMapAnalyzer.pl',
  PATH_INSERT_SIZE_CORRECTION => 'MERACULOUS/insertSizeCorrection2.pl',
  PATH_ONO2 => 'MERACULOUS/oNo2.pl',
  PATH_MERAUDER2 => 'MERACULOUS/merauder2.pl',
  PATH_DIVIDE_IT => 'scripts/divide_it.pl',
  PATH_SCAFFOLD2CONTIG => 'scripts/scaffold2contig.pl',
  PATH_MERGRAPH_GEN => 'MERACULOUS/mergraphGen',
  PATH_FINDDMIN => 'MERACULOUS/findDMin.pl',
  PATH_LOAD_BALANCE_MERCOUNT => 'MERACULOUS/loadBalanceMers.pl',
  PATH_MERBLAST => 'MERACULOUS/merBlast',
  PATH_MERGE_MERBLASTS => 'MERACULOUS/mergeMerBlasts.pl',
  PATH_UNIQUE => 'scripts/unique.pl',
  PATH_SCREEN_GENERIC => 'scripts/screen_generic.pl',
  PATH_UFX_NORMALIZER => 'MERACULOUS/UFXnormalizer.pl',

};


# SGE related constants
#
# platform to run SGE jobs on. Currently only linux
use constant sge_platform => 'linux';

# wait for all commands to finish before returning
use constant sge_wait_for_completion => 1;

#### TEMP! END block here catches any innapropriate exit() calls made from external modules during startup,
#### and dumps the error log
#### They should be die statements.
END
{
  my $error_log = LOG_PREFIX . ".err.log";

  if ( -e $error_log )
  {
    system "cat $error_log";
           #    exit(JGI_FAILURE);
  }
     
}


##########################################################################
#
# Ported over functions from rqc ( originally written by Harris Shapiro )
#
#########################################################################

# runs a single local command synchronously
sub run_jgi_cmd
{
    # Please refer to the above function description for details of
    # what these variables are.
    my ($cmd, $log, $cmd_output_ref) = @_;

    # A string to identify this function.
    my $function_id = "run_jgi_cmd";
    
    
    #
    # The method of running the specified command will depend on
    # whether the user wants the command's output back.
    #
    if ($#_ == 2)
    {
      
    #
    # If the function was called with 3 arguments, one needs
    # to return the output of the command.
    #
    $$cmd_output_ref = `$cmd`;
	
    #
    # If the command failed, generate an error.
    #
    if ($?)
    {
	    
        if (defined($log))
        {
		
        $log->error("Command failed! ($cmd)");
        }
        else
        {
	      
        #
        # If no logging object was provided, don't produce
        # any error messages, as it's not clear where they
        # should be sent.
        #
        }
        return JGI_FAILURE;
    }
    }
    else
    {
    #
    # The function was called without a variable to store the
    # command results, so just verify that the command ran
    # successfully.
    #
    
 
    if (system("$cmd"))
    {
	
        if (defined($log))
        {
        $log->error("Command failed! ($cmd)");
	    }
        else
        {
        #
        # If no logging object was provided, don't produce
        # any error messages, as it's not clear where they
        # should be sent.
        #
        }
        return JGI_FAILURE;
    }
    }

   
    #
    # If one gets here, everything must have worked, so return
    # success.
    #

    return JGI_SUCCESS;
} # End of the run_jgi_cmd function.


# 
# Runs a list of command lines in the background
# Waits for them to complete
# 
# If any fail, returns JGI_FAILURE, else JGI_SUCCESS
#
sub run_local_batch_and_wait
{
  my ( $pCmds, $pLog ) = @_;

  my @cmds = @{ $pCmds };
  my @procs;
  for( 0..$#cmds )
  {
    my $cmd = $cmds[ $_ ];
    $procs[ $_ ] = async{ return( system( $cmd ) ) };
  }  
  
  my $isError = 0;
  for( 0..$#cmds )
  {
     my $returnVal = $procs[ $_ ]->join;
     $pLog->info( "run_local_batch_and_wait: $cmds[ $_ ] returned a value of $returnVal\n" );
     if ( $returnVal ) { $isError = 1; }
  }
  return ( $isError ? JGI_FAILURE : JGI_SUCCESS );  
}



# dies if any child process fails
sub run_jazz_command_set
{
    my ( $pUserParams, $cmd_set_ref, $stdout_set_ref, $stderr_set_ref, $working_dir_set_ref, $pLocalParams, $pLog, $use_cluster_override, $local_num_procs ) = @_;  
    my $release_root = $pUserParams->get_param( USER_PARAM_release_root, $pLog );
  my $max_jobs = $pUserParams->get_param( USER_PARAM_local_num_procs, $pLog );

  $pLog->info( "bulk command set of size ".scalar( @{ $cmd_set_ref } ). ": ".$cmd_set_ref->[0] );
 
  
  #write list of commands/stdout/stderr to temp files in working dir
  my $numJobs = scalar( @{$cmd_set_ref} );
  my @commandLines = ();
  for( my $i=0; $i < $numJobs; $i++ )
  {
    my $cmdLine = $cmd_set_ref->[ $i ]." > ".$stdout_set_ref->[ $i ]." 2> ".$stderr_set_ref->[ $i ];
    push( @commandLines, $cmdLine );

    # if the batch is full, run it
    # note, this will run a max of max_jobs at a time, wait, then run the next
    #  batch.  If load balancing is poor then you will waste lots of time...
    if ( scalar( @commandLines ) == $max_jobs || $i == $numJobs - 1 )
    {       
       if ( JGI_FAILURE == run_local_batch_and_wait( \@commandLines, $pLog ) )
       {
           $pLog->error( "local_batch_and_wait: returned FAILURE!\n" );
	   die;
       }
       @commandLines = ();
    }
  }
    print "batch successfully run\n";
  return( JGI_SUCCESS );
}





###################################################################
#                                                                 #
#  The Main Script Start                                          #
#                                                                 #
###################################################################

{
  # A string to identify this function.
  my $function_id = "main";

  # parameter file
  my $userParamsFileName = "autoRinger.params";

  # parameter definition files
  my $userDefFileName;
  my $localDefFileName;

  # JGI_Generic_Config objects for loading local and user parameters
  my $pUserParams;
  my $pLocalParams;

  # startup logging object
  my $startup_log;

  # the main assembly directory
  my $assembly_dir;

  # add to the environment, so that system calls to perl will pass it along
  if (not exists $ENV{ PERL5LIB })
  {
    $ENV{ PERL5LIB } = "";
  }
  $ENV{ PERL5LIB } = $INC[0].":".$ENV{ PERL5LIB };

  
  
  #
  # Set the umask value to standardize the manner in which files are
  # created.
  #
  my $previous_umask = umask(QC_UMASK);

  #
  # Create a startup logging object
  #
  Log::Log4perl->easy_init( $ERROR );
  $startup_log = get_logger();

  if ( !defined($startup_log) )
  {
    print_jgi_msg( $SCRIPT_ID, $VERSION_ID, $function_id, JGI_MSG_ERROR, "Fatal Error: Cannot create startup log" );
    exit(0);
  }

  my $cmdLine = "";
  foreach my $arg( @ARGV )
  {
    $cmdLine .= $arg." ";
  }

  my $start_stage;
  my $stop_stage = last_stage();

  eval                                      # now that we have a log file, catch all exceptions up until the main loop.
  {

 #stages
 

    #
    # Get the command line options
    #
    my $userSpecifiedParamsFileName;
    my $label;
    my ( $project_tracking, $stepping, $archive );
    my ( $resume, $restart );  
  
    unless (
      &GetOptions(
        'c|config=s', \$userSpecifiedParamsFileName,
        'label=s',    \$label,
        'track',     \$project_tracking,
        'restart',  \$restart,
        'resume',   \$resume,
        'start=s',    \$start_stage,
        'stop=s',     \$stop_stage,
        'step',       \$stepping,
        'archive',    \$archive,
        'output_dir=s', \$assembly_dir,
        'full_outputs', \$bKeepFullOutputs,
        'h|help',     \&script_usage,
        'v|version',  \&script_version
      )
      )
    {
      $startup_log->error("Unable to process the command line arguments");
      script_usage();
      die;
    }
    

    # Check cmd line options
    if ( defined($resume) and defined($restart) )
    {
      $startup_log->error("Please specify either -resume OR -restart");
      die;
    }

    if ( defined($start_stage) and not ( defined $restart|| defined ( $resume) ) )
    {
      $startup_log->error("Cannot use -start without -restart");
      die;
    }

    if ( defined($archive) and not defined $restart)
    {
      $startup_log->error("Cannot use -archive without -restart");
      die;
    }

    # translate these user command-line params which may be undefined into global flags that are always 0 or 1
    $g_resuming = ( defined( $resume ) );
    $g_restarting = ( defined( $restart ) );

    # use the user-specified params file
    if ( defined($userSpecifiedParamsFileName) )
    {
      $userParamsFileName = $userSpecifiedParamsFileName;
    }

    #
    # Make sure user parameter file exists
    #
    if ( !-e $userParamsFileName )
    {
      $startup_log->error(" Parameter file: $userParamsFileName not found");
      die;
    }


    # Look for parameter definition files in the local directory
    {
      $0 = readlink($0) if (-l $0); #  if jazz.pl is a symlink, get actual path      
      my $script_dir = dirname($0);         # $0 is first cmd line argument

      $userDefFileName  = $userParamsFileName.".defs";
      $localDefFileName = "autoRinger.local.params.defs";

      foreach my $def_file ( $userDefFileName, $localDefFileName )
      {
        if ( !-e $def_file )
        {
          $startup_log->error("Parameter definition file: $def_file not found!");
          die;
        }
      }
    }

    #
    #  Create user parameters object and load user parameters
    #
    $pUserParams = JGI_Generic_Config->new( $userDefFileName, $startup_log, $userParamsFileName );
    if ( !defined $pUserParams || $@ )
    {
      $startup_log->error("Cannot load $userParamsFileName");
      die;
    }


    #
    #  Open stages script, and load stage names into queue
    #
    my $stages_script = $pUserParams->get_param( USER_PARAM_stages_script, $startup_log );
    die "stages_script not specified" unless defined( $stages_script );
    open( STAGES_SCRIPT, $stages_script ) || die "$stages_script could not be opened\n";
    @stages = ();
    while( <STAGES_SCRIPT> )
    {
        chomp;
        push @stages, $_;
    }
  
   
    validate_stage_name( $start_stage, $startup_log );
    validate_stage_name( $stop_stage,  $startup_log );
        

    if ( $g_resuming || $g_restarting )    # User is re-running an assembly
    {
      ### Setup re-run directory
      if ( $assembly_dir eq "" )     # user did not specify a dir so find the latest one.
      {
        my $label = create_label( $pUserParams, $startup_log );
        $assembly_dir = ( split( /\n/, `ls -d1t $label*` ) )[0];

      }
      if ( $assembly_dir !~ /^\// )    # if cmd line option for re-run dir is a relative path
      {
        my $pwd = cwd();
        $assembly_dir = "$pwd/$assembly_dir";    # make it absolute
      }

      # Check if it's a valid dir
      if ( not -d $assembly_dir )
      {
        $startup_log->error("$assembly_dir is not a valid directory\n");
        die;
      }

      ### Setup start stage      
      my $checkpoint_dir = "$assembly_dir/checkpoints";
      my $last_failed_stage;    # stage at which last run failed

      determine_checkpoint( \@stages, $checkpoint_dir, \$last_failed_stage, $startup_log );
      if ( not defined($start_stage) or stage_is_after( $start_stage, $last_failed_stage ) )
      {                         #user did not specify or specified a stage that has not run yet
        $start_stage = $last_failed_stage;
      }
      

      ### Load local parameter file
      my $local_params_file = "$checkpoint_dir/$start_stage.local.params";
      if ( not -e $local_params_file )
      {
	  ### Create local parameters object and load in local parameters
	  my $localParamsFileName = "$checkpoint_dir/$start_stage.local.params";
	  open( F, ">$localParamsFileName" );    #create a blank parameter file

	  $pLocalParams = JGI_Generic_Config->new( $localDefFileName, $startup_log, $localParamsFileName );
	  $pLocalParams->put_param( LOCAL_PARAM_assembly_dir, $assembly_dir, $startup_log );
	  $pLocalParams->put_param( LOCAL_PARAM_label,        $label,        $startup_log );
		$pLocalParams->put_param( LOCAL_PARAM_local_resume_checkpoint, "", $startup_log );
      }   
      else
      {
	  $pLocalParams = JGI_Generic_Config->new( $localDefFileName, $startup_log, $local_params_file );
      }


      ### Cleanup any old stage directories
      if (not $g_resuming)
      {    
        cleanup_for_restart($start_stage,$assembly_dir, $archive, $pUserParams, $pLocalParams,$startup_log);
      } 
      
      # remove old checkpoints and parameter files
      for ( my $stage = $start_stage ; defined($stage) ; $stage = stage_after($stage) )
      {
        eval { unlink( "$checkpoint_dir/$stage" . QC_CHECKPOINT_SUFFIX ) };          #remove checkpoint
        next if ( ( $stage eq $start_stage ) );
        eval { unlink("$checkpoint_dir/$stage.local.params") };                      #remove param file
      }    
    }
    else                                                                            #### a new run
    {
      $start_stage = first_stage(); 
      $stop_stage = last_stage();


      ### Create label, if not specfied on command line
      $label = create_label( $pUserParams, $startup_log ) if ( not defined($label) );

      ### Create a timestamp-based working directory
      if ( !defined( $assembly_dir ) )
      {
        $assembly_dir = create_assembly_directory_name($label);
      }

      run_sys_cmd( $pUserParams, "mkdir $assembly_dir", $startup_log );

      ### Setup checkpoint directory
      my $checkpoint_dir = "$assembly_dir/checkpoints";
      run_sys_cmd( $pUserParams, "mkdir $checkpoint_dir", $startup_log );

      ### Create local parameters object and load in local parameters
      my $localParamsFileName = "$checkpoint_dir/$start_stage.local.params";
      open( F, ">$localParamsFileName" );    #create a blank parameter file

      $pLocalParams = JGI_Generic_Config->new( $localDefFileName, $startup_log, $localParamsFileName );
      $pLocalParams->put_param( LOCAL_PARAM_assembly_dir, $assembly_dir, $startup_log );
      $pLocalParams->put_param( LOCAL_PARAM_label,        $label,        $startup_log );


  }    # end of else (not a previous run)

    ### Find the appropriate Jazz and Perl directories and update local params file with these values
    if ( compute_platform_specific_paths( $pLocalParams, $pUserParams, $startup_log ) != JGI_SUCCESS )
    {
      $startup_log->error("could not compute platform specific paths!");
      die;
    }
  

    if ( defined($stepping) )
    {
      $stop_stage = $start_stage;
    }

  };    #end of startup eval block

  if ($@)
  {

    print STDERR $@;
    #goes to END block above if error during startu
    exit(JGI_FAILURE);
   
  }
  undef $startup_log;




  #
  # Setup running log directory
  #

  # create running log object  
  my $logDir = "$assembly_dir/log";
  my $conf_ref = \qq (

 # Custom filters for separating warn, error, fatal

 # Filter to match levels ERROR to FATAL
log4perl.filter.MatchError = Log::Log4perl::Filter::LevelRange
log4perl.filter.MatchError.LevelMin      = ERROR
log4perl.filter.MatchError.LevelMax      = FATAL
log4perl.filter.MatchError.AcceptOnMatch = true

 # Filter to match level WARN
log4perl.filter.MatchWarn  = Log::Log4perl::Filter::LevelMatch
log4perl.filter.MatchWarn.LevelToMatch  = WARN
log4perl.filter.MatchWarn.AcceptOnMatch = true

# root logger uncategorized loggers end up here

log4perl.logger                           = DEBUG, rootlogDebug, rootlogErrors, rootlogWarn
log4perl.appender.rootlogDebug            = Log::Log4perl::Appender::File
log4perl.appender.rootlogDebug.mkdir      = sub { mkdir '$logDir' unless(-e '$logDir') }
log4perl.appender.rootlogDebug.filename   = $logDir/debug.log
log4perl.appender.rootlogDebug.layout     = Log::Log4perl::Layout::PatternLayout
log4perl.appender.rootlogDebug.layout.ConversionPattern = %d %F{1} %M %L> %m %n

# root level error log
log4perl.appender.rootlogErrors           = Log::Log4perl::Appender::File
log4perl.appender.rootlogErrors.mkdir     = sub { mkdir '$logDir' unless(-e '$logDir') }
log4perl.appender.rootlogErrors.filename  = $logDir/errors.log
log4perl.appender.rootlogErrors.Filter    = MatchError
log4perl.appender.rootlogErrors.layout    = Log::Log4perl::Layout::PatternLayout
log4perl.appender.rootlogErrors.layout.ConversionPattern = %d %F{1} %M %L> %m %n

# root level warn log
log4perl.appender.rootlogWarn            = Log::Log4perl::Appender::File
log4perl.appender.rootlogWarn.mkdir      = sub { mkdir '$logDir' unless(-e '$logDir') }
log4perl.appender.rootlogWarn.Filter     = MatchWarn
log4perl.appender.rootlogWarn.filename   = $logDir/warn.log
log4perl.appender.rootlogWarn.layout     = Log::Log4perl::Layout::PatternLayout
log4perl.appender.rootlogWarn.layout.ConversionPattern = %d %F{1} %M %L> %m %n

# commands logger, note not using filter

log4perl.logger.commands             = INFO, commands
log4perl.appender.commands           = Log::Log4perl::Appender::File
log4perl.appender.commands.mkdir     = sub { use File::Path; mkpath '$logDir' unless(-e '$logDir') }
log4perl.appender.commands.filename  = $logDir/commands.log
log4perl.appender.commands.layout    = Log::Log4perl::Layout::PatternLayout
log4perl.appender.commands.layout.ConversionPattern = %d %F{1} %M %L> %m %n

# a modules logger 
log4perl.logger.modulelogtest          = DEBUG, modulelogtest
log4perl.category.modulelogtest          = DEBUG, modulelogtest
log4perl.appender.modulelogtest          = Log::Log4perl::Appender::File
log4perl.appender.modulelogtest.mkdir    = sub { use File::Path; mkpath '$logDir' unless(-e '$logDir') }
log4perl.appender.modulelogtest.filename = $logDir/modules.log
log4perl.appender.modulelogtest.layout   = Log::Log4perl::Layout::PatternLayout
log4perl.appender.modulelogtest.layout.ConversionPattern = %-4r [%t] %-5p %c %x - %m%n 
); 

  Log::Log4perl->init( $conf_ref );
  my $pLog = Log::Log4perl->get_logger("main");
  if ( !defined($pLog) )
  {
    print_jgi_msg( $SCRIPT_ID, $VERSION_ID, $function_id, JGI_MSG_ERROR, "Error creating log directory" );
    die;

  }

  # write out some convienience scripts for controlling this process 
  my %signal_map = ( 'pause'      , SGE_PAUSE_SIGNAL,
                     'pause-hard' , SGE_PAUSE_HARD_SIGNAL,
                     'resume'     , SGE_RESUME_SIGNAL,
                     'halt'       , SGE_HALT_SIGNAL,
                 );
                 
  while (my ($script,$signal) = each %signal_map)
  {      
    $script = "$assembly_dir/sge-$script";
     system ("echo kill -$signal $$ > $script; chmod  +x $script")  # $$ = this process ID
   }  
  
  # redirect stdout and stderr to a log file
  open( my $oldout, ">&STDOUT" ) or warn "Can't dup STDOUT: $!";    #save old stdout
   open( STDOUT, '>>', "$assembly_dir/log/jazz.stdout" ) or warn "Can't redirect STDOUT: $!";
   open( STDERR, ">&STDOUT" ) or warn "Can't dup STDOUT: $!";


  # log the user parameter file
  $pLog->info( join( '', `cat $userParamsFileName` ) );
  $pLog->info( "jazz.pl running" );

  # write command-line params to log
  $pLog->info( "Command Line: ".$cmdLine );
  # save start stage local params file
  write_stage_params_file( $pLocalParams, $start_stage, $pLog );

  # save current directory
  my $saved_dir = cwd();
  
  #
  # Finally, execute all stages
  #

  my $status = JGI_SUCCESS;

  for ( $g_stage = $start_stage ; defined($g_stage) ; $g_stage = stage_after($g_stage) )
  {
    my $params = '($pUserParams, $pLocalParams, $pLog)';

		$pLog->info( "Running stage: $g_stage\n" );

    ## check inputs for stage
    my $check_input_function = "check_inputs_stage_$g_stage";    # create function name
    $status = eval $check_input_function . $params;
    if ( $@ || $status != JGI_SUCCESS )                        # $@: a die statement was caught
    {
      $status = JGI_FAILURE;
      $pLog->error("Inputs for stage $g_stage not valid.\n$@");
      last;
    }

    ## create stage directory if required
    eval 
    {
      my $stage_dir = stage_work_directory($g_stage);
      $stage_dir = "$assembly_dir/$stage_dir";
      run_sys_cmd( $pUserParams, "mkdir $stage_dir", $pLog ) if ( not -d $stage_dir );
      chdir $stage_dir;
    };
    last if ($@);

    ## create log for stage
    my $stage_log  = Log::Log4perl->get_logger("$g_stage");

    $stage_log = $pLog if (not defined($stage_log) ); # use general log if creating failed

    ## run the stage
    $pLog->info( "Running stage: $g_stage" );

    my $stage_function = "run_$g_stage";    # create function name
    $status = eval $stage_function . '($pUserParams, $pLocalParams,$stage_log)' ;
             
    if ( $@ || $status != JGI_SUCCESS )    # $@: a die statement was caught
    {

      $status = JGI_FAILURE;
      $pLog->error("Stage $g_stage failed.\n$@");
      last;
    }

    ## check outputs for stage
    my $check_output_function = "check_outputs_stage_$g_stage";    # create function name
    $status = eval $check_output_function . $params;
    if ( $@ || $status != JGI_SUCCESS )                          # $@: a die statement was caught
    {
      $status = JGI_FAILURE;
      $pLog->error("outputs for stage $g_stage not valid.\n$@");
      last;
    }

    ##create checkpoint for stage
    if ( create_checkpoint( $g_stage, "$assembly_dir/checkpoints", $pLog ) != JGI_SUCCESS )
    {
      $status = JGI_FAILURE;
      $pLog->error("Checkpoint creation failed!");
      last;
    }

    ## write local params file of next stage
    my $next_stage = stage_after($g_stage);
    write_stage_params_file( $pLocalParams, $next_stage, $pLog );

    last if ( $g_stage eq $stop_stage );
  }
  
  #restore original directory
  chdir $saved_dir;
  
  #send_email( $pUserParams,$pLocalParams,$pLog,$stage);

  open( STDOUT, ">&", $oldout );    # restore STDOUT

  if ( $status == JGI_SUCCESS )
  {

    # print the disk usage

    $pLog->info( "ran to completion succesfully" );
  }
  else
  {
    system "cat $assembly_dir/" . LOG_PREFIX . '.err.log';
  }


  #
  # Restore the old umask value.
  #
  umask($previous_umask);

  exit($status);
}



######################## End of main script #############################

###################################################################
# Function Definitions                                            #
###################################################################

# Place any function definitions here.  A sample skeleton function is
# provided.
#
# Function: function_skeleton
# ---------------------------
# This is a skeleton outline of a local function.
# This function takes the following arguments:
#
# $_[0]     The first argument.
#
# $_[n]     The last argument.
#
# This function returns the following values:
#
# JGI_SUCCESS     The function successfully finished.
#
# JGI_FAILURE     Something went wrong.
#
sub function_skeleton
{

  # Please refer to the above function description for details of
  # what these variables are.
  my $in = @_;

  # A string to identify this function.
  my $function_id = "function_skeleton";

  #
  # If one gets here, everything must have worked, so return
  # success.
  #
  return JGI_SUCCESS;
}

#################################################################################
#
# Stage functions
#

sub first_stage
{
  return $stages[0];
}

sub last_stage
{
  return $stages[$#stages];
}

sub stage_before
{
  my ($stage) = @_;

  for ( my $i = 1 ; $i <= $#stages ; $i++ )
  {
    next if ( $stage ne $stages[$i] );
    return $stages[ $i - 1 ];
  }
  return undef;
}

sub stage_after
{
  my ($stage) = @_;

  for ( my $i = 0 ; $i < $#stages ; $i++ )
  {
    next if ( $stage ne $stages[$i] );
    return $stages[ $i + 1 ];
  }
  return undef;
}

sub stage_is_after
{
  my ( $stage1, $stage2 ) = @_;

  for ( my $i = 0 ; $i <= $#stages ; $i++ )
  {
    return 0 if ( $stage1 eq $stages[$i] );
    return 1 if ( $stage2 eq $stages[$i] );
  }
  return 0;
}

sub validate_stage_name
{
  my ( $stage, $log ) = @_;

  my %stages = map { $_ => 1 } @stages;
  if ( defined $stage and not exists $stages{$stage} )
  {
    $log->error( "$stage is not a valid stage:\n" . join( ', ', @stages ) );
    die;
  }
}

#  Function: stage_directory
#  Returns the directory of the given stage.
#  Most of the time, this is the stage name, but there are exceptions,(see map below)
#  undef is returned for stages that use a previous stage's directory

sub stage_directory
{
  my ($stage) = @_;
  my %stage_directory_map = (
    'import',            'input',
    'brd',               'brd',
    'malign',            'gaps',
                             
                             );

  if ( exists $stage_directory_map{$stage} )
  {
    return $stage_directory_map{$stage};
  }

  return $stage;
}

# Function: stage_work_directory
# Returns the working directory of a stage. This could be the stage directory
# of some previous stage

sub stage_work_directory
{
  my ($stage) = @_;
  my $dir = stage_directory($stage);
  while(not defined ($dir))
  {
    $stage = stage_before($stage);
    $dir = stage_directory($stage);
  }  
  return $dir;
}


# returns the array of values for a given library
# value: 
sub get_info_for_lib
{
  my ( $pLibName, $pLibInfo ) = @_;
  for( my $i=0; $i<scalar( @{$pLibInfo} ); $i++ )
  {
    if ( $pLibInfo->[$i]->[0] eq $pLibName )
    {
	return( $pLibInfo->[ $i ] );
    }
  }
  return( 0 );
}

sub load_all_lib_info
{
    my ( $assembly_dir ) = @_;
    my @libInfo;

  open( LIB_FILE, "$assembly_dir/meraculous_import/lib_info_file.txt" );
  while( <LIB_FILE> )
  {
    # expected fields: name, insert_avg, insertsdev, avgReadLen, libIsIlluminaLongInsert
    chomp;
    my @temp = split( /\t/, $_ );
    push @libInfo, \@temp;
  }

  #sort the array based on insert size, ascending
  my @temp = sort { $a->[1] <=> $b->[1] } @libInfo;
  @libInfo = @temp;
  return( @libInfo );
}


#
# format: /(\S+)....(\S+).scarf
sub get_lib_name_from_scarf_path
{
  my ( $scarfPath ) = @_;
  
  $scarfPath =~ /\S+\/(\S+)\.scarf/;
  return( $1 );
}  



# writes a local parameter file for a given stage
# undoes some damage done by config_print_all, which renames the previous local params file
sub write_stage_params_file
{
  my ( $pLocalParams, $stage, $pLog ) = @_;

  my ($assembly_dir);
  $assembly_dir = $pLocalParams->get_param( LOCAL_PARAM_assembly_dir, $pLog );

  if ( defined($stage) )
  {
    my $prev_file              = $pLocalParams->{'CONFIG_FILE'};                    # save old filename
    my $local_params_file_name = "$assembly_dir/checkpoints/$stage.local.params";
    if ( $pLocalParams->config_print_all( $pLog, $local_params_file_name ) != JGI_SUCCESS )
    {
      $pLog->error("Params file creation failed for stage $stage!");
      die;
    }
#run_jgi_cmd( "mv $prev_file\_old $prev_file", $pLog );                          #restore old filename
  }
}

###  removes stage directories 
sub cleanup_for_restart
{
  my ($start_stage,$assembly_dir, $archive, $pUserParams, $pLocalParams,$startup_log ) = @_;
  
  # move old stage dirs to one directory (old)
  my $old_dir = "$assembly_dir/old";
  run_sys_cmd( $pUserParams, "mkdir $old_dir", $startup_log ) if ( not -d $old_dir );    # create old dir
  run_sys_cmd( $pUserParams, "mkdir $old_dir/logs", $startup_log ) if ( not -d "$old_dir/logs/" );    # create old dir
  
  for ( my $stage = $start_stage ; defined($stage) ; $stage = stage_after($stage) )
  {
    if (-d "$assembly_dir/logs/$stage")
    {
      system( "rm $assembly_dir/logs/$stage" )  ;      #move stage log dir to old dir
    }
    
    my $stage_dir = "$assembly_dir/".stage_work_directory($stage);
    if ( -d "$stage_dir" )
    { 
	system( "rm -r $stage_dir" ); 
    }
  }
}

#################################################################################

#
# Function: called_from
# -----------------------
# Returns the name of the function from which a function is called from
#
# Uses perl's caller() function, but also filters out (eval) (which counts as a stack frame)
# ( caller(x) returns info about the x'th stack frame up in the stack, 0 is the current frame)
#
# Takes no arguments

sub called_from
{
  my $i           = 2;    # two up in the stack ( 0 is this func, 1 is caller of this)
  my $function_id = "";
  while ( defined($function_id) )
  {
    $function_id = ( caller($i) )[3];    # get name of caller
    last if not defined($function_id);
    last if ( $function_id ne '(eval)' );    # got an actual function, exit loop
    $i++;    # got an eval, try next frame up;
  }

  if ( not defined($function_id) or $function_id eq '' )    # got to the top
  {
    return 'main';
  }

  $function_id =~ /.*::(.*)/;                               # gets rid of module name (CF perhaps we should not?)
  return $1;
}

#
# Function: JGI_Log::error
# ----------------------------
# Logs an error using this file's $SCRIPT_ID and $VERSION_ID
#
#
# $_[0]  A JGI::log object
# $_[1]  The error strign
# $_[3]  Optional: Name of a calling function

#sub JGI_Log::error
#{
#  my ( $self, $error, $function_id ) = @_;

#  $function_id = called_from() if ( not defined $function_id );

#  $self->write_jgi_msg( JGI_ERROR, $SCRIPT_ID, $VERSION_ID, $function_id, $error );
#}

#
# Function: put_param
# -----------------------
# Saves a key-value pair in a params object.
#
# This function takes the following arguments:
#
# $_[0]     A reference to a params object
# $_[2]     A string that represents  the key
# $_[3]     A string that represents the value
# $_[4]     A reference to the logging object.#
#
sub JGI_Generic_Config::put_param
{
  my ( $self, $key, $value, $pLog ) = @_;
  if ( $self->store_key_value( $key, $value, $pLog ) != JGI_SUCCESS )
  {
    $pLog->error( "could not set key $key to value $value!", called_from() );
    die;
  }

  # always update the file for each modification to params 
	write_stage_params_file( $self, $g_stage, $pLog );

}

#
# Function: get_param
# -----------------------
# Gets the value(s) associated with a key that is optionally passed as an argument.
#
# This function takes the following arguments:
#
# $_[0]     A reference to a params object#
# $_[2]     A reference to the logging object.
# $_[3]     A string that represents  the key
# $_[4]
#
#

sub JGI_Generic_Config::get_param
{
  my ( $self, $key, $pLog, $return_ref ) = @_;
  if ( not defined $return_ref )
  {
    $return_ref = \$return_ref;
  }
  if ( $self->get_key_values( $pLog, $key, $return_ref ) != JGI_SUCCESS )
  {
    $pLog->error( "could not get value associated with the key $key!", called_from() );
    die;
  }
  undef $return_ref if $return_ref eq \$return_ref;
  return $return_ref;
}

# creates a label out of the genus species and strain user parameters if given
sub create_label
{
  my ( $pUserParams, $log ) = @_;
  my $label = 'run';    # default label

  #check if the user specified a genus+species , strain
  my ( $genus, $species, $strain );
#$genus   = $pUserParams->get_param( USER_PARAM_genus,   $log );
#  $species = $pUserParams->get_param( USER_PARAM_species, $log );
#  $strain  = $pUserParams->get_param( USER_PARAM_strain,  $log );
  if ( defined($genus) && defined($species) )
  {
    $label = $genus . '_' . $species;
    $label .= '_'.$strain if ( defined($strain) );
  }

  return $label;
}

sub get_dna_prefix_list_filename
{
  my ( $assembly_dir ) = @_;
  
  return( "$assembly_dir/meraculous_import/prefixBlocks.list" );
}

sub create_assembly_directory_name
{
  my ($label) = @_;

  my ( $sec, $min, $hour, $mday, $mon, $year, $wday, $yday, $isdst ) = localtime();
  $year += 1900;    #the year starts from 1900
  my $timestamp = sprintf( '%4d-%02d-%02d_%02dh%02dm%02ds', $year, $mon + 1, $mday, $hour, $min, $sec );   #which gives YYYY-MM-DD_HHhMMmSSs
  my $pwd       = cwd();

  return $pwd . "/" . $label . "_" . $timestamp;
}

#
# Function: run_sys_cmd
# ---------------------
# Wrapper around run_jgi_cmd
# that ensures all such commands are logged in the appropriate file.
# This function takes the following arguments:
#
# $_[0]     A string containing the to-be-run command.
#
# $_[1]     The logging object for the command.
#
# Note:  This function throws a die exception instead of returning a status value since there are so many
#        calls to it, checking each one for errors would be tedious.
#        However the line number and name of the caller is logged
#
# This function returns the following values:
#
#  The  $result string from run_jgi_cmd
#

sub run_sys_cmd
{
  my ( $pUserParams, $cmd, $pLog ) = @_;
  my $result=JGI_FAILURE;

  my $release_root = $pUserParams->get_param( USER_PARAM_release_root, $pLog );


  if ( ($result = run_jgi_cmd( $cmd, $pLog)) != JGI_SUCCESS )
  {
    my $line_num = ( caller(0) )[2];
    $pLog->error( "System command at line $line_num failed:\n$cmd", called_from() );
    die;    # Throw an exception
  }


  return $result;
}

sub run_sys_cmd_failOk
{  
  my ( $cmd, $pLog ) = @_;
  if ( (run_jgi_cmd( $cmd, $pLog)) != JGI_SUCCESS )
  {
    my $line_num = ( caller(0) )[2];
    $pLog->info( "System command at line $line_num failed in failOk section:\n$cmd", called_from() );
  }
  return JGI_SUCCESS;
}

sub remove_temp_files
{
  my ($dir,$files,$pLog) = @_;       
  my $cmd = "find $dir -name \"$files\" -delete";
  if ( run_jgi_cmd( $cmd, $pLog) != JGI_SUCCESS )
  {
    $pLog->warn( "Failed to remove temporary files \n$cmd" );
  }  
}


sub read_count
{
  my ($fasta) = @_;
  my $reads   = 0;
  my $done    = 0;
  $reads = `grep -c ">" $fasta`;
  return $reads;
}


# Computes the platform-specific path for the jazz root and the Perl binary
# Params:
#    pLocalParams - pointer to the local params object
#    pUserParams - pointer to the user params object
#    pLog - pointer to log object
#
# This function returns the following values:
#
# JGI_SUCCESS     The function successfully finished.
#
# JGI_FAILURE     Something went wrong.
#

sub compute_platform_specific_paths
{

  #
  # Identify the platform and the running environment
  #
  my $os_name;
  $os_name = "x86_64"; 
  $os_name =~ s/^\s+//;
  $os_name =~ s/\s+$//;

  my ( $pLocalParams, $pUserParams, $pLog ) = @_;
  my $jazz         = "";
  my $release_root = "";
  my $perl         = "perl";
  my $sun_version  = "";

  $release_root = $pUserParams->get_param( USER_PARAM_release_root,     $pLog );
#$sun_version  = $pUserParams->get_param( USER_PARAM_sun_jazz_version, $pLog );

  print "os name...$os_name...\n";
  if ( $os_name eq "x86_64" )
  {
    print "running on 64-bit Linux...\n";

  }
  elsif ( $os_name eq "i686" )
  {
    print "running on 32-bit Linux...\n";

  }
  $jazz = $release_root;
  $pLog->info( "$os_name operation system detected" );
  $pLog->info( "Using $jazz as Jazz runtime root directory " );
  if ( -l $jazz )
  {
    my $target = readlink($jazz);
    if ( $target ne "" )
    {

      #setting jazz  root  to the real release directory - we don't want to use link to the CURRENT directory because
      # jazz can be reinstalled at any time during the execution
      $jazz = $target;

    }

  }

  $pLog->info( "Root directory points to: $jazz" );

  $jazz .= '/';    #add final / since it is a path
  
  $pLocalParams->put_param( LOCAL_PARAM_perl,      $perl, $pLog );
  return JGI_SUCCESS;
}

#
# Function: script_usage
# ----------------------
# A function for printing the script usage, invoked when
# the "-h" flag is found on the command line.
# This function takes the following arguments:
#
# $_[0]     [Optional] A variable to indicate the status of the script.
#           If this is set to JGI_FAILURE, the function will return
#           after printing its output, to avoid interfering with any
#           error logging being done by the calling script.
#
# Otherwise, this function exits from the script upon completion,
# returning a value of JGI_SUCCESS.
#
sub script_usage
{

  # Please refer to the above function description for details of
  # what these variables are.
  my ($script_status) = @_;

  print "\nCommand line arguments for $SCRIPT_ID ($VERSION_ID):\n";
  print "\n";
  print <<END_HERE ;
jazz.pl [-c -label | -resume | -restart |-step | -start | -stop | -debug | -track | -h | -v]

  -c/config <parameter file>
  -label   <label>                : provide a label name
  -restart [JAZZ dir to restart]  : restart a previously failed assembly
  -resume  [JAZZ dir to resume]   : restart but preserve any partial results    
  -step                           : execute one stage and stop
  -start   <stage to start from>  : re-run from this stage
  -stop    <stage to end at>      : run to this stage
  -archive                        : save any old files that might be overwritten  
  -debug   <level>                  
  -track                          : track usage             

  -h                Help (Alternatively: --help) 
  -v                Version (Alternatively: --version);

All command line arguments are optional

The default configuration file is 'autoRinger.params', which must be present

The default label name is  <genus>_<species>_[strain] if these are defined in 
the configuration file, and 'run' otherwise;

-resume/-restart : If no directory is given, the most recently run dir. is used.

Invalid command line combinations:
  -restart with     -resume
  -label   with    -restart or -resume
  -start   without -restart or -resume
  -archive without -restart or -resume
  
Valid stages for -start and -stop are:
END_HERE
  foreach (@stages) { print "    $_\n"; }

  #
  # If the script status isn't defined, or is set to something other
  # than failure, exit the script with success.
  #
  if ( ( !defined($script_status) ) || ( $script_status ne JGI_FAILURE ) )
  {
    exit(JGI_SUCCESS);
  }
}





#
# Function: fetch_jgi_fasta
# ----------------------------
# Locates the fasta/qual files for a given jgi library in the
#  central JGI repository, and copies them to a target directory.
#
# $_[0]     [Required] JGI library name
# $_[1]     [Required] Directory to write to
# $_[2]     [Required] Pointer to logging object
#
# returns  wildcard of fasta files fetched ( quals not included )
#
sub fetch_jgi_fasta
{
  my ( $lib, $target_dir, $pLog ) = @_;

  my @fetched_fasta;

  # If the library is badly named, generate an error and exit.
  #
  if ( !( $lib =~ /^[A-Za-z]+$/ ) )
  {
    $pLog->error("Bad library name [ $lib ]!\n");
    die;
  }

  #
  # Construct the local fasta sequence and quality file names, then
  # create them.
  #
  my $sequence_file = $target_dir . "/${lib}" . QC_FASTA_SEQUENCE_SUFFIX;
  my $quality_file  = $target_dir . "/${lib}" . QC_FASTA_QUALITY_SUFFIX;
  if ( create_source_files( $lib, $sequence_file, $quality_file, $pLog ) != JGI_SUCCESS )
  {
    $pLog->error("Unable to construct the sequence/quality files!\n");
    die;
  }

  #
  # Make sure the fasta sequence and quality files are non-empty
  #
  if ( -z $sequence_file || -z $quality_file )
  {
    $pLog->error("Empty sequence/quality file!\n");
    die;
  }

  #
  # Ensure that fasta sequence and quality files contain the same reads, in the same order.
  #
  my $temp_file_1 = rand();
  my $temp_file_2 = rand();

  # Extract list of read IDs from sequence FASTA file
  if ( run_jgi_cmd( "awk '/^>/ {print \$1}' $sequence_file | sed -e 's/^>//' > $temp_file_1", $pLog ) != JGI_SUCCESS )
  {
    $pLog->error("Unable to extract $lib read ID from sequence FASTA file!\n");
    die;
  }

  # Extract list of read IDs from quality file
  if ( run_jgi_cmd( "awk '/^>/ {print \$1}' $quality_file | sed -e 's/^>//' > $temp_file_2", $pLog ) != JGI_SUCCESS )
  {
    $pLog->error("Unable to extract $lib read ID from sequence quality file!\n");
    die;
  }

  my $num_diff;    # stores the number of differences
                   # Compare the two lists of read IDs
  if ( run_jgi_cmd( "diff $temp_file_1 $temp_file_2 | wc -l", $pLog, \$num_diff ) != JGI_SUCCESS )
  {
    $pLog->error("Unable to compare $lib read IDs from sequence and quality files!\n");
    die;
  }

  $num_diff =~ s/\s*//g;

  #print "$lib: $num_diff\n";

  # Check to see if any differences were found
  if ( $num_diff != 0 )
  {
    $pLog->error("$lib FASTA sequence and quality files contain different reads\n");
    die;
  }

  # Delete temporary files containing read IDs
  if ( run_jgi_cmd( "rm $temp_file_1; rm $temp_file_2", $pLog ) != JGI_SUCCESS )
  {
    $pLog->error("Unable to delete temporary $lib read ID files!\n");
    die;
  }
  return ($sequence_file);
}

#
# Function: script_version
# ------------------------
# A function for printing the script version, invoked when
# the "-v" flag is found on the command line.
# This function takes the following arguments:
#
# $_[0]     [Optional] A variable to indicate the status of the script.
#           If this is set to JGI_FAILURE, the function will return
#           after printing its output, to avoid interfering with any
#           error logging being done by the calling script.
#
# Otherwise, this function exits from the script upon completion,
# returning a value of JGI_SUCCESS.
#
sub script_version
{

  # Please refer to the above function description for details of
  # what these variables are.
  my ($script_status) = @_;

  print "$SCRIPT_ID ($VERSION_ID)\n";
  print "Author: $AUTHOR_ID\n";

  #
  # If the script status isn't defined, or is set to something other
  # than failure, exit the script with success.
  #
  if ( ( !defined($script_status) ) || ( $script_status ne JGI_FAILURE ) )
  {
    exit(JGI_SUCCESS);
  }
}

#
# Function: check_files_exist:
# ----------------------------
# Checks that all files in a given list exist in the given directroy
#
# $_[0]     [Required] Directory name
# $_[1]     [Required] A reference to a list of filenames
# $_[3]     [Required] A log object
#
# returns  JGI_SUCCESS if completed successfully
# otherwise returns JGI_FAILURE
#

sub check_files_exist
{
  my ( $directory, $files, $pLog ) = @_;
  my $index = 0;

  while ( $index < $#$files + 1 )
  {
    unless ( -e "$directory/$$files[$index]" )
    {
      $pLog->error( " $$files[$index] not found in $directory", called_from() );    #maybe this is not an error?
      return JGI_FAILURE;                                                               # or continue on and check all files?
    }
    $index++;
  }

  return JGI_SUCCESS;
}

    
# returns: number of bad rows 
sub filter_append_scarf_file
{
    my ( $seqFile, $outputFile ) = @_;
    my $badRows = 0;
    open( S, $seqFile ) || die "coudln't open $seqFile for filter_append";
    open( O, ">>$outputFile" ) || die "couldn't open $outputFile for filter_append";
    # check that it is right # of columns, and that seq length = qual length
    # The column # check also implicity sort of checks that the quality values are in range of what we expect, in that ":" is a character that is used for old Illumina qualities of an earlier format 
    while( <S> )
    {
      chomp;
      my @cols = split( /:/, $_ );
      if ( scalar( @cols ) != 7 || length( $cols[5] ) != length( $cols[6] ) ) 
      {
	$badRows++;
      }
      else
      {
	  print O $_."\n";
      }
    }
    return( $badRows );
}



### Stage: meraculous_import
# The purpose of this stage is to:
# - collate input files into a single file per lib
# - convert to scarf if needed
# - verify them rudimentarily
# - divide into chunks of size of our choosing ( for parallelization --
#    only if num_sub_blocks > 1 )
# - provide a way to easily map each of these files with a library name/insert size info
#
#  At the end of the stage, we get scarf files of a certain size where 
#    all the reads for library code $lib can be found by $lib.scarf.*, whose
#    insertAvg/Sdev can be found by loading lib_info_file.txt into memory
#  To get all the reads using one wildcard, use "*.scarf.*"
sub run_meraculous_import
{
  my $function_id = 'run_meraculous_import';
  my ( $pUserParams, $pLocalParams ) = @_;
  my $tempfile;
  my $directory_structure = DIR_STRUCTURE_FILENAME;
  my $pLog = get_logger( "main::meraculous_import" );
  my $assembly_dir = $pLocalParams->get_param( LOCAL_PARAM_assembly_dir, $pLog );
  my $perl         = $pLocalParams->get_param( LOCAL_PARAM_perl,         $pLog );
  my $release_root    = $pUserParams->get_param( USER_PARAM_release_root,    $pLog );
  my $mer_size     = $pUserParams->get_param( USER_PARAM_meraculous_mer_size,     $pLog );
  my $numPrefixBlocks = $pUserParams->get_param( USER_PARAM_num_prefix_blocks, $pLog );
  my $work_dir = "$assembly_dir/meraculous_import";
  mkdir $work_dir;
  my $minQualToKeep = $pUserParams->get_param( USER_PARAM_min_qual_to_keep, $pLog );

  my $pwd = $ENV{'PWD'};    # save current dir.
  chdir $work_dir;
   
  # get key values
  my( @libWildCards, @libNames, @libInsertSizeAvg, @libInsertSizeSdev );
  my ( @libAvgReadLen, @libIsIlluminaLongInsert );
  my ( @libIsOldQualFormat, @libPMin, @libUseForContigs, @libUseForOno );
  $pUserParams->get_key_values( $pLog, USER_PARAM_lib_seq, \@libWildCards, \@libNames,\@libInsertSizeAvg, \@libInsertSizeSdev, \@libAvgReadLen, \@libIsIlluminaLongInsert, \@libIsOldQualFormat, \@libPMin, \@libUseForContigs, \@libUseForOno );
  my ( @all_fasta_files );
  my $totalReads = 0;

  open( LIB_FILE, ">lib_info_file.txt" );
  for( my $i=0; $i < scalar(@libWildCards); $i++ )
  {
    my $libName = $libNames[ $i ];
    my $fileOrWildcard = $libWildCards[ $i ];
    my $insertAvg = $libInsertSizeAvg[ $i ];
    my $insertSdev = $libInsertSizeSdev[ $i ];
    my $avgReadLen = $libAvgReadLen[ $i ];
    my $isIlluminaLongInsert = $libIsIlluminaLongInsert[ $i ];
    my $isOldQualFormat = $libIsOldQualFormat[ $i ];
    my $pMin = $libPMin[ $i ];
    my $bUseForContigs = $libUseForContigs[ $i ];
    my $bUseForOno = $libUseForOno[ $i ];

    chomp($fileOrWildcard);
    $fileOrWildcard =~ s/^\s+//;
    $fileOrWildcard =~ s/\s+$//;

    my @wildcardSet;

    # is this a wildcard?
    if ( $fileOrWildcard =~ /\*/ )
    {
      #$fileOrWildcard =~ s/\*/\\\*/g;

      @wildcardSet = split( /\s+/, `ls $fileOrWildcard` );
      if ( $? )
      {
         die $pLog->error("Cannot open fasta $fileOrWildcard");
      }
    }
    else
    {
        push @wildcardSet, $fileOrWildcard;
    }

    
    # at this point, @wildcardSet has all the files for this lib
    # we want:
    # - to verify it is proper scarf
    # - combine into one big file so we can...
    # - divide the big file into pieces of a workable size
    `rm $libName.scarf`;

    foreach my $seqFile( @wildcardSet )
    {
      my $numBadReads = filter_append_scarf_file( $seqFile, "$libName.scarf" );
      if ( $numBadReads > 0 )
      {
	$pLog->error( "$seqFile is not proper scarf format: $numBadReads bad reads found" );
	die;
      }
    }
    
    #divide it up
    my @a=split(/\s+/, `wc $libName.scarf` );
    my $totalReadsInThisLib = $a[1];
    $totalReads += $totalReadsInThisLib;

    my $numSubBlocksPerLib = $pUserParams->get_param( USER_PARAM_num_sub_blocks_per_lib, $pLog );

    my $numReadsPerFile = int( $totalReadsInThisLib / $numSubBlocksPerLib ) + 1;
    run_sys_cmd( $pUserParams, "split -l $numReadsPerFile $libName.scarf $libName.scarf.", $pLog );
  
    # print to LIB_FILE  
    print LIB_FILE "$libName\t$insertAvg\t$insertSdev\t$avgReadLen\t$isIlluminaLongInsert\t$isOldQualFormat\t$pMin\t$bUseForContigs\t$bUseForOno\n";

    # rm original file
    run_sys_cmd( $pUserParams, "rm $libName.scarf", $pLog );
  }  

  # determine prefix boundaries per block, for load balancing
  # 1. sample the reads ( 100K total )
       
  my $scarfFileSample = "$assembly_dir/meraculous_import/sample.all";
  my $desiredSampleSize =  ( 100000 > $totalReads ? $totalReads : 100000 );
  my $sampleRatio = $desiredSampleSize / $totalReads;

  if ( JGI_SUCCESS != run_jgi_cmd( "cat $assembly_dir/meraculous_import/*.scarf.* | perl ".$release_root.PATH_RANDOM_LIST2." - $sampleRatio $desiredSampleSize > $scarfFileSample", $pLog ) ) 
  {
    return JGI_FAILURE;
  }

  # 2. mercount the sample
  my $mercounterS = $release_root . PATH_MERCOUNTERS;
 
  run_sys_cmd( $pUserParams, "$mercounterS $scarfFileSample $mer_size \"\" \"\" 0 $minQualToKeep > sample.mercount 2> sample.mercount.err", $pLog );

  run_sys_cmd( $pUserParams,"sort -S 2G -k 1,1 sample.mercount > sample.mercount.sorted", $pLog );

  # 3. load balance the mercount
  my $loadBalanceMercount = $release_root.PATH_LOAD_BALANCE_MERCOUNT;
  
  run_sys_cmd( $pUserParams, "perl $loadBalanceMercount sample.mercount.sorted $numPrefixBlocks > ".get_dna_prefix_list_filename( $assembly_dir ), $pLog );

  close LIB_FILE;

  return JGI_SUCCESS;
}

sub check_inputs_stage_meraculous_import
{
    return JGI_SUCCESS;
}
sub check_outputs_stage_meraculous_import
{
    return JGI_SUCCESS;
}


  
### Stage: meraculous_mercount
sub run_meraculous_mercount
{
  my $function_id = 'run_meraculous_mercount';
  my ( $pUserParams, $pLocalParams ) = @_;
  my $pLog = get_logger( "main::meraculous_mercount" );

  my $tempfile;
  my $directory_structure = DIR_STRUCTURE_FILENAME;

  my $assembly_dir = $pLocalParams->get_param( LOCAL_PARAM_assembly_dir, $pLog );
  my $perl         = $pLocalParams->get_param( LOCAL_PARAM_perl,         $pLog );
  my $release_root    = $pUserParams->get_param( USER_PARAM_release_root,    $pLog );
  my $mer_size     = $pUserParams->get_param( USER_PARAM_meraculous_mer_size,     $pLog );
  my $use_cluster = $pUserParams->get_param( USER_PARAM_use_cluster, $pLog );
  my $minQualToKeep = $pUserParams->get_param( USER_PARAM_min_qual_to_keep, $pLog );
  my $localResumeCheckpoint = $pLocalParams->get_param( LOCAL_PARAM_local_resume_checkpoint, $pLog );
  my $minMercountToReport = $pUserParams->get_param( USER_PARAM_min_mercount_to_report, $pLog );
  my $numProcs = $pUserParams->get_param( USER_PARAM_local_num_procs, $pLog );
  my $mercountNumProcs = $pUserParams->get_param( USER_PARAM_num_procs_mercount, $pLog );
  if ( $mercountNumProcs == 0  ) { $mercountNumProcs = $numProcs; }


  my $work_dir = "$assembly_dir/meraculous_mercount";
  mkdir $work_dir;

  my $pwd = $ENV{'PWD'};    # save current dir.
  chdir $work_dir;

  my @prefixes = compute_dna_prefixes( get_dna_prefix_list_filename( $assembly_dir ), $pLog );

  my @commands;
  my @stdouts;
  my @stderrs;
  my @workingDirs;
  my @libInfo = load_all_lib_info( $assembly_dir );

  my @scarf_files = split( /\s+/, `ls $assembly_dir/meraculous_import/*.scarf.*` );
  chomp @scarf_files;

  if ( $g_resuming && $localResumeCheckpoint eq "sort_mers" ) { goto sort_mers; }

  for( my $i=0; $i < scalar( @scarf_files ); $i++ )
  {
      my $libName = get_lib_name_from_scarf_path( $scarf_files[ $i ] );
      if ( !( get_info_for_lib( $libName, \@libInfo )->[ 7 ] ) )
      {
	  # skip if flag set to not use for contigging
	  next;
      }

      my $localized_scarf_file = "`/jgi/tools/bin/localize_file ".$scarf_files[ $i ]."`";
    for( my $j=0; $j < scalar( @prefixes ); $j++ )
    {
      my $mercounterS = $release_root . PATH_MERCOUNTERS;
      
      push( @commands, "$mercounterS ".$localized_scarf_file." $mer_size ".$prefixes[ $j ]->[0]. " ".$prefixes[ $j ]->[1]." $minMercountToReport $minQualToKeep" );
      push( @stdouts, "$work_dir/mercount.$i.".$prefixes[ $j ]->[0] );
      push( @stderrs, "$work_dir/mercount.$i.".$prefixes[ $j ]->[0].".err" );
      push( @workingDirs, "$work_dir" );
    }
  }
  run_jazz_command_set( $pUserParams, \@commands, \@stdouts, \@stderrs, \@workingDirs, $pLocalParams, $pLog, 0, $mercountNumProcs );

  $pLocalParams->put_param( LOCAL_PARAM_local_resume_checkpoint, "sort_mers", $pLog );
sort_mers:
  # sort prefixes 
  @commands = ();
  @stdouts = ();
  @stderrs = ();
  @workingDirs = ();
  foreach my $prefix ( @prefixes )
  {
      my $sortSmash = $release_root . PATH_SORT_SMASH;
      push ( @commands, "sort -k 1,1 -S 2G -T .  ".$work_dir."/mercount.*.".$prefix->[0]." | perl $sortSmash -c - " );
      push ( @stdouts, "$work_dir/mercount.all.sorted.".$prefix->[0] );
      push ( @stderrs, "$work_dir/mercount.all.sorted.".$prefix->[0].".err" );
      push( @workingDirs, "$work_dir" );
  }

  run_jazz_command_set( $pUserParams, \@commands, \@stdouts, \@stderrs, \@workingDirs, $pLocalParams, $pLog, 0, $mercountNumProcs );
  
  # histogram generation
  my $randomList = $release_root . PATH_RANDOM_LIST2;
  my $histogram2 = $release_root . PATH_HISTOGRAM;
  if ( JGI_SUCCESS != run_sys_cmd( $pUserParams, "cat mercount.all.sorted.*[ACGT] | perl $randomList - .01 10000000 | perl $histogram2 - 2 1 > mercount.hist", $pLog ) )
  {
      $pLog->error( "couldn't generate histogram" );
      return JGI_FAILURE;
  }
  if ( JGI_SUCCESS != run_sys_cmd( $pUserParams, "echo \"set terminal png; set output 'mercount.png'; set log y; plot [5:100] 'mercount.hist' using 2:7 with boxes\" | gnuplot", $pLog ) )
  {
      $pLog->error( "couldn't generate png" );
      return JGI_FAILURE;
  }

  # cleanup
  if ( !$bKeepFullOutputs )
  {
    run_sys_cmd_failOk( "rm mercount.[0-9]*[A-Z]", $pLog );
  }
  return JGI_SUCCESS;
}

sub check_inputs_stage_meraculous_mercount
{
    return JGI_SUCCESS;
}
sub check_outputs_stage_meraculous_mercount
{
    return JGI_SUCCESS;
}

# converts a base 10 number to a given base ( less than 10 )
sub to_base
{
  my $base   = shift;
  my $number = shift;
  die if ( $base > 10 );
  my @nums = (0..9);
  return $nums[0] if $number == 0;
  my $rep = ""; # this will be the end value.
  while( $number > 0 )
  {
    $rep = $nums[$number % $base] . $rep;
    $number = int( $number / $base );
  }
  return $rep;
}

# for a given length, returns the array of DNA prefix codes required to 
#   sample the entire search space of possible DNA sequences
# returns pairs of "$startPrefix $endPrefix"
sub compute_dna_prefixes
{
  my ( $prefixListFile, $pLog ) = @_;

    
  my @prefixes;
  if ( !open( F, $prefixListFile ) )
  {
    $pLog->error( "couldn't open prefixlistfile $prefixListFile\n" );
    return JGI_FAILURE;
  }

  while( <F> )
  {
    chomp;
    my ( $start, $end ) = split;
    push @prefixes, [ $start, $end ];
  }
  return @prefixes;
}  


sub check_inputs_stage_meraculous_mergraph
{
    return JGI_SUCCESS;
}

sub check_outputs_stage_meraculous_mergraph
{
    return JGI_SUCCESS;
}

sub run_meraculous_mergraph
{
  my $function_id = 'run_meraculous_mergraph';
  my ( $pUserParams, $pLocalParams ) = @_;
  my $pLog = get_logger( "main::meraculous_mergraph" );
  my $tempfile;
  my $directory_structure = DIR_STRUCTURE_FILENAME;

  my $meraculous_min_depth_cutoff = $pUserParams->get_param( USER_PARAM_meraculous_min_depth_cutoff );
  my $assembly_dir = $pLocalParams->get_param( LOCAL_PARAM_assembly_dir, $pLog );
  my $perl         = $pLocalParams->get_param( LOCAL_PARAM_perl,         $pLog );
  my $release_root    = $pUserParams->get_param( USER_PARAM_release_root,    $pLog );
  my $local_resume_checkpoint = $pLocalParams->get_param( LOCAL_PARAM_local_resume_checkpoint, $pLog );

  my $mer_size     = $pUserParams->get_param( USER_PARAM_meraculous_mer_size,     $pLog );
  my $use_cluster = $pUserParams->get_param( USER_PARAM_use_cluster, $pLog );
  my $numProcs = $pUserParams->get_param( USER_PARAM_local_num_procs, $pLog );
  my $numProcsMergraph = $pUserParams->get_param( USER_PARAM_num_procs_mergraph, $pLog );
  if ( $numProcsMergraph == 0 ) { $numProcsMergraph = $numProcs; }

  if ( $meraculous_min_depth_cutoff == 0  )
  {
    # perform automated min-depth detection
    my $cmd = "perl ".$release_root."/".PATH_FINDDMIN." $assembly_dir/meraculous_mercount/mercount.hist ";
    $meraculous_min_depth_cutoff = `$cmd`;
    $pLog->info( "findDMin: setting to $meraculous_min_depth_cutoff" );
    die "findDMin.pl returned illegal value of $meraculous_min_depth_cutoff" unless ( $meraculous_min_depth_cutoff > 2 && $meraculous_min_depth_cutoff < 100 );
    $pLocalParams->put_param( LOCAL_PARAM_autocalibrated_min_depth_cutoff, $meraculous_min_depth_cutoff, $pLog );
 	}
  my $work_dir = "$assembly_dir/meraculous_mergraph";
  mkdir $work_dir;

  my $pwd = $ENV{'PWD'};    # save current dir.
  chdir $work_dir;

  # use m-letter mer prefix to subdivide
  my @prefixes = compute_dna_prefixes( get_dna_prefix_list_filename( $assembly_dir ), $pLog );

  if ( $g_resuming && length( $local_resume_checkpoint ) ) { goto mergraph; }

	foreach my $prefix( @prefixes )
  {
    run_sys_cmd( $pUserParams, "cat $assembly_dir/meraculous_mercount/mercount.all.sorted.".$prefix->[0]." | perl -ane 'if ( \$F[1] >= $meraculous_min_depth_cutoff ) { print \$_; }' > mercount.sorted.".$prefix->[0], $pLog );
  }

	$pLocalParams->put_param( LOCAL_PARAM_local_resume_checkpoint, "mergraph", $pLog );
mergraph:
                   
  my @scarf_files = split( /\s+/, `ls $assembly_dir/meraculous_import/*.scarf.*`);
  chomp @scarf_files;

  my @commands = ();
  my @stdouts = ();
  my @stderrs = ();
  my @workingDirs = ();
  my $mergraphGen = $release_root.PATH_MERGRAPH_GEN;
  my @libInfo = load_all_lib_info( $assembly_dir );

  foreach my $prefix( @prefixes )
  {
    for( my $i = 0; $i < scalar( @scarf_files ); $i++ )
    {
      my $libName = get_lib_name_from_scarf_path( $scarf_files[ $i ] );
      if ( !( get_info_for_lib( $libName, \@libInfo )->[ 7 ] ) )
      {
	  # skip if flag set to not use for contigging
	  next;
      }
    my $localized_scarf_file = "`/jgi/tools/bin/localize_file ".$scarf_files[ $i ]."`";
        push( @commands, "$mergraphGen $work_dir/mercount.sorted.".$prefix->[0]." $mer_size ".$localized_scarf_file." $meraculous_min_depth_cutoff ".$prefix->[0]." ".$prefix->[1] );
        push( @stdouts, "$work_dir/mergraph.$i.".$prefix->[0] );
        push( @stderrs, "$work_dir/mergraph.$i.".$prefix->[0].".stderr" );
				

	push( @workingDirs, "$work_dir" );
    }
  }
  run_jazz_command_set( $pUserParams, \@commands, \@stdouts, \@stderrs, \@workingDirs, $pLocalParams, $pLog, 0, $numProcsMergraph );  
 
 
  return JGI_SUCCESS;
} 



sub check_inputs_stage_meraculous_ufx
{
    return JGI_SUCCESS;
}

sub check_outputs_stage_meraculous_ufx
{
    return JGI_SUCCESS;
}

sub run_meraculous_ufx
{
  my $function_id = 'run_meraculous_ufx';
  my ( $pUserParams, $pLocalParams ) = @_;
  my $pLog = get_logger( "main::meraculous_ufx" );
  my $tempfile;
  my $directory_structure = DIR_STRUCTURE_FILENAME;

  my $meraculous_min_depth_cutoff = $pUserParams->get_param( USER_PARAM_meraculous_min_depth_cutoff );
  if ( 0 == $meraculous_min_depth_cutoff )
  {
      $meraculous_min_depth_cutoff = $pLocalParams->get_param( LOCAL_PARAM_autocalibrated_min_depth_cutoff );
  }
  my $assembly_dir = $pLocalParams->get_param( LOCAL_PARAM_assembly_dir, $pLog );
  my $perl         = $pLocalParams->get_param( LOCAL_PARAM_perl,         $pLog );
  my $release_root    = $pUserParams->get_param( USER_PARAM_release_root,    $pLog );
  my $mer_size     = $pUserParams->get_param( USER_PARAM_meraculous_mer_size,     $pLog );
  my $use_cluster = $pUserParams->get_param( USER_PARAM_use_cluster, $pLog );
  my $numProcs = $pUserParams->get_param( USER_PARAM_local_num_procs, $pLog );
  my $numProcsUfx = $pUserParams->get_param( USER_PARAM_num_procs_ufx, $pLog );
  if ( $numProcsUfx == 0 ) { $numProcsUfx = $numProcs; }

  my $work_dir = "$assembly_dir/meraculous_ufx";
  mkdir $work_dir;

  my $pwd = $ENV{'PWD'};    # save current dir.
  chdir $work_dir;


  my @prefixes = compute_dna_prefixes( get_dna_prefix_list_filename( $assembly_dir ), $pLog );
  my $meraculous = "perl ".$release_root.PATH_MERACULOUS;

  my @commands = ();
  my @stdouts = ();
  my @stderrs = ();
  my @workingDirs = ();
  foreach my $prefix( @prefixes )
  {
      push( @commands, "cat $assembly_dir/meraculous_mergraph/mergraph.*.".$prefix->[0]." | $meraculous -M - -d UFX -D $meraculous_min_depth_cutoff" );
      push( @stdouts, "$work_dir/UFX.".$prefix->[0] );
      push( @stderrs, "$work_dir/UFX.".$prefix->[0].".stderr" );
      push( @workingDirs, "$work_dir" );
  }
  run_jazz_command_set( $pUserParams, \@commands, \@stdouts, \@stderrs, \@workingDirs, $pLocalParams, $pLog, 0, $numProcsUfx );     

  #cleanup
  if ( !$bKeepFullOutputs )
  {
    run_sys_cmd_failOk( "rm ../meraculous_mergraph/mergraph.*[A-Z]", $pLog );
  }  
  return JGI_SUCCESS;
}

sub check_inputs_stage_meraculous_contigs
{
    return JGI_SUCCESS;
}

sub check_outputs_stage_meraculous_contigs
{
    return JGI_SUCCESS;
}

sub run_meraculous_contigs
{
	my $cmd = "";
  my $function_id = 'run_meraculous_contigs';
  my ( $pUserParams, $pLocalParams ) = @_;
  my $pLog = get_logger( "main::meraculous_contigs" );
  my $tempfile;
  my $localResumeCheckpoint = $pLocalParams->get_param( LOCAL_PARAM_local_resume_checkpoint, $pLog );

  my $directory_structure = DIR_STRUCTURE_FILENAME;
  my $meraculous_min_depth_cutoff = $pUserParams->get_param( USER_PARAM_meraculous_min_depth_cutoff );
  if ( 0 == $meraculous_min_depth_cutoff )
  {
      $meraculous_min_depth_cutoff = $pLocalParams->get_param( LOCAL_PARAM_autocalibrated_min_depth_cutoff, $pLog );
  }
  my $assembly_dir = $pLocalParams->get_param( LOCAL_PARAM_assembly_dir, $pLog );
  my $perl         = $pLocalParams->get_param( LOCAL_PARAM_perl,         $pLog );
  my $release_root    = $pUserParams->get_param( USER_PARAM_release_root,    $pLog );
  
  my $mer_size     = $pUserParams->get_param( USER_PARAM_meraculous_mer_size,     $pLog );
  my $use_cluster = $pUserParams->get_param( USER_PARAM_use_cluster, $pLog );
  my $numProcs = $pUserParams->get_param( USER_PARAM_local_num_procs, $pLog );
  my $numProcsContigs = $pUserParams->get_param( USER_PARAM_num_procs_contigs, $pLog );
  if ( $numProcsContigs == 0 ) { $numProcsContigs = $numProcs; }

  my $min_contig_size = 2 * $mer_size; 

  my $work_dir = "$assembly_dir/meraculous_contigs";
  mkdir $work_dir;

  my $pwd = $ENV{'PWD'};    # save current dir.
  chdir $work_dir;

  # ufx normalization
  my $ufxNormalizer = "perl ".$release_root.PATH_UFX_NORMALIZER;  
  my @prefixes = compute_dna_prefixes( get_dna_prefix_list_filename( $assembly_dir ), $pLog );

  my @commands = ();
  my @stdouts = ();
  my @stderrs = ();
  my @workingDirs = ();
  if ( ( length( $localResumeCheckpoint ) ) && $g_resuming ) { goto $localResumeCheckpoint; } 
 
	$pLocalParams->put_param( LOCAL_PARAM_local_resume_checkpoint, "uun", $pLog );

uun:
  foreach my $p ( @prefixes )
  {
    my $prefix = $p->[0];
    my $cmd = "$ufxNormalizer -u ../meraculous_ufx/UFX.$prefix -U \"../meraculous_ufx/UFX.*\" -m 0";
    push( @commands, $cmd );
    push( @stdouts, "$work_dir/$prefix.uun" );
    push( @stderrs, "$work_dir/$prefix.uun.err" );
    push( @workingDirs, "$work_dir" );
  }
  run_jazz_command_set( $pUserParams, \@commands, \@stdouts, \@stderrs, \@workingDirs, $pLocalParams, $pLog, 0, $numProcsContigs );

  @commands = ();
  @stdouts = ();
  @stderrs = ();
 
	$pLocalParams->put_param( LOCAL_PARAM_local_resume_checkpoint, "irreg", $pLog );

irreg:
  foreach my $p ( @prefixes )
  {
    my $prefix = $p->[0];
    my $cmd = "$ufxNormalizer -u $prefix.uun -U \"*.uun\" -m 1";
    push( @commands, $cmd );
    push( @stdouts, "$work_dir/$prefix.irreg" );
    push( @stderrs, "$work_dir/$prefix.irreg.err" );
  }
  run_jazz_command_set( $pUserParams, \@commands, \@stdouts, \@stderrs, \@workingDirs, $pLocalParams, $pLog, 0, $numProcsContigs );

  my $unique = "perl ".$release_root.PATH_UNIQUE; 
  $cmd = "cat *.irreg | $unique - 1 | awk '{print \$1 \"\\t**\"}' > irreg.uunr";
  run_sys_cmd( $pUserParams, $cmd, $pLog );

  @commands = ();
  @stdouts = ();
  @stderrs = ();
 
	$pLocalParams->put_param( LOCAL_PARAM_local_resume_checkpoint, "screen_generic", $pLog );

screen_generic:
  my $screen_generic = "perl ".$release_root.PATH_SCREEN_GENERIC; 
  foreach my $p ( @prefixes )
  {
    my $prefix = $p->[0];
    my $cmd = "$screen_generic irreg.uunr 1 $prefix.uun 1";
    push( @commands, $cmd );
    push( @stdouts, "$work_dir/$prefix.uunr" );
    push( @stderrs, "$work_dir/$prefix.uunr.err" );
  }
  run_jazz_command_set( $pUserParams, \@commands, \@stdouts, \@stderrs, \@workingDirs, $pLocalParams, $pLog, 0, $numProcsContigs );

  @commands = ();
  @stdouts = ();
  @stderrs = ();

	$pLocalParams->put_param( LOCAL_PARAM_local_resume_checkpoint, "uunrext", $pLog );
uunrext:
  foreach my $p ( @prefixes )
  {
    my $prefix = $p->[0];
    my $cmd = "$ufxNormalizer -u $prefix.uunr -U \"*.uun\" -m 2";
    push( @commands, $cmd );
    push( @stdouts, "$work_dir/$prefix.uunrext" );
    push( @stderrs, "$work_dir/$prefix.uunrext.err" );
  }
  run_jazz_command_set( $pUserParams, \@commands, \@stdouts, \@stderrs, \@workingDirs, $pLocalParams, $pLog, 0, $numProcsContigs );
  
  $cmd = "cat *.uunrext | $unique - 1 | awk '{print \$1\"\\t**\"}' > term.uunr";
  run_sys_cmd( $pUserParams, $cmd, $pLog );

   $pLocalParams->put_param( LOCAL_PARAM_local_resume_checkpoint, "contigs", $pLog );

contigs:
  my $meraculous = "perl ".$release_root.PATH_MERACULOUS;
  $cmd = "$meraculous -U \"*.uunr\" -c ".$min_contig_size." > contigs.fa 2> contigs.err";  
  if ( JGI_SUCCESS != run_sys_cmd( $pUserParams, $cmd, $pLog ) )
  {
      $pLog->error( "couldn't generate contigs" );
      return JGI_FAILURE;
  }
}


sub check_inputs_stage_prepare_external_inputs_for_blast_ono
{
    return JGI_SUCCESS;
}

sub check_outputs_stage_prepare_external_inputs_for_blast_ono
{
    return JGI_SUCCESS;
}


sub run_prepare_external_inputs_for_blast_ono
{
  # create fake meraculous_contigs dir / contigs.fa
  my $function_id = 'prepare_external_inputs_for_blast_ono';
  my ( $pUserParams, $pLocalParams ) = @_;
  my $pLog = get_logger( "main::$function_id" );

  my $assembly_dir = $pLocalParams->get_param( LOCAL_PARAM_assembly_dir, $pLog );
 
  my $externalContigsFile = $pUserParams->get_param( USER_PARAM_external_contigs_file, $pLog );
  run_sys_cmd( $pUserParams, "mkdir $assembly_dir/meraculous_contigs", $pLog );
  run_sys_cmd( $pUserParams, "ln -s $externalContigsFile $assembly_dir/meraculous_contigs/contigs.fa", $pLog );

  return JGI_SUCCESS;
}
sub check_inputs_stage_blast_ono_old
{
	return JGI_SUCCESS;
}
sub check_outputs_stage_blast_ono_old
{
	return JGI_SUCCESS;
}

sub run_blast_ono
{
  my $function_id = 'run_blast_ono';
  my ( $pUserParams, $pLocalParams ) = @_;
  my $pLog = get_logger( "main::blast_ono" );
  
  my $tempfile;
  my $directory_structure = DIR_STRUCTURE_FILENAME;

  my $meraculous_num_linking_pairs_cutoff = $pUserParams->get_param( USER_PARAM_meraculous_num_linking_pairs_cutoff );
  my $meraculous_min_depth_cutoff = $pUserParams->get_param( USER_PARAM_meraculous_min_depth_cutoff );
  if ( 0 == $meraculous_min_depth_cutoff )
  {
      $meraculous_min_depth_cutoff = $pLocalParams->get_param( LOCAL_PARAM_autocalibrated_min_depth_cutoff, $pLog );
  }
  my $assembly_dir = $pLocalParams->get_param( LOCAL_PARAM_assembly_dir, $pLog );
  my $perl         = $pLocalParams->get_param( LOCAL_PARAM_perl,         $pLog );
  my $release_root    = $pUserParams->get_param( USER_PARAM_release_root,    $pLog );

  my $mer_size     = $pUserParams->get_param( USER_PARAM_meraculous_mer_size,     $pLog );
  my $use_cluster = $pUserParams->get_param( USER_PARAM_use_cluster, $pLog );
  my $cluster_queue = $pUserParams->get_param( USER_PARAM_cluster_queue, $pLog );
  my $numProcs = $pUserParams->get_param( USER_PARAM_local_num_procs, $pLog );
  my $numProcsBlastOno = $pUserParams->get_param( USER_PARAM_num_procs_blast_ono, $pLog );
  my $localResumeCheckpoint = $pLocalParams->get_param( LOCAL_PARAM_local_resume_checkpoint, $pLog );
  my $min_contig_size_for_scaffold_links_by_user = $pUserParams->get_param( USER_PARAM_min_contig_size_for_scaffold_links, $pLog );
  my $min_contig_size_for_scaffold_links = $min_contig_size_for_scaffold_links_by_user;

  if ( $numProcsBlastOno == 0 ) { $numProcsBlastOno = $numProcs; }

  my $bRunThoroughBlast = $pUserParams->get_param( USER_PARAM_blast_thoroughly_for_ono, $pLog );

  my $work_dir = "$assembly_dir/blast_ono";
  mkdir $work_dir;

  my $pwd = $ENV{'PWD'};    # save current dir.
  chdir $work_dir;


  my @scarf_files = split( /\s+/,`ls $assembly_dir/meraculous_import/*.scarf.*`);
  chomp @scarf_files;
  my @commands = (); my @stdouts = (); my @stderrs = (); my @workingDirs = ();

  my @fasta_files = split( /\s+/, `ls $work_dir/*.fasta.*` );

  #open lib_info.txt into 2-D array
  my @libInfo = load_all_lib_info( $assembly_dir );

  # initially, we oNo the original contigs, but this will change with each
  #   iteration of oNoing below
  my $remoteContigsFile = "$assembly_dir/meraculous_contigs/contigs.fa";

  my $contigsFile = "$assembly_dir/blast_ono/contigs.fa";

  my $contigEndsFile = "$assembly_dir/blast_ono/contigs.fa.ends";
  run_sys_cmd_failOk(  "ln -s $remoteContigsFile $contigsFile", $pLog );

  my $bowtieBuild =  PATH_BOWTIE_BUILD;
  my $bowtie = PATH_BOWTIE;
  
  foreach my $pInfo ( @libInfo )
  {
    my $contigsFileForBlasting = $contigsFile;
    
	  my ( $libName, $libInsertAvg, $libInsertSdev, $libAvgReadLen, $libIsIlluminaLongInsert, , $libIsOldQual, $libPMin, $libUseForContigs, $libUseForOno ) = @{ $pInfo };

		if ( $bRunThoroughBlast ) 
		{
      # for blast we have 2 different versions of the contigs file
  		$contigsFileForBlasting = "contigs.fa.forBlast";
	  }
    if ( $libInsertAvg == -1 ) { next; } # skip
	  if ( $libUseForOno == 0 ) { next; }

		if ( $g_resuming )
		{
		  if ( !( $localResumeCheckpoint eq $libName ))
		  {
		    next;
	    }
			else
			{
			  # jump to last resume checkpoint
		    goto blast_ono_local_resume_checkpoint;
		  }
	  }
    my $NOut = "perl ".$release_root."/".PATH_NOUT;
    if ( $bRunThoroughBlast )
     {

	# replace lower case letters with n's so blast doesn't try to align
	# we use the other version of the file for the consensus generation
	# this version is just to get blastMap coords
	run_sys_cmd( $pUserParams, "cat $contigsFile | perl -ane 'if ( !( \$_ =~ />/ ) ) { \$_ =~ tr/[a-z]/n/ } print \$_' > $contigsFileForBlasting", $pLog );
    }
    else
    {
	run_sys_cmd( $pUserParams, $bowtieBuild." -f $contigsFile $contigsFile", $pLog );
	run_sys_cmd( $pUserParams, "cat $contigsFile | $NOut 100 > $contigEndsFile", $pLog );
    }

    my $scarf2Fasta = $release_root . PATH_SCARF2FASTA;
    my $divideIt = $release_root . PATH_DIVIDE_IT;
    run_sys_cmd( $pUserParams, "cat $assembly_dir/meraculous_import/$libName.scarf.* | perl $scarf2Fasta $libName > $libName.fasta; perl $divideIt $libName.fasta 500000; rm $libName.fasta", $pLog );

    if ( $bRunThoroughBlast ) { run_sys_cmd( $pUserParams, "/jgi/tools/bin/formatdb -i $contigsFileForBlasting -p F", $pLog ); }
    
blast_ono_local_resume_checkpoint:
    my $x = 0;
    my @fasta_files_for_lib = split( /\s+/, `ls $assembly_dir/blast_ono/$libName.fasta.*` );

    $pLocalParams->put_param( LOCAL_PARAM_local_resume_checkpoint, $libName, $pLog );
		
		my @commands = (); my @stdouts= (); my @stderrs = (); my @workingDirs = ();
    foreach my $file( @fasta_files_for_lib )
    {        
      my $blastView3 = "perl ".$release_root."/".PATH_BLASTVIEW3ILLUMINA;	
      if ( $bRunThoroughBlast )
      {
	# blast all reads vs. contigs: thorough but slow
        my $blastOut = $work_dir."/$libName.$x.blastOut";

        push ( @commands, "/jgi/tools/bin/blastall -b 99 -v 100 -K 100 -p blastn -S 3 -d $contigsFileForBlasting -e 1e-10 -F F -W $mer_size -o $blastOut -i $file; $blastView3 -S $mer_size -b $blastOut -p hits=all,HSPs=best,sequence=0" );
        push ( @stdouts, "$work_dir/blastMap.$libName.$x" );
        push ( @stderrs, "$work_dir/$libName.$x.blastOut.err" );
        push( @workingDirs, "$work_dir" );
      }
      else
      {
        # use speedy bowtie to align most reads, blast for the ends of contigs
        my $screenList = "perl ".$release_root."/".PATH_SCREEN_LIST;
        my $fastaLengths = "perl ".$release_root."/".PATH_FASTALENGTHS;
        my $bowtie2HackedBvp = "perl ".$release_root."/".PATH_BOWTIE2HACKEDBVP;
        my $bv3Switcheroo = "perl ".$release_root."/".PATH_BV3SWITCHEROO;
        my $blastOut = $work_dir."/$libName.$x.blastOut";

        # bowtie-align reads to contigs
        my $cmd = "$bowtie -l $mer_size -k 3 -f $contigsFile --un $libName.$x.bowtie.unaligned -n 0 $file $libName.$x.bowtie; ";
  
        # convert to bvp format
        $cmd .= "$fastaLengths $contigsFile | $bowtie2HackedBvp $libName.$x.bowtie $libAvgReadLen - > $libName.$x.v.contigs.bvp; ";

        # get unaligned reads in fasta format
        my $unalignedReads = "$assembly_dir/blast_ono/$libName.$x.unaligned.fa";
        $cmd .= "grep \">\" $libName.$x.bowtie.unaligned | sed \"s/>//g\" | $screenList - $file 1 > $unalignedReads; ";

        # blast unaligned reads vs. contig ends
        $cmd .= "/jgi/tools/bin/formatdb -i $unalignedReads -p F; ";
        my $contigEndsBlastOut = "contigEnds.v.$libName.$x.unalignedReads.blastOut";
        $cmd .= "/jgi/tools/bin/blastall -b 9999 -v 10000 -K 10000 -p blastn -S 3 -i $contigEndsFile -e 1e-10 -U -F \"m D\" -W $mer_size -o $contigEndsBlastOut -d $unalignedReads; ";

        # convert to bvp
        $cmd .= "$blastView3 -S $mer_size -b $contigEndsBlastOut -p hits=all,HSPs=best,sequence=0  > contigEnds.v.$libName.$x.unalignedReads.bvp; ";
        $cmd .= "$bv3Switcheroo -b contigEnds.v.$libName.$x.unalignedReads.bvp > $libName.$x.unalignedReads.v.contigEnds.bvp; ";

        # combine two alignment results into one
        $cmd .= "cat $libName.$x.v.contigs.bvp $libName.$x.unalignedReads.v.contigEnds.bvp";
 
        push( @commands, $cmd );
        push ( @stdouts, "$work_dir/blastMap.$libName.$x" );
        push ( @stderrs, "$work_dir/$libName.$x.blastMap.err" );
        push( @workingDirs, "$work_dir" );
      }

      $x++;
    }

    run_jazz_command_set( $pUserParams, \@commands, \@stdouts, \@stderrs, \@workingDirs, $pLocalParams, $pLog, 0, $numProcsBlastOno );  
      
    run_sys_cmd( $pUserParams, "cat blastMap.$libName.* | sort -S 2G  -T . -k 2,2 > $libName.blastMap", $pLog );


    my $bGapClose = ( $libInsertAvg < 1000 );
    my $minMatch = $mer_size;

    my $blastMapAnalyzer = $release_root . PATH_BLASTMAPANALYZER;
    my $cmd = "perl $blastMapAnalyzer -b $libName.blastMap -m $minMatch -I ".( $libIsIlluminaLongInsert ? " -L $libInsertAvg " : "" )." > $libName.insertSize.dist";
    run_sys_cmd( $pUserParams, $cmd, $pLog );
    
    my ( @insertDist ) = split( /\n/, `cat $libName.insertSize.dist` );
    my $total = 0;
    my $totalCnt = 0;
    foreach my $x ( @insertDist )
    {
      unless ( $x =~ /\#/ ) 
      {
        my ( $size, $cnt ) = split( /\t/, $x );

        # illumina long insert libs currently show a tail of
        #   mispaired reads below 1Kb in span; we ignore these
        #   in the insert dist calculation, and in the later oNo
        unless ( $libIsIlluminaLongInsert && $size < 1000 )
        {
          $total += $size * $cnt;
          $totalCnt += $cnt;
        }
      }
    }
    if ( $totalCnt == 0 ) 
    {
	$pLog->error( "Couldn't find any candidate inserts in $libName.insertSize.dist" );
	exit_failure();
    }
    my $bootStrappedInsertAvg = $total / $totalCnt;

    $total = 0;  # reuse the counter
    foreach my $x ( @insertDist )
    {
      unless ( $x =~ /\#/ )
      {
        my ( $size, $cnt ) = split( /\t/, $x );
        unless( $libIsIlluminaLongInsert && $size < 1000 )
        {
          $total += abs( $size - $bootStrappedInsertAvg ) * $cnt;
        }
      }
    }
    my $bootStrappedInsertSdev = $total / $totalCnt;

    # build the oNo map

    if ( $min_contig_size_for_scaffold_links_by_user == 0 ) 
    { 
       $min_contig_size_for_scaffold_links = ( $bootStrappedInsertAvg + ( 3 * $bootStrappedInsertSdev ) + 2 * $mer_size ) / 2; 
    }

    run_sys_cmd( $pUserParams, "perl $blastMapAnalyzer -b $libName.blastMap -m $minMatch -i $bootStrappedInsertAvg ".( $libIsIlluminaLongInsert ? " -L ".($min_contig_size_for_scaffold_links)." " : "" )." > $libName.onoMap", $pLog );

    # fix the insert size estimate
    my $insertSizeCorrection = $release_root . PATH_INSERT_SIZE_CORRECTION;
    run_sys_cmd( $pUserParams, "perl $insertSizeCorrection -m $mer_size -i $libName.insertSize.dist > $libName.gapModel", $pLog );       

    # do the oNo

    # couples the linking threshold to the min_depth threshold
    my $minNumLinksToJoin = $meraculous_num_linking_pairs_cutoff ;
    if ( $libPMin > 0 ) { $minNumLinksToJoin = $libPMin; }

    # typically this is way smaller than min_contig_size_for_scaffold_links
    # contigs < this will be ignored by ono2.pl and also not reported in output
    my $min_contig_size_to_ono = ( $libIsIlluminaLongInsert ? 2 * $mer_size : $min_contig_size_for_scaffold_links ); 

    my $ono = $release_root . PATH_ONO2;
    run_sys_cmd( $pUserParams, "perl $ono -c $min_contig_size_to_ono -m $minMatch -b $libName.onoMap -f $contigsFile -i $bootStrappedInsertAvg -g $libName.gapModel -p $minNumLinksToJoin > $libName.scaffolds.fa 2> $libName.scaffolds.err; grep \"^GAP\" $libName.scaffolds.err > $libName.gapFile", $pLog );
    
    if ( $bGapClose )  # close gaps with short-inserts
    {
      # close gaps
      my $scarf_files_wc = "$assembly_dir/meraculous_import/$libName.scarf.*"; 
      my $merauder_min_depth_cutoff = int( $meraculous_min_depth_cutoff / 2 + 0.5 );
      my $merauder = $release_root . PATH_MERAUDER2;
      # XXX - -A causes errors, but is more aggressive, we took it out for the paper
      run_sys_cmd( $pUserParams, "cat $scarf_files_wc | perl $merauder -b $libName.onoMap -g $libName.gapFile -m $mer_size -s $libName.scaffolds.fa -D $merauder_min_depth_cutoff -i $bootStrappedInsertAvg:".int($bootStrappedInsertSdev)." -S - > $libName.scaffolds.gapClosed.fa 2> $libName.scaffolds.gapClosed.err", $pLog );

      # prepare for next iteration
      $contigsFile = "$libName.scaffolds.gapClosed.fa";
    }
    else
    {
      # prepare for next iteration
      $contigsFile = "$libName.scaffolds.fa";
    }

    # mark the output of this round as the current 'final output'
    `rm final.scaffolds.fa; ln -s $contigsFile final.scaffolds.fa`;    

    # cleanup
		 if ( !$bKeepFullOutputs )
		 {
							       run_sys_cmd_failOk( "rm blastMap.$libName.*;  rm *.bvp; rm *bowtie", $pLog );
										       run_sys_cmd_failOk( "rm $libName.fasta.*; rm *.unaligned*; rm *.blastOut", $pLog );
		 }

  }
  
  # create final contigs file
  my $cmd = "perl $release_root/".PATH_SCAFFOLD2CONTIG." final.scaffolds.fa > final.contigs.fa";
  run_sys_cmd( $pUserParams, $cmd, $pLog );
  return( JGI_SUCCESS );
}


sub check_inputs_stage_blast_ono
{
    return JGI_SUCCESS;

}
sub check_outputs_stage_blast_ono
{
    return JGI_SUCCESS;
}

sub check_inputs_stage_merblast_ono
{
    return JGI_SUCCESS;

}
sub check_outputs_stage_merblast_ono
{
    return JGI_SUCCESS;
}


sub run_merblast_ono
{
  my $function_id = 'run_blast_ono';
  my ( $pUserParams, $pLocalParams ) = @_;
  my $pLog = get_logger( "main::merblast_ono" );
  
  my $tempfile;
  my $directory_structure = DIR_STRUCTURE_FILENAME;

  my $meraculous_num_linking_pairs_cutoff = $pUserParams->get_param( USER_PARAM_meraculous_num_linking_pairs_cutoff );
  my $meraculous_min_depth_cutoff = $pUserParams->get_param( USER_PARAM_meraculous_min_depth_cutoff );
  if ( 0 == $meraculous_min_depth_cutoff )
  {
      $meraculous_min_depth_cutoff = $pLocalParams->get_param( LOCAL_PARAM_autocalibrated_min_depth_cutoff, $pLog );
  }
  my $assembly_dir = $pLocalParams->get_param( LOCAL_PARAM_assembly_dir, $pLog );
  my $perl         = $pLocalParams->get_param( LOCAL_PARAM_perl,         $pLog );
  my $release_root    = $pUserParams->get_param( USER_PARAM_release_root,    $pLog );

  my $mer_size     = $pUserParams->get_param( USER_PARAM_meraculous_mer_size,     $pLog );
  my $use_cluster = $pUserParams->get_param( USER_PARAM_use_cluster, $pLog );
  my $cluster_queue = $pUserParams->get_param( USER_PARAM_cluster_queue, $pLog );


  my $bRunThoroughBlast = $pUserParams->get_param( USER_PARAM_blast_thoroughly_for_ono, $pLog );

  my $work_dir = "$assembly_dir/blast_ono";
  mkdir $work_dir;

  my $pwd = $ENV{'PWD'};    # save current dir.
  chdir $work_dir;


  my @scarf_files = split( /\s+/,`ls $assembly_dir/meraculous_import/*.scarf.*`);
  chomp @scarf_files;
  my @commands = (); my @stdouts = (); my @stderrs = (); my @workingDirs = ();
  my @fasta_files = split( /\s+/, `ls $work_dir/*.fasta.*` );

  #open lib_info.txt into 2-D array
  my @libInfo = ();
  open( LIB_FILE, "$assembly_dir/meraculous_import/lib_info_file.txt" );
  while( <LIB_FILE> )
  {
    # expected fields: name, insert_avg, insertsdev, avgReadLen, libIsIlluminaLongInsert
    chomp;
    my @temp = split( /\t/, $_ );
    push @libInfo, \@temp;
  }

  #sort the array based on insert size, ascending
  my @temp = sort { $a->[1] <=> $b->[1] } @libInfo;
  @libInfo = @temp;

  # initially, we oNo the original contigs, but this will change with each
  #   iteration of oNoing below
  my $remoteContigsFile = "$assembly_dir/meraculous_contigs/contigs.fa";

  my $contigsFile = "$assembly_dir/blast_ono/contigs.fa";
  my $contigEndsFile = "$assembly_dir/blast_ono/contigs.fa.ends";
  run_sys_cmd( $pUserParams, "ln -s $remoteContigsFile $contigsFile", $pLog );
  #run_sys_cmd( "/jgi/tools/bin/formatdb -i $contigsFile -p F", $pLog );
  my $bowtieBuild =  PATH_BOWTIE_BUILD;
  my $bowtie = PATH_BOWTIE;
  
  foreach my $pInfo ( @libInfo )
  {
    my ( $libName, $libInsertAvg, $libInsertSdev, $libAvgReadLen, $libIsIlluminaLongInsert ) = @{ $pInfo };

    if ( $libInsertAvg == -1 ) { next; } # skip 

    # number of prefixes == # of processes to run simultaneously
    my @prefixes = compute_dna_prefixes( get_dna_prefix_list_filename( $assembly_dir ), $pLog );

    my $totalReadsInThisLib = 0;
    foreach my $scarfFile ( `ls $assembly_dir/meraculous_import/$libName.scarf.*` )
    {
      chomp $scarfFile;

      # numReads
      my @temp = split( /\s+/, `wc $scarfFile` );       
      $totalReadsInThisLib += $temp[1];
    }
    run_jgi_cmd( "cat $assembly_dir/meraculous_import/$libName.scarf.* > $libName.scarf", $pLog );

    my $readsPerBlock = int( $totalReadsInThisLib / scalar( @prefixes ) ) + 1;

    my @commands = (); my @stdouts= (); my @stderrs = (); my @workingDirs = ();

    my $merBlast = $release_root . PATH_MERBLAST;

    foreach my $prefixPair( @prefixes )
    {        
      my $cmd = "$merBlast $contigsFile $mer_size $libName.scarf $work_dir/unmerged.blastMap.$libName.".$prefixPair->[0]." $readsPerBlock ".$prefixPair->[0]." ".$prefixPair->[1]."\n";
 
      push( @commands, $cmd );
      push ( @stdouts, "/dev/null" );
      push ( @stderrs, "$work_dir/$libName.".$prefixPair->[0].".blastMap.err" );
      push( @workingDirs, "$work_dir" );
    }

    run_jazz_command_set( $pUserParams, \@commands, \@stdouts, \@stderrs, \@workingDirs, $pLocalParams, $pLog, 0 );  

     
    @commands = ();
    @stdouts = ();
    @stderrs= ();
    @workingDirs = ();

    for( my $i = 0; $i < scalar( @prefixes ); $i++ )
    {        
      my $mergeMerBlast = $release_root.PATH_MERGE_MERBLASTS;
      my $cmd = "x=0; cat unmerged.blastMap.$libName.*.$i | sort -T . -S 2G -k 2,2 -k 6,6 -k 7,7n | perl $mergeMerBlast";
 
      push( @commands, $cmd );
      push ( @stdouts, "$work_dir/blastMap.$libName.$i" );
      push ( @stderrs, "$work_dir/err.blastMap.$libName" );
      push( @workingDirs, "$work_dir" );
    }
    run_jazz_command_set( $pUserParams, \@commands, \@stdouts, \@stderrs, \@workingDirs, $pLocalParams, $pLog, 0 );  

    run_sys_cmd( $pUserParams, "cat blastMap.$libName.* | sort -S 2G -T . -k 2,2 > $libName.blastMap", $pLog );

    if ( !$bKeepFullOutputs )
    { 
      run_sys_cmd_failOk( "rm blastMap.$libName.*;  rm *.bvp; rm *bowtie", $pLog );
      run_sys_cmd_failOk( "rm $libName.fasta.*; rm *.unaligned*; rm *.blastOut", $pLog );
    }

    my $bGapClose = ( $libInsertAvg < 1000 );
    my $minMatch = $mer_size;  

    my $blastMapAnalyzer = $release_root . PATH_BLASTMAPANALYZER;
    my $cmd = "perl $blastMapAnalyzer -b $libName.blastMap -m $minMatch -I ".( $libIsIlluminaLongInsert ? " -L $libInsertAvg " : "" )." > $libName.insertSize.dist 2> $libName.blastMapAnalyzer.err";
    run_sys_cmd( $pUserParams, $cmd, $pLog );
    
    my ( @insertDist ) = split( /\n/, `cat $libName.insertSize.dist` );
    my $total = 0;
    my $totalCnt = 0;
    foreach my $x ( @insertDist )
    {
      unless ( $x =~ /\#/ ) 
      {
        my ( $size, $cnt ) = split( /\t/, $x );

        # illumina long insert libs currently show a tail of
        #   mispaired reads below 1Kb in span; we ignore these
        #   in the insert dist calculation, and in the later oNo
        unless ( $libIsIlluminaLongInsert && $size < 1000 )
        {
          $total += $size * $cnt;
          $totalCnt += $cnt;
        }
      }
    }
    if ( $totalCnt == 0 )
    {
	$pLog->error("unable to find any suitable pairs in insert distribution: $libName.insertSize.dist" );
	exit_failure();
    }
    my $bootStrappedInsertAvg = $total / $totalCnt;

    $total = 0;  # reuse the counter
    foreach my $x ( @insertDist )
    {
      unless ( $x =~ /\#/ )
      {        my ( $size, $cnt ) = split( /\t/, $x );
        unless( $libIsIlluminaLongInsert && $size < 1000 )
        {
          $total += abs( $size - $bootStrappedInsertAvg ) * $cnt;
        }
      }
    }
    my $bootStrappedInsertSdev = $total / $totalCnt;

    # build the oNo map
    run_sys_cmd( $pUserParams, "perl $blastMapAnalyzer -b $libName.blastMap -m $minMatch -i $bootStrappedInsertAvg ".( $libIsIlluminaLongInsert ? " -L ".( $libInsertAvg / 2 )." " : "" )." > $libName.onoMap", $pLog );

    # fix the insert size estimate
    my $insertSizeCorrection = $release_root . PATH_INSERT_SIZE_CORRECTION;
    run_sys_cmd( $pUserParams, "perl $insertSizeCorrection -i $bootStrappedInsertAvg -m $minMatch -L $libAvgReadLen -s $bootStrappedInsertSdev > $libName.gapModel", $pLog );       

    # do the oNo

    # couples the linking threshold to the min_depth threshold
    my $minNumLinksToJoin = $meraculous_num_linking_pairs_cutoff ;
    my $ono = $release_root . PATH_ONO2;
    run_sys_cmd( $pUserParams, "perl $ono -m $minMatch -b $libName.onoMap -f $contigsFile -i $bootStrappedInsertAvg -g $libName.gapModel -p $minNumLinksToJoin > $libName.scaffolds.fa 2> $libName.scaffolds.err; grep \"^GAP\" $libName.scaffolds.err > $libName.gapFile", $pLog );
    
    if ( $bGapClose )  # close gaps with short-inserts
    {
      # close gaps
      my $scarf_files_wc = "$assembly_dir/meraculous_import/$libName.scarf.*"; 
      my $merauder_min_depth_cutoff = int( $meraculous_min_depth_cutoff / 2 + 0.5 );
      my $merauder = $release_root . PATH_MERAUDER2;
      # XXX - -A causes errors, but is more aggressive, we took it out for the paper
      run_sys_cmd( $pUserParams, "cat $scarf_files_wc | perl $merauder -b $libName.onoMap -g $libName.gapFile -m $mer_size -s $libName.scaffolds.fa -D $merauder_min_depth_cutoff -i $bootStrappedInsertAvg:".int($bootStrappedInsertSdev)." -S - > $libName.scaffolds.gapClosed.fa 2> $libName.scaffolds.gapClosed.err", $pLog );

      # prepare for next iteration
      $contigsFile = "$libName.scaffolds.gapClosed.fa";
    }
    else
    {
      # prepare for next iteration
      $contigsFile = "$libName.scaffolds.fa";
    }

    # mark the output of this round as the current 'final output'
    `rm final.scaffolds.fa; ln -s $contigsFile final.scaffolds.fa`;    
  }
  
  # create final contigs file
  my $cmd = "perl $release_root/".PATH_SCAFFOLD2CONTIG." final.scaffolds.fa > final.contigs.fa";
  run_sys_cmd( $pUserParams, $cmd, $pLog );
}









# quality range, average, %q20 bases
# return hash:
# - numreads
# - numbases
# - qmax
# - qmin
# - qaverage
# - numQ20bases
sub getScarfInfo
{
   my ( $fh ) = @_;
   my %returnVals = ();

   my $numReads = 0;
   my $numBases = 0;
   my $qMax = -1;
   my $qMin = -1;
   my $numQ20Bases = 0;
   my $qRunningAverage = 0;

   while( my $line = $fh->getline() )
   {
       chomp $line;
       my @a = split( /:/, $line );
       my $qSumForRead = 0;
       my $readLen = length( $a[6] );
       for( my $i=0; $i< $readLen; $i++ )
       {
	   my $q = int( ord( substr( $a[6], $i, 1 ) ) - 64 );
	   $qSumForRead += $q;
	   if ( $qMin == -1 || $q < $qMin ) { $qMin = $q }
	   if ( $qMax == -1 || $q > $qMax ) { $qMax = $q }
	   if ( $q >= 20 ) { $numQ20Bases++ }
       }
       
       # tally
       my $qReadAverage = $qSumForRead / $readLen;
       $numReads++;
       $numBases += $readLen;
       # due to overflow issues, we compute the average like this
       $qRunningAverage =  $qRunningAverage * ( $numBases - $readLen ) / ( $numBases ) + $qReadAverage * ( $readLen / $numBases );
   }

   $returnVals{ "numBases" } = $numBases;
   $returnVals{ "numReads" } = $numReads;
   $returnVals{ "qMin" }  = $qMin;
   $returnVals{ "qMax" }  = $qMax;
   $returnVals{ "numQ20Bases" } = $numQ20Bases;
   $returnVals{ "qAvg" } = $qRunningAverage;

   return( \%returnVals );
}


sub getFastaSizeInfo
{
  my ( $file, $pUserParams, $pLog ) = @_;

  my %results = ();

  my $cnt = `grep -c ">" $file`;
  chomp $cnt;
  my $release_root = $pUserParams->get_param( USER_PARAM_release_root, $pLog );
	my $cmd = "perl ".$release_root."scripts/fastaLengths.pl $file | perl -ane 'BEGIN{ \$x=0 } { \$x += \$F[1] } END { print \$x }'";
	my $totalBases = `$cmd`;
	


  $cmd = "perl ".$release_root."scripts/N50.pl $file 0 | perl -ane 'if ( \$F[4] > 0.5 ) { print \$F[0]."\t".\$F[2]."\t".\$F[3]."\n"; exit }'";
  my @N50 = split( /\t/, `$cmd` );

  my $fastaLengths = $release_root.PATH_FASTA_LENGTHS;
  my $histogram = $release_root.PATH_HISTOGRAM;
  my @rangeData = split( /\s+/, `perl $fastaLengths $file | perl $histogram - 2 10000 | grep Range` );
  my $minSize = $rangeData[ 2 ];
  my $maxSize = $rangeData[ 4 ];
  
  $results{ "cnt" } = $cnt;
  $results{ "totalBases" } = $totalBases;
  $results{ "N50Cnt" } = $N50[ 0 ];
  $results{ "N50Size" } = $N50[ 1 ];
  $results{ "N50TotalBases" } = $N50[ 2 ];
  $results{ "minSize" } = $minSize;
  $results{ "maxSize" } = $maxSize;

  return( \%results );
}

sub check_inputs_stage_meraculous_final_results
{
    return JGI_SUCCESS;
}
sub check_outputs_stage_meraculous_final_results
{
    return JGI_SUCCESS;
}

sub run_meraculous_final_results
{
  my $function_id = 'run_meraculous_final_results';
  my ( $pUserParams, $pLocalParams ) = @_;
  my $pLog = get_logger( "main::meraculous_final_results" );
  my $tempfile;
  my $directory_structure = DIR_STRUCTURE_FILENAME;

  my $assembly_dir = $pLocalParams->get_param( LOCAL_PARAM_assembly_dir, $pLog );
  my $perl         = $pLocalParams->get_param( LOCAL_PARAM_perl,         $pLog );
  my $release_root    = $pUserParams->get_param( USER_PARAM_release_root,    $pLog );
  
  my $work_dir = "$assembly_dir/meraculous_final_results";
  mkdir $work_dir;
  chdir $work_dir;

  # summarize input data
  my $input_dir = "$assembly_dir/meraculous_import";

  open( SUMMARY_REPORT, ">SUMMARY.txt" );
  
  my( @libWildCards, @libNames, @libInsertSizeAvg, @libInsertSizeSdev );
  my ( @libAvgReadLen, @libIsIlluminaLongInsert );
  my ( @libIsOldQualFormat );
  $pUserParams->get_key_values( $pLog, USER_PARAM_lib_seq, \@libWildCards, \@libNames,\@libInsertSizeAvg, \@libInsertSizeSdev, \@libAvgReadLen, \@libIsIlluminaLongInsert, \@libIsOldQualFormat );
  my ( @all_fasta_files );


  print SUMMARY_REPORT "== Key Parameters ==\n";
  print SUMMARY_REPORT "Assembly_dir: ".$assembly_dir."\n";
  print SUMMARY_REPORT "Mer_size: ".$pUserParams->get_param( USER_PARAM_meraculous_mer_size, $pLog )."\n";
  print SUMMARY_REPORT "Min_depth: ".( 0 != $pUserParams->get_param( USER_PARAM_meraculous_min_depth_cutoff, $pLog ) ? $pUserParams->get_param( USER_PARAM_meraculous_min_depth_cutoff, $pLog ) : ( $pLocalParams->get_param( LOCAL_PARAM_autocalibrated_min_depth_cutoff, $pLog ) ) )."\n\n";
  print SUMMARY_REPORT "== Input Data Summary ==\n ";
  print SUMMARY_REPORT "\nlib\tinsertAvg\tnumReads\treadLen\ttotalSeq\tqAvg [ min - max ]\tpctQ20Bases\n";
  for( my $i=0; $i < scalar(@libWildCards); $i++ )
  {
    my $libName = $libNames[ $i ];
    my $fileOrWildcard = $libWildCards[ $i ];

    my $insertAvg = $libInsertSizeAvg[ $i ];
    my $insertSdev = $libInsertSizeSdev[ $i ];
    my $avgReadLen = $libAvgReadLen[ $i ];
    my $isIlluminaLongInsert = $libIsIlluminaLongInsert[ $i ];
    my $isOldQualFormat = $libIsOldQualFormat[ $i ];

    chomp($fileOrWildcard);
    $fileOrWildcard =~ s/^\s+//;
    $fileOrWildcard =~ s/\s+$//;

    my @wildcardSet;
    # is this a wildcard?
    if ( $fileOrWildcard =~ /\*/ )
    {
      @wildcardSet = split( /\s+/, `ls $fileOrWildcard` );
  
      if ( $? )
      {
         die $pLog->error("Cannot open fasta $fileOrWildcard");
      }
    }
    else
    {
        push @wildcardSet, $fileOrWildcard;
    }
    my %scarfInfoHashForLib = ();
    foreach my $scarfFile ( `ls $assembly_dir/meraculous_import/$libName.scarf.*` )
    {
	chomp $scarfFile;

	# numReads
	my @temp = split( /\s+/, `wc $scarfFile` );       
	my $numReads = $temp[1];

	# take a random sample
	$scarfFile =~ /(\S+)\/(\S+)/;
	my $scarfFileNameNoPath = $2;
	my $scarfFileSample = "$assembly_dir/meraculous_final_results/sample.$scarfFileNameNoPath";
	my $desiredSampleSize = 1000000;
	my $sampleRatio = $desiredSampleSize / $numReads;
	if ( $sampleRatio < 1 )
	{
	    if ( JGI_SUCCESS != run_jgi_cmd( "perl ".$release_root.PATH_RANDOM_LIST2." $scarfFile $sampleRatio $desiredSampleSize > $scarfFileSample", $pLog ) ) 
	    {
		return JGI_FAILURE;
	    }
	}
	else
	{
	    # couldn't create sample: use the whole thing ( probably the file was too small )
	    `cp $scarfFile $scarfFileSample`;
	}


	my $fh = FileHandle->new;
	die "couldn't open file $scarfFileSample for input" unless $fh->open( $scarfFileSample );
	my $scarfInfoHashRefForFile = getScarfInfo( $fh );
	# you got the info for a sample, but you need to correct it to project
	#   for the whole file
	

	
	# adjust: numBases
	my $avgReadLen = $scarfInfoHashRefForFile->{ "numBases" } / $scarfInfoHashRefForFile->{ "numReads" }; 
        my $numBases = $numReads * $avgReadLen;

	# adjust: numQ20Bases
	my $numQ20Bases = $scarfInfoHashRefForFile->{ "numQ20Bases" } * $numReads / $scarfInfoHashRefForFile->{ "numReads" };

	$scarfInfoHashRefForFile->{ "numReads" } = $numReads;
	$scarfInfoHashRefForFile->{ "numBases" } = $numBases;
	$scarfInfoHashRefForFile->{ "numQ20Bases" } = $numQ20Bases;

        # collate data
	foreach my $key( keys( %{ $scarfInfoHashRefForFile } ) )
	{
	    my $value = $scarfInfoHashRefForFile->{ $key };
	    # easy case: if empty, populate
	    if ( !exists( $scarfInfoHashForLib{ $key } ) )
	    {
		$scarfInfoHashForLib{ $key } = $value;
		next;
	    }

	    # now, the key is guaranteed to be in the lib hash
	    # so, we must process according to the type of key
	    if ( $key eq "qMin" )
	    {
		if ( $value < $scarfInfoHashForLib{ $key } )
		{
		    $scarfInfoHashForLib{ $key } = $value;
		}
	    }
	    elsif ( $key eq "qMax" )
	    {
		if ( $value > $scarfInfoHashForLib{ $key } )
		{
		    $scarfInfoHashForLib{ $key } = $value;
		}
	    }
	    elsif( $key eq "qAvg" )
	    {
		my $newAvg = $scarfInfoHashForLib{ "qAvg" } * ( $scarfInfoHashForLib{ "numBases" } / ($scarfInfoHashForLib{ "numBases" } + $scarfInfoHashRefForFile->{ "numBase" } ) ) + $value * ( $scarfInfoHashRefForFile->{ "numBases" } / ( $scarfInfoHashForLib{ "numBases" } + $scarfInfoHashRefForFile->{ "numBases" } ) );

		$scarfInfoHashForLib{ $key } = $newAvg;
		    
	    }
	    else
	    {
		# simple summation
		$scarfInfoHashForLib{ $key } += $value;
	    }
	}
    } # ... end of for each scarf file
    
    print SUMMARY_REPORT "$libName\t$insertAvg\t".
	$scarfInfoHashForLib{ "numReads" }."\t".
	sprintf( "%.1f", $scarfInfoHashForLib{ "numBases" } / $scarfInfoHashForLib{ "numReads" } )."\t".
	sprintf( "%.1f", $scarfInfoHashForLib{ "numBases" } / 1000000000 )."Gb\t".
	sprintf( "%.1f", $scarfInfoHashForLib{ "qAvg" } )." [ ".
	$scarfInfoHashForLib{ "qMin" }." - ".
	$scarfInfoHashForLib{ "qMax" }." ]\t".
	sprintf( "%.2f", 100.0 * $scarfInfoHashForLib{ "numQ20Bases" } / $scarfInfoHashForLib{ "numBases" } )."%\n";

    
  } # ... end of for each lib


  # print final scaffold stats
  my $finalScaffoldSizeInfo = getFastaSizeInfo( "$assembly_dir/blast_ono/final.scaffolds.fa", $pUserParams, $pLog );
  $finalScaffoldSizeInfo->{ "label" } = "Final Scaffolds\t";
  my $finalContigSizeInfo = getFastaSizeInfo( "$assembly_dir/blast_ono/final.contigs.fa", $pUserParams, $pLog );
  $finalContigSizeInfo->{ "label" } = "Final Contigs\t";
  my $preGapClosedContigSizeInfo = getFastaSizeInfo( "$assembly_dir/meraculous_contigs/contigs.fa", $pUserParams, $pLog );
  $preGapClosedContigSizeInfo->{ "label" } = "Pre-gapclosed Contigs";

  print SUMMARY_REPORT "\n";
  print SUMMARY_REPORT "== Assembly Stats ===\n";
  print SUMMARY_REPORT "\n";
  print SUMMARY_REPORT "Description     \tcnt\ttotal\tmax\tN50 stats\n";
  print SUMMARY_REPORT "-" x 78;
  print SUMMARY_REPORT "\n";

  foreach my $s ( $finalScaffoldSizeInfo, $finalContigSizeInfo, $preGapClosedContigSizeInfo )
  {
     print SUMMARY_REPORT 
	 $s->{ "label" }."\t".
	 $s->{ "cnt" }."\t".
	 sprintf( "%.1f", $s->{ "totalBases" }/1000000 )."Mb\t".
	 sprintf( "%.1f", $s->{"maxSize"}/1000 )."Kb\t".
	 $s->{ "N50Cnt" }." > ".sprintf( "%.1f", $s->{ "N50Size" } / 1000 ).
	   "Kb totalling ".
	   sprintf( "%.1f", $s->{ "N50TotalBases" } / 1000000 )."Mb\n";
  }
  close SUMMARY_REPORT;
      
  return JGI_SUCCESS;
}

#
# Function: print_jazz_disk_usage
# -------------------------------
# Prints disk space used by the various JAZZ stages, logging in logs/jazz_disk_usage.txt.
#
# This function takes the following arguments:
#
# $_[0]     A string identifying the assembly directory
#
sub print_jazz_disk_usage
{
  my ($assembly_dir) = @_;
  my $destination_file = "$assembly_dir/logs/jazz_disk_usage.txt";
  my @file_list;
  my $directory;

  # source
  @file_list = ("*fasta.screen.*", "*fasta.qual");
  print_disk_usage($destination_file, "$assembly_dir/source", @file_list);

  # brd
  @file_list = ("trim.brd.*", "trim.fasta.*", "[0-9]*.fasta", "[0-9]*.fasta.qual");
  print_disk_usage($destination_file, "$assembly_dir/brd", @file_list);

  # mercounter
  @file_list = ();
  print_disk_usage($destination_file, "$assembly_dir/mercounter", @file_list);

  # graphy
  $directory = "gbi";
  print_disk_usage($destination_file, "$assembly_dir/graphy", $directory);

  # gaps
  @file_list = ("*unhash*");
  print_disk_usage($destination_file, "$assembly_dir/gaps", @file_list);

  # gapmalign
  @file_list = ("fasta.*.*", "gapreads.*.edges", "gapreads.*.gaps.*", "gapreads.*.brd*", "fasta.*.*");
  print_disk_usage($destination_file, "$assembly_dir/gapmalign", @file_list);

  # regraphy
  $directory = "gbi";
  print_disk_usage($destination_file, "$assembly_dir/regraphy", $directory);

  # consensus
  @file_list = ("*.display");
  print_disk_usage($destination_file, "$assembly_dir/consensus", @file_list);

  # assembly_results
  $directory = "gbi";
  print_disk_usage($destination_file, "$assembly_dir/assembly_results", $directory);
}

#
# Function: print_disk_usage
# --------------------------
# Prints disk space of specified files within a specified directory.
#
# This function takes the following arguments:
#
# $_[0]     A string identifying the destination file to print results
# $_[1]     A string identifying the current working directory
# $_[2]     A list of files/directories to get disk usage.  If this is an empty list, will get
#           the disk space of the working directory.
#
sub print_disk_usage
{
  my ($destination_file, $work_dir, @file_list) = @_; 
  my $total_disk_usage = 0;
  my ($disk_usage, $cmd, $output);

  open(DESTINATIONFILE, ">>$destination_file");
  print DESTINATIONFILE "$work_dir:\n";

  # get size of each specified file
  if (@file_list)
  {
    foreach my $file (@file_list)
    {
      $disk_usage = 0;
      if (-e $work_dir)
      {
        # feed 'du' results to sum.pl to get total disk space of all files matching the pattern
        $output = readpipe($cmd);

        # extract 4th word from output of sum.pl and filter out decimal point
        $output = (split(/ /, $output))[4];
        if ($output =~ /(\d+)\.\d*/) 
        {
          $disk_usage = $1;
        }
      }
      print DESTINATIONFILE "\t".$disk_usage."K\t$file\n";
    }
  }

  # get size of whole working directory
  $cmd = "du -ks $work_dir";
  $output = readpipe($cmd);
  if ($output =~ /(\d+)*/) 
  {
    $total_disk_usage = $1;
  }
  print DESTINATIONFILE "\t".$total_disk_usage."K\ttotal\n\n";
  close(DESTINATIONFILE);
}

#
# Function: print_jazz_timing_info
# --------------------------------
# Gathers JAZZ CPU timing info from files.
#
# This function takes the following arguments:
#
# $_[0]     A string identifying the assembly directory
# $_[1]     A list of files to search
#
sub print_jazz_timing_info
{
  my ($stage_name, $assembly_dir, @file_list) = @_;

  # extract and log timing info
  my $pattern          = "JAZZ_TIMING";
  my $destination_file = "$assembly_dir/logs/jazz_timing.txt";

  # print some comments at top of timing log describing the columns
  if (!(-e $destination_file)) 
  {
    open(DESTINATIONFILE, ">>$destination_file");
    print DESTINATIONFILE "# Timing information has the following tab-delimited fields:\n";
    print DESTINATIONFILE "# 1: Stage name\n";
    print DESTINATIONFILE "# 2: Program/Script name\n";
    print DESTINATIONFILE "# 3: Start time with format 'YYYY-MM-DD hh:mi:ss:mls'\n";
    print DESTINATIONFILE "# 4: End time with format 'YYYY-MM-DD hh:mi:ss:mls'\n";
    print DESTINATIONFILE "# 5: Duration of running time in milliseconds\n";
    print DESTINATIONFILE "# 6: Command arguments (if any)\n\n";
    close(DESTINATIONFILE);
  }

     extract_pattern($pattern, $stage_name, $destination_file, @file_list);
}

#
# Function: extract_pattern
# -------------------------
# A general use function to gather lines from files that begin with a header that
# matches the specified pattern.  Can also replace the header with another string.
#
# This function takes the following arguments:
#
# $_[0]     A string identifying the pattern to search file list for 
# $_[1]     A string to replace the pattern with
# $_[2]     A string identifying the file to print results
# $_[3]     A list of files to search
#
sub extract_pattern
{
  my ($pattern_search, $pattern_replace, $destination_file, @file_list) = @_;

  my $num_files = @file_list;
  my $index     = 0;
  while ($index < $num_files) 
  {
    # only send 'cat' 10 files at a time ('cat' silently drops input if too many files)
    my @sub_list;
    if ($index+10 < $num_files) 
    {
      @sub_list = @file_list[$index..$index+9];
    }
    else
    {
      @sub_list = @file_list[$index..$#file_list];
    }
    system("cat @sub_list | grep ^$pattern_search | sed -e 's/$pattern_search/$pattern_replace/' >> $destination_file");
    $index=$index+10;
  }
}

#
# Function: create_subdirectory_structure
# ---------------------------------------
# Creates a partitioned subdirectory structure, integer-indexed with 
# a start index of 0, and returns a hashtable mapping with these
# subdirectories as well as writing the hashtable to a file called
# "directory_structure".
#
# This function takes the following arguments:
#
# $_[0]     A string specifying the top-level directory under which the 
#      partitioned subdirectories will be created.
#
# $_[1]     The number of files to create
#
# $_[2]     [Optional] The number of files to create per subdirectory.  If
#      undefined, the default value of 100 will be used.
#
# $_[3]     A reference to a logging object
#
# $_[4]     A reference to a hashtable will be returned, where the keys are the
#           ID# (file index) and the values are the subdirectories with full
#      path information prepended.
#
# This function returns the following values:
#
# JGI_SUCCESS     The directories were successfully created.
#
# JGI_FAILURE     The directories could not be created.
#
sub create_subdirectory_structure 
{
  my ($work_dir, $total_number_files, $number_files_per_subdir, $log, $directories_ref) = @_;

  my $function_id = "create_subdirectory_structure";
  my $directory_structure = DIR_STRUCTURE_FILENAME;

  if (! -d $work_dir) 
  {
    $log->error( "Directory does not exist! ($work_dir)" );
    return JGI_FAILURE; 
  }

  # default to 100 if files per subdirectory is not specified
  if (! defined($number_files_per_subdir))
  {
    $number_files_per_subdir = 100;
  }
  my $number_of_dir = ceil($total_number_files / $number_files_per_subdir);

  if (create_dir_structure($work_dir, $number_of_dir, $log) == JGI_FAILURE) { return JGI_FAILURE; }

  my $file_id = 0;
  my ($i,$j);
  for ($i=0; $i<$number_of_dir; $i++)
  {
    for ($j=0; $j<$number_files_per_subdir; $j++) 
    {
      $directories_ref->[$file_id] = "$work_dir/$i";
      $file_id ++;
      if ($file_id >= $total_number_files) { 
        last;
      }
    }
  }

  # write the subdirectory structure to a file called directory_structure
  open (DESTINATIONFILE, ">$work_dir/$directory_structure");
  print DESTINATIONFILE Dumper $directories_ref;
  close(DESTINATIONFILE);

  return JGI_SUCCESS;
}

#
# Function: lookup_subdirectory_structure
# ---------------------------------------
# Accesses the "directory_structure" file created by create_subdirectory_structure
# and returns a hashtable reference with the information.
#
# This function takes the following arguments:
#
# $_[0]     A string specifying the top-level directory under which the 
#      partitioned subdirectories lie.
#
# Returns:
#
# A reference to a hashtable, where the keys are the ID# (file index) and the values 
# are the subdirectories with full path information prepended.
#
sub lookup_subdirectory_structure {
  my ($work_dir, $log) = @_;
  my $directory_structure = DIR_STRUCTURE_FILENAME;

  if (! -e "$work_dir/$directory_structure") {
    $log->error("$work_dir/$directory_structure not found!");
    die;
  }

  return do "$work_dir/$directory_structure";
}




