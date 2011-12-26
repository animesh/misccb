#!/util/bin/perl -w
use strict;
use FileHandle;

use vars qw($dataDir
	    $runDir
	    $bHelp
	    $bVHelp
	    $lookalignsDir
	    $chr
	    $outdirBase
	    $startContig
	    $stopContig
	    $bVerboseMR
	    $moduleToRun);

main();

sub PrintOptions
{
  print << "EOF";
Usage: chimp_on_human.pl [options]
Options:
--help                     Display this information.
--what_this_does           What does this script do?
--data=<dir>               Specifiy an Arachne 'DATA' directory.
--run=<dir>                Specifiy an Arachne 'RUN' directory.
--lookaligns_dir=<dir>     Directory with the chimp-reads-on-human files.
--chrm=<chromosome>        Specify a chromosome.
--outdir_base=<string>     Base output-directory name.
--start_contig=<integer>   Start contig for GenerateOnMasterConsensus.
--stop_contig=<integer>    Stop contig for GenerateOnMasterConsensus.
--verbose_mergereads       Verbose logging for MergeReads.
--module_to_run=<option>   If set, run only a specific executable.
EOF
}

sub BigIdea
{
  print  << "EOF";
  
  Run one or all of the following modules:

  GenerateProtoContigs
  ImproveProtoContigs
  GenerateOnMasterConsensus
  ValidateChimpOnHuman
  GenerateOnMasterSupers
 
EOF
}

sub main
{
  autoflush STDOUT 1;
  
  # set our environment variables so we can find our executables
  $ENV{PATH} = "/util/bin:/usr/bin";
  die( "ARACHNE_PRE was not defined in your environment.\n" )
    if ( ! exists( $ENV{'ARACHNE_PRE'} ) );
  
  use File::Path;
  use Getopt::Long;

  $dataDir = "";
  $runDir = "";
  $bHelp = 0;
  $lookalignsDir = "";
  $chr = 0;
  $outdirBase = "";
  $startContig = -1;
  $stopContig = -1;
  $bVerboseMR = 0;
  $moduleToRun = "";

  &GetOptions( "data=s"               => \$dataDir,
	       "run=s"                => \$runDir,
	       "lookaligns_dir=s"     => \$lookalignsDir,
	       "chrm=i"               => \$chr,
	       "help"                 => \$bHelp,
	       "what_this_does"       => \$bVHelp,
	       "outdir_base=s"        => \$outdirBase,
	       "start_contig=i"       => \$startContig,
	       "stop_contig=i"        => \$stopContig,
	       "verbose_mergereads"   => \$bVerboseMR,
	       "module_to_run=s"      => \$moduleToRun );
  
  if ($bHelp) {
    PrintOptions();
    exit;
  }
  
  if ($bVHelp) {
    BigIdea();
    exit;
  }

  if ( $chr == 0 ) {
    print "You must assign a chromosome (use --chrm=<chromosome>)\n";
    exit;
  }

  my $tomakeDir = $ENV{'ARACHNE_PRE'} . "/$dataDir/$runDir/$outdirBase";
  my $strDataRun = "DATA=$dataDir RUN=$runDir";
  my $strLAdir = "LOOKALIGNS_DIR=$lookalignsDir";
  my $strChr = "CHR_ID=$chr";
  my $commonCmd = "$strDataRun $strLAdir $strChr";
  my $validateCmd = "${tomakeDir}/Consensus/validate/on_human.$chr";

  mkpath( $tomakeDir );
  
  my $cmmdGPC = "./GenerateProtoContigs $commonCmd OUTDIR=$outdirBase/ProtoContigs";

  my $cmmdImprove = "./ImproveProtoContigs $commonCmd INDIR=$outdirBase/ProtoContigs OUTDIR=$outdirBase/ImprovedProtoContigs";

  my $cmmdGOMC = "./GenerateOnMasterConsensus $commonCmd INDIR=$outdirBase/ImprovedProtoContigs OUTDIR=$outdirBase/Consensus";

  my $cmmdVCOH = "./ValidateChimpOnHuman $strDataRun SUBDIR=$outdirBase/Consensus AUGMENTED=$lookalignsDir PROBLEMS_ONLY=False MAX_OVERLAP=1000000 FIRST_CHR=$chr LAST_CHR=$chr VERBOSE=True";

  my $cmmdGOMS = "./GenerateOnMasterSupers $strDataRun VALIDATE=$validateCmd SUBDIR=$outdirBase/Consensus CHROMOSOME=$chr OUTDIR=$outdirBase/Supers";

  my @cmmdsToRun;
  if ( $moduleToRun eq "" ) {
    push @cmmdsToRun, $cmmdGPC;
    push @cmmdsToRun, $cmmdImprove;
    push @cmmdsToRun, $cmmdGOMC;
    push @cmmdsToRun, $cmmdVCOH;
    push @cmmdsToRun, $cmmdGOMS;
  }
  elsif ( $moduleToRun =~ m/ProtoContigs/i ) {
    push @cmmdsToRun, $cmmdGPC;
  }
  elsif ( $moduleToRun =~ m/Improve/i ) {
    push @cmmdsToRun, $cmmdImprove;
  }
  elsif ( $moduleToRun =~ m/Consensus/i ) {
    push @cmmdsToRun, $cmmdGOMC;
  }
  elsif ( $moduleToRun =~ m/Validate/i ) {
    push @cmmdsToRun, $cmmdVCOH;
  }
  elsif ( $moduleToRun =~ m/Supers/i ) {
    push @cmmdsToRun, $cmmdGOMS;
  }
  
  my $result;
  foreach my $cmmd (@cmmdsToRun) {
    print "\nRunning:  $cmmd\n";
    $result = system("$cmmd");
    if ($result != 0) {
      system("echo $cmmd terminated with return code = $result" );
    }
  }
  
}
