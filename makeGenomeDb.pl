#!/usr/bin/env perl

# DO NOT EDIT the /cluster/bin/scripts copy of this file -- 
# edit ~/kent/src/hg/utils/automation/makeGenomeDb.pl instead.

# $Id: makeGenomeDb.pl,v 1.2 2006/11/06 21:50:26 angie Exp $

use Getopt::Long;
use warnings;
use strict;
use FindBin qw($Bin);
use lib "$Bin";
use HgAutomate;
use HgRemoteScript;
use HgStepManager;


my $stepper = new HgStepManager(
    [ { name => 'seq',     func => \&makeBuildDir },
      { name => 'agp',     func => \&checkAgp },
      { name => 'db',      func => \&makeDb },
      { name => 'dbDb',    func => \&makeDbDb },
      { name => 'trackDb', func => \&makeTrackDb },
    ]
			       );

my $base = $0;
$base =~ s/^(.*\/)?//;

# Option defaults:
my $dbHost = 'hgwdev';

sub usage {
  # Usage / help / self-documentation:
  my ($status, $detailed) = @_;
  # Basic help (for incorrect usage):
  print STDERR "
usage: $base config.ra
options:
";
  print STDERR $stepper->getOptionHelp();
  print STDERR &HgAutomate::getCommonOptionHelpNoClusters($dbHost,
							  "least loaded");
  print STDERR "
Automates the first steps of building a genome database:
    seq:     Translates fasta into /cluster/data/\$db/\$db.unmasked.2bit .
    agp:     Checks consistency of fasta and AGP (or runs hgFakeAgp).
    db:      Creates the genome database and basic tables.
    dbDb:    Adds the genome database to central database tables.
    trackDb: Creates a trackDb/ checkout and adds -- but does not check in --
             template trackDb.ra and assembly description .html files.
config.ra describes the species, assembly and downloaded files.
To see detailed information about what should appear in config.ra,
run \"$base -help\".
";
  # Detailed help (-help):
  print STDERR "
-----------------------------------------------------------------------------
Required config.ra settings:

db xxxYyyN
  - The UCSC database name and assembly identifier.  xxx is the first three
    letters of the genus and Yyy is the first three letters of the species.
    N is the sequence number of this species' build at UCSC.  For some
    species that we have been processing since before this convention, the
    pattern is xyN.

scientificName Xxxxxx yyyyyy
  - The genus and species name.

assemblyDate Mmm. YYYY
  - The month and year in which this assembly was released by the sequencing
    center.

assemblyLabel XXXXXX
  - The sequencing center's label or version identifier for this release.

orderKey NN
  - A priority number (for the central database's dbDb.orderKey column)
    that will determine db's relative position in the assembly menu.

mitoAcc XXXXXXX
  - A numeric Genbank identifier (\"gi number\") for the complete
    mitochondrial sequence of this species, or \"none\" to disable fetching
    and inclusion in the build.  To determine this, do an Entrez Nucleotide
    query on \"Xxxxxx yyyyyy mitochondrion complete genome\" and choose a
    result that looks best (there may be multiple complete sequences in
    Genbank, or there may be none in which case just say \"none\").

fastaFiles /path/to/downloaded.fa
  - A complete path, possibly with wildcard characters, to FASTA sequence
    files which have already been downloaded from the sequencing center.

dbDbSpeciesDir dirName
  - The name of the subdirectory of trackDb/ in which the \$db subdirectory
    will be added.  For vertebrates, this is often a lower-cased common name,
    but various patterns have been used especially for non-vertebrates.


-----------------------------------------------------------------------------
Conditionally required config.ra settings:

1. Required when AGP is not included in the assembly.  In this case we run
   hgFakeAgp to deduce gap annotations from runs of Ns in the sequence.
   The faGapSizes program shows a histogram that emphasizes overrepresented
   round-number sizes, which are decent hints for these parameters.

fakeAgpMinContigGap NN
  - hgFakeAgp -minContigGap param.  Minimum size for a run of Ns that
    will be considered a bridged gap with type \"contig\".

fakeAgpMinScaffoldGap NN
  - hgFakeAgp -minScaffoldGap param.  Minimum size for a run of Ns that
    will be considered an unbridged gap with type \"scaffold\".


2. Required when the central database table genomeClade does not yet have a
   row for this species:

clade cccccc
  - UCSC's \"clade\" category for this species, as specified in the central
    database tables clade and genomeClade.

genomeCladePriority NN
  - Relative priority of this species compared to others in the same clade.
    This central database query can be helpful in picking a value:
    select * from genomeClade where clade = '\$clade' order by priority;


-----------------------------------------------------------------------------
Optional config.ra settings:

commonName Xxxx
  - Common name to use as the menu label for this species (and in central
    database's genome column) instead of abbreviated scientific name.

agpFiles /path/to/downloaded.agp
  - A complete path, possibly with wildcard characters, to AGP files
    which have already been downloaded from the sequencing center.

qualFiles [/path/to/downloaded.qual | /path/to/qacAgpLift-ed.qac]
  - A complete path, possibly with wildcard characters, to quality score
    files which have already been downloaded from the sequencing center,
    or a complete path to a single .qac file (in case you need to pre-process
    qual files with qaToQac | qacAgpLift, for example).
" if ($detailed);
  print STDERR "\n";
  exit $status;
} # usage


# Globals:
# Command line argument:
my $CONFIG;
# Command line option vars:
use vars @HgAutomate::commonOptionVars;
use vars @HgStepManager::optionVars;
# Required config parameters:
my ($db, $scientificName, $assemblyDate, $assemblyLabel, $orderKey,
    $mitoAcc, $fastaFiles, $dbDbSpeciesDir);
# Conditionally required config parameters:
my ($fakeAgpMinContigGap, $fakeAgpMinScaffoldGap,
    $clade, $genomeCladePriority);
# Optional config parameters:
my ($commonName, $agpFiles, $qualFiles);
# Other globals:
my ($gotMito, $gotAgp, $gotQual, $topDir, $chromBased);
my ($bedDir, $scriptDir, $endNotes);

sub checkOptions {
  # Make sure command line options are valid/supported.
  my $ok = GetOptions(@HgStepManager::optionSpec,
		      @HgAutomate::commonOptionSpec,
		     );
  &usage(1) if (!$ok);
  &usage(0, 1) if ($opt_help);
  &HgAutomate::processCommonOptions();
  my $err = $stepper->processOptions();
  usage(1) if ($err);
  $dbHost = $opt_dbHost if (defined $opt_dbHost);
} # checkOptions

sub requireVar {
  # Ensure that var is in %config and return its value.
  # Remove it from %config so we can check for unrecognized contents.
  my ($var, $config) = @_;
  my $val = $config->{$var}
    || die "Error: $CONFIG is missing required variable \"$var\".\n" .
      "For a detailed list of required variables, run \"$base -help\".\n";
  delete $config->{$var};
  return $val;
} # requireVar

sub optionalVar {
  # If var has a value in %config, return it.
  # Remove it from %config so we can check for unrecognized contents.
  my ($var, $config) = @_;
  my $val = $config->{$var};
  delete $config->{$var} if ($val);
  return $val;
} # optionalVar

sub parseConfig {
  # Parse config.ra file, make sure it contains the required variables.
  my ($configFile) = @_;
  my %config = ();
  my $fh = &HgAutomate::mustOpen($configFile);
  while (<$fh>) {
    next if (/^\s*#/ || /^\s*$/);
    if (/^\s*(\w+)\s*(.*)$/) {
      my ($var, $val) = ($1, $2);
      if (! exists $config{$var}) {
	$config{$var} = $val;
      } else {
	die "Duplicate definition for $var line $. of config file $configFile.\n";
      }
    } else {
      die "Can't parse line $. of config file $configFile:\n$_\n";
    }
  }
  close($fh);
  # Required variables.
  $db = &requireVar('db', \%config);
  $scientificName = &requireVar('scientificName', \%config);
  $assemblyDate = &requireVar('assemblyDate', \%config);
  $assemblyLabel = &requireVar('assemblyLabel', \%config);
  $orderKey = &requireVar('orderKey', \%config);
  $mitoAcc = &requireVar('mitoAcc', \%config);
  $fastaFiles = &requireVar('fastaFiles', \%config);
  $dbDbSpeciesDir = &requireVar('dbDbSpeciesDir', \%config);
  # Conditionally required variables -- optional here, but they might be
  # required later on in some cases.
  $fakeAgpMinContigGap = &optionalVar('fakeAgpMinContigGap', \%config);
  $fakeAgpMinScaffoldGap = &optionalVar('fakeAgpMinScaffoldGap', \%config);
  $clade = &optionalVar('clade', \%config);
  $genomeCladePriority = &optionalVar('genomeCladePriority', \%config);
  # Optional variables.
  $commonName = &optionalVar('commonName', \%config);
  $agpFiles = &optionalVar('agpFiles', \%config);
  $qualFiles = &optionalVar('qualFiles', \%config);
  # Make sure no unrecognized variables were given.
  my @stragglers = sort keys %config;
  if (scalar(@stragglers) > 0) {
    die "Error: config file $CONFIG has unrecognized variables:\n" .
      "    " . join(", ", @stragglers) . "\n" .
      "For a detailed list of supported variables, run \"$base -help\".\n";
  }
  $gotMito = ($mitoAcc ne 'none');
  $gotAgp = (defined $agpFiles);
  $gotQual = (defined $qualFiles);
  $topDir = "/cluster/data/$db";
} # parseConfig


#########################################################################
# * step: seq [workhorse]

sub mkClusterDataLink {
  # Make a /cluster/data/$tDb/ -> /cluster/store?/$tDb/ if it doesn't exist
  if (! -e "$topDir") {
    my $clusterStore = &HgAutomate::choosePermanentStorage();
    &HgAutomate::mustMkdir("$clusterStore/$db");
    # Don't use HgAutomate::run because this needs to happen despite -debug:
    (system("ln -s $clusterStore/$db $topDir") == 0)
      || die "Couldn't ln -s $clusterStore/$db $topDir\n";
  }
} # mkClusterDataLink

sub getMito {
  # Get the mitochondrion from Genbank (if specified) and rename it chrM
  if ($gotMito) {
    my $whatItDoes =
	"It fetches mitochondrial sequence from GenBank and renames it chrM.";
    # Use $dbHost because this needs to wget and some of our workhorses
    # can't do that, and this is computationally cheap.
    my $bossScript = new HgRemoteScript("$scriptDir/getMito.csh",
					$dbHost, $topDir, $whatItDoes,
					$CONFIG);
    my $mitoFile = "$topDir/M/$mitoAcc.fa";
    $bossScript->add(<<_EOF_
mkdir M
wget -O $mitoFile \\
  'http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?cmd=Text&db=Nucleotide&uid=$mitoAcc&dopt=FASTA'

# Make sure there's exactly one fasta record:
if (`grep '^>' $mitoFile | wc -l` != 1) then
  echo "getMito: $mitoFile"
  echo "         does not have exactly one fasta record"
  exit 1
endif

# Make sure what we got is of about the right size:
set mSize = `faSize $mitoFile | awk '{print \$1;}'`
if (\$mSize < 10000 || \$mSize > 25000) then
  echo "getMito: $mitoFile"
  echo "         fasta size \$mSize is out of expected range [10000, 25000]"
  exit 1
endif

# Make sure the fasta header looks plausible:
set keyCount = 0
set header = `grep '^>' $mitoFile`
foreach keyword ('$scientificName' mitochondr complete genome)
  set count = `echo \$header | grep -i "\$keyword" | wc -l`
  set keyCount = `expr \$keyCount + \$count`
end
if (\$keyCount < 2) then
  echo "getMito: $mitoFile"
  echo "         fasta header does not have very many expected keywords"
  echo "         (looking for '$scientificName' mitochondr complete genome)"
  echo "         Here is the header:"
  echo \$header
  exit 1
endif

# Make chrM.fa with >chrM:
sed -e 's/^>.*/>chrM/' $mitoFile > $topDir/M/chrM.fa
rm $mitoFile
_EOF_
    );
    $bossScript->execute();
  }
} # getMito

sub makeUnmasked2bit {
  my ($workhorse) = @_;
  my $whatItDoes =
"It installs assembly fasta (and agp, if given) files in the usual dirs.";
  my $bossScript = new HgRemoteScript("$scriptDir/makeUnmasked2bit.csh",
				      $workhorse, $topDir, $whatItDoes,
				      $CONFIG);

  my $chrM = $gotMito ? "$topDir/M/chrM.fa" : "";
  # Forget the fasta, just make an unmasked 2bit for now!
  # If we really need a fasta hierarchy, we can make it later, after
  # RepeatMasking!!  The whole point of the thing is to provide a
  # structure for the RM run and subsequent masking of the sequence.
  # But the RM+masking task can structure itself -- it can simply make
  # masked .2bit, and then subsequent jobs can do their own split and
  # distribute if necessary (or better yet just use 2bit specs).

  # possibilities for $fastaFiles:
  # {.fa, .fa.gz}
  # {many single-fasta, one multi-fasta, several multi-fasta}

  # If AGP is given:
  # 1. if first column of AGP matches sequence names, use as-is.
  # 2. if sixth column of AGP matches seqnames, assemble with agpToFa.

  # installation options:
  # 1. chrom-based (numSeqs <= 100): directories containing single-fasta,
  #    per-sequence files; randoms lumped into same directories with reals.
  # 2. scaffold-based: scaffolds.fa
  # Traditionally we have split to 5M in prep for RM -- but let the RM take
  # care of its own splitting (better yet, make it use 2bit specs).

  if ($gotAgp) {
    $bossScript->add(<<_EOF_
# Get sorted IDs from fasta sequence files:
if (`ls $fastaFiles | grep \.gz | wc -l`) then
  set fcat = zcat
else
  set fcat = cat
endif
set fastaIds = `mktemp -p /tmp makeGenomeDb.fastaIds.XXXXXX`
\$fcat $fastaFiles | grep '^>' | perl -wpe 's/^>(\\S+).*/\$1/' | sort \\
  > \$fastaIds

# Get sorted "big" (1st column) and "little" (6th column) IDs from AGP files:
if (`ls $agpFiles | grep \.gz | wc -l`) then
  set acat = zcat
else
  set acat = cat
endif
set agpBigIds = `mktemp -p /tmp makeGenomeDb.agpIds.XXXXXX`
\$acat $agpFiles | awk '{print \$1;}' | sort -u \\
  > \$agpBigIds
set agpLittleIds = `mktemp -p /tmp makeGenomeDb.agpIds.XXXXXX`
\$acat $agpFiles | awk '\$5 != "N" {print \$6;}' | sort -u \\
  > \$agpLittleIds

# Compare fasta IDs to first and sixth columns of AGP:
set diffBigCount = `comm -3 \$fastaIds \$agpBigIds | wc -l`
set diffLittleCount = `comm -3 \$fastaIds \$agpLittleIds | wc -l`

# If AGP "big" IDs match sequence IDs, use sequence as-is.
# If AGP "little" IDs match sequence IDs, assemble sequence with agpToFa.
if (\$diffLittleCount == 0) then
  set agpTmp = `mktemp -p /tmp makeGenomeDb.agp.XXXXXX`
  \$acat $agpFiles > \$agpTmp
  \$fcat $fastaFiles \\
  | agpToFa -simpleMultiMixed \$agpTmp all stdout stdin \\
  | faToTwoBit stdin $chrM $db.unmasked.2bit
  rm -f \$agpTmp
else if (\$diffBigCount == 0) then
  faToTwoBit $fastaFiles $chrM $db.unmasked.2bit
else
  echo "Error: IDs in fastaFiles ($fastaFiles)"
  echo "do not perfectly match IDs in either the first or sixth columns of"
  echo "agpFiles ($agpFiles)"
  echo "Please examine and then remove these temporary files:"
  echo "  \$fastaIds -- fasta IDs"
  echo "  \$agpBigIds -- AGP first column IDs ('big' sequences)"
  echo "  \$agpLittleIds -- AGP sixth column IDs ('little' sequences)"
  exit 1
endif
rm -f \$fastaIds \$agpBigIds \$agpLittleIds
_EOF_
    );
  } else {
    # No AGP -- just make an unmasked 2bit.
    $bossScript->add(<<_EOF_
faToTwoBit $fastaFiles $chrM $db.unmasked.2bit
_EOF_
    );
  }

  # Having made the unmasked .2bit, make chrom.sizes and chromInfo.tab:
  $bossScript->add(<<_EOF_

twoBitInfo $db.unmasked.2bit chrom.sizes

rm -rf $HgAutomate::trackBuild/chromInfo
mkdir $HgAutomate::trackBuild/chromInfo
awk '{print \$1 "\t" \$2 "\t$HgAutomate::gbdb/$db/$db.2bit";}' chrom.sizes \\
  > $HgAutomate::trackBuild/chromInfo/chromInfo.tab

if (`wc -l < chrom.sizes` < 1000) then
  # Install per-chrom .agp files for download.
  \$acat $agpFiles \\
  | splitFileByColumn -col=1 -ending=.agp stdin $topDir -chromDirs
endif
_EOF_
    );
  $bossScript->execute();

  # Now that we have created chrom.sizes (unless we're in -debug mode),
  # re-evaluate $chromBased for subsequent steps:
  if (-e "$topDir/chrom.sizes") {
    $chromBased = (`wc -l < $topDir/chrom.sizes` < $HgAutomate::splitThreshold);
  }
} # makeUnmasked2bit

sub makeBuildDir {
  # * step: seq [workhorse]
  &mkClusterDataLink();
  &HgAutomate::checkCleanSlate('seq', 'agp',
			       "$topDir/M", "$topDir/chrom.sizes");
  &HgAutomate::mustMkdir($scriptDir);
  &HgAutomate::mustMkdir($bedDir);
  my $workhorse = &HgAutomate::chooseWorkhorse();
  &getMito();
  &makeUnmasked2bit($workhorse);
} # makeBuildDir


#########################################################################
# * step: agp [workhorse]

sub requireFakeAgpParams {
  # When assembly does not include AGP, run hgFakeAgp and require the
  # developer to specify its parameters.  If not specified, run faGapSizes
  # to give some hints.
  my $problem = 0;
  if (! defined $fakeAgpMinContigGap) {
    warn "Error: $CONFIG is missing required variable " .
      "\"fakeAgpMinContigGap\".\n";
    $problem = 1;
  }
  if (! defined $fakeAgpMinScaffoldGap) {
    warn "Error: $CONFIG is missing required variable " .
      "\"fakeAgpMinScaffoldGap\".\n";
    $problem = 1;
  }
  if ($problem) {
    warn "\nWhen the assembly does not include AGP, $CONFIG must specify " .
      "fakeAgpMinContigGap and fakeAgpMinScaffoldGap.\n";
    warn "Printing a gap-size histogram from faGapSizes to stdout... " .
      "Check for overrepresented round numbers.\n";
    warn "See usage messages of hgFakeAgp and faGapSizes for more hints.\n\n";
    my $fileServer = &HgAutomate::chooseFileServer($topDir);
    print "\n";
    &HgAutomate::run("ssh -x $fileServer nice " .
		     "twoBitToFa $topDir/$db.unmasked.2bit stdout " .
		     "\\| faGapSizes stdin -niceSizes=" .
		     "10,20,25,50,100,1000,2000,5000,10000,20000,50000");
    print "\n";
    exit 1;
    }
}

sub checkAgp {
  my $workhorse = &HgAutomate::chooseWorkhorse();
  # Check or generate AGP, depending on whether it was provided:
  if ($gotAgp) {
    &HgAutomate::checkCleanSlate('agp', 'db', "$topDir/$db.agp");
    my $whatItDoes = "It checks consistency of AGP and fasta.";
    my $bossScript = new HgRemoteScript("$scriptDir/checkAgpAndFa.csh",
					$workhorse, $topDir, $whatItDoes,
					$CONFIG);
    my $allAgp = "$topDir/$db.agp";
    # If we added chrM from GenBank, exclude it from fasta:
    my $exclude = ($mitoAcc eq 'none') ? "" : "-exclude=chrM";
    $bossScript->add(<<_EOF_
# When per-chrom AGP and fasta files are given, it would be much more
# efficient to run this one chrom at a time.  However, since the filenames
# are arbitrary, I'm not sure how to identify the pairings of AGP and fa
# files.  So cat 'em all together and check everything at once:

if (`ls $agpFiles | grep \.gz | wc -l`) then
  set acat = zcat
else
  set acat = cat
endif
\$acat $agpFiles | sort -k1,1 -k2n,2n > $allAgp

set result = `checkAgpAndFa $exclude $allAgp $db.unmasked.2bit | tail -1`

if ("\$result" != 'All AGP and FASTA entries agree - both files are valid') then
  echo "Error: checkAgpAndFa failed\\!"
  echo "Last line of output: \$result"
  exit 1
endif
_EOF_
    );
    $bossScript->execute();
  } else {
    my $runDir = "$bedDir/hgFakeAgp";
    &HgAutomate::mustMkdir($runDir);
    &HgAutomate::checkCleanSlate('agp', 'db', "$runDir/$db.agp");
    &requireFakeAgpParams();
    my $whatItDoes = "It runs hgFakeAgp (because no AGP was provided).";
    my $bossScript = new HgRemoteScript("$runDir/doFakeAgp.csh",
					$workhorse, $runDir, $whatItDoes,
					$CONFIG);
    $bossScript->add(<<_EOF_
twoBitToFa $topDir/$db.unmasked.2bit stdout \\
| hgFakeAgp stdin $db.agp \\
  -minContigGap=$fakeAgpMinContigGap -minScaffoldGap=$fakeAgpMinScaffoldGap
_EOF_
    );
    $bossScript->execute();
  }

#*** Produce a gap report so we can say something sensible in gap.html.

} # checkAgp


#########################################################################
# * step: db [dbHost]

sub makeDb {
  # Create a database on hgwdev, grp, chromInfo, gold/gap.
  my $qual = (defined $qualFiles) ? " qual" : "";
  my $whatItDoes =
"It creates the genome database ($db) and loads the most basic tables:
chromInfo, grp, gap, gold,$qual and gc5Base.";
  my $bossScript = new HgRemoteScript("$scriptDir/makeDb.csh", $dbHost,
				      $topDir, $whatItDoes, $CONFIG);

  # Actually, build some basic track files on $workhorse, then load.
  $qual = (defined $qualFiles) ? " qual and" : "";
  $whatItDoes = "It generates$qual gc5Base track files for loading.";
  my $workhorse = &HgAutomate::chooseWorkhorse();
  my $horseScript = new HgRemoteScript("$scriptDir/makeTrackFiles.csh",
				       $workhorse,
				       $topDir, $whatItDoes, $CONFIG);
  # Build quality wiggle track files (if provided).
  if (defined $qualFiles) {
    $horseScript->add(<<_EOF_
# Translate qual files to wiggle encoding.  If there is a problem with
# sequence names, you may need to tweak the original qual sequence names
# and/or lift using qacAgpLft.
mkdir -p $bedDir/qual
cd $bedDir/qual
_EOF_
      );
    if ($qualFiles =~ /^\S+\.qac$/) {
      # Single .qac file:
      $horseScript->add(<<_EOF_
qacToWig -fixed $qualFiles stdout \\
_EOF_
        );
    } else {
      # Possible wildcard of qual file(s):
      $horseScript->add(<<_EOF_
if (`ls $qualFiles | grep \.gz | wc -l`) then
  set qcat = zcat
else
  set qcat = cat
endif
\$qcat $qualFiles \\
| qaToQac stdin stdout \\
| qacToWig -fixed stdin stdout \\
_EOF_
      );
    }
    $horseScript->add(<<_EOF_
| wigEncode stdin qual.{wig,wib}

_EOF_
    );
  }

  # Build gc5Base files.
  $horseScript->add(<<_EOF_
# Make gc5Base wiggle files.
mkdir -p $bedDir/gc5Base
cd $bedDir/gc5Base
hgGcPercent -wigOut -doGaps -file=stdout -win=5 -verbose=0 $db \\
  $topDir/$db.unmasked.2bit \\
| wigEncode stdin gc5Base.{wig,wib}
_EOF_
  );

  # Now start the database creation and loading commands.
  $bossScript->add (<<_EOF_
hgsql '' -e 'create database $db'
df -h /var/lib/mysql
hgsql $db < \${HOME}/kent/src/hg/lib/grp.sql
hgLoadSqlTab $db chromInfo \${HOME}/kent/src/hg/lib/chromInfo.sql \\
  $HgAutomate::trackBuild/chromInfo/chromInfo.tab
_EOF_
  );

  my $allAgp = "$topDir/$db.agp";
  $allAgp = "$bedDir/hgFakeAgp/$db.agp" if (! $gotAgp);
  if ($chromBased) {
    $bossScript->add(<<_EOF_
# Split AGP into per-chrom files/dirs so we can load split gold and gap tables.
cp /dev/null chrom.lst.tmp
foreach chr (`awk '{print \$1;}' chrom.sizes`)
  set c = `echo \$chr | sed -e 's/^chr//; s/_random\$//;'`
  mkdir -p \$c
  echo \$c >> chrom.lst.tmp
  awk '\$1 == "'\$chr'" {print;}' $allAgp > \$c/\$chr.agp
end
sort -u chrom.lst.tmp > chrom.lst
rm chrom.lst.tmp
hgGoldGapGl -noGl -chromLst=chrom.lst $db $topDir .
_EOF_
    );
  } else {
    $bossScript->add("hgGoldGapGl -noGl $db $allAgp\n");
  }

  $bossScript->add(<<_EOF_

# Load gc5base
mkdir -p $HgAutomate::gbdb/$db/wib
rm -f $HgAutomate::gbdb/$db/wib/gc5Base.wib
ln -s $bedDir/gc5Base/gc5Base.wib $HgAutomate::gbdb/$db/wib
hgLoadWiggle -pathPrefix=$HgAutomate::gbdb/$db/wib \\
  $db gc5Base $bedDir/gc5Base/gc5Base.wig
_EOF_
  );
  if (defined $qualFiles) {
    $bossScript->add(<<_EOF_

# Load qual
cd $bedDir/qual
rm -f $HgAutomate::gbdb/$db/wib/qual.wib
ln -s $bedDir/qual/qual.wib $HgAutomate::gbdb/$db/wib/
hgLoadWiggle -pathPrefix=$HgAutomate::gbdb/$db/wib \\
  $db quality qual.wig
_EOF_
    );
  }

  $bossScript->add(<<_EOF_
rm -f wiggle.tab

_EOF_
  );
  $horseScript->execute();
  $bossScript->execute();
} # makeDb


#########################################################################
# * step: dbDb [dbHost]

sub requireCladeAndPriority {
  # If the genomeClade table in the central database does not have a row
  # for this $db's genome, require user to specify clade and priority.
  my ($genome) = @_;
  my $problem = 0;
  if (! defined $clade) {
    warn "Error: $CONFIG is missing required variable " .
      "\"clade\".\n";
    $problem = 1;
  }
  if (! defined $genomeCladePriority) {
    warn "Error: $CONFIG is missing required variable " .
      "\"genomeCladePriority\".\n";
    $problem = 1;
  }
  if ($problem) {
    warn "\nCentral database table genomeClade does not have a row for " .
      "${db}'s genome \"$genome\",\n";
    warn "so $CONFIG must specify clade and genomeCladePriority.\n";
    warn "Examine the contents of the genomeClade table to get an idea " .
      "where \"$genome\" fits in.\n\n";
    exit 1;
  }
}

sub getGenome {
  # Return what we'll use in the genome (and organism) column of dbDb.
  # This is the label that appears in the gateway menu.
  my $genome;
  if ($commonName) {
    $genome = $commonName;
  } else {
    $genome = $scientificName;
    $genome =~ s/^(\w)\w+\s+(\w+)$/$1. $2/;
  }
  return $genome;
}

sub makeDbDb {
# * step: dbDb [dbHost... or just direct to hgcentraltest??]
# - create hgcentraltest entry, (if necessary) defaultDb and genomeClade
  my $genome = &getGenome();
  my $defaultPos;
  my ($seq, $size) = $opt_debug ? ("chr1", 1000) :
    split(/\s/, `head -1 "$topDir/chrom.sizes"`);
  my $start = int($size / 2);
  $size = ($start + 9999) if ($size > ($start + 9999));
  $defaultPos = "$seq:$start-$size";

  my $dbDbInsert = "$topDir/dbDbInsert.sql";
  my $fh = &HgAutomate::mustOpen(">$dbDbInsert");
  print $fh <<_EOF_
DELETE from dbDb where name = "$db";
INSERT INTO dbDb
    (name, description, nibPath, organism,
     defaultPos, active, orderKey, genome, scientificName,
     htmlPath, hgNearOk, hgPbOk, sourceName)
VALUES
    ("$db", "$assemblyDate", "$HgAutomate::gbdb/$db", "$genome",
     "$defaultPos", 1, $orderKey, "$genome", "$scientificName",
     "$HgAutomate::gbdb/$db/html/description.html", 0, 0, "$assemblyLabel");
_EOF_
  ;
  close($fh);
  my $centDbSql = "ssh -x $dbHost $HgAutomate::centralDbSql";
  &HgAutomate::run("$centDbSql < $dbDbInsert");

  # Add a row to defaultDb if this is the first usage of $genome.
  my $sql = "'select count(*) from defaultDb where genome = \"$genome\"'";
  if (`echo $sql | $centDbSql` == 0) {
    $sql = "'INSERT INTO defaultDb (genome, name) " .
      "VALUES (\"$genome\", \"$db\")'";
    &HgAutomate::run("echo $sql | $centDbSql");
  }

  # If $genome does not already appear in genomeClade, warn user that
  # they will have to manually add it.
  $sql = "'select count(*) from genomeClade where genome = \"$genome\"'";
  if (`echo $sql | $centDbSql` == 0) {
    &requireCladeAndPriority($genome);
    $sql = "'INSERT INTO genomeClade (genome, clade, priority) " .
      "VALUES (\"$genome\", \"$clade\", $genomeCladePriority)'";
    &HgAutomate::run("echo $sql | $centDbSql");
  }
} # makeDbDb


#########################################################################
# * step: trackDb [dbHost]

sub makeDescription {
  # Make a template description.html (for the browser gateway page).
  my $sciUnderscore = $scientificName;
  $sciUnderscore =~ s/ /_/g;
  my $genome = &getGenome();
  my $anchorRoot = lc($genome);
  $anchorRoot =~ s/\. /_/;
  $anchorRoot =~ s/ /_/;
  my $fh = &HgAutomate::mustOpen(">$topDir/html/description.html");
  print $fh <<_EOF_
<!-- Display image in righthand corner -->
<TABLE ALIGN=RIGHT BORDER=0 WIDTH=185>
  <TR><TD ALIGN=RIGHT>
    <A HREF="*** link to sequencing center's project page"
    TARGET=_blank>
    <IMG SRC="/images/$sciUnderscore.jpg" WIDTH=*** HEIGHT=*** ALT="$genome"></A>
  </TD></TR>
  <TR><TD ALIGN=RIGHT> 
    <FONT SIZE=-1><em>$scientificName</em><BR> 
    </FONT>
    <FONT SIZE=-2> 
      Photo courtesy of ***
      (<A HREF="*** link to source" 
      TARGET=_blank>***source</A>)
    </FONT>
  </TD></TR>
</TABLE>

<P>
The $assemblyDate <em>$scientificName</em> draft assembly 
($assemblyLabel) was produced by the *** sequencing center.

<H3>Sample position queries</H3>
<P>
A genome position can be specified by the accession number of a
sequenced genomic region, an mRNA or EST, a chromosomal coordinate
range, or keywords from the GenBank description of an mRNA. The
following list shows examples of valid position queries for the
$genome genome.  See the
<A HREF="../goldenPath/help/hgTracksHelp.html"
    TARGET=_blank>User's Guide</A> for more information.
<P>
<TABLE border=0 CELLPADDING=0 CELLSPACING=0>
    <TR><TH ALIGN=LEFT>Request:</TH>
        <TH WIDTH=14>&nbsp;</TH>
       	<TH ALIGN=LEFT>Genome Browser Response:</TH></TR>
    <TR><TD VALIGN=Top COLSPAN=3>&nbsp;</TD></TR>
    <TR><TD VALIGN=Top>chrZ</TD>
        <TD WIDTH=14>&nbsp;</TD>
       	<TD VALIGN=Top>Displays all of chromosome Z</TD>
    </TR>
    <TR><TD VALIGN=Top>chr3:1-1000000</TD>
        <TD WIDTH=14>&nbsp;</TD>
       	<TD VALIGN=Top>Displays first million bases of chr 3</TD>
    </TR>
    <TR><TD VALIGN=Top NOWRAP>chr3:1000000+2000</TD>
       	<TD WIDTH=14></TD>
       	<TD VALIGN=Top>Displays a region of chr 3 that spans 2000 bases, starting with position 1000000 </TD>
    </TR>
    <TR><TD VALIGN=Top COLSPAN=3>&nbsp;<br></TD></TR>
    <TR><TD VALIGN=Top>BX950328</TD>
        <TD WIDTH=14>&nbsp;</TD>
       	<TD VALIGN=Top>Displays region of mRNA with GenBank accession
		number BX950328</TD>
    </TR>
    <TR><TD VALIGN=Top>BU335953</TD>
        <TD WIDTH=14>&nbsp;</TD>
      	<TD VALIGN=Top>Displays region of EST with GenBank accession
		BU335953 on chr Z</TD>
    </TR>
    <TR><TD VALIGN=Top COLSPAN=3>&nbsp;<br></TD></TR>
    <TR><TD VALIGN=Top>pseudogene mRNA</TD>
        <TD WIDTH=14>&nbsp;</TD>
       	<TD VALIGN=Top>Lists transcribed pseudogenes, but not cDNAs</TD>
    </TR>
    <TR><TD VALIGN=Top>homeobox caudal</TD>
        <TD WIDTH=14>&nbsp;</TD>
       	<TD VALIGN=Top>Lists mRNAs for caudal homeobox genes</TD>
    </TR>
    <TR><TD VALIGN=Top>zinc finger</TD>
        <TD WIDTH=14>&nbsp;</TD>
       	<TD VALIGN=Top>Lists many zinc finger mRNAs</TD>
    </TR>
    <TR><TD VALIGN=Top>kruppel zinc finger</TD>
        <TD WIDTH=14>&nbsp;</TD>
       	<TD VALIGN=Top>Lists only kruppel-like zinc fingers</TD>
    </TR>
    <TR><TD VALIGN=Top>zhang</TD>
        <TD WIDTH=14>&nbsp;</TD>
	<TD VALIGN=Top>Lists mRNAs deposited by scientist named Zhang</TD>
    </TR>
    <TR><TD VALIGN=Top>Taguchi,T.</TD>
        <TD WIDTH=14>&nbsp;</TD>
       	<TD VALIGN=Top>Lists mRNAs deposited by co-author T. Taguchi</TD>
    </TR>
    <TR><TD VALIGN=Top COLSPAN=3>&nbsp;</TD></TR>
    <TR><TD COLSPAN="3"> Use this last format for author queries. Although
	GenBank requires the search format <em>Taguchi T</em>, internally it
	uses the format <em>Taguchi,T.</em>.</TD>
    </TR>
</TABLE>

<HR>
<H3>Assembly details</H3>
<P>
The genome has been sequenced to ***X coverage.
Approximately 88% of the sequence has been anchored to chromosomes, 
which include autosomes 1-24, 26-28, and 32, and sex chromosomes W and Z. 
(In contrast to mammals, the female chicken is heterogametic (ZW) and the 
male is homogametic (ZZ).) 
The remaining unanchored contigs that could be localized to a chromosome 
have been concatenated into the virtual 
chromosomes &quot;chr*_random&quot;, separated by gaps of 10,000 bp. 
Unanchored contigs that could not be localized to a chromosome 
have been concatenated into the virtual 
chromosome &quot;chrUn&quot;, separated by gaps of 1,000 bp in order 
to reduce the total size of chrUn. 
The chicken 
mitochondrial sequence is also available as the virtual chromosome 
&quot;chrM&quot;. 
</P>


<P>
Bulk downloads of the sequence and annotation data are available via 
the Genome Browser <A HREF="ftp://hgdownload.cse.ucsc.edu/goldenPath/$db/">FTP
server</A> or the <A HREF="http://hgdownload.cse.ucsc.edu/downloads.html#$anchorRoot">Downloads</A> page. 

  *** Developer -- remove this if no conditions for use:
These data have
<A HREF="../goldenPath/credits.html#${anchorRoot}_use">specific 
conditions for use</A>. 

The $genome browser annotation tracks were generated by UCSC and 
collaborators worldwide. See the 
<A HREF="/goldenPath/credits.html#${anchorRoot}_credits">Credits</A> 
page for a detailed list of the organizations and individuals who contributed 
to the success of this release. 
</P>
_EOF_
  ;
  close($fh);
} # makeDescription

sub makeLocalTrackDbRa {
  # Make an assembly-level trackDb.ra, gap.html and gold.html.
  my @localFiles = qw( trackDb.ra gap.html gold.html );

  my $fh = &HgAutomate::mustOpen(">$topDir/html/trackDb.ra");
  print $fh <<_EOF_
# Local declaration so that local gold.html is picked up.
track gold
shortLabel Assembly
longLabel Assembly from Fragments
group map
priority 10
visibility hide
color 150,100,30
altColor 230,170,40
type bed 3 +

# Local declaration so that local gap.html is picked up.
track gap
shortLabel Gap
longLabel Gap Locations
priority 11
group map
visibility dense
type bed 3 +

_EOF_
  ;
  if (! $chromBased) {
    print $fh <<_EOF_
# Unsplit mRNA track for scaffold-based assembly.
track all_mrna
shortLabel \$Organism mRNAs
longLabel \$Organism mRNAs from GenBank
group rna
priority 54
visibility pack
spectrum on
type psl .
cdsDrawOptions enabled
baseColorUseCds genbank
baseColorUseSequence genbank
showDiffBasesAllScales .

_EOF_
    ;
  }
  close($fh);

  $fh = &HgAutomate::mustOpen(">$topDir/html/gap.html");
  if ($gotAgp) {
    print $fh <<_EOF_
<H2>Description</H2>
This track depicts gaps in the assembly.

  *** Developer: remove this statement if no future assemblies are expected:

Many of these gaps &mdash; with the
exception of intractable heterochromatic gaps &mdash; may be closed during the
finishing process.

<P>
Gaps are represented as black boxes in this track.
If the relative order and orientation of the contigs on either side
of the gap is supported by read pair data, 
it is a <em>bridged</em> gap and a white line is drawn 
through the black box representing the gap. 
</P>
<P>This assembly contains the following principal types of gaps:
<UL>

  *** Developer: make sure this part is accurate.  Here is a WUSTL template:

<LI><B>Fragment</B> - gaps between the Whole Genome Shotgun contigs of a 
supercontig.  (In this context, a contig is a set of overlapping sequence reads.  
A supercontig is a set of contigs ordered and oriented during the 
Whole Genome Shotgun process using paired-end reads.)
These are represented by varying numbers of <em>N</em>s  in the assembly.  
Fragment gap sizes are usually taken from read pair data.  
<LI><B>Clone</B> - gaps between supercontigs linked by the fingerprint map.  
In general, these are represented by 10,000 <em>N</em>s  in the assembly.
<LI><B>Contig</B> - gaps between supercontigs not linked by the fingerprint 
map, but instead by marker data.  (In this context, the &quot;Contig&quot; gap 
type refers to a map contig, not a sequence contig.)  
In general, these are represented by 10,000 <em>N</em>s  in the assembly for all 
chromosomes except chrUn (concatenation of unplaced supercontigs), where 
gaps of 1,000 <em>N</em>s  are used.  Gaps of other sizes were used when mRNA or 
other data suggested possible but not confirmed links between supercontigs.
<LI><B>Centromere</B> - gaps for centromeres were included when they could be 
reasonably localized.  These are represented by 1,500,000 <em>N</em>s  in the 
assembly for the macrochromosomes 1-10 and Z, and by 500,000 <em>N</em>s  for 
all others (microchromosomes).  

  *** Developer: here is a Broad template:

<LI><B>Fragment</B> - gaps between the Whole Genome Shotgun contigs of a 
supercontig. These are represented by varying numbers of <em>N</em>s in the 
assembly. In this context, a contig is a set of overlapping sequence reads and  
a supercontig is a set of contigs ordered and oriented during the 
Whole Genome Shotgun process using paired-end reads. Fragment gap sizes 
are usually taken from read pair data.  
</LI>
<LI><B>Clone</B> - gaps between supercontigs linked by the physical map.  
In general, these are represented by 1,000 <em>N</em>s  in the assembly.  
Clone gaps of 3,000,000 have been placed at the end of chrX and at the 
beginning of all other chromosomes.  
</LI>


</UL></P>


_EOF_
    ;
  } else {
    print $fh <<_EOF_
<H2>Description</H2>
This track depicts gaps in the assembly.

  *** Developer: remove this statement if no future assemblies are expected:

Many of these gaps &mdash; with the
exception of intractable heterochromatic gaps &mdash; may be closed during the
finishing process.
<P>
Gaps are represented as black boxes in this track.
If the relative order and orientation of the contigs on either side
of the gap is known, it is a <em>bridged</em> gap and a white line is drawn 
through the black box representing the gap.
</P>
<P>
This assembly was sequenced with paired reads taken from

  *** Developer: check if this is accurate:

plasmids and fosmids of various sizes.

Overlapping reads were merged into contigs,
and pairing information was then used to join the contigs into scaffolds.
The gap sizes are estimated from the size of the 

plasmids and fosmids,

with a minimum gap size of $fakeAgpMinContigGap.
</P>
<P></P>
<P>This assembly contains the following types of gaps:
<UL>
<LI><B>Contig</B> - gaps of size $fakeAgpMinContigGap to $fakeAgpMinScaffoldGap.
<LI><B>Scaffold</B> - gaps greater than $fakeAgpMinScaffoldGap in size.
</UL>
</P>
_EOF_
    ;
  }
  close($fh);

  $fh = &HgAutomate::mustOpen(">$topDir/html/gold.html");
  my $em = $commonName ? "" : "<em>";
  my $noEm = $commonName ? "" : "</em>";
  if ($gotAgp) {
    print $fh <<_EOF_
<H2>Description</H2>
<P>
This track shows the draft assembly of the $em\$organism$noEm genome.  

  *** Developer: check if this is accurate:

Whole-genome shotgun reads were assembled into contigs.  When possible, 
contigs were grouped into scaffolds (also known as &quot;supercontigs&quot;).
The order, orientation and gap sizes between contigs within a scaffold are
based on paired-end read evidence. </P>
<P>
In dense mode, this track depicts the contigs that make up the 
currently viewed scaffold. 
Contig boundaries are distinguished by the use of alternating gold and brown 
coloration. Where gaps
exist between contigs, spaces are shown between the gold and brown
blocks.  The relative order and orientation of the contigs
within a scaffold is always known; therefore, a line is drawn in the graphical
display to bridge the blocks.</P>
<P>
All components within this track are of fragment type &quot;W&quot;: 
Whole Genome Shotgun contig. </P>
_EOF_
    ;
  } else {
    print $fh <<_EOF_
<H2>Description</H2>
<P>
This track shows the draft assembly of the $em\$organism$noEm genome.  

  *** Developer: check if this is accurate:

Whole-genome shotgun reads were assembled into contigs.  When possible, 
contigs were grouped into scaffolds (also known as &quot;supercontigs&quot;).
The order, orientation and gap sizes between contigs within a scaffold are
based on paired-end read evidence. </P>
<P>
Locations of contigs and scaffolds were deduced from runs of Ns in the
assembled sequence.  A run of Ns of more than $fakeAgpMinScaffoldGap bases
was assumed to be a gap between scaffolds, and a run of Ns between
$fakeAgpMinContigGap and $fakeAgpMinScaffoldGap was assumed to be a gap
between contigs.</P>
<P>
In dense mode, this track depicts the contigs that make up the 
currently viewed scaffold. 
Contig boundaries are distinguished by the use of alternating gold and brown 
coloration. Where gaps
exist between contigs, spaces are shown between the gold and brown
blocks.  The relative order and orientation of the contigs
within a scaffold is always known; therefore, a line is drawn in the graphical
display to bridge the blocks.</P>
<P>
All components within this track are of fragment type &quot;W&quot;: 
Whole Genome Shotgun contig. </P>
_EOF_
    ;
  }
  close($fh);

  my $localFiles = "$topDir/html/{" . join(',', @localFiles) . '}';
  return $localFiles;
} # makeLocalTrackDbRa

sub makeTrackDb {
# * step: trackDb [dbHost]
  my $runDir = "$topDir/TemporaryTrackDbCheckout";
  &HgAutomate::mustMkdir($runDir);
  &HgAutomate::mustMkdir("$topDir/html");
  &makeDescription();
  my $localFiles = &makeLocalTrackDbRa();
  my $whatItDoes =<<_EOF_
It makes a local checkout of kent/src/hg/makeDb/trackDb/
and makes *suggested* modifications.  Checking in modified files, and adding
new files, is left up to the user in case this script has made some wrong
guesses about proper names and locations.
_EOF_
  ;
  my $bossScript = new HgRemoteScript("$scriptDir/makeTrackDb.csh", $dbHost,
				      $runDir, $whatItDoes, $CONFIG);

  $bossScript->add(<<_EOF_
# These directories are necessary for running make in trackDb:
$HgAutomate::cvs -q co -P \\
  kent/src/inc kent/src/hg/lib kent/src/hg/makeDb/trackDb

cd kent/src/hg/makeDb/trackDb

# Create the expected species-level directory for $db if necessary:
mkdir -p $dbDbSpeciesDir/$db

# Move local description files into place:
mv $localFiles $dbDbSpeciesDir/$db/

# Copy the template description.html for $db into place, and link to it
# from $HgAutomate::gbdb/$db/html/ .
if (! -e $dbDbSpeciesDir/$db/description.html) then
  cp -p $topDir/html/description.html $dbDbSpeciesDir/$db/description.html
endif
mkdir -p $HgAutomate::gbdb/$db/html
rm -f $HgAutomate::gbdb/$db/html/description.html
ln -s $topDir/html/description.html $HgAutomate::gbdb/$db/html/

# Do a test run with the generated files:
make update DBS=$db
_EOF_
  );

  $bossScript->execute();

  $endNotes .=<<_EOF_

Template trackDb.ra and .html's have been created, but they all need editing!

cd $runDir/kent/src/hg/makeDb/trackDb/$dbDbSpeciesDir/$db

Search for '***' notes in each file in and make corrections (sometimes the
files used for a previous assembly might make a better template):
  description.html $localFiles

Then cd ../.. (to trackDb/) and
 - edit makefile to add $db to DBS.
 - (if necessary) cvs add $dbDbSpeciesDir
 - cvs add $dbDbSpeciesDir/$db
 - cvs add $dbDbSpeciesDir/$db/*.{ra,html}
 - cvs ci -m "Added $db to DBS." makefile
 - cvs ci -m "Initial descriptions for $db." $dbDbSpeciesDir/$db
 - (if necessary) cvs ci $dbDbSpeciesDir
 - Run make update DBS=$db and make alpha when done.
 - (optional) Clean up $runDir
 - cvsup your ~/kent/src/hg/makeDb/trackDb and make future edits there.

_EOF_
  ;
} # makeTrackDb


#########################################################################
# main

# Prevent "Suspended (tty input)" hanging:
&HgAutomate::closeStdin();

&checkOptions();
&usage(1) if (scalar(@ARGV) != 1);
($CONFIG) = @ARGV;

#*** Force -verbose until this is really ready to go:
$opt_verbose = 3 if (! $opt_verbose);

&parseConfig($CONFIG);

$endNotes = "";
$bedDir = "$topDir/$HgAutomate::trackBuild";
$scriptDir = "$topDir/jkStuff";
if (-e "$topDir/chrom.sizes") {
  $chromBased = (`wc -l < $topDir/chrom.sizes` < $HgAutomate::splitThreshold);
}

$stepper->execute();

my $stopStep = $stepper->getStopStep();
my $upThrough = ($stopStep eq 'trackDb') ? "" :
  "  (through the '$stopStep' step)";

HgAutomate::verbose(1,
	"\n *** All done!$upThrough\n");

if ($endNotes) {
  #*** Should mail this to $ENV{'USER'} so it's not so easy to ignore.
  #*** Does mail work on all of our machines??  Even if it works on one,
  #*** we can ssh it.  Should be in an HgAutomate routine.
  HgAutomate::verbose(0,
		      "\n\nNOTES -- STUFF THAT YOU WILL HAVE TO DO --\n\n" . 
		      "$endNotes\n");
}

