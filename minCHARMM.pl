#!/usr/bin/env perl

# minimize structure from PDB file using CHARMM
#
# http://mmtsb.scripps.edu/doc/minCHARMM.pl.html
# 2000, Michael Feig, Brooks group, The Scripps Research Institute

sub usage {
  printf STDERR "usage:   minCHARMM.pl [options] [PDBfile]\n";
  printf STDERR "options: [-par CHARMMparams]\n";
  printf STDERR "         [-psf PSFfile CRDfile]\n";
  printf STDERR "         [-crdout file]\n";
  printf STDERR "         [-l [ca|cb|cab|heavy] force self|refpdb min:max[=...]]\n";
  printf STDERR "         [-cons [ca|cb|cab|heavy] self|refpdb min:max[_force][=...]]\n";
  printf STDERR "         [-hmcm chainFile min:max[_force][=...]]\n";
  printf STDERR "         [-rmsd [ca|cb|cab|heavy] refpdb refval min:max[_force][=...]]\n";
  printf STDERR "         [-custom file[:file]]\n";
  printf STDERR "         [-log logFile] [-elog energyLogFile]\n";
  printf STDERR "         [-cmd logFile]\n";
  exit 1;
}

use vars qw ( $perllibdir );

BEGIN {
  $perllibdir="$ENV{MMTSBDIR}/perl" if (defined $ENV{MMTSBDIR});
  ($perllibdir=$0)=~s/[^\/]+$// if (!defined $perllibdir);
}

use lib $perllibdir;
use strict;

use GenUtil;
use Molecule;
use CHARMM;

my $cons=();

my $logFile;
my $cmdlog;
my $energyFile;

my $inpfile="-";
my $base="";

my %par;

my $psffile;
my $crdfile;

my $crdout;

my $customfile;

my $done=0;
while ($#ARGV>=0 && !$done) {
  if ($ARGV[0] eq "-par") {
    shift @ARGV;
    &GenUtil::parsePar(\%par,shift @ARGV);
  } elsif ($ARGV[0] eq "-cons") {
    shift @ARGV;
    my $c={};
    if ($ARGV[0] =~ /^(ca|cb|cab|heavy)$/) {
      $c->{sel}=shift @ARGV;
    } else {
      $c->{sel}="heavy";
    }
    if ($ARGV[0] eq "self") {
      shift @ARGV;
      $c->{type}="self";
    } else {
      $c->{type}="ref";
      $c->{reffile}=shift @ARGV;
    }

    $c->{list}=&GenUtil::fragListFromOption(shift @ARGV);
    $c->{exclmode}=0;
    push(@{$cons},$c);
  } elsif ($ARGV[0] eq "-rmsd") {
    shift @ARGV;
    my $c={};
    if ($ARGV[0] =~ /^(ca|cb|cab|heavy)$/) {
      $c->{sel}=shift @ARGV;
    } else {
      $c->{sel}="heavy";
    }
    $c->{type}="rmsd";
    $c->{reffile}=shift @ARGV;
    $c->{refe}=shift @ARGV;
    $c->{list}=&GenUtil::fragListFromOption(shift @ARGV);
    $c->{orient}=1;
    $c->{exclmode}=0;
    push(@{$cons},$c);
  } elsif ($ARGV[0] eq "-l") {
    shift @ARGV;
    my $c={};
    if ($ARGV[0] =~ /^(ca|cb|cab|heavy)$/) {
      $c->{sel}=shift @ARGV;
    } else {
      $c->{sel}="heavy";
    }

    $c->{force}=shift @ARGV;

    if ($ARGV[0] eq "self") {
      shift @ARGV;
      $c->{type}="self";
    } else {
      $c->{type}="ref";
      $c->{reffile}=shift @ARGV;
    }

    $c->{list}=&GenUtil::fragListFromOption(shift @ARGV);
    $c->{exclmode}=1;
    push(@{$cons},$c);
  } elsif ($ARGV[0] eq "-hmcm") {
    shift @ARGV;
    my $c={};
    $c->{type}="hmcm";
    $c->{reffile}=shift @ARGV;
    $c->{list}=&GenUtil::fragListFromOption(shift @ARGV);
    push(@{$cons},$c);
  } elsif ($ARGV[0] eq "-log") {
    shift @ARGV;
    $logFile=(shift @ARGV);
  } elsif ($ARGV[0] eq "-cmd") {
    shift @ARGV;
    $cmdlog=(shift @ARGV);
  } elsif ($ARGV[0] eq "-elog") {
    shift @ARGV;
    $energyFile=(shift @ARGV);
  } elsif ($ARGV[0] eq "-psf") {
    shift @ARGV;
    $psffile=shift @ARGV;
    $crdfile=shift @ARGV;
  } elsif ($ARGV[0] eq "-crdout") {
    shift @ARGV;
    $crdout=shift @ARGV;
  } elsif ($ARGV[0] eq "-custom") {
    shift @ARGV;
    $customfile=shift @ARGV;
  } elsif ($ARGV[0] eq "-help" || $ARGV[0] eq "-h") {
    &usage();
  } else {
    die "Unknown option $ARGV[0]" if ($ARGV[0]=~/^-/);
    $inpfile=(shift @ARGV);
    $done=1;
  }
}

my $charmm=&CHARMM::new($logFile,$cmdlog);
$charmm->setEnergyLogFile($energyFile) 
  if (defined $energyFile);

$charmm->loadParameters(%par);

if (defined $psffile) {
  $charmm->setupFromPSF($psffile,$crdfile);
} else {
  $charmm->setupFromPDB($inpfile);
}

$charmm->setupEnergy();
$charmm->shake();

$charmm->noeRestraints();

$charmm->setupRestraints(1.0,$cons)
  if ($#{$cons}>=0);

if (defined $customfile) {
  foreach my $c ( split(/:/,$customfile))  {
    if (&GenUtil::checkFile($c)) {
      my $custom=&GenUtil::readData(&GenUtil::getInputFile($c));
      $charmm->stream($custom);
    }
  }
}

if ($charmm->{par}->{sdsteps}>0) {
  $charmm->minimizeSD();
  $charmm->logEnergy("SD");
}

if ($charmm->{par}->{minsteps}>0) {
  $charmm->minimize();
  $charmm->logEnergy("Min final");
}

$charmm->writeCRD($crdout) if (defined $crdout);

my $chmoutpdb=lc "pdb$$-out";
$charmm->writePDB($chmoutpdb);

my $outmol=Molecule::new();
$outmol->readPDB($chmoutpdb,translate=>&CHARMM::getConvType($charmm->{par}->{param}),
		 chainfromseg=>(defined $psffile)?0:1);

$outmol->setSSBonds($charmm->{molecule}->getSSBonds());
$outmol->writePDB("-",translate=>"CHARMM22");
&GenUtil::remove($chmoutpdb);

$charmm->finish();

exit 0;

