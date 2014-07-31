#!/usr/bin/env perl

# CHARMM trajectory analysis 
#
# 2005, Michael Feig, Michigan State University
#

sub usage {
  printf STDERR "usage:   analyzeCHARMM.pl [options] [DCDfile(s)]\n";
  printf STDERR "options: [-inx from:to] [-time from:to] [-step size]\n";
  printf STDERR "         [-psf PSFfile] [-pdb PDBfile] [-crd CRDfile] [-comp PDBfile]\n";
  printf STDERR "         [-log logFile] [-cmd logFile]\n";
  printf STDERR "         [-f listfile]\n";
  printf STDERR "         [-par CHARMMparams]\n";
  printf STDERR "         [-custom file]\n";
  printf STDERR "         [-sel string] [-fitsel string] [-dsel string string]\n";
  printf STDERR "         [-tsel string string string] [-qsel string string string string]\n";
  printf STDERR "         [-mass] [-[no]fit] [-ref x y z] [-cofm] [-ndens]\n";
  printf STDERR "         [-rgyr] [-center] [-rms] [-mindist] [-maxdist]\n";
  printf STDERR "         [-dist] [-angle] [-dihedral] [-pucker]\n";
  printf STDERR "         [-phi] [-psi] [-omega] [-chi1]\n";
  printf STDERR "         [-epsilon] [-zeta] [-delta] [-alpha] [-beta] [-gamma]\n";
  printf STDERR "         [-volume] [-surface] [-ientropy]\n";
  printf STDERR "         [-rdist] [-diffusion]\n";
  exit 1;
}

use vars qw ( $perllibdir );

BEGIN {
  $perllibdir="$ENV{MMTSBDIR}/perl" if (defined $ENV{MMTSBDIR});
  ($perllibdir=$0)=~s/[^\/]+$// if (!defined $perllibdir);
}

use lib $perllibdir;
use strict;

use POSIX ":sys_wait_h";

use Fcntl;

use GenUtil;
use Molecule;
use CHARMM;

my %par;

my $logFile;
my $cmdlog;

my $pdbfile;
my $psffile;
my $crdfile;

my $listfile;

my $ifrom;
my $ito;

my $tfrom;
my $tto;

my $stepsize=1;

my $pdbcomp;

my $customfile;

my @dcdfiles;

my $selection1;
my $selection2;
my $selection3;
my $selection4;

my $mass=0;
my $cofm=0;
my $refx;
my $refy;
my $refz;

my $ndens=0;

my $fit=1;
my $fitsel;

my @analyze=();

while ($#ARGV>=0) {
  if ($ARGV[0] eq "-par") {
    shift @ARGV;
    &GenUtil::parsePar(\%par,shift @ARGV);
  } elsif ($ARGV[0] eq "-log") {
    shift @ARGV;
    $logFile=(shift @ARGV);
  } elsif ($ARGV[0] eq "-cmd") {
    shift @ARGV;
    $cmdlog=(shift @ARGV);
  } elsif ($ARGV[0] eq "-custom") {
    shift @ARGV;
    $customfile=shift @ARGV;
  } elsif ($ARGV[0] eq "-f") {
    shift @ARGV;
    $listfile=shift @ARGV;
  } elsif ($ARGV[0] eq "-pdb") {
    shift @ARGV;
    $pdbfile=shift @ARGV;
  } elsif ($ARGV[0] eq "-crd") {
    shift @ARGV;
    $crdfile=shift @ARGV;
  } elsif ($ARGV[0] eq "-comp") {
    shift @ARGV;
    $pdbcomp=shift @ARGV;
  } elsif ($ARGV[0] eq "-psf") {
    shift @ARGV;
    $psffile=shift @ARGV;
  } elsif ($ARGV[0] eq "-inx") {
    shift @ARGV;
    my @f=split(/:/,shift @ARGV);
    $ifrom=$f[0];
    $ito=($#f>0)?$f[1]:$ifrom;
  } elsif ($ARGV[0] eq "-time") {
    shift @ARGV;
    my @f=split(/:/,shift @ARGV);
    $tfrom=$f[0];
    $tto=($#f>0)?$f[1]:$tfrom;
  } elsif ($ARGV[0] eq "-step") {
    shift @ARGV;
    $stepsize=shift @ARGV;
  } elsif ($ARGV[0] eq "-sel") {
    shift @ARGV;
    $selection1=shift @ARGV;
  } elsif ($ARGV[0] eq "-dsel") {
    shift @ARGV;
    $selection1=shift @ARGV;
    $selection2=shift @ARGV;
  } elsif ($ARGV[0] eq "-tsel") {
    shift @ARGV;
    $selection1=shift @ARGV;
    $selection2=shift @ARGV;
    $selection3=shift @ARGV;
  } elsif ($ARGV[0] eq "-qsel") {
    shift @ARGV;
    $selection1=shift @ARGV;
    $selection2=shift @ARGV;
    $selection3=shift @ARGV;
    $selection4=shift @ARGV;
  } elsif ($ARGV[0] eq "-mass") {
    shift @ARGV;
    $mass=1;
  } elsif ($ARGV[0] eq "-fit") {
    shift @ARGV;
    $fit=1;
  } elsif ($ARGV[0] eq "-nofit") {
    shift @ARGV;
    $fit=0;
  } elsif ($ARGV[0] eq "-ref") {
    shift @ARGV;
    $refx=shift @ARGV;
    $refy=shift @ARGV;
    $refz=shift @ARGV;
  } elsif ($ARGV[0] eq "-cofm") {
    shift @ARGV;
    $cofm=1;
  } elsif ($ARGV[0] eq "-ndens") {
    shift @ARGV;
    $ndens=1;
  } elsif ($ARGV[0] eq "-fitsel") {
    shift @ARGV;
    $fitsel=shift @ARGV;
  } elsif ($ARGV[0] eq "-help" || $ARGV[0] eq "-h") {
    &usage();
  } elsif ($ARGV[0] =~ /-([a-zA-Z0-9]+)/) {
    shift @ARGV;
    push(@analyze,$1);
  } else {
    die "Unknown option $ARGV[0]" if ($ARGV[0]=~/^-/);
    my $fname=shift @ARGV;
    push(@dcdfiles,$fname) if (-r $fname);
  }
}

if (-r $listfile) {
  open INP,"$listfile";
  while (<INP>) {
    chomp;
    push(@dcdfiles,$_) if (-r $_);
  }
}

if ($#dcdfiles<0) {
  printf STDERR "no DCD files given\n";
  usage();
}

my $charmm=CHARMM::new($logFile,$cmdlog);

$charmm->loadParameters(%par);

if (defined $psffile) {
  $charmm->setupFromPSF($psffile,$crdfile);
  $charmm->readFromPDB($pdbfile) if (defined $pdbfile && !defined $crdfile);
} else {
  $charmm->setupFromPDB($pdbfile);
}

if (grep(/rdist|diffusion/,@analyze) && !defined $charmm->{par}->{boxsize} && 
    !defined $charmm->{par}->{boxx}) {
  my ($bx,$by,$bz)=&_getBoxSizeFromDCD($dcdfiles[0]);
  $charmm->loadParameters(boxx=>$bx, boxy=>$by, boxz=>$bz) if (defined $bx);
}

#$charmm->setupEnergy() if ((defined $pdbfile || defined $crdfile) && grep(/rdist/,@analyze));

$charmm->loadReference($pdbcomp,9001.0) if (defined $pdbcomp);

my $sel1;
my $sel2;
my $sel3;
my $sel4;
my $selft;

my @selections=();
my @puckerlist=();

if (grep(/phi|psi|omega|chi|alpha|beta|gamma|delta|epsilon|zeta|pucker/,@analyze)) {
  push(@analyze,"dihedral") if (grep(/phi|psi|omega|chi|alpha|beta|gamma|delta|epsilon|zeta/,@analyze)); 
  my $m=$charmm->{molecule};
  $m->setValidSelection($selection1);
  foreach my $cm ( @{$m->{chain}} ) {
    for (my $ir=0; $ir<=$#{$cm->{res}}; $ir++) {
      my $rm=$cm->{res}->[$ir];
      if ($rm->{valid}) {
       if ($rm->{name}=~/ALA|ARG|ASN|ASP|CYS|GLN|GLU|GLY|HSD|HSE|HSP|HIS|HSP|HID|HIE|HIP|ILE|LEU|LYS|MET|PHE|PRO|SER|THR|TRP|TYR|VAL|CYX/) {
        foreach my $a ( @analyze ) {
         if ($a eq "phi" && $ir>0 && $rm->{num}-1==$cm->{res}->[$ir-1]->{num}) {
          my $srec={};
          $srec->{sel1}=sprintf("ATOM %s %d C ",$rm->{seg},$rm->{num}-1);
          $srec->{sel2}=sprintf("ATOM %s %d N ",$rm->{seg},$rm->{num});
          $srec->{sel3}=sprintf("ATOM %s %d CA",$rm->{seg},$rm->{num});
          $srec->{sel4}=sprintf("ATOM %s %d C ",$rm->{seg},$rm->{num});
          push(@selections,$srec);
         } 
         if ($a eq "psi" && $ir<$#{$cm->{res}} && $rm->{num}+1==$cm->{res}->[$ir+1]->{num}) {
          my $srec={};
          $srec->{sel1}=sprintf("ATOM %s %d N ",$rm->{seg},$rm->{num});
          $srec->{sel2}=sprintf("ATOM %s %d CA ",$rm->{seg},$rm->{num});
          $srec->{sel3}=sprintf("ATOM %s %d C ",$rm->{seg},$rm->{num});
          $srec->{sel4}=sprintf("ATOM %s %d N ",$rm->{seg},$rm->{num}+1);
          push(@selections,$srec);
         } 
         if ($a eq "omega" && $ir<$#{$cm->{res}} && $rm->{num}+1==$cm->{res}->[$ir+1]->{num}) {
          my $srec={};
          $srec->{sel1}=sprintf("ATOM %s %d CA ",$rm->{seg},$rm->{num});
          $srec->{sel2}=sprintf("ATOM %s %d C ",$rm->{seg},$rm->{num});
          $srec->{sel3}=sprintf("ATOM %s %d N ",$rm->{seg},$rm->{num}+1);
          $srec->{sel4}=sprintf("ATOM %s %d CA ",$rm->{seg},$rm->{num}+1);
          push(@selections,$srec);
         } 
         if ($a eq "chi1") {
          my $srec={};
          $srec->{sel1}=sprintf("ATOM %s %d N ",$rm->{seg},$rm->{num});
          $srec->{sel2}=sprintf("ATOM %s %d CA ",$rm->{seg},$rm->{num});
          $srec->{sel3}=sprintf("ATOM %s %d CB ",$rm->{seg},$rm->{num});
          $srec->{sel4}=sprintf("ATOM %s %d CG ",$rm->{seg},$rm->{num}) 
            if ($rm->{name}=~/GLU|PRO|LYS|GLN|ARG|LEU|PHE|TYR|TRP|ASN|ASP|MET|HIS|HSD|HSE|HSP/);
          $srec->{sel4}=sprintf("ATOM %s %d OG ",$rm->{seg},$rm->{num}) 
            if ($rm->{name} eq "SER");
          $srec->{sel4}=sprintf("ATOM %s %d OG1 ",$rm->{seg},$rm->{num}) 
            if ($rm->{name} eq "THR");
          $srec->{sel4}=sprintf("ATOM %s %d CG1 ",$rm->{seg},$rm->{num}) 
            if ($rm->{name}=~/VAL|ILE/);
          $srec->{sel4}=sprintf("ATOM %s %d SG ",$rm->{seg},$rm->{num}) 
            if ($rm->{name} eq "CYS");
          push(@selections,$srec) if (defined $srec->{sel4});
         } 
        }
       } elsif ($rm->{name}=~/ADE|GUA|CYT|THY|URA/) {
        foreach my $a ( @analyze ) {
         if ($a eq "pucker") {
           my $srec={};
           $srec->{segid}=$rm->{seg};
           $srec->{resid}=$rm->{num};
           push(@puckerlist,$srec);
         } 
         if ($a eq "zeta" && $ir<$#{$cm->{res}} && $rm->{num}+1==$cm->{res}->[$ir+1]->{num}) {
          my $srec={};
          $srec->{sel1}=sprintf("ATOM %s %d C3' ",$rm->{seg},$rm->{num});
          $srec->{sel2}=sprintf("ATOM %s %d O3' ",$rm->{seg},$rm->{num});
          $srec->{sel3}=sprintf("ATOM %s %d P ",$rm->{seg},$rm->{num}+1);
          $srec->{sel4}=sprintf("ATOM %s %d O5' ",$rm->{seg},$rm->{num}+1);
          push(@selections,$srec);
         } 
         if ($a eq "epsilon" && $ir<$#{$cm->{res}} && $rm->{num}+1==$cm->{res}->[$ir+1]->{num}) {
          my $srec={};
          $srec->{sel1}=sprintf("ATOM %s %d C4' ",$rm->{seg},$rm->{num});
          $srec->{sel2}=sprintf("ATOM %s %d C3' ",$rm->{seg},$rm->{num});
          $srec->{sel3}=sprintf("ATOM %s %d O3' ",$rm->{seg},$rm->{num});
          $srec->{sel4}=sprintf("ATOM %s %d P ",$rm->{seg},$rm->{num}+1);
          push(@selections,$srec);
         } 
         if ($a eq "delta" && $ir<$#{$cm->{res}} && $rm->{num}+1==$cm->{res}->[$ir+1]->{num}) {
          my $srec={};
          $srec->{sel1}=sprintf("ATOM %s %d C5' ",$rm->{seg},$rm->{num});
          $srec->{sel2}=sprintf("ATOM %s %d C4' ",$rm->{seg},$rm->{num});
          $srec->{sel3}=sprintf("ATOM %s %d C3' ",$rm->{seg},$rm->{num});
          $srec->{sel4}=sprintf("ATOM %s %d O3' ",$rm->{seg},$rm->{num});
          push(@selections,$srec);
         } 
         if ($a eq "gamma" && $ir>0 && $rm->{num}-1==$cm->{res}->[$ir-1]->{num}) {
          my $srec={};
          $srec->{sel1}=sprintf("ATOM %s %d O5'",$rm->{seg},$rm->{num});
          $srec->{sel2}=sprintf("ATOM %s %d C5'",$rm->{seg},$rm->{num});
          $srec->{sel3}=sprintf("ATOM %s %d C4'",$rm->{seg},$rm->{num});
          $srec->{sel4}=sprintf("ATOM %s %d C3'",$rm->{seg},$rm->{num});
          push(@selections,$srec);
         } 
         if ($a eq "beta" && $ir>0 && $rm->{num}-1==$cm->{res}->[$ir-1]->{num}) {
          my $srec={};
          $srec->{sel1}=sprintf("ATOM %s %d P",$rm->{seg},$rm->{num});
          $srec->{sel2}=sprintf("ATOM %s %d O5'",$rm->{seg},$rm->{num});
          $srec->{sel3}=sprintf("ATOM %s %d C5'",$rm->{seg},$rm->{num});
          $srec->{sel4}=sprintf("ATOM %s %d C4'",$rm->{seg},$rm->{num});
          push(@selections,$srec);
         } 
         if ($a eq "alpha" && $ir>0 && $rm->{num}-1==$cm->{res}->[$ir-1]->{num}) {
          my $srec={};
          $srec->{sel1}=sprintf("ATOM %s %d O3'",$rm->{seg},$rm->{num}-1);
          $srec->{sel2}=sprintf("ATOM %s %d P",$rm->{seg},$rm->{num});
          $srec->{sel3}=sprintf("ATOM %s %d O5'",$rm->{seg},$rm->{num});
          $srec->{sel4}=sprintf("ATOM %s %d C5'",$rm->{seg},$rm->{num});
          push(@selections,$srec);
         } 
        }
       }
      }
    }  
  }
} else {
  if (defined $selection1) {
    $sel1="SL1";
    $charmm->defineSelection($selection1,$sel1);
  }

  if (defined $selection2) {
    $sel2="SL2";
    $charmm->defineSelection($selection2,$sel2);
  }

  if (defined $selection3) {
    $sel3="SL3";
    $charmm->defineSelection($selection3,$sel3);
  }

  if (defined $selection4) {
    $sel4="SL4";
    $charmm->defineSelection($selection4,$sel4);
  }

  if (defined $fitsel) {
    $selft="SLF";
    $charmm->defineSelection($fitsel,$selft);
  }
}

my $accu;
my $naccu=0;
foreach my $f ( @dcdfiles ) {  
  foreach my $a ( @analyze ) {
    if ($a eq "rdist") {
       my ($n,$data)=$charmm->analyzeRadialDistribution($f,
         selection=>$sel1,xselection=>$sel2,refx=>$refx,refy=>$refy,refz=>$refz,
         cofm=>$cofm, ndens=>$ndens );

       if (!defined $accu) {
	 $accu=();
         foreach my $d (@{$data} ) {
           $d->{val}*=$n;
           push(@{$accu},$d);
         }
       } else {
         for (my $id=0; $id<=$#{$data}; $id++) {
           $accu->[$id]->{val}+=$n*$data->[$id]->{val};
         }
       }
       $naccu+=$n;
     } elsif ($a eq "diffusion") {
       my ($n,$diff)=$charmm->analyzeDiffusion($f,
	selection=>$sel1,xselection=>$sel2,refx=>$refx,refy=>$refy,refz=>$refz,cofm=>$cofm);
       if (!defined $accu) {
	 $accu=();
	 my $d={};
	 $d->{val}=$diff*$n;
	 push(@{$accu},$d);
       } else {
	 $accu->[0]->{val}+=$diff*$n;
       }
       $naccu+=$n;
     }
  }
}

if ($naccu>0) {
  my $oneline=0;
  foreach my $d ( @{$accu} ) {
    if (exists $d->{x}) {
      printf "%f %f\n",$d->{x},$d->{val}/$naccu;
    } else {
      printf "%f ",$d->{val}/$naccu;
      $oneline=1;
    }
  }
  printf "\n" if ($oneline);
} else {
 my $iframe=0;
 foreach my $f ( @dcdfiles ) {
  $charmm->initTrajectory($f);

  while ($charmm->nextFrame()>0) {
    my $timestep=($charmm->{_firstframe}+$charmm->{_readframes}-1)*
      $charmm->{_trajfreq}*$charmm->{_trajdelta};
    
    $iframe++;
    if ((!defined $ifrom || ($iframe>=$ifrom && $iframe<=$ito)) && 
	(!defined $tfrom || ($timestep>=$tfrom-0.0000001 && $timestep<=$tto+0.0000001)) &&
	(!defined $stepsize || ($iframe%$stepsize==0))) {
      if (defined $customfile && &GenUtil::checkFile($customfile)) {
	my $custom=&GenUtil::readData(&GenUtil::getInputFile($customfile));
	$charmm->stream($custom);
      } elsif ($#analyze>=0) {
	my @results=();
	foreach my $a ( @analyze ) {
	  if ($a eq "rgyr") {
	    push(@results,$charmm->analyzeRadiusOfGyration(selection=>$sel1, mass=>$mass));
	  } elsif ($a eq "center") {
	    push(@results,$charmm->analyzeCenterOfMass(selection=>$sel1, mass=>$mass));
	  } elsif ($a eq "rms") {
	    push(@results,$charmm->analyzeRMS(selection=>$sel1, mass=>$mass, fitsel=>$selft, fit=>$fit));
	  } elsif ($a eq "mindist") {
	    push(@results,$charmm->analyzeMinimumDistance(selection=>$sel1, xselection=>$sel2));
	  } elsif ($a eq "maxdist") {
	    push(@results,$charmm->analyzeMaximumDistance(selection=>$sel1, xselection=>$sel2));
	  } elsif ($a eq "volume") {
	    push(@results,$charmm->analyzeMolecularVolume(selection=>$sel1));
	  } elsif ($a eq "surface") {
	    push(@results,$charmm->analyzeAccessibleSurface(selection=>$sel1));
	  } elsif ($a eq "ientropy") {
	    push(@results,$charmm->analyzeInertiaEntropy(selection=>$sel1));
          } elsif ($a eq "dist") {
            if ($#selections>=0) {
              foreach my $s ( @selections ) {
  	        push(@results,$charmm->analyzeDistance(selection1=>$s->{sel1},selection2=>$s->{sel2},
                                                       mass=>$mass));
              } 
            } else { 
  	      push(@results,$charmm->analyzeDistance(selection1=>$sel1,selection2=>$sel2,
		 				     mass=>$mass));
            }
          } elsif ($a eq "angle") {
            if ($#selections>=0) {
              foreach my $s ( @selections ) {
  	        push(@results,$charmm->analyzeAngle(selection1=>$s->{sel1},selection2=>$s->{sel2},
                                                    selection3=>$s->{sel3},mass=>$mass));
              } 
            } else { 
  	      push(@results,$charmm->analyzeAngle(selection1=>$sel1,selection2=>$sel2,
		 				  selection3=>$sel3,mass=>$mass));
            }
          } elsif ($a eq "dihedral") {
            if ($#selections>=0) {
              foreach my $s ( @selections ) {
  	        push(@results,$charmm->analyzeDihedral(selection1=>$s->{sel1},selection2=>$s->{sel2},
	  	 				       selection3=>$s->{sel3},selection4=>$s->{sel4},
                                                       mass=>$mass));
              } 
            } else { 
  	      push(@results,$charmm->analyzeDihedral(selection1=>$sel1,selection2=>$sel2,
		 				     selection3=>$sel3,selection4=>$sel4,mass=>$mass));
            }
	  } elsif ($a eq "pucker" && $#puckerlist>=0) {
            foreach my $p ( @puckerlist ) {
  	      push(@results,$charmm->analyzePucker(segid=>$p->{segid},resid=>$p->{resid}));
            }
	  }
	}
	printf "%d %d %f",$iframe,$charmm->{_readframes},$timestep;
	foreach my $r (@results) {
	  printf " %s",$r; 
	}
	printf "\n";
      }
    }
  }
  $charmm->closeTrajectory();
 }
}

$charmm->finish();

exit 0;

1;
 
sub _getBoxSizeFromDCD() {
  my $dcd=shift;

  my $dcdfile=&GenUtil::getInputFile($dcd);
  binmode $dcdfile;

  my $buffer;
  my $len;
  ($buffer,$len)=&readFortran($dcdfile);
  my ($tag,@icontrol)=unpack("A4L*",$buffer);
  
  ($buffer,$len)=&readFortran($dcdfile);
  ($buffer,$len)=&readFortran($dcdfile);
  my $crystal=$icontrol[10];

  if ($crystal) {
    my ($tbuf,$tlen)=&readFortran($dcdfile);
    my @cdat=unpack("d*",$tbuf);
    undef $dcdfile;
    return ($cdat[0],$cdat[2],$cdat[5]);
  } else {
    undef $dcdfile;
    return (undef,undef,undef);
  }
}

sub readFortran {
  my $handle=shift;

  my $dat;
  my $tdat;

  read($handle,$tdat,4) || die "cannot read data";
  my $len=unpack("L",$tdat);
  read($handle,$dat,$len) || die "cannot read data";
  read($handle,$tdat,4) || die "cannot read data";

#  printf STDERR "Fread %d bytes\n",$len;

  return ($dat,$len);
}
