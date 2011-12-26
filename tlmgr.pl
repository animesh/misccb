#!/usr/bin/env perl
# $Id: tlmgr.pl 12152 2009-02-12 12:08:37Z preining $
#
# Copyright 2008, 2009 Norbert Preining
# This file is licensed under the GNU General Public License version 2
# or any later version.
#
# TODO:
# - in GUI mode updating bin-texlive/texlive.infra DOES work without
#   the warning, but it does NOT force to restart the GUI. THAT IS BAD!!!
#
# - when tlmgr2.pl is shipped globally as tlmgr.pl we can switch the
#   installer from using either texconfig paper or texlua ... to
#   tlmgr paper letter which will work on all platforms transparently.
#
# - tlmgr should have a "progress" bar for the update --all and install
#   etc options, echoing number of total packages etc etc.
#

my $svnrev = '$Revision: 12152 $';
my $datrev = '$Date: 2009-02-12 13:08:37 +0100 (Thu, 12 Feb 2009) $';
my $tlmgrrevision;
if ($svnrev =~ m/: ([0-9]+) /) {
  $tlmgrrevision = $1;
} else {
  $tlmgrrevision = "unknown";
}
$datrev =~ s/^.*Date: //;
$datrev =~ s/ \(.*$//;
$tlmgrrevision .= " ($datrev)";

our $Master;

BEGIN {
  $^W = 1;
  # make subprograms (including kpsewhich) have the right path:
  $mydir = $0;
  $mydir =~ s!\\!/!g if $^O =~ /^MSWin(32|64)$/i;
  $mydir =~ s,/[^/]*$,,;
  if ($^O=~/^MSWin(32|64)$/i) {
    $ENV{"PATH"} = "$mydir;$ENV{PATH}";
  } else {
    $ENV{"PATH"} = "$mydir:$ENV{PATH}";
  }
  #
  chomp($Master = `kpsewhich -var-value=SELFAUTOPARENT`);
  #
  # make Perl find our packages first:
  unshift (@INC, "$Master/tlpkg");
  unshift (@INC, "$Master/texmf/scripts/texlive");
}

use Cwd qw/abs_path/;
use Pod::Usage;
use Getopt::Long qw(:config no_autoabbrev permute);
use strict;

use TeXLive::TLConfig;
use TeXLive::TLMedia;
use TeXLive::TLPDB;
use TeXLive::TLPOBJ;
use TeXLive::TLPostActions;
use TeXLive::TLUtils;
use TeXLive::TLWinGoo;
TeXLive::TLUtils->import(qw(member info give_ctan_mirror win32 dirname
                            mkdirhier merge_into copy log debug));

use TeXLive::TLPaper;

binmode(STDOUT, ":utf8");
binmode(STDERR, ":utf8");

our $tlmediasrc;   # media from which we install/update
our $tlmediatlpdb;
our $location;     # location from which the new packages come
our $localtlpdb;   # local installation which we are munging
my %options;       # TL options from local tlpdb

# flags for machine-readable form
our $FLAG_REMOVE = "d";
our $FLAG_FORCIBLE_REMOVED = "f";
our $FLAG_UPDATE = "u";
our $FLAG_REVERSED_UPDATE = "r";
our $FLAG_AUTOINSTALL = "a";

# option variables
$::gui_mode = 0;
$::machinereadable = 0;

my %globaloptions = (
  "gui" => 1,
  "gui-lang" => "=s",
  "location" => "=s",
  "machine-readable" => 1,
  "package-logfiles" => "=s",
  "pause" => 1,
  "version" => 1,
  "help|h|?" => 1);

my %actionoptions = (
  "remove"   =>  { "no-depends" => 1, 
                   "no-depends-at-all" => 1,
                   "force" => 1,
                   "dry-run|n" => 1 },
  "show"     =>  { "list" => 1 },
  "search"   =>  { "global" => 1, 
                   "file" => 1 },
  "restore"  =>  { "backupdir" => "=s", 
                   "dry-run|n" => 1, 
                   "force" => 1 },
  "backup"   =>  { "backupdir" => "=s", 
                   "clean" => ":-99", 
                   "all" => 1, 
                   "dry-run|n" => 1 },
  "update"   =>  { "no-depends" => 1, 
                   "no-depends-at-all" => 1, 
                   "all" => 1, 
                   "list" => 1,
                   "no-remove" => 1, 
                   "force" => 1, 
                   "backupdir" => "=s", 
                   "backup" => 1, 
                   "dry-run|n" => 1 },
  "paper"    =>  { "list" => 1 },
  "install"  =>  { "no-depends" => 1, 
                   "no-depends-at-all" => 1, 
                   "reinstall" => 1,
                   "force" => 1, 
                   "dry-run|n" => 1 },
  "arch"     =>  { "dry-run|n" => 1 },
  "generate" =>  { "localcfg" => "=s", 
                   "dest" => "=s" },
  "uninstall"=>  { "force" => 1 },
  "check"    =>  { "use-svn" => 1 },
  "recreate-tlpdb" => { "arch" => "=s" }
  );

my %optarg;
for my $k (keys %globaloptions) { 
  if ($globaloptions{$k} eq "1") {
    $optarg{$k} = 1;
  } else {
    $optarg{"$k" . $globaloptions{$k}} = 1;
  }
}
for my $v (values %actionoptions) {
  for my $k (keys %$v) { 
    if ($v->{$k} eq "1") {
      $optarg{$k} = 1;
    } else {
      $optarg{"$k" . $v->{$k}} = 1;
    }
  }
}
TeXLive::TLUtils::process_logging_options();

our %opts;

GetOptions(\%opts, keys(%optarg)) or pod2usage(2);

$::machinereadable = $opts{"machine-readable"}  
  if (defined($opts{"machine-readable"}));

my $action = shift;
if (!defined($action)) {
  $action = $opts{"gui"} ? "gui" : "";
}

ddebug("action = $action\n");
for my $k (keys %opts) {
  ddebug("$k => $opts{$k}\n");
}
ddebug("arguments: @ARGV\n") if @ARGV;

sub give_version {
  if (!defined($::version_string)) {
    $::version_string = "";
    $::version_string .= "tlmgr revision $tlmgrrevision\n";
    $::version_string .= "tlmgr using installation: $Master\n";
    if (open (REL_TL, "$Master/release-texlive.txt")) {
      # print first and last lines, which have the TL version info.
      my @rel_tl = <REL_TL>;
      $::version_string .= $rel_tl[0];
      $::version_string .= $rel_tl[$#rel_tl];
      close (REL_TL);
    }
  }
  return $::version_string;
}

if ($opts{"version"} || (defined $action && $action eq "version")) {
  info(give_version());
  finish(0);
}

if (defined($action) && ($action =~ m/^help/i)) {
  $opts{"help"} = 1;
}

if ((!defined($action) || !$action) && !$opts{"help"}) {
  die "$0: missing action; try --help if you need it.\n";
}

if ($opts{"help"}) {
  # perldoc does ASCII emphasis on the output, so it's nice to use it.
  # But not all Unix platforms have it, and on Windows our Config.pm
  # can apparently interfere, so always skip it there.
  my @noperldoc = ();
  if (win32() || ! TeXLive::TLUtils::which("perldoc")) {
    @noperldoc = ("-noperldoc", "1");
  }
  pod2usage("-exitstatus" => 0, "-verbose" => 2, @noperldoc);
}

# unify arguments so that the $action contains paper in all cases
# and push the first arg back to @ARGV for action_paper processing
if ($action =~ /^(paper|xdvi|pdftex|dvips|dvipdfmx?|context)$/) {
  unshift(@ARGV, $action);
  $action = "paper";
}

# --machine-readable is only supported by update.
# 
if ($::machinereadable && $action ne "update") {
  tlwarn("tlmgr: --machine-readable output not supported for $action\n");
}

# check on supported arguments
#
my %suppargs;
%suppargs = %{$actionoptions{$action}}
  if defined($actionoptions{$action});
my @notvalidargs;
for my $k (keys %opts) {
  my $kk = $k;
  if ($k eq "n" || $k eq "dry-run") {
    $kk = "dry-run|n";
  }
  if (!defined($suppargs{$kk}) && !defined($globaloptions{$kk})) {
    push @notvalidargs, $k;
  }
}
if (@notvalidargs) {
  my $msg = 
    "The following arguments are not supported for the action $action:\n";
  for my $c (@notvalidargs) {
    $msg .= " $c";
  }
  $msg .= "\n";
  # here we should call pod2usage actually with the argument
  # -verbose => 99
  # -sections => "ACTIONS/$action.*"
  # to show the correct invocation of the action
  my @noperldoc = ();
  if (win32() || ! TeXLive::TLUtils::which("perldoc")) {
    @noperldoc = ("-noperldoc", "1");
  }
  pod2usage(-msg => $msg, -exitstatus => 1, -verbose => 1, @noperldoc);
}

# besides doing normal logging if -logfile is specified, we try to log
# package related actions (install, remove, update) to 
# the package-log file TEXMFSYSVAR/web2c/tlmgr.log
my $packagelogged = 0;  # how many msgs we logged
chomp (my $texmfsysvar = `kpsewhich -var-value=TEXMFSYSVAR`);
my $packagelogfile = $opts{"package-logfile"};
$packagelogfile ||= "$texmfsysvar/web2c/tlmgr.log";
#
# Try to open the packagelog file, but do NOT die when that does not work
if (!open(PACKAGELOG, ">>$packagelogfile")) {
  debug("Cannot open package log file $packagelogfile for appending\n");
  debug("Will not log package installation/removal/update for that run\n");
  $packagelogfile = "";
}

my $loadmediasrcerror = "Cannot load TeX Live database from ";

execute_action($action, @ARGV);
# end of main program.



sub execute_action {
  my ($action, @argv) = @_;
  my %ret;

  # we have to set @ARGV to the @argv since many of the action_* subs
  # use GetOption
  @ARGV = @argv;

  # actions which shouldn't have any lasting effects, such as search or
  # list, end by calling finish(0), which skips postinstall actions.
  if ($action =~ m/^_include_tlpobj$/) {
    # this is an internal function that should not be used outside
    init_local_db();
    for my $f (@ARGV) {
      my $tlpobj = TeXLive::TLPOBJ->new;
      $tlpobj->from_file($f);
      # we now have to check whether that is a .doc or .src package, so shipping
      # src or doc files from a different package.
      # We should have that package already installed ...
      my $pkg = $tlpobj->name;
      if ($pkg =~ m/^(.*)\.(source|doc)$/) {
        # got a .src or .doc package
        my $type = $2;
        my $mothership = $1;
        my $mothertlp = $localtlpdb->get_package($mothership);
        if (!defined($mothertlp)) {
          tlwarn("We are trying to add ${type} files to a not existing package $mothership!\n");
          tlwarn("Trying to continue!\n");
          # the best we can do is rename that package to $mothername and add it!
          $tlpobj->name($mothership);
          # add the src/docfiles tlpobj under the mothership name
          $localtlpdb->add_tlpobj($tlpobj);
        } else {
          if ($type eq "source") {
            $mothertlp->srcfiles($tlpobj->srcfiles);
            $mothertlp->srcsize($tlpobj->srcsize);
          } else {
            # must be "doc"
            $mothertlp->docfiles($tlpobj->docfiles);
            $mothertlp->docsize($tlpobj->docsize);
          }
          # that make sure that the original entry is overwritten
          $localtlpdb->add_tlpobj($mothertlp);
        }
      } else {
        # completely normal package, just add it
        $localtlpdb->add_tlpobj($tlpobj);
      }
      $localtlpdb->save;
    }
    finish(0);
  } elsif ($action =~ m/^get-mirror$/i) {
    my $loc = give_ctan_mirror();
    print "$loc\n";
    finish(0);
  } elsif ($action =~ m/^generate$/i) {
    merge_into(\%ret, action_generate());
  } elsif ($action =~ m/^gui$/i) {
    action_gui();
  } elsif ($action =~ m/^arch$/i) {
    merge_into(\%ret, action_arch());
  } elsif ($action =~ m/^option$/i) {
    merge_into(\%ret, action_option());
    finish(0);
  } elsif ($action =~ m/^list$/i) {
    merge_into(\%ret, action_list());
    finish(0);
  } elsif ($action =~ m/^check$/i) {
    merge_into(\%ret, action_check());
  } elsif ($action =~ m/^install$/i) {
    merge_into(\%ret, action_install());
  } elsif ($action =~ m/^update$/i) {
    merge_into(\%ret, action_update());
  } elsif ($action =~ m/^backup$/i) {
    merge_into(\%ret, action_backup());
  } elsif ($action =~ m/^restore$/i) {
    merge_into(\%ret, action_restore());
  } elsif ($action =~ m/^symlinks$/i) {
    merge_into(\%ret, action_symlinks());
    finish(0);
  } elsif ($action =~ m/^search$/i) {
    merge_into(\%ret, action_search());
    finish(0);
  } elsif ($action =~ m/^show$/i) {
    merge_into(\%ret, action_show());
    finish(0);
  } elsif ($action =~ m/^remove$/i) {
    merge_into(\%ret, action_remove());
  } elsif ($action =~ /^paper$/) {
    merge_into(\%ret, action_paper());
  } elsif ($action =~ m/^uninstall$/i) {
    merge_into(\%ret, action_uninstall());
    finish(0);
  } elsif ($action =~ m/^recreate-tlpdb$/i) {
    merge_into(\%ret, action_recreate_tlpdb());
    finish(0);
  } else {
    die "$0: unknown action: $action; try --help if you need it.\n";
  }

  # close the special log file
  if ($packagelogfile && !$::gui_mode) {
    info("tlmgr: package log updated at $packagelogfile\n") if $packagelogged;
    close(PACKAGELOG);
  }

  # run external programs.
  my $error_count = &handle_ret_hash(%ret);

  # done, just in case there are 256 errors.
  finish($error_count ? 1 : 0);
}



# run external programs (mktexlsr, updmap-sys, etc.) as specified by the
# keys in the RET hash.  We return the number of unsuccessful runs, zero
# if all ok.
# 
# If the "map" key is specified, the value may be a reference to a list
# of map command strings to pass to updmap, e.g., "enable Map=ascii.map".
# 
sub handle_ret_hash {
  my (%ret) = @_;
  my $errors = 0;
  
  if (exists $ret{'mktexlsr'}) {
    info("running mktexlsr\n");
    $errors += system("mktexlsr");
  }

  if (exists $ret{'map'}) {
    # We run updmap-sys instead of calling our create_updmap() function
    # (as we do for formats and languages), because many users run
    # updmap themselves (not using our local cfg feature), since that's
    # been the documented way to add fonts for many years.  If we called
    # create_updmap, such user maps would be lost.
    foreach my $m (@{$ret{'map'}}) {
      my $cmd = "updmap-sys --nomkmap --nohash --$m";
      info("running $cmd\n");
      $errors += system($cmd);
    }
    info("running updmap-sys once more\n");
    $errors += system("updmap-sys"); # runs mktexlsr.
  }

  chomp(my $TEXMFSYSVAR = `kpsewhich -var-value=TEXMFSYSVAR`);
  chomp(my $TEXMFLOCAL = `kpsewhich -var-value=TEXMFLOCAL`);

  # format means create missing formats (because a new one was added).
  # format-regenerate is used when the paper size changes.  In that
  # case, if option_create_formats is set, we simply want to generate
  # all formats; if not, then we only want to refresh existing ones.
  # 
  my $opt_fmt = $localtlpdb->option_create_formats;
  if (exists $ret{'format'}) {
    info("regenerating fmtutil.cnf in $TEXMFSYSVAR\n");
    TeXLive::TLUtils::create_fmtutil($localtlpdb,
                                     "$TEXMFSYSVAR/web2c/fmtutil.cnf",
                                     "$TEXMFLOCAL/web2c/fmtutil-local.cnf");
    if ($opt_fmt && !$ret{'format-regenerate'}) {
      info("running fmtutil-sys --missing\n");
      $errors += system("fmtutil-sys", "--missing");
    }
  }
  #
  if (exists $ret{'format-regenerate'}) {
    my $a = $opt_fmt ? "--all" : "--refresh";
    info("running fmtutil-sys $a to regenerate formats\n");
    $errors += system("fmtutil-sys", $a);
  }

  if (exists $ret{'language'}) {
    for my $ext ("dat", "def") {
      my $lang = "language.$ext";
      info("regenerating $lang\n");
      
      my $arg1 = "$TEXMFSYSVAR/tex/generic/config/language.$ext";
      my $arg2 = "$TEXMFLOCAL/tex/generic/config/language-local.dat";
      if ($ext eq "dat") {
        TeXLive::TLUtils::create_language_dat($localtlpdb, $arg1, $arg2);
      } else {
        TeXLive::TLUtils::create_language_def($localtlpdb, $arg1, $arg2);
      }

      if (! TeXLive::TLUtils::win32()) {
        # Use full path for external command, except on Windows.
        $lang = "$TEXMFSYSVAR/tex/generic/config/$lang";
      }
      if ($localtlpdb->option_create_formats) {
        info("running fmtutil-sys --byhyphen $lang\n");
        $errors += system("fmtutil-sys", "--byhyphen", $lang);
      }
    }
  }

  return $errors / 256; # we were accumulating wait statuses
}


# 
# remove_package removes a single package with all files (including the
# tlpobj files) and the entry from the tlpdb.
#
sub remove_package {
  my ($pkg, $localtlpdb) = @_;
  my $tlp = $localtlpdb->get_package($pkg);
  my %ret;
  if (!defined($tlp)) {
    tlwarn ("$pkg: package not present, cannot remove\n");
  } else {
    if ($pkg =~ m/^texlive\.infra/) {
      log ("Not removing $pkg, it is essential!\n");
      return;
    }
    # we have to chdir to $localtlpdb->root
    my $Master = $localtlpdb->root;
    chdir ($Master) || die "chdir($Master) failed: $!";
    my @files = $tlp->all_files;
    # also remove the .tlpobj file
    push @files, "tlpkg/tlpobj/$pkg.tlpobj";
    # and the ones from src/doc splitting
    if (-r "tlpkg/tlpobj/$pkg.source.tlpobj") {
      push @files, "tlpkg/tlpobj/$pkg.source.tlpobj";
    }
    if (-r "tlpkg/tlpobj/$pkg.doc.tlpobj") {
      push @files, "tlpkg/tlpobj/$pkg.doc.tlpobj";
    }
    #
    # we want to check that a file is only listed in one package, so
    # in case that a file to be removed is listed in another package
    # we will warn and *not* remove it
    my %allfiles;
    for my $p ($localtlpdb->list_packages) {
      next if ($p eq $pkg); # we have to skip the to be removed package
      for my $f ($localtlpdb->get_package($p)->all_files) {
      	$allfiles{$f} = $p;
      }
    }
    my @goodfiles = ();
    my @badfiles = ();
    for my $f (@files) {
      if (defined($allfiles{$f})) {
        # this file should be removed but is mentioned somewhere, too
        push @badfiles, $f;
      } else {
        push @goodfiles, $f;
      }
    }
    if ($#badfiles >= 0) {
      # warn the user
      tlwarn("The following files should be removed due to the removal of $pkg,\n");
      tlwarn("but are part of another package, too.\n");
      for my $f (@badfiles) {
        tlwarn(" $f - $allfiles{$f}\n");
      }
    }
    my @removals = &removed_dirs (@goodfiles);
    foreach my $entry (@goodfiles) {
      unlink $entry;
    }
    foreach my $entry (@removals) {
      rmdir $entry;
    }
    $localtlpdb->remove_package($pkg);
    merge_into(\%ret, $tlp->make_return_hash_from_executes("disable"));
    $ret{'mktexlsr'} = 1;
    # should we save at each removal???
    # advantage: the tlpdb actually reflects what is installed
    # disadvantage: removing a collection calls the save routine several times
    # still I consider it better that the tlpdb is in a consistent state
    $localtlpdb->save;
    # do the post removal actions
    if (defined($PostRemove{$pkg})) {
      info("running post remove action for $pkg\n");
      &{$PostRemove{$pkg}}($localtlpdb->root);
    }
  }
  return \%ret;
}


#  REMOVE
#
# tlmgr remove foo bar baz
#   will remove the packages foo bar baz itself
#   and will remove all .ARCH dependencies, too
#   and if some of them are collections it will also remove the
#   depending packages which are NOT Collections|Schemes.
#   if some of them are referenced somewhere they will not be removed
#   unless --force given
#
# tlmgr remove --no-depends foo bar baz
#   will remove the packages foo bar baz itself without any dependencies
#   but it will still remove all .ARCH dependency
#   if some of them are referenced somewhere they will not be removed
#   unless --force given
#
# tlmgr remove --no-depends-at-all foo bar baz 
#   willabsolutely only install foo bar baz not even taking .ARCH into
#   account
#
sub action_remove {
  if ($opts{"gui"}) {
    action_gui("remove");
  }
  # we do the following:
  # - (not implemented) order collections such that those depending on
  #   other collections are first removed, and then those which only
  #   depend on packages. Otherwise
  #     remove collection-latex collection-latexrecommended
  #   will not succeed
  # - first loop over all cmd line args and consider only the collections
  # - for each to be removed collection:
  #   . check that no other collections/scheme asks for that collection
  #   . remove the collection
  #   . remove all dependencies
  # - for each normal package not already removed (via the above)
  #   . check that no collection/scheme still depends on this package
  #   . remove the package
  #
  my %ret;
  $opts{"no-depends"} = 1 if $opts{"no-depends-at-all"};
  my %already_removed;
  my @more_removal;
  init_local_db();
  info("remove: dry run, no changes will be made\n") if $opts{"dry-run"};
  my @packs = @ARGV;
  #
  # we have to be carefull not to remove too many packages. The idea is
  # as follows:
  # - let A be the set of all packages to be removed from the cmd line
  # - let A* be the set of A with all dependencies expanded
  # - let B be the set of all packages
  # - let C = B \ A*, ie the set of all packages without those packages
  #   in the set of A*
  # - let C* be the set of C with all dependencies expanded
  # - let D = A* \ C*, ie the set of all packages to be removed (A*)
  #   without all the package that are still needed (C*)
  # - remove all package in D
  # - for any package in A (not in A*, in A, ie on the cmd line) that is
  #   also in C* (so a package that was asked for to be removed on the
  #   cmd line, but it isn't because someone else asks for it), warn the
  #   user that it is still needed
  #
  # remove all .ARCH dependencies, too, unless $opts{"no-depends-at-all"}
  @packs = $localtlpdb->expand_dependencies("-only-arch", $localtlpdb, @packs) unless $opts{"no-depends-at-all"};
  # remove deps unless $opts{"no-depends"}
  @packs = $localtlpdb->expand_dependencies("-no-collections", $localtlpdb, @packs) unless $opts{"no-depends"};
  my %allpacks;
  for my $p ($localtlpdb->list_packages) { $allpacks{$p} = 1; }
  for my $p (@packs) { delete($allpacks{$p}); }
  my @neededpacks = $localtlpdb->expand_dependencies($localtlpdb, keys %allpacks);
  my %packs;
  my %origpacks;
  my @origpacks = $localtlpdb->expand_dependencies("-only-arch", $localtlpdb, @ARGV) unless $opts{"no-depends-at-all"};
  for my $p (@origpacks) { $origpacks{$p} = 1; }
  for my $p (@packs) { $packs{$p} = 1; }
  for my $p (@neededpacks) { 
    if (defined($origpacks{$p})) {
      # that package was asked for to be removed on the cmd line
      my @needed = $localtlpdb->needed_by($p);
      if ($opts{"force"}) {
        info("tlmgr: $p is needed by " . join(" ", @needed) . "\n");
        info("tlmgr: still removing it due to --force\n");
      } else {
        delete($packs{$p});
        tlwarn("tlmgr: not removing $p, needed by " .
          join(" ", @needed) . "\n");
      }
    } else {
      delete($packs{$p});
    }
  }
  @packs = keys %packs;
  foreach my $pkg (sort @packs) {
    my $tlp = $localtlpdb->get_package($pkg);
    next if defined($already_removed{$pkg});
    if (!defined($tlp)) {
      info("$pkg: package not present, cannot remove\n");
    } else {
      # in the first round we only remove collections, nothing else
      # but removing collections will remove all dependencies, too
      # save the information of which packages have already been removed
      # into %already_removed.
      if ($tlp->category eq "Collection") {
        my %foo;
        info ("remove $pkg\n");
        if ($opts{"dry-run"}) {
          # we need to set $foo to something positive otherwise
          # the rest will not be run in dry_run mode
          $foo{'mktexlsr'} = 1;
        } else {
          merge_into(\%foo, &remove_package($pkg, $localtlpdb));
          logpackage("remove: $pkg");
        }
        if (keys %foo) {
          # removal was successful, so the return is at least 0x0001 mktexlsr
          # remove dependencies, too
          merge_into(\%ret, \%foo);
          $already_removed{$pkg} = 1;
        }
      } else {
        # save all the other packages into the @more_removal list to
        # be removed at the second state. Note that if a package has
        # already been removed due to a removal of a collection
        # it will be marked as such in %already_removed and not tried again
        push @more_removal, $pkg;
      }
    }
  }
  foreach my $pkg (sort @more_removal) {
    if (!defined($already_removed{$pkg})) {
      info ("remove $pkg\n");
      if (!$opts{"dry-run"}) {
        my %foo;
        merge_into(\%foo, &remove_package($pkg, $localtlpdb));
        if (keys %foo) {
          # removal was successful
          logpackage("remove: $pkg");
          merge_into(\%ret, \%foo);
          $already_removed{$pkg} = 1;
        }
      }
    }
  }
  if ($opts{"dry-run"}) {
    # stop here, don't do any postinstall actions
    return;
  } else {
    $localtlpdb->save;
    my @foo = sort keys %already_removed;
    if (@foo) {
      info("tlmgr: actually removed these packages: @foo\n");
    } else {
      info("tlmgr: no packages removed.\n");
    }
    return(\%ret);
  }
}


#  PAPER
# ARGV can look like:
#   paper a4 
#   paper letter
#   [xdvi|...|context] paper [help|papersize|--list]
#
sub action_paper {
  my %ret = ();
  action_gui("config") if $opts{"gui"};

  init_local_db();
  chomp(my $texmfsysconfig = `kpsewhich -var-value=TEXMFSYSCONFIG`);
  $ENV{"TEXMFCONFIG"} = $texmfsysconfig;

  my $action = shift @ARGV;
  if ($action =~ m/^paper$/i) {  # generic paper
    my $newpaper = shift @ARGV;
    if ($opts{"list"}) {  # tlmgr paper --list => complain.
      tlwarn("tlmgr: ignoring paper setting to $newpaper with --list\n")
        if $newpaper;  # complain if they tried to set, too.
      tlwarn("tlmgr: please specify a program before paper --list, ",
             "as in: tlmgr pdftex paper --list\n");

    } elsif (!defined($newpaper)) {  # tlmgr paper => show all current sizes.
      TeXLive::TLPaper::paper_all($texmfsysconfig,undef);
      
    } elsif ($newpaper !~ /^(a4|letter)$/) {  # tlmgr paper junk => complain.
      $newpaper = "the empty string" if !defined($newpaper);
      tlwarn("tlmgr: expected `a4' or `letter' after paper, not $newpaper\n");

    } else { # tlmgr paper {a4|letter} => do it.
      merge_into(\%ret,TeXLive::TLPaper::paper_all($texmfsysconfig,$newpaper));
    }

  } else {  # program-specific paper
    my $prog = $action;     # first argument is the program to change
    my $arg = shift @ARGV;  # get "paper" argument
    if (!defined($arg) || $arg ne "paper") {
      $arg = "the empty string." if ! $arg;
      tlwarn("tlmgr: expected `paper' after $prog, not $arg\n");
      return;
    }
    # the do_paper progs check for the argument --list, so if given
    # restore it to the cmd line.
    unshift(@ARGV, "--list") if $opts{"list"};
    merge_into(\%ret, TeXLive::TLPaper::do_paper($prog,$texmfsysconfig,@ARGV));
  }
  return \%ret;
}


#  SHOW
#
sub action_show {
  if ($opts{"gui"}) {
    action_gui("config");
  }
  init_local_db();
  foreach my $pkg (@ARGV) {
    my $tlpdb = $localtlpdb;
    my $tlp = $localtlpdb->get_package($pkg);
    my $installed = 0;
    if (!$tlp) {
      if (!$tlmediatlpdb) {
        init_tlmedia();
      }
      $tlp = $tlmediatlpdb->get_package($pkg);
      $tlpdb = $tlmediatlpdb;
    } else {
      $installed = 1;
    }
    if ($tlp) {
      my @colls;
      if ($tlp->category ne "Collection" && $tlp->category ne "Scheme") {
        @colls = $localtlpdb->needed_by($pkg);
        if (!@colls) {
          # not referenced in the local tlpdb, so try the remote here, too
          if (!$tlmediatlpdb) {
            init_tlmedia();
          }
          @colls = $tlmediatlpdb->needed_by($pkg);
        }
      }
      print "Package:    ", $tlp->name, "\n";
      print "Category:   ", $tlp->category, "\n";
      print "ShortDesc:  ", $tlp->shortdesc, "\n" if ($tlp->shortdesc);
      print "LongDesc:   ", $tlp->longdesc, "\n" if ($tlp->longdesc);
      print "Installed:  ", ($installed ? "Yes" : "No"), "\n";
      print "Revision:   ", $tlp->revision, "\n" if ($installed);
      print "Collection: ", @colls, "\n" if (@colls);
      if ($opts{"list"}) {
        print "Included files, by type:\n";
        # if the package has a .ARCH dependency we also list the files for
        # those packages
        my @todo = $tlpdb->expand_dependencies("-only-arch", $tlpdb, ($pkg));
        for my $d (sort @todo) {
          my $foo = $tlpdb->get_package($d);
          if (!$foo) {
            warn "That should not happen, no such package here!";
            next;
          }
          if ($d ne $pkg) {
            print "depending package $d:\n";
          }
          if ($foo->runfiles) {
            print "run files:\n";
            for my $f (sort $foo->runfiles) { print "  $f\n"; }
          }
          if ($foo->srcfiles) {
            print "source files:\n";
            for my $f (sort $foo->srcfiles) { print "  $f\n"; }
          }
          if ($foo->docfiles) {
            print "doc files:\n";
            for my $f (sort $foo->docfiles) { 
              print "  $f"; 
              my $dfd = $foo->docfiledata;
              if (defined($dfd->{$f})) {
                for my $k (keys %{$dfd->{$f}}) {
                  print " $k=\"", $dfd->{$f}->{$k}, '"';
                }
              }
              print "\n";
            }
          }
          # in case we have them
          if ($foo->allbinfiles) {
            print "bin files (all architectures):\n";
            for my $f (sort $foo->allbinfiles) { print " $f\n"; }
          }
        }
      }
      print "\n";
    } else {
      printf STDERR "tlmgr: cannot find $pkg\n";
    }
  }
  return;
}

#  SYMLINKS
#
sub action_symlinks {
  my %ret;
  my $what = shift @ARGV;
  if (!defined($what) || ($what !~ m/^(add|remove)$/i)) {
    tlwarn("action symlinks needs one argument, either add or remove\n");
    return;
  }
  init_local_db();
  if ($what =~ m/^add$/i) {
    $localtlpdb->add_symlinks();
  } elsif ($what =~ m/^remove$/i) {
    # remove symlinks
    $localtlpdb->remove_symlinks();
  } else {
    # that should not happen
    tlwarn("that should not happen, action_symlinks what=$what\n");
    exit 1;
  }
  return;
}

#  SEARCH
#
sub action_search {
  my %ret;
  my $r = shift @ARGV;
  my $ret = "";
  my $tlpdb;
  init_local_db();
  if ($opts{"global"}) {
    init_tlmedia();
    $tlpdb = $tlmediasrc->tlpdb;
  } else {
    $tlpdb = $localtlpdb;
  }
  foreach my $pkg ($tlpdb->list_packages) {
    if ($opts{"file"}) {
      my @ret = grep(m;$r;, $tlpdb->get_package($pkg)->all_files);
      if (@ret) {
        print "$pkg:\n";
        foreach (@ret) {
          print "\t$_\n";
        }
      }
    } else {
      next if ($pkg =~ m/\./);
      my $t = $tlpdb->get_package($pkg)->shortdesc;
      $t |= "";
      my $lt = $tlpdb->get_package($pkg)->longdesc;
      $lt |= "";
      if (($pkg =~ m/$r/) || ($t =~ m/$r/) || ($lt =~ m/$r/)) {
        $ret .= " $pkg - $t\n";
      }
    }
  }
  print "$ret";
  return;
}


#  RESTORE
# 
sub action_restore {
  # tlmgr restore --backupdir dir 
  #   lists all packages with all revisions
  # tlmgr restore --backupdir dir pkg
  #   lists all revisions of pkg
  # tlmgr restore --backupdir dir pkg rev
  #   restores pkg to revision rev
  my %ret;
  # check the backup dir argument
  if ($opts{"backupdir"}) {
    my $ob = abs_path($opts{"backupdir"});
    $ob && ($opts{"backupdir"} = $ob);
    if (! -d $opts{"backupdir"}) {
      tlwarn ("backupdir argument $opts{'backupdir'} is not a directory.\n");
      tlwarn ("Don't know from where to restore backups, terminating.\n");
      exit 1;
    }
  } else {
    # no argument, check for presence in TLPDB
    $opts{"backupdir"} = $localtlpdb->option("backupdir");
    if (!$opts{"backupdir"}) {
      tlwarn ("Don't know from where to restore backups, terminating.\n");
      exit 1;
    }
    # we are stil here, there is something set in tlpdb
    my $ob = abs_path($opts{"backupdir"});
    $ob && ($opts{"backupdir"} = $ob);
    if (! -d $opts{"backupdir"}) {
      tlwarn ("backupdir as set in tlpdb $opts{'backupdir'} is not a directory.\n");
      tlwarn ("Don't know from where to restore backups, terminating.\n");
      exit 1;
    }
  }

  info("restore: dry run, no changes will be made\n") if $opts{"dry"};

  # initialize the hash(packages) of hash(revisions)
  my %backups;
  opendir (DIR, $opts{"backupdir"}) || die "opendir($opts{'backupdir'}) failed: $!";
  my @dirents = readdir (DIR);
  closedir (DIR) || warn "closedir($opts{'backupdir'}) failed: $!";
  for my $dirent (@dirents) {
    next if (-d $dirent);
    next if ($dirent !~ m/^(.*)\.r([0-9]+)\.tar\.lzma$/);
    $backups{$1}->{$2} = 1;
  }
  my ($pkg, $rev) = @ARGV;
  if (!defined($pkg)) {
    if (keys %backups) {
      print "Available backups:\n";
      foreach my $p (sort keys %backups) {
        print "$p: ";
        my @rs = sort (keys %{$backups{$p}});
        print "@rs\n";
      }
    } else {
      print "No backups available in $opts{'backupdir'}\n";
    }
    finish(0);
  }
  if (!defined($rev)) {
    print "Available backups for $pkg: ";
    my @rs = sort (keys %{$backups{$pkg}});
    print "@rs\n";
    finish(0);
  }
  # we did arrive here, so we try to restore ...
  if (defined($backups{$pkg}->{$rev})) {
    if (!$opts{"force"}) {
      print "Do you really want to restore $pkg to revision $rev (y/N): ";
      my $yesno = <STDIN>;
      if ($yesno !~ m/^y(es)?$/i) {
        print "Ok, cancelling the restore!\n";
        finish(0);
      }
    }
    print "Restoring $pkg, $rev from $opts{'backupdir'}/${pkg}.r${rev}.tar.lzma\n";
    if (!$opts{"dry"}) {
      init_local_db(1);
      # first remove the package, then reinstall it
      # this way we get rid of useless files
      $opts{"backupdir"} = abs_path($opts{"backupdir"});
      merge_into(\%ret, &remove_package($pkg, $localtlpdb));
      TeXLive::TLMedia->_install_package("$opts{'backupdir'}/${pkg}.r${rev}.tar.lzma" , [] ,$localtlpdb);
      logpackage("restore: $pkg ($rev)");
      # now we have to read the .tlpobj file and add it to the DB
      my $tlpobj = TeXLive::TLPOBJ->new;
      $tlpobj->from_file($localtlpdb->root . "/tlpkg/tlpobj/$pkg.tlpobj");
      $localtlpdb->add_tlpobj($tlpobj);
      merge_into(\%ret, $localtlpdb->get_package($pkg)->make_return_hash_from_executes("enable"));
      $localtlpdb->save;
    }
  } else {
    print "revision $rev for $pkg is not present in $opts{'backupdir'}\n";
  }
  return \%ret;
}

sub action_backup {
  init_local_db(1);
  # --clean argument
  # can be either -1 ... don't clean
  #               0  ... remove all backups
  #               N  ... keep only N backups
  # that parallels the value of autoclean in the configuration
  # we have to be careful, because if simply --clean is given, we should
  # check for the value saved in the tlpdb, and if that is not present
  # do nothing. 
  # We have set clean to clean:-99 which makes -99 the default value
  # if only --clean is given without any argument
  # !defined($opts{"clean"})  -> no --clean given
  # $opts{"clean"} = -99      -> --clean without argument given, check tlpdb
  # $opts{"clean"} = -1, 0, N -> --clean=N given, check argument
  #
  my $clean_mode = 0;
  $clean_mode = 1 if defined($opts{"clean"});
  if ($clean_mode) {
    if ($opts{"clean"} == -99) {
      # we need to check the tlpdb
      $opts{"clean"} = $localtlpdb->option("autobackup");
      if (!$opts{"clean"}) {
        tlwarn ("--clean given without an argument, but no default clean\n");
        tlwarn ("mode specified in the tlpdb, terminating.\n");
        exit 1;
      }
    }
    # now $opts{"clean"} is something, but maybe not a number, check for
    # validity
    if ($opts{"clean"} =~ m/^(-1|[0-9]+)$/) {
      # get rid of leading zeros etc etc
      $opts{"clean"} = $opts{"clean"} + 0;
    } else {
      tlwarn ("clean mode as specified on the command line or as given by default\n");
      tlwarn ("must be an integer larger or equal than -1, terminating.\n");
      exit 1;
    }
  }
  # check the backup dir argument
  if ($opts{"backupdir"}) {
    my $ob = abs_path($opts{"backupdir"});
    $ob && ($opts{"backupdir"} = $ob);
    if (! -d $opts{"backupdir"}) {
      tlwarn ("backupdir argument $opts{'backupdir'} is not a directory.\n");
      if ($clean_mode) {
        tlwarn ("Cannot clean a non existing directory, terminating.\n");
      } else {
        tlwarn ("Don't know where to save backups, terminating.\n");
      }
      exit 1;
    }
  } else {
    # no argument, check for presence in TLPDB
    $opts{"backupdir"} = $localtlpdb->option("backupdir");
    if (!$opts{"backupdir"}) {
      if ($clean_mode) {
        tlwarn ("Cannot clean an unknown directory, terminating.\n");
      } else {
        tlwarn ("Don't know where to save backups, terminating.\n");
      }
      exit 1;
    }
    # we are stil here, there is something set in tlpdb
    my $ob = abs_path($opts{"backupdir"});
    $ob && ($opts{"backupdir"} = $ob);
    if (! -d $opts{"backupdir"}) {
      tlwarn ("backupdir as set in tlpdb $opts{'backupdir'} is not a directory.\n");
      if ($clean_mode) {
        tlwarn ("Cannot clean a non existing directory, terminating.\n");
      } else {
        tlwarn ("Don't know where to save backups, terminating.\n");
      }
      exit 1;
    }
  }

  my %ret;
  my @todo;
  if ($opts{"all"}) {
    @todo = $localtlpdb->list_packages;
  } else {
    @todo = @ARGV;
    @todo = $localtlpdb->expand_dependencies("-only-arch", $localtlpdb, @todo);
  }
  if (!@todo) {
    printf "tlmgr backup takes either a list of packages or --all\n";
  }
  foreach my $pkg (@todo) {
    if ($clean_mode) {
      clear_old_backups ($pkg, $opts{"backupdir"}, $opts{"clean"}, $opts{"dry-run"});
    } else {
      my $tlp = $localtlpdb->get_package($pkg);
      info("saving current status of $pkg to $opts{'backupdir'}/${pkg}.r" . 
        $tlp->revision . "tar.lzma\n");
      if (!$opts{"dry-run"}) {
        $tlp->make_container("lzma", $localtlpdb->root, 
                             $opts{"backupdir"}, "${pkg}.r" . $tlp->revision);
      }
    }
  }
  return(\%ret);
}

#
sub write_w32_updater {
  my @w32_updated = @_;
  my $media = $tlmediasrc->media;
  my $mediatlpdb = $tlmediasrc->tlpdb;
  # we have to download/copy also the src/doc files if necessary!
  my $container_src_split = $mediatlpdb->config_src_container;
  my $container_doc_split = $mediatlpdb->config_doc_container;
  # get options about src/doc splitting from $totlpdb
  # TT: should we use local options to decide about install of doc & src?
  my $opt_src = $localtlpdb->option_install_srcfiles;
  my $opt_doc = $localtlpdb->option_install_docfiles;
  my $root = $localtlpdb->root;
  my $temp = "$root/temp";
  my $repo = $mediatlpdb->root . "/$Archive";
  TeXLive::TLUtils::mkdirhier($temp);
  tlwarn("Backup option not implemented for infrastructure update.\n") if ($opts{"backup"});
  if ($media eq 'DVD') {
    tlwarn("Creating updater from DVD currently not implemented!\n");
    tlwarn("But it should not be necessary!\n");
    return 1; # abort
  }
  my ($mediatlp, $localtlp, $opt_real_doc, $sysret, $size, $md5, $fullname,
      $oldrev, $newrev, $pkg_part, @pkg_parts, @updater_args);
  my $totalnr = $#w32_updated + 1;
  my $currnr = 0;
  foreach my $pkg (@w32_updated) {
    $mediatlp = $mediatlpdb->get_package($pkg);
    $localtlp = $localtlpdb->get_package($pkg);
    $oldrev = $localtlp->revision;
    $newrev = $mediatlp->revision;
    # we do install documenation files for category Documentation even if option_install_docfiles is false
    $opt_real_doc = ($mediatlp->category =~ m/documentation/i) ? 1 : $opt_doc;
    @pkg_parts = ($pkg);
    push(@pkg_parts, "$pkg.source") if ($container_src_split && $opt_src && $mediatlp->srcfiles);
    push(@pkg_parts, "$pkg.doc") if ($container_doc_split && $opt_real_doc && $mediatlp->docfiles);
    $currnr++;
    if ($opts{"dry-run"}) {
      info("[$currnr/$totalnr] update: $pkg ($oldrev -> $newrev)\n");
      next;
    }
    # create backup; make_container expects file name in a format: some-name.r[0-9]+
    ($size, $md5, $fullname) = $localtlp->make_container("tar", $root, $temp, "__BACKUP_$pkg.r$oldrev");
    if ($size <= 0) {
      tlwarn("Creation of backup container of $pkg failed.\n");
      return 1; # backup failed? abort
    }
    foreach $pkg_part (@pkg_parts) {
      if ($media eq 'CD') {
        copy("$repo/$pkg_part.tar.lzma", "$temp");
      } else { # net
        TeXLive::TLUtils::download_file("$repo/$pkg_part.tar.lzma", "$temp/$pkg_part.tar.lzma");
      }
      # now we should have the file present
      if (!-r "$temp/$pkg_part.tar.lzma") {
        tlwarn("Couldn't get $pkg_part.tar.lzma, that is bad\n");
        return 1; # abort
      } 
      # unpack lzma archive
      $sysret = system("$::progs{'lzmadec'} < \"$temp/$pkg_part.tar.lzma\" > \"$temp/$pkg_part.tar\"");
      if ($sysret) {
        tlwarn("Couldn't unpack $pkg_part.tar.lzma\n");
        return 1; # unpack failed? abort
      }
      unlink("$temp/$pkg_part.tar.lzma"); # we don't need that archive anymore
      push @updater_args, $pkg_part, $oldrev, $newrev;
    }
  }
  return 0 if ($opts{"dry-run"}); # nothing else to do
  # write the updater script file
  open UPDATER, ">$root/temp/run-updater-w32"
    or die "Cannot create updater: $!";
  print UPDATER '@echo off
setlocal enableextensions
(set errorlevel=)
>nul copy /y "%~dp0..\texmf\scripts\texlive\updater-w32.bat" "%~dp0updater-w32.bat"
call "%~dp0updater-w32" ' . join(' ',@updater_args) . '
if errorlevel 1 call exit /b %%errorlevel%%
start /b cmd /c del "%~f0" "%~dp0updater-w32.bat"';
  close UPDATER;
  return 0;
}


#  UPDATE
#
# tlmgr update foo
#   if foo is of type Package|Documentation it will update only foo
#     and the respective .ARCH dependencies
#   if foo is of type Collection|Scheme it will update itself AND
#     will check all depending packs of type NOT(COllection|Scheme)
#     for necessary updates
#
# tlmgr update --no-depends foo
#   as above, but will not check for depends of Collections/Schemes
#   but it will still update .ARCH deps
#
# tlmgr update --no-depends-at-all foo
#   will absolutely only update foo not even taking .ARCH into account
# 
# TLMedia->install_package INSTALLS ONLY ONE PACKAGE, no deps whatsoever
# anymore. That has all to be done by hand.
# 
sub machine_line {
  my ($flag1) = @_;
  my $ret = 0;
  if ($flag1 eq "-ret") {
    $ret = 1;
    shift;
  }
  my ($pkg, $flag, $lrev, $rrev, @args) = @_;
  $lrev ||= "-";
  $rrev ||= "-";
  $flag ||= "?";
  my $str = "$pkg\t$flag\t$lrev\t$rrev\t" . join("\t", @args) . "\n";
  return($str) if $ret;
  print $str;
}

sub action_update {
  if ($opts{"gui"}) {
    action_gui("update");
  }
  init_local_db(1);
  $opts{"no-depends"} = 1 if $opts{"no-depends-at-all"};
  my %ret;
  
  init_tlmedia();
  info("update: dry run, no changes will be made\n") if $opts{"dry-run"};

  # if the update is not for one of the critical packages then do
  # check for updates to tlmgr and die unless either --force or --list
  # is given
  my $other_updates_asked_for = 0;
  if ($opts{"all"}) {
    $other_updates_asked_for = 1;
  } else {
    for my $p (@ARGV) {
      my $matched = 0;
      for my $cp (@CriticalPackagesList) {
        # we match for initial package name, that shold be fine
        if ($p =~ m/^$cp/) {
          $matched = 1;
          last;
        }
      }
      if (!$matched) {
        $other_updates_asked_for = 1;
        last;
      }
    }
  } 
  if ($other_updates_asked_for) {
    if (check_for_critical_updates($localtlpdb, $tlmediatlpdb)) {
      if ($opts{"force"}) {
        tlwarn("$0: Continuing due to --force.\n");
      } elsif ($opts{"list"}) {
        # do not warn here
      } else {
        if ($::gui_mode) {
          # return here and don't do any updates
          return;
        } else {
          die "$0: Exiting, please read above warning.\n";
        }
      }
    }
  }

  # do backup dir checking now so that we don't run into troubles
  # later, and exit if that doesn't work
  if ($opts{"backupdir"}) {
    $opts{"backupdir"} = abs_path($opts{"backupdir"});
    if (! -d $opts{"backupdir"}) {
      tlwarn("Argument for --backupdir must be an existing directory. Terminating.\n");
      exit 1;
    }
  }

  my $autobackup = 0;
  # check for the tlpdb option autobackup, and if present and true (!= 0)
  # assume we are doing backups
  if (!$opts{"backup"}) {
    $autobackup = $localtlpdb->option("autobackup");
    if ($autobackup) {
      # check the format, we currently allow only natural numbers, and -1
      if ($autobackup eq "-1") {
        debug ("Automatic backups activated, keeping \\infty backups.\n");
        $opts{"backup"} = 1;
      } elsif ($autobackup eq "0") {
        debug ("Automatic backups disabled.\n");
      } elsif ($autobackup =~ m/^[0-9]+$/) {
        debug ("Automatic backups activated, keeping $autobackup backups.\n");
        $opts{"backup"} = 1;
      } else {
        tlwarn ("Option autobackup can only be an integer >= -1.\n");
        tlwarn ("Disabling auto backups.\n");
        $localtlpdb->option("autobackup", 0);
        $autobackup = 0;
      }
    }
  }

  # cmd line --backup, we check for --backupdir, and if that is not given
  # we try to get the default from the tlpdb. If that doesn't work, exit.
  if ($opts{"backup"}) {
    my $diebackupdir = 0;
    if (!$opts{"backupdir"}) {
      $opts{"backupdir"} = $localtlpdb->option("backupdir");
      if ($opts{"backupdir"}) {
        # check again:
        $opts{"backupdir"} = abs_path($opts{"backupdir"});
        $diebackupdir = 1 if (! -d $opts{"backupdir"});
      } else {
        # not set in the tlpdb, and not set on cmd line, but asked for
        # --backup 
        $diebackupdir = 1;
      }
    } 
    # no else branch necessary, we already checked that --backupdir if 
    # given is ok, see above
    if ($diebackupdir) {
      tlwarn("You have asked for backups, but the backup directory as specified\n");
      tlwarn("in the local TLPDB or the cmd line does not exists, exiting.\n");
      exit 1;
    }
  }

  # finally, if we have --backupdir, but no --backup, just enable it
  $opts{"backup"} = 1 if $opts{"backupdir"};
      
  debug("Doing backups to $opts{'backupdir'}\n") if $opts{"backup"};

  # these two variables are used throughout this function
  my $root = $localtlpdb->root;
  my $temp = "$root/temp";

  # remove old _BACKUP packages that have piled up in temp
  # they can be recognized by their name starting with __BACKUP_
  for my $f (<$temp/__BACKUP_*>) {
    unlink($f) unless $opts{"dry-run"};
  }

  my @todo;
  my %removals;
  my %forcermpkgs;
  my %newpkgs;
  # check for new/removed/forcibly removed packages.
  # we start from the list of installed collections in the local tlpdb
  # which are also present in the remote database
  # and expand this list once with expand_dependencies in the local tlpdb
  # and once in the tlmedia tlpdb. Then we compare the lists
  # let A = set of local expansions
  #     B = set of remote expansions
  # then we should(?) have
  #     B \ A  set of new packages
  #     A \ B  set of packages removed on the server
  #     A \cup B set of packages which should be checked for forcible removal
  #
  my @colls = ();
  for my $p ($localtlpdb->collections) {
    push @colls, $p
      if defined($tlmediatlpdb->get_package($p));
  }
  my @localexpansion = 
    $localtlpdb->expand_dependencies($localtlpdb, @colls);
  my @remoteexpansion = 
    $tlmediatlpdb->expand_dependencies($localtlpdb, @colls);
  for my $p (@remoteexpansion) {
    $newpkgs{$p} = 1;
  }
  for my $p (@localexpansion) {
    delete($newpkgs{$p});
    $removals{$p} = 1;
  }
  for my $p (@remoteexpansion) {
    delete($removals{$p});
  }
  for my $p (@localexpansion) {
    # intersection, don't check A\B and B\A
    next if $newpkgs{$p};
    next if $removals{$p};
    my $tlp = $localtlpdb->get_package($p);
    if (!defined($tlp)) {
      $forcermpkgs{$p} = 1;
    }
  }
  debug ("tlmgr: new pkgs: " . join("\n\t",keys %newpkgs) . "\n");
  debug ("tlmgr: deleted : " . join("\n\t",keys %removals) . "\n");
  debug ("tlmgr: forced  : " . join("\n\t",keys %forcermpkgs) . "\n");

  if ($opts{"all"} || $opts{"list"}) {
    @todo = $localtlpdb->list_packages;
  } else {
    @todo = @ARGV;
  }
  # don't do anything if we have been invoced in a strange way
  if (!@todo) {
    tlwarn("tlmgr update: please specify a list of packages or --all.\n");
  }

  # update all .ARCH dependencies, too, unless $opts{"no-depends-at-all"}:
  @todo = $tlmediatlpdb->expand_dependencies("-only-arch", $localtlpdb, @todo)
    unless $opts{"no-depends-at-all"}; 
  #
  # update general deps unless $opts{"no-depends"}:
  @todo = $tlmediatlpdb->expand_dependencies("-no-collections",$localtlpdb,@todo)
    unless $opts{"no-depends"}; 

  #
  # we first collect the list of packages to be actually updated or installed
  my @updated;
  my @w32_updated; # win32 special packages that need the updater batch script
  my @new;
  my @addlines;

  foreach my $pkg (sort @todo) {
    next if ($pkg =~ m/^00texlive/);
    my $tlp = $localtlpdb->get_package($pkg);
    if (!defined($tlp)) {
      # if the user has forcibly removed (say) bin-makeindex, then the
      # loop above has no way to add bin-makeindex.ARCH into the
      # %forcermpkgs hash, but the .ARCH will still be in the dependency
      # expansion.  So try both with and without the .ARCH extension.
      (my $pkg_noarch = $pkg) =~ s/\.[^.]*$//;
      my $forcerm_coll = $forcermpkgs{$pkg} || $forcermpkgs{$pkg_noarch};
      
      # similarly for new packages.  If latexmk is new, latexmk.ARCH
      # will be in the dependency expansion, and we want it.
      my $newpkg_coll = $newpkgs{$pkg} || $newpkgs{$pkg_noarch};
      if ($forcerm_coll) {
        if ($::machinereadable) {
          # TODO should we add a revision number
          push @addlines, 
            machine_line("-ret", $pkg, $FLAG_FORCIBLE_REMOVED, "-", "-", "-");
        } else {
          info("skipping forcibly removed package $pkg\n");
        }
        next;
      } elsif ($newpkg_coll) {
        # do nothing here, it will be reported below.
      } elsif (defined($removals{$pkg})) {
        # skipping this package, it has been removed due to server removal
        # and has already been removed
        next;
      } else {
        tlwarn("\ntlmgr: $pkg mentioned, neither new nor forcibly removed\n");
        next;
      }
      # install new packages
      my $mediatlp = $tlmediatlpdb->get_package($pkg);
      if (!defined($mediatlp)) {
        tlwarn("\nShould not happen: $pkg not found in $location\n");
        next;
      }
      my $mediarev = $mediatlp->revision;
      push @new, $pkg;
      next;
    }
    my $rev = $tlp->revision;
    my $mediatlp = $tlmediatlpdb->get_package($pkg);
    if (!defined($mediatlp)) {
      debug("$pkg cannot be found in $location\n");
      next;
    }
    my $mediarev = $mediatlp->revision;
    if ($rev < $mediarev) {
      if (win32() && !$opts{"list"} && ($pkg =~ m/$WinSpecialUpdatePackagesRegexp/)) {
        # w32 specials are treated as other packages for --list
        push @w32_updated, $pkg;
      } else {
        push @updated, $pkg;
      }
    } elsif ($rev > $mediarev) {
      if ($::machinereadable) {
        push @addlines,
          machine_line("-ret", $pkg, $FLAG_REVERSED_UPDATE, $rev, $mediarev, "-");
      } else {
        info("$pkg: local revision ($rev) is newer than revision in $location"
            . " ($mediarev), not updating.\n");
      }
    }
  }
  for my $i (sort @new) {
    debug("$i new package\n");
  }
  for my $i (sort @updated) {
    debug("$i upd package\n");
  }

  # number calculation
  # without w32 special packages, those are dealt with in the updater batch script
  my $totalnr = $#new + $#updated + 2; 
  my $nrupdated = 0;
  my $currnr = 1;
  # sizes_of_packages returns the sizes of *all* packages if nothing
  # is passed over, so if @new and @updated both are empty we will 
  # get something wrong back, namely the total size of all packages
  my %sizes;
  #  for size calculation we also want w32 special packages
  if (@new || @updated || @w32_updated) {
    %sizes = %{$tlmediatlpdb->sizes_of_packages( 
      $localtlpdb->option_install_srcfiles,
      $localtlpdb->option_install_docfiles,
      @new, @updated, @w32_updated)};
  } else {
    $sizes{'__TOTAL__'} = 0;
  }

  print "total-bytes\t$sizes{'__TOTAL__'}\n" if $::machinereadable;
  print "end-of-header\n" if $::machinereadable;

  # print deferred machine-readable lines after the header
  for (@addlines) { print; }

  #
  # we have to remove all the stuff before we install other packages
  # to support moving of files from one package to another.
  # remove the packages that have disappeared:
  for my $p (keys %removals) {
    if ($opts{"no-remove"}) {
      info("not removing $p due to -no-remove (removed on server)\n");
    } else {
      if ($::machinereadable) {
        # TODO version numbers
        machine_line($p, $FLAG_REMOVE, "-", "-", "-");
      } else {
        info("remove $p (removed on server)\n");
      }
    }
    if (!($opts{"dry-run"} or $opts{"list"} or $opts{"no-remove"})) {
      merge_into(\%ret, &remove_package($p, $localtlpdb));
      logpackage("remove: $p");
    }
  }


  # install all the new packages first
  for my $pkg (sort @new) {
    # install new packages
    my $mediatlp = $tlmediatlpdb->get_package($pkg);
    if (!defined($mediatlp)) {
      tlwarn("\nShould not happen: $pkg not found in $location\n");
      next;
    }
    my $mediarev = $mediatlp->revision;
    if ($::machinereadable) {
      machine_line($pkg, $FLAG_AUTOINSTALL, "-", $mediatlp->revision, $sizes{$pkg});
    } else {
      info("[$currnr/$totalnr] auto-install: $pkg\n");
    }
    $currnr++;
    next if ($opts{"dry-run"} || $opts{"list"});
    my $foo = $tlmediasrc->install_package($pkg, $localtlpdb);
    if (ref $foo) {
      # installation succeeded because we got a reference
      merge_into (\%ret, $foo);
      logpackage("auto-install new: $pkg ($mediarev)");
      $nrupdated++;
    } else {
      tlwarn("$0: couldn't install new package $pkg\n");
    }
  }

  #
  foreach my $pkg (sort @updated) {
    next if ($pkg =~ m/^00texlive/);
    my $tlp = $localtlpdb->get_package($pkg);
    # we checked already that this package is present!
    my $unwind_package;
    my $remove_unwind_container = 0;
    my $rev = $tlp->revision;
    my $mediatlp = $tlmediatlpdb->get_package($pkg);
    if (!defined($mediatlp)) {
      debug("$pkg cannot be found in $location\n");
      next;
    }
    my $mediarev = $mediatlp->revision;
    $nrupdated++;
    if ($opts{"list"}) {
      if ($::machinereadable) {
        machine_line($pkg, $FLAG_UPDATE, $rev, $mediarev, $sizes{$pkg});
      } else {
        info("$pkg: local: $rev, source: $mediarev\n");
      }
      next;
    }
    
    if ($opts{"backup"} && !$opts{"dry-run"}) {
      $tlp->make_container("lzma", $root, 
                           $opts{"backupdir"}, "${pkg}.r" . $tlp->revision);
      $unwind_package = 
        "$opts{'backupdir'}/${pkg}.r" . $tlp->revision . ".tar.lzma";

      if ($autobackup) {
        # in case we do auto backups we remove older backups
        clear_old_backups($pkg, $opts{"backupdir"}, $autobackup);
      }
    }

    if ($::machinereadable) {
      machine_line($pkg, $FLAG_UPDATE, $rev, $mediarev, $sizes{$pkg});
    } else {
      info("[$currnr/$totalnr] update: $pkg ($rev -> $mediarev)");
    }
    $currnr++;
    if ($opts{"dry-run"}) {
      info("\n") unless $::machinereadable;
      next;
    } else {
      info(" ... ") unless $::machinereadable;  # more to come
    }

    if (!$unwind_package) {
      # no backup was made, so let us create a temporary .tar file
      # of the package
      my $tlp = $localtlpdb->get_package($pkg);
      my ($s, $m, $fullname) = $tlp->make_container("tar", $root, $temp, 
                                    "__BACKUP_${pkg}.r" . $tlp->revision);
      if ($s <= 0) {
        tlwarn("\n$0: Creation of backup container of $pkg failed.\n");
        tlwarn("Continuing to update other packages, please retry...\n");
        # we should try to update other packages at least
        next;
      }
      $remove_unwind_container = 1;
      $unwind_package = "$fullname";
    }
    # first remove the package, then reinstall it
    # this way we get rid of useless files
    # force the deinstallation since we will reinstall it
    #
    # the remove_package should also remove empty dirs in case
    # a dir is changed into a file
    merge_into(\%ret, &remove_package($pkg, $localtlpdb));
    my $foo = $tlmediasrc->install_package($pkg, $localtlpdb);
    if (ref $foo) {
      # installation succeeded because we got a reference
      merge_into (\%ret, $foo);
      logpackage("update: $pkg ($rev -> $mediarev)");
      unlink($unwind_package) if $remove_unwind_container;
    } else {
      # install_package returned a scalar, so error.
      # now in fact we should do some cleanup, removing files and
      # dirs from the new package before re-installing the old one.
      # TODO
      logpackage("failed update: $pkg ($rev -> $mediarev)");
      tlwarn("\n\nInstallation of new version of $pkg did fail, trying to unwind.\n");
      if (win32()) {
        # w32 is notorious for not releasing a file immediately
        # we experienced permission denied errors 
        my $newname = $unwind_package;
        $newname =~ s/__BACKUP/___BACKUP/;
        copy ("-f", $unwind_package, $newname);
        # try to remove the file if has been created by us
        unlink($unwind_package) if $remove_unwind_container;
        # and make sure that the temporary file is removed in any case
        $remove_unwind_container = 1;
        $unwind_package = $newname;
      }
      my $instret = TeXLive::TLMedia->_install_package("$unwind_package",
                                                       [], $localtlpdb);
      if ($instret) {
        # now we have to include the tlpobj
        my $tlpobj = TeXLive::TLPOBJ->new;
        $tlpobj->from_file($root . "/tlpkg/tlpobj/$pkg.tlpobj");
        $localtlpdb->add_tlpobj($tlpobj);
        $localtlpdb->save;
        logpackage("restore: $pkg ($rev)");
        tlwarn("Restoring old package state succeeded.\n");
      } else {
        logpackage("failed restore: $pkg ($rev)");
        tlwarn("Restoring of old package did NOT succeed.\n");
        tlwarn("Most likely repair: run tlmgr install $pkg and hope.\n");
      }
      unlink($unwind_package) if $remove_unwind_container;
    }
    info("done\n") unless $::machinereadable;
  }
  
  if (win32() && !$opts{"list"} && @w32_updated) {
    info("Preparing TeX Live infrastructure update...\n");
    my $ret = write_w32_updater(@w32_updated);
    tlwarn ("Aborting infrastructure update.\n") if ($ret);
  }
  
  if (($nrupdated == 0) && ($tlmediasrc->media ne "NET") && $opts{"all"}) {
    # for all but net updates we warn if nothing is updated
    tlwarn("\nNothing to update.\n");
    tlwarn("\nYour installation is set up to look on the disk for updates.\n");
    tlwarn("To install from the Internet for this one time only, run\n");
    tlwarn("  tlmgr -location $TeXLiveURL\n");
    tlwarn("\nTo change the default for all future updates, run\n");
    tlwarn("  tlmgr option location $TeXLiveURL\n\n");
  }
  return(\%ret);
}


#  INSTALL
#
# tlmgr install foo bar baz
#   will create the closure under dependencies of {foo,bar,baz}, i.e. all
#   dependencies recursively down to the last package, and install all
#   the packages that are necessary
#
# tlmgr install --no-depends foo bar baz
#   will *only* install these three packages (if they are not already installed
#   but it will STILL INSTALL foo.ARCH if they are necessary.
#
# tlmgr install --no-depends-at-all foo bar baz
#   will absolutely only install these three packages, and will not even
#   take .ARCH deps into account
#
# tlmgr install --reinstall ...
#   behaves exactely like without --reinstall BUT the following two 
#   differences:
#   . dependencies are not expanded from collection to collection, so
#     if you reinstall a collection then all its dependencies of type
#     Package will be reinstalled, too, but not the dependencies on
#     other collection, because that would force the full reinstallation
#     of the whole installation
#   . it does not care for whether a package seems to be installed or
#     not (that is the --reinstall)
#
# TLMedia->install_package does ONLY INSTALL ONE PACKAGE, no deps whatsoever
# anymore!  That has all to be done by hand.
# 
sub action_install {
  if ($opts{"gui"}) {
    action_gui("install");
  }
  init_local_db(1);
  # initialize the TLMedia from $location
  $opts{"no-depends"} = 1 if $opts{"no-depends-at-all"};
  my %ret;
  init_tlmedia();

  # check for updates to tlmgr itself, and die unless --force is given
  if (check_for_critical_updates( $localtlpdb, $tlmediatlpdb)) {
    if ($opts{"force"}) {
      tlwarn("Continuing due to --force\n");
    } else {
      if ($::gui_mode) {
        # return here and don't do any updates
        return;
      } else {
        die "tlmgr: Not continuing, please see warning above!\n";
      }
    }
  }

  $opts{"no-depends"} = 1 if $opts{"no-depends-at-all"};
  info("install: dry run, no changes will be made\n") if $opts{"dry-run"};

  my @packs = @ARGV;
  # first expand the .ARCH dependencies unless $opts{"no-depends-at-all"}
  @packs = $tlmediatlpdb->expand_dependencies("-only-arch", $localtlpdb, @ARGV) unless $opts{"no-depends-at-all"};
  # now expand all others unless $opts{"no-depends"}
  # if $opts{"reinstall"} do not collection->collection dependencies
  if ($opts{"reinstall"}) {
    @packs = $tlmediatlpdb->expand_dependencies("-no-collections", $localtlpdb, @packs) unless $opts{"no-depends"};
  } else {
    @packs = $tlmediatlpdb->expand_dependencies($localtlpdb, @packs) unless $opts{"no-depends"};
  }
  foreach my $pkg (sort @packs) {
    my $re = "";
    if (defined($localtlpdb->get_package($pkg))) {
      if ($opts{"reinstall"}) {
        $re = "re";
      } else {
        debug("already installed: $pkg\n");
        next;
      }
    }
    info("${re}install: $pkg\n");
    if (!$opts{"dry-run"}) {
      merge_into(\%ret, $tlmediasrc->install_package($pkg, $localtlpdb));
      logpackage("${re}install: $pkg");
    }
  }
  if ($opts{"dry-run"}) {
    # stop here, don't do any postinstall actions
    return(0);
  }
  return(\%ret);
}

sub action_list {
  init_local_db();
  # make sure that the @ARGV hash is not changed in case we switch to 
  # show mode
  my ($what) = @ARGV;
  if ($what) {
    # if the argument to list is either 'collection' or 'scheme'
    # we list them, otherwise we go direct into tlmgr show $pkg mode
    if ($what !~ m/^(collection|scheme)/i) {
      tlwarn("(switching to show mode)\n");
      action_show();
      return;
    }
  } else {
    $what = "";
  }
  init_tlmedia();
  my @whattolist;
  if ($what =~ m/^collection/i) {
    @whattolist = $tlmediasrc->tlpdb->collections;
  } elsif ($what =~ m/^scheme/i) {
    @whattolist = $tlmediasrc->tlpdb->schemes;
  } else {
    @whattolist = $tlmediasrc->tlpdb->list_packages;
  }
  foreach (@whattolist) {
    if (defined($localtlpdb->get_package($_))) {
      print "i ";
    } else {
      print "  ";
    }
    my $foo = $tlmediasrc->tlpdb->get_package($_)->shortdesc;
    print "$_: ", defined($foo) ? $foo : "(shortdesc missing)" , "\n";
  }
  return;
}


#  OPTION
# 
sub action_option {
  if ($opts{"gui"}) {
    action_gui("config");
  }
  my $what = shift @ARGV;
  $what = "show" unless defined($what);
  init_local_db();
  if ($what =~ m/^show$/i) {
    print "Default installation location (location):          ",
          $localtlpdb->option_location, "\n";
    print "Create formats on installation (formats):          ", 
          ($localtlpdb->option_create_formats ? "yes" : "no"), "\n";
    print "Install documentation files (docfiles):            ",
          ($localtlpdb->option_install_docfiles ? "yes": "no"), "\n";
    print "Install source files (srcfiles):                   ",
          ($localtlpdb->option_install_srcfiles ? "yes": "no"), "\n";
    print "Destination for symlinks for binaries (sys_bin):   ",
          $localtlpdb->option_sys_bin, "\n"
      if $localtlpdb->option_sys_bin;
    print "Destination for symlinks for man pages (sys_man):  ",
          $localtlpdb->option_sys_man, "\n"
      if $localtlpdb->option_sys_man;
    print "Destination for symlinks for info docs (sys_info): ",
          $localtlpdb->option_sys_info, "\n"
      if $localtlpdb->option_sys_info;
    print "Directory for backups (backupdir):                 ",
          $localtlpdb->option("backupdir"), "\n"
      if $localtlpdb->option("backupdir");
    print "Number of backups to keep (autobackup):            ",
          $localtlpdb->option("autobackup"), "\n"
      if $localtlpdb->option("autobackup");
  } elsif ($what =~ m/^location$/i) {
    # changes the default location
    my $loc = shift @ARGV;
    if ($loc) {
      # support "ctan" on the cmd line, and don't abs_path it!
      if ($loc =~ m/^ctan$/i) {
        $loc = "$TeXLive::TLConfig::TeXLiveURL";
      }
      if ($loc !~ m!^(http|ftp)://!i) {
        # seems to be a local path, try to normalize it
        my $testloc = abs_path($loc);
        # however, if we were given a url, that will get "normalized" to the
        # empty string, it not being a path.  Restore the original value if so.
        $loc = $testloc if $testloc;
      }
      info("tlmgr: setting default installation location to $loc\n");
      $localtlpdb->option_location($loc);
      $localtlpdb->save;
    } else {
      info("Default installation location: ",
           $localtlpdb->option_location, "\n");
    }
  } elsif ($what =~ m/^docfiles$/i) {
    # changes the default docfiles
    my $loc = shift @ARGV;
    if (defined($loc)) {
      info("tlmgr: defaulting to", ($loc ? "" : " not"),
           " installing documentation files.\n");
      $localtlpdb->option_install_docfiles($loc);
      $localtlpdb->save;
    } else {
      info("Install documentation files: ",
           $localtlpdb->option_install_docfiles, "\n");
    }
  } elsif ($what =~ m/^srcfiles$/i) {
    # changes the default srcfiles
    my $loc = shift @ARGV;
    if (defined($loc)) {
      info("tlmgr: defaulting to", ($loc ? "" : " not"),
           " installing source files.\n");
      $localtlpdb->option_install_srcfiles($loc);
      $localtlpdb->save;
    } else {
      info("Install source files: ",
           $localtlpdb->option_install_srcfiles, "\n");
    }
  } elsif ($what =~ m/^formats$/i) {
    # changes the default formats
    my $loc = shift @ARGV;
    if (defined($loc)) {
      info("tlmgr: defaulting to", ($loc ? "" : " not"),
           " generating format files on installation.\n");
      $localtlpdb->option_create_formats($loc);
      $localtlpdb->save;
    } else {
      info("Create formats on installation: ",
            $localtlpdb->option_create_formats, "\n");
    }
  } elsif ($what =~ m/^sys_man$/i) {
    # changes the default sys_man
    my $loc = shift @ARGV;
    if (defined($loc)) {
      info("tlmgr: setting destination for symlinks to man pages to $loc\n");
      $localtlpdb->option_sys_man ($loc);
      $localtlpdb->save;
    } else {
      info("Destination for symlinks to man pages: ",
           $localtlpdb->option_sys_man, "\n");
    }
  } elsif ($what =~ m/^sys_info$/i) {
    # changes the default sys_info
    my $loc = shift @ARGV;
    if (defined($loc)) {
      info("tlmgr: setting destination for symlinks to info pages to $loc\n");
      $localtlpdb->option_sys_info ($loc);
      $localtlpdb->save;
    } else {
      info("Default destination for symlinks to info pages: ",
           $localtlpdb->option_sys_info, "\n");
    }
  } elsif ($what =~ m/^sys_bin$/i) {
    # changes the default sys_bin
    my $loc = shift @ARGV;
    if (defined($loc)) {
      info("tlmgr: setting destination for symlinks to binaries to $loc\n");
      $localtlpdb->option_sys_bin ($loc);
      $localtlpdb->save;
    } else {
      info("Default destination for symlinks to binaries: ",
           $localtlpdb->option_sys_bin, "\n");
    }
  } elsif (member($what, @AllowedConfigOptions)) {
    # for all further options not handled above, we check whether they
    # appear in the list of allowed options; if they do, we set/read
    # the values, otherwise we warn.
    my $val = shift @ARGV;
    if (defined($val)) {
      info("tlmgr: setting option $what to $val.\n");
      $localtlpdb->option($what,$val);
      $localtlpdb->save;
    } else {
      info("Option $what = ", $localtlpdb->option($what), "\n");
    }
  } else {
    tlwarn "Option $what is currently not supported.\n";
  }
  return;
}


#  ARCH
#
sub action_arch {
  if ($^O=~/^MSWin(32|64)$/i) {
    warn("action `arch' not supported on Windows\n");
  }
  if ($opts{"gui"}) {
    action_gui("arch");
  }
  my %ret;
  my $what = shift @ARGV;
  init_local_db(1);
  info("arch: dry run, no changes will be made\n") if $opts{"dry-run"};
  $what || ($what = "list");
  if ($what =~ m/^list$/i) {
    # list the available architectures
    # initialize the TLMedia from $location
    init_tlmedia();
    my $mediatlpdb = $tlmediasrc->tlpdb;
    my @already_installed_arch = $localtlpdb->available_architectures;
    print "Available architectures:\n";
    foreach my $a ($mediatlpdb->available_architectures) {
      if (member($a,@already_installed_arch)) {
        print "(i) $a\n";
      } else {
        print "    $a\n";
      }
    }
    print "Already installed architectures are marked with (i)\n";
    print "You can add new architectures with tlmgr arch add arch1 arch2\n";
    finish(0);
  } elsif ($what =~ m/^add$/i) {
    init_tlmedia();
    my $mediatlpdb = $tlmediasrc->tlpdb;
    my @already_installed_arch = $localtlpdb->available_architectures;
    my @available_arch = $mediatlpdb->available_architectures;
    my @todoarchs;
    foreach my $a (@ARGV) {
      if (TeXLive::TLUtils::member($a, @already_installed_arch)) {
        print "Arch $a is already installed\n";
        next;
      }
      if (!TeXLive::TLUtils::member($a, @available_arch)) {
        print "Arch $a not available, use 'tlmgr available_archs'!\n";
        next;
      }
      push @todoarchs, $a;
    }
    foreach my $pkg ($localtlpdb->list_packages) {
      next if ($pkg =~ m/^00texlive/);
      my $tlp = $localtlpdb->get_package($pkg);
      foreach my $dep ($tlp->depends) {
        if ($dep =~ m/^(.*)\.ARCH$/) {
          # we have to install something
          foreach my $a (@todoarchs) {
            if ($opts{"dry-run"}) {
              info("Installing $pkg.$a\n");
            } else {
              info("install: $pkg.$a\n");
              merge_into(\%ret, $tlmediasrc->install_package("$pkg.$a", $localtlpdb));
            }
          }
        }
      }
    }
    if (TeXLive::TLUtils::member('win32', @todoarchs)) {
      # install the necessary w32 stuff
      info("install: bin-tlperl.win32\n");
      merge_into (\%ret, $tlmediasrc->install_package("bin-tlperl.win32", $localtlpdb));
      info("install: bin-tlgs.win32\n");
      merge_into (\%ret, $tlmediasrc->install_package("bin-tlgs.win32", $localtlpdb));
      info("install: bin-tlpsv.win32\n");
      merge_into (\%ret, $tlmediasrc->install_package("bin-tlpsv.win32", $localtlpdb));
    }
    # update the option_archs list of installed archs
    my @larchs = $localtlpdb->option_available_architectures;
    push @larchs, @todoarchs;
    $localtlpdb->option_available_architectures(@larchs);
    $localtlpdb->save;
  } else {
    die "Unknown option for arch: $what";
  }
  return(\%ret);
}


#  GENERATE
# 
sub action_generate {
  my $dest = defined($opts{"dest"}) ? $opts{"dest"} : "";
  my $localconf = defined($opts{"localcfg"}) ? $opts{"localcfg"} : "";
  my $what = shift @ARGV;
  init_local_db();

  # we create fmtutil.cnf, language.dat, language.def in TEXMFSYSVAR
  # and updmap.cfg in TEXMFSYSCONFIG. The reason is that calls to
  # updmap-sys (as is done by the tlmgr update call when packages with
  # maps are installed) will create the updmap.cfg file in TEXMFSYSCONFIG
  # from the version in TEXMFSYSVAR. But after that the TEXMFSYSCONFIG
  # takes precedence and the mechanism explained in updmap.cfg header
  # does not work.
  #
  chomp (my $TEXMFSYSVAR = `kpsewhich -var-value=TEXMFSYSVAR`);
  chomp (my $TEXMFSYSCONFIG = `kpsewhich -var-value=TEXMFSYSCONFIG`);
  chomp (my $TEXMFLOCAL = `kpsewhich -var-value=TEXMFLOCAL`);
  
  if ($what =~ m/^language(\.dat|\.def)?$/i) {
    if ($what =~ m/^language(\.dat)?$/i) {
      $dest ||= "$TEXMFSYSVAR/tex/generic/config/language.dat";
      $localconf ||= "$TEXMFLOCAL/tex/generic/config/language-local.dat";
      debug ("$0: writing language.dat data to $dest\n");
      TeXLive::TLUtils::create_language_dat($localtlpdb, $dest, $localconf);
      $dest .= ".def";
    } 
    if ($what =~ m/^language(\.def)?$/i) {
      $dest ||= "$TEXMFSYSVAR/tex/generic/config/language.def";
      $localconf ||= "$TEXMFLOCAL/tex/generic/config/language-local.def";
      debug("$0: writing language.def data to $dest\n");
      TeXLive::TLUtils::create_language_def($localtlpdb, $dest, $localconf);
    } 

  } elsif ($what =~ m/^fmtutil$/i) {
    $dest ||= "$TEXMFSYSVAR/web2c/fmtutil.cnf";
    $localconf ||= "$TEXMFLOCAL/web2c/fmtutil-local.cnf";
    debug("$0: writing new fmtutil.cnf to $dest\n");
    TeXLive::TLUtils::create_fmtutil($localtlpdb, $dest, $localconf);

  } elsif ($what =~ m/^updmap$/i) {
    $dest ||= "$TEXMFSYSCONFIG/web2c/updmap.cfg";
    $localconf ||= "$TEXMFLOCAL/web2c/updmap-local.cfg";
    debug("$0: writing new updmap.cfg to $dest\n");
    TeXLive::TLUtils::create_updmap($localtlpdb, $dest, $localconf);

  } else {
    die "$0: Unknown option for generate: $what; try --help if you need it.\n";
  }

  return;
}


#  GUI
# 
sub action_gui {
  my ($guiscreen) = @_;
  # yes, two times to make perl warnings shut up ...
  $::guiscreen = $guiscreen;
  $::guiscreen = $guiscreen;
  unshift (@INC, "$Master/texmf/scripts/texlive/tlmgrgui");
  eval { require Tk; };
  if ($@) {
    # that didn't work out, give some usefull error message and stop
    my $tkmissing = 0;
    if ($@ =~ /^Can\'t locate Tk\.pm/) {
      $tkmissing = 1;
    }
    if ($tkmissing) {
      if ($^O=~/^MSWin(32|64)$/i) {
        # that should not happen, we are shipping Tk!!
        require Win32;
        my $msg = "Cannot load Tk, that should not happen as we ship it!\nHow did you start tlmgrgui??\n(Error message: $@)\n";
        Win32::MsgBox($msg, 1|Win32::MB_ICONSTOP(), "Warning");
      } else {
        printf STDERR "
Cannot load Tk, thus the GUI cannot be started!
The Perl/Tk module is not shipped with the TeX Live installation.
You have to install it to get tlmgr GUI running. See
http://tug.org/texlive/distro.html for more details.

";
      }
    } else {
      printf STDERR "Problem loading Tk: $@\n";
    }
    exit 1;
  }

  # now check that we can actually create a top level window, 
  # on darwin the X server might not be started, or on unix we are working
  # on a console, or whatever.
  eval { my $foo = Tk::MainWindow->new; $foo->destroy; };
  if ($@) {
    printf STDERR "perl/Tk unusable, cannot create main windows.
That could be a consequence of not having X Windows installed or started!
Error message from creating MainWindow:
  $@
";
    exit 1;
  }

  # be sure that sub actions do *not* finish
  $::gui_mode = 1;
  # also unset the $opts{"gui"} to make recursive calls to action_* not starting
  # another GUI instance (or better trying to ...)
  $opts{"gui"} = 0;

  require("tlmgrgui.pl");
  # should not be reached
  exit(1);
}


#  UNINSTALL
# 
sub action_uninstall {
  if (win32()) {
    printf STDERR "Please use \"Add/Remove Programs\" from the Control Panel to removing TeX Live!\n";
    finish(1);
  }
  if ($opts{"gui"}) {
    action_gui("uninstall");
  }
  my $force = defined($opts{"force"}) ? $opts{"force"} : 0;
  if (!$force) {
    print("If you answer yes here the whole TeX Live installation will be removed!\n");
    print "Remove TeX Live (y/N): ";
    my $yesno = <STDIN>;
    if ($yesno !~ m/^y(es)?$/i) {
      print "Ok, cancelling the removal!\n";
      finish(0);
    }
  }
  print ("Ok, removing the whole installation:\n");
  init_local_db();
  $localtlpdb->remove_symlinks;
  # now do remove the rest
  system("rm", "-rf", "$Master/texmf-dist");
  system("rm", "-rf", "$Master/texmf-doc");
  system("rm", "-rf", "$Master/texmf-var");
  system("rm", "-rf", "$Master/texmf");
  system("rm", "-rf", "$Master/tlpkg");
  system("rm", "-rf", "$Master/bin");
  system("rm", "-rf", "$Master/readme-html.dir");
  system("rm", "-rf", "$Master/readme-txt.dir");
  for my $f (qw/doc.html index.html LICENSE.CTAN LICENSE.TL README
                README.usergroups release-texlive.txt texmf.cnf/) {
    system("rm", "-f", "$Master/$f");
  }
  if (-d "$Master/temp") {
    system("rmdir", "--ignore-fail-on-non-empty", "$Master/temp");
  }
  unlink("$Master/install-tl.log");
  # should we do that????
  system("rm", "-rf", "$Master/texmf-config");
  system("rmdir", "--ignore-fail-on-non-empty", "$Master");
}

#  RECREATE TLPDB
#
sub action_recreate_tlpdb {
  my $tlpdb = TeXLive::TLPDB->new;
  $tlpdb->root($Master);
  my $inst = TeXLive::TLPOBJ->new;
  $inst->name("00texlive-installation.config");
  $inst->category("TLCore");
  my @deps;
  push @deps, "location:$TeXLive::TLConfig::TeXLiveURL";
  push @deps, "opt_create_formats:0";
  push @deps, "opt_create_symlinks:0";
  push @deps, "opt_sys_bin:/usr/local/bin";
  push @deps, "opt_sys_info:/usr/local/info";
  push @deps, "opt_sys_man:/usr/local/man";
  push @deps, "opt_install_docfiles:0";
  push @deps, "opt_install_srcfiles:0";
  # find list of available archs
  my @archs;
  opendir (DIR, "$Master/bin") || die "opendir($Master/bin) failed: $!";
  my @dirents = readdir (DIR);
  closedir (DIR) || warn "closedir($Master/bin) failed: $!";
  for my $dirent (@dirents) {
    next if $dirent eq ".";
    next if $dirent eq "..";
    next unless -d "$Master/bin/$dirent";
    if (-r "$Master/bin/$dirent/kpsewhich" || -r "$Master/bin/$dirent/kpsewhich.exe") {
      push @archs, $dirent;
      debug("Skipping directory $Master/bin/$dirent, no kpsewhich there\n");
    }
  }
  push @deps, "available_architectures:" . join(" ",@archs);
  # we have to find out the default arch
  # if there is only one dir in $Master/bin then we are settled,
  # otherwise we expect the user to pass a correct arch string
  if ($^O =~ /^MSWin(32|64)$/i) {
    push @deps, "platform:win32";
  } else {
    if ($#archs == 0) {
      # only one arch available, fine, use it as default
      push @deps, "platform:$archs[0]";
    } else {
      if (defined($opts{"arch"})) {
        if (member($opts{"arch"}, @archs)) {
          push @deps, "platform:" . $opts{"arch"};
        } else {
          tlwarn("The architecture you passed in with --arch is not present in $Master/bin\n");
          tlwarn("Please specify one from the available ones: @archs\n");
          exit(1);
        }
      } else {
        tlwarn("More than one architecture available: @archs\n");
        tlwarn("Please pass one as the default you are running on with --arch=...\n");
        exit(1);
      }
    }
  }
  $inst->depends(@deps);
  # now we have all the stuff for 00texlive-installation.config done
  $tlpdb->add_tlpobj($inst);
  # add the other stuff in $Master/tlpkg/tlpobj/*.tlpobj
  # we can ignore *.{source,doc}.tlpobj because they are already
  # included in the *.tlpobj parent one at install time
  # (TODO: we should actually REMOVE the *.{source,doc}.tlpobj files
  #        at package install time)
  opendir (DIR, "$Master/tlpkg/tlpobj") or die "opendir($Master/tlpkg/tlpobj) failed: $!";
  my @tlps = readdir(DIR);
  closedir (DIR) || warn "closedir($Master/tlpkg/tlpobj) failed: $!";
  for my $t (@tlps) {
    next if -d $t; # also does . and ..
    next if ($t !~ m/\.tlpobj$/i);
    # ignore .source and .doc tlpobjs
    next if ($t =~ m/\.(source|doc)\.tlpobj$/i);
    my $tlp = TeXLive::TLPOBJ->new;
    $tlp->from_file("$Master/tlpkg/tlpobj/$t");
    $tlpdb->add_tlpobj($tlp);
  }
  # writeout the re-created tlpdb to stdout
  $tlpdb->writeout;
  return;
}

#  CHECK
#
sub action_check {
  my $svn = defined($opts{"use-svn"}) ? $opts{"use-svn"} : 0;
  my $what = shift @ARGV;
  $what || ($what = "all");
  init_local_db();
  my $ret = 0;
  if ($what =~ m/^all/i) {
    $ret ||= check_files($svn);
    $ret ||= check_collections($svn);
  } elsif ($what =~ m/^files/i) {
    $ret ||= check_files($svn);
  } elsif ($what =~ m/^collections/i) {
    $ret ||= check_collections($svn);
  } else {
    print "No idea how to check that: $what\n";
  }
  finish($ret);
}

# check file coverage, roughly equivalent to tlpkg/bin/check-file-coverage
#
sub check_files {
  my $svn = shift;
  my $ret = 0;
  my %filetopacks;
  my $Master = $localtlpdb->root;
  debug("Collecting all files of all packages\n");
  for my $p ($localtlpdb->list_packages()) {
    # ignore files in the installer
    next if ($p eq "00texlive.installer");
    my $tlp = $localtlpdb->get_package($p);
    for my $f ($tlp->all_files) {
      push @{$filetopacks{$f}}, $p;
    }
  }
  my @multiple = ();
  my @missing = ();
  debug("Checking for occurrences and existence of all files\n");
  for (keys %filetopacks) {
    push @missing, $_ if (! -r "$Master/$_");
    my @foo = @{$filetopacks{$_}};
    if ($#foo < 0) {
      warn "that shouldn't happen: $_\n";
    } elsif ($#foo > 0) {
      push @multiple, $_;
    }
  }
  if ($#multiple >= 0) {
    $ret = 1;
    print "Multiple included files (relative to $Master):\n";
    for (sort @multiple) {
      my @foo = @{$filetopacks{$_}};
      print "  $_ (@foo)\n";
    }
    print "\n";
  }
  if ($#missing >= 0) {
    $ret = 1;
    print "Files mentioned in tlpdb but missing (relative to $Master):\n";
    for my $m (@missing) {
      print "\t$m\n";
    }
    print "\n";
  }

  # if we are on Win32 or MacOS we return, they currently do not allow
  # find -wholename (missing find on w32, or bsd find on darwin).
  # we need the -use-svn version only for the check-file-coverage 
  # replacement anyway, so it will be used on tug, which is neither w32 nor
  # darwin.
  my $arch = $localtlpdb->option_platform;
  return $ret if ($arch eq "win32" || $arch eq "universal-darwin");

  # do check that all files in the trees are covered
  #
  my @IgnorePatterns = qw!
    .mkisofsrc$ autorun.inf$
    support/ source/ setuptl/
    texmf-dist/ls-R$ texmf-doc/ls-R$ texmf/ls-R$
    tlpkg/tlpsrc tlpkg/bin tlpkg/lib/ tlpkg/libexec tlpkg/tests/ tlpkg/etc
    tlpkg/texlive.tlpdb
    tlpkg/tlpobj
    texmf-var/
    texmf-config/
    texmf.cnf
    install-tl.log
    tlpkg/texlive.profile
    tlpkg/installer
    install-tl$
    install-tl.bat$
    install-tl.bat.manifest$
  !;
  my $tltree = TeXLive::TLTREE->new ("svnroot" => $Master);
  if ($svn) {
    debug("Initializine TLTREE from svn\n");
    $tltree->init_from_svn;
  } else {
    debug("Initializine TLTREE from find\n");
    $tltree->init_from_files;
  }
  my %tltreefiles = %{$tltree->{'_allfiles'}};
  my @tlpdbfiles = keys %filetopacks;
  my @nohit;
  for my $f (keys %tltreefiles) {
    # if it is mentioned in the tlpdb or is ignored it is considered
    # as covered, thus, otherwise we push it onto the nothit list
    if (!defined($filetopacks{$f})) {
      my $ignored = 0;
      for my $p (@IgnorePatterns) {
        if ($f =~ m/^$p/) {
          $ignored = 1;
          last;
        }
      }
      if (!$ignored) {
        push @nohit, $f;
      }
    }
  }
  if (@nohit) {
    $ret = 1;
    print "Files present but not covered (relative to $Master):\n";
    for my $f (sort @nohit) {
      print "  $f\n";
    }
    print "\n";
  }
  return($ret);
}

# check collections
#
sub check_collections {
  my @missing = ();
  for my $p ($localtlpdb->collections()) {
    my $col = $localtlpdb->get_package($p);
    for my $d ($col->depends) {
      push @missing, "$d ($p)" if (!defined($localtlpdb->get_package($d)));
    }
  }
  return(0) if (!@missing);
  print "Packages listed in collections but not present:\n";
  for my $m (@missing) {
    print "\t$m\n";
  }
  print "\n";
}



# Subroutines galore.
# 
# set global $location variable.
# if we cannot read tlpdb, die if arg SHOULD_I_DIE is true.
#
# if an argument is given and is true init_local_db will die if 
# setting up of programs failed.
# 
sub init_local_db {
  my ($should_i_die) = @_;
  # if the localtlpdb is already defined do simply return here already
  # to make sure that the settings in the local tlpdb do not overwrite
  # stuff changed via the GUI
  return if defined $localtlpdb;
  $localtlpdb = TeXLive::TLPDB->new ("root" => $Master);
  die("cannot find tlpdb in $Master") unless (defined($localtlpdb));
  # setup the programs, for w32 we need the shipped wget/lzma etc, so we
  # pass the location of these files to setup_programs.
  if (!setup_programs("$Master/tlpkg/installer", 
                      $localtlpdb->option_platform)) {
    tlwarn("Couldn't set up the necessary programs.\nInstallation of packages is not supported.\nPlease report to texlive\@tug.org.\n");
    if (defined($should_i_die) && $should_i_die) {
      finish(1);
    } else {
      tlwarn("Continuing anyway ...\n");
    }
  }
  # let cmd line options override the settings in localtlpdb
  my $loc = $localtlpdb->option_location;
  if (defined($loc)) {
    $location = $loc;
  }
  if (defined($opts{"location"})) {
    $location = $opts{"location"};
  }
  if (!defined($location)) {
    die("No installation source found, nor in the texlive.tlpdb nor on the cmd line.\nPlease specify one!");
  }
  if ($location =~ m/^ctan$/i) {
    $location = "$TeXLive::TLConfig::TeXLiveURL";
  }
  # we normalize the path only if it is
  # - neither a URL starting with http or ftp
  # - if we are on windows it does not start with Drive:
  if (! ( $location =~ m!^(http|ftp)://!i  ||
          (win32() && $location =~ m!^.:!) ) ) {
    # seems to be a local path, try to normalize it
    my $testloc = abs_path($location);
    # however, if we were given a url, that will get "normalized" to the
    # empty string, it not being a path.  Restore the original value if so.
    $location = $testloc if $testloc;
  }
}


# initialize the global $tlmediasrc object, or die.
# uses the global $location.
#
sub init_tlmedia {
  if (defined($tlmediatlpdb) && ($tlmediatlpdb->root eq $location)) {
    # nothing to be done
  } else {
    if ($location =~ m/^($TeXLiveServerURL|ctan$)/) {
      $location = give_ctan_mirror();
    }
    # that "location-url" line should not be changed since GUI programs
    # depend on it.
    print "location-url\t$location\n" if $::machinereadable;
    info("tlmgr: installation location $location\n");

    # $tlmediasrc is a global variable
    $tlmediasrc = TeXLive::TLMedia->new($location);
    die($loadmediasrcerror . $location) unless defined($tlmediasrc);
    $tlmediatlpdb = $tlmediasrc->tlpdb;
  }
}

# finish handles the -pause option (wait for input from stdin),
# and then exits unless  the global $::gui_mode is set, in which case we
# merely return.
# 
sub finish {
  my ($ret) = @_;

  if ($ret > 0) {
    print "tlmgr: exiting unsuccessfully (status $ret).\n";
  }
  
  if ($opts{"pause"}) {
    print "Press Enter to exit the program.\n";
    <STDIN>;
  }

  if ($::gui_mode) {
    return($ret);
  } else {
    exit($ret);
  }
}

# if the packagelog variable is set then write to PACKAGELOG filehandle
#
sub logpackage {
  if ($packagelogfile) {
    $packagelogged++;
    my $tim = localtime();
    print PACKAGELOG "[$tim] @_\n";
  }
}

# clear the backup dir for $pkg and keep only $autobackup packages
# mind that with $autobackup == 0 all packages are cleared
sub clear_old_backups
{
  my ($pkg, $backupdir, $autobackup, $dry) = @_;

  my $dryrun = 0;
  $dryrun = 1 if ($dry);
  # keep arbitrary many backups
  return if ($autobackup == -1);
 
  opendir (DIR, $backupdir) || die "opendir($backupdir) failed: $!";
  my @dirents = readdir (DIR);
  closedir (DIR) || warn "closedir($backupdir) failed: $!";
  my @backups;
  for my $dirent (@dirents) {
    next if (-d $dirent);
    next if ($dirent !~ m/^$pkg\.r([0-9]+)\.tar\.lzma$/);
    push @backups, $1;
  }
  my $i = 1;
  for my $e (reverse sort {$a <=> $b} @backups) {
    if ($i > $autobackup) {
      info ("Removing backup $backupdir/$pkg.r$e.tar.lzma\n");
      unlink("$backupdir/$pkg.r$e.tar.lzma") unless $dryrun;
    }
    $i++;
  }
}

# check for updates to tlcritical packages
#
sub check_for_critical_updates
{
  my ($localtlpdb, $mediatlpdb) = @_;

  my $criticalupdate = 0;
  my @critical = $localtlpdb->expand_dependencies("-no-collections",
    $localtlpdb, @CriticalPackagesList);
  for my $pkg (sort @critical) {
    my $tlp = $localtlpdb->get_package($pkg);
    if (!defined($tlp)) {
      # that should not happen, we expanded in the localtlpdb so why 
      # should it not be present, any anyway, those are so fundamental
      # so they have to be there
      tlwarn("Fundamental package $pkg not present, that is sooo bad.\n");
      die "Serious error, $pkg not found";
    }
    my $localrev = $tlp->revision;
    my $mtlp = $mediatlpdb->get_package($pkg);
    if (!defined($mtlp)) {
      debug("Very surprising, $pkg is not present in the remote tlpdb.\n");
      next;
    }
    my $remoterev = $mtlp->revision;
    if ($remoterev > $localrev) {
      $criticalupdate = 1;
      last;
    }
  }
  if ($criticalupdate) {
    tlwarn("Updates for tlmgr itself are present.\n");
    tlwarn("=" x 79, "\n");
    tlwarn("Please update the packages bin-texlive and texlive.infra first, ");
    tlwarn("via either\n");
    tlwarn("  tlmgr update bin-texlive texlive.infra\n");
    tlwarn("or by getting the latest updater for Unix-ish systems:\n");
    tlwarn("  $TeXLiveURL/update-tlmgr-latest.sh\n");
    tlwarn("and/or Windows systems:\n");
    tlwarn("  $TeXLiveURL/update-tlmgr-latest.exe\n");
    tlwarn("Then continue with other updates.\n");
    tlwarn("=" x 79, "\n");
  }
  return($criticalupdate);
}


# return all the directories from which all content will be removed
#
# idea:
# - create a hashes by_dir listing all files that should be removed
#   by directory, i.e., key = dir, value is list of files
# - for each of the dirs (keys of by_dir and ordered deepest first)
#   check that all actually contained files are removed
#   and all the contained dirs are in the removal list. If this is the
#   case put that directory into the removal list
# - return this removal list
#
sub removed_dirs
{
  my (@files) = @_;
  my %removed_dirs;
  my %by_dir;

  # construct hash of all directories mentioned, values are lists of the
  # files/dirs in that directory.
  for my $f (@files) {
    # what should we do with not existing entries????
    next if (! -r "$f");
    my $abs_f = abs_path ($f);
    if (!$abs_f) {
      warn ("oops, no abs_path($f) from " . `pwd`);
      next;
    }
    (my $d = $abs_f) =~ s,/[^/]*$,,;
    my @a = exists $by_dir{$d} ? @{$by_dir{$d}} : ();
    push (@a, $abs_f);
    $by_dir{$d} = \@a;
  }

  # for each of our directories, see if we are removing everything in
  # the directory.  if so, return the directory; else return the
  # individual files.
  for my $d (reverse sort keys %by_dir) {
    opendir (DIR, $d) || die "opendir($d) failed: $!";
    my @dirents = readdir (DIR);
    closedir (DIR) || warn "closedir($d) failed: $!";

    # initialize test hash with all the files we saw in this dir.
    # (These idioms are due to "Finding Elements in One Array and Not
    # Another" in the Perl Cookbook.)
    my %seen;
    my @rmfiles = @{$by_dir{$d}};
    @seen{@rmfiles} = ();

    # see if everything is the same.
    my $cleandir = 1;
    for my $dirent (@dirents) {
      next if $dirent =~ /^\.(\.|svn)?$/;  # ignore . .. .svn
      my $item = "$d/$dirent";  # prepend directory for comparison
      if (
           ((-d $item) && (defined($removed_dirs{$item})))
           ||
           (exists $seen{$item})
         ) {
        # do nothing
      } else {
        $cleandir = 0;
        last;
      }
    }
    if ($cleandir) {
      $removed_dirs{$d} = 1;
    }
  }
  return keys %removed_dirs;
}

__END__

=head1 NAME

tlmgr - the TeX Live Manager

=head1 SYNOPSIS

tlmgr [I<option>]... I<action> [I<option>]... [I<operand>]...

=head1 DESCRIPTION

B<tlmgr> manages an existing TeX Live installation, both packages and
configuration options.  It performs many of the same actions as
B<texconfig>(1), and more besides.  (C<texconfig> continues to be
included and supported, but C<tlmgr> is now preferred.)

The most up-to-date version of this documentation is available at
L<http://tug.org/texlive/tlmgr.html>, along with procedures for updating
tlmgr itself, disaster recovery when it's broken, and test versions.

=head1 OPTIONS

The following options to C<tlmgr> have to be given I<before> you specify
the main action to perform.  In all cases, C<-->I<option> and
C<->I<option> are equivalent, and an C<=> is optional between an option
name and its value.

=over 4

=item B<--location> I<location>

Specifies the location from which packages should be installed or
updated, overriding the location found in the installation's TeX Live
Package Database (TLPDB).

=item B<--gui> [I<action>]

You can give this option together with an action to be brought directly
into the respective screen of the GUI.  For example, running

  tlmgr --gui update

starts you directly at the update screen.

=item B<--gui-lang> I<llcode>

Normally the GUI tries to deduce your language from the environment (on
Windows via the registry, on Unix via C<LC_MESSAGES>). If that fails you
can select a different language by giving this option a two-letter
language code.

=item B<--machine-readable>

In lieu of the normal output intended for human consumption, write to
standard output in a fixed format more suitable for machine parsing.
See the L</"MACHINE-READABLE OUTPUT"> section below for details.

=item B<--package-logfile> I<file>

B<tlmgr> logs all package actions (install, remove, update, failed
updates, failed restores) to a separate log file, by default
C<TEXMFSYSVAR/web2c/tlmgr.log>. This option allows you to select a
different file for that.  This is separate from normal logging; for
that, see the option C<-v> below, and TeXLive::TLUtils.

=item B<--pause>

This option make B<tlmgr> wait for user input before exiting.  Useful on
Windows to avoid command windows disappearing.

=back

The standard options for TeX Live programs are also accepted:
C<--help/-h/-?>, C<--version>, C<-q> (no informational messages), C<-v>
(debugging messages, can be repeated).  For the details about these, see
the TeXLive::TLUtils documentation.

The C<--version> option shows version information about the TeX Live
release as well as the C<tlmgr> script itself.

=head1 ACTIONS

=head2 help

Gives this help information (same as C<--help>).

=head2 version

Gives version information (same as C<--version>).

=head2 gui

Start the graphical user interface.


=head2 install [I<option>]... I<pkg>...

Install each I<pkg> given on the command line. By default this installs
all packages on which the given I<pkg>s are dependent, also.  Options:

=over 4

=item B<--reinstall>

Reinstall a package (including dependencies for collections) even if it
seems to be already installed (i.e, is present in the TLPDB).  This is
useful to recover from accidental removal of files in the hierarchy.

When re-installing, only dependencies on normal packages are followed
(not those of category Scheme or Collection).

=item B<--no-depends>

Do not install dependencies.  (By default, installing a package ensures
that all dependencies of this package are fulfilled.)

=item B<--no-depends-at-all>

When you install a package which ships binary files the respective binary
package will also be installed. (So for package bin-foobar also the package
bin-foobar.i386-linux will be installed on an i386-linux system.)
This switch supresses this behaviour, and also implies C<--no-depends>.
Don't use it unless you are sure of what you are doing.

=item B<--dry-run>

Nothing is actually installed; instead, the actions to be performed are
written to the terminal.

=item B<--force>

If updates to the tlmgr itself (and the underlying infrastructure) are 
present tlmgr will bail out and not perform the installation unless this option
is given.

=back


=head2 update [I<option>]... [I<pkg>]...

Updates the packages given as arguments to the latest version available
at the installation source.  Either C<--all> or at least one I<pkg> name
must be specified.  Options:

=over 4

=item B<--all>

Update all installed packages.  For packages contained in an installed
collection, also install new packages and remove deleted packages in
those collections.  Packages not contained in an installed collection
are not considered for addition or removal.

More precisely, if both the server and the local installation have a
collection C, and C on the server contains a package A that is not
present locally, A will be added.  Conversely, if the local collection C
contains a package B, but B is no longer in the server's C, then B will
be deleted.

=item B<--list>

Concisely list the packages which would be updated, newly installed, or
removed, without actually changing anything.

=item B<--dry-run>

Nothing is actually installed; instead, the actions to be performed are
written to the terminal.  This is a more detailed report than C<--list>.

=item B<--force>

If updates to tlmgr itself (that is, the infrastructure packages) are
present, tlmgr will not perform the update unless this option is given.
(C<--list> is still performed regardless of this option.)

=item B<--no-remove>

Under normal circumstances tlmgr tries to remove packages which have
disappeared on the server when called with C<--all>, as described
above. This prevents any such removals.

=item B<--backup> and B<--backupdir> I<directory>

These two options control the creation of backups of packages before an
update is started; that is, the backup is of the package as it's
installed.  If neither of the two are given, no backup package will be
saved. If B<--backupdir> is given and specifies a writable directory
then a backup will be made in that location. If only B<--backup> is
given, then a backup will be made to the directory previously set via
the C<option> action (see below). If both are given then a backup will
be made to the specified I<directory>.

You can set options via the C<option> action to automatically create
backups for all packages, and/or keep only a certain number of
backups. Please see the C<option> action for details. 

C<tlmgr> always makes a temporary backup when updating packages, in case
of download or other failure during an update.  The purpose of this
B<--backup> option is to allow you to save a persistent backup in case
the actual I<content> of the update causes problems, e.g., introduces an
incompatibility.

The C<restore> action explains how to restore from a backup.

=item B<--no-depends>

If you call for updating a package normally all depending packages
will also be checked for updates and updated if necessary. This switch
supresses this behaviour.

=item B<--no-depends-at-all>

By default, when you update a package which ships binary files the
respective binary package will also be updated.  That is, for an update
of a package C<foo>, the package C<foobar.i386-linux> will also be
updated if the i386-linux platform is installed.  This option suppresses
this behaviour, and implies C<--no-depends>.  Don't use it unless you
are sure of what you are doing.

=back

If the package on the server is older than the package already installed
(e.g., if the selected mirror is out of date), C<tlmgr> does not
downgrade.  Also, packages for uninstalled platforms are not installed.


=head2 backup [--clean[=I<N>]] [--backupdir I<dir>] [--all] [I<pkg>]...

If the B<--clean> option is not specified, this action makes a backup of
the given packages, or all packages given C<--all>. These backups are
saved to the value of B<--backupdir> option if that is an existing and
writable directory; if B<--backupdir> is not given, the C<backupdir>
option setting in the TLPDB is used, if present.  If both are missing,
no backups are made.

If the B<--clean> option is specified, backups are cleaned (pruned)
instead of made.  If the value for the B<--clean> switch is not given,
the value of the C<autobackup> option is used. If both are missing,
an error is issued.  For the details of backup cleaning, see the
C<option> action below.

Options:

=over 4

=item B<--backupdir> I<directory>

The I<directory> argument is required and must specify an existing
directory where backups are to be placed.

=item B<--all>

Make a backup of all packages in the TeX Live installation.  This will
take quite a lot of space and time.

=item B<--clean>[=I<N>]

Instead of making backups, prune the backup directory of old backups.

=item B<--dry-run>

Nothing is actually backed up or removed; instead, the actions to be 
performed are written to the terminal. 

=back


=head2 restore --backupdir I<dir> [I<pkg> [I<rev>]

Restore a package from a previously-made backup.

If neither I<pkg> nor I<rev> are given, list the available backup
revisions for all packages.

With I<pkg> given but no I<rev>, list all available backup revisions of
I<pkg>.

With both I<pkg> and I<rev>, tries to restore the package from the
specified backup.

Options:

=over 4

=item B<--dry-run>

Nothing is actually restored; instead, the actions to be performed are
written to the terminal.

=item B<--backupdir> I<directory>

Specify the directory where the backups are to be found.

=back


=head2 remove [I<option>]... I<pkg>...

Remove each I<pkg> specified.  Removing a collection removes all package
dependencies, but not collection dependencies, in that collection
(unless C<--no-depends> is specified).  However, when removing a
package, dependencies are never removed.  Options:

=over 4

=item B<--no-depends>

Do not remove dependent packages.

=item B<--no-depends-at-all>

See above under B<action> (and beware).

=item B<--force>

By default, removal of a package or collection that is a dependency of
another collection or scheme is not allowed.  With this option, the
package will be removed unconditionally.  Use with care.

=item B<--dry-run>

Nothing is actually removed; instead, the actions to be performed are
written to the terminal.

=back


=head2 option

=over 4

=item B<option [show]>

=item B<option I<key> [I<value>]>

=back

The first form shows the global TeX Live settings currently saved in the
TLPDB.

In the second form, if I<value> is not given, the setting for I<key> is
displayed.  If I<value> is present, I<key> is set to I<value>.

Possible values for I<key> are:
 C<location> (default installation location),
 C<formats> (create formats at installation time),
 C<docfiles> (install documentation files),
 C<srcfiles> (install source files),
 C<backupdir> (default directory for backups),
 C<autobackup> (number of backups to keep).
 C<sys_bin> (location where binaries are linked to by action symlinks)
 C<sys_man> (location where man pages are linked to by action symlinks)
 C<sys_info> (location where info pages are linked to by action symlinks)

Perhaps the most common use of C<option> is if you originally installed from
DVD, and want to permanently change the installation to get further
updates from the Internet.  To do this, you can run

 tlmgr option location http://mirror.ctan.org/systems/texlive/tlnet/YYYY

where YYYY is the TeX Live release year.

The two options C<autobackup> and C<backupdir> determine defaults for
the C<update>, C<backup> and C<restore> actions.  These three actions
need a directory to write to or read from the backups.  If
C<--backupdir> is not specified on the command line, the C<backupdir>
option value is used (if set).

The C<autobackup> option (de)activates the automatic generation of
backups. Its value is an integer.  If C<autobackup> is C<-1>, backups
will not be removed by default.  If C<autobackup> is 0 or positive, it
specifies the number of backups to be kept.  Thus, backups are disabled
with 0 as the values.

In the C<--clean> mode of the C<backup> action this option also
specifies the number to be kept.

=head2 symlinks [add|remove]

Adds or removes symlinks for binaries, man pages, and info pages in the
directories specified by the respective options (see above).


=head2 paper

=over 4

=item B<paper [a4|letter]>

=item B<S<xdvi|pdftex|dvips|dvipdfmx|dvipdfm|context paper [I<papersize>|--list]>>

=back

With no arguments (C<tlmgr paper>), shows the default paper size setting
for all known programs.

With one argument (e.g., C<tlmgr paper a4>), sets the default for all
known programs to that paper size.

With a program given as the first argument and no paper size specified
(e.g., C<tlmgr dvips paper>), shows the default paper size for that
program.

With a program given as the first argument and a paper size as the last
argument (e.g., C<tlmgr dvips paper a4>), set the default for that
program to that paper size.

With a program given as the first argument and C<--list> given as the
last argument (e.g., C<tlmgr dvips paper --list>), shows all valid paper
sizes for that program.  The first size shown is the default.

Incidentally, this syntax of having a specific program name before the
C<paper> keyword may seem strange.  It is inherited from the
longstanding C<texconfig> script, which supports other configuration
settings for some programs, notably C<dvips>.  C<tlmgr> does not support
those extra settings at present.

=head2 arch list|add I<arch>...

C<arch list> lists the TeX Live names of all the architectures
(C<i386-linux>, ...) available at the default install location.

C<arch add> I<arch> adds the executables for each given architecture
I<arch> to the installation.  Options:

=over 4

=item B<--dry-run>

Nothing is actually installed; instead, the actions to be performed are
written to the terminal.

=back


=head2 search [I<option>]... I<what>

By default searches the names, short and long descriptions of all
locally installed packages for the given argument (interpreted as
regexp).  Options:

=over 4

=item B<--file>

List all filenames containing I<what>.

=item B<--global>

Search the TeX Live Database of the installation medium, instead of the
local installation.

=back


=head2 show [--list] I<pkg>...

Shows information about I<pkg>: the name, category, installation status,
short and long description.  Searches in the remote installation source
for the package if it is not locally installed.

If the option B<--list> is given the list of contained files is also
shown, including those for architecture specific dependencies.


=head2 list [collections|schemes|I<pkg>...]

With no argument, lists all packages available at the default install
location, prefixing those already installed with C<i>.

With the single word C<collections> or C<schemes> as the argument, lists
the request type.

With anything else as arguments, operates as the C<show> action.


=head2 check [I<option>]... [files|collections|all]

Executes one (or all) check(s) on the consistency of the installation.

=over 4

=item B<files>

Checks that all files listed in the TeX Live Database (C<texlive.tlpdb>)
are actually present, and lists those missing.

=item B<collections>

Lists those packages which occur as dependencies in an installed collections,
but are themselves not installed.  Options:

=back

=over 4

=item B<--use-svn>

Use svn status instead of listing the files, for checking the
development repository.

=back


=head2 uninstall

Uninstalls the entire TeX Live installation.  Options:

=over 4

=item B<--force>

Do not ask for confirmation, remove immediately.

=back


=head2 generate [I<option>]... I<what>

=over 4

=item B<generate language>

=item B<generate language.dat>

=item B<generate language.def>

=item B<generate fmtutil>

=item B<generate updmap>

=back

The C<generate> action overwrites any manual changes made in the
respective files: it recreates them from scratch.

For C<fmtutil> and the language files, this is normal, and both the TeX
Live installer and C<tlmgr> routinely call I<generate> for them.

For C<updmap>, however, neither the installer nor C<tlmgr> use
C<generate>, because the result would be to disable all maps which have
been manually installed via S<C<updmap-sys --enable>>, e.g., for
proprietary or local fonts.  Only the changes in the C<--localcfg> file
mentioned below are incorporated by I<generate>.  Furthermore, C<tlmgr>
updates and maintains the final C<updmap.cfg> in C<TEXMFSYSCONFIG> (the
others use C<TEXMFSYSVAR>) because that is the location that
C<updmap-sys> (via C<tcfmgr>) uses.

Notwithstanding the above, if you only use the fonts and font packages
within TeX Live, and maintain your local fonts (if any) with the local
config file, there is nothing wrong with using C<generate updmap>.
Indeed, we use it ourselves to generate the C<updmap.cfg> file that is
maintained in the live source repository.

In more detail: C<generate> remakes any of the four config files
C<language.dat>, C<language.def>, C<fmtutil.cnf>, and C<updmap.cfg> from
the information present in the local TLPDB. If the files
C<language-local.dat>, C<language-local.def>, C<fmtutil-local.cnf>, or
C<updmap-local.cfg> are present under C<TEXMFLOCAL> in the respective
directories, their contents will be simply merged into the final files,
with no error checking of any kind.

The form C<generate language> recreates both the C<language.dat> and the
C<language.def> files, while the forms with extension only recreates
the given file.

Options:

=over 4

=item B<--dest> I<output file>

specifies the output file (defaults to the respective location in
C<TEXMFSYSVAR> for C<language*> and C<fmtutil>, and C<TEXMFSYSCONFIG>
for C<updmap>).  If B<--dest> is given to C<generate language>, its
value will be used for the C<language.dat> output, and C<.def> will be
appended to the value for the name of the C<language.def> output file.
(This is just to avoid overwriting; if you want a specific name for each
output file, we recommend invoking C<tlmgr> twice.)

=item B<--localcfg> I<local conf file>

specifies the (optional) local additions (defaults to the respective
location in C<TEXMFLOCAL>).

=back

The respective locations are as follows:

  tex/generic/config/language.dat (and language-local.dat);
  tex/generic/config/language.def (and language-local.def);
  web2c/fmtutil.cnf (and fmtutil-local.cnf);
  web2c/updmap.cfg (and updmap-local.cfg).


=head1 MACHINE-READABLE OUTPUT

Given the C<--machine-readable> option, C<tlmgr> writes to stdout in the
fixed line-oriented format described here, and the usual informational
messages for human consumption are written to stderr (normally they are
written to stdout).  The idea is that a program can get all the
information it needs by reading stdout.

Currently this option only applies to the L<update|/"update
[I<option>]... [I<pkg>]..."> action.  The output format is as follows:

  fieldname "\t" value
  ...
  "end-of-header"
  pkgname status localrev serverrev size
  ...  

Currently the header section contains two fields.  The field name and
value are separated by a tab.  Line endings may be LF or CRLF depending
on the current platform.

=over 4

=item C<location-url> I<location>

The I<location> may be a url (including C<file:///foo/bar/...>), or a
directory name (C</foo/bar>).  It is the server repository from which
the new package information was drawn.

=item C<total-bytes> I<count>

The I<count> is simply a decimal number, the sum of the sizes of all the
packages that need updating or installing (which are listed subseqently).

=back

Then comes a line with only the literal string C<end-of-header>.

Each following line until EOF reports on one package.  The fields on
each line are separated by a tab.  Here are the fields.

=over 4

=item I<pkgname>

The TeX Live package identifier, with a possible architecture suffix for
executables.   For instance, C<pdftex> and C<pdftex.i386-linux> are
given as two separate packages, one on each line.

=item I<status>

The status of the package update.  One character, as follows:

=over 8

=item C<d>

The package was removed on the server.

=item C<f>

The package was removed in the local installation, even though a
collection depended on it.  (E.g., the user ran C<tlmgr remove
--force>.)

=item C<u>

Normal update is needed.

=item C<r>

Reversed non-update: the locally-installed version is newer than the
version on the server.

=item C<a>

Automatically-determined need for installation, the package is new on
the server and is (most probably) part of an installed collection.

=back

=item I<localrev>

The revision number of the installed package, or C<-> if it is not
present locally.

=item I<serverrev>

The revision number of the package on the server, or C<-> if it is not
present on the server.

=item I<size>

The size in bytes of the package on the server.  The sum of all the
package sizes is given in the C<total-bytes> header field mentioned above.

=back

If you are developing a program that uses this output, and find that
changes would be helpful, do not hesitate to write the mailing list.


=head1 AUTHORS AND COPYRIGHT

This script and its documentation were written for the TeX Live
distribution (L<http://tug.org/texlive>) and both are licensed under the
GNU General Public License Version 2 or later.

=cut

# to remake HTML version: pod2html --cachedir=/tmp tlmgr.pl >/foo/tlmgr.html.

### Local Variables:
### perl-indent-level: 2
### tab-width: 2
### indent-tabs-mode: nil
### End:
# vim:set tabstop=2 expandtab: #
