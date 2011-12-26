#!/usr/bin/env perl
#
# $Id: tlmgrgui.pl 12151 2009-02-12 12:07:22Z preining $
#
# Copyright 2008 Tomasz Luczak, Norbert Preining
#
# GUI for tlmgr
#


use Tk;
use Tk::Dialog;
use Tk::NoteBook;
use Tk::BrowseEntry;
use Tk::ROText;
use Tk::Balloon;
use TeXLive::Splashscreen;

use TeXLive::TLUtils qw(setup_programs platform_desc win32 debug);

our $Master;
our $tlmediatlpdb;
our $tlmediasrc;
our $location;
#
# translation facility, not fully functional by now
#
our %TRANS;
our $LANG;
our %opts;

$TRANS{'en'} = {
  about         => "About",
  addpkg        => "Adding packages",
  archs         => "Architectures",
  cancel        => "Cancel",
  change        => "Change",
  changesrc     => "Change Location",
  changesrclong => "Change source from where packages are fetched at installation and update time.",
  changedefaultsrc => "Change default installation source",
  newdefaultsrc => "New default installation source",
  choosedir     => "Choose Directory",
  config        => "Configuration",
  createformats => "Create formats on installation",
  currentsource => "Current installation source: ",
  debug         => "Debug",
  defaultsource => "Default installation source",
  defaultnet    => "Default net location",
  defaultpaperfor => "Default paper for",
  defaultpaperall => "Default paper for all",
  defaultsettings => "Default settings",
  force         => "Force",
  forceballoon  => "Force the removal of a package even if it is referenced in a collection.",
  nodepballoon  => "For collections: install or remove will not install/remove the dependencies",
  infoitem      => "Information on the selected item",
  install       => "Installation",
  installdoc    => "Install macro/font docs",
  installsrc    => "Install macro/font sources",
  installsel    => "Install selected",
  load          => "Load",
  warningtxt    => "The database of the installation source has not been loaded.\n\nPlease use the \"Load\" (and possibly \"Change\") button to do so.",
  newsource     => "New location: ",
  next          => "Next",
  ok            => "Ok",
  paperfor      => "Select paper format for",
  papersettings => "Paper settings",
  pressbutton   => "Press this button to load the database from the specified location.",
  quit          => "Quit",
  reallyremove  => "Really remove the complete TeX Live 2008 installation?\nYour last chance to change your mind!",
  remove        => "Remove",
  removesel     => "Remove selected",
  removetl      => "Remove TeX Live 2008",
  rempkg        => "Removing packages",
  search        => "Search",
  remarchnotpos => "Select architectures to be added (removal not possible)",
  selpkg        => "Select packages",
  toggle        => "Toggle",
  debugballoon  => "Turn on debug mode when calling tlmgr.",
  removaltab    => "Uninstallation",
  update        => "Update",
  updateall     => "Update all",
  updatesel     => "Update selected",
  updatepkg     => "Updating packages",
  ctrlshift     => "Use Ctrl or Shift or drag to select more",
  withoutdep    => "without depends",
  yes           => "Yes",
  no            => "No",
  starting      => "Starting",
  maytaketime   => "This may take some time!\nPlease wait, the output will appear here when ready.\n",
  completed     => "Completed",
  loaderrortxt  => 'Could not load the TeX Live Database from $newroot\nIf you want to install or update packages, please try with a different installation source/location!\n\nFor configuration and removal you don\'t have to do anything.',
  changeme      => "...please change me...",
  nodescription => "(no description available)",
  applychanges  => "Apply changes",
  resetchanges  => "Reset changes",
  remarchinfo   => "Removals of binary systems currently not supported!",
  pleaseuse     => "Please use the \"Add/Remove Programs\" from the Control Panel!",
  completerem   => "Complete removal completed",
  loadtlpdbwait => "Loading local TeX Live Database\nThis may take some time, please wait!",
  loadremotetlpdbwait => "Loading remote TeX Live Database\nThis may take some time, please wait!",
  runupdater    => "Some package will only be updated after you have terminated the GUI!\nPlease do so and after a short time restart the GUI.",
  actions       => "Actions",
  reinitlsr     => "Re-initialize file database",
  recreateformats => "Re-create all formats",
  updatemaps    => "Update font map database",
  warningnonetupdate => "No updates found.\n\nYour installation is set up to look on the disk for updates.\n\nIf you want to install from the Internet for this one time only, click on the \"Change\" button above and select \"Default Net Location\" (or any other network location you know to be working).\n\nIf you want to change it permanently, go to the \"Configuration\" Tab and change the default installation source.",
  pleaseclick   => "Please click on an item on the left for details",
  alluptodate   => "Everything up-to-date!",
  # new ones
  backupdir     => "Default backup directory",
  autobackup    => "Auto backup setting",
  critupdates   => "Updates for the tlmgr are present.\nInstallation and upgrades won't work without being forced.\nPlease go to the update screen and press the \"update all\" button.\nThe program will terminate after the update.\nThen you can restart the program for further updates.",
  critupdw32    => "Please wait a bit after the program has terminated so that the update can be completed."
};


sub update_status_window {
  update_status(join(" ", @_));
  $mw->update;
  $::sww->update;
}

sub init_hooks {
  push @::info_hook, \&update_status_window;
  push @::warn_hook, \&update_status_window;
  push @::debug_hook, \&update_status_window;
  push @::ddebug_hook, \&update_status_window;
  push @::dddebug_hook, \&update_status_window;
}

sub update_status {
  my ($p) = @_;
  $::progressw->insert("end", "$p");
  $::progressw->see("end");
}


#
# we keep the translations in different arrays since we MAY add the feature
# to switch language on the fly
sub ___ ($) {
  my $s = shift;
  # if no $LANG is set just return without anything
  return $TRANS{"en"}->{$s} if !defined($LANG);
  # if the translation is defined return it
  return $TRANS{$LANG}->{"$s"} if defined($TRANS{$LANG}->{"$s"});
  return $TRANS{"en"}->{$s} if defined($TRANS{"en"}->{$s});
  return "$s";
}

my $opt_screen = $::guiscreen;

if (defined($opts{"gui-lang"})) {
  $LANG = $opts{"gui-lang"};
} else {
  if ($^O =~ /^MSWin(32|64)$/i) {
    # trying to deduce automatically the country code
    my $foo =  TeXLive::TLWinGoo::reg_country();
    if ($foo) {
      $LANG = $foo;
    } else {
      warn "Didn't get any useful code from reg_country: $foo...\n";
    }
  } else {
    # we load POSIX and locale stuff
    require POSIX;
    import POSIX qw/locale_h/;
    # now we try to deduce $LANG
    my $loc = setlocale(&POSIX::LC_MESSAGES);
    my ($lang,$area,$codeset);
    if ($loc =~ m/^([^_.]*)(_([^.]*))?(\.([^@]*))?(@.*)?$/) {
      $lang = defined($1)?$1:"";
    }
    $LANG = $lang if ($lang);
  }
}


#
# try loading the lang file
#
if (defined($LANG) && (-r "$Master/texmf/scripts/texlive/tlmgrgui/lang/$LANG")) {
  open(LANG,"<$Master/texmf/scripts/texlive/tlmgrgui/lang/$LANG");
  while (<LANG>) {
    chomp;
    next if m/^\s*#/;
    next if m/^\s*$/;
    my ($a,$b) = split(/:/,$_,2);
    $b =~ s/^\s*([^\s])/$1/;
    $b =~ s/\s*$//;
    next if ($b =~ m/^\s*$/);
    if (!utf8::decode($b)) {
      warn("decoding string to utf8 didn't work:$b\n");
    }
    $b =~ s/\\n/\n/g;
    $TRANS{$LANG}{"$a"} = "$b";
  }
}

our @update_function_list;

our $mw = MainWindow->new;
$mw->title("TeX Live Manager 2008");
$mw->withdraw;

# create a progress bar window
$::sww = $mw->Toplevel(-title => "log window",
                       -width => 400);
$::sww->transient($mainwindow);
#$::sww->grab();
$::sww->Label(-text => "Log output")->pack;
$::progressw = $::sww->Scrolled("ROText", -scrollbars => "e", -height => 16);
$::progressw->pack(-expand => 1, -fill => "both");


init_hooks();

info(___("loadtlpdbwait") . "\n");

our $localtlpdb = TeXLive::TLPDB->new ("root" => "$Master");
die("cannot find tlpdb!") unless (defined($localtlpdb));

our @alllocalpackages = setup_list(0,$localtlpdb->list_packages);
our @updatepackages;
setup_programs("$Master/tlpkg/installer", $localtlpdb->option_platform);

my $loc = $localtlpdb->option_location;
if (defined($loc)) {
  $location = $loc;
}
if (defined($opt_location)) {
  $location = $opt_location;
}

our @allpackages;

our $balloon = $mw->Balloon();


push @update_function_list, \&check_location_on_ctan;
push @update_function_list, \&init_install_media;
push @update_function_list, \&create_update_list;


our $top = $mw->Frame;

our $quit = $top->Button(-text => ___"quit",
                         -command => sub { $mw->destroy; exit(0); });

my $tlmgrrev = give_version();
chomp($tlmgrrev);
our $about = $top->Button(-text => ___"about",
  -command => sub {
    my $sw = $mw->DialogBox(-title => ___"about", 
                            -buttons => [ ___"ok" ]);
    $sw->add("Label", -text => "TeX Live Manager
$tlmgrrev
Copyright 2008 Tomasz Luczak, Norbert Preining
License under the GNU General Public License version 2 or higher
In case of problems, please contact: texlive\@tug.org"
      )->pack(-padx => "3m", -pady => "3m");
    $sw->Show;
    });

$about->pack(-side => 'right');
$quit->pack(-side => 'right');

$mw->bind('<Escape>', [ $quit, 'Invoke' ]);

$top->Label(-text => ___("currentsource") . " ")->pack(-side => 'left');
$top->Label(-textvariable => \$location,  -relief => "sunken")->pack(-side => 'left');

$balloon->attach(
  $top->Button(-text => ___"load", -command => sub { run_update_functions(); })->pack(-side => 'left'),
  -balloonmsg => ___"pressbutton");

$balloon->attach(
  $top->Button(-text => ___"change", -command => sub { menu_edit_location(); })->pack(-side => 'left'),
  -balloonmsg => ___"changesrclong");

# here we add a bit of trickery to make sure that the verbosity level
# as set on the cmd line (-v or -v -v) is kept during pressing the 
# debug button on and off. If -v is *not* given activating it afterwards
# defaults to one -v, while if -v -v is given on the cmd line, we keep
# it that way
$balloon->attach(
  $top->Checkbutton(-text => ___"debug", 
                    -onvalue => ($opt_verbosity == 0 ? 1 : $opt_verbosity),
                    -variable => \$::opt_verbosity)->pack(-side => 'left'),
  -balloonmsg => ___"debugballoon");

# frame .back -borderwidth 2
our $back = $mw->NoteBook(-borderwidth => 2, -dynamicgeometry => 1);


# pack .top .back -side top -fill both -expand 1
$top->pack(-side => 'top', -fill => 'x', -expand => 0);
$back->pack(-side => 'top', -fill => 'both', -expand => 1);

require ("do_listframe.pl");
# install screen
our $back_f1 = $back->add("install",-label => ___"install");
$screens{"install"} = $back_f1;
my ($install_win, $install_lb) = do_listframe($back_f1,
             ___"addpkg",
             \@allpackages,
             { install => { -text => ___"installsel",
                            -command => \&install_selected_packages}},
             1,1
            );
set_text_win($install_win, ___"warningtxt");
# update screen
our $back_up = $back->add("update", -label => ___"update");
$screens{"update"} = $back_up;
my $critical_updates_present = 0;
my ($update_win, $update_lb) = do_listframe($back_up,
             ___"updatepkg",
             \@updatepackages,
             { updateall => { -text => ___"updateall",
                              -command => \&update_selected_packages,
                              -args => [ "--all" ]
                            },
               updatesel => { -text => ___"updatesel",
                              -command => \&update_selected_packages
                            }},
             1,0
            );
set_text_win($update_win, ___"warningtxt");
# remove screen
our $back_f2 = $back->add("remove", -label => ___"remove");
$screens{"remove"} = $back_f2;
my ($remove_win, $remove_lb) = do_listframe($back_f2,
             ___"rempkg",
             \@alllocalpackages,
             { remove => { -text => ___"removesel",
                           -command => \&remove_selected_packages}},
             1,1
            );
set_text_win($remove_win, ___"pleaseclick");
# uninstall screen
require("gui-uninstall.pl");
# arch support not be done via tlmgr on win32
if (!win32()) {
  require("gui-arch.pl");
}
# config screen
require("gui-config.pl");

if ($opt_load) {
  run_update_functions();
}


if (defined($opt_screen)) {
  $back->raise("$opt_screen");
}

info(___("completed") . "\n");
$mw->deiconify;



Tk::MainLoop();


sub init_install_media {
  my $newroot = $location;
  if (defined($tlmediatlpdb) && ($tlmediatlpdb->root eq $newroot)) {
    # nothing to be done
  } else {
    $mw->Busy(-recurse => 1);
    info(___("loadremotetlpdbwait") . "\n");
    $tlmediasrc = TeXLive::TLMedia->new($newroot);
    info(___("completed") . "\n");
    $mw->Unbusy;
    if (!defined($tlmediasrc)) {
      # something went badly wrong, maybe the newroot is wrong?
      $mw->Dialog(-title => "warning",
                 -text => ___"loaderrortxt",
                        -buttons => [ ___"ok" ])->Show;
      $location = ___"changeme";
      @allpackages = ();
    } else {
      $tlmediatlpdb = $tlmediasrc->tlpdb;
      @allpackages = setup_list(1,$tlmediatlpdb->list_packages);
      set_text_win($install_win, ___"pleaseclick");
      set_text_win($remove_win,  ___"pleaseclick");
      set_text_win($update_win,  ___"pleaseclick");
    }
  }
}

sub set_text_win {
  my ($w, $t) = @_;
  $w->delete("0.0", "end");
  $w->insert("0.0", "$t");
  $w->see("0.0");
}

sub run_program_show_output {
  my $td = $mw->Toplevel(-title => ___"tlmgr process");
  $td->transient($mw);
  $td->grab();
  my $tf = $td->Scrolled("ROText", -width => 80,
                                   -height => 10,
                                   -wrap => "none",
                         -scrollbars => "ose"
                        )->pack(-expand => 1, -fill => "both");
  my $ok = $td->Button(-text => ___"ok", -padx => "3m", -pady => "3m",
                       -command => sub { $td->destroy; });
  # start the installation, read the output
  $mw->update;
  $::sww->update;
  #
  # ok, that stupid perl for windows does not have fork, why? no idea
  # we have to deal with that
  if ($^O=~/^MSWin(32|64)$/i) {
    $tf->insert("end", ___("starting") . " @_\n\n" . ___"maytaketime");
    $mw->update;
    $::sww->update;
    my $ret = `@_`;
    $tf->insert("end", "$ret\n\n" . ___("completed") . "\n");
    $tf->see("end");
    $ok->pack;
    $mw->update;
    $::sww->update;
  } else {
    my $pid = open(KID_TO_READ, "-|");
    if ($pid) {   # parent
      while (<KID_TO_READ>) {
        $tf->insert("end",$_);
        $tf->see("end");
        $mw->update;
        $::sww->update;
      }
      close(KID_TO_READ) || warn "kid exited $?";
      $tf->insert("end","\n\nCOMPLETED\n");
      $tf->see("end");
      $ok->pack;
    } else { #kid
      # do not buffer lines ...
      $| = 1;
      open STDERR, '>&STDOUT';
      print ___("starting") . " @_\n";
      exec(@_)
        || die "can't exec program: $!";
      # NOTREACHED
    }
  }
}

sub install_selected_packages {
  if (@_) {
    my @args = qw/install/;
    push @args, @_;
    execute_action_gui(@args);
    reinit_local_tlpdb();
    # now we check that the installation has succeeded by checking that 
    # all packages in @_ are installed. Otherwise we pop up a warning window
    my $do_warn = 0;
    for my $p (@_) {
      if (!defined($localtlpdb->get_package($p))) {
        $do_warn = 1;
        last;
      }
    }
    give_warning_window(___"install", @_) if $do_warn;
  }
}

sub update_selected_packages {
  if (@_) {
    my @args = qw/update/;
    # argument processing
    # in case we have critical updates present we do put the list of
    # critical updates into the argument instead of --all
    if ($_[0] eq "--all") {
      if ($critical_updates_present) {
        push @args, @updatepackages;
      } else {
        $opts{"all"} = 1;
      }
      # shift away the --all
      shift;
    }
    push @args, @_;
    execute_action_gui(@args);
    if ($critical_updates_present) {
      # terminate here immediately so that we are sure the auto-updater
      # is run immediately
      # make sure we exit in finish(0)
      $::gui_mode = 0;
      finish(0); 
    }
    reinit_local_tlpdb();
  }
}

sub remove_selected_packages {
  if (@_) {
    my @args = qw/remove/;
    push @args, @_;
    execute_action_gui(@args);
    reinit_local_tlpdb();
    my $do_warn = 0;
    for my $p (@_) {
      if (defined($localtlpdb->get_package($p))) {
        $do_warn = 1;
        last;
      }
    }
    give_warning_window(___"remove", @_) if $do_warn;
  }
}

sub reinit_local_tlpdb {
  $mw->Busy(-recurse => 1);
  $localtlpdb = TeXLive::TLPDB->new ("root" => "$Master");
  die("cannot find tlpdb!") unless (defined($localtlpdb));
  @alllocalpackages = setup_list(0,$localtlpdb->list_packages);
  if (defined($tlmediatlpdb)) {
    @allpackages = setup_list(1,$tlmediatlpdb->list_packages);
  }
  create_update_list();
  $mw->Unbusy;
}

#
# create_update_list
# checks for updates and builds up the list @updatepackages which
# is shown on the update screen
#
# if critical packages do have a pending update *only* those packages
# are shown in the list so that pressing the --all will succeed
sub create_update_list {
  my @ret = ();
  my @archret = ();
  my @critical = $localtlpdb->expand_dependencies("-no-collections",
    $localtlpdb, @TeXLive::TLConfig::CriticalPackagesList);
  if (defined($tlmediatlpdb)) {
    foreach my $lp ($localtlpdb->list_packages) {
      next if ($lp =~ m/00texlive-installation.config/);
      my $lrev = $localtlpdb->get_package($lp)->revision;
      my $up = $tlmediatlpdb->get_package($lp);
      my $urev;
      if ($up) {
        $urev = $up->revision;
      } else {
        $urev = 0;
      }
      if ($urev > $lrev) {
        if ($lp =~ m/\./) {
          push @archret, $lp;
        } else {
          push @ret, $lp;
        }
      }
    }
    foreach my $p (@archret) {
      my $foundparent = 0;
      foreach my $q (@ret) {
        $foundparent = 1 if ($p =~ m/^$q\./);
      }
      push @ret, $p unless $foundparent;
    }
    # issue a warning if no updates are available, the tlmediatlpdb is loaded
    # and is not from the net
    if ($#ret < 0) {
      if ($tlmediasrc->media ne "NET") {
        set_text_win($update_win, ___"warningnonetupdate");
      } else {
        set_text_win($update_win, ___"alluptodate");
      }
    }
  } else {
    @ret = ();
  }
  # sort the critical packages out
  my @critupd = ();
  OUTER: for my $p (@ret) {
    for my $cp (@critical) {
      if ($p =~ m/^$cp/) {
        push @critupd, $p;
        next OUTER;
      }
    }
  }
  if (@critupd) {
    @updatepackages = @critupd;
    # set background of critical packages to red
    for my $i (0..$#updatepackages) {
      $update_lb->itemconfigure($i, -background => "red", 
                                    -selectforeground => "red");
    }
    $critical_updates_present = 1;
  } else {
    @updatepackages = @ret;
    $critical_updates_present = 0;
  }
  if ($critical_updates_present) {
    my $sw = $mw->DialogBox(-title => ___"warning", -buttons => [ ___"ok" ]);
    my $t = ___"critupdates";
    $t .= "\n\n" . ___"critupdw32" if win32();
    $sw->add("Label", -text => $t)->pack(-padx => "3m", -pady => "3m");
    $sw->Show;
  }
}

sub setup_list {
  my $addi = shift;
  my @ret;
  my @other;
  foreach my $p (@_) {
    if ($p !~ m;\.;) {
      my $pushstr = "";
      if ($addi) {
        if (defined($localtlpdb->get_package($p))) {
          $pushstr = "(i) ";
        } else {
          $pushstr = "    ";
        }
      }
      $pushstr .= "$p";
      if ($p =~ m;^collection-;) {
        push @ret, $pushstr;
      } else {
        push @other, $pushstr;
      }
    }
  }
  push @ret, @other;
  return(@ret);
}


sub menu_edit_location {
  my $key = shift;
  my $val;
  my $sw = $mw->Toplevel(-title => ___"changesrc");
  $sw->transient($mw);
  $sw->grab();
  $sw->Label(-text => ___"newsource")->pack(-padx => "2m", -pady => "2m");
  my $entry = $sw->Entry(-text => $location, -width => 30);
  $entry->pack();
  my $f1 = $sw->Frame;
  $f1->Button(-text => ___"choosedir",
    -command => sub {
                      my $var = $sw->chooseDirectory;
                      if (defined($var)) {
                        $entry->delete(0,"end");
                        $entry->insert(0,$var);
                      }
                    })->pack(-side => "left", -padx => "2m", -pady => "2m");
  $f1->Button(-text => ___"defaultnet",
    -command => sub {
                      $entry->delete(0,"end");
                      $entry->insert(0,$TeXLiveURL);
                    })->pack(-side => "left", -padx => "2m", -pady => "2m");
  $f1->pack;
  my $f = $sw->Frame;
  my $okbutton = $f->Button(-text => ___"ok",
    -command => sub { $location = $entry->get;
                      run_update_functions();
                      $sw->destroy;
                    })->pack(-side => 'left', -padx => "2m", -pady => "2m");
  my $cancelbutton = $f->Button(-text => 'Cancel',
          -command => sub { $sw->destroy })->pack(-side => 'right', -padx => "2m", -pady => "2m");
  $f->pack(-expand => 'x');
  $sw->bind('<Return>', [ $okbutton, 'Invoke' ]);
  $sw->bind('<Escape>', [ $cancelbutton, 'Invoke' ]);
}

sub run_update_functions {
  foreach my $f (@update_function_list) {
    &{$f}();
  }
}

sub check_location_on_ctan {
  # we want to check that if mirror.ctan.org
  # is used that we select a mirror once
  if ($location =~ m/$TeXLive::TLConfig::TeXLiveServerURL/) {
    $location = TeXLive::TLUtils::give_ctan_mirror();
  }
}

sub execute_action_gui {
  # my $td = $mw->Toplevel(-title => ___"info window");
  # $td->transient($mw);
  # $td->grab();
  # my $ok = $td->Button(-text => ___"ok", -padx => "3m", -pady => "3m",
  #                      -command => sub { $td->destroy; });
  # my $lab = $td->Label(-text => ___("starting") . " @_\n");
  # $lab->pack;
  $mw->Busy(-recurse => 1);
  execute_action(@_);
  info(___("completed") . "\n");
  $mw->Unbusy;
  # my $labb = $td->Label(-text => ___"finished");
  # $labb->pack;
  # $ok->pack;
}

sub give_warning_window {
  my ($act, @args) = @_;
  my $sw = $mw->DialogBox(-title => ___"info window", -buttons => [ ___"ok" ]);
  $sw->add("Label", -text => "Executing action $act on @args failed.
Please consult the log window for details."
    )->pack(-padx => "3m", -pady => "3m");
  $sw->Show;
}

1;

__END__


### Local Variables:
### perl-indent-level: 2
### tab-width: 2
### indent-tabs-mode: nil
### End:
# vim:set tabstop=2 expandtab: #
