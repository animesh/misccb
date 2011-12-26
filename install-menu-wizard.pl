#!/usr/bin/env perl
# $Id: install-menu-wizard.pl 12114 2009-02-07 23:42:14Z preining $
#
# Copyright 2009 Norbert Preining
# This file is licensed under the GNU General Public License version 2
# or any later version.
#

use strict;
$^W = 1;

my $svnrev = '$Revision: 11925 $';
$svnrev =~ m/: ([0-9]+) /;
$::menurevision = $1;


our %vars;
our $tlpdb;
our $texlive_release;

our $MENU_INSTALL = 0;
our $MENU_ABORT   = 1;
our $MENU_QUIT    = 2;
our $MENU_ALREADYDONE = 3;


my $return = $MENU_INSTALL;

require Tk;
require Tk::Dialog;
require Tk::DialogBox;
require Tk::PNG;
require Tk::ROText;
require Tk::ProgressBar;
require Tk::Font;

use utf8;
no utf8;

#
#
my $tit;
my $can;
my $prv;
my $nxt;
my $img;
my $dest;
my $warning;
my $mw;
my $usedfont;
my $fmain;
my $fbuttons;
my $ftitle;
my $counter;
my $lineskip;

my $LEFT = 130;
my $RIGHT = 50;
my $TOP  = 50;
my $BOTTOM = 50;
my $INF = 300;
my $MWIDTH = 730;
my $MHEIGHT = 480;
my $TITLEHEIGHT = 30;
my $BUTTONSHEIGHT = 50;
my $INNERWIDTH = ($MWIDTH - $LEFT - $RIGHT);
my $INNERHEIGHT = ($MHEIGHT - $TOP - $TITLEHEIGHT - $BOTTOM - $BUTTONSHEIGHT);

our %text;
our %labels;
require("installer/install-translations.pl");

# the main installer runs %{$::run_menu}
$::run_menu = \&run_menu_wizard;

######################################################################
# From here on only function definitions
# ####################################################################

sub setup_hooks_wizard {
  @::info_hook = ();
  push @::info_hook,
    sub {
      return unless defined($mw);
      wizard_update_status(join(" ",@_));
      $mw->update;
    };
  @::warn_hook = ();
  push @::warn_hook,
    sub {
      return unless defined($mw);
      wizard_update_status(join(" ",@_));
      $mw->update;
    };
  @::install_packages_hook = ();
  push @::install_packages_hook, \&wizard_update_progressbar;
  push @::install_packages_hook, sub { $mw->update; };
}

sub wizard_update_status {
  my ($p) = @_;
  $::progressw->insert("end", "$p");
  $::progressw->see("end");
}
sub wizard_update_progressbar {
  my ($n,$total) = @_;
  if (defined($n) && defined($total)) {
    $::progress->value(int($n*100/$total));
  }
}

################# WELCOME SCREEN ######################################

sub run_menu_wizard {
  $mw = Tk::MainWindow->new(-width => $MWIDTH, -height => $MHEIGHT);
  setup_hooks_wizard();

  $dest = $vars{'TEXDIR'};

  my $img = $mw->Photo(-format => 'png', -file => "$::installerdir/tlpkg/installer/texlive2008-wizard.png");
  $mw->Label(-image => $img, -background => "#0078b8")->place(-x => 0, -y => 0);

  $ftitle = $mw->Frame(-height => $TITLEHEIGHT, -width => $INNERWIDTH);
  $ftitle->update;
  $ftitle->place(-x => $LEFT, -y => $TOP);

  $tit = $ftitle->Label(-text => $text{'title'});

  $usedfont= $tit->cget("-font");
  $lineskip = $usedfont->metrics("-linespace");

  $tit->place(-x => 0, -y => 0);

  $counter = $ftitle->Label(-text => "1/4");
  $counter->place(-x => $INNERWIDTH, -y => 0, -anchor => "ne");

  $fmain = $mw->Frame(-height => $INNERHEIGHT, -width => $INNERWIDTH);
          #, -borderwidth => 1, -relief => "ridge");
  $fmain->place(-x => $LEFT, -y => ($TOP + $TITLEHEIGHT));


  $can = $mw->Button(-width => 10, -relief => "ridge", -text => $text{'cancel'},
               -command => sub { $return = $MENU_ABORT; $mw->destroy; });
  $nxt = $mw->Button(-width => 10, -relief => "ridge", -text => $text{'next'});
  $prv = $mw->Button(-width => 10, -relief => "ridge", -text => $text{'prev'});

  $can->place(-x => $LEFT, -y => ($MHEIGHT - $BOTTOM), -anchor => "sw");

  my $rb = $MWIDTH - $RIGHT;
  $nxt->place(-x => ($MWIDTH - $RIGHT) , 
              -y => ($MHEIGHT - $BOTTOM), -anchor => "se");

  reset_start();

  Tk::MainLoop();
  return($return);
}

sub reset_start {
  for ($fmain->children) {
    $_->destroy;
  }
  $counter->configure(-text => "1/4");
  $prv->placeForget;

  my $inf = $fmain->Label(-text => $text{'wizhello'}, -justify => "left");
  $inf->place(-x => 0, -y => 100);

  $nxt->configure(-text => $text{'next'}, -command => \&ask_path );
  $nxt->configure(-state => "normal");
}

################## PATH SCREEN ################################

sub ask_path {
  for ($fmain->children) {
    $_->destroy;
  }
  $counter->configure(-text => "2/4");
  my $lab = $fmain->Label(-text => $text{'destfolder'});
  my $val = $fmain->Label(-textvar => \$dest);
  my $but = $fmain->Button(-text => $text{'change'}, -command => \&change_path,
                           -relief => "ridge", -width => 10);

  my $pa = $fmain->Checkbutton(-text => $labels{'optletter'}, 
                               -variable => \$vars{"option_letter"});
  #
  # disable the "Advanced Configuration" button switching to the 
  # perltk installer
  #
  #my $cb = $fmain->Button(-text => $text{'advcustom'}, 
  #       -relief => "ridge",
  #       -command => sub { $mw->destroy; 
  #                         require("installer/install-menu-perltk.pl");
  #                         setup_hooks_perltk();
  #                         $return = run_menu_perltk();
  #                       });

  calc_depends();


  $fmain->Label(-text => $text{'pathinfo'}, 
                -justify => "left")->place(-x => 0, -y => 30);

  my $ytmp = 100;
  $lab->place(-x => 0, -y => $ytmp, -anchor => "w");
  $ytmp += ($lineskip + 10);
  $val->place(-x => 0, -y => $ytmp, -anchor => "w");

  $but->place(-x => $INNERWIDTH, -y => $ytmp, -anchor => "e");

  $warning = $fmain->Label(-foreground => "red");
  check_show_warning();
  $ytmp += ($lineskip + 10);
  $warning->place(-x => 0, -y => $ytmp, -anchor => "w");


  $pa->place(-x => 0, -y => $ytmp + 30);


  #$cb->place(-x => $INNERWIDTH, -y => $INNERHEIGHT, -anchor => "se");

  $fmain->Label(-text => "$text{'diskreq'}: $vars{'total_size'} MB", 
                -justify => "left"
             )->place(-x => 0, -y => $fmain->height, -anchor => "sw");

  $prv->configure(-text => $text{'prev'}, -command => \&reset_start );
  $nxt->configure(-text => $text{'next'}, -command => \&ask_go );

  my $rb = $MWIDTH - $RIGHT;
  $rb -= $nxt->width;
  $rb -= 30;

  $prv->place(-x => $rb, -y => ($MHEIGHT - $BOTTOM), -anchor => "se");
}

sub check_show_warning {
  if (TeXLive::TLUtils::texdir_check($vars{'TEXDIR'})) {
    $warning->configure(-text => "");
    $nxt->configure(-state => "normal");
  } else {
    $warning->configure(-text => $text{'notwritable'});
    $nxt->configure(-state => "disabled");
  }
}
  
sub change_path {
  my $val = $dest;
  my $sw = $mw->Toplevel(-title => "Changing TEXDIR");
  $sw->transient($mw);
  $sw->grab();
  $sw->Label(-text => $text{'enterpath'} . " TEXDIR: ")->pack(-padx => "2m", -pady => "2m");
  my $entry = $sw->Entry(-textvariable => $val, -width => 60);
  $entry->pack(-padx => "2m", -pady => "2m");
  my $f = $sw->Frame;
  my $okbutton = $f->Button(-text => $text{'ok'}, -width => 10, 
     -relief => "ridge",
     -command => sub { $val = $entry->get; callback_change_texdir($val) ; $sw->destroy })->pack(-side => 'left', -padx => "2m", -pady => "2m");
  my $cancelbutton = $f->Button(-text => $text{'cancel'}, -relief => "ridge",
     -width => 10,
     -command => sub { $sw->destroy })->pack(-side => 'right', -padx => "2m", -pady => "2m");
  $f->pack(-expand => 'x');
  # bindings
  $sw->bind('<Return>' => [ $okbutton, 'Invoke']);
  $sw->bind('<Escape>' => [ $cancelbutton, 'Invoke']);
}

sub callback_change_texdir {
  my ($val) = @_;
  my $home = getenv('HOME');
  if (win32()) {
    $home = getenv('USERPROFILE');
    $home =~ s!\\!/!g;
  }
  $home ||= '~';
  $val =~ s!\\!/!g;
  $vars{'TEXDIR'} = $val;
  $vars{'TEXDIR'} =~ s/^~/$home/;
  $vars{'TEXMFLOCAL'} =~ s/^~/$home/;
  $vars{'TEXMFSYSVAR'} =~ s/^~/$home/;
  $vars{'TEXMFSYSCONFIG'} =~ s/^~/$home/;
  # only if we set TEXDIR we set the others in parallel
  if ($vars{'TEXDIR'}=~/^(.*)\/$texlive_release$/) {
    $vars{'TEXMFLOCAL'}="$1/texmf-local";
    $vars{'TEXMFSYSVAR'}="$1/$texlive_release/texmf-var";
    $vars{'TEXMFSYSCONFIG'}="$1/$texlive_release/texmf-config";
  } elsif ($vars{'TEXDIR'}=~/^(.*)$/) {
    $vars{'TEXMFLOCAL'}="$1/texmf-local";
    $vars{'TEXMFSYSVAR'}="$1/texmf-var";
    $vars{'TEXMFSYSCONFIG'}="$1/texmf-config";
  }
  $vars{'TEXDIRW'}=$vars{'TEXDIR'};
  $dest = $vars{'TEXDIR'};
  check_show_warning();
}

################## INSTALL SCREEN #############################

sub ask_go {
  for ($fmain->children) {
    $_->destroy;
  }
  $counter->configure(-text => "3/4");
  my $inf = $fmain->Label(-justify => "left", -text => $text{'readyinst'} . "\n\n" . $text{'destfolder'} . ":   $dest");

  $inf->place(-x => 0, -y => 100);
  
  $nxt->configure(-text => $text{'instshort'}, 
                  -command => \&wizard_installation_window);
  $prv->configure(-text => $text{'prev'}, -command => \&ask_path);
}

sub wizard_installation_window {
  for ($fmain->children) {
    $_->destroy;
  }
  $counter->configure(-text => "4/4");
  # create a progress bar window
  # compute the height of the Text by the given font

  $::progressw = $fmain->Scrolled("ROText", -scrollbars => "e",
                                -wrap => "word");

  # we want to have the progressbar about 20px wide, so compute how
  # many lines of the current font do fit into the remaining area
  my $lines = int( ($INNERHEIGHT - 20) / $lineskip);
  # it seems that on Windows the computation is not right, the
  # progress bar overwrites the last line of the text widget
  # remove one line here
  $lines-- if win32();
  $::progressw->configure(-height => $lines);
  $::progressw->place(-x => 0, -y => 0, -width => $INNERWIDTH); 

  # that is necessary otherwise the progressbar gets very strange dimensions
  $fmain->update;


  my $percent_done = 0;

  # compute the remaining space in pixel for the progressbar
  my $pw = $INNERHEIGHT - ($lines * $lineskip) - 5;
  # make sure that the line we remove above for Windows is not re-added
  # to the size of the progressbar. The 7 was found by trial and error
  $pw -= ($lineskip + 7) if win32();
  $::progress = $fmain->ProgressBar(-variable => \$percent_done,
      -width => $pw, -length => $INNERWIDTH,
      -from => 0, -to => 110, -blocks => 10,
      -colors => [ 0, '#0078b8' ]);
  $::progress->place(-x => 0, -y => $INNERHEIGHT, -anchor => "sw");

  #
  # change the buttons so that the Prev disappears, the Next becomes
  # Cancel, and the Cancel button disappears
  $prv->placeForget;
  $can->placeForget;
  $nxt->configure(-text => $text{'cancel'},
                  -command => sub { $return = $MENU_ABORT; $mw->destroy; });
  calc_depends();
  do_installation();
  $::progress->value(110);
  $return = $MENU_ALREADYDONE;
  my $t = $text{'finished'};
  if (!win32()) {
    $t .= "\n\n$text{'finishedpath'}";
  }
  $t .= "\n\n$text{'welcome'}";
  $t =~ s/TEXDIR/$::vars{'TEXDIR'}/g;
  $t =~ s/PLATFORM/$::vars{'this_platform'}/g;
  $t =~ s/\\n/\n/g;
  my $linechar = $::progressw->index("end");
  $::progressw->markSet("finaltext", $linechar);
  $::progressw->markGravity("finaltext", "left");
  $::progressw->insert("end", "\n$t");
  $::progressw->see("end");
  $::progressw->tagAdd("centered", $linechar, "end");
  $::progressw->tagConfigure("centered", -justify => "center");
  $nxt->configure(-text => $text{'finbut'},
                -command => sub { $mw->destroy; });
}

################### END OF MODULE RETURN 1 FOR REQUIRE ###########

1;

__END__

### Local Variables:
### perl-indent-level: 2
### tab-width: 2
### indent-tabs-mode: nil
### End:
# vim:set tabstop=2 expandtab: #

