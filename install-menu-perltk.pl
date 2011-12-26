#!/usr/bin/env perl
# $Id: install-menu-perltk.pl 12130 2009-02-10 08:59:31Z preining $
#
# Copyright 2008 Norbert Preining, Reinhard Kotucha
# This file is licensed under the GNU General Public License version 2
# or any later version.
#
# TODO:
# - make the fancy selector the default, at least on unix
# - for win32 find out the necessary files for the fancy selector and move
#   them to the installer perl package

use strict;
$^W = 1;

my $svnrev = '$Revision: 12130 $';
$svnrev =~ m/: ([0-9]+) /;
$::menurevision = $1;

require Tk;
require Tk::Dialog;
require Tk::DialogBox;
require Tk::PNG;
require Tk::ROText;
require Tk::ProgressBar;

if ($::alternative_selector) {
  require Tk::DirTree;
}

use utf8;
no utf8;

our %vars;
our $tlpdb;
our @collections_std;
our @collections_lang;
our @collections_lang_doc;
our $texlive_release;

our $MENU_INSTALL = 0;
our $MENU_ABORT   = 1;
our $MENU_QUIT    = 2;
our $MENU_ALREADYDONE = 3;

my $return = $MENU_INSTALL;

our %text;
our %labels;
require("installer/install-translations.pl");


my $mw;
my $subframe;
my $mainwindow;
my $bintextbutton;
my $schemebutton;
my $collectionstext;
my $texmflocaltext;
my $texmfsysvartext;
my $texmfsysconfigtext;
my $texmfhometext;
my $texdirtext;
my $langcoltext;
my $symlinktext;
my $optletterstate;
my $optfmtstate;
my $optsrcstate;
my $optdocstate;
my $letteryesno = ( $vars{'option_letter'} ? $text{'yes'} : $text{'no'} );
my $fmtyesno = ( $vars{'option_fmt'} ? $text{'yes'} : $text{'no'} );
my $srcyesno = ( $vars{'option_src'} ? $text{'yes'} : $text{'no'} );
my $docyesno = ( $vars{'option_doc'} ? $text{'yes'} : $text{'no'} );

$::run_menu = \&run_menu_perltk;


####################################################################
# From here on only function definitions
# ##################################################################


sub setup_hooks_perltk {
  @::info_hook = ();
  push @::info_hook,
    sub {
      return unless defined $mainwindow;
      update_status(join(" ",@_));
      $mainwindow->update;
    };
  @::warn_hook = ();
  push @::warn_hook,
    sub {
      return unless defined $mainwindow ;
      update_status(join(" ",@_));
      $mainwindow->update;
    };
  @::install_packages_hook = ();
  push @::install_packages_hook, \&update_progressbar;
  push @::install_packages_hook,
    sub { 
      return unless defined $mainwindow;
      return unless defined $::sww;
      $mainwindow->update; 
      $::sww->update; 
    };
}

sub update_status {
  my ($p) = @_;
  return unless defined $::progressw;
  $::progressw->insert("end", "$p");
  $::progressw->see("end");
}
sub update_progressbar {
  my ($n,$total) = @_;
  return unless defined $::progress;
  if (defined($n) && defined($total)) {
    $::progress->value(int($n*100/$total));
  }
}


sub run_menu_perltk {
  calc_depends();
  $mainwindow = Tk::MainWindow->new;
  setup_hooks_perltk();

  if (!win32()) {
    require Tk::Pane;
    $subframe = $mainwindow->Scrolled("Frame", -scrollbars => "oe");
  } else {
    $subframe = $mainwindow->Frame;
  }
  $mw = $subframe->Frame;

  # image frame on the left
  my $fl = $mw->Frame(-background => "#0078b8");
  my $img = $fl->Photo(-format => 'png', -file => "$::installerdir/tlpkg/installer/texlive2008.png");
  $fl->Label(-image => $img, -background => "#0078b8")->pack(-expand => 1, -fill => "y");
  $fl->Label(-text => "v$::installerrevision/$::menurevision", -background => "#0078b8")->pack;

  # data frame on the right
  my $fr = $mw->Frame;
  $fl->pack(-side => 'left', -expand => 1, -fill => "y");
  $fr->pack(-side => 'right');


  my $row = 1;
  $fr->Label(-text => $text{'title'})->grid(-row => $row, -column => 1, -columnspan => 3);

  $row++;
  $fr->Label(-text => "------- $text{'basicinfo'} -------")->grid(-row => $row, -column => 1, -columnspan => 3);


  # binary system line
  if (!win32()) {
    $row++;
    $fr->Label(-text => $labels{'binsys'}, -anchor => 'w')->grid(-row => $row, -column => 1, -sticky => 'w');
    $bintextbutton = $fr->Label(-anchor => 'w');
    $bintextbutton->grid(-row => $row, -column => 2, -padx => "2m");
    $fr->Button(-text => $text{'change'}, -command => sub { menu_select_binsystems(); })->grid(-row => $row, -column => 3, -sticky => "ew", -padx => "2m");
  }

  $row++;

  # scheme line
  $fr->Label(-text => $labels{'scheme'}, -anchor => 'w')->grid(-row => $row, -column => 1, -sticky => 'w');
  $schemebutton = $fr->Label(-anchor => 'w');
  $schemebutton->grid(-row => $row, -column => 2, -padx => "2m");
  $fr->Button(-text => $text{'change'}, -command => sub { menu_select_scheme(); })->grid(-row => $row, -column => 3, -sticky => "ew", -padx => "2m");

  $row++;

  # further customization
  $fr->Label(-text => "------- $text{'custom'} -------")->grid(-row => $row, -column => 1,-columnspan => 3);

  $row++;
  # standard collection line
  $fr->Label(-text => $labels{'stdcoll'}, -anchor => 'w')->grid(-row => $row, -column => 1, -sticky => 'w');
  $fr->Button(-text => $text{'change'}, -command => sub { menu_select_standard_collections(); })->grid(-row => $row, -column => 3, -sticky => "ew", -padx => "2m");

  $row++;
  # lang collection line
  $fr->Label(-text => $labels{'langcoll'}, -anchor => 'w')->grid(-row => $row, -column => 1, -sticky => 'w');
  $langcoltext = $fr->Label(-anchor => 'w')->grid(-row => $row, -column => 2, -padx => "2m");
  $fr->Button( -text => $text{'change'}, -command => sub { menu_select_lang_collections(); })->grid(-row => $row, -column => 3, -sticky => "ew", -padx => "2m");

  $row++;
  $collectionstext = $fr->Label();
  $collectionstext->grid(-row => $row, -column => 1, -columnspan => 3);

  $row++;
  # further customization
  $fr->Label(-text => "------- $text{'dirsetup'} -------")->grid(-row => $row, -column => 1, -columnspan => 3);

  $row++;
  # texdir line
  $fr->Label(-text => $labels{'texdir'}, -anchor => 'w')->grid(-row => $row, -column => 1, -sticky => 'w');
  $texdirtext = $fr->Label(-anchor => 'w')->grid(-row => $row, -column => 2, -padx => "2m");
  if ($::alternative_selector) {
    $fr->Button(-text => $text{'change'}, -command => sub { menu_edit_texdir("TEXDIR"); })->grid(-row => $row, -column => 3, -sticky => "ew", -padx => "2m");
  } else {
    $fr->Button(-text => $text{'change'}, -command => sub { menu_edit_vars_value("TEXDIR"); })->grid(-row => $row, -column => 3, -sticky => "ew", -padx => "2m");
  }


  $row++;
  # texmflocal line
  $fr->Label(-text => $labels{'localdir'}, -anchor => 'w')->grid(-row => $row, -column => 1, -sticky => 'w');
  $texmflocaltext = $fr->Label(-anchor => 'w')->grid(-row => $row, -column => 2, -padx => "2m");
  $fr->Button(-text => $text{'change'}, -command => sub { menu_edit_vars_value("TEXMFLOCAL"); })->grid(-row => $row, -column => 3, -sticky => "ew", -padx => "2m");

  $row++;
  # texmfsysvar line
  $fr->Label(-text => $labels{'sysvardir'}, -anchor => 'w')->grid(-row => $row, -column => 1, -sticky => 'w');
  $texmfsysvartext = $fr->Label(-anchor => 'w')->grid(-row => $row, -column => 2, -padx => "2m");
  $fr->Button(-text => $text{'change'}, -command => sub { menu_edit_vars_value("TEXMFSYSVAR"); })->grid(-row => $row, -column => 3, -sticky => "ew", -padx => "2m");

  $row++;
  # texmfsysvar line
  $fr->Label(-text => $labels{'sysconfigdir'}, -anchor => 'w')->grid(-row => $row, -column => 1, -sticky => 'w');
  $texmfsysconfigtext = $fr->Label(-anchor => 'w')->grid(-row => $row, -column => 2, -padx => "2m");
  $fr->Button(-text => $text{'change'}, -command => sub { menu_edit_vars_value("TEXMFSYSCONFIG"); })->grid(-row => $row, -column => 3, -sticky => "ew", -padx => "2m");

  $row++;
  # texmfhome line
  $fr->Label(-text => $labels{'texmfhome'}, -anchor => 'w')->grid(-row => $row, -column => 1, -sticky => 'w');
  $texmfhometext = $fr->Label(-anchor => 'w')->grid(-row => $row, -column => 2, -padx => "2m");
  $fr->Button(-text => $text{'change'}, -command => sub { menu_edit_vars_value("TEXMFHOME"); })->grid(-row => $row, -column => 3, -sticky => "ew", -padx => "2m");

  $row++;
  # Options
  $fr->Label(-text => "------- $text{'options'} -------")->grid(-row => $row, -column => 1, -columnspan => 3);

  $row++;
  # optpaper
  $fr->Label(-text => $labels{'optletter'}, -anchor => 'w')->grid(-row => $row, -column => 1, -sticky => 'w');
  $fr->Label(-anchor => 'w', -textvariable => \$letteryesno)->grid(-row => $row, -column => 2, -padx => "2m");
  $fr->Button(-text => $text{'toggle'}, -command => sub { toggle_and_set_opt_variable(\$vars{'option_letter'}, \$letteryesno); })->grid(-row => $row, -column => 3, -sticky => "ew", -padx => "2m");

  $row++;
  $fr->Label(-text => $labels{'optfmt'}, -anchor => 'w')->grid(-row => $row, -column => 1, -sticky => 'w');
  $fr->Label(-anchor => 'w', -textvariable => \$fmtyesno)->grid(-row => $row, -column => 2, -padx => "2m");
  $fr->Button(-text => $text{'toggle'}, -command => sub { toggle_and_set_opt_variable(\$vars{'option_fmt'}, \$fmtyesno); })->grid(-row => $row, -column => 3, -sticky => "ew", -padx => "2m");

  if ($vars{'doc_splitting_supported'}) {
    $row++;
    $fr->Label(-text => $labels{'optdoc'}, -anchor => 'w')->grid(-row => $row, -column => 1, -sticky => 'w');
    $fr->Label(-anchor => 'w', -textvariable => \$docyesno)->grid(-row => $row, -column => 2, -padx => "2m");
    $fr->Button(-text => $text{'toggle'}, -command => sub { toggle_and_set_opt_variable(\$vars{'option_doc'}, \$docyesno); })->grid(-row => $row, -column => 3, -sticky => "ew", -padx => "2m");
  }

  if ($vars{'src_splitting_supported'}) {
    $row++;
    $fr->Label(-text => $labels{'optsrc'}, -anchor => 'w')->grid(-row => $row, -column => 1, -sticky => 'w');
    $fr->Label(-anchor => 'w', -textvariable => \$srcyesno)->grid(-row => $row, -column => 2, -padx => "2m");
    $fr->Button(-text => $text{'toggle'}, -command => sub { toggle_and_set_opt_variable(\$vars{'option_src'}, \$srcyesno); })->grid(-row => $row, -column => 3, -sticky => "ew", -padx => "2m");
  }

  if (unix()) {
    $row++;
    # Symlinks
    $fr->Label(-text => "------- $text{'sysint'} -------")->grid(-row => $row, -column => 1,-columnspan => 3);

    $row++;
    $fr->Label(-text => $labels{'symlink'}, -anchor => 'w')->grid(-row => $row, -column => 1, -sticky => 'w');
    $symlinktext = $fr->Label(-anchor => 'w')->grid(-row => $row, -column => 2, -padx => "2m");
    $fr->Button(-text => $text{'change'}, -command => sub { menu_select_symlink(); })->grid(-row => $row, -column => 3, -sticky => "ew", -padx => "2m");
  }

  # install/cancel buttons
  my $f3 = $fr->Frame;
  $f3->Button(
    -text    => $text{'install'},
    -command => sub { installation_window(); }
  )->pack(-side => 'left', -padx => "2m", -pady => "2m");
  my $quitbutton = $f3->Button(
    -text    => $text{'quit'},
    -command => sub { $return = $MENU_ABORT; $mainwindow->destroy }
  )->pack(-side => 'right', -padx => "2m", -pady => "2m");
  $mw->bind('<Escape>', [ $quitbutton, 'Invoke' ]);
  #my $wizardbutton = $f3->Button(
  #  -text    => $text{'wizard'},
  #  -command => sub {
  #     $mainwindow->destroy; 
  #     require("installer/install-menu-wizard.pl");
  #     setup_hooks_wizard();
  #     $return = run_menu_wizard();
  #   })->pack(-side => 'right', -padx => "2m", -pady => "2m");
  $row++;
  $f3->grid(-row => $row, -column => 1, -columnspan => 3);
  menu_update_texts();
  $fr->gridColumnconfigure(2, -minsize => 300);
  $mw->pack(-expand => 1, -fill => "both");
  $mw->update;
  my $rh = $mw->reqheight;
  my $rw = $mw->reqwidth;
  my $maxheight = $mainwindow->screenheight() - 20;
  debug("Requested height: $rh, requested width: $rw, max height: $maxheight\n");
  if ($rh > $maxheight) {
    $rh = $maxheight;
    $rw += 20; # for the scrollbar =  =
  }
  $subframe->configure(-height => $rh, -width=>$rw);
  $subframe->pack(-expand => 1, -fill => "both");
  $mainwindow->configure(-height => $rh, -width=>$rw);
  #$mw->pack(-expand => 1, -fill => "both");
  Tk::MainLoop();
  return($return);
}

sub installation_window {
  # create a progress bar window
  $::sww = $mainwindow->Toplevel(-title => "installation process",
                                 -width => 400);
  $::sww->transient($mainwindow);
  $::sww->grab();
  $::sww->Label(-text => "Installation process")->pack;
  $::progressw = $::sww->Scrolled("ROText", -scrollbars => "e", -height => 16);
  $::progressw->pack(-expand => 1, -fill => "both");
  my $percent_done = 0;
  $::progress = $::sww->ProgressBar(-variable => \$percent_done,
    -width => 20, -length => 400, -from => 0, -to => 100, -blocks => 10,
    -colors => [ 0, '#0078b8' ]);
  $::progress->pack(-fill => "x");
  my $f = $::sww->Frame;
  my $b = $f->Button(-text => $text{'cancel'},
                     -command => sub { $::sww->destroy; $mainwindow->destroy;
                                       do_cleanup(); exit(1); }
                    )->pack(-pady => "2m");
  $f->pack;
  do_installation();
  $return = $MENU_ALREADYDONE;
  my $t = $text{'finished'};
  if (!win32()) {
    $t .= "\n\n$text{'finishedpath'}";
  }
  $t .= "\n\n$text{'welcome'}";
  $t =~ s/TEXDIR/$::vars{'TEXDIR'}/g;
  $t =~ s/PLATFORM/$::vars{'this_platform'}/g;
  $t =~ s/\\n/\n/g;
  $::progressw->insert("end", "\n");
  my $linechar = $::progressw->index("end");
  $::progressw->markSet("finaltext", $linechar);
  $::progressw->markGravity("finaltext", "left");
  $::progressw->insert("end", "\n$t");
  $::progressw->see("end");
  $::progressw->tagAdd("centered", $linechar, "end");
  $::progressw->tagConfigure("centered", -justify => "center");
  $b->configure(-text => $text{'finbut'},
                -command => sub { $mainwindow->destroy; });
}

sub menu_edit_texdir {
  my $key = shift;
  our $addyear = 1;
  our $addtexlive = 1;
  my $val = $vars{$key};
  our $currsel;
  our $entry;
  sub update_label {
    my $t = $currsel;
    $t .= "/texlive" if ($addtexlive);
    $t .= "/$texlive_release" if ($addyear);
    $entry->configure(-text => "$t");
  }
  my $hint_var;
  if ($key ne 'TEXMFHOME') {
    $hint_var = win32() ? $ENV{'USERPROFILE'} : $ENV{'HOME'};
  } else {
    $hint_var = win32() ? '%USERPROFILE%' : '$HOME';
  }
  my $hint_text =  $text{'hinthome'};
  $hint_text =~ s/%%%/$hint_var/;
  if ($val =~ m!^(.*)/texlive/$texlive_release$!) {
    $currsel = "$1";
    $addyear = 1;
    $addtexlive = 1;
  } elsif ($val =~ m!^(.*)/$texlive_release$!) {
    $currsel = "$1";
    $addyear = 1;
    $addtexlive = 0;
  } elsif ($val =~ m!^(.*)/texlive$!) {
    $currsel = "$1";
    $addyear = 0;
    $addtexlive = 1;
  } else {
    $addyear = 0;
    $addtexlive = 0;
    $currsel = $val;
  }
  my $sw = $mainwindow->Toplevel(-title => $text{'changevar'});
  $sw->transient($mainwindow);
  $sw->grab();
  $sw->Label(-text =>  $text{'enterpath'} . " $key: " . $hint_text)->pack(-padx => "2m", -pady => "2m");
  $entry = $sw->Entry(-width => 60)->pack(-padx => "2m", -pady => "2m");
  my $f = $sw->Frame;
  my $c1 = $f->Checkbutton(-text => 'Add "texlive"', -variable => \$addtexlive,
    -command => \&update_label);
  my $c2 = $f->Checkbutton(-text => "Add \"$texlive_release\"", -variable => \$addyear,
    -command => \&update_label);
  my $foo = $sw->Scrolled("DirTree", -scrollbars => "osoe",
                          -browsecmd => sub { my ($d) = @_; $currsel = $d; update_label(); },
                          -directory => "$currsel");
  my $ff = $sw->Frame;
  my $ok = $ff->Button(-text => $text{'ok'}, -command => sub { $val = $entry->get; callback_edit_directories($key,$val); $sw->destroy; });
  my $cancel = $ff->Button(-text => $text{'cancel'}, -command => sub { $sw->destroy; });
  update_label();
  $c1->pack(-side => 'left', -padx => "2m", -pady => "2m");
  $c2->pack(-side => 'right', -padx => "2m", -pady => "2m");
  $f->pack;
  $foo->pack(-fill => "both", -expand => 1);
  $ok->pack(-side => 'left' , -padx => "2m", -pady => "2m");
  $cancel->pack(-side => 'right', -padx => "2m", -pady => "2m");
  $ff->pack;
  # bindings
  $sw->bind('<Return>' => [ $ok, 'Invoke']);
  $sw->bind('<Escape>' => [ $cancel, 'Invoke']);
}

sub menu_edit_vars_value {
  my $key = shift;
  my $sw = $mainwindow->Toplevel(-title => $text{'changevar'});
  $sw->transient($mainwindow);
  $sw->grab();
  my $val = $vars{$key};
  my $hint_var;
  if ($key ne 'TEXMFHOME') {
    $hint_var = win32() ? $ENV{'USERPROFILE'} : $ENV{'HOME'};
  } else {
    $hint_var = win32() ? '%USERPROFILE%' : '$HOME';
  }
  my $hint_text =  $text{'hinthome'};
  $hint_text =~ s/%%%/$hint_var/;
  $sw->Label(-text => $text{'enterpath'} . " $key: " . $hint_text)->pack(-padx => "2m", -pady => "2m");
  my $entry = $sw->Entry(-textvariable => $val, -width => 60);
  $entry->pack(-padx => "2m", -pady => "2m");
  my $f = $sw->Frame;
  my $okbutton = $f->Button(-text => $text{'ok'},
     -command => sub { $val = $entry->get; callback_edit_directories($key,$val) ; $sw->destroy })->pack(-side => 'left', -padx => "2m", -pady => "2m");
  my $cancelbutton = $f->Button(-text => $text{'cancel'},
     -command => sub { $sw->destroy })->pack(-side => 'right', -padx => "2m", -pady => "2m");
  $f->pack(-expand => 'x');
  # bindings
  $sw->bind('<Return>' => [ $okbutton, 'Invoke']);
  $sw->bind('<Escape>' => [ $cancelbutton, 'Invoke']);
}


sub menu_select_scheme {
  my $sw = $mainwindow->Toplevel(-title => $labels{'scheme'});
  $sw->transient($mainwindow);
  $sw->grab();
  my @schemes;
  foreach my $pkg ($tlpdb->list_packages) {
    my $tlpobj = $tlpdb->{'tlps'}{$pkg};
    if ($tlpobj->category eq "Scheme") {
      push @schemes, $pkg;
      $vars{"$pkg"}=($vars{'selected_scheme'} eq $pkg)? 1:0;
    }
  }
  @schemes=sort @schemes;
  my $selected = $vars{'selected_scheme'};
  $sw->Label(-text => $text{'selectscheme'})->pack(-padx => "2m", -pady => "2m");
  my $f2 = $sw->Frame;
  my $f2l = $f2->Frame;
  my $f2r = $f2->Frame;
  my $nrfh = $#schemes / 2;
  my $i = 0;
  foreach my $scheme (@schemes) {
    my $tlpobj = $tlpdb->get_package("$scheme");
    if ($i < $nrfh) {
      $f2l->Radiobutton(-variable => \$selected, -value => $scheme,
        -text => $tlpobj->shortdesc)->pack(-anchor => 'w');
    } else {
      $f2r->Radiobutton(-variable => \$selected, -value => $scheme,
        -text => $tlpobj->shortdesc)->pack(-anchor => 'w');
    }
    $i++;
  }
  $f2l->pack(-side => 'left', -padx => "2m", -pady => "2m");
  $f2r->pack(-side => 'right', -padx => "2m", -pady => "2m");
  $f2->pack;
  my $f3 = $sw->Frame;
  my $okbutton = $f3->Button(-text => $text{'ok'},
     -command => sub { callback_select_scheme($selected) ; $sw->destroy })->pack(-side => 'left', -padx => "2m", -pady => "2m");
  my $cancelbutton = $f3->Button(-text => $text{'cancel'},
     -command => sub { $sw->destroy })->pack(-side => 'left', -padx => "2m", -pady => "2m");
  $f3->pack(-expand => 'x');
  $sw->bind('<Return>' => [ $okbutton, 'Invoke']);
  $sw->bind('<Escape>' => [ $cancelbutton, 'Invoke']);
}

sub menu_select_standard_collections {
  my $sw = $mainwindow->Toplevel(-title => $labels{'stdcoll'});
  $sw->transient($mainwindow);
  $sw->grab();
  my $fb = $sw->Frame;
  my $fc = $sw->Frame;
  my $fd = $sw->Frame;
  my $f1 = $fb->Frame;
  my $f2 = $fb->Frame;
  my %lvars = %vars;
  $sw->Label(-text => $text{'selectstdcol'})->pack(-padx => "2m", -pady => "2m");
  my $halfcol = $#collections_std / 2;
  my $i = 0;
  foreach my $coll (sort @collections_std) {
    my $tlpobj = $tlpdb->get_package("$coll");
    if ($i < $halfcol) {
      $f1->Checkbutton(-variable => \$lvars{$coll}, -text => $tlpobj->shortdesc)->pack(-anchor => 'w');
    } else {
      $f2->Checkbutton(-variable => \$lvars{$coll}, -text => $tlpobj->shortdesc)->pack(-anchor => 'w');
    }
    $i++;
  }
  $f1->pack(-side => 'left', -padx => "2m", -pady => "2m");
  $f2->pack(-side => 'right', -padx => "2m", -pady => "2m");
  $fb->pack(-padx => "2m", -pady => "2m");
  $fd->pack();
  $fd->Button(-text => $text{'selectall'},
    -command => sub { select_collections(\%lvars, @collections_std) })->pack(-side => 'left', -padx => "2m", -pady => "2m");
  $fd->Button(-text => $text{'selectnone'},
    -command => sub { deselect_collections(\%lvars, @collections_std) })->pack(-side => 'right', -padx => "2m", -pady => "2m");
  $fc->pack(-expand => 'x', -padx => "2m", -pady => "2m");
  my $okbutton = $fc->Button(-text => $text{'ok'},
     -command => sub { %vars = %lvars; callback_select_collection() ; $sw->destroy })->pack(-side => 'left', -padx => "2m", -pady => "2m");
  my $cancelbutton = $fc->Button(-text => $text{'cancel'},
     -command => sub { $sw->destroy })->pack(-side => 'right', -padx => "2m", -pady => "2m");
  $sw->bind('<Return>' => [ $okbutton, 'Invoke']);
  $sw->bind('<Escape>' => [ $cancelbutton, 'Invoke']);
}

sub menu_select_lang_collections {
  my $sw = $mainwindow->Toplevel(-title => $labels{'langcoll'});
  $sw->transient($mainwindow);
  $sw->grab();
  my $f1 = $sw->Frame;
  $f1->pack;
  my $f1lang = $f1->Labelframe(-text => $text{'selectlang'});
  my $f1doc  = $f1->Labelframe(-text => $text{'selectdoc'});
  $f1lang->pack(-side => 'left', -padx => "2m", -pady => "2m", -ipadx => "2m", -ipady => "2m");
  $f1doc->pack(-side => 'right', -padx => "2m", -pady => "2m", -ipadx => "2m", -ipady => "2m", -expand => 1, -fill => "y");
  my $f1langT = $f1lang->Frame; $f1langT->pack;
  my $f1langL = $f1lang->Frame; $f1langL->pack;
  my $f1langLa = $f1langL->Frame; $f1langLa->pack(-side => 'left');
  my $f1langLb = $f1langL->Frame; $f1langLb->pack(-side => 'right');
  my $langh = $#collections_lang / 2;
  my $i = 0;
  my %lvars = %vars;
  foreach my $coll (sort @collections_lang) {
    my $tlpobj = $tlpdb->get_package("$coll");
    if ($i < $langh) {
      $f1langLa->Checkbutton(-variable => \$lvars{$coll}, -text => $tlpobj->shortdesc)->pack(-anchor => 'w');
    } else {
      $f1langLb->Checkbutton(-variable => \$lvars{$coll}, -text => $tlpobj->shortdesc)->pack(-anchor => 'w');
    }
    $i++;
  }
  my $f1langB = $f1lang->Frame;
  $f1langB->pack(-expand => 'x');
  $f1langB->Button(-text => $text{'selectall'},
    -command => sub { select_collections(\%lvars, @collections_lang) })->pack(-side => 'left', -padx => "2m", -pady => "2m");
  $f1langB->Button(-text => $text{'selectnone'},
    -command => sub { deselect_collections(\%lvars, @collections_lang) })->pack(-side => 'right', -padx => "2m", -pady => "2m");

  my $f1docT = $f1doc->Frame; $f1docT->pack;
  my $f1docL = $f1doc->Frame; $f1docL->pack;
  my $f1docLa = $f1docL->Frame; $f1docLa->pack(-side => 'left');
  my $f1docLb = $f1docL->Frame; $f1docLb->pack(-side => 'right');
  my $doch = $#collections_lang_doc / 2;
  $i = 0;
  foreach my $coll (sort @collections_lang_doc) {
    my $tlpobj = $tlpdb->get_package("$coll");
    if ($i < $doch) {
      $f1docLa->Checkbutton(-variable => \$lvars{$coll}, -text => $tlpobj->shortdesc)->pack(-anchor => 'w');
    } else {
      $f1docLb->Checkbutton(-variable => \$lvars{$coll}, -text => $tlpobj->shortdesc)->pack(-anchor => 'w');
    }
    $i++;
  }
  my $f1docB = $f1doc->Frame;
  $f1docB->pack(-expand => 1, -side => "bottom");
  $f1docB->Button(-text => $text{'selectall'},
    -command => sub { select_collections(\%lvars, @collections_lang_doc) })->pack(-side => 'left', -padx => "2m", -pady => "2m");
  $f1docB->Button(-text => $text{'selectnone'},
    -command => sub { deselect_collections(\%lvars, @collections_lang_doc) })->pack(-side => 'left', -padx => "2m", -pady => "2m");
  my $f2 = $sw->Frame; $f2->pack(-expand => 'x');
  my $okbutton = $f2->Button(-text => $text{'ok'},
     -command => sub { %vars = %lvars; callback_select_collection() ; $sw->destroy })->pack(-side => 'left', -padx => "2m", -pady => "2m");
  my $cancelbutton = $f2->Button(-text => $text{'cancel'},
     -command => sub { $sw->destroy })->pack(-side => 'right', -padx => "2m", -pady => "2m");
  $sw->bind('<Return>' => [ $okbutton, 'Invoke']);
  $sw->bind('<Escape>' => [ $cancelbutton, 'Invoke']);
}

sub menu_select_symlink {
  our ($lbin,$lman,$linfo);
  our $osym = $vars{'option_symlinks'};
  our ($binlab,$binb,$manlab,$manb,$infolab,$infob);
  sub set_unset_buttons {
    $lbin = ($osym ? $vars{'sys_bin'} : '');
    $linfo = ($osym ? $vars{'sys_info'} : '');
    $lman = ($osym ? $vars{'sys_man'} : '');
    if ($osym) {
      $binlab->grid(-row => 2, -column => 1, -sticky => "w", -padx => "2m");
      $binb->grid(-row => 2, -column => 2, -sticky => "ew", -padx => "2m");
      $manlab->grid(-row => 3, -column => 1, -sticky => "w", -padx => "2m");
      $manb->grid(-row => 3, -column => 2, -sticky => "ew", -padx => "2m");
      $infolab->grid(-row => 4, -column => 1, -sticky => "w", -padx => "2m");
      $infob->grid(-row => 4, -column => 2, -sticky => "ew", -padx => "2m");
    } else {
      $infob->gridForget;
      $infolab->gridForget;
      $manb->gridForget;
      $manlab->gridForget;
      $binb->gridForget;
      $binlab->gridForget;
    }
  }
  sub return_callback {
    if ($osym) {
      my $home = getenv('HOME');
      $home = getenv('USERPROFILE') if (win32());
      $home ||= '~';
      $lbin =~ s/^~/$home/;
      $linfo =~ s/^~/$home/;
      $lman =~ s/^~/$home/;
      $vars{'sys_bin'} = $lbin;
      $vars{'sys_info'} = $linfo;
      $vars{'sys_man'} = $lman;
    }
    $vars{'option_symlinks'} = $osym;
    menu_update_texts();
  }
  my $sw = $mainwindow->Toplevel(-title => $labels{'symlink'});
  $sw->transient($mainwindow);
  $sw->grab();
  $sw->Checkbutton(-variable => \$osym,
                   -text => $text{'createsym'},
		   -command => sub { set_unset_buttons(); } )->grid(-column => 1,
                                                        -row => 1,
                                                        -columnspan => 2,
                                                        -padx => "2m");
  $binlab = $sw->Label(-text => $text{'binto'});
  $binb = $sw->Entry(-textvariable => \$lbin);
  $manlab = $sw->Label(-text => $text{'manto'});
  $manb = $sw->Entry(-textvariable => \$lman);
  $infolab = $sw->Label(-text => $text{'infoto'});
  $infob = $sw->Entry(-textvariable => \$linfo);
  set_unset_buttons();
  my $f2 = $sw->Frame; $f2->grid(-column => 1, -columnspan => 2, -row => 5);
  my $okbutton = $f2->Button(-text => $text{'ok'},
     -command => sub { return_callback(); $sw->destroy })->pack(-side => 'left');
  my $cancelbutton = $f2->Button(-text => $text{'cancel'},
     -command => sub { $sw->destroy })->pack(-side => 'right');
  $sw->bind('<Return>' => [ $okbutton, 'Invoke']);
  $sw->bind('<Escape>' => [ $cancelbutton, 'Invoke']);
}

sub menu_select_binsystems {
  my $f2r;
  my $f2;
  my $sw = $mainwindow->Toplevel(-title => $labels{'binsys'});
  $sw->transient($mainwindow);
  $sw->grab();
  my @diskarchs = ();
  foreach my $key (keys %vars) {
    if ($key=~/binary_(.*)/) {
      push @diskarchs, $1;
    }
  }
  $sw->Label(-text => $text{'selectsys'})->pack(-padx => "2m", -pady => "2m");
  $f2 = $sw->Frame;
  my $f2l = $f2->Frame;
  foreach my $sys (sort @diskarchs) {
    $f2l->Checkbutton(-variable => \$vars{"binary_$sys"}, -text => platform_desc($sys))->pack(-anchor => 'w');
  }
  $f2l->pack(-side => 'left');
  $f2->pack(-padx => "2m", -pady => "2m");
  my $f3 = $sw->Frame;
  my $okbutton = $f3->Button(-text => $text{'ok'},
     -command => sub { callback_select_systems() ; $sw->destroy })->pack(-side => 'left', -padx => "2m", -pady => "2m");
  my $cancelbutton = $f3->Button(-text => $text{'cancel'},
     -command => sub { $sw->destroy })->pack(-side => 'right', -padx => "2m", -pady => "2m");
  $f3->pack(-expand => 'x');
  $sw->bind('<Return>' => [ $okbutton, 'Invoke']);
  $sw->bind('<Escape>' => [ $cancelbutton, 'Invoke']);
}


sub menu_set_text {
  my $w = shift;
  my $t = shift;
  $w->configure(-text => $t, @_);
}

sub menu_set_symlink_text {
  menu_set_text($symlinktext, (($vars{'option_symlinks'})?$text{'yes'}:$text{'no'}));
}

sub menu_set_schemebutton_text {
  menu_set_text($schemebutton, "$vars{'selected_scheme'}");
}

sub menu_set_binbutton_text {
  if (!win32()) {
    menu_set_text($bintextbutton, "$vars{'n_systems_selected'} $text{'outof'} $vars{'n_systems_available'}");
  }
}

sub menu_set_collections_text {
  menu_set_text($collectionstext, "$vars{'n_collections_selected'} $text{'collof'} $vars{'n_collections_available'}, $text{'diskreq'}: $vars{'total_size'} MB");
}

sub menu_set_pathes_text {
  if (check_on_lang_collection_installed()) {
    # good
    menu_set_text($langcoltext, "");
  } else {
    # bad
    menu_set_text($langcoltext, $text{'nolangcol'}, -foreground => "red");
  }
  if (TeXLive::TLUtils::texdir_check($vars{'TEXDIR'})) {
    menu_set_text($texdirtext, "$vars{'TEXDIR'}", -foreground => "black");
  } else {
    menu_set_text($texdirtext, $text{'notwritable'}, -foreground => "red");
  }
  menu_set_text($texmflocaltext, "$vars{'TEXMFLOCAL'}");
  if ((-w $vars{'TEXMFSYSVAR'}) || (-w dirname($vars{'TEXMFSYSVAR'}))) {
    menu_set_text($texmfsysvartext, "$vars{'TEXMFSYSVAR'}", -foreground => "black");
  } elsif ("$vars{'TEXMFSYSVAR'}" =~ m;^$vars{'TEXDIR'};) {
    if (TeXLive::TLUtils::texdir_check($vars{'TEXDIR'})) {
      menu_set_text($texmfsysvartext, "$vars{'TEXMFSYSVAR'}", -foreground => "black");
    } else {
      menu_set_text($texmfsysvartext, $text{'changetexdir'}, -foreground => "red");
    }
  } else {
    menu_set_text($texmfsysvartext, $text{'notwritable'});
  }
  if ((-w $vars{'TEXMFSYSCONFIG'}) || (-w dirname($vars{'TEXMFSYSCONFIG'}))) {
    menu_set_text($texmfsysconfigtext, "$vars{'TEXMFSYSCONFIG'}", -foreground => "black");
  } elsif ("$vars{'TEXMFSYSCONFIG'}" =~ m;^$vars{'TEXDIR'};) {
    if (TeXLive::TLUtils::texdir_check($vars{'TEXDIR'})) {
      menu_set_text($texmfsysconfigtext, "$vars{'TEXMFSYSCONFIG'}", -foreground => "black");
    } else {
      menu_set_text($texmfsysconfigtext, $text{'changetexdir'}, -foreground => "red");
    }
  } else {
    menu_set_text($texmfsysconfigtext, $text{'notwritable'});
  }
  menu_set_text($texmfhometext, "$vars{'TEXMFHOME'}");
}


sub menu_update_texts {
  menu_set_pathes_text;
  menu_set_collections_text;
  menu_set_binbutton_text;
  menu_set_schemebutton_text;
  $optletterstate = ($vars{'option_letter'} ? $text{'yes'} : $text{'no'});
  $optfmtstate = ($vars{'option_fmt'} ? $text{'yes'} : $text{'no'});
  $optsrcstate = ($vars{'option_src'} ? $text{'yes'} : $text{'no'});
  $optdocstate = ($vars{'option_doc'} ? $text{'yes'} : $text{'no'});
  if (unix()) { menu_set_symlink_text; };
}

sub callback_select_scheme {
  my $s = shift;
  select_scheme($s);
  menu_update_texts();
}

sub callback_select_collection {
  calc_depends();
  update_numbers();
  menu_update_texts();
}

sub callback_select_systems() {
  if ($vars{"binary_win32"}) {
    $vars{"collection-wintools"} = 1;
  } else {
    $vars{"collection-wintools"} = 0;
  }
  calc_depends();
  update_numbers();
  menu_update_texts();
}

sub callback_edit_directories {
  my ($key,$val) = @_;
  my $home = getenv('HOME');
  if (win32()) {
    $home = getenv('USERPROFILE');
    $home =~ s!\\!/!g;
  }
  $home ||= '~';
  $val =~ s!\\!/!g;
  $vars{$key} = $val;
  $vars{'TEXDIR'} =~ s/^~/$home/;
  $vars{'TEXMFLOCAL'} =~ s/^~/$home/;
  $vars{'TEXMFSYSVAR'} =~ s/^~/$home/;
  $vars{'TEXMFSYSCONFIG'} =~ s/^~/$home/;
  # only if we set TEXDIR we set the others in parallel
  if ($key eq "TEXDIR") {
    if ($vars{'TEXDIR'}=~/^(.*)\/$texlive_release$/) {
      $vars{'TEXMFLOCAL'}="$1/texmf-local";
      $vars{'TEXMFSYSVAR'}="$1/$texlive_release/texmf-var";
      $vars{'TEXMFSYSCONFIG'}="$1/$texlive_release/texmf-config";
    } elsif ($vars{'TEXDIR'}=~/^(.*)$/) {
      $vars{'TEXMFLOCAL'}="$1/texmf-local";
      $vars{'TEXMFSYSVAR'}="$1/texmf-var";
      $vars{'TEXMFSYSCONFIG'}="$1/texmf-config";
    }
    $vars{'TEXDIRW'}=$vars{'TEXDIR'}
  }
  menu_update_texts();
}

sub  callback_edit_var() {
  my ($key,$val) = @_;
  $vars{$key} = $val;
  menu_update_texts();
}

sub dump_vars_stdout {
  foreach my $k (keys %vars) {
    print "DEBUG: vars{$k} = $vars{$k}\n";
  }
}

sub toggle_and_set_opt_variable {
  my ($varsref, $toggleref) = @_;
  $$toggleref = ($$toggleref eq $text{'yes'}) ? $text{'no'} : $text{'yes'};
  $$varsref = 0;
  $$varsref = 1 if ($$toggleref eq $text{'yes'});
  calc_depends();
  menu_update_texts();
}

1;

__END__

### Local Variables:
### perl-indent-level: 2
### tab-width: 2
### indent-tabs-mode: nil
### End:
# vim:set tabstop=2 expandtab: #

