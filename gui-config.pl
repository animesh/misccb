# gui-config2.pl
# $Id: gui-config.pl 11983 2009-01-26 19:32:44Z preining $
#
# Copyright 2008 Tomasz Luczak, Norbert Preining
#
# GUI for tlmgr
#

our $back_config = $back->add("config",-label => ___"config");
$screens{"config"} = $back_config;

my $default_location = $localtlpdb->option_location;
my $default_formats = ($localtlpdb->option_create_formats ? ___"yes" : ___"no");
my $default_docfiles = ($localtlpdb->option_install_docfiles ? ___"yes" : ___"no");
my $default_srcfiles = ($localtlpdb->option_install_srcfiles ? ___"yes" : ___"no");
my $default_backupdir = $localtlpdb->option("backupdir");
my $default_autobackup = $localtlpdb->option("autobackup");

# this will be loaded only on unix systems!
#

my $back_config_set = $back_config->Labelframe(-text => ___"defaultsettings");
$back_config_set->pack(-fill => "both", -padx => "2m", -pady => "2m");

my $back_config_set_l1 = $back_config_set->Label(-text => ___"defaultsource", -anchor => "w");
my $back_config_set_m1 = $back_config_set->Label(-textvariable => \$default_location);
my $back_config_set_r1 = $back_config_set->Button(-text => ___"change",
     -command => sub { menu_default_location(); });

my $back_config_set_l2 = $back_config_set->Label(-text => ___"createformats", -anchor => "w");
my $back_config_set_m2 = $back_config_set->Label(-textvariable => \$default_formats);
my $back_config_set_r2 = $back_config_set->Button(-text => ___"toggle",
     -command => sub { toggle_setting("formats"); });

my $back_config_set_l3 = $back_config_set->Label(-text => ___"installdoc", -anchor => "w");
my $back_config_set_m3 = $back_config_set->Label(-textvariable => \$default_docfiles);
my $back_config_set_r3 = $back_config_set->Button(-text => ___"toggle",
     -command => sub { toggle_setting("docfiles"); });

my $back_config_set_l4 = $back_config_set->Label(-text => ___"installsrc", -anchor => "w");
my $back_config_set_m4 = $back_config_set->Label(-textvariable => \$default_srcfiles);
my $back_config_set_r4 = $back_config_set->Button(-text => ___"toggle",
     -command => sub { toggle_setting("srcfiles"); });

my $back_config_set_l5 = $back_config_set->Label(-text => ___"backupdir", -anchor => "w");
my $back_config_set_m5 = $back_config_set->Label(-textvariable => \$default_backupdir);
my $back_config_set_r5 = $back_config_set->Button(-text => ___"change",
     -command => sub { 
        my $dir = $back_config->chooseDirectory();
        if (defined($dir) && ($default_backupdir ne $dir)) {
          # see warning concerning UTF8 or other encoded dir names!!
          $localtlpdb->option("backupdir", $dir);
          $default_backupdir = $dir;
          $localtlpdb->save;
        }
      });

my $back_config_set_l6 = $back_config_set->Label(-text => ___"autobackup", -anchor => "w");
my $back_config_set_m6 = $back_config_set->Label(-textvariable => \$default_autobackup);
my $back_config_set_r6 = $back_config_set->Button(-text => ___"change",
            -command => sub { select_autobackup($p); }, -anchor => "w");

$back_config_set_l1->grid( $back_config_set_m1, $back_config_set_r1,
                           -padx => "2m", -pady => "2m", -sticky => "nwe");
$back_config_set_l2->grid( $back_config_set_m2, $back_config_set_r2,
                           -padx => "2m", -pady => "2m", -sticky => "nwe");
$back_config_set_l3->grid( $back_config_set_m3, $back_config_set_r3,
                           -padx => "2m", -pady => "2m", -sticky => "nwe");
$back_config_set_l4->grid( $back_config_set_m4, $back_config_set_r4,
                           -padx => "2m", -pady => "2m", -sticky => "nwe");
$back_config_set_l5->grid( $back_config_set_m5, $back_config_set_r5,
                           -padx => "2m", -pady => "2m", -sticky => "nwe");
$back_config_set_l6->grid( $back_config_set_m6, $back_config_set_r6,
                           -padx => "2m", -pady => "2m", -sticky => "nwe");

#############

my $lower = $back_config->Frame;
$lower->pack(-fill => "both");

my $back_config_pap = $lower->Labelframe(-text => ___"papersettings");


my $back_config_pap_l1 = $back_config_pap->Label(-text => ___"defaultpaperall", -anchor => "w");
my $back_config_pap_m1 = $back_config_pap->Button(-text => "A4",
      -command => sub { change_paper("all", "a4"); });
my $back_config_pap_r1 = $back_config_pap->Button(-text => "letter",
      -command => sub { change_paper("all", "letter"); });

$back_config_pap_l1->grid( $back_config_pap_m1, $back_config_pap_r1,
           -padx => "2m", -pady => "2m", -sticky => "nswe");

my %papers;
my %defaultpaper;

sub init_paper_xdvi {
  if (!win32()) {
    @{$papers{"xdvi"}} = TeXLive::TLPaper::get_paper_list("xdvi");
    $defaultpaper{"xdvi"} = ${$papers{"xdvi"}}[0];
  }
}
sub init_paper_pdftex {
  @{$papers{"pdftex"}} = TeXLive::TLPaper::get_paper_list("pdftex");
  $defaultpaper{"pdftex"} = ${$papers{"pdftex"}}[0];
}
sub init_paper_dvips {
  @{$papers{"dvips"}} = TeXLive::TLPaper::get_paper_list("dvips");
  $defaultpaper{"dvips"} = ${$papers{"dvips"}}[0];
}
sub init_paper_dvipdfm {
  @{$papers{"dvipdfm"}} = TeXLive::TLPaper::get_paper_list("dvipdfm");
  $defaultpaper{"dvipdfm"} = ${$papers{"dvipdfm"}}[0];
}
sub init_paper_context {
  if (defined($localtlpdb->get_package("bin-context"))) {
    @{$papers{"context"}} = TeXLive::TLPaper::get_paper_list("context");
    $defaultpaper{"context"} = ${$papers{"context"}}[0];
  }
}
sub init_paper_dvipdfmx {
  @{$papers{"dvipdfmx"}} = TeXLive::TLPaper::get_paper_list("dvipdfmx");
  $defaultpaper{"dvipdfmx"} = ${$papers{"dvipdfmx"}}[0];
}

my %init_paper_subs;
$init_paper_subs{"xdvi"} = \&init_paper_xdvi;
$init_paper_subs{"pdftex"} = \&init_paper_pdftex;
$init_paper_subs{"dvips"} = \&init_paper_dvips;
$init_paper_subs{"context"} = \&init_paper_context;
$init_paper_subs{"dvipdfm"} = \&init_paper_dvipdfm;
$init_paper_subs{"dvipdfmx"} = \&init_paper_dvipdfmx;

sub init_all_papers {
  for my $p (keys %init_paper_subs) {
    &{$init_paper_subs{$p}}();
  }
}

init_all_papers();

my (%l,%m,%r);
foreach my $p (sort keys %papers) {
  if (($p eq "context") && !defined($localtlpdb->get_package("bin-context"))) {
    next;
  }
  $l{$p} = $back_config_pap->Label(-text => ___("defaultpaperfor") . " $p", -anchor => "w");
  $m{$p} = $back_config_pap->Label(-textvariable => \$defaultpaper{$p}, -anchor => "w");
  $r{$p} = $back_config_pap->Button(-text => ___"change",
            -command => sub { select_paper($p); }, -anchor => "w");
  $l{$p}->grid( $m{$p}, $r{$p},
           -padx => "2m", -pady => "2m", -sticky => "nsw");
}

$back_config_pap->pack(-side => 'left', -fill => "both", -padx => "2m", -pady => "2m");


my $back_config_act = $lower->Labelframe(-text => ___"actions");


$back_config_act->Button(-text => ___"reinitlsr",
  -command => sub { run_program_show_output("mktexlsr"); })->pack(-expand => 1, -fill => "x", -padx => "2m", -pady => "2m");
$back_config_act->Button(-text => ___"recreateformats",
  -command => sub { run_program_show_output("fmtutil-sys --all"); })->pack(-expand => 1, -fill => "x", -padx => "2m", -pady => "2m");
$back_config_act->Button(-text => ___"updatemaps",
  -command => sub { run_program_show_output("updmap-sys"); })->pack(-expand => 1, -fill => "x", -padx => "2m", -pady => "2m");

$back_config_act->pack(-side => 'right', -fill => "both", -padx => "2m", -pady => "2m", -expand => 1, -ipadx => "2m", -ipady => "2m");



sub menu_default_location {
  my $val = $default_location;
  my $sw = $mw->Toplevel(-title => ___"changedefaultsrc");
  $sw->transient($mw);
  $sw->grab();
  $sw->Label(-text => ___"newdefaultsrc")->pack(-padx => "2m", -pady => "2m");

  my $f1 = $sw->Frame;
  my $entry = $f1->Entry(-text => $val, -width => 50);
  $entry->pack(-side => "left",-padx => "2m", -pady => "2m");

  my $f2 = $sw->Frame;
  $f2->Button(-text => ___"choosedir", 
    -command => sub {
                      my $var = $sw->chooseDirectory;
                      if (defined($var)) {
                        $entry->delete(0,"end");
                        $entry->insert(0,$var);
                      }
                    })->pack(-side => "left",-padx => "2m", -pady => "2m");
  $f2->Button(-text => ___"defaultnet",
    -command => sub {
                      $entry->delete(0,"end");
                      $entry->insert(0,$TeXLiveURL);
                    })->pack(-side => "left",-padx => "2m", -pady => "2m");
  $f1->pack;
  $f2->pack;

  my $f = $sw->Frame;
  my $okbutton = $f->Button(-text => ___"ok", 
    -command => sub { $default_location = $entry->get;
                      $localtlpdb->option_location($default_location);
                      $localtlpdb->save;
                      $sw->destroy })->pack(-side => 'left',-padx => "2m", -pady => "2m");
  my $cancelbutton = $f->Button(-text => ___"cancel", 
          -command => sub { $sw->destroy })->pack(-side => 'right',-padx => "2m", -pady => "2m");
  $f->pack(-expand => 'x');
  $sw->bind('<Return>', [ $okbutton, 'Invoke' ]);
  $sw->bind('<Escape>', [ $cancelbutton, 'Invoke' ]);
}

sub toggle_setting() {
  my ($key) = @_;
  if ($key eq "formats") { 
    my $new = ($localtlpdb->option_create_formats ? 0 : 1);
    $localtlpdb->option_create_formats($new);
    $default_formats = ($new ? ___"yes" : ___"no");
  } elsif ($key eq "srcfiles") {
    my $new = ($localtlpdb->option_install_srcfiles ? 0 : 1);
    $localtlpdb->option_install_srcfiles($new);
    $default_srcfiles = ($new ? ___"yes" : ___"no");
  } elsif ($key eq "docfiles") {
    my $new = ($localtlpdb->option_install_docfiles ? 0 : 1);
    $localtlpdb->option_install_docfiles($new);
    $default_docfiles = ($new ? ___"yes" : ___"no");
  }
  $localtlpdb->save;
}


sub change_paper {
  my ($prog, $pap) = @_;
  if ($prog eq "all") {
    execute_action_gui ("paper", "paper", $pap);
    init_all_papers();
  } else {
    execute_action_gui ( "paper", $prog, "paper", $pap);
    &{$init_paper_subs{$prog}}();
  }
}

sub select_paper {
  my $prog = shift;
  my $foo = $back_config->Toplevel(-title => ___("paperfor") . " $prog");
  $foo->transient($mw);
  $foo->grab();
  my $var = $defaultpaper{$prog};
  my $opt = $foo->BrowseEntry(-label => ___("defaultpaperfor") . " $prog", -variable => \$var);
  foreach my $p (sort @{$papers{$prog}}) {
    $opt->insert("end",$p);
  }
  $opt->pack(-padx => "2m", -pady => "2m");
  my $f = $foo->Frame;
  my $okbutton = $f->Button(-text => ___"ok", -command => sub { change_paper($prog,$var); $foo->destroy; })->pack(-side => "left", -padx => "2m", -pady => "2m");
  my $cancelbutton = $f->Button(-text => ___"cancel", -command => sub { $foo->destroy; })->pack(-side => "left", -padx => "2m", -pady => "2m");
  $f->pack;
  $foo->bind('<Return>', [ $okbutton, 'Invoke' ]);
  $foo->bind('<Escape>', [ $cancelbutton, 'Invoke' ]);
}

sub select_autobackup {
  my $foo = $back_config->Toplevel(-title => ___"autobackup");
  $foo->transient($mw);
  $foo->grab();
  my $var = $default_autobackup;
  my $opt = $foo->BrowseEntry(-label => ___"autobackup", 
                              -variable => \$var);
  my @al;
  push @al, "-1 (keep arbitrarly many)";
  push @al, "0  (disable)";
  for my $i (1..100) {
    push @al, $i;
  }
  foreach my $p (@al) {
    $opt->insert("end",$p);
  }
  $opt->pack(-padx => "2m", -pady => "2m");
  my $f = $foo->Frame;
  my $okbutton = $f->Button(-text => ___"ok", 
        -command => sub { 
                          $var =~ s/ .*$//;
                          $localtlpdb->option("autobackup", $var);
                          $default_autobackup = $var;
                          $localtlpdb->save;
                          $foo->destroy;
                        }
     )->pack(-side => "left", -padx => "2m", -pady => "2m");
  my $cancelbutton = $f->Button(-text => ___"cancel", -command => sub { $foo->destroy; })->pack(-side => "left", -padx => "2m", -pady => "2m");
  $f->pack;
  $foo->bind('<Return>', [ $okbutton, 'Invoke' ]);
  $foo->bind('<Escape>', [ $cancelbutton, 'Invoke' ]);
}


1;

### Local Variables:
### perl-indent-level: 2
### tab-width: 2
### indent-tabs-mode: nil
### End:
# vim:set tabstop=2 expandtab: #
