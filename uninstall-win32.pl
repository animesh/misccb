#!/usr/bin/env perl
#
# $Id: uninstall-win32.pl 10391 2008-08-21 13:59:47Z siepo $
#
# Copyright 2008 Norbert Preining
#
# GUI for tlmgr
#

my $Master;

BEGIN {
  $^W = 1;
  $Master = `%COMSPEC% /c kpsewhich -var-value=SELFAUTOPARENT`;
  chomp($Master);
  unshift (@INC, "$Master/tlpkg");
}

use TeXLive::TLWinGoo;
use Tk;
use Tk::Dialog;

my $mw = MainWindow->new(-title => "remove tlmgr 2008");

my $lab = $mw->Label(-justify    => 'left',
                     -text => "Do you really want to remove TeX Live 2008?");

$lab->pack(-padx => "10m", -pady => "5m");

my $f = $mw->Frame;
$f->pack(-padx => "10m", -pady => "5m");

my $ok = $f->Button(-text => "Ok",
                    -command => sub { $mw->destroy; doit(); exit(0); });
my $cancel = $f->Button(-text => "Cancel",
                        -command => sub { $mw->destroy; exit(1); });

$ok->pack(-side => 'left', -padx => "3m");
$cancel->pack(-side => 'left' , -padx => "3m");

sub doit {
  remove_texbindirs_from_path();
  unsetenv_reg("TEXBINDIR");
  # from_dvd case:
  unsetenv_reg("TEXMFSYSVAR");
  unsetenv_reg("TEXMFCNF");
  unregister_uninstaller();
  exit if is_vista();
  broadcast_env();
  update_assocs();
}

Tk::MainLoop();

__END__


### Local Variables:
### perl-indent-level: 2
### tab-width: 2
### indent-tabs-mode: nil
### End:
# vim:set tabstop=2 expandtab: #
