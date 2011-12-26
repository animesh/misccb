# gui-uninstall.pl
# $Id: gui-uninstall.pl 8494 2008-06-02 17:59:28Z preining $
#
# Copyright 2008 Tomasz Luczak, Norbert Preining
#
# GUI for tlmgr
#

our $back_f3 = $back->add("uninstall", -label => ___"removaltab");
$screens{"uninstall"} = $back_f3;

if ($^O=~/^MSWin(32|64)$/i) {
  my $lab = $back_f3->Label(-justify    => 'left', 
           -text => ___"pleaseuse");

  $lab->pack(-padx => "10m", -pady => "5m");
} else {
  my $lab = $back_f3->Label(-justify    => 'left', 
           -text => ___"reallyremove");

  $lab->pack(-padx => "10m", -pady => "5m");

  my $f = $back_f3->Frame;
  $f->pack(-padx => "10m", -pady => "5m");

  my $ok = $f->Button(-text => ___"removetl", 
          -command => sub { 
                            system("tlmgr", "uninstall", "--force");
                            $mw->Dialog(-text => ___"completerem", -buttons => [ ___"ok" ])->Show;
                            $mw->destroy; 
                            exit(0); 
                          });

  $ok->pack(-side => 'left', -padx => "3m");
}

1;

### Local Variables:
### perl-indent-level: 2
### tab-width: 2
### indent-tabs-mode: nil
### End:
# vim:set tabstop=2 expandtab: #
