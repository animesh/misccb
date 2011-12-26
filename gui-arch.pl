# gui-arch.pl
# $Id: gui-arch.pl 11917 2009-01-19 22:09:34Z preining $
#
# Copyright 2008 Tomasz Luczak, Norbert Preining
#
# GUI for tlmgr
#


our $back_arch = $back->add("arch",-label => ___"archs");
$screens{"arch"} = $back_arch;

# this will be loaded only on unix systems!
#

our @archsavail;
our @archsinstalled;
our %archs;

our %archsbuttons;
our $arch_frame;
our $subframe;

push @update_function_list, \&init_archs;

$back_arch->Label(-text => ___"remarchnotpos")->pack(-padx => "5m", -pady => "5m");


sub init_archs {
  if (!defined($tlmediatlpdb)) {
    @archsavailable = ();
  } else {
    @archsavail = $tlmediatlpdb->available_architectures;
  }
  @archsinstalled = $localtlpdb->available_architectures;
  foreach my $a (@archsavail) {
    $archs{$a} = 0;
    if (grep(/^$a$/,@archsinstalled)) {
      $archs{$a} = 1;
    }
  }
  foreach my $a (keys %archsbuttons) {
    $archsbuttons{$a}->destroy;
  }
  $arch_frame->destroy if defined($arch_frame);
  $subframe->destroy if defined($subframe);
  $subframe = $back_arch->Frame;
  foreach my $a (@archsavail) {
    $archsbuttons{$a} = 
      $subframe->Checkbutton(-command => sub { check_on_removal($a); },
                          -variable => \$archs{$a}, 
                          -text => platform_desc($a)
                         )->pack(-anchor => 'w');
  }
  $subframe->pack;
  $arch_frame = $back_arch->Frame;
  $arch_frame->pack(-padx => "10m", -pady => "5m");
  $arch_frame->Button(-text => ___"applychanges", -command => sub { apply_changes(); })->pack(-side => 'left', -padx => "3m");
  $arch_frame->Button(-text => ___"resetchanges", -command => sub { init_archs(); })->pack(-side => 'left', -padx => "3m");
}

sub check_on_removal {
  my $a = shift;
  if (!$archs{$a} && grep(/^$a$/,@archsinstalled)) {
    # removal not supported
    $archs{$a} = 1;
    $arch_frame->Dialog(-title => "info",
                        -text => ___"remarchinfo",
                        -buttons => [ ___"ok" ])->Show;
  }
}


sub apply_changes {
  my @todo;
  foreach my $a (@archsavail) {
    next if grep(/^$a$/,@archsinstalled);
    push @todo, $a if $archs{$a};
  }
  if (@todo) {
    execute_action_gui ( "arch", "add", @todo );
    reinit_local_tlpdb();
    init_archs();
  }
}

1;

### Local Variables:
### perl-indent-level: 2
### tab-width: 2
### indent-tabs-mode: nil
### End:
# vim:set tabstop=2 expandtab: #
