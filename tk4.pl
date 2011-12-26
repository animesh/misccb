               #! /usr/bin/perl5 -w

               use strict;
               use Tk;

               my $main = MainWindow->new;
               fill_window($main, 'Main');
               my $top1 = $main->Toplevel;
               fill_window($top1, 'First top-level');
               my $top2 = $main->Toplevel;
               fill_window($top2, 'Second top-level');
               MainLoop;

               sub fill_window {
                   my ($window, $header) = @_;
                   $window->Label(-text => $header)->pack;
                   $window->Button(-text => 'close',
                                   -command => [$window => 'destroy']
                                   )->pack(-side => 'left');
                   $window->Button(-text => 'exit',
                                   -command => [$main => 'destroy']
                                   )->pack(-side => 'right');
               }
