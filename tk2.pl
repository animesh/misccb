#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#    Code base of Animesh Sharma [ sharma.animesh@gmail.com ]


               use strict;
               use Tk;

               my $main = MainWindow->new;
               $main->Label(-text => 'Print file')->pack;
               my $font = $main->Entry(-width => 10);
               $font->pack;
               my $filename = $main->Entry(-width => 10);
               $filename->pack;
               $main->Button(-text => 'Fax',
                             -command => sub{do_fax($filename, $font)}
                             )->pack;
               $main->Button(-text => 'Print',
                             -command => sub{do_print($filename, $font)}
                             )->pack;
               MainLoop;

               sub do_fax {
                   my ($file, $font) = @_;
                   my $file_val = $file->get;
                   my $font_val = $font->get;
                   print "Now faxing $file_val in $font_val\n";
               }

               sub do_print {
                   my ($file, $font) = @_;
                   my $file_val = $file->get;
                   my $font_val = $font->get;
                   print "Sending file $file_val to printer in $font_val\n";
               }
