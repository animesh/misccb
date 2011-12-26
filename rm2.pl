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

#!/usr/local/bin/perl
## Time-stamp: "1998-11-10 14:11:49 MST" ##
use MIDI::Simple 0.7;

# Main
new_score;
@subs = ( \&measure_counter, \&psycho, \&boom, \&tboom, \&clap );
foreach (1 .. 24) { synch(@subs) }
write_score("rhythm2.midi");
exit;

# Subs
sub measure_counter {
  my $it = shift;
  $it->r(wn); # a whole rest
  ++$measure;
}

sub boom {
  my $it = shift;
  return if $measure % 4 < 2;
  $it->n(c9, ff, n41, qn);  $it->r;
  $it->n(f);  r;
}

sub tboom {
  my $it = shift;
  return if $measure % 4 < 2;
  # 42 = 'Closed Hi-Hat' ; 43 = 'High Floor Tom'
  # In quick succession...
  $it->n( c9, ff, n43, sn); $it->n( n42 ); $it->r(dqn);
  # dqn = dotted quarter note/rest
  $it->r( c9, ff, n43, sn); $it->n( n42 ); $it->r(dqn);
}

sub clap {
  my $it = shift;
  return if  $measure < 4;
  $it->n(c9, ff, n39, sn); $it->n;
  $it->r(dqn);
  $it->r(hn);
}

sub psycho {
  my $it = shift;
  my $pattern =
    "  !.!.!.   !!!!!!   !.!.  " ;
  $pattern =~ tr<\cm\cj\t ><>d; # kill whitespace
  warn "<$pattern> doesn't add up to a whole measure\n"
    unless length($pattern) == 16;
  $it->noop(c9, mf, n37, sn);
  # setup: n37 on c9 = side stick
  foreach (split('', $pattern)) {
    if($_ eq '!') { $it->n }
    else { $it->r }
  }
}

system("wmplayer rhythm2.midi");