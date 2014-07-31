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

  use LWP::UserAgent;
  $ua = new LWP::UserAgent;
  $ua->env_proxy; # initialize from environment variables
  # or
  #$ua->proxy(http  => 'http://www.hotmail.com');
  #$ua->proxy(ftp  => 'http://proxy.myorg.com');
  #$ua->proxy(wais => 'http://proxy.myorg.com');
  $ua->no_proxy(qw(no se fi));

  #my $req = new HTTP::Request 'wais://xxx.com/';
  my $req = new HTTP::Request 'http://www.hotmail.com/';
  print $ua->request($req)->as_string;

