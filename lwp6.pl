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
	$get=shift;if($get !~ /^http/){$get="http\:\/\/".$get;}
        $ua = LWP::UserAgent->new;#$a=$ENV{'env_pro'};$b=$ENV{'env_pas'};
	#$tcpip="http\:\/\/".$ENV{'env_pro'}."\:".$ENV{'env_pas'}."\@192.168.100.25";#'tcpip'=$tcpip;
        $ua->agent("i\*_\*dunno/0.1 ");$ua->proxy(['http', 'ftp'] => 'http://animesh_sharma:Infosys123@192.168.100.25');
	#$ua->agent("i\*_\*dunno/0.1 ");$ua->proxy(['http', 'ftp'] => $tcpip );
	my $req = HTTP::Request->new(GET => $get);
        $req->content_type('application/x-www-form-urlencoded');
        $req->content('match=www&errors=0');
	my $res = $ua->request($req);
	if ($res->is_success) {
            print $res->content;
        } else {
            print "u ' ve  b e e n  f * * h k'd  t h i s t i m e\n";
        }
