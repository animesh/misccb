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

 $ua = LWP::UserAgent->new;
 $ua->proxy(['http'] => 'http://animesh_sharma:Infosys123@ad.infosys.com');

 $req = HTTP::Request->new( GET ,"http://sparsh/");

 $res = $ua->request($req);
 print $res->content if $res->is_success ;