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

         # Create a user agent object
         use LWP::UserAgent;
         $ua = LWP::UserAgent->new;
         $ua->agent("MyApp/0.1 ");

         # Create a request
         my $req = HTTP::Request->new(POST => 'http://www.perl.com/cgi-bin/BugGlimpse');
         $req->content_type('application/x-www-form-urlencoded');
         $req->content('match=www&errors=0');

         # Pass request to the user agent and get a response back
         my $res = $ua->request($req);

         # Check the outcome of the response
         if ($res->is_success) {
             print $res->content;
         } else {
             print "Bad luck this time\n";
         }


