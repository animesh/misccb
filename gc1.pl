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

#!usr/bin/perl
use LWP::UserAgent;
open(F,"genecardid.txt");
while($line=<F>)
{
 $gc_id=$line;
 chomp $gc_id;
 print $gc_id;
 $gc_id=~ tr/a-z/A-Z/;
 $localfile=$gc_id.'.gc.html';
 	unless (open(OUT,">$localfile")){
 	print "ERROR IN OPENING FILE  $localfile FOR WRITING:$!\n";exit;
 	}
 $web_site='http://bioinfo.weizmann.ac.il/cards-bin/';
 $full_path=$web_site.'carddisp?'.$gc_id;
 $ua=new LWP::UserAgent;
 $request = new HTTP::Request('GET', $full_path);
 $response = $ua->request($request);
 $content= $response->content;
 	unless($response->is_success){
 	die "$full_path,$response->error_as_HTML\n";
 	}
 print OUT $content;
 close (OUT);
}
close F;
