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
         use HTML::LinkExtor;
         use URI::URL;
	 use proxy;
	 
	 $l1=shift @ARGV;
         if ($l1 !~ /^http/){$url = "http\:\/\/".$l1;}
	 else{$url=$l1;}  # for instance
         $ua = LWP::UserAgent->new;
         # Set up a callback that collect image links
         my @imgs = ();
         sub callback {
            my($tag, %attr) = @_;
            return if $tag ne 'img';  # we only look closer at <img ...>
            push(@imgs, values %attr);
         }
         # Make the parser.  Unfortunately, we don't know the base yet
         # (it might be diffent from $url)
         $p = HTML::LinkExtor->new(\&callback);
	 #foreach ($p->links) { print $_;}
         # Request document and parse it as it arrives
         $res = $ua->request(HTTP::Request->new(GET => $url),
                             sub {$p->parse($_[0])});
	 $ua=p1::p1($ua);
         # Expand all image URLs to absolute ones
         my $base = $res->base;
         @imgs = map { $_ = url($_, $base)->abs; } @imgs;

         # Print them out
         print join("\n", @imgs), "\n";

