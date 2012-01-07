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
         $ua = LWP::UserAgent->new;$ua=p1::p1($ua);
         my @imgs = ();
         sub callback {
            my($tag, %attr) = @_;
            return if $tag ne 'img';  
            push(@imgs, values %attr);
         }
         $p = HTML::LinkExtor->new(\&callback);
	 foreach ($p->links) { print $_;}
         $res = $ua->request(HTTP::Request->new(GET => $url),
                             sub {$p->parse($_[0])});
	 my $base = $res->base;
         @imgs = map { $_ = url($_, $base)->abs; } @imgs;
         print join("\n", @imgs), "\n";

