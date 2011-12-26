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

system ("ls -1 > t");
open F,"t";
while($l = <F>){
	chomp $l;
	if($l =~ /html$/){
	$lt=$l;$lt =~ s/\.html//;
	$ld=$lt;#$js="js";$href="href";$error="error";$img="img";
	#system("mkdir $fsd/$ld/");open FE,">./$fsd/$ld/$ld.err";open FS,">./$fsd/$ld/$ld.suc";system("mkdir $fsd/$ld/js");system("mkdir $fsd/$ld/img");system("mkdir $fsd/$ld/href");
	NLE($l);
	}
}
close F;
sub NLE{
	 use LWP::UserAgent;
         use HTML::LinkExtor;
         use URI::URL;
	 use proxy;
	 $l1=shift;
	 
         undef $/;
	 open(FILE, $l1);
	 $url = <FILE>;
	 
	 close(FILE);
 	 print "$l1\nstarting$url";
	 my @href = ();
         sub callbacka {
            my($tag, %attr) = @_;
            return if $tag ne 'a'; 
            push(@href, values %attr);
         }
         $p = HTML::LinkExtor->new(\&callbacka);
	 $res = sub {$p->parse($url)};
         #my $base = $res->base;
         #@href = map { $_ = url($_, $base)->abs; } @href;
	 print join("\n", @href), "\n";
}
#if(length($tz)>240){$ttz=substr($tz,0,240);}
