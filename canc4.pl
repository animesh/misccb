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
#!usr/bin/perl
use LWP::UserAgent;
$file=shift@ARGV;
open(F,$file);
open(S,">$file.success.txt");
open(E,">$file.error.txt");
while($l=<F>)
{
if ($l=~/\<href\>/)
        {
	
        $lt=$l;chomp $l;
        $l=~s/\<href\>//;
        $l=~s/\<\/href\>//;
        $l=~s/\$/\&/g;$l=~s/\s+//g;
        if($l=~/javascript/)
	{
	$l=~s/javascript\:popup//g;
	$l=~s/\(//g;$l=~s/\)//g;$l=~s/'//g;$l=~s/\s+//g;
	@temp=split(/,/,$l);
	$ltemp=@temp[0];
	print "$ltemp\t$lt\n";
	spidez($ltemp,$file);
	}
	else
	{
	spidez($l,$file);
        print "$l\n$lt\n";
        }
}
else{next;}
}
sub spidez
   {
   $f=shift;chomp $f;
   $fo=shift;chomp $fo;
   $localfile=$fo.'-'.$f.'.html';
   $localfile=~s/\//\-/g;
   print "$f\t$localfile\t$fo\n";
   unless (open(OUT,">$localfile")){print E"ERROR FOR WRITING:$localfile $!\n";exit;}
   $full_path=$f;
   $ua=new LWP::UserAgent;
   $request = new HTTP::Request('GET', $full_path);
   $response = $ua->request($request);
   $content= $response->content;
   unless($response->is_success){die "$full_path,$response->error_as_HTML\n";}
   print OUT $content;close (OUT);print S"$f\n";
}
close F;
close S;close E;
