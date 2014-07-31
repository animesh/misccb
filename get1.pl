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
$f1 = shift @ARGV;$f2=$f1.".err.txt";
open(F,$f1);
open(E,">$f2");
while($line=<F>)
{
 @t1=split(/\s+/,$line);
 @t2=split(/\./,@t1[1]);
 @t2[1]=~s/\s+//g;
 if(@t2[1]=~/^b/){$gc_id=@t2[1];
 #http://www.genome.jp/dbget-bin/www_bget?eco+b0115+-s
 $full_path='http://www.genome.jp/dbget-bin/www_bget'.'\?'.'eco+'.@t2[1].'+-s';
 $localfile='Eco.'.$gc_id.'.Glu.txt';
 unless (open(OUT,">$localfile")){print E"ERROR FOR WRITING:$localfile $!\n";exit;}
 $full_path='http://www.genome.jp/dbget-bin/www_bget'.'?'.'eco+'.$gc_id.'+-s';
 $full_path="$full_path";
 #until(system("wget $full_path")){print "not able to write $gc_id\n";}
 $ua=new LWP::UserAgent;
 $request = new HTTP::Request('GET', $full_path);
 $response = $ua->request($request);
 $content= $response->content;
 unless($response->is_success){
 	die "$full_path , $response->error_as_HTML\n";
 	}print OUT $content;close (OUT);print "$gc_id\n";
 }
}
close F;close E;
