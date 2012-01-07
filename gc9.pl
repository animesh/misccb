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

$fs = shift @ARGV;
$date=time();
open F,$fs;
$fsd=$fs.".down.".$date;
system("mkdir $fsd");
while($l = <F>){
	chomp $l;
	@temp=split(/\s+/,$l);
	#foreach (@temp) {print "$_\t";}print "\n";
	if(@temp[2] ne ""){
	push(@list,(@temp[2]));}
	}
	#print @list;
close F;
 open FE,">./$fsd/$fs.err";
 open FS,">./$fsd/$fs.suc";
foreach (@list) { DOWN($_); }
 sub DOWN {
 $get="http://www.zbit.uni-tuebingen.de/cgi-bin/genecards/carddisp?";
 @temp=split(/\//,$get);$t=@temp[-1];$t=~s/\?//g;
 #print "$t\n";
 open F,">./$fsd/$_.html";
 $i=$_;
 $get=$get.$_;
 print "$get\n";
 use LWP::UserAgent;use proxy;
 $ua = LWP::UserAgent->new;
 	#$ua->proxy(['http', 'ftp'] => 'http://animesh_sharma:Infosys123@proxy_realm\MicrosoftAD');
 	$ua=p1::p1($ua);
	#$ua->proxy(['http', 'ftp'] => 'http://animesh_sharma:Infosys123@192.168.100.25');
 #$req = HTTP::Request->new( GET ,"http://sparsh/");
 $req = HTTP::Request->new( GET ,$get);
 $res = $ua->request($req);
 if( $res->is_success ){print FS "$i\n";print F $res->content;}
 else {print FE"$i\n";}
 close F;
}
close FS;close FE;
