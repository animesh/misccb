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
$date=time();
$fsd="t.".$date;$fs=t;
mkdir $fsd;
while($l = <F>){
	chomp $l;
	if($l =~ /html$/){
	$lt=$l;$lt =~ s/\.html//;
	$ld=$lt;#$js="js";$href="href";$error="error";$img="img";
	system("mkdir $fsd/$ld/");
	open FE,">./$fsd/$ld/$ld.err";
	open FS,">./$fsd/$ld/$ld.suc";
	system("mkdir $fsd/$ld/js");
	system("mkdir $fsd/$ld/img");
	system("mkdir $fsd/$ld/href");
	$test=LE($l);
	print "$test\n\n";
	}close FE;close FS;
}
close F;
sub LE {
	   use HTML::LinkExtractor;
           use Data::Dumper;
	   $f= shift;
	   #print "$f-\n\n\n\n";
	   #open (F,$f);
	   use HTML::TokeParser::Simple;
           my $p = HTML::TokeParser::Simple->new( $f );

        	while ( my $token = $p->get_token ) {
            	#This prints all text in an HTML doc (i.e., it strips the HTML)
            	next unless $token->is_tag;
		$t1=$token->as_is;
            		if($t1 =~ /\<a\ href/){
			#print $token->as_is; print "a href \n";
			@temp=split(/"/,$t1);foreach (@temp){$_=~s/\s+//g;
				if($_ =~ /^http/ and $_ =~ /\?/ )
					{print "href-$_\n";
					$fft=DL($_);					
					$t111=MO($_);
					if(length($t111)>200){$t111=TR($t111);$t111.=".tr";}
					$t111.=".html";
					if($fft ne "")
					{system("mv $fft $fsd/$ld/href/$t111");}
					}
				if($_ =~ /^javascript/ )
					{
					@tjs=split(/'/,$_);#print "javascript-$_\n";
						foreach $w (@tjs){$w=~s/\s+//g;
							if($w =~ /^http/ and $w =~ /\?/){print "javascript-$w\n";
							$fft=DL($w);
							$t111=MO($_);
							if(length($t111)>200){$t111=TR($t111);$t111.=".tr";}
							$t111.=".html";
							if($fft ne ""){
							system("mv $fft $fsd/$ld/js/$t111");}
							}
						}				
					}
				}
			}
			if($t1 =~ /\<img/){
				@tjs=split(/"/,$t1);#print "img-$t1\n";
						foreach $w (@tjs){$w=~s/\s+//g;
							if($w =~ /^http/ and $w =~ /\?/){print "img-$w\n";
							$fft=DL($w);
							$t111=MO($_);
							if(length($t111)>200){$t111=TR($t111);$t111.=".tr";}
							$t111.=".html";
							if($fft ne ""){
							system("mv $fft $fsd/$ld/img/$t111");}
							}
						}
			#print $token->as_is; print "img \n";
			}
	        }
	return $f;
	}

sub DL {
 $get=shift;
 @temp=split(/\//,$get);$t=@temp[-1];
 if ( $t =~ /\?/ ) 
 {
 open FDL,">temp.html";
 print "$get\n";$i=$get;
 use LWP::UserAgent;
 	$ua = LWP::UserAgent->new;$ua->proxy(['http', 'ftp'] => 'http://animesh_sharma:Infosys123@192.168.100.25');
 $req = HTTP::Request->new( GET ,$get);
 $res = $ua->request($req);
 if( $res->is_success ){print FS "$i\n";print FDL $res->content;}
 else {print FE"$i\n";}
 return "temp.html";
 }
}
sub MO
{
$tt=shift;
					$tt=~s/\//-fs-/g;$tt=~s/\\/-bs-/g;$tt=~s/\&/-amp-/g;
					$tt=~s/\+/-plus-/g;$tt=~s/\_/-us-/g;
					$tt=~s/\?/-ques-/g;$tt=~s/\=/-equal-/g;$tt=~s/\|/-pipe-/g;
					$tt=~s/\~/-tilde-/g;$tt=~s/\*/-star-/g;$tt=~s/\%/-pc-/g;
					$tt=~s/\:/-colon-/g;$tt=~s/\;/-sc-/g;$tt=~s/\"/-qoute-/g;
					$tt=~s/\'/-sq-/g;$tt=~s/\`/-apos-/g;$tt=~s/\$/-dollar-/g;
					$tt=~s/\#/-hash-/g;$tt=~s/\@/-at-/g;$tt=~s/\!/-exclam-/g;
					$tt=~s/\(/-bo-/g;$tt=~s/\)/-bc-/g;$tt=~s/\,/-comma-/g;
return $tt;
}
sub TR {
	$tz = shift;
	$ttz=substr($tz,0,200);
	return $ttz;
	}

#if(length($tz)>240){$ttz=substr($tz,0,240);}