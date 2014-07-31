#!/usr/bin/perl
 use LWP::UserAgent;
 $get=shift;if( $get !~ /^http/ ){$get="http\:\/\/".$get;}
 					$ua = LWP::UserAgent->new;$ua->proxy(['http', 'ftp'] => 'http://infosys_account:pass@192.168.100.25');
 $req = HTTP::Request->new( GET , $get );
 $res = $ua->request($req);
 print $res->content if $res->is_success ;
