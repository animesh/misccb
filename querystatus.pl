#!@WHICHPERL@
##
## $Id: querystatus.pl 2278 2007-12-17 23:15:02Z wes $
##
## $Log$
## $Rev: 2278 $ $Date: 2007-12-17 15:15:02 -0800 (Mon, 17 Dec 2007) $ $Author: wes $
## uses opal to query job status
##

use lib qw(@PERLLIBDIR@);
use Globals;
use Validation;
use CGI qw/:standard/;          # use the CGI package
use SOAP::Lite;
use OpalServices;
use OpalTypes;

$service_url = "@OPAL@";
$refresh = 60;

# start the response form
sub printheaders {
print <<END; 
Content-type: text/html

<HTML>
<HEAD>
<META HTTP-EQUIV="Refresh" CONTENT="$refresh">
<TITLE> MEME - Query Job Status </TITLE>
</HEAD>
<BODY BACKGROUND=\"../images/bkg.jpg\">
<HR>
END
}

$jobid = param('jobid');

my $meme = OpalServices->new(service_url => $service_url);
my $status = $meme->queryStatus($jobid);

if (eval {$status->fault}) {
  print "<h2>FAULT</h2>";
  print "Code:".$status->faultcode."<br>";
  print "String:".$status->faultstring."<br>";
} else { # our soap call didn't fail
  # find out what our status is
  $resp_code = $status->getCode();
  $resp_msg = $status->getMessage();
  $out_url = $status->getBaseURL();
  if ($resp_code==8) { # GramJob.STATUS_DONE
    print redirect(-uri=>$out_url,-status=>302);
  } elsif ($resp_code==4) { # GramJob.STATUS_FAILED
    &printheaders;
    print "<h2>Job Failed</h2><br><hr><br>";
    print "Output code: $resp_code<br>";
    print "Message: $resp_msg<br>";
    print "Base Output URL: <a href=\"$out_url\">$out_url</a><br>";
  } else {
    &printheaders;
    print "<h2>$resp_msg</h2><br><hr><br>";
    print "Output code: $resp_code<br>";
    print "Message: $resp_msg<br>";
    print "Base Output URL: <a href=\"$out_url\">$out_url</a><br>";
    print "Page will refresh in $refresh seconds.<br>";
  }
}

print "
<HR>
</BODY>
</HTML>
";

exit(0);