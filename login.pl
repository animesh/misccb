#!/usr/bin/perl
# login.pl   - Login Page        
# file path /var/www/cgi-bin



use CGI qw(:standard escapeHTML);
use warnings;
use POSIX;
use Qui::QuiDB;
my ($dbh) = Qui::QuiDB::connect();		#Assigning database handler for MySQL#Assigning global variables

my ($query) = new CGI;								
#my ($JSCRIPT) = Qui::QuiDB::jscript();	        #calling for javascript fuctionality
my ($filepath) = Qui::QuiDB::get_file_path();
my ($jspath) = Qui::QuiDB::get_js_path();
my ($uid) =param("uid");
my ($pass)=param("pword");
my ($ch) = lc(Qui::QuiDB::collapse_whitespace(Qui::QuiDB::trim(param("click"))));
#added for hospital maintanance on 22nd march 1016hrs

$cookie1 = $query->cookie(-name => 'savedhid', -value => ''); 
$cookie2 = $query->cookie(-name => 'saveddeptid', -value => '');
$cookie3 = $query->cookie(-name => 'frm', -value => '');
$cookie4 = $query->cookie(-name => 'inserteddrids', -value => '');
$cookie5 = $query->cookie(-name=>'uid', -value=>'');

#print $query->header (-cookie=>$cookie);
		
		print $query->header (-cookie=>[$cookie1,$cookie2,$cookie3,$cookie4,$cookie5]);
	
#####################################################

#@ BEGIN MAIN_PROGRAM
	#print $query->header (-cookie=>$cookie);
    	    	print $query->start_html(-bgcolor => "#FFF8DF", -title=>'Doctor database-Login Page ',-onLoad=>"if (document.frm.uid){document.frm.uid.focus();}", -script => $JSCRIPT);

	##Added for menu###########################
		print "<style>\n";
		print "a:hover{color:ff0033}\n";
		print "a{text-decoration:none;}\n";
		print ".txt { font-family: arial;font-size: 10 } \n ";
		print "</style>\n";

	##Added for menu###########################
	
		print "<TABLE cellSpacing=0 cellPadding=0 width=\"100%\" border=0>\n";
	
		print "<TR>\n";
		print br ();print br ();
	    	print "<TD align=center><img src='".$jspath."logo.jpg'>\n";
		print "</TD>\n";
		print "</TR>\n";
		print "</TABLE>\n";
	
   	##################INSERT FOR VALIDATION
   	if ($ch eq "login")
    	{
    	
 
		$pass=set_password($pass);
		$sth = $dbh->prepare("select * from userprofile where uid = ? and pwd = ?");
		#$sth = $dbh->prepare("select * from userprofile where uid='rajaram' and pwd='infosys'");
	    	$sth->execute($uid, $pass);
	    	#$sth->execute('rajaram', 'infosys');
 
    	if ($sth->rows() != 0) {
    		
    	  while($sthref = $sth->fetchrow_hashref())
    		{
	    		$acctstat=$sthref->{ind};
	    		$numattempt=$sthref->{numattempt};
	    		$expdt=$sthref->{expdt};
	    		if ($acctstat ne "Y")
	    	{
	    	
		    	$message =td ({-align => "center"}, "&nbsp", span({-style => 'font-family: arial;font-size: 12; color: red'},
					b("Your account is currently inactive. Please contact your system administrator.")));
			
		    	last;
	    	}
    		if ($numattempt <= 0)
	    	{
	    	
		    	$message =td ({-align => "center"}, "&nbsp", span({-style => 'font-family: arial;font-size: 12; color: red'},
					b("You have exceeded the number of invalid login attempts. Please contact your system administrator.")));
			
		    	last;
	    	}
	    		if (defined $expdt and $expdt ne "0000-00-00")
	    		{
	    			
		    		($yy1,$mm1,$dd1)=split(/-/,$expdt);
		    		
		    		$sec = 1;
				$min = 0;
				$hour = 0;
				$mday = $dd1;
				$mon = $mm1 - 1;
				$year = $yy1 - 1900;
				
				$timestamp = mktime($sec,$min,$hour,$mday,$mon,$year,0,-1);
				
				$xxxx=compare_date($timestamp);
				
				if ($xxxx <= 7 and $xxxx > 0)
				{
				
						$plpath=$filepath."index.pl?msg=$xxxx&uid=$uid";
						
						$xxxx=sprintf("%d",$xxxx);
						print start_form(-name=>"frm",-action=>url());
						print "<script language='javascript' type='text/javascript'>";
						print "   <!--
	     					Newsite= window.open('$plpath','_self');
	     					// -->";
	 					print "</script>";
						print $query->hidden(-name=>"uid",-value=>$uid);	
						print end_form();
						exit(0);
				}
				elsif($xxxx <= 0)
				{
						
						$message =td ({-align => "center"}, "&nbsp", span({-style => 'font-family: arial;font-size: 12; color: red'},
							b("Your password has expired.Please contact your system administrator.")));
						last;
				}
				else
				{
						$plpath=$filepath."index.pl?uid=$uid";
						print start_form(-name=>"frm",-action=>url());
						print "<script language='javascript' type='text/javascript'>";
						print "   <!--
	     					Newsite= window.open('$plpath','_self');
	     					// -->";
	 					print "</script>";
						print $query->hidden(-name=>"uid",-value=>$uid);	
						print end_form();
						exit(0);
				}
			}
			else
			{
						$plpath=$filepath."index.pl?uid=$uid";
						print start_form(-name=>"frm",-action=>url());
						print "<script language='javascript' type='text/javascript'>";
						print "   <!--
	     					Newsite= window.open('$plpath','_self');
	     					// -->";
	 					print "</script>";
						print $query->hidden(-name=>"uid",-value=>$uid);	
						print end_form();
						exit(0);			}
			
	    		
	    		    		
	    	}
    		
    	} else {
	    		$message=td ({-align => "center"}, "&nbsp", span({-style => 'font-family: arial;font-size: 12; color: red'},
					b("Your previous logon was unsuccessful."+$pass)));
	
	    		
	    		my $sqt="UPDATE userprofile set numattempt= numattempt - 1 where uid='$uid' and numattempt > 0";
	    		$sth=$dbh->prepare($sqt);$sth->execute();	
	    		
    		
    	
    		
    	}
	}
	
	###############################	        		
	show_add_form ();	
	print $query->end_html ();
	exit (0);
#@ END MAIN_PROGRAM

#@ SHOW_ADD_FORM
sub show_add_form
{
	print br ();
	$spc = "&nbsp" x 40;
	$spc1 = "&nbsp" x 35;
	print $query->start_form(-method => "post", -name => "frm", -action => url());
		print br ();#print br ();print br ();#print br ();print br ();print br ();

		print $query->table ({-width => "50%", -cellspacing => "2", -cellpadding => "1", -align=>"center"},
		Tr(td),
		Tr(td),

		"<tr><td colspan=3 align=\"center\"><span style='font-family: arial;font-size: 23'><strong>Doctor Database v1.0.2</strong></td></tr>",			
#		"<tr><td colspan=3 align=\"center\"><span style='font-family: arial;font-size: 23'><strong>Prototype v1.23</strong></td></tr>",			
#		Tr(td({-colspan=>2,-align => 'center'},$spc1, span({-style => 'font-family: arial;font-size: 23'},
#        	strong("Doctors Database v1.0"))),
#        	
#        	),
#
#		Tr(td({-colspan=>2,-align => 'center'},$spc1, span({-style => 'font-family: arial;font-size: 23'},
#        	strong("Prototype v1.23"))),
#        	
#        	),
#


#		Tr(
#		({-align => 'center'},$spc1, span({-style => 'font-family: arial;font-size: 23'},
#        	strong("Doctors Database v1.0")))),
#        	hidden(-name=>"msg", -value=>""),
#        	Tr(
#        	({-align => 'center'},$spc,span({-style => 'font-family: arial;font-size: 20'}, "&nbsp","&nbsp",
#        		strong("Prototype v1.23")))),



#		Tr(
#		({-align => 'center'},$spc1, span({-style => 'font-family: arial;font-size: 23'},
#        	strong("Doctors Database v1.0")))),
#        	hidden(-name=>"msg", -value=>""),
#        	Tr(
#        	({-align => 'center'},$spc,span({-style => 'font-family: arial;font-size: 20'}, "&nbsp","&nbsp",
#        		strong("Prototype v1.23")))),


		Tr(td),
		Tr(td),
		Tr(
			td($message)
		),
		Tr(td),
		Tr(td),
		Tr(td),
		Tr(td),
		Tr(td),
		Tr(td),
		Tr(td),
		Tr(td),
		Tr(td),
		Tr(td),
		Tr(td),
		Tr(td),
		Tr(td),
		Tr(td),
		
		
		
		
		Tr (
			td ({-align => "right"}, "&nbsp",  
				span({-style => 'font-family: arial;font-size: 12'},
				"User ID:")),
			td ({-align => "center"},textfield (-name => "uid", -size => 29,
				-maxlength => 10))
		),
		
		Tr (
			td ({-align => "right"}, "&nbsp", 
				span({-style => 'font-family: arial;font-size: 12'},
				"Password:")),
			td ({-align => "center"},password_field (-name => "pword", -size => 29, -maxlength => 50 
				)), 
			td (submit(-name => "click", -value => "Login" ))
		),
		
		);
		print br ();print br ();print br ();print br ();
		print br ();print br ();print br ();print br ();
		#print br ();print br ();print br ();print br ();
		print $query->table ({-width => "100%", -cellspacing => "2", -cellpadding => "1", -align=>"center"},
		Tr (
			td ({-align => "center"}, "&nbsp", span({-style => 'font-family: arial;font-size: 12; color: red'},
				b("Quintiles Doctor Database"))),
		),
		Tr (
			td ({-align => "center"}, "&nbsp", span({-style => 'font-family: arial;font-size: 12; color: red'},
				b(" Access is restricted to authorised personnel only. Unauthorised access to this system will lead to prosecution."))),

		)
		);
	print $query->end_form();
}
#@ SHOW_ADD_FORM



###########SET_PASSWORD
#sub set_password
#{
#my $string1=shift;
#if (defined $string1 and $string1 ne "")
#{
#@out = unpack("H*",$string1);
#$storedp=join("",@out);
#print p($storedp );
#
##$storedp = ($storedp);
#return ($storedp);
#}
#else
#{
#return;
#}
#}
sub set_password
{
my $string1=shift;
if (defined $string1 and $string1 ne "")
	{
	@out = unpack("C*",$string1);
	
	foreach my $val(@out)
	{
		$val2 = $val2 + $val;
	}
	
	$storedp = ($val2*7);
	return ($storedp);
	}
else
	{
	return;
	}


}
sub compare_date {
my $timestamp=shift;
#print p($timestamp);
if (defined $timestamp and $timestamp ne "")
{
# Define arrays for the day of the week and month of the year.
my @days   = ('Sunday','Monday','Tuesday','Wednesday',
   'Thursday','Friday','Saturday');
my @months = ('01','02','03','04','05','06','07',
    '08','09','10','11','12');

# Get the current time and format the hour, minutes and seconds.  Add    #
# 1900 to the year to get the full 4 digit yeaAr.                         #
my ($sec,$min,$hour,$mday,$mon,$year) = (localtime(time))[0,1,2,3,4,5];
my ($sec1,$min1,$hour1,$mday1,$mon1,$year1) = (localtime($timestamp))[0,1,2,3,4,5];
my $time = sprintf("%02d:%02d:%02d",$hour,$min,$sec);
$year1 += 1900;
$year  += 1900;
# Format the date.


$a=time;

$b=$timestamp;
$returnvar= (($b - $a)/86400);
#print p($returnvar);
my $date  = "$year-$months[$mon]-$mday";
my $date1 = "$year1-$months[$mon1]-$mday1";
return $returnvar;
}
else
{
return;
}
}
