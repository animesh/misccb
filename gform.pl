#!/usr/bin/perl

########################################################################################
########################################################################################

# Get the form variables

	if ($ENV{'REQUEST_METHOD'} eq 'GET') {
        	$buffer = $ENV{'QUERY_STRING'};
	}	
	else {
        	read(STDIN, $buffer, $ENV{'CONTENT_LENGTH'});
	}

# Break em up into a format the script can read

	@pairs = split(/&/, $buffer);
	foreach $pair (@pairs) {
        	($name, $value) = split(/=/, $pair);
        	$value =~ tr/+/ /;
        	$value =~ s/%([a-fA-F0-9][a-fA-F0-9])/pack("C", hex($1))/eg;
        	$FORM{$name} = $value;
	}

########################################################################################
########################################################################################

$from_name = $FORM{'from_name'};
$to_email = $FORM{'to_email'};
$from_email = $FORM{'from_email'};
$subject = $FORM{'subject'};
$body = $FORM{'body'};
$redirect = $FORM{'redirect'};

$mailprogram = "/usr/sbin/sendmail";

$homepage = "http://www.corpbrain.com";
$scripturl = "http://www.corpbrain/perl/gform.cgi";

# Change to 'no' if you don't want to send a autoreply to the sender
$autoreply = "yes";



##############################################################


if ($FORM{'from_name'} eq "") { 

print "Content-type: text/html\n\n";
print "error, no from name\n";
exit;

}

if ($FORM{'to_email'} eq "") { 

print "Content-type: text/html\n\n";
print "error, no to email\n";
exit;

}

if ($FORM{'from_email'} eq "") { 

print "Content-type: text/html\n\n";
print "error, no from email\n";
exit;

}

if ($FORM{'subject'} eq "") { 

print "Content-type: text/html\n\n";
print "error, no subject\n";
exit;

}

if ($FORM{'body'} eq "") { 

print "Content-type: text/html\n\n";
print "error, no body message\n";
exit;

}

if ($FORM{'redirect'} eq "") { 

print "Content-type: text/html\n\n";
print "error, no redirect url\n";
exit;

}


open (MAIL,"|$mailprogram -t");
               	print MAIL "To: $to_email\n";
		print MAIL "From: $from_email\n";
		print MAIL "Subject: $subject\n";
		print MAIL "$body\n";
	close(MAIL);


if ($autoreply eq "yes") {

open (MAIL,"|$mailprogram -t");
               	print MAIL "To: $from_email\n";
		print MAIL "From: $to_email\n";
		print MAIL "Subject: RE:$subject\n";
		print MAIL "Dear $from_name,\n\n";
		print MAIL "Thanks for your message.\n";
		print MAIL "We will respond to your message A.S.A.P\n\n";
		print MAIL "$homepage\n";
	close(MAIL);

}

print "Location: $redirect\n\n";

exit;