#!@WHICHPERL@

##
## $Id: job-status.pl 176 2005-08-19 22:55:31Z nadya $
##
## $Log$
## Revision 1.1  2005/08/19 22:55:31  nadya
## initial revision
##
##

print "Content-type:text/html\n\n";

#form data
read(STDIN, $buffer, $ENV{'CONTENT_LENGTH'});
@pairs = split(/&/, $buffer);
foreach $pair (@pairs) {
    ($name, $value) = split(/=/, $pair);
    $value =~ tr/+/ /;
    $value =~ s/%([a-fA-F0-9][a-fA-F0-9])/pack("C", hex($1))/eg;
    $FORM{$name} = $value;
}

#get form data
$submit = "$FORM{'submit'}";
$jobid = "$FORM{'jobid'}";
#$email = "$FORM{'email'}";

if($submit) {

	$error_flag = 0;
	if(!$jobid) {
		print "<font color=red><b>Error: Missing Job ID</b></font><br>";
		$error_flag++;
	}
	if($jobid =~ '\D') {
		print "<font color=red><b>You may only enter numbers.</b></font><br>";
		$error_flag++; 
	}	
	#if(!$email) {
	#	print "<font color=red><b>Error: Missing email address</b></font><br>";
	#	$error_flag++;
	#}
	if($error_flag > 0) {
		print "<font color=red><b>Please hit back and try again</b></font><br>";
		exit(0);
	}

	@jobinfo = `grep $jobid /var/www/html/meme/meme/LOGS/meme.startup.log`;

	if(@jobinfo != 0) {

		$job = pop(@jobinfo);

 		@queueid = split('\s+', $job);

		$queueline = `/opt/gridengine/bin/glinux/qstat | grep @queueid[2]`;

		if($queueline) {
		
			print "<center><b>Your job is still processing</b></center>";
			exit(0);

		}
		else {
			$ls = `ls /var/www/html/meme/meme/LOGS/meme.$jobid.results`;
			if(!$ls) {
				print "<center><b>Your job has already successfully completed</b></center>";
			}
			else {
				print "<center><b>Your job did not successfully complete. ".
				      " Please try resubmitting it or email <a href=mailto:meme\@nbcr.net>meme\@nbcr.net</a>.  Thank you.";
				exit(0);  
			}
		}
	}
	else {
		print "<center><font color=red><b>No job by that ID found</b></font></center>";
		exit(0); 
	}

}


