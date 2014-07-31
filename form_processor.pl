#!/usr/bin/perl
####################################################### 
#form_processor.pl 1.0                                
#                                                     
#by Command-O Software                                
#http://www.command-o.com                             
#Programmer: Kendall Comey                             
#For support, write: support@command-o.com            
#Copyright, 1997, All Rights Reserved                 
#                                                     
#By using this script Licensee agrees to all terms of 
#the License Agreement.  
#
#If you find this script useful, we would appreciate a 
#donation to keep this service active for the community.
#Command-O Software, P.O. Box 12200, Jackson WY 83002                             #
#######################################################
#Require the cgi library 
require './cgi-lib.pl';

#Turn off Perl buffering
$| = 1;

&ReadParse(*input);

########################################################
# Client Customization Variables

#Option No. 1
$email_admin = 0; #1 is on; 0 if off;

#Option No. 2
$write_form_info = 1; #1 is on; 0 is off;
#Path of file to write form data to. Must be read/writeable
$form_info_path = "/usr/local/etc/httpd/htdocs/demos/feedback_archive.txt";

#Option No. 3
$email_datafile = 1; #1 is on; 0 is off;
#Path to email addresses datafile
$email_datafile_path = "/usr/local/etc/httpd/htdocs/demos/feedback_emails.txt";

#Option No. 4
$send_email_to_user = 1; #1 is on; 0 is off;
#Path of text file to be sent to user
$feedback_response_path = "/usr/local/etc/httpd/htdocs/demos/feedback_response.txt";

#Option No. 5
$include_field_name = 1; #1 is on; 0 if off;

#Subject of email sent to user
$email_subject = "Thank You for Visiting Freescripts";

#Path to sendmail
$mailprog = "/bin/sendmail -t";

$date_command = "/usr/bin/date";

#You should not need to change anything beyond this point

$date = `$date_command +"%D"`; 

#Go through the required fields and send to user an error message if field is empty
while (($key, $value) = each %input)	{
if ($key =~ /required-/ && $value eq "")	{
$key =~ s/^\d*\)*required\-*//g;
print &PrintHeader;
print "The $key field is required. Please use the back button on your browser to ";
print "enter the required information and submit the form again. Thank you.";
exit;
}
}

#Take all the fields that begin with a number, sort them by number and put them into #the sortedkeys array
foreach $key (sort(keys(%input)))	{
if ($key =~ /user_email/)	{
$user_email = "$input{$key}";
$user_email =~ s/^\d*\)\s*//;
$user_email =~ s/required-//;
}
unless ($key =~ /^\d+/)	{
next;
}
push (@sortedkeys, "$key|$input{$key}");
}

#Send email to admin if selected
if ($email_admin == 1)	{
&email_admin;
}
#Send text file to user if selected
if ($send_email_to_user == 1)	{
&send_info;
}
#Write form data to file if selected
if ($write_form_info == 1)	{
&write_form_info;
}
#Write email addresses to datafile if selected
if ($email_datafile == 1)	{
&write_email_datafile;
}

sub write_email_datafile	{
open (EMAIL, ">>$email_datafile_path") || die ("I am sorry, but I was unable to open the file $email_datafile_path");
print EMAIL "$user_email\n";
}

sub write_form_info	{
open (FORM, ">>$form_info_path") || die ("I am sorry, but I was unable to open the file $form_info_path");
foreach $sortedkeys (@sortedkeys)	{
$sortedkeys =~ s/^\d*\)\s*//;
$sortedkeys =~ s/required-//;
($name, $answer) = split (/\|/, $sortedkeys);
print FORM "$name -- $answer\n";
}
print FORM "\n";
close (FORM);
}

sub send_info	{
if ($user_email =~ /^[\w-.]+\@[\w-.]+$/)	{
open (MAIL, "|$mailprog $user_email") || die "Can't open $mailprog!\n";
print MAIL "To: $user_email\n";
print MAIL "From: $input{'admin'}\n";
print MAIL "Subject: $email_subject\n\n";
open (PAGE, "$feedback_response_path") || die ("I am sorry, but I was unable to open the file $feedback_response_path.");
			while (<PAGE>)		{
						print MAIL $_;
			}
			close (PAGE);
close (MAIL);
}
}

sub email_admin	{
if ($input{'admin'} =~ /^[\w-.]+\@[\w-.]+$/)	{
open (MAIL, "|$mailprog $input{'admin'}") || die "Can't open $mailprog!\n";
print MAIL "To: $input{'admin'}\n";
print MAIL "From: $user_email\n";
print MAIL "Subject: $input{'subject'}\n\n";
foreach $sortedkeys (@sortedkeys)	{
$sortedkeys =~ s/^\d*\)\s*//;
$sortedkeys =~ s/required-//;
($name, $answer) = split (/\|/, $sortedkeys);
if ($include_field_name == 1)	{
print MAIL "$name -- $answer\n";
}
else	{
print MAIL "$answer\n";
}
}
close (MAIL);
}
}

#Send them to the proper URL
print "Location: $input{'redirect'}\n\n";

