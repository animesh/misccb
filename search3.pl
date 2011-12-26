#!/opt/gnu/bin/perl

#############################################################################
# SearchWWW for Solaris and Linux and AIX.                      Version 1.6 #
#                                                                           #
# Copyright © 1997 Bill Gilmore                      Last Modified 04/02/97 #
#                                                                           #              
# Email questions or comments to billg@wcc.net                              #
#############################################################################
# SearchWWW is a simple script that takes input from a html and submits it  #
# to a search engine.  The there are two input variable location (the name  #
# of the search engine) and search (this is the string you want to search   #
# for)                                                                      #  
#                                                                           #
# You must have access to Perl 5 and be able to run a cgi application.      #
# Program might require minor modifications in order to get it to compile   #
# on your platform.                                                         #
#############################################################################
# This program is free software; you can redistribute it and/or             #
# modify it under the terms of the GNU General Public License               #
# as published by the Free Software Foundation; either version 2            #
# of the License, or (at your option) any later version.                    #
#                                                                           #
# This program is distributed in the hope that it will be useful,           #
# but WITHOUT ANY WARRANTY; without even the implied warranty of            #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             # 
# GNU General Public License for more details.                              #
#                                                                           #
# You should have received a copy of the GNU General Public License         #
# along with this program; if not, write to the Free Software               #
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.                 #
#############################################################################

# @referers allows forms to be located only on servers which are defined 
# in this field.  This fixes a security hole in the last version which 
# allowed anyone on any server to use the SearchWWW script.

@referers = ('http://www.wcc.net','http://wcc2.wcc.net');

# Check Referring URL
&check_url;

# Parse Form Contents
&parse_form;

# Submit to search engine
&search;

sub check_url {

   if ($ENV{'HTTP_REFERER'}) {
      foreach $referer (@referers) {
         if ($ENV{'HTTP_REFERER'} =~ /$referer/i) {
	    $check_referer = '1';
	    last;
         }
      }
   }
   else {
      $check_referer = '1';
	}

	if ($check_referer != 1) {
	   &error('bad_referer');
	}

}

sub parse_form {

	if ($ENV{'REQUEST_METHOD'} eq 'GET') {
      # Split the name-value pairs
      @pairs = split(/&/, $ENV{'QUERY_STRING'});
	}
   elsif ($ENV{'REQUEST_METHOD'} eq 'POST') {
      # Get the input
		read(STDIN, $buffer, $ENV{'CONTENT_LENGTH'});
 
      # Split the name-value pairs
		@pairs = split(/&/, $buffer);
	}
	else {
		&error('request_method');
	}

	foreach $pair (@pairs) {
		($name, $value) = split(/=/, $pair);
		$value =~ tr/+/ /;
		$value =~ s/%([a-fA-F0-9][a-fA-F0-9])/pack("C", hex($1))/eg;

		# If they try to include server side includes, erase them, so they
		# arent a security risk if the html gets returned.  Another
		# security hole plugged up.
		$value =~ s/<!--(.|\n)*-->//g;

		# Create an associative array
		$contents{$name} = $value;
   }

}

sub search {

# Assign the value from search to search_for
$search_for = $contents{'search'};

# Places a plus between two words
$search_for =~ tr/ /+/;

# If no engine entered goto lycos by default
if ($contents{'location'} eq "99")
	 {
	 print "Location: http://www.lycos.com/cgi-bin/pursuit?cat=lycos&query=$search_for&x=56&y=13\n\n";
	 }

# Search Alta Vista
if ($contents{'location'} eq "1")
	{
   print "Location: http://altavista.digital.com/cgi-bin/query?pg=q&what=web&fmt=d&q=$search_for\n\n";
	}

# Search Deja News
if ($contents{'location'} eq "2")
	{
	print "Location: http://search.dejanews.com/dnquery.xp?query=$search_for&defaultOp=AND&format=terse\n\n";
	}

# Search Excite
if ($contents{'location'} eq "3")
	{
	print "Location: http://www.excite.com/search.gw?search=$search_for&trace=a&collection=web\n\n";
	}

# Search Hotbot
if ($contents{'location'} eq "4")
	{
	print "Location: http://www.hotbot.com/search.html?_v=1.0&OP=0&SW=web&SM=MC&MT=$search_for&MOD=0&date=WH&DR=newer&DM=1&DD=1&DY=96&DV=10&DU=years&smiley=&RD=AN&RG=NA&domain=&DC=10&FJS=off&FJA=off&FRA=off&FVI=off&FAC=off&FSW=off&FVR=off&FSU=off&FSM=off&OP=0&MOD=0&FS=\n\n";
	}

# Search Infoseek
if ($contents{'location'} eq "5")
	{
	print "Location: http://guide-p.infoseek.com/Titles?qt=$search_for&col=WW&sv=IS&lk=noframes\n\n";
	}

# Search Lawcrawler
if ($contents{'location'} eq "6")
	{
	print "Location: http://www.lawcrawler.com/cgi-bin/lawcrawler?entry=$search_for&sites=legal&context=top\n\n";
	}

# Search Lycos
if ($contents{'location'} eq "7")
	{
	print "Location: http://www.lycos.com/cgi-bin/pursuit?cat=lycos&query=$search_for&x=56&y=13\n\n";
	}

# Search Magellen
if ($contents{'location'} eq "8")
	{
	print "Location: http://searcher.mckinley.com/searcher.cgi?query=$search_for&q.x=551&q.y=50\n\n";
	}

# Search Metacrawler
if ($contents{'location'} eq "9")
	{
	print "Location: http://www.metacrawler.com/cgi-bin/nph-metaquery?general=$search_for&method=0&type=Fast+Search&sort=relevance&ltarget=window&useFrames=1&iface=int1\n\n";
	}

# Search Webcrawler
if ($contents{'location'} eq "10")
	{
	print "Location: http://query.webcrawler.com/cgi-bin/WebQuery?searchtext=$search_for&maxHits=25\n\n";
	}

# Search Yahoo!
if ($contents{'location'} eq "11")
	{
	print "Location: http://search.yahoo.com/bin/search?p=$search_for\n\n";
	}


# Search Shareware-Macintosh
if ($contents{'location'} eq "12")
	{
	print "Location: http://search.shareware.com/code/engine/Find?logop=and&cfrom=quick&orfile=True&hits=25&search=$search_for&category=Macintosh\n\n";
	}

# Search Shareware-Windows
if ($contents{'location'} eq "13")
	{
	print "Location: http://search.shareware.com/code/engine/Find?logop=and&cfrom=quick&orfile=True&hits=25&search=$search_for&category=MS-Windows%28all%29\n\n";
	}

exit;
}
sub error {

	($error,@error_fields) = @_;

	print "Content-type: text/html\n\n";

	if ($error eq 'bad_referer') {
		print "<html>\n <head>\n  <title>Bad Referrer - Access Denied</title>\n </head>\n";
		print " <body>\n  <center>\n   <h1>Bad Referrer - Access Denied</h1>\n  </center>\n";
		print "The form that is trying to use this SearchWWW Program</a>\n";
		print "resides at: $ENV{'HTTP_REFERER'}, which is not allowed to access this cgi script.<p>\n";
		print "Sorry!\n";
		print "</body></html>\n";
	}

	elsif ($error eq 'request_method') {
		print "<html>\n <head>\n  <title>Error: Request Method</title>\n </head>\n";
		print "</head>\n <body>";
		print "<center>\n\n";
		print "<h1>Error: Request Method</h1>\n  </center>\n\n";
		print "The Request Method of the Form you submitted did not match\n";
		print "either GET or POST.  Please check the form, and make sure the\n";
		print "method= statement is in upper case and matches GET or POST.\n";
		print "</body></html>\n";
	}

	elsif ($error eq 'no_engine') {
		print "<html>\n<head>\n<title>Error: No Search Engine</title>\n</head>\n";
		print "<body bgcolor=#FFFFFF text=#00007F>\n";
		print "<b><h1>ERROR:  You must choose a search engine.</h1></b>";
		print "</body>\n</html>";
	}
exit;
}