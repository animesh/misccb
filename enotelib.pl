#  Perl 5 required.
require 5.000;

$main::version = "EN 1.12";
$main::copyright = "Copyright (C) 1996-2003 Oak Ridge National Laboratory";

#  version EN 1.12
#  a-changes January 2, 2003
#    added pjpeg, x-png to image upload types to match new web servers 
#    added subscription and daily notification feature of changed pages
#    added option to print all or a set of pages
#    fix search function to always search author, title, date, keywords
#    added platform independant emailing, email change notification pooling,
#    remove java applet sketchpad because of interface problems with new browsers
#  b-changes January 23, 2003
#    fixed a bug in subscription feature that only affects January
#
#  version EN 1.11
#  a-changes September 18 2001
#    added "showtags" tag to allow XML input onto a notebook page.
#    displays all tags between <enote:showtags> and </enote:showtags>
#  b-changes September 22 2001
#    modified table of contents view to improve layout
#    and last modified date to include addendums
#
#  Previous modifications
#  version EN 1.10
#  a-changes Feb 1 1998
#    edit and delete checks if page notarized
#    annotate sketchpad works like edit sketchpad
#    annotate and image upload leave mime type unchanged
#    notarize date placed in history and marked at end of page
#    notarize reloads updated page.
#  b-changes Feb 10 1998
#    sketchpad points to correct notebook in multi-notebook setups
#  c-changes Feb 25 1998
#    fixed typo causing annotate to be active in read-only mode
#    edit now correctly uploads plain and html files
#  d-changes April 21 1998
#    added ability to turn off control panel
#  e-changes May 8 1998
#    added ability to add captions to uploaded images in ADD or EDIT
#  f-changes June 11 1998  (thanks to Martin Beaudoin)
#    added code to fix large-file upload bug in some NT-Perl versions
#    added a confirm dialog to notarize and delete buttons
#  g-changes May 14 1999
#    added last-modified date to TOC entries
#  h-changes April 14 2000
#    added ability to turn off notarize
#  i-changes June 26 2001
#    added fix to strip LF out of page title 
#    (contributed by Gerhard Heinzel)

#  Strict syntax checking.
use strict;

# use for sending email, if $main::useSendmailCommand is 0
use Sendmail;


# Add this for wrapping plain text on 80 characters 
# (gave errors so commented out)
#use Text::Wrap qw(wrap $columns);
#$columns = 80;


=head1 NAME

ENoteLib - electronic notebook shared library routines

=head1 SYNOPSIS

    require 'enotelib.pl';

=head1 DESCRIPTION

=head1 AUTHOR

Noel Nachtigal E<lt>F<santa@msr.epm.ornl.gov>E<gt>
Al Geist E<lt>F<geist@msr.epm.ornl.gov>E<gt>
David Jung E<lt>F<jungdl@ornl.gov>E<gt>

=cut

###############################################################################
#
#  Miscellaneous global flags and variables.
#
###############################################################################

#  File locking constants.
$main::LOCK_SH = 1;			# Shared lock
$main::LOCK_EX = 2;			# Exclusive lock
$main::LOCK_NB = 4;			# Non-blocking lock
$main::LOCK_UN = 8;			# Unlock

#  Return codes.
$main::RET_NO_ERROR = 0;		# No error
$main::RET_BAD_SRVR = 1;		# Bad server environment
$main::RET_BAD_RQST = 2;		# Bad HTTP request
$main::RET_BAD_NTBK = 3;		# Invalid notebook requested
$main::RET_BAD_PAGE = 4;		# Invalid page or object number
$main::RET_BAD_FILE = 5;		# File error
$main::RET_BAD_INVOKE = 6;              # Unknown/unsupported invocation environment

###############################################################################
#
#  This is main.
#
###############################################################################

sub main {


# First, figure out if we're being executed as a web request via
#  CGI, or from the command line/batch script

#  Get the request method
  $main::method = uc( $ENV{'REQUEST_METHOD'} );
  $main::servername = "$ENV{'SERVER_NAME'}";

  if ( ( "$main::method" eq '') || ( "$main::servername" eq '') ) {
      $main::invoked = 'command';
  }
  else {
      $main::invoked = 'webserver';
  }

  # mark the %main::notebook's so that we know which belong to the
  #  enote.pl we were invoked from (as others will be added for
  #  various reasons - such as change notification processing)
  my $notebook = '';
  foreach $notebook ( keys %main::notebook ) {
      $main::notebook{$notebook}{'native'} = 1;
  }


  if ( "$main::invoked" eq 'webserver' ) {  return &webmain(); }
  elsif ( "$main::invoked" eq 'command' ) { return &commandmain(); }
  
  #error (output standard http header in-case it was a browser invocation)
  print <<EOF;
Content-type: text/plain

Unknown/unsupported invocation environment.  Only command-line and CGI supported.\n
EOF
  exit $main::RET_BAD_INVOKE;
}



###############################################################################
#
#  Handle command-line/script invocation
#
###############################################################################

sub commandmain {
    print "Electronic notebook version $main::version\n$main::copyright\n";


    # some defaults
    if ("$main::subscription" eq '') {
	$main::subscription = '/var/www/html/subscriptions.ens';
    }
    if ("$main::subscriptionReplyTo" eq '') {
	$main::subscriptionReplyTo = 'E-Note <NoReturnAddress@nowhere.com>';
    }
    if ("$main::sendmail" eq '') {
	$main::sendmail = "/usr/sbin/sendmail"; 
    }

    # Currently the only command line action is to process update notification e-mail's
    print "Checking for notebook updates...\n";
    my $anychanges = notifySubscribers();

    if ($anychanges == 1) {
	print "Subscribed users notified of updates to notebooks: ";
	my $notebook = '';
	foreach $notebook ( keys %main::notebook ) {
	    if ($main::notebook{$notebook}{'native'} == 1) {
		print $notebook." ";
	    }
	}
	print " (if any).\n";
    }
    else {
	print "No changes to notebooks since last execution.\n";
    }


    exit $main::RET_NO_ERROR;
}


###############################################################################
#
#  Handle requests via the web server
#
###############################################################################

sub webmain {

#  Build the URL for the notebook script.
  if ( "$main::servername" eq '' ) {
    print <<EOF;
Content-type: text/html

<html>
<head>
<title>Invalid Environment</title>
</head>
<body background="$main::backgrnd">
<h1>Invalid Server Environment</h1>
<p><b>The server accessed does not provide an environment that is fully
compliant with the CGI protocol.<br>
Please contact the Web server administrator for further details.</b><br>
Please use your browser to go back.<hr> 
</body>
</html>
EOF
    exit $main::RET_BAD_SRVR;
}


  my $https = ( "$ENV{'HTTPS'}" ne '' ) ? 's' : '';
  my $port = "$ENV{'SERVER_PORT'}";
  if ( "$port" eq "80" ) {
    $https = ''; $port = '';
  }
  elsif ( "$port" eq "443" ) {
    $https = 's'; $port = '';
  }
  my $name = $main::servername;

  $main::relscript = $main::script; #relative script (without http://server/)

  if ( "$name" ne '' ) {
    $name = "//$name" . (( "$port" ne '' ) ? ":$port" : '' );
  }

  $main::script = "http$https:$name$main::script";

# only accept GET and POST web requests.
  if ( $main::method !~ m/^(GET|POST)$/ ) {
    $main::method = '(null)' if ( "$main::method" eq '' );
    print <<EOF;
Content-type: text/html

<html>
<head>
<title>Communication Error</title>
</head>
<body background="$main::backgrnd">
<h1>Communication Error</h1>
<p><b>Your browser sent me a request I do not understand.
I was expecting a GET or a POST, and I got a "$main::method".</b><br>
Please use your browser to go back.<hr>
</body>
</html>
EOF
    exit $main::RET_BAD_RQST;
  }

#  Parse the parameters.
  &ParseCGIParms();

#  Extract the notebook name and info.
  $main::nb = $main::in{'nb'} if ( "$main::in{'nb'}" ne '' );
  $main::nb = lc( $main::nb );
  if ( ! $main::notebook{$main::nb} ) {
    print <<EOF;
Content-type: text/html

<html>
<head>
<title>Invalid Notebook</title>
</head>
<body background="$main::backgrnd">
<h1>Invalid Notebook</h1>
<p><b>The notebook requested ($main::nb) cannot be found or is not accessible.</b><br>
Please use your browser to go back.<hr>
</body>
</html>
EOF
    exit $main::RET_BAD_NTBK;
  }
  $main::root	 = $main::notebook{$main::nb}{'dir'};
  $main::title	 = $main::notebook{$main::nb}{'title'};
  
#  Make sure the index file exists and is OK.
  $main::idxfile = "$main::root/index.lst";
  if ( ! -e $main::idxfile ) {
    open( IDX, ">$main::idxfile" ) || &InvalidFile($main::idxfile);
    close( IDX );
  }

#debugging output (set to 1 to show the important vars on the generated output)
  my $debugvars = 0;
  if ( $debugvars == 1) {
      print <<EOF;
Content-type: text/html

main::in{'notebook'} = $main::in{'notebook'}<br>
main::in{'page'} = $main::in{'page'}<br>
main::in{'action'} = $main::in{'action'}<br>
main::in{'method'} = $main::in{'method'}<br>
main::in{'postpage'} = $main::in{'postpage'}<br>
main::in{'submit'} = $main::in{'submit'}<br>
main::in{'search'} = $main::in{'search'} : (main::in{'everything'} = $main::in{'everything'})<br>
EOF
  }


#  Extract the global page number; default is 'last'.
  $main::page = ( "$main::in{'page'}" eq '' ) ? 'last' :
		lc( $main::in{'page'} );

#  Extract the action --- it's case-insensitive, default is 'view'.
  my $action = ( "$main::in{'action'}" eq '' ) ? 'view' :
	       lc( $main::in{'action'} );

#  Extract method (if any) - a kind of 'subaction'
  my $method = ( "$main::in{'method'}" eq '' ) ? 'ask_contents' :
      lc ( $main::in{'method'} );

  $action = 'view' if
    ( $action !~ m/^(add|annotate|delete|edit|notarize|search|toc|view|print|subscription)$/ );

  if ("$action" eq 'subscription') {
      $method = 'subscription' if
	  ( $method !~ m/^(subscribe|unsubscribe)$/ );
  }

  $method = 'ask_contents' if
      ( $method !~ m/^(page|ask_contents|contents|notebook|ask_search|search|search_pages|subscription|unsubscription|subscribe|unsubscribe)$/ );

#  If 'cancel', then we really want just 'view'.
  $action = 'view' if ( $main::in{'cancel'} );


# Switch POST and regular GET requests to implement search printing:
#  (Search Print selection page uses POST to re-transmit the search criteria)

# A POST from the 'ask_search' page uses input type=image buttons to indicate 
#  the 'print' action and 'method'
  if ( ( "$main::in{'postpage'}" eq 'ask_search' ) ) {
      if ( "$main::in{'print_method_search.x'}" ne '') {
	  $action = 'print';
	  $method = 'search';
      }
      if ( "$main::in{'print_method_search_pages.x'}" ne '') {
	  $action = 'print';
	  $method = 'search_pages';
      }
  }

# A POST from the Subscription page is either a subscribe or an unsubscribe
  if ( ( "$main::in{'postpage'}" eq 'subscription' ) ) {
      $action = 'subscription';
      $method = 'subscribe';
      if ("$main::in{'submit'}" eq 'Unsubscribe') {
	  $method = 'unsubscribe';
      }
  }




#  print <<EOF;
#Content-type: text/html
#
#<html>
#<head>
#</head>
#<body background="$main::backgrnd">
#<h3>DEBUG - IGNORE.</h3>
#<p>action=$action method=$method page=$main::page submit=$main::in{'submit'} submit2=$main::in{'submit2'}
#  print=$main::in{'print'}<p>
#</body>
#</html>
#EOF

  # Action dispatch
  if	( "$action" eq 'add' )	    { &ActionAdd(); }
  elsif ( "$action" eq 'annotate' ) { &ActionAnnotate(); }
  elsif ( "$action" eq 'delete' )   { &ActionDelete(); }
  elsif ( "$action" eq 'edit' )     { &ActionEdit(); }
  elsif ( "$action" eq 'notarize' ) { &ActionNotarize(); }
  elsif ( "$action" eq 'search' )   { &ActionSearch(); }
  elsif ( "$action" eq 'toc' )	    { &ActionToc(); }
  elsif ( "$action" eq 'view' )     { &ActionView(); }
  elsif ( "$action" eq 'print' )    { &ActionPrint( $method ); }
  elsif ( "$action" eq 'subscription' ){ &ActionSubscription( $method ); }

  exit $main::RET_NO_ERROR;
}

###############################################################################
#
#  Primary notebook function routines
#
###############################################################################

###############################################################################
#
#  ( ) = &ActionAdd();
#
#  Adds an entry to the notebook.
#
###############################################################################

sub ActionAdd {

#  Extract info if available.
  my ( $author, $body, $kwords, $mimetype, $title ) = ( '', '', '', '' );
  my ( $caption ) = ('');
  my $isentry = 1;

  if ( "$main::in{'reset'}" ne '' ) {
    $main::method = 'GET';
  }
  else {
    $author   = "$main::in{'author'}"; $author =~ s/"/&quot;/g;
    $kwords   = lc( "$main::in{'keywords'}" ); $kwords =~ s/"/&quot;/g;
    $mimetype = "$main::mimetypes{$main::in{'mimetype'}}";
    $mimetype = "$main::up{'upfile'}{'mime'}"
      if ( "$main::up{'upfile'}{'mime'}" ne '' );
    $isentry  = ( "$mimetype" =~ m;^text/(html|plain)$; );
    $title    = "$main::in{'title'}"; $title =~ s/"/&quot;/g; $title =~ s/\n/ /g;
    $caption  = "$main::in{'caption'}";
    if ( $isentry ) {
      $body = "$main::in{'body'}"; $body =~ s/\r/ /g; $body =~ s/\0//; #"
    }
    $body = $main::up{'upfile'}{'data'} if ( $main::up{'upfile'}{'size'} > 0 );
  }

#  GET:  we have either a new form, or a cancelled form.
#  In either case, put up a fresh form.
  if ( "$main::method" eq 'GET' ) {
    print <<EOF;
Content-type: text/html

<html>
<head>
<title>Add a Notebook Entry</title>
</head>
<body background="$main::backgrnd">
$main::linehorz

<h2>Add a Notebook Entry to a New Page</h2>
<p>
This form allows the user to choose from three different 
input methods. <br>
<b>Text/HTML,</b> or <b> Image upload</b>, or <b>File  upload</b>.
</p>
Fill out <b>Name</b> and <b>Title</b> (required).

EOF
  }

#  POST: we have a submitted form, either a notebook entry or an object.
#  It might be incomplete.
  elsif ( $author && $body && $mimetype && $title ) {	# Complete, save it

    if ( ! $isentry ) {				# Notebook object
      my $date = time;
      my $strdate = gmtime( $date );
      $date = substr( $date, 0, length( $date ) - 3 ) . '.' .
	      substr( $date, -3 );
      my $filename = "$main::root/$date";
      open( FILE, ">$filename" ) || &InvalidFile();
      binmode( FILE );
      print FILE $body;
      close( FILE );
      chmod 0666, ( $filename );
      my $objs = 0;
      open( IDX, "<$main::idxfile" ) || &InvalidFile();
      flock( IDX, $main::LOCK_EX ) if ( $main::flock );
      seek( IDX, 0, 0 );
      while ( <IDX> ) {
	next if ( m/^\s*$/ );			# Blank lines
	next if ( m/^\s*#/ );			# Comment lines
	my ( $tag ) = split;
	$objs++ if ( $tag < 0 );
      }
      $objs++;
      flock( IDX, $main::LOCK_UN ) if ( $main::flock );
      close( IDX );
      open( IDX, ">>$main::idxfile" ) || &InvalidFile();
      flock( IDX, $main::LOCK_EX ) if ( $main::flock );
      print IDX "-$objs $date $mimetype 0\n";
      flock( IDX, $main::LOCK_UN ) if ( $main::flock );
      close( IDX );
      sleep( 1 );
      if ( "$mimetype" =~ m;^image/(jpeg|pjpeg|gif|x-png|bmp)$; ) {
	$body = <<EOF;
<p>
<center>
<img src="$main::relscript?nb=$main::nb&action=view&page=-$objs">
<br><b> $caption </b>
</center>
</p>
EOF
      }
      else {
	$body = <<EOF;
<p>
Click <a href="$main::relscript?nb=$main::nb&action=view&page=-$objs">here</a>
to view the object ($caption) you uploaded.
</p>
EOF
      }
      $mimetype = 'text/html';
    }

# Notebook entry
#  First parse for special SHOWTAGS regions
    $body = &parseForXML( $body );

#  Make new filename from the time.
    my $date = time;
    my $strdate = gmtime( $date );
    $date = substr( $date, 0, length( $date ) - 3 ) . '.' .
	    substr( $date, -3 );
    my $filename = "$main::root/$date";
    open( FILE, ">$filename" ) || &InvalidFile();
    print FILE <<EOF;
<!-- Author: $author -->
<!-- Date: $strdate (GMT) -->
<!-- Title: $title -->
<!-- Keywords: $kwords -->
<!-- Changes: Added $strdate (GMT) $author | -->
$body
EOF
    close( FILE );
    chmod 0666, ( $filename );

#  Open the index file and count number of entries.
    my $pages = 0;
    open( IDX, "<$main::idxfile" ) || &InvalidFile();
    flock( IDX, $main::LOCK_EX ) if ( $main::flock );
    seek( IDX, 0, 0 );
    while ( <IDX> ) {
      next if ( m/^\s*$/ );			# Blank lines
      next if ( m/^\s*#/ );			# Comment lines
      my ( $tag ) = split;
      $pages++ if ( $tag > 0 );
    }
    $pages++;
    flock( IDX, $main::LOCK_UN ) if ( $main::flock );
    close( IDX );

#  Add new file to the index file.
    open( IDX, ">>$main::idxfile" ) || &InvalidFile();
    flock( IDX, $main::LOCK_EX ) if ( $main::flock );
    print IDX "$pages $date $mimetype 0\n";
    flock( IDX, $main::LOCK_UN ) if ( $main::flock );
    close( IDX );

#  Redirect the browser to the new entry.
    print "Location: $main::script?nb=$main::nb&action=view&page=last\n\n";
    exit $main::RET_NO_ERROR;
  }

  else {					# Incomplete entry, retry
    $body = '' if ( ! $isentry );
    if ( "$mimetype" ne '' ) {
      foreach ( sort keys %main::mimetypes ) {
	$main::def_type = "$_" if ( "$main::mimetypes{$_}" eq "$mimetype" );
      }
    }
    print <<EOF;
Content-type: text/html

<html>
<head>
<title>Incomplete Entry</title>
</head>
<body background="$main::backgrnd">
$main::linehorz

<h2>Incomplete Entry</h2>

Every notebook entry must have an author, a title, and some text.<br>
Please complete the missing parts.

EOF
  }

#  The rest of the 'Add' form.


  print <<EOF;
<form action="$main::script" enctype="multipart/form-data" method=post>
<p>
<pre>
<b>Your name</b>: <input type=text name=author value="$author"
 size=40>
<b>The title</b>: <input type=text name=title value="$title"
 size=40>
<b>Keywords</b> : <input type=text name=keywords value="$kwords"
 size=40> (optional)
</pre>
</p>
<p>
You can <b>type</b> or <b>cut and paste</b> text or HTML 
into the text area (it will scroll to accommodate the input):<br>
<textarea rows=15 cols=75 name=body>$body</textarea><br>
</p>

<p>
Or you can <b>upload</b> an <b>image</b> or <b>file</b> to a new page.
<br>The image will be centered and displayed with the specified caption,
if you choose an image MIME type below.
<br><b>Upload:</b> <input type=file name=upfile value="" size=20>
<br>Image Caption: <input type=text name=caption value="$caption" size=40> (optional)
</p>
<p>
<b>Choose the MIME type</b> of your input:
<SELECT NAME="mimetype">
EOF

  foreach ( sort keys %main::mimetypes ) {
    print '<OPTION';
    print ' SELECTED' if ( "$_" eq "$main::def_type" );
    print "> $_</OPTION>\n";
  }

  print <<EOF;
</SELECT>
</p>
<p>
Then submit your entry:<br>
<input type=submit name=submit value="Add this entry">
<input type=submit name=reset value="Start over">
<input type=submit name=cancel value="Cancel">
<input type=hidden name=action value="add">
<input type=hidden name=nb value="$main::nb">
<input type=hidden name=page value="$main::page">
</p>
</form>
</p>
</body>
</html>
EOF

  return;
}

###############################################################################
#
#  ( ) = &ActionAnnotate();
#
#  Annotates an entry in the notebook.
#  NEW - shows copy of the page and a text box inwhich to place annotation.
#
###############################################################################

sub ActionAnnotate {

#  Initialize variables.
  my ( $authorig, $body, $changes, $kwords, $mimetype, $title ) =
     ( '', '', '', '', '', '' );
  my ( $author, $text, $date ) = ( '', '', '' );
  my ( $base, $filename, $update ) = ( '', '', 0 );

#  Open index file and extract file name and mimetype
  open( IDX, "<$main::idxfile" ) || &InvalidFile();
  flock( IDX, $main::LOCK_EX ) if ( $main::flock );
  seek( IDX, 0, 0 );
  my @lines = <IDX>;
  foreach ( @lines ) {
    next if ( m/^\s*$/ );			# Blank lines
    next if ( m/^\s*#/ );			# Comment lines
    my ( $tag, $name, $type ) = split;
    if ( "$tag" eq "$main::page" ) {
      $base = "$name"; $mimetype = "$type"; last;
    }
  }
  flock( IDX, $main::LOCK_UN ) if ( $main::flock );
  close( IDX );

#  Set update if mimetype isn't text/html
  $filename = lc( $mimetype );
  $mimetype = "$main::mimetypes{$main::in{'mimetype'}}" if
    ( "$main::in{'mimetype'}" ne '' );
  $mimetype = lc( $mimetype );
  $update = ( "$filename" ne "$mimetype" );
  $filename = "$main::root/$base";

#  Open file extracting existing page content.
    open( FILE, "<$filename" ) || &InvalidFile();
    while ( <FILE> ) {
      if    ( m/^<!-- Author: (.*) -->$/ )   { $authorig = $1; }
      elsif ( m/^<!-- Date: (.*) -->$/ )     { $date = $1; }
      elsif ( m/^<!-- Title: (.*) -->$/ )    { $title = $1; }
      elsif ( m/^<!-- Keywords: (.*) -->$/ ) { $kwords = $1; }
      elsif ( m/^<!-- Changes: (.*) -->$/ )  { $changes = $1; }
      else { $body .= $_; }
    }
    close( FILE );

# Request for input page.
  if ( "$main::method" eq 'GET' ) {

#  Build header of normal input page
    print <<EOF;
Content-type: text/html

<html>
<head>
<title>Annotate $main::page</title>
</head>
<body background="$main::backgrnd"> 
<center>
<h2>Annotate page $main::page </h2>
<h3>Insert note at bottom and submit</h3>
</center>
$main::linehorz
EOF
  }
#  else assume POST: we have a submitted annotation form.
  else {
    $author   = "$main::in{'author'}"; $author =~ s/"/&quot;/g; #"
    $text     = "$main::in{'text'}"; $text =~ s/\r/ /g; $text =~ s/\0//;

    if ( $author && $text ) {	# Information complete. 
#  First parse for special SHOWTAGS regions
      $text = &parseForXML( $text );

#  Append annotation to existing file.
#  Update Last Modified change history.
      my $strdate = gmtime( time );
      open( FILE, ">$filename" ) || &InvalidFile();
      print FILE <<EOF;
<!-- Author: $authorig -->
<!-- Date: $strdate (GMT) -->
<!-- Title: $title -->
<!-- Keywords: $kwords -->
<!-- Changes: Annotated $strdate (GMT) $author | $changes -->
$body
<p><hr>
<font color=RED><b><u>Addendum</u>:</b></font>&nbsp;&nbsp; <b>$author</b> &nbsp; Date: <b>$strdate</b> (GMT) <br>
$text
<hr>
EOF
      close( FILE );
      chmod 0666, ( $filename );

#  If the MIME type changed, update the index file.
      if ( $update ) {
        open( IDX, "<$main::idxfile" ) || &InvalidFile();
        flock( IDX, $main::LOCK_EX ) if ( $main::flock );
        seek( IDX, 0, 0 );
        my @lines = <IDX>;
        flock( IDX, $main::LOCK_UN ) if ( $main::flock );
        close( IDX );
        open( IDX, ">$main::idxfile" ) || &InvalidFile();
        flock( IDX, $main::LOCK_EX ) if ( $main::flock );
        seek( IDX, 0, 0 );
        foreach ( @lines ) {
          next if ( m/^\s*$/ );			# Blank lines
          next if ( m/^\s*#/ );			# Comment lines
          my ( $tag, $name, $type, $as_is ) = split;
          print IDX ( "$tag" eq "$main::page" ) ?
	    "$tag $base $type $as_is\n" : "$_";
        }
        flock( IDX, $main::LOCK_UN ) if ( $main::flock );
        close( IDX );
      }

#  Redirect the browser to the new entry.
      print <<EOF;
Location: $main::script?nb=$main::nb&action=view&page=$main::page

EOF
      exit $main::RET_NO_ERROR;
    }

# Incomplete entry, build retry header
    print <<EOF;
Content-type: text/html

<html>
<head>
<title>Incomplete Annotation</title>
</head>
<body background="$main::backgrnd">
$main::linehorz

<center><h2>Incomplete Annotation</h2></center>

An annotation must have an author and some text.<br>
Please press <b>Back</b> on your browser and<br>
complete the missing parts.
<br>
</body>
</html>
EOF
      exit $main::RET_NO_ERROR;
  }
#  Set the default MIME type.
  if ( "$mimetype" ne '' ) {
    foreach ( sort keys %main::mimetypes ) {
      $main::def_type = "$_" if ( "$main::mimetypes{$_}" eq "$mimetype" );
    }
  }

# Finish building the input page for both normal and incomplete entry cases.


#  Build the existing page body.
  $changes =~ s/\|/<br>/g;
  if ( "$changes" ne '' ) { print "\n<h4>$changes</h4>\n"; }
  else { print "\n<h4>$date</h4>\n"  if ( "$date" ne '' ); }
  print "\n<h4>Keywords: $kwords</h4>\n"     if ( "$kwords" ne '' );
  print "\n<center><h3>$title</h3></center>" if ( "$title" ne '' );
  print "\n<p>";
  print "\n<pre>"  if ( "$mimetype" eq 'text/plain' );
  #
  # Wrap plain text within 80 characters if text/plain data
  #if ( "$mimetype" eq 'text/plain' ){
  #  print "\n"; print wrap("", "", $body);
  #}
  #else {
    print "\n$body";
  #}
  print "\n</pre>" if ( "$mimetype" eq 'text/plain' );
  print "\n</p><p>";
 
# ADD FORM HERE
  print <<EOF;
<form action="$main::script" enctype="multipart/form-data" method=post>
<p>
<pre>
<b>Your name</b>: <input type=text name=author value="$author"
 size=40>
</pre>
</p>
<p>
You can <b>type</b> or <b>cut and paste</b> text or HTML 
into the text area (it will scroll to accommodate the input):<br>
<textarea rows=5 cols=75 name=text>$text</textarea><br>
</p>

<p>
Then submit your entry:<br>
<input type=submit name=submit value="Add this annotation">
<input type=submit name=cancel value="Cancel">
<input type=hidden name=action value="annotate">
<input type=hidden name=mimetype value="text/html">
<input type=hidden name=nb value="$main::nb">
<input type=hidden name=page value="$main::page">
</p>
</form>
</p>
</body>
</html>
EOF

  return;
}

###############################################################################
#
#  ( ) = &ActionDelete();
#
#  Deletes a notebook page.
#
###############################################################################

sub ActionDelete {

#  Open the index file and find our entry.
  open( IDX, "<$main::idxfile" ) || &InvalidFile();
  flock( IDX, $main::LOCK_EX ) if ( $main::flock );
  seek( IDX, 0, 0 );
  my @lines = <IDX>;
  flock( IDX, $main::LOCK_UN ) if ( $main::flock );
  close( IDX );
  open( IDX, ">$main::idxfile" ) || &InvalidFile();
  flock( IDX, $main::LOCK_EX ) if ( $main::flock );
  seek( IDX, 0, 0 );
  foreach ( @lines ) {
    next if ( m/^\s*$/ );			# Blank lines
    next if ( m/^\s*#/ );			# Comment lines
    my ( $tag, $name, $type, $as_is ) = split;
    print IDX ( ("$tag" eq "$main::page") && ($as_is == 0) ) ? "$tag\n" : "$_";
  }
  flock( IDX, $main::LOCK_UN ) if ( $main::flock );
  close( IDX );

#  Display deleted page.
  print <<EOF;
Location: $main::script?nb=$main::nb&action=view&page=$main::page

EOF

  return ;
}

###############################################################################
#
#  ( ) = &ActionEdit();
#
#  Edits an entry to the notebook.
#
###############################################################################

sub ActionEdit {

#  Extract info if available.

  if ( "$main::in{'reset'}" ne '' ) {
    print <<EOF;
Location: $main::script?nb=$main::nb&action=view&page=$main::page

EOF
    return;
  }

  my ( $author, $body, $changes, $kwords, $mimetype, $title ) =
     ( '', '', '', '', '', '' );
  my ( $base, $filename, $notarized, $update ) = ( '', '', 0, 0 );
  my ( $pagetype ) = ( '' );
  my ( $caption  ) = ( '' );


  open( IDX, "<$main::idxfile" ) || &InvalidFile();
  flock( IDX, $main::LOCK_EX ) if ( $main::flock );
  seek( IDX, 0, 0 );
  my @lines = <IDX>;
  foreach ( @lines ) {
    next if ( m/^\s*$/ );			# Blank lines
    next if ( m/^\s*#/ );			# Comment lines
    my ( $tag, $name, $type, $as_is ) = split;
    if ( "$tag" eq "$main::page" ) {
      $base = "$name"; $mimetype = "$type"; $notarized = $as_is; last;
    }
  }
  flock( IDX, $main::LOCK_UN ) if ( $main::flock );
  close( IDX );

  $filename = lc( $mimetype );
  $mimetype = "$main::mimetypes{$main::in{'mimetype'}}" if
    ( "$main::in{'mimetype'}" ne '' );
  $mimetype = "$main::up{'upfile'}{'mime'}" if
    ( "$main::up{'upfile'}{'mime'}" ne '' );
  $mimetype = lc( $mimetype );
  $update = ( "$filename" ne "$mimetype" );
  $filename = "$main::root/$base";

  if ( ("$main::method" eq 'GET') && ($notarized == 0)) {
    open( FILE, "<$filename" ) || &InvalidFile();
    while ( <FILE> ) {
      if    ( m/^<!-- Author: (.*) -->$/ )   { $author = $1; }
      elsif ( m/^<!-- Date: (.*) -->$/ )     { ; }
      elsif ( m/^<!-- Title: (.*) -->$/ )    { $title = $1; }
      elsif ( m/^<!-- Keywords: (.*) -->$/ ) { $kwords = $1; }
      elsif ( m/^<!-- Changes: (.*) -->$/ )  { $changes = $1; }
      else { $body .= $_; }
    }
    close( FILE );
    print <<EOF;
Content-type: text/html

<html>
<head>
<title>Edit a Notebook Entry</title>
</head>
<body background="$main::backgrnd">
$main::linehorz

<h2>Edit the Notebook Entry on This Page</h2>
<p>
This form allows the you to choose from three different
methods to modify this page. <br>
You can change or add too the existing <b>Text/HTML,</b><br>
or you can append an <b>Image</b> to the end of the page,<br>
or you can <b>upload a File</b> to the end of the page.
<p>
The <b>Name</b> and <b>Title</b> are required.

EOF
  } elsif ( ("$main::method" eq 'GET') && ($notarized == 1)) {
    print <<EOF;
Content-type: text/html

<html>
<head>
<title>Edit invalid</title>
</head>
<body background="$main::backgrnd">
$main::linehorz

<h2>Notarized Page Can Not be Edited</h2>
<p>
The requested page has been previously notarized.<br>
Notarized pages can not be edited, but they can be annotated.
</body>
</html>
EOF
  }


#  POST: we have a submitted form, either a notebook entry or an object.
#  It might be incomplete.
  else {
    $author   = "$main::in{'author'}"; $author =~ s/"/&quot;/g;
    $changes  = "$main::in{'changes'}"; $changes =~ s/"/&quot;/g;
    $kwords   = lc( "$main::in{'keywords'}" ); $kwords =~ s/"/&quot;/g;
    $title    = "$main::in{'title'}"; $title =~ s/"/&quot;/g; $title =~ s/\n/ /g;
    $caption  = "$main::in{'caption'}";
    $body     = "$main::in{'body'}"; $body =~ s/\r/ /g; $body =~ s/\0//;

    if (( "$mimetype" =~ m;^text/(html|plain)$; ) &&
    ( $main::up{'upfile'}{'size'} > 0 )) {
      $body .= $main::up{'upfile'}{'data'};
    }

    if (( "$mimetype" !~ m;^text/(html|plain)$; ) &&
	( $main::up{'upfile'}{'size'} > 0 )) {
      my $date = time;
      my $strdate = gmtime( $date );
      $date = substr( $date, 0, length( $date ) - 3 ) . '.' .
	      substr( $date, -3 );
      my $filename = "$main::root/$date";
      open( FILE, ">$filename" ) || &InvalidFile();
      binmode( FILE );
      print FILE $main::up{'upfile'}{'data'};
      close( FILE );
      chmod 0666, ( $filename );
      my $objs = 0;
      open( IDX, "<$main::idxfile" ) || &InvalidFile();
      flock( IDX, $main::LOCK_EX ) if ( $main::flock );
      seek( IDX, 0, 0 );
      while ( <IDX> ) {
	next if ( m/^\s*$/ );			# Blank lines
	next if ( m/^\s*#/ );			# Comment lines
      my ( $tag, $name, $type ) = split;
           if ( "$tag" eq "$main::page" ) {
           $base = "$name"; $pagetype = $type;
           }
	$objs++ if ( $tag < 0 );            # Count objects
      }
      $objs++;
      flock( IDX, $main::LOCK_UN ) if ( $main::flock );
      close( IDX );
      open( IDX, ">>$main::idxfile" ) || &InvalidFile();
      flock( IDX, $main::LOCK_EX ) if ( $main::flock );
      print IDX "-$objs $date $mimetype 0\n";
      flock( IDX, $main::LOCK_UN ) if ( $main::flock );
      close( IDX );
      sleep( 1 );
# Check if editor upload of Nob. If so get body and changes.
      if ( $main::in{'editor'} ne '') {
         open( IDX, "<$main::idxfile" ) || &InvalidFile();
         flock( IDX, $main::LOCK_EX ) if ( $main::flock );
         seek( IDX, 0, 0 );
         my @lines = <IDX>;
         foreach ( @lines ) {
         next if ( m/^\s*$/ );			# Blank lines
         next if ( m/^\s*#/ );			# Comment lines
         my ( $tag, $name, $type ) = split;
           if ( "$tag" eq "$main::page" ) {
           $base = "$name"; last;
           }
         }
         flock( IDX, $main::LOCK_UN ) if ( $main::flock );
         close( IDX );

         $filename = "$main::root/$base";

         open( FILE, "<$filename" ) || &InvalidFile();
         while ( <FILE> ) {
         if ( m/^<!-- Keywords: (.*) -->$/ ) { $kwords = $1; }
         elsif ( m/^<!-- Changes: (.*) -->$/ )  { $changes = $1; }
         else { $body .= $_; }
         }
         close( FILE );
      }
      if ( "$mimetype" =~ m;^image/(jpeg|pjpeg|gif|x-png|bmp)$; ) {
	$body .= <<EOF;
<p>
<center>
<img src="$main::relscript?nb=$main::nb&action=view&page=-$objs">
<br><b> $caption </b>
</center>
</p>
EOF
      }
      else {
	$body .= <<EOF;
<p>
Click <a href="$main::relscript?nb=$main::nb&action=view&page=-$objs">here</a>
to view the object ($caption) you uploaded.
</p>
EOF
      }
      $mimetype = $pagetype;
    }

    if ( $author && $body && $mimetype && $title ) {	# Complete, save it
#  First parse and replace special SHOWTAGS regions
    $body = &parseForXML( $body );

#  Write the new file out.
      my $strdate = gmtime( time );
      open( FILE, ">$filename" ) || &InvalidFile();
      print FILE <<EOF;
<!-- Author: $author -->
<!-- Date: $strdate (GMT) -->
<!-- Title: $title -->
<!-- Keywords: $kwords -->
<!-- Changes: Modified $strdate (GMT) $author | $changes -->
$body
EOF
      close( FILE );
      chmod 0666, ( $filename );

#  If the MIME type changed, update the index file.
      if ( $update ) {
        open( IDX, "<$main::idxfile" ) || &InvalidFile();
        flock( IDX, $main::LOCK_EX ) if ( $main::flock );
        seek( IDX, 0, 0 );
        my @lines = <IDX>;
        flock( IDX, $main::LOCK_UN ) if ( $main::flock );
        close( IDX );
        open( IDX, ">$main::idxfile" ) || &InvalidFile();
        flock( IDX, $main::LOCK_EX ) if ( $main::flock );
        seek( IDX, 0, 0 );
        foreach ( @lines ) {
          next if ( m/^\s*$/ );			# Blank lines
          next if ( m/^\s*#/ );			# Comment lines
          my ( $tag, $name, $type, $as_is ) = split;
          print IDX ( "$tag" eq "$main::page" ) ?
	    "$tag $base $mimetype $as_is\n" : "$_";
        }
        flock( IDX, $main::LOCK_UN ) if ( $main::flock );
        close( IDX );
      }

#  Redirect the browser to the new entry.
      print <<EOF;
Location: $main::script?nb=$main::nb&action=view&page=$main::page

EOF
      exit $main::RET_NO_ERROR;
    }

# Incomplete entry, retry
    print <<EOF;
Content-type: text/html

<html>
<head>
<title>Incomplete Entry</title>
</head>
<body background="$main::backgrnd">
$main::linehorz

<h2>Incomplete Entry</h2>

Every notebook entry must have an author, a title, and some text.<br>
Please complete the missing parts.

EOF
  }

#  Set the default MIME type.
  if ( "$mimetype" ne '' ) {
    foreach ( sort keys %main::mimetypes ) {
      $main::def_type = "$_" if ( "$main::mimetypes{$_}" eq "$mimetype" );
    }
  }

#  The rest of the 'Edit' form.
  if ($notarized == 0 ) {



  print <<EOF;
<form action="$main::script" enctype="multipart/form-data" method=post>
<p>
<pre>
<b>Your name</b>: <input type=text name=author value="$author"
 size=40>
<b>The title</b>: <input type=text name=title value="$title"
 size=40>
<b>Keywords</b> : <input type=text name=keywords value="$kwords"
 size=40> (optional)
</pre>
</p>
<p>
You can <b>type</b> or <b>cut and paste</b> into the text area (it will
scroll to accommodate the input):<br>
<textarea rows=15 cols=75 name=body>$body</textarea><br>
</p>

<p>

<p>
Or you can <b>upload</b> an <b>image</b> or <b>file</b> to the end of this page.
<br>The image will be centered and displayed with the specified caption,
if you choose an image MIME type below. 
<br><b>Upload:</b> <input type=file name=upfile value="" size=20>
<br>Image caption: <input type=text name=caption value="$caption" size=40> (optional)
</p>
<p>
<b>Choose the MIME type</b> of your input:
<SELECT NAME="mimetype">
EOF

  foreach ( sort keys %main::mimetypes ) {
    print '<OPTION';
    print ' SELECTED' if ( "$_" eq "$main::def_type" );
    print "> $_</OPTION>\n";
  }

  print <<EOF;
</SELECT>
</p>
<p>
Then submit your entry:<br>
<input type=submit name=submit value="Update this entry">
<input type=submit name=reset value="Start over">
<input type=submit name=cancel value="Cancel">
<input type=hidden name=action value="edit">
<input type=hidden name=nb value="$main::nb">
<input type=hidden name=page value="$main::page">
<input type=hidden name=changes value="$changes">
</p>
</form>
</p>
</body>
</html>
EOF
  }
  return;
}

###############################################################################
#
#  ( ) = &ActionNotarize();
#
#  Notarizes an entry in the notebook.
#  Presently Sets flag to 1 in index file to signify this.
#  INCOMPLETE - needs to do MD5 get stamp from notary server.
#
###############################################################################

sub ActionNotarize {

 my ( $author, $body, $changes, $kwords, $mimetype, $title ) =
     ( '', '', '', '', '', '' );
  my ( $base, $filename, $date ) = ( '', '', '');

# Update Index file to indicate page has been notarized

        open( IDX, "<$main::idxfile" ) || &InvalidFile();
        flock( IDX, $main::LOCK_EX ) if ( $main::flock );
        seek( IDX, 0, 0 );
        my @lines = <IDX>;
        flock( IDX, $main::LOCK_UN ) if ( $main::flock );
        close( IDX );
        open( IDX, ">$main::idxfile" ) || &InvalidFile();
        flock( IDX, $main::LOCK_EX ) if ( $main::flock );
        seek( IDX, 0, 0 );
        foreach ( @lines ) {
          next if ( m/^\s*$/ );			# Blank lines
          next if ( m/^\s*#/ );			# Comment lines
          my ( $tag, $name, $type, $as_is ) = split;
          if ( "$tag" eq "$main::page" ) {
	      print IDX "$tag $name $type 1\n";
            $base = $name;
          } else { print IDX "$_" ; }
        }
        flock( IDX, $main::LOCK_UN ) if ( $main::flock );
        close( IDX );


#  Read in file.
    $filename = "$main::root/$base";
    open( FILE, "<$filename" ) || &InvalidFile();
    while ( <FILE> ) {
      if    ( m/^<!-- Author: (.*) -->$/ )   { $author = $1; }
      elsif ( m/^<!-- Date: (.*) -->$/ )     { $date = $1; }
      elsif ( m/^<!-- Title: (.*) -->$/ )    { $title = $1; }
      elsif ( m/^<!-- Keywords: (.*) -->$/ ) { $kwords = $1; }
      elsif ( m/^<!-- Changes: (.*) -->$/ )  { $changes = $1; }
      else { $body .= $_; }
    }
    close( FILE );

#  Modify change history and Write the file back out.
      my $strdate = gmtime( time );
      open( FILE, ">$filename" ) || &InvalidFile();
      print FILE <<EOF;
<!-- Author: $author -->
<!-- Date: $strdate (GMT) -->
<!-- Title: $title -->
<!-- Keywords: $kwords -->
<!-- Changes: Notarized $strdate (GMT) by N/A | $changes -->
$body
<p>
<b>Notarized</b> from start of page to here on $strdate (GMT)<br>
<HR>
EOF
      close( FILE );
      chmod 0666, ( $filename );

#  Instruct the browser to reload this page.
      print <<EOF;
Location: $main::script?nb=$main::nb&action=view&page=$main::page

EOF
      exit $main::RET_NO_ERROR;
}
###############################################################################
#
#  ( ) = &ActionSearch();
#
#  Searches for entries in the notebook.
#
###############################################################################

sub ActionSearch {

#  Extract info if available.
  my ( $casesen, $everything, $hdrtext, $search, $text ) =
     ( '', '', 'Search', '', '' );

  $casesen    = $main::in{'casesen'}	if ( "$main::in{'casesen'}" ne '' );
  $everything = $main::in{'everything'} if ( "$main::in{'everything'}" ne '' );
  $search     = $main::in{'search'}	if ( "$main::in{'search'}" ne '' );
  $search     =~ s/"/&quot;/g; #"

  my @pagelist = searchNotebook( $search, $casesen, $everything );

  if ( $#pagelist >= 0 ) {
    $hdrtext .= ' Results';

    my $page = 0;
    foreach $page (@pagelist) {

      my @pageinfo = getPageInfo( $page );
      my $title = $pageinfo[3];
      my $author = $pageinfo[0];

      $text .= "<li>Page $page. <a href=\"$main::script?nb=$main::nb&action=view&" .
		 "page=$page\">$title\n($author)</a></li>\n";
    }
  }


#  Build the page.
  print <<EOF;
Content-type: text/html

<html>
<head>
<title>$hdrtext</title>
</head>
<body background="$main::backgrnd">
<center><h2>$main::title - Search</h2></center>

<form action="$main::script" method=post>
EOF

# Place the navigation bar at the top

   if ( $main::nobar != 1 ) {
       print "<center>";

       if ($#pagelist >= 0) { # any results to Print?
	   outputButtons( ('print:search_pages') );
       }
       
       outputButtons( ('toc') );
       print "</center>";
   } # end nobar block
  
  
#  The search header.
  print <<EOF;
<hr>
$main::linehorz\n
<p>
<center>
<b>Search for</b>: <input type=text name=search value="$search" size=35>
<input type=submit name=submit value="Search">
<input type=submit name=cancel value="Cancel">
<input type=hidden name=action value="search">
<input type=hidden name=postpage value="search">
<input type=hidden name=nb value="$main::nb">
<input type=hidden name=page value="$main::page">
<br>
EOF
  print '<input type=checkbox name=everything value=1';
  if ( "$everything" eq '1' ) { print ' checked'; }
  print '> Search in entries text also';
  print '<input type=checkbox name=casesen value=1';
  if ( "$casesen" eq '1' ) { print ' checked'; }
  print '> Case sensitive';
  print <<EOF;
</center>
<center>
Title, authors, keywords and dates are always searched (case-insensitive).
</center>
</p>
</form>
EOF

  #  The results of the search, if we found anything.
  print "\n<hr>\n";
  if ( $#pagelist >= 0 ) {
    print "<h2>$hdrtext</h2>\n<ul>$text</ul>\n";  }
  else {
    if ($search ne '') {
      print "\n<h2>No results.</h2>\n";
    }
  }

  print "\n</body></html>\n";

  return;
}

###############################################################################
#
#  ( ) = &ActionToc();
#
#  Builds a table of contents for the notebook.
#
###############################################################################

sub ActionToc {

#  Build the page.
  print <<EOF;
Content-type: text/html

<html>
<head>
<title>Table of Contents</title>
</head>
<body background="$main::backgrnd">
<center><h2>$main::title</h2></center>
EOF

    my $toctext = buildTOCText();

   # buttons
   if ( $main::nobar != 1 ) {

       print "<center>";
       outputButtons( ('',      'add',  '',        'nr',
		       'search','print:notebook','subscribe') );

       print "</center><hr>\n";
  }


  print <<EOF;
$main::linehorz
<h4>Table of Contents</h4>
$toctext
</body>
</html>
EOF

  return;
}

###############################################################################
#
#  ( ) = &ActionView();
#
#  Views an entry from the notebook.
#
###############################################################################

sub ActionView {

  my ( $filename, $filetype, $objs, $pageno, $pages ) = ( '', '', 0, 0, 0 );
  my  $flag = 0 ;
#  Open the index file and parse it.
  open( IDX, "<$main::idxfile" ) || &InvalidFile($main::idxfile);
  flock( IDX, $main::LOCK_EX ) if ( $main::flock );
  seek( IDX, 0, 0 );
  while ( <IDX> ) {

    next if ( m/^\s*$/ );			# Blank lines
    next if ( m/^\s*#/ );			# Comment lines

    my ( $tag, $name, $type, $as_is ) = split;
    
    if ( $tag < 0 ) {				# Object
      $objs++;
      if ( "$tag" eq "$main::page" ) {
	$pageno   = $objs;
	$filename = $name;
	$filetype = lc( $type );
      }
    }
    elsif ( $tag > 0 ) {			# Page
      $pages++;
      if (( "$main::page" eq 'first' ) ||	# Looking for first file
	  ( "$tag" eq "$main::page" )) {	# Found a matching tag
	$main::page = '';
	if ("$main::page" eq 'first') {
	    $main::in{'page'} = 1;
	}
	$pageno     = $pages;
	$filename   = $name;
	$filetype   = lc( $type );
      $flag = $as_is ;
      }
      elsif ( "$main::page" eq 'last' ) {	# Looking for last file
	$pageno   = $pages;
	$main::in{'page'} = $pageno;
	$filename = $name;
	$filetype = lc( $type );
      $flag = $as_is ;
      }
    }
  }
  flock( IDX, $main::LOCK_UN ) if ( $main::flock );
  close( IDX );
  
 if ( $flag == 1 ) {
      $main::noedit = 1;
      $main::nodelete = 1;
 }

#  Check that we found our page.
  my ( $author, $changes, $date, $kwords, $text, $title ) =
     ( '', '', '', '', '', '' );
  if ( $pageno == 0 ) {
    if (( $pages == 0 ) &&
	(( $main::page == 0 ) || ( "$main::page" eq 'last' ))) {
      ; 					# Empty notebook, OK
    }
    else {					# Request out of bounds
      print <<EOF;
Content-type: text/html

<html>
<head>
<title>Request Error</title>
</head>
<body background="$main::backgrnd">
<h1>Request Error</h1>
<p><b>The requested notebook page or object does not exist.</b></p>
Please use your browser to go back.<hr>
</body>
</html>
EOF
      exit $main::RET_BAD_PAGE;
    }
  }

#  Check that the object or page has not been deleted.
  my $deleted = 0;
  if ( "$filename" eq '' ) {
    if ( $main::page < 0 ) {			# Requested a deleted object
      print "Content-type: text/html\n\n";
      exit $main::RET_NO_ERROR;
    }
    elsif ( $pages > 0 ) {			# Requested a deleted page
      $text  = '<p><center><b>This page was deleted.</b></center></p>';
      $title = 'Deleted page';
      $deleted = 1;
    }
  }

  else {
    $filename = "$main::root/$filename" if ( $filename !~ m/^\// );
    open( FILE, "<$filename" ) || &InvalidFile($filename);

#  For HTML and plain text files, extract the author name, creation date,
#  title, and change history.
    if ( "$filetype" =~ m;^text/(html|plain)$; ) {
      while ( <FILE> ) {
	if    ( m/^<!-- Author: (.*) -->$/ )   { $author = $1; }
	elsif ( m/^<!-- Date: (.*) -->$/ )     { $date = $1; }
	elsif ( m/^<!-- Title: (.*) -->$/ )    { $title = $1; }
	elsif ( m/^<!-- Keywords: (.*) -->$/ ) { $kwords = $1; }
	elsif ( m/^<!-- Changes: (.*) -->$/ )  { $changes = $1; }
	else { $text .= $_; }
      }
      close( FILE );
    }

#  All other files, send as is and exit.
    else {
      print "Content-type: $filetype\n\n";
      $| = 1;
      binmode( FILE );
      binmode( STDOUT );
      while ( <FILE> ) { print; }
      close( FILE );
      exit $main::RET_NO_ERROR;
    }
  }

#  Have a valid page file, build the page.
#  NEW added JavaScript to fire up external annotate window

  my $wintitle = ( "$title" eq '' ) ? "Page $pageno of $pages" :
		 "$title ($pageno/$pages)";
  print <<EOF;
Content-type: text/html

<html>
<head>
<meta name="robots" content="index,nofollow">
<title>Notebook page $pageno</title>
</head>
<body background="$main::backgrnd"> 
<center><h3>$main::title - page $pageno of $pages</h3></center>
EOF

  if ( $main::nopanel == 0 ) {
#  NEW Refresh Control Panel
#  Print script that raises the button(s) on external control panel
print <<EOF;
<script>
Ctrl = window.open("", "Ctrl", "width=180,height=160,scrollbars=0,resizable=0");
Ctrl.opener.name = "creator";
Ctrl.document.writeln("<HTML><HEAD><TITLE>$main::title</TITLE></HEAD>");
Ctrl.document.writeln("<BODY BGCOLOR=#C0C0C0>");
Ctrl.document.writeln("<a "); 
Ctrl.document.writeln("href=\\"$main::script?nb=$main::nb&action=toc&page=$pageno\\" TARGET='creator' onMouseDown=\\"content.src='$main::newgifs/Contents1.gif'; return true;\\" ><IMG "); 
Ctrl.document.writeln("SRC=\\"$main::newgifs/Contents.gif\\" Border=0 name=content></a><nobr><a ");
Ctrl.document.writeln("href=\\"$main::script?nb=$main::nb&action=search&page=$pageno\\" TARGET='creator' onMouseDown=\\"find.src='$main::newgifs/Search1.gif'; return true;\\"><IMG "); 
Ctrl.document.writeln("SRC=\\"$main::newgifs/Search.gif\\" Border=0 name=find></a><br> ");
EOF
#  If not first page, put a 'Prev' link.
  if ( $pageno > 1 ) {
    my $target = $pageno - 1;
print <<EOF;
Ctrl.document.writeln("<a ");
Ctrl.document.writeln("href=\\"$main::script?nb=$main::nb&action=view&page=$target\\" TARGET='creator' onMouseDown=\\"previous.src='$main::newgifs/Previous1.gif'; return true;\\"><IMG ");
Ctrl.document.writeln("SRC=\\"$main::newgifs/Previous.gif\\" Border=0 name=previous></a");
EOF
  }
  else {
    print <<EOF;
Ctrl.document.writeln("<IMG SRC=\\"$main::newgifs/Previous2.gif\\" Border=0 name=previous");
EOF
  }
  if ( $pageno < $pages ) {
    my $target = $pageno + 1;
    print <<EOF;
Ctrl.document.write("><a ");
Ctrl.document.writeln("href=\\"$main::script?nb=$main::nb&action=view&page=$target\\" TARGET='creator' onMouseDown=\\"next.src='$main::newgifs/Next1.gif'; return true;\\"><IMG ");
Ctrl.document.writeln("SRC=\\"$main::newgifs/Next.gif\\" Border=0 name=next></a><br> ");
EOF
  }
  else {
    print <<EOF;
Ctrl.document.writeln("><IMG SRC=\\"$main::newgifs/Next2.gif\\" Border=0 name=next><br> ");
EOF
  }
print <<EOF;
Ctrl.document.writeln("<a ");
Ctrl.document.writeln("href=\\"$main::script?nb=$main::nb&action=view&page=1\\" TARGET='creator' onMouseDown=\\"first.src='$main::newgifs/First1.gif'; return true;\\"><IMG ");
Ctrl.document.writeln("SRC=\\"$main::newgifs/First.gif\\" Border=0 name=first></a><a "); 
Ctrl.document.writeln("href=\\"$main::script?nb=$main::nb&action=view&page=last\\" TARGET='creator' onMouseDown=\\"last.src='$main::newgifs/Last1.gif'; return true;\\"><IMG ");
Ctrl.document.writeln("SRC=\\"$main::newgifs/Last.gif\\" Border=0 name=last></a><br> ");
EOF
# allow add function to be disabled in config
  if ( $main::noadd == 0 ) {
    print <<EOF;
Ctrl.document.writeln("<a ");
Ctrl.document.writeln("href=\\"$main::script?nb=$main::nb&action=add&page=$pageno\\" TARGET='creator' onMouseDown=\\"add.src='$main::newgifs/Add1.gif'; return true;\\"><IMG ");
Ctrl.document.writeln("SRC=\\"$main::newgifs/Add.gif\\" Border=0 name=add></a");
EOF
  }
  else {
    print <<EOF;
Ctrl.document.writeln("<IMG SRC=\\"$main::newgifs/Add2.gif\\" Border=0 name=add");
EOF
  }
# allow delete function to be disabled in config
  if ( $main::nodelete == 0 ) {
    print <<EOF;
Ctrl.document.write("><a ");
Ctrl.document.writeln("href=\\"$main::script?nb=$main::nb&action=delete&page=$pageno\\" TARGET='creator' onMouseDown=\\"del.src='$main::newgifs/Delete1.gif'; return true;\\"><IMG ");
Ctrl.document.writeln("SRC=\\"$main::newgifs/Delete.gif\\" Border=0 name=del></a><br> ");
EOF
  }
  else {
    print <<EOF;
Ctrl.document.writeln("><IMG SRC=\\"$main::newgifs/Delete2.gif\\" Border=0 name=del><br> ");
EOF
  }
# allow edit function to be disabled in config
  if ( $main::noedit == 0 ) {
    print <<EOF;
Ctrl.document.writeln("<a ");
Ctrl.document.writeln("href=\\"$main::script?nb=$main::nb&action=edit&page=$pageno\\" TARGET='creator' onMouseDown=\\"edit.src='$main::newgifs/Edit1.gif'; return true;\\"><IMG ");
Ctrl.document.writeln("SRC=\\"$main::newgifs/Edit.gif\\" Border=0 name=edit");
EOF
  }
  else {
    print <<EOF;
Ctrl.document.writeln("<IMG SRC=\\"$main::newgifs/Edit2.gif\\" Border=0 name=edit");
EOF
  }
# allow annotate function to be disabled in config
  if ( $main::noannotate == 0 ) {
    print <<EOF;
Ctrl.document.write("><a ");
Ctrl.document.writeln("href=\\"$main::script?nb=$main::nb&action=annotate&page=$pageno\\" TARGET='creator' onMouseDown=\\"annotate.src='$main::newgifs/Annotate1.gif'; return true;\\"><IMG ");
Ctrl.document.writeln("SRC=\\"$main::newgifs/Annotate.gif\\" Border=0 name=annotate></a><br> ");
EOF
  }
  else {
    print <<EOF;
Ctrl.document.writeln("><IMG SRC=\\"$main::newgifs/Annotate2.gif\\" Border=0 name=annotate><br> ");
EOF
  }
# allow notarize function to be disabled in config
  if ( $main::nonotarize == 0 ) {
    print <<EOF;
Ctrl.document.write("<a ");
Ctrl.document.writeln("href=\\"$main::script?nb=$main::nb&action=notarize&page=$pageno\\" TARGET='creator' onMouseDown=\\"notarize.src='$main::newgifs/Notarize1.gif'; return true;\\"><IMG ");
Ctrl.document.writeln("SRC=\\"$main::newgifs/Notarize.gif\\" Border=0 name=notarize ");
EOF
  }
  else {
    print <<EOF;
Ctrl.document.writeln("<IMG SRC=\\"$main::newgifs/Notarize2.gif\\" Border=0 name=notarize");
EOF
  }
# Finally put in the close button
print <<EOF;
Ctrl.document.write("><a ");
Ctrl.document.writeln("href=\\"../enote/stop.html\\" onMouseDown=\\"exit.src='$main::newgifs/Close1.gif'; self.close(); return true;\\"><IMG SRC=\\"$main::newgifs/Close.gif\\" Border=0 name=exit></a> ");
Ctrl.document.writeln("</BODY></HTML>");
Ctrl.document.close();
</script>
EOF
# END refreshCtrl JavaScript block
  }

#  Print the navigation bar at the top.
   if ( $main::nobar != 1 ) {
       my @buttons = ();

       print "<center>";

       
#  Put the 'Add' link.
  if ( $main::noadd == 0 ) {
      @buttons = ( @buttons, 'add');
  }
  else {
      @buttons = ( @buttons, 'add0');
  }

#  Put the 'Edit' link.
  if (( $pageno > 0 ) && ( ! $deleted ) && ( $main::noedit == 0 )) {
      @buttons = ( @buttons, 'edit');
  }
  else {
      @buttons = ( @buttons, 'edit0');
  }

#  Put the 'Delete' link.
  if (( $pageno > 0 ) && ( ! $deleted ) && ( $main::nodelete == 0 )) {
      @buttons = ( @buttons, 'delete');
  }
  else {
      @buttons = ( @buttons, 'delete0');
  }

#  Put the 'Annotate' link.
  if (( $pageno > 0 ) && ( ! $deleted ) && ( $main::noannotate == 0 )) {
      @buttons = ( @buttons, 'annotate');
  }
  else {
      @buttons = ( @buttons, 'annotate0');
  }

#  Put the 'Notarize' link.
  if (( $pageno > 0 ) && ( ! $deleted ) && ( $main::nonotarize == 0 )) {
      @buttons = ( @buttons, 'notarize');
  }
  else {
      @buttons = ( @buttons, 'notarize0');
  }

  outputButtons(@buttons);
  @buttons = ();

  # next line of buttons

#  If not first page, put a 'First' link.
  if ( $pageno > 1 ) {
      @buttons = ( @buttons, 'first');
  }
  else {
      @buttons = ( @buttons, 'first0');
  }

#  If not first page, put a 'Prev' link.
  if ( $pageno > 1 ) {
      @buttons = ( @buttons, 'prev');
  }
  else {
      @buttons = ( @buttons, 'prev0');
  }

#  If not first page, put a 'Next' link.
  if ( $pageno < $pages ) {
      @buttons = ( @buttons, 'next');
  }
  else {
      @buttons = ( @buttons, 'next0');
  }

#  If not first page, put a 'Last' link.
  if ( $pageno < $pages ) {
      @buttons = ( @buttons, 'last');
  }
  else {
      @buttons = ( @buttons, 'last0');
  }

#  Put the 'Table of Contents' link.
  if ( $pages > 0 ) {
       @buttons = ( @buttons, 'toc');
  }
  else { 
      @buttons = ( @buttons, 'toc0');
  }

  if ( ( $main::printpage == 1) && ( ! $deleted ) ) {
#  Put the 'Print' link.
      @buttons = ( @buttons, 'print:page');
  }

#  Put the 'Search' link.
  if ( $pages > 0 ) {
      @buttons = ( @buttons, 'search');
  }
  else { 
      @buttons = ( @buttons, 'search0');
  } 

  outputButtons(@buttons);
  print "</center><hr>";
  }
# End nobar block

# Use drop down menu rather than single line for change history
  $changes =~ s/\|/<OPTION>/g;
print <<EOF; 
<TABLE FRAME=ALL BORDER=0 RULES=Basic CELLPADDING=0 HEIGHT=20>
<TR ALIGN="left" VALIGN="top">
<TD><b>Date and Author(s)</b>
<TD><FORM><SELECT SIZE=1 NAME="History">
<OPTION>$changes
</SELECT> 
</FORM>
</TABLE>
EOF
  print "$main::lineview <br>\n";  
  print "\n<center><h3>$title</h3></center>" if ( "$title" ne '' );
  print "\n<p>";
  print "\n<pre>"  if ( "$filetype" eq 'text/plain' );
  #
  # Wrap plain text within 80 characters
  #if ( "$filetype" eq 'text/plain' ){
  #  print "\n"; print wrap("", "", $text);
  #}
  #else {
    print "\n$text";
  #}
  print "\n</pre>" if ( "$filetype" eq 'text/plain' );
  print "\n</p>";

#  Put the trailer.
  print "\n</body>\n</html>\n";

  return;
}

###############################################################################
#
#  ( ) = &ActionPrint( $method );
#
#  Prints the TOC or Search result list and optionally the pages themselves
#   $method eq 'ask_contents' - ask if the user wants just the TOC or all pages
#   $method eq 'contents' - print Contents (TOC)
#   $method eq 'notebook' - print Contents plus entire notebook!
#   $method eq 'ask_search' - ask if the user wants just the search result list
#                              or all the pages matching the search also
#   $method eq 'search' - print search results list page  (like TOC)
#   $method eq 'search_pages' - print the search results list and all listed pages
#
###############################################################################

sub ActionPrint {

    my $method = $_[0];



    if ( $method eq 'ask_contents') {

#Page that gives user option to print just the TOC, or the TOC & the entire notebook!
# (or return to the TOC without printing)

    print <<EOF;
Content-type: text/html

<html>
<head>
<meta name="robots" content="index,nofollow">
<title>$main::title</title>
</head>
<body background="$main::backgrnd">
<h1>$main::title - Print selection</h1>
$main::linehorz\n

<h3><img
 src="$main::gifs/lend.gif" height=$main::btn_ht width=$main::lend_wd
 border=0 alt=""
><a href="$main::script?nb=$main::nb&action=print&method=contents"
 onMouseOver="print.src='$main::gifs/print2.gif';
 window.status='Print Table of Contents'; return true;"
 onMouseOut="print.src='$main::gifs/print1.gif'; return true;"><img
 src="$main::gifs/print1.gif" height=$main::btn_ht width=$main::print_wd
 border=0 alt="Print" name=print></a><img
 src="$main::gifs/rend.gif" height=$main::btn_ht width=$main::rend_wd
 border=0 alt="">
Table of Contents page
</h3>

<h3><img
 src="$main::gifs/lend.gif" height=$main::btn_ht width=$main::lend_wd
 border=0 alt=""
><a href="$main::script?nb=$main::nb&action=print&method=notebook"
 onMouseOver="printnotebook.src='$main::gifs/print2.gif';
 window.status='Print Entire Notebook'; return true;"
 onMouseOut="printnotebook.src='$main::gifs/print1.gif'; return true;"><img
 src="$main::gifs/print1.gif" height=$main::btn_ht width=$main::print_wd
 border=0 alt="Print" name=printnotebook></a><img
 src="$main::gifs/rend.gif" height=$main::btn_ht width=$main::rend_wd
 border=0 alt="">
Entire Notebook(!)
</h3>

<h3><img
 src="$main::gifs/lend.gif" height=$main::btn_ht width=$main::lend_wd
 border=0 alt=""
><a href="$main::script?nb=$main::nb&action=toc"
 onMouseOver="toc.src='$main::gifs/toc2.gif';
 window.status='Return to Table of Contents'; return true;"
 onMouseOut="toc.src='$main::gifs/toc1.gif'; return true;"><img
 src="$main::gifs/toc1.gif" height=$main::btn_ht width=$main::toc_wd
 border=0 alt="Contents" name=toc></a><img
 src="$main::gifs/rend.gif" height=$main::btn_ht width=$main::rend_wd
 border=0 alt="">
Return to Contents page (without printing)
</h3>

<hr>
(Use the browser 'Back' button to return after printing)

</body></html>
EOF

      return;
    }
    elsif ( $method eq 'ask_search') {

#Page that gives user option to print just the search results list, or the list & all pages in it
# (or return to the TOC without printing)
# (Note: the buttons do a POST so we can pass the search criteria back here)

    print <<EOF;
Content-type: text/html

<html>
<head>
<meta name="robots" content="index,nofollow">
<title>$main::title</title>
</head>
<body background="$main::backgrnd">
<h1>$main::title - Search Print selection</h1>
EOF

# Reproduce the POST data
    print <<EOF;
<form action="$main::script" method=post>
<input type=hidden name=action value="print">
<input type=hidden name=postpage value="ask_search">
<input type=hidden name=nb value="$main::nb">
<input type=hidden name=search value="$main::in{'search'}">
<input type=hidden name=casesen value="$main::in{'casesen'}">
<input type=hidden name=everything value="$main::in{'everything'}">
EOF


print <<EOF;
$main::linehorz\n

<h3><img
 src="$main::gifs/lend.gif" height=$main::btn_ht width=$main::lend_wd
 border=0 alt=""
><input type=image name=print_method_search src="$main::gifs/print1.gif"
><img
 src="$main::gifs/rend.gif" height=$main::btn_ht width=$main::rend_wd
 border=0 alt="">
Search results page list (for text \"$main::in{'search'}\")
</h3>

<h3><img
 src="$main::gifs/lend.gif" height=$main::btn_ht width=$main::lend_wd
 border=0 alt=""
><input type=image name=print_method_search_pages src="$main::gifs/print1.gif"
><img
 src="$main::gifs/rend.gif" height=$main::btn_ht width=$main::rend_wd
 border=0 alt="">
All pages matching the search (for text \"$main::in{'search'}\")
</h3>

<h3><img
 src="$main::gifs/lend.gif" height=$main::btn_ht width=$main::lend_wd
 border=0 alt=""
><a href="$main::script?nb=$main::nb&action=toc"
 onMouseOver="toc.src='$main::gifs/toc2.gif';
 window.status='Table of Contents'; return true;"
 onMouseOut="toc.src='$main::gifs/toc1.gif'; return true;"
><img
 src="$main::gifs/toc1.gif" height=$main::btn_ht width=$main::toc_wd
 border=0 alt="Contents" name=toc></a><img
 src="$main::gifs/rend.gif" height=$main::btn_ht width=$main::rend_wd
 border=0 alt="">
Return to Contents page (without printing)
</h3>
EOF

    print <<EOF;
</form>
<hr>
<p>(Use the browser 'Back' button to return after printing)</p>

</body></html>
EOF

      return;
    }


#Page header
    print <<EOF;
Content-type: text/html

<html>
<head>
<meta name="robots" content="noindex,nofollow">
<title>$main::title</title>
</head>
<body background="$main::backgrnd">
<h1>$main::title</h1>
EOF


    # Print single page
    if ( $method eq 'page') {

	my $page = $main::page;
        my @pageinfo = getPageInfo( $page );
        my $title = $pageinfo[3];
	my $author = $pageinfo[0];
	my $filename = $pageinfo[5];
	my $filetype = $pageinfo[6];

	if ( "$filename" ne '') { # not deleted?

	    # read in file and display it
	    $filename = "$main::root/$filename" if ( $filename !~ m/^\// );
	    open( FILE, "<$filename" ) || &InvalidFile($filename);
	    
	    if ( "$filetype" =~ m;^text/(html|plain)$; ) {
		my ( $changes, $date, $kwords, $text ) =
		    ( '', '', '', '' );
		
		while ( <FILE> ) {
		    if    ( m/^<!-- Author: (.*) -->$/ )   { }
		    elsif ( m/^<!-- Date: (.*) -->$/ )     { $date = $1; }
		    elsif ( m/^<!-- Title: (.*) -->$/ )    { }
		    elsif ( m/^<!-- Keywords: (.*) -->$/ ) { $kwords = $1; }
		    elsif ( m/^<!-- Changes: (.*) -->$/ )  { $changes = $1; }
		    else { $text .= $_; }
		}
		close( FILE );
		
		# extract last modification
		my @changelist = split(/\|/, $changes);
		my $lastchange = $changelist[0];

		print <<EOF;
<hr>
$main::linehorz\n
<h2>page $page - $title ($author)</h2>
<H4>[$lastchange]</H4>
EOF
                print "\n<p>";
		print "\n<pre>"  if ( "$filetype" eq 'text/plain' );
		print "\n$text";
		print "\n</pre>" if ( "$filetype" eq 'text/plain' );
		print "\n</p>";
		
	    }
	    else {
		print "\n<h4>Content with MIME type $filetype not shown.</h4>";
	    }
	}
	else {
	    print "\n<h4>Page has been deleted.</h4>";
	}

	print "\n<hr>\n";

    } #end Print single page block



    #
    # Print the TOC if requested
    if ( ($method eq 'contents') or ($method eq 'notebook')) {

	my $toctext = &buildTOCText();
  print <<EOF;
<DIV style="page-break-after:always"><hr>
$main::linehorz
<h4>Table of Contents</h4>
$toctext
</DIV>
EOF
    }

    #
    # Print search result list, if requested
    my @pagelist = (); #save matching page list for printing the actual pages themselves (if requested)

    if ( ($method eq 'search') or ($method eq 'search_pages')) {
	# Perform the search again
	#  (Print button was really a POST that re-transmitted all the search field
	#   to us again)
	my ( $casesen, $everything, $hdrtext, $search, $text ) =
	    ( '', '', 'Search', '', '' );
	
	$casesen    = $main::in{'casesen'}	if ( "$main::in{'casesen'}" ne '' );
	$everything = $main::in{'everything'} if ( "$main::in{'everything'}" ne '' );
	$search     = $main::in{'search'}	if ( "$main::in{'search'}" ne '' );
	$search     =~ s/"/&quot;/g;
#"
        @pagelist = searchNotebook( $search, $casesen, $everything );

        if ( $#pagelist >= 0 ) {
          $hdrtext .= ' Results';

          my $page = 0;
          foreach $page (@pagelist) {

            my @pageinfo = getPageInfo( $page );
            my $title = $pageinfo[3];
            my $author = $pageinfo[0];

            $text .= "<li>Page $page. <a href=\"$main::script?nb=$main::nb&action=view&" .
		     "page=$page\">$title\n($author)</a>\n";
          }

          print "\n<hr>$main::linehorz\n";
	  print "\n<h3>Search results for text \"$search\"</h3>";
          print "\n<ul>$text</ul>\n";
        }
        else {
          print "\n<hr>No Results.\n";
        }
	
        
    }


    #
    # Print the entire notebook if requested!
    if ( $method eq 'notebook') {
    

      # Open the index file and scan through it.  For each page output it to
      #  build up a mega web page that contains the content of all of them
      #  (this is then printed)


  my ( $filename, $filetype, $objs, $pageno, $pages ) = ( '', '', 0, 0, 0 );
  my  $flag = 0 ;

  open( IDX, "<$main::idxfile" ) || &InvalidFile($main::idxfile);
  flock( IDX, $main::LOCK_EX ) if ( $main::flock );
  seek( IDX, 0, 0 );
  while ( <IDX> ) {

    next if ( m/^\s*$/ );			# Blank lines
    next if ( m/^\s*#/ );			# Comment lines

    my ( $tag, $name, $type, $as_is ) = split;

    if ( $tag > 0 ) {			# Page (ignore objects - they are displayed in-line by pages)
	$pages++;
	$pageno     = $pages;
	$filename   = $name;
	$filetype   = lc( $type );
	$flag = $as_is ;

	if ("$filename" ne '') { # if object not deleted
	    $filename = "$main::root/$filename" if ( $filename !~ m/^\// );
	    open( FILE, "<$filename" ) || &InvalidFile($filename);

	    #  For HTML and plain text files, extract the author name, creation date,
	    #  title, and change history.  For all other types, just show that the object
	    #  exists but don't place it in-line.

	    if ( "$filetype" =~ m;^text/(html|plain)$; ) {
		my ( $author, $changes, $date, $kwords, $text, $title ) =
		    ( '', '', '', '', '', '' );

		while ( <FILE> ) {
		    if    ( m/^<!-- Author: (.*) -->$/ )   { $author = $1; }
		    elsif ( m/^<!-- Date: (.*) -->$/ )     { $date = $1; }
		    elsif ( m/^<!-- Title: (.*) -->$/ )    { $title = $1; }
		    elsif ( m/^<!-- Keywords: (.*) -->$/ ) { $kwords = $1; }
		    elsif ( m/^<!-- Changes: (.*) -->$/ )  { $changes = $1; }
		    else { $text .= $_; }
		}
		close( FILE );
		
		# extract last modification
		my @changelist = split(/\|/, $changes);
		my $lastchange = $changelist[0];

		print <<EOF;
<br>
<DIV style="page-break-after:always"><hr>
$main::linehorz\n
<H2>$main::title page $pageno - $title ($author)</H2>
<H4>[$lastchange]</H4>
EOF
                print "\n<p>";
		print "\n<pre>"  if ( "$filetype" eq 'text/plain' );
		print "\n$text";
		print "\n</pre>" if ( "$filetype" eq 'text/plain' );
		print "\n</p>";
		print "</DIV>\n";
	    }
	    else {
		print "\n<h4>Content with MIME type $filetype not shown.</h4>";
	    }

        }
	else {
            if ( $main::showdeleted == 1) {
	        print "\n<h4>Notebook page $pageno has been deleted.</h4>";
            }
	}
    }
  }
  flock( IDX, $main::LOCK_UN ) if ( $main::flock );
  close( IDX );
  print "\n<hr>\nEnd of Notebook.\n";

    } # end of Print entire notebook block



    #
    # Print the all the pages that match the search
    if ( $method eq 'search_pages') {

        # If we get here, the print block for the search results list
        # has already performed the search and left us with @pagelist

        if ( $#pagelist >= 0 ) {

          my $page = 0;
          foreach $page (@pagelist) {

            my @pageinfo = getPageInfo( $page );
            my $title = $pageinfo[3];
            my $author = $pageinfo[0];
            my $filename = $pageinfo[5];
            my $filetype = $pageinfo[6];

            # read in file and display it
	    $filename = "$main::root/$filename" if ( $filename !~ m/^\// );
	    open( FILE, "<$filename" ) || &InvalidFile($filename);

	    if ( "$filetype" =~ m;^text/(html|plain)$; ) {
		my ( $changes, $date, $kwords, $text ) =
		    ( '', '', '', '' );

		while ( <FILE> ) {
		    if    ( m/^<!-- Author: (.*) -->$/ )   { }
		    elsif ( m/^<!-- Date: (.*) -->$/ )     { $date = $1; }
		    elsif ( m/^<!-- Title: (.*) -->$/ )    { }
		    elsif ( m/^<!-- Keywords: (.*) -->$/ ) { $kwords = $1; }
		    elsif ( m/^<!-- Changes: (.*) -->$/ )  { $changes = $1; }
		    else { $text .= $_; }
		}
		close( FILE );
		
		# extract last modification
		my @changelist = split(/\|/, $changes);
		my $lastchange = $changelist[0];

		print <<EOF;
<DIV style="page-break-after:always"><hr>
$main::linehorz\n
<h2>$main::title page $page - $title ($author)</h2>
<h4>[$lastchange]</h4>
EOF
                print "\n<p>";
		print "\n<pre>"  if ( "$filetype" eq 'text/plain' );
		print "\n$text";
		print "\n</pre>" if ( "$filetype" eq 'text/plain' );
		print "\n</p>";
		print "</DIV>\n";

	    }
	    else {
		print "\n<h4>Content with MIME type $filetype not shown.</h4>";
	    }

          } #foreach
 
          print "\n<hr>\nEnd of Notebook pages matching search.\n";
        } #end if $#pagelist > 0

    } #end print all pages matching search block


#ask browser to bring up a print dialog
    print <<EOF;
<script>
    window.print();
</script>
</body>
</html>
EOF



}


###############################################################################
#
#  ( ) = &ActionSubscription( $method );
#
#  Displays page allowing user to subscribe/unsubscribe to update notification e-mails
#   for this Notebook.  ($method subscription)
# Also actually subscribes/unsubscribes them ($method subscribe/unsubscribe)
#
###############################################################################

sub ActionSubscription {

    my $method = $_[0];

    if ("$method" eq 'subscription') {
	# Display page allowing the user to [un]subscribe to notifications

    print <<EOF;
Content-type: text/html

<html>
<head>
<title>Subscription</title>
</head>
<body background="$main::backgrnd">
<center><h2>$main::title - Subscription</h2></center>

<form action="$main::script" method=post>
EOF

    # Place the navigation bar at the top
    if ( $main::nobar != 1 ) {
	print "<center>";
	outputButtons( ('toc') );
	print "</center>\n";
    } 


#  The [un]subscribe header.
    print <<EOF;
<hr>
$main::linehorz\n
<p>
Enter your name and e-mail address and press <b>Subscribe</b> if you would like to be sent
an update notification via e-mail when additions or changes are made
to pages in this notebook.  Press 
<b>Unsubscribe</b> to remove yourself from the subscription list 
 <i>(both name and email must match that used during subscription)</i>.
</p>
<p><b>Name</b>: <input type=text name=name size=35 value=$main::in{'name'}>
<br><br>
<b>E-Mail address</b>: <input type=text name=email size=35 value=$main::in{'email'}>
<br><br>
<input type=submit name=submit value="Subscribe">
<input type=submit name=submit value="Unsubscribe">
<input type=hidden name=postpage value="subscription">
<input type=hidden name=nb value="$main::nb">
<br>
</p>
</form>
EOF

      # The current subscription list
      &outputSubscriptions( $main::nb );

      print <<EOF;

</body></html>
EOF
	
    }
    elsif ($method =~ m/u?n?subscribe/) {

  print <<EOF;
Content-type: text/html

<html>
<head>
<title>Subscription</title>
</head>
<body background="$main::backgrnd">
<center><h2>$main::title - Subscription</h2></center>

EOF

        # Place the navigation bar at the top
        if ( $main::nobar != 1 ) {
	    print "<center>";
	    outputButtons( ('toc') );
	    print "</center>\n";
	} 


        # Subscribe/unsubscribe e-mail
        my $email = $main::in{'email'};
        my $name = $main::in{'name'};
        my $act = 'added to';
        my $ok = 1;
        if ($email =~ m/.?\w*@\w+\.\w+/) { # basic e-mail validity check
	    if ("$method" eq 'subscribe') {
		$ok = addSubscription( $main::nb, $name, $email ); 
	    }
	    else {
		$ok = removeSubscription( $main::nb, $name, $email );
		$act = 'removed from';
	    }
	}
        else { $ok = 0; } # bad email address
  
        if ($ok) {
	    print <<EOF;
      
<p>The user <b>$name \&lt;$email\&gt;</b> has been $act the subscription list
for notebook: $main::notebook{$main::nb}{'title'}.</p>

EOF

        }
        else { #not ok
	    if ("$method" eq 'subscribe') {
		print "<p><b>Subscription failed.  User <i>$name</i> may already be in the subscription list for this notebook.</b></p>";
	    }
	    else {
		print "<p><b>Unsubscription failed.  User <i>$name \&lt;$email\&gt;</i> may not be in the subscription list for this notebook. User name <i>and</i> e-mail address must match.</b></p>";
	    }
	}
        &outputSubscriptions( $main::nb );

        print "\n</body></html>\n";
  
  
    }
    
}


###############################################################################
#
#  Auxiliary support routines
#
###############################################################################



###############################################################################
#
#  ( ) = &outputButtons( @buttonlist );
#
#  Outputs the HTML code for a row of buttons. $buttons is a list of string 
#   button names.
#   Names: '': No button - useful for empty table elements.
#          'nr': Start a new table row for buttons.
#
#          'toc' : Table of Contents button. action=toc
#          'add' : Add page.      action=add
#          'edit': Edit page.     action=edit
#          'delete': Delete page. action=delete
#          'annotate': Annotate page.  action=annotate
#          'notarize': Notorize page.  action=
#          'search':   Search page.    action=search
#          'print:method':    Print displayed pages. action=print
#          'subscribe':Subscription page.     action=subscription
#          'first' : view first page
#          'last'  : view last page
#          'next'  : view next page
#          'prev'  : view previous page
#
# NB: Most button names can haev '0' appended to obtain the inactive version
#      of the button (no action on click)
#
###############################################################################
sub outputButtons
{
    my @buttonlist = @_;

    # some handy strings
    my $lend = <<EOF;
<img
 src="$main::gifs/lend.gif" height=$main::btn_ht width=$main::lend_wd
 border=0 alt=""
EOF
    $lend .= " >";

    my $rend = <<EOF;
<img
 src="$main::gifs/rend.gif" height=$main::btn_ht width=$main::rend_wd
 border=0 alt=""
EOF
    $rend .= " >";

    print '<table cellspacing="2"><tr>'."\n";

    my $button = '';
    foreach $button ( @buttonlist ) {

	print "<td>";
	if (("$button" ne '') && ("$button" ne 'nr')) { print $lend; }

	if ("$button" eq 'nr') {
	    print "</tr><tr>";
	}
	elsif ("$button" eq 'toc') {
	    print <<EOF;
<a
 href="$main::script?nb=$main::nb&action=toc"
 onMouseOver="toc.src='$main::gifs/toc2.gif';
 window.status='Table of Contents'; return true;"
 onMouseOut="toc.src='$main::gifs/toc1.gif'; return true;"><img
 src="$main::gifs/toc1.gif" height=$main::btn_ht width=$main::toc_wd  
 border=0 alt="Table of Contents" name=toc

EOF
	}
	elsif ("$button" eq 'toc0') {
	    print <<EOF;
<img
 src="$main::gifs/toc0.gif" height=$main::btn_ht width=$main::toc_wd  
 border=0 alt="Table of Contents" 
EOF
            print " >";
	}
	elsif ("$button" eq 'add') {

            if ( $main::noadd == 0 ) {
                print <<EOF;
<a
 href="$main::script?nb=$main::nb&action=add"
 onMouseOver="add.src='$main::gifs/add2.gif';
 window.status='Add a page'; return true;"
 onMouseOut="add.src='$main::gifs/add1.gif'; return true;"><img
 src="$main::gifs/add1.gif" height=$main::btn_ht width=$main::add_wd
 border=0 alt="Add" name=add
EOF
                print " ></a>";
            }
	    else {
    print <<EOF;
<img
 src="$main::gifs/add0.gif" height=$main::btn_ht width=$main::add_wd 
 border=0 alt="Add"
EOF
                print " >";
	    }

	}
	elsif ("$button" eq 'edit') {
	    print <<EOF;
<a
 href="$main::script?nb=$main::nb&action=edit&page=$main::in{'page'}"
 onMouseOver="edit.src='$main::gifs/edit2.gif';
 window.status='Edit current page'; return true;"
 onMouseOut="edit.src='$main::gifs/edit1.gif'; return true;"><img
 src="$main::gifs/edit1.gif" height=$main::btn_ht width=$main::edit_wd
 border=0 alt="Edit" name=edit
EOF
            print " ></a>";
	}
	elsif ("$button" eq 'edit0') {
            print <<EOF;
<img
 src="$main::gifs/edit0.gif" height=$main::btn_ht width=$main::edit_wd 
 border=0 alt="Edit"
EOF
            print " >";
	}
	elsif ("$button" eq 'delete') {
	    print <<EOF;
<a
 href="$main::script?nb=$main::nb&action=delete&page=$main::in{'page'}"
 onClick="return confirm('     Warning: Irreversible operation\\n\\nPress OK to Delete current document \\n\\nPress Cancel otherwise\\n\\n');"
 onMouseOver="del.src='$main::gifs/del2.gif';
 window.status='Delete current page'; return true;"
 onMouseOut="del.src='$main::gifs/del1.gif'; return true;"><img
 src="$main::gifs/del1.gif" height=$main::btn_ht width=$main::del_wd
 border=0 alt="Delete" name=del
EOF
            print " ></a>";
	}
	elsif ("$button" eq 'delete0') {
	    print <<EOF;
<img
 src="$main::gifs/del0.gif" height=$main::btn_ht width=$main::del_wd
 border=0 alt="Delete"
EOF
            print " >";

	}
	elsif ("$button" eq 'annotate') {
	    print <<EOF;
<a
 href="$main::script?nb=$main::nb&action=annotate&page=$main::in{'page'}"
 onMouseOver="anno.src='$main::gifs/anno2.gif';
 window.status='Annotate current page'; return true;"
 onMouseOut="anno.src='$main::gifs/anno1.gif'; return true;"><img
 src="$main::gifs/anno1.gif" height=$main::btn_ht width=$main::anno_wd  
 border=0 alt="Annotate" name=anno
EOF
            print " ></a>";
	}	
	elsif ("$button" eq 'annotate0') {
	    print <<EOF;
<img
 src="$main::gifs/anno0.gif" height=$main::btn_ht width=$main::anno_wd
 border=0 alt="Annotate"
EOF
            print " >";
	}
	elsif ("$button" eq 'notarize') {
	    print <<EOF;
<a
 href="$main::script?nb=$main::nb&action=notarize&page=$main::in{'page'}"
 onClick="return confirm('     Warning: Irreversible operation\\n\\nPress OK to Notarize current document \\n\\nPress Cancel otherwise\\n\\n');"
 onMouseOver="notar.src='$main::gifs/notar2.gif';
 window.status='Notarize current page'; return true;"
 onMouseOut="notar.src='$main::gifs/notar1.gif'; return true;"><img
 src="$main::gifs/notar1.gif" height=$main::btn_ht width=$main::notar_wd
 border=0 alt="Notarize" name=notar 
EOF
            print " ></a>";
	}
	elsif ("$button" eq 'notarize0') {
	    print <<EOF;
<img
 src="$main::gifs/notar0.gif" height=$main::btn_ht width=$main::notar_wd
 border=0 alt="Notarize"
EOF
            print " >";
	}
	elsif ("$button" eq 'search') {
            print <<EOF;    
<a      
 href="$main::script?nb=$main::nb&action=search"
 onMouseOver="srch.src='$main::gifs/srch2.gif';
 window.status='Search'; return true;" 
 onMouseOut="srch.src='$main::gifs/srch1.gif'; return true;"><img
 src="$main::gifs/srch1.gif" height=$main::btn_ht width=$main::srch_wd
 border=0 alt="Search" name=srch
EOF
            print " ></a>";

	}
	elsif ("$button" eq 'search0') {
	    print <<EOF;
<img
 src="$main::gifs/srch0.gif" height=$main::btn_ht width=$main::srch_wd
 border=0 alt="Search"
EOF
            print " >";
	}
	elsif ("$button" eq 'print:notebook') {
            print <<EOF;
<a
 href="$main::script?nb=$main::nb&action=print&method=notebook"
 onClick="return confirm('     Warning: Are you sure you want to print the entire notebook on paper?\\n');"
 onMouseOver="print.src='$main::gifs/print2.gif';
 window.status='Print'; return true;"
 onMouseOut="print.src='$main::gifs/print1.gif'; return true;"><img
 src="$main::gifs/print1.gif" height=$main::btn_ht width=$main::print_wd
 border=0 alt="Print" name=print
EOF
            print " ></a>";
	}
	elsif ("$button" eq 'print:search_pages') {
            print <<EOF;
<a
 href="$main::script?nb=$main::nb&action=print&method=search_pages&search=$main::in{'search'}&casesen=$main::in{'casesen'}&everything=$main::in{'everything'}"
 onClick="return confirm('     Warning: Are you sure you want to print all the matching pages on paper?\\n');"
 onMouseOver="print.src='$main::gifs/print2.gif';
 window.status='Print'; return true;"
 onMouseOut="print.src='$main::gifs/print1.gif'; return true;"><img
 src="$main::gifs/print1.gif" height=$main::btn_ht width=$main::print_wd
 border=0 alt="Print" name=print
EOF
            print " ></a>";
	}
	elsif ("$button" eq 'print:page') {
            print <<EOF;
<a
 href="$main::script?nb=$main::nb&action=print&method=page&page=$main::in{'page'}"
 onMouseOver="print.src='$main::gifs/print2.gif';
 window.status='Print'; return true;"
 onMouseOut="print.src='$main::gifs/print1.gif'; return true;"><img
 src="$main::gifs/print1.gif" height=$main::btn_ht width=$main::print_wd
 border=0 alt="Print" name=print
EOF
            print " ></a>";
	}
	elsif ("$button" eq 'subscribe') {
	    if ( $main::subscribe == 1 ) {
		print <<EOF;
<a
 href="$main::script?nb=$main::nb&action=subscription"
 onMouseOver="subscribe.src='$main::gifs/subscribe2.gif';
 window.status='Subscribe to change notifications'; return true;"
 onMouseOut="subscribe.src='$main::gifs/subscribe1.gif'; return true;"><img
 src="$main::gifs/subscribe1.gif" height=$main::btn_ht width=$main::subscribe_wd
 border=0 alt="Subscribe" name=subscribe
EOF
                print " ></a>";
	    }

	}
	elsif ("$button" eq 'first') {
	    print <<EOF;
<a
 href="$main::script?nb=$main::nb&action=view&page=1"
 onMouseOver="first.src='$main::gifs/first2.gif';
 window.status='Go to first page'; return true;"
 onMouseOut="first.src='$main::gifs/first1.gif'; return true;"><img
 src="$main::gifs/first1.gif" height=$main::btn_ht width=$main::first_wd  
 border=0 alt="First Page" name=first
EOF
            print " ></a>";
	}
	elsif ("$button" eq 'first0') {
	    print <<EOF;
<img
 src="$main::gifs/first0.gif" height=$main::btn_ht width=$main::first_wd  
 border=0 alt="First Page" 
EOF
            print " >";
	}
	elsif ("$button" eq 'last') {
	    print <<EOF;
<a
 href="$main::script?nb=$main::nb&action=view&page=last"
 onMouseOver="last.src='$main::gifs/last2.gif';
 window.status='Go to last page'; return true;"
 onMouseOut="last.src='$main::gifs/last1.gif'; return true;"><img
 src="$main::gifs/last1.gif" height=$main::btn_ht width=$main::last_wd  
 border=0 alt="Last Page" name=last
EOF
            print " ></a>";
	}
	elsif ("$button" eq 'last0') {
	    print <<EOF;
<img
 src="$main::gifs/last0.gif" height=$main::btn_ht width=$main::last_wd  
 border=0 alt="Last Page"
EOF
            print " >";
	}
	elsif ("$button" eq 'prev') {
	    my $target = $main::in{'page'} - 1;
	    print <<EOF;
<a
 href="$main::script?nb=$main::nb&action=view&page=$target"
 onMouseOver="prev.src='$main::gifs/prev2.gif';
 window.status='Go to previous page'; return true;"
 onMouseOut="prev.src='$main::gifs/prev1.gif'; return true;"><img
 src="$main::gifs/prev1.gif" height=$main::btn_ht width=$main::prev_wd  
 border=0 alt="Previous Page" name=prev
EOF
            print " ></a>";
	}
	elsif ("$button" eq 'prev0') {
	    print <<EOF;
<img
 src="$main::gifs/prev0.gif" height=$main::btn_ht width=$main::prev_wd  
 border=0 alt="Previous Page" 
EOF
            print " >";
	}
	elsif ("$button" eq 'next') {
	    my $target = $main::in{'page'} + 1;
	    print <<EOF;
<a
 href="$main::script?nb=$main::nb&action=view&page=$target"
 onMouseOver="next.src='$main::gifs/next2.gif';
 window.status='Go to next page'; return true;"
 onMouseOut="next.src='$main::gifs/next1.gif'; return true;"><img
 src="$main::gifs/next1.gif" height=$main::btn_ht width=$main::next_wd  
 border=0 alt="Next Page" name=next
EOF
            print " ></a>";
	}
	elsif ("$button" eq 'next0') {
	    print <<EOF;
<img
 src="$main::gifs/next0.gif" height=$main::btn_ht width=$main::next_wd  
 border=0 alt="Next Page" 
EOF
            print " >";
	}

	if (("$button" ne '') && ("$button" ne 'nr')) { print $rend; }
	print "</td>";
    } #foreach $button

    print "</tr></table>\n";
}


###############################################################################
#
#  ( $pagetext ) = &buildTOCText();
#
#  Builts the body text for TOC (Table of pages and objects at end)
#
###############################################################################

sub buildTOCText {

#  Open the index file.
  open( IDX, "<$main::idxfile" ) || &InvalidFile($main::idxfile);
  flock( IDX, $main::LOCK_EX ) if ( $main::flock );
  seek( IDX, 0, 0 );

#  Parse the index file.
  my $text = '';
  my $text2= '';
  my $text3= '';
  while ( <IDX> ) {
    next if ( m/^\s*$/ );		# Blank lines
    next if ( m/^\s*#/ );		# Comment lines

    my ( $tag, $name, $type, $as_is ) = split;

#   next if ( $tag < 0 );		# Skip notebook objects
    if ( $tag < 0 ) {          # NEW -- list objects at the end
      $text2 .= "<li><a href=\"$main::relscript?nb=$main::nb&action=view&" .
	       "page=$tag\">$main::relscript?nb=$main::nb&action=view&page=$tag</a> $type\n";
    }

    if ( "$name" eq '' ) {		# Deleted page
	if ( $main::showdeleted == 1 ) {
	    $text .= "<li>Deleted page\n";
	    $text3 .= "<br>\n";
	}
    }
    elsif ( $tag > 0) {
      $name = "$main::root/$name" if ( $name !~ m/^\// );
      open( FILE, "<$name" ) || &InvalidFile();

#  Scan the file for the author name, creation date, and title.
      my ( $author, $title, $date ) = ( '', '', '' );
      while ( <FILE> ) {
	if    ( m/^<!-- Author: (.*) -->$/ ) { $author = $1; }
	elsif ( m/^<!-- Date: (.*) -->$/ )   { $date = $1; }
	elsif ( m/^<!-- Keywords: .* -->$/ ) { ; }
	elsif ( m/^<!-- Title: (.*) -->$/ )  { $title = $1; }
      }
      close( FILE );
      $text .= "<li><a href=\"$main::relscript?nb=$main::nb&action=view&" .
	       "page=$tag\"><nobr>$title\n($author)\n</a></nobr>\n";
      $text3 .= "<nobr>[Last Modified $date]</nobr><br>\n";
    }
  }

  flock( IDX, $main::LOCK_UN ) if ( $main::flock );
  close( IDX );

    my $pagetext = <<EOF;
<table>
<tr>
<td align=left>
<ol>
$text
</ol>
</td>
<td align=right>
$text3 
</td>
</tr>
</table>
<hr>
<center><b> Other uploaded notebook objects</b></center>
<ul>
$text2
</ul>
EOF

  return $pagetext;
}


###############################################################################
#
#  ( @pagelist ) = &searchNotebook( $search, $casesen, $everything );
#
#  Search the notebook for pages matching the criteria and return a list
#  of matching page numbers. Search text is in $search, $casesen -> case sensitive
#  and $everything -> search the context text or just the header info (title etc.)
#
###############################################################################

sub searchNotebook {

    my ( $search, $casesen, $everything ) = ( $_[0], $_[1], $_[2] );
    if ( "$casesen" ne '1' )    { $casesen = 0; }
    if ( "$everything" ne '1' ) { $everything = 0; }

    my @pagelist = ();
    my ( $pageno, $pages ) = ( 0, 0 );

#  If we have something to search for, open the index file and parse it.
  if ( "$search" ne '' ) {
    
    open( IDX, "<$main::idxfile" ) || &InvalidFile();
    flock( IDX, $main::LOCK_EX ) if ( $main::flock );
    seek( IDX, 0, 0 );

    while ( <IDX> ) {
      next if ( m/^\s*$/ );		# Blank lines
      next if ( m/^\s*#/ );		# Comment lines

      my ( $tag, $name, $type, $as_is ) = split;

      next if ( $tag < 0 );		# Skip notebook objects
      $pages++;
      next if ( "$name" eq '' );	# Skip deleted pages

      $pageno = $pages;
      $name = "$main::root/$name" if ( $name !~ m/^\// );
      open( FILE, "<$name" ) || &InvalidFile($name);

#  Scan the file for the author name, creation date, and title.
      my ( $author, $body, $date, $kwords, $title, $changes ) = ( '', '', '', '', '' );
      while ( <FILE> ) {
	if    ( m/^<!-- Author: (.*) -->$/ )   { $author = $1; }
	elsif ( m/^<!-- Date: (.*) -->$/ )     { $date = $1; }
	elsif ( m/^<!-- Keywords: (.*) -->$/ ) { $kwords = $1; }
	elsif ( m/^<!-- Title: (.*) -->$/ )    { $title = $1; }
	elsif ( m/^<!-- Changes: (.*) -->$/ )  { $changes = $1; }
	else				       { $body .= $_; }
      }
      close( FILE );

#  Search this entry.
      my ( $found ) = ( 0 );
      $found = ( $kwords =~ m/$search/io );
      if ( ! $found) {
	  $found = ( $title =~ m/$search/io );
	  if ( ! $found ) {
	      $found = ( $author =~ m/$search/io );
	      if ( ! $found ) {
		  $found = ( $date =~ m/$search/io );
		  if ( ! $found ) {
		      $found = ( $changes =~ m/$search/io );
		  }
	      }
	  }
      }
      if (( ! $found ) && ( "$everything" eq '1' )) {
	if ( "$casesen" eq '1' ) {
	  $found = ( $body =~ m/$search/o );
	}
	else {
	  $found = ( $body =~ m/$search/io );
	}
      }

      if ( $found ) {
	  @pagelist = ( @pagelist, $pageno );
      }
    }

    flock( IDX, $main::LOCK_UN ) if ( $main::flock );
    close( IDX );
  }

  return @pagelist;
}


###############################################################################
#
#  ( @pagelist ) = &searchNotebookChanges( $since );
#
#  Search the notebook for pages that have changed since $since secs after epoch 
#   (as returned by time()) and return a list of matching page numbers. 
#
###############################################################################

sub searchNotebookChanges {

    my $since = $_[0];
    
    my @pagelist = ();
    my ( $pageno, $pages ) = ( 0, 0 );
    
#  Open the index file and parse it.

    open( IDX, "<$main::idxfile" ) || &InvalidFile();
    flock( IDX, $main::LOCK_EX ) if ( $main::flock );
    seek( IDX, 0, 0 );
    
    while ( <IDX> ) {
	next if ( m/^\s*$/ );		# Blank lines
#	next if ( m/^\s*#/ );		# Comment lines
		  
	my ( $tag, $name, $type, $as_is ) = split;
	
	next if ( $tag < 0 );		# Skip notebook objects
	$pages++;
	next if ( "$name" eq '' );	# Skip deleted pages
	
	$pageno = $pages;
        $name = "$main::root/$name" if ( $name !~ m/^\// );
        open( FILE, "<$name" ) || &InvalidFile($name);
	
#  Scan the file for the author name, creation date, and title.
        my ( $author, $body, $date, $kwords, $title ) = ( '', '', '', '' );
        while ( <FILE> ) {
	    if    ( m/^<!-- Author: (.*) -->$/ )   { $author = $1; }
	    elsif ( m/^<!-- Date: (.*) -->$/ )     { $date = $1; }
	    elsif ( m/^<!-- Keywords: (.*) -->$/ ) { $kwords = $1; }
	    elsif ( m/^<!-- Title: (.*) -->$/ )    { $title = $1; }
	    else				   { $body .= $_; }
        }
        close( FILE );

#  Check if changed since $since
	my $changedsecs = convertStringDateToSecsSinceEpoch($date);
	if ($changedsecs > $since) {
	    @pagelist = ( @pagelist, $pageno );
	}
    }
    
    flock( IDX, $main::LOCK_UN ) if ( $main::flock );
    close( IDX );
    
    
    return @pagelist;
}


###############################################################################
#
#  ( @info = ( $author, $date, $kwords, $title, $changes, $filename, $filetype ) ) = &getPageInfo( $pageno );
#
# Read page file for $pageno and extract the info
#  (will all be '' if not a text or html file (or deleted))
#
###############################################################################

sub getPageInfo {

    my $pageno = $_[0];
    
    if ($pageno < 0) { return (); }

    my ( $author, $date, $kwords, $title, $changes, $filename, $filetype) = ( '', '', '', '', '', '', '');
    
# Open the index file and scan through to find $pageno
    my ( $page, $pages ) = ( 0, 0 );
    open( IDX, "<$main::idxfile" ) || &InvalidFile();
    flock( IDX, $main::LOCK_EX ) if ( $main::flock );
    seek( IDX, 0, 0 );

    my $gotinfo = 0;
    SCANINDEX: while ( <IDX> ) {
      next if ( m/^\s*$/ );		# Blank lines
      next if ( m/^\s*#/ );		# Comment lines

      my ( $tag, $name, $type, $as_is ) = split;
      $filename = $name;
      $filetype  = $type;

      next if ( $tag < 0 );		# Skip notebook objects
      $pages++;
      next if ( "$name" eq '' );	# Skip deleted pages

      $page = $pages;
      if ($page == $pageno) {

        $name = "$main::root/$name" if ( $name !~ m/^\// );
	open( FILE, "<$name" ) || &InvalidFile($name);

        #  Scan the file for the author name, creation date, and title.
	while ( <FILE> ) {
	    if    ( m/^<!-- Author: (.*) -->$/ )   { $author = $1; }
	    elsif ( m/^<!-- Date: (.*) -->$/ )     { $date = $1; }
	    elsif ( m/^<!-- Keywords: (.*) -->$/ ) { $kwords = $1; }
	    elsif ( m/^<!-- Title: (.*) -->$/ )    { $title = $1; }
	    elsif ( m/^<!-- Changes: (.*) -->$/ )  { $changes = $1; }
	    else  { $gotinfo = 1; close (FILE); last SCANINDEX; }
	}
	close( FILE );
      }
    }

    flock( IDX, $main::LOCK_UN ) if ( $main::flock );
    close( IDX );
  
    if ( $gotinfo == 1) {
	return ( $author, $date, $kwords, $title, $changes, $filename, $filetype );
    }

    return ();
}


###############################################################################
#
#  ( ) = &loadSubscriptions( $filename );
#
#  Loads the subscription list for notebooks %main::notebook from $file
#
###############################################################################

sub loadSubscriptions
{
    my ( $name ) = @_;

    # clear out $main::notebook{$notebook}{'subscriptions'} first
    foreach ( keys %main::notebook ) {
	$main::notebook{$_}{'subscriptions'} = [];
	$main::notenook{$_}{'last_notify_secs'} = 0;
	$main::notenook{$_}{'last_notify_str'} = 'created';
    }

    # build per-notebook e-mail address subscription lists from file
    if ( -e $name ) {
	
	open( FILE, "<$name" ) || &InvalidFile($name);
	flock( FILE, $main::LOCK_EX ) if ( $main::flock );

	my $lastnotify = 0; # secs since epoch
	my $lastnotifystr = '';
	my $email = '';
	my $name = '';
	my $scriptURL = '';
	while ( <FILE> ) {
	    next if ( m/^\s*$/ );		# Blank lines
	    next if ( m/^\s*#/ );		# Comment lines
	    my ( $notebook, $entrytype, @rest ) = split '::' ;
            chomp @rest;

	    if ("$entrytype" eq 'subscriptions') {
		my $nameemailstr = '';
		foreach $nameemailstr ( @rest ) {
		    my @nameemaillist = split ',',$nameemailstr ;
		    $name = $nameemaillist[0];
		    $email= $nameemaillist[1];
		    push @{$main::notebook{$notebook}{'subscriptions'}}, [ $name, $email];
		}
	    }
	    elsif ("$entrytype" eq 'notified') {
		$lastnotify = $rest[0];
		$lastnotifystr = "@rest[1 .. scalar(@rest) ]";

		$main::notebook{$notebook}{'last_notify_secs'} = $lastnotify;
		$main::notebook{$notebook}{'last_notify_str'} = $lastnotifystr;
	    }
	    elsif ("$entrytype" eq 'scriptURL') {
		$scriptURL = $rest[0];
		$main::notebook{$notebook}{'scriptURL'} = $scriptURL;
	    }
	
	}

	flock( FILE, $main::LOCK_UN ) if ( $main::flock );
	close(FILE);
    }

}


###############################################################################
#
#  ( ) = &saveSubscriptions( $filename );
#
#  Saves the subscription list for notebooks %main::notebook to $file
#
###############################################################################

sub saveSubscriptions
{
    my ( $name ) = @_;

    open( FILE, ">$name" ) || &InvalidFile($name);
    flock( FILE, $main::LOCK_EX ) if ( $main::flock );

    print FILE "# e-note subscription database\n";

    my $subentry = [];
    foreach ( keys %main::notebook ) {
	my $notebook = $_;
	my $notifiedsecs = "$main::notebook{$notebook}{'last_notify_secs'}";
	if ("$notifiedsecs" eq '') { $notifiedsecs="0"; }
	my $notifiedstr = "$main::notebook{$notebook}{'last_notify_str'}";
	if ("$notifiedstr" eq '') { $notifiedstr = "created"; }
	#store the script URL for 'native' notebooks (those in the enote.pl
	# that invoked us.  The notifications themselves are send out from
	# a command-line invocation - hence it will need the URL for links
	if (("$main::notebook{$notebook}{'native'}" eq '1') &&
	    ("$main::invoked" eq 'webserver')) {
	    print FILE "$notebook\:\:scriptURL\:\:$main::script\n";
	}
	else {
	    print FILE "$notebook\:\:scriptURL\:\:$main::notebook{$notebook}{'scriptURL'}\n";
	}
	#store last notified time
	print FILE "$notebook\:\:notified\:\:$notifiedsecs\:\:$notifiedstr\n";
	#store subscription list
	print FILE "$notebook\:\:subscriptions";
	foreach $subentry ( @{$main::notebook{$notebook}{'subscriptions'}} ) {
	    print FILE "\:\:$$subentry[0],$$subentry[1]";
	}
	print FILE "\n";
    }    

    flock( FILE, $main::LOCK_UN ) if ( $main::flock );
    close(FILE);
}


###############################################################################
#
#  ( $added ) = &addSubscription( $notebook, $name, $email );
#
#  Adds a notification subscription for the name $name and e-mail address 
#    $email for the notebook $notebook .
#
###############################################################################

sub addSubscription
{
    my $notebook = $_[0];
    my $name = $_[1];
    my $email = $_[2];
    
    &loadSubscriptions( $main::subscription );

    #check if already subscribed
    my $found = 0;
    foreach ( @{$main::notebook{$notebook}{'subscriptions'}} ) {
	if ($$_[0] eq $name) { $found=1; }
    }
    
    if (! $found) { # no, add to list
	push @{$main::notebook{$notebook}{'subscriptions'}}, [$name, $email];
	&saveSubscriptions( $main::subscription );
    }

    return ! $found;
}


###############################################################################
#
#  ( $removed ) = &removeSubscription( $notebook, $name, $email );
#
#  Removes notification subscription for the name $name for the 
#   notebook $notebook (no-op if there is no such subscription).
#  The email address listed for $name must match $email for unsubscription
#   to succeed.
#
###############################################################################

sub removeSubscription
{
    my $notebook = $_[0];
    my $name = $_[1];
    my $email = $_[2];
    
    &loadSubscriptions( $main::subscription );

    #check if subscribed
    my $found = -1;
    my $offset = 0;
    foreach ( @{$main::notebook{$notebook}{'subscriptions'}} ) {
	if (($$_[0] eq $name) && ($$_[1] eq $email)) { $found=$offset; }
	$offset++;
    }
    
    if ($found != -1) { # yes, remove from list
	splice @{$main::notebook{$notebook}{'subscriptions'}}, $found, 1; 
	&saveSubscriptions( $main::subscription );
    }

    return ($found != -1);
}

###############################################################################
#
#  ( ) = &outputSubscriptions( $notebook );
#
#  Output the subscription list for $notebook (as HTML)
#
###############################################################################
sub outputSubscriptions
{
    my $notebook = $_[0];
    &loadSubscriptions( $main::subscription );
    if ( scalar(@{$main::notebook{$notebook}{'subscriptions'}}) > 0) {
	print "<hr><h2>Current Subscriptions</h2>\n";
	print "<p><ul>\n";
	foreach ( @{$main::notebook{$main::nb}{'subscriptions'}} ) {
	    print "<li>$$_[0]";
	    if ($main::showemails == 1) { 
		print " - <a href=\"mailto:$$_[1]\">$$_[1]</a>"; 
	    }
	    print "</li>\n";
	}
	print "</ul></p><hr>\n";

	if ($main::showemails == 1) {
	    print "<i>(if you prefer not to have email addresses shown here, ask the notebook administrator to";
	    print " add the line \$main::showemails = 0 to the enote.pl configuration file.)</i>\n";
	}

    }
    else {
	print "<hr><p>No current subscriptions.</p>\n";
    }
}

###############################################################################
#
#  ( $anychanges ) = &notifySubscribers();
#
# Checks for notebook pages that have changes since the last notification 
#  and e-mails all subscribers. (checks all notebooks in %main::notebook)
#
###############################################################################

sub notifySubscribers
{
    #First, construct the change message text for each notebook.
    # Also construct a list of changed notebooks for each subscriber
    #Next, email a message to each subscriber, consisting of a message header,
    #  the change message text for each notebook to which they are subscribed,
    #  and a message footer.


    my $anychanges = 0;
    &loadSubscriptions( $main::subscription );
    # keep open to lock
    open( FILE, "<$main::subscription" ) || &InvalidFile($main::subscription);
    flock( FILE, $main::LOCK_EX ) if ( $main::flock );

    my %notebookchanges = {};      # change messages by notebook
    my %subscribernotebooks = {};  # changed notebooks (subscribed) by subscriber ('::' delim. string)
    my %subscribernameemail = {};  # seperate name & email strings by subscriber (which is a combo. name&email string)

    my $notebook = '';
    foreach $notebook ( keys %main::notebook ) {

	if ($main::notebook{$notebook}{'native'} == 1) { # ignore notebooks belonging to other enote.pl's

	    $main::notebook{$notebook}{'changed_pages'} = [];
	    my $notifiedsecs = $main::notebook{$notebook}{'last_notify_secs'};

	    # setup vars search expects for this notebook
	    $main::nb = $notebook;
	    $main::root	 = $main::notebook{$main::nb}{'dir'};
	    $main::idxfile = "$main::root/index.lst";
	    
	    # find changed pages
	    my @changedpages = searchNotebookChanges( $notifiedsecs );
	    push @{$main::notebook{$notebook}{'changed_pages'}}, @changedpages;
	    
	    #print "notebook:$notebook changed:@changedpages\n";

	    $notebookchanges{$notebook} = "<p>no changes.</p>";

	    if (scalar(@changedpages) > 0) { # any changes?

		# generate change notification messages for $notebook
		my $lastnotified = "never";
		if ($notifiedsecs > 0) {
		    $lastnotified = gmtime($notifiedsecs)." GMT  (localtime: ".localtime($notifiedsecs).")";
		}
		my $message = <<EOF;
<hr>
<h3>Notebook: $main::notebook{$notebook}{'title'} ($notebook)</h3>
<p><b>Last update notification sent on $lastnotified</b>
EOF
		if ("$main::notebook{$notebook}{'scriptURL'}" ne '') {
		    $message .= <<EOF;
<i>Click
 <a href="$main::notebook{$notebook}{'scriptURL'}?nb=$notebook&action=subscription&name=\%22!!name\%22&email=\%22!!email\%22">here to unsubscribe</a>
 from this notebook.</i>
EOF
                }
                $message .= <<EOF;
</p>
<ul>
EOF

                my $page = 0;
		foreach $page ( @changedpages ) {
		    my @pageinfo = getPageInfo( $page );
		    my $title = $pageinfo[3];
		    my $author = $pageinfo[0];
		    my $changes = $pageinfo[4];
		    if ($changes =~ m/([^|]+)|(.*)/) { # get most recient change
			$changes = $1;
		    }
		    $message .= "<li>Page $page. <a href=\"$main::notebook{$notebook}{'scriptURL'}?nb=$notebook&action=view&" .
			"page=$page\"><b>$title</b>\n($author)</a>  <i>[$changes]</i></li>\n";
		}

		$message .= "</ul>\n";

		$notebookchanges{$notebook} = $message;

		# add this notebook to the list of subscribed notebooks for each subscriber
		my ( $name, $email, $nameemail ) = ('','','');
		foreach ( @{$main::notebook{$notebook}{'subscriptions'}}) {
		    $name = $$_[0];
		    $email = $$_[1];
		    $nameemail = "$name \<$email\>";
		    
		    if (defined $subscribernotebooks{$nameemail}) {
			$subscribernotebooks{$nameemail} .= "::$notebook";
		    }
		    else {
			$subscribernotebooks{$nameemail} = "$notebook";
		    }

		    @subscribernameemail{$nameemail} = [ $name, $email ];
		}

		$anychanges = 1;
	    } 
	    
	    #update notification time
	    my $currenttime = time();
	    $main::notebook{$notebook}{'last_notify_secs'} = $currenttime;
	    $main::notebook{$notebook}{'last_notify_str'} = gmtime($currenttime);
	}

    } #foreach $notebook


    # message header
    my $messageheader = <<EOF;
<html>
<head>
</head>
<body>
<h2>Automated E-Note Update Notification</h2>
<p>Sent to !!name &lt;!!email&gt;.
  The following is a list of pages from your subscribed notebooks, that have been updated 
since the last update notification e-mail.</p>
EOF

    my $messagefooter = <<EOF;
<hr>
<p>You have recieved this e-mail because you are subscribed to E-Note update
notifications.  If you no longer wish to recieve updates, go to the appropriate notebook
subscription page and unsubscribe.</p>
</body></html>
EOF


    # now, for each subscriber, send an email of changes to any subscribed notebooks
    print "Sending email...\n";
    foreach (keys %subscribernotebooks) {
	my $subscriber = $_;

	if ( defined $subscribernotebooks{$subscriber} ) {
	    my $message = $messageheader;
	    my @notebooks = split('::', $subscribernotebooks{$subscriber} );
	    foreach ( @notebooks ) {
		my $notebook = $_;
		$message .= $notebookchanges{$notebook};
	    }
	    
	    $message .= $messagefooter;
	    
	    #send out
	    my $name = $subscribernameemail{$subscriber}[0];
	    my $email = $subscribernameemail{$subscriber}[1];
	    my $replyto = $main::subscriptionReplyTo;
	    my @subscriberlist = [ "$name", "$email" ];
	    emailMessage( "E-Note update",
			  $message, $replyto, \@subscriberlist );
	}
    }
    print "Done.\n";





    flock( FILE, $main::LOCK_UN ) if ( $main::flock );
    close(FILE);
    if ($anychanges == 1) {
	&saveSubscriptions( $main::subscription );
    }
    return $anychanges;
}


###############################################################################
#
#  ( ) = &emailMessage( $subject, $message, $replyto, \@recipients );
#
#  Send an e-mail with body $message and subject line $subject to the
#   list of recipient contained in \@recipients.
#  @recipients is a list of list references, each containing the pair
#   of strings $name, $email. e.g. @recipients = ( ['david','d@p.com'],[...] )
#  ($message assumed to be text/html )
#
###############################################################################

sub emailMessage {
    my $subject = $_[0];
    my $message = $_[1];
    my $replyto = $_[2];
    my @recipients = @{$_[3]};

    my $sendmail = $main::sendmail.' -t';
    #!!!my $sendmail = 'cat';
    my $to = '';
    my $name = '';
    my $email = '';

    #Send each individually (so we can customize the name & email)
    foreach ( @recipients ) {
	$name = $$_[0];
	$email = $$_[1];
	$to = "$name \<$email\>";
	
	$message =~ s/!!name/$name/g;
	$message =~ s/!!email/$email/g;

	if ($main::useSendmailCommand == 1) {

	    open(SENDMAIL, "|$sendmail") || &InvalidFile($sendmail);
	    print SENDMAIL "From: E-Note \<no_return\@address.com\>\n";
	    print SENDMAIL "Reply-to: $replyto\n";
	    print SENDMAIL "Subject: $subject\n";
	    print SENDMAIL "To: $to\n";
	    print SENDMAIL "Content-type: text/html\n\n";
	    print SENDMAIL $message;
	    close(SENDMAIL);
	}
	else {
	    #use Sendmail package
	    my %mail = ( To => $to,
			 From => $replyto,
			 'Content-Type' => "text/html",
			 Subject => "$subject",
			 Smtp => $main::SMTPHost,
			 Message => $message,
		       );
	    Mail::Sendmail::sendmail(%mail) || print "Error sending mail: $Mail::Sendmail::error\n";
	    #print "************************************************\n";
	    #print "************************************************\n";
	    #print "*** Sent:\nTo:$to\nFrom:E-Note \<$replyto\>\nMessage:$message\nHost:$main::SMTPHost\n";
	}
    }

}


###############################################################################
#
#  ( ) = &parseForXML();
#
#  Searches a text string (as returned by the form variable BODY)
#  for the begin and ending tags <enote:showtags> and </enote:showtags>.
#  Between these two tags all occurances of "<" are replaced by "&lt;"
#  This prevents the browser from hiding notebook input that looks like
#  HTML or XML but is really part of the text.
#
###############################################################################

sub parseForXML 
{
  my ( $text ) = @_; 
  my ( @blocks ) = split( m/(\<enote\:showtags\>)|(\<\/enote\:showtags\>)/i, $text);
  my ( $xml ) = 0;

  foreach ( @blocks )
  {
    if ( $_ =~ m/\<enote\:showtags\>/i )
    {
        $xml = 1;
    }
    elsif ( $_ =~ m/\<\/enote\:showtags\>/i )
    {
        $xml = 0;
    }
    elsif ( $xml == 1 )
    {
       $_ =~ s/\</\&lt\;/g;
    }
    else
    {
      # do nothing
    }
  }
  $text = join( "", @blocks );      
}


###############################################################################
#
#  ( ) = &InvalidFile();
#
#  Prints an error message for invalid file and exits the script.
#
###############################################################################

sub InvalidFile {

  my $filename = @_ ? " on @_[0]" : '';

  if ($main::invoked eq 'command') {
      print "Configuration error: file error or invalid file request $filename.\n";
  }
  elsif ($main::invoked eq 'webserver') {

  print <<EOF;
Content-type: text/html

<html>
<head>
<title>File Error</title>
</head>
<body background="$main::backgrnd">
<h1>File Error</h1>
<p><b>Configuration error: the server encountered a file error or an
invalid file request$filename.</b></p>
Please use your browser to go back.<hr>
</body>
</html>
EOF

}
  exit $main::RET_BAD_FILE;

}


use Time::Local 'timegm';
use Time::Local 'timegm_nocheck';
use POSIX qw(strftime);

###############################################################################
#
#  $secs = &convertStringDateToSecsSinceEpoch( $dateGMT );
#
#  Convert a date/time string like 'Tue May  7 14:47:53 2002 (GMT)'
#   into seconds since epoch (ala time() ).
#
###############################################################################

sub convertStringDateToSecsSinceEpoch {
    my $date = $_[0];

    my $nmon = 0;
    if ($date =~ m/(\S+)\s+(\S+)\s+(\d+)\s(\d\d):(\d\d):(\d\d)\s(\d\d\d\d)/) {

	my ( $sec,$min,$hours,$mday,$mon,$year ) = ( $6, $5, $4, $3, $2, $7);
	$mon = uc($mon);
	if ($mon =~ m/JAN.*/) {
	    $nmon = 0;
	} elsif ($mon =~ m/FEB.*/) {
	    $nmon = 1;
	} elsif ($mon =~ m/MAR.*/) {
	    $nmon = 2;
	} elsif ($mon =~ m/APR.*/) {
	    $nmon = 3;
	} elsif ($mon =~ m/MAY.*/) {
	    $nmon = 4;
	} elsif ($mon =~ m/JUN.*/) {
	    $nmon = 5;
	} elsif ($mon =~ m/JUL.*/) {
	    $nmon = 6;
	} elsif ($mon =~ m/AUG.*/) {
	    $nmon = 7;
	} elsif ($mon =~ m/SEP.*/) {
	    $nmon = 8;
	} elsif ($mon =~ m/OCT.*/) {
	    $nmon = 9;
	} elsif ($mon =~ m/NOV.*/) {
	    $nmon = 10;
	} elsif ($mon =~ m/DEC.*/) {
	    $nmon = 11;
	}
	else {
	    return 0;
	}

	$year = $year - 1900;
	my $time = timegm($sec,$min,$hours,$mday,$nmon,$year);
	return $time;
    }
 
    return 0;
}


###############################################################################
#
#  $num = &ParseCGIParms();
#
#  Parse the CGI parameters.
#
###############################################################################

sub ParseCGIParms {

  my $ret = 0;
  my $read_offset = 0;
  my $read_length = $ENV{'CONTENT_LENGTH'};

  $main::in = ''; @main::in = (); %main::in = ();
  $main::up = ''; @main::up = (); %main::up = ();
  if ( $ENV{'REQUEST_METHOD'} eq 'GET'  ) {
    $main::in = $ENV{'QUERY_STRING'}; }
  elsif ( $ENV{'REQUEST_METHOD'} eq 'POST' ) {
    binmode( STDIN );

    while($ret = read( STDIN, $main::in, $read_length-$read_offset, $read_offset)) {
      $read_offset += $ret;
    }
  }
  if ( $ENV{'CONTENT_TYPE'} =~ m|^multipart/form-data| ) {
    my ( $s7 ) = $ENV{'CONTENT_TYPE'} =~ /boundary=(\S+)/;
    $s7 = "--$s7"; my $i2 = length( $s7 ); return 0 if $i2 <= 0;
    $_ = index( $main::in, 'Content-' ) - $i2;
    my $s4 = ( $_ > 0 ) ? substr( $main::in, $i2, $_ ) : '';
    my $i1 = length( $s4 ); return 0 if $i1 <= 0;
    my $s5 = '^Content-[Dd]isposition: form-data; name="(.+?)"';
    $s5 .= '(|; filename="(.*?)")$';
    my $s6 = '^Content-[Tt]ype: (\S+)$';
    my ( $body ) = ( '' );
    foreach ( split( /$s7/, $main::in )) {
      next if ( ! m/^$s4/o );
      my $s3 = substr( $_, 0, index( $_, "$s4$s4" ) + 2 * $i1 );
      next if ( "$s3" eq '' );
      my ( $key, $b1, $mime, $name ) = ( '', 0, '', '' );
      my ( $s1, $s2 ) = ( 0, index( $s3, $s4 ));
      while ( $s2 >= 0 ) {
	my $s = substr( $s3, $s1, $s2 - $s1 );
	$s1 = $s2 + $i1; $s2 = index( $s3, $s4, $s1 );
	if ( $s =~ m-$s5-o ) {
	  $key = "$1"; $b1 = ( "$2" ne '' ); $name = "$3"; }
	elsif ( $s =~ m-$s6-o ) { $mime = "$1"; }
	else { ; }}
      next if ( "$key" eq '' );
      if ( ! $b1 ) {
	$main::in{$key} .= "\0" if ( defined( $main::in{$key} ));
	$main::in{$key} .= substr( $_, $s1, -$i1 ); next; }
      if ( "$name" ne '' ) {
	$main::up{$key}{'mime'} = "$mime"; $main::up{$key}{'name'} = "$name";
	$main::up{$key}{'data'} = substr( $_, $s1, -$i1 );
	$main::up{$key}{'size'} = length( $main::up{$key}{'data'} ); next; }}}
  else {
    @main::in = split( /[&;]/, $main::in ); my $i;
    foreach $i (0 .. $#main::in) {
      $main::in[$i] =~ s/\+/ /g;
      my ($key, $val) = split( /=/, $main::in[$i], 2 );
      $key =~ s/%(..)/pack('c',hex($1))/ge;
      $val =~ s/%(..)/pack('c',hex($1))/ge;
      $main::in{$key} .= "\0" if (defined($main::in{$key}));
      $main::in{$key} .= $val; }}
  return scalar(@main::in); 
}

1;

__END__

