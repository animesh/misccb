#! /usr/local/bin/perl -w

# rcsfreeze - assign a symbolic revision number to a configuration of RCS files

#Id: rcsfreeze.sh,v 4.6 1993/11/03 17:42:27 eggert Exp
# Perl version by Ray McVay
# $Id$

#       The idea is to run rcsfreeze each time a new version is checked
#       in. A unique symbolic revision number (C_[number], where number
#       is increased each time rcsfreeze is run) is then assigned to the most
#       recent revision of each RCS file of the main trunk.
#
#       If the command is invoked with an argument, then this
#       argument is used as the symbolic name to freeze a configuration.
#       The unique identifier is still generated
#       and is listed in the log file but it will not appear as
#       part of the symbolic revision name in the actual RCS file.
#
#       A log message is requested from the user which is saved for future
#       references.
#
#       The shell script works only on all RCS files at one time.
#       It is important that all changed files are checked in (there are
#       no precautions against any error in this respect).
#       file names:
#       {RCS/}.rcsfreeze.ver	version number
#       {RCS/}.rscfreeze.log	log messages, most recent first

@DATE = localtime(time);
$DATE = sprintf("%4d/%02d/%02d %02d:%02d:%02d",
	$DATE[5]+1900, $DATE[4]+1, $DATE[3], $DATE[2], $DATE[1], $DATE[0]);
	
# Check whether we have an RCS subdirectory, so we can have the right
# prefix for our paths.
# Also, make a guess as to what kind of directory separator char we
# should use by examining the PATH
if ( -d 'RCS')
{
	$RCSDIR = 'RCS';
	$EXT = '';
}
else
{
	$RCSDIR = '';
	$EXT = ',v';
}
if ( $ENV{'PATH'} =~ /\// )
{
	$DIRSEP = '/';
}
else 
{
	$DIRSEP = '\\';
}

# Version number stuff, log message file
$VERSIONFILE = qq(${RCSDIR}${DIRSEP}.rcsfreeze.ver);
$LOGFILE = qq(${RCSDIR}${DIRSEP}.rcsfreeze.log);
# Initialize, rcsfreeze never run before in the current directory
if (! -e $VERSIONFILE )
{
	open VERFILE, ">$VERSIONFILE" or die "Can't open $VERSIONFILE: $!\n";
	open LOGFILE, ">>$LOGFILE" or die "Can't open $LOGFILE: $!\n";
	print VERFILE '0';
	print LOGFILE '0';
	close VERFILE;
	close LOGFILE;
}

# Get Version number, increase it, write back to file.
open VERFILE, "+<$VERSIONFILE" or die "Can't open $VERSIONFILE: $!\n";
$VERSIONNUMBER = <VERFILE>;
$VERSIONNUMBER = $VERSIONNUMBER + 1;
seek VERFILE, 0, 0;
truncate VERFILE, 0;
print VERFILE $VERSIONNUMBER;

# Symbolic Revision Number
$SYMREV = 'C_' . $VERSIONNUMBER;

# Allow the user to give a meaningful symbolic name to the revision.
$SYMREVNAME = "";
$SYMREVNAME .= "$ARGV[0]-" if $ARGV[0];
$SYMREVNAME .= $SYMREV;
print	"rcsfreeze: symbolic revision number computed: \"${SYMREV}\"\n",
		"rcsfreeze: symbolic revision number used:     \"${SYMREVNAME}\"\n",
		"rcsfreeze: the two differ only when rcsfreeze invoked with argument\n\n",
		"Enter a log message, summarizing changes (end with EOF or single '.'):\n",
		"Version: $SYMREVNAME($SYMREV), Date: $DATE -----------\n";

# Stamp the logfile. Because we order the logfile the most recent
# first we will have to save everything right now in a temporary file.
#TMPLOG=/tmp/rcsfrz$$
#trap 'rm -f $TMPLOG; exit 1' 1 2 13 15
# Perl don't need no stinkin' temp file
open OLDLOG, $LOGFILE;
@oldlog = <OLDLOG>;
close OLDLOG;

# Now ask for a log message, continously add to the log file
@newlog = ("Version: $SYMREVNAME($SYMREV), Date: $DATE -----------\n");
while (<STDIN>)
{
	last if $_ eq ".\n";
	push @newlog, ($_);
}
push @newlog, ("-----------\n");

# combine old and new logfiles
open LOG, ">$LOGFILE" or die "Can't write to $LOGFILE: $!\n";
for ( $i = 0; $i <= $#newlog; $i++ )
{
	print LOG $newlog[$i];
}
for ( $i = 0; $i <= $#oldlog; $i++ )
{
	print LOG $oldlog[$i];
}
close LOG;

# Now the real work begins by assigning a symbolic revision number
# to each rcs file.  Take the most recent version on the default branch.

# If there are any .*,v files, throw them in too.
# But ignore RCS/.* files that do not end in ,v.
@VERFILES = ();

opendir VDIR, $RCSDIR;
@vfiles = readdir VDIR;
closedir VDIR;

foreach ( grep /^[^.].*$/, @vfiles )
{
	if ( -f $_ )
	{
		push @VERFILES, $_;
	}
}

foreach ( grep /\..*\,v/, @vfiles )
{
	if ( -f $_ )
	{
		push @VERFILES, $_;
	}
}

#print	"Test info:\n",
#		"RCSDIR = $RCSDIR\n",
#		"DIRSEP = $DIRSEP\n",
#		"EXT = $EXT\n",
#		"vfiles = @vfiles\n",
#		"VERFILES = @VERFILES\n\n";

foreach (@VERFILES)
{
	print "Freezing ${RCSDIR}${DIRSEP}$_\n";
	system("rcs -q -n${SYMREVNAME}: ${RCSDIR}${DIRSEP}$_") == 0
		or die "system(rcs -q -n${SYMREVNAME}: ${RCSDIR}${DIRSEP}$_) failed: $!\n";
}

