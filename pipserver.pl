#!/usr/local/bin/perl -w
# $Id: pipserver.pl,v 1.15 2001/05/11 01:15:42 schwartz Exp $

use 5.6.0;
use warnings;
use strict;

my $HOME  = $ENV{'PIPHOME'} || die;
my $pm    = "$HOME/bin/pipmaker";
my $SPOOL = "$HOME/spool";
my $new   = "$SPOOL/new";
my $work  = "$SPOOL/work";

-x $pm || die;
-d $new || die;
-d $work || die;

my $delay = 10;          # seconds

my $batch = "at -qb now";
my $use_batch = 1;

sub batch
{
    my $job = shift;
    if (!open(BATCH, "|$batch >/dev/null 2>&1")) {
	warn("open: $batch: $!");
	return 0;
    }
    if (!print BATCH qq{$pm $job\n}) {
	warn("print: '$pm' '$job': $!");
	return 0;
    }
    if (!close(BATCH)) {
	warn $! ? "error closing pipe: $!"
		: "exit status from pipe: $?";
	return 0;
    }
    return 1;
}

sub onejob
{
    my $job = shift;
    if ($use_batch) {
	return &batch($job);
    } else {
	return system($pm, $job) == 0;
    }
}

sub workdirs
{
    my @d;
    if (opendir(D,".")) {
	@d = grep /^[^.]+\.[^.]+\.[^.]+$/, readdir(D);
	closedir(D);
    }
    return @d;
}

$|=1;

# XXX - no mutual exclusion.  Two running copies of this will be bad.

chdir "$work" or die "cannot chdir $work: $!";
foreach my $d (&workdirs) {
    open(F, ">>", "$d/dead-job");
    printf F "%d\n", time();
    close(F);
    # XXX - we don't notify the user!
    rename($d, "../err/$d") or die("cannot move $d to err: $!\n");
    # Perhaps we should resubmit the job instead?
}

chdir "$new" or die "cannot chdir $new: $!";
for (;;) {
    my $err = 0;
    my $d = "";
    sleep($delay);  # poll for new work
    foreach $d (&workdirs) {
    	# print "$d\n";
	if (! rename($d, "../work/$d")) {
	    warn("cannot move $d to work: $!\n");
	    ++$err;
        } elsif (! onejob($d)) {
	    warn("batch submission failed");
	    rename("../work/$d", $d) or warn("help! can't re-rename: $!");
	    ++$err;
	}
	sleep($delay*10) if $err > 0;
    }
}

