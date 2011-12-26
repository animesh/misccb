#!/usr/bin/env perl
#
###########################################################################
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received (LICENSE.txt) a copy of the GNU General Public 
# License along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
###########################################################################
#
# $Id: apply_assembler.pl,v 1.5 2006/08/03 22:22:50 catmandew Exp $
#

use strict;
use English;

# Launch the Celera Assembler on datasets found in subdirectories.
# Look for tmp.frg in immediate subdirectories of current directory.
# Launch assembly processes in serial.
# By Jason Miller.

die ("usage: $0 <version> <platform> <run_CA_dir> [<dir> ...]\n")
  unless (@ARGV>=3);
my $VERSION  = shift;   # e.g. "CA3.05"
my $PLATFORM = shift;   # e.g. "linux64" or "alpha"
my $RUN_CA_DIR = shift;
my @PROJECTS = @ARGV;   # e.g. "BMT DMG PVG"  (default is all subdirs)
my $user = getlogin;

# Edit this file!
# Make it contains the path to the correct bin for this platform.
my $CONFIG  = "$RUN_CA_DIR/run_CA.config.$PLATFORM.$VERSION";  

my $WORKDIR = "$VERSION-$PLATFORM";
my $ASM_CMD = "$RUN_CA_DIR/run_CA.pl tmp.frg -dir $WORKDIR -C $CONFIG -local -noedit -nojoin -noupload -clean 1";

# Read names of all subdirectories.
my @subdirs;
if (scalar(@PROJECTS)>0) 
{
    @subdirs = @PROJECTS;
} else 
{
    opendir (THISDIR, ".");
    @subdirs = grep (-d, readdir (THISDIR));
    closedir (THISDIR);
}

print "Info! Launch run_CA on selected subdirectories.\n";
print "Info! Will process these directories: @subdirs\n";
print "Info! Code version assumed to be $VERSION\n";
print "Info! Config file set to $CONFIG\n";
print "Info! Current directory is "; system ("pwd");
print "Info! Current machine is "; system ("uname -a");
print "Info! Path to binaries set to "; system ("grep '^root' $CONFIG");

# Process each subdirectory.
foreach my $subdir (@subdirs) {
    my $result = 0;

    # Ignore the subdirectories '.' and '..'.
    # Ignore subdirectories that don't contain a frag file.
    next if ($subdir =~ /^\.+$/);
    next if (!-f "$subdir/tmp.frg");

    print "Entering [$subdir] at "; system ("date");
    chdir $subdir or die ("Could not change to $subdir");

    # Launch the assembler.
    print "$ASM_CMD\n";
    $result = system ($ASM_CMD);
    $result /= 256;  # perl wierdness
    print "Exit status $result\n";   # STDOUT
    die ("Bad exit status $result") unless ($result==0);   #STDERR

    print "Leaving [$subdir] at "; system ("date");
    chdir ".." or die ("Could not get out of $subdir");
    sleep (1);
}
print "Done at "; system ("date");

