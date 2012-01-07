# File: bootver.pl
#
# Abstract:
#   Define the version string (as defined in Contents.m) and use
#   a hash table to look up the corresponding boot code version
#   number. This file must be edited to add a new entry to the
#   hash table each time a new version is released.
#
# Parameters:
#
#   ARGV[0] - outputtype - 'txt' | 'boot'
#             
#             'txt': a text file that will contain the mapping info (can be
#             processed by other utilities - eg. M-code, Java)
#
#             'boot': the assembler file that includes the bootcode version number
#
#   ARGV[1] - outputfilename - filename of the output file
#
# $Revision: 1.1.6.23 $
# $Date: 2009/05/11 14:34:12 $
#
# Copyright 2004-2009 The MathWorks, Inc.

#
# add a new entry to this hash table each time a new version
# is released
# 
# NOTE: It is suggested that the MAJOR boot code version
# is incremented once per full product version increment - 
# eg. from R13 to R14 to R15.
#
# It is suggested that the MINOR boot code version is 
# incremeneted once per minor product version increment - 
# eg. from Beta1 to Beta2 to LCS to FCS
#
# DO NOT change bootcode version numbers for old releases!
#
%bootVersions = ('Prior to release 14' => '0.0',
                 '1.2 (r14 beta 2)' => '1.0',
                 '2.0 (r14 prerelease)' => '1.1',
                 '2.0 (r14 prerelease 2)' => '1.2',
                 '2.0 (r14)' => '1.3',
                 '2.0 (r14sp1beta)' => '1.4',
                 '2.0.1 (r14sp1)' => '1.5',
                 '2.0.1 (r14sp2)' => '1.6',
                 '2.0.2 (r14sp2)' => '1.7',
                 '2.0.3 (r14sp3)' => '1.8',
                 '2.0.3 (2006a_beta)' => '1.9',
                 '2.0.4 (2006a)' => '2.0',
                 '2.0.4 (r2006a prerelease)' => '2.1',
                 '2.0.4 (r2006a)' => '2.2',
                 '2.0.5 (r2006b prerelease)' => '2.3',
                 '2.0.5 (r2006b)' => '2.4',
                 '2.1 (r2007a prerelease)' => '2.5',
                 '2.1 (r2007a)' => '2.6',
                 '2.2 (r2007b prerelease)' => '2.7',
                 '2.2 (r2007b)' => '2.8',
                 '2.2.1 (r2008a prerelease)' => '2.9',
                 '2.2.1 (r2008a)' => '3.0',
                 '2.2.2 (r2008b)' => '3.1',
                 '2.2.3 (r2009a)' => '3.2',
                 'R2009b' => '3.3');

# The version string currently in Contents.m
# Update this string as Contents.m changes
$currentVersionNum = 'R2009b';

$outputtype       = $ARGV[0];
$outputfilename   = $ARGV[1];

$bootVersion = $bootVersions{$currentVersionNum};
# check that a valid bootcode version was determined
if ($bootVersion eq "") {
   die "\nThere is no bootcode version defined for ET MPC5xx version $currentVersionNum\nYou must edit the perl script that reported this error and add a new entry to the hash table."
}
# split into major and minor versions 
@parts = split(/\./, $bootVersion);
$majorBootVersion = $parts[0];
$minorBootVersion = $parts[1];
print "\nGenerating files for bootcode version number\n";
print "ET MPC5xx version number = $currentVersionNum\n";
print "ET MPC5xx major boot version = $majorBootVersion\n";
print "ET MPC5xx minor boot version = $minorBootVersion\n";

# Generate the appropriate output file type
if ($outputtype eq "txt") {
   $textFileName = $outputfilename;
   &CreateTextFile;  
} elsif ($outputtype eq "boot") {
   $bootFileName = $outputfilename;
   &CreateBootver;  
} else {
   die "Unsupported outputtype!"
}

sub CreateBootver {
    $bootFileString = "#
# File: bootver.s
#
# Abstract:
#    This file defines the bootcode version number.
#
# Copyright 2002 The MathWorks, Inc.
# THIS FILE IS AUTOMATICALLY GENERATED: DO NOT MAKE CHANGES
# DIRECTLY TO THIS FILE

#
# Use the first 2-bytes of internal flash to store the version 
# number of the MathWorks bootcode.
#
# Current version of product: $currentVersionNum
# Current major bootcode version number: $majorBootVersion
# Current minor bootcode version number: $minorBootVersion
#
   .org 0x0
# Write the major boot version
   .byte $majorBootVersion
# Write the minor boot version
   .byte $minorBootVersion 
";

# Open boot version file for writing
    open(OUTPUTFILE, (">" . $bootFileName)) 
	|| die "PERL Error: Couldn't open output file: ", 
	$bootFileName, "\n";
    print OUTPUTFILE $bootFileString;
    close(OUTPUTFILE);
    print "Created $bootFileName\n";
}

sub CreateTextFile {
   @versionNums = keys %bootVersions;
   foreach $ver (@versionNums) {
	   $allVersionNums = "$allVersionNums$ver\n";
	   $allBootNums = "$allBootNums$bootVersions{$ver}\n";
   }
   
   # Open text file for writing
   open(OUTPUTFILE, (">" .  $textFileName)) 
	|| die "PERL Error: Couldn't open output file: ", 
	$textFileName, "\n";
   
   select OUTPUTFILE;
   print OUTPUTFILE "# Autogenerated file containing information about bootcode versions.\n\n"; 
   print OUTPUTFILE "# Section 1: Current product version string.\n";
   print OUTPUTFILE "$currentVersionNum\n\n";
   print OUTPUTFILE "# Section 2: All known product version strings.\n";
   print OUTPUTFILE "$allVersionNums\n";
   print OUTPUTFILE "# Section 3: Corresponding bootcode version numbers.\n";
   print OUTPUTFILE "$allBootNums\n";
   close(OUTPUTFILE);
   
   select STDOUT;
   print "Created $textFileName\n";
}
