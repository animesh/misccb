#!/usr/bin/perl -w
 
use strict;
use warnings;

# TIGR Modules
use TIGR::Foundation;

my $tigr_tf = new TIGR::Foundation;
my $PRG = $tigr_tf->getProgramInfo('name');
my $REV="1.0";
my @DEPENDS=("TIGR::Foundation");

# help info
my $HELPTEXT = qq~
Program that sorts numerically a TAB file according to 2 columns

Usage: $PRG file [options]
	  
  INPUT:   
	TAB delimited file; The sorting columns (start from 0) must have numeric values  
  
  options:

	-i <n>		- First  column to sort after (Default 0)
  	-j <n>		- Second column to sort after (Default 1)
	
	-h|help		- Print this help and exit;
	-V|version	- Print the version and exit;
	-depend		- Print the program and database dependency list;
	-debug <level>	- Set the debug <level> (0, non-debug by default); 
 
  OUTPUT:  
~;

my $MOREHELP = qq~
Return Codes:   0 - on success, 1 - on failure.
~;


###############################################################################
#
# Main program
#
###############################################################################

MAIN:
{
	my $i=0;
	my $j=1;
	my %h;
	
	# Configure TIGR Foundation
	$tigr_tf->setHelpInfo($HELPTEXT.$MOREHELP);
        $tigr_tf->setUsageInfo($HELPTEXT);
        $tigr_tf->setVersionInfo($REV);
        $tigr_tf->addDependInfo(@DEPENDS);
	
	# validate input parameters
	my $result = $tigr_tf->TIGR_GetOptions(
		"i=i"	=>	\$i,
		"j=i"   =>      \$j
	);
	$tigr_tf->printUsageInfoAndExit() if (!$result);
	
	#parse input file
	while(<>)
	{
		my @f=split;
		next unless(@f);

		my @fi=split //,$f[$i];
		my @fj=split //,$f[$j];

		while(scalar(@fi)<7) { unshift @fi,0; }
		while(scalar(@fj)<7) { unshift @fj,0; }

		my $key=join "",(@fi,@fj);
		$h{$key}=$_;
	}

	foreach my $key (sort {$a<=>$b} keys %h)
	{
		print $h{$key};
	}
	
	exit 0;
}
