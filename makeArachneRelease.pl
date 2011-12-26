#!/usr/bin/perl

$|=1;
use strict;
use Getopt::Long;

sub Usage {
    print "Usage: makeArachneRelease.pl [--editor=<edit_cmd>] <output release directory>\n";
    print "\tThe script must be run from the root directory of the \n";
    print "\tArachne sandbox you want to make into a new release.\n\n";
    print "\tUpon exiting from the editor an interactive confirmation\n";
    print "\tprompt will be displayed that will allow to abort/proceed\n";
    print "with the release.\n\n";
    print "\tIf shell command that invokes your favorite editor is not specified\n";
    print "\t(with --editor option), the environment variable EDITOR will be used.\n\n";
}

my $EDITOR;
if ( exists $ENV{"EDITOR"} ) {
  $EDITOR = $ENV{"EDITOR"};
}

my $cvs_root;
my $force_root;

GetOptions('--editor:s' => \$EDITOR,
	   '--force-root!' => \$force_root) or do {
  Usage();
  die();
};

if ( ! defined( $EDITOR ) ) 
{
  die("No editor command found either on command line or in the environment.\n");
}

if ( ! defined $force_root || $force_root == 0 ) {
  $cvs_root = "";
} else {
  $cvs_root = "-d ".$ENV{"CVSROOT"};
}
my ($output_release_directory) = @ARGV;

if (scalar(@ARGV) != 1)
{
    print "Illegal command line format.\n\n";
    Usage();
    exit(-1);
}

my $cvs_sandbox_directory = ".";

my $USERNAME = $ENV{"USER"};
my $PID = $$;

my $temp_file_name = "/tmp/$USERNAME" . "_" . $$ .".makeArachneRelease.tmp";

my $history_listing = `cvs $cvs_root log doc/releases.txt | grep ReleaseArachne`;
$history_listing =~ s/(^\s+)|(\:.*?($|\n))//g;
my @release_tags = split(/\s+/, $history_listing);
@release_tags = sort {
			my ($name_a, $major_a, $minor_a) = split(/\-/, $a);
			my ($name_b, $major_b, $minor_b) = split(/\-/, $b);

			my $result;
			if ($major_a != $major_b)
			{
				$result = $major_b <=> $major_a;
			}
			else
			{
				$result = $minor_b <=> $minor_a;
			}
		     } @release_tags;

my $last_tag = $release_tags[0];
$last_tag =~ m/ReleaseArachne\-(\d+)\-(\d+)/;
my ($last_major_version, $last_minor_version) = ($1, $2);

my $new_tag = "ReleaseArachne-$last_major_version-" . ($last_minor_version + 1);

open(OUT, ">$temp_file_name") or die;
my $temp_file = "";
$temp_file .= "Last Arachne Release                          : $last_tag\n";
$temp_file .= "New Arachne Release (change this if you want) : $new_tag\n";
$temp_file .= "\n";
$temp_file .= "Comment (below this line):\n";
$temp_file .= "\n\n\n\n\n";
$temp_file .= "Edit/Add here (prefetched change history since last release is shown below):\n\n";
print OUT $temp_file;
close(OUT);

if ( $cvs_root ne "" ) {
  `$cvs_sandbox_directory/cvsUpdatesReport.pl --r=\"$last_tag\" --d=$ENV{"CVSROOT"} solexaPipeline >> $temp_file_name 2>&1`;
} else {
  `$cvs_sandbox_directory/cvsUpdatesReport.pl --r=\"$last_tag\" solexaPipeline >> $temp_file_name 2>&1`;
}

system("$EDITOR $temp_file_name");

open(IN, $temp_file_name) or die;
my $comment = "";
my $done_with_header = 0;
my $edited_temp_file = "";
while(<IN>)
{
    $edited_temp_file .= $_;	
	if ($_ =~ m/New Arachne Release .*?\:(.*)$/)
	{
		$new_tag = $1;
		$new_tag =~ s/(^\s+)|(\s+$)//g;
	}

	if ($_ =~ m/^Comment \(below this line\):\n$/)
	{
		$done_with_header = 1;
		next;		
	}

	if ($done_with_header)
	{
		$comment = $comment . $_;
	}
}
$comment =~ s/\s*$//g;

my $prompt;
while ( $prompt ne "yes" and $prompt ne "no" ) {
  print "Ready to build new release. PROCEED (yes/no)? ";
  chomp ($prompt = <STDIN>);
  $prompt = lc $prompt;
}



if ($prompt eq "no" )
{
    print "Aborting release. See you later.\n";
    exit;
}

my $log_command              = "echo \">$new_tag\" >> doc/releases.txt ; echo \"$comment\" >> doc/releases.txt";
my $commit_log_command       = "cvs $cvs_root commit -m \"updated with new release message\" doc/releases.txt";
my $tag_command              = "cvs $cvs_root tag $new_tag";
my $output_release_directory = $output_release_directory . "/$new_tag/"; 
my $mkdir_command            = "mkdir $output_release_directory";
my $checkout_command         = "cvs $cvs_root checkout -r $new_tag -d $output_release_directory Arachne";
my $make_command             = " cd $output_release_directory;setenv CROSS True; unsetenv ARACHNE_BIN_DIR ; make -j 5 CROSS=yes";

print "Will execute the following commands:\n";
print "$log_command\n$commit_log_command\n$tag_command\n$mkdir_command\n$checkout_command\n$make_command\n\n";

system($log_command);
system($commit_log_command);
system($tag_command);
system($mkdir_command);
system($checkout_command);
chdir($output_release_directory);
system($make_command);

