#!/usr/local/bin/perl -w
# $Id: mpm-html.pl,v 1.3 2008/03/05 22:42:32 rico Exp $

use 5.006_000;
use warnings;
use strict;

use Cwd;
use MIME::Lite;
use Getopt::Long;
use File::Basename;

# ... args ...

my $o_chdir;
my ($o_to, $o_args, $o_title);
my ($o_pipps, $o_pippdf);
my ($o_acgtps, $o_acgtpdf);
my $o_acgttxt;
my (%o_pip1ps, %o_pip1pdf);
my $o_base;

die unless GetOptions(
	'to=s' => \$o_to,
	'title=s' => \$o_title,
	'args=s' => \$o_args,

	'dir=s' => \$o_chdir,

	'pipps=s' => \$o_pipps,
	'pippdf=s' => \$o_pippdf,

	'acgtps=s' => \$o_acgtps,
	'acgtpdf=s' => \$o_acgtpdf,

	'acgttxt=s' => \$o_acgttxt,

	'pip1eps' => \%o_pip1ps,
	'pip1pdf' => \%o_pip1pdf,

	'base=s' => \$o_base,
);

die "--to is required" unless defined($o_to);

if (defined $o_base) { ; }
elsif (defined $ENV{'PIPURL'}) { $o_base = "$ENV{'PIPURL'}/results"; }
else { die "base is required"; }

if (defined($o_chdir)) { die unless chdir($o_chdir); }
my $dir = basename(cwd);

$o_title = "blastz output from the MultiPipMaker server"
	unless (defined($o_title) && $o_title =~ /\S/);

# ... message ...

my $LINK = "<a href=\"$o_base/$dir/%s\">%s</a><br>\n";

my $body = <<END;
<html>
<h1>$o_title</h1>
<p>As requested, the following are pointers to
files temporarily stored on our server:<br><br>
END

add_pdf_ps("pip",        "blastz output, PIP",  $o_pippdf, $o_pipps);
add_pdf_ps("acgt",       "blastz output, ACGT", $o_acgtpdf, $o_acgtps);
add_plain ("acgt",       "blastz output, verbose", $o_acgttxt);
add_plain ("parameters", "input parameters (text)",       $o_args);
add_underlays();

$body .= <<END;
<p>Please download these files promptly: they will automatically be deleted
in a few hours.  Don't try to bookmark them.<br>
END

my $msg = MIME::Lite->new(
	From    => 'PipMaker <pipmaster@bio.cse.psu.edu>',
        To      => $o_to,
        Subject => $o_title,
        Type    => 'text/html',
	Encoding => '8bit',
	Data	=> $body,
);

$msg->print(\*STDOUT) or die;

sub usable_file
{
	return (defined($_[0]) && -s $_[0]);
}

sub add_pdf_ps
{
	my ($name, $desc, $pdf, $ps) = @_;
	
	if (usable_file $pdf) {
		$body .= sprintf($LINK, "$pdf", "$desc (PDF)");
	} elsif (usable_file $ps) {
		$body .= sprintf($LINK, "$ps", "$desc (Postscript)");
	}
}

sub add_plain
{
	my ($name, $desc, $text) = @_;
	
	if (usable_file $text) {
                $body .= sprintf($LINK, "$text", "$desc (text)");
	}
}

sub add_underlays
{
    my $genuly = 0;

    if (open(GENULY, "< genuly")) {
        chomp(my $foo = <GENULY>);
        if ($foo =~ /^\d+$/ && $foo == 1) {
            $genuly = 1;
        }
        close(GENULY);
    }

    if ($genuly && opendir(DOT, '.')) {
        while(defined(my $file = readdir(DOT))) {
            if ($file =~ /^seq(\d{2})\.ov\.ulay$/) {
                my $seqnum = $1;
                my $desc = sprintf("overview underlay for seq%02d", $seqnum);
                add_plain($file, $desc, $file);
            }
        }
        closedir(DOT);
    }
}
