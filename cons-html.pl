#!/usr/local/bin/perl -w
# $Id: cons-html.pl,v 1.7 2008/03/05 22:45:18 rico Exp $

use 5.006_000;
use warnings;
use strict;

use Cwd;
use Getopt::Long;
use File::Basename;
use MIME::Lite;


# ... args ...

my $o_chdir;
my ($o_to, $o_txt, $o_pp, $o_id);
my ($o_pipps, $o_pippdf);
my ($o_dotps, $o_dotpdf);
my ($o_onops, $o_onopdf);
my ($o_xan, $o_lav, $o_args, $o_title);
my ($o_ono, $o_onoseq);

my $o_base;

die unless GetOptions(
	'to=s' => \$o_to,
	'title=s' => \$o_title,

	'dir=s' => \$o_chdir,

	'txt=s' => \$o_txt,
	'pipps=s' => \$o_pipps,
	'pp=s' => \$o_pp,
	'id=s' => \$o_id,
	'xan=s' => \$o_xan,
	'lav=s' => \$o_lav,
	'ono=s' => \$o_ono,
	'onoseq=s' => \$o_onoseq,
	'dotps=s' => \$o_dotps,
	'onops=s' => \$o_onops,
	'pippdf=s' => \$o_pippdf,
	'dotpdf=s' => \$o_dotpdf,
	'onopdf=s' => \$o_onopdf,
	'args=s' => \$o_args,
	'base=s' => \$o_base,
);

die "--to is required" unless defined($o_to);

if (defined $o_base) { ; }
elsif (defined $ENV{'PIPURL'}) { $o_base = "$ENV{'PIPURL'}/results"; }
else { die "base is required"; }

if (defined($o_chdir)) { die unless chdir($o_chdir); }

my $dir = basename(cwd);
my $LINK = "<a href=\"$o_base/$dir/%s\">%s</a><br>\n";

$o_title = "blastz output from the PipMaker server"
	unless (defined($o_title) && $o_title =~ /\S/);


my $body = <<END;
<html>
<h1>$o_title</h1>
<p>As requested, the following are pointers to
files temporarily stored on our server:<br><br>
END

add_pdf_ps("pip",        "blastz output, PIP",     $o_pippdf, $o_pipps);
add_pdf_ps("dot",        "blastz output, Dotplot", $o_dotpdf, $o_dotps);
add_pdf_ps("ono",        "blastz output, Dotplot", $o_onopdf, $o_onops);
add_plain ("concise",    "blastz output, concise", $o_pp);
add_plain ("text",       "blastz output, verbose", $o_txt);
add_plain ("lav",        "blastz output, raw",     $o_lav);
add_plain ("ono",        "order and orientation",  $o_ono);
add_plain ("onoseq",     "reordered sequence2",    $o_onoseq);
add_plain ("analysis.txt",   "analysis of exons",      $o_xan);
add_plain ("parameters", "input parameters",       $o_args);
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

# ... util ...

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
				add_plain ($file, $desc, $file);
			}
		}
		closedir(DOT);
	}
}

