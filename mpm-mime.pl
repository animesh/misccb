#!/usr/local/bin/perl -w
# $Id: mpm-mime.pl,v 1.10 2008/03/05 22:46:04 rico Exp $

use 5.006_000;
use warnings;
use strict;

use MIME::Lite;
use Getopt::Long;

# ... args ...

my $o_dir;
my ($o_to, $o_args, $o_title);
my ($o_pipps, $o_pippdf);
my ($o_acgtps, $o_acgtpdf);
my $o_acgttxt;
my (%o_pip1ps, %o_pip1pdf);

die unless GetOptions(
	'to=s' => \$o_to,
	'title=s' => \$o_title,
	'args=s' => \$o_args,

	'dir=s' => \$o_dir,

	'pipps=s' => \$o_pipps,
	'pippdf=s' => \$o_pippdf,

	'acgtps=s' => \$o_acgtps,
	'acgtpdf=s' => \$o_acgtpdf,

	'acgttxt=s' => \$o_acgttxt,

	'pip1eps' => \%o_pip1ps,
	'pip1pdf' => \%o_pip1pdf,
);

if (defined($o_dir)) { die unless chdir($o_dir); }

die "--to is required" unless defined($o_to);

$o_title = "blastz output from the MultiPipMaker server"
	unless (defined($o_title) && $o_title =~ /\S/);

# ... message ...

my $msg = MIME::Lite->new(
	From    => 'PipMaker <pipmaster@bio.cse.psu.edu>',
        To      => $o_to,
        Subject => $o_title,
        Type    => 'multipart/mixed'
);

$msg->attach(
        Type => 'TEXT',
        Encoding => 'quoted-printable',
        Disposition => 'inline',
        Data => "Here is your output (several attachments).\n"
);

add_pdf_ps($msg, "pip",        "blastz output, PIP",  $o_pippdf, $o_pipps);
add_pdf_ps($msg, "acgt",       "blastz output, ACGT", $o_acgtpdf, $o_acgtps);
add_plain ($msg, "acgt",       "blastz output, verbose", $o_acgttxt);
add_plain ($msg, "parameters", "input parameters (text)",       $o_args);
add_underlays($msg);

$msg->print(\*STDOUT) or die;

# ... util ...

sub usable_file
{
	return (defined($_[0]) && -s $_[0]);
}

sub add_pdf_ps
{
	my ($msg, $name, $desc, $pdf, $ps) = @_;
	if (usable_file $pdf) {
		$name .= ".pdf" unless $name =~ /pdf$/;
		$msg->attach(Type => 'application/pdf',
			     Filename => $name,
			     Disposition => 'attachment', Path => $pdf)
		    ->attr('Content-Description' => "$desc (pdf)");
	} elsif (usable_file $ps) {
		$name .= ".ps" unless $name =~ /ps$/;
		$msg->attach(Type => 'application/postscript',
			     Filename => $name,
			     Disposition => 'attachment', Path => $ps)
		    ->attr('Content-Description' => "$desc (postscript)");
	}
}

sub add_plain
{
	my ($msg, $name, $desc, $text) = @_;
	
	if (usable_file "$text.gz") {
		$msg->attach(Type => 'application/x-gzip',
			     Encoding => 'base64',
			     Filename => "$name.gz",
			     Disposition => 'attachment',
			     Path => "$text.gz")
		    ->attr('Content-Description' => "$desc");
	} elsif (usable_file $text) {
		$msg->attach(Type => 'TEXT',
			     Encoding => 'quoted-printable',
			     Filename => $name,
			     Disposition => 'attachment', Path => $text)
		    ->attr('Content-Description' => "$desc");
	}
}

sub add_underlays
{
	my $msg = shift;

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
                add_plain($msg, $file, $desc, $file);
            }
        }
        closedir(DOT);
    }
}
