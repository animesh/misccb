#!/usr/local/bin/perl -w
# $Id: cons-mime.pl,v 1.12 2008/03/05 22:43:54 rico Exp $

use 5.006_000;
use warnings;
use strict;

use MIME::Lite;
use Getopt::Long;

# ... args ...

my $o_dir;
my ($o_to, $o_txt, $o_pp, $o_id);
my ($o_pipps, $o_pippdf);
my ($o_dotps, $o_dotpdf);
my ($o_onops, $o_onopdf);
my ($o_xan, $o_lav, $o_args, $o_title);
my ($o_ono, $o_onoseq);

die unless GetOptions(
	'to=s' => \$o_to,
	'title=s' => \$o_title,

	'dir=s' => \$o_dir,

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
);

if (defined($o_dir)) { die unless chdir($o_dir); }

die "--to is required" unless defined($o_to);

$o_title = "blastz output from the PipMaker server"
	unless (defined($o_title) && $o_title =~ /\S/);

# sub chk { foreach $_ (@_) { croak if defined($_) && ! -e $_; } }
# chk $o_txt, $o_pp, $o_id, $o_pipps, $o_pippdf, $o_dotps, $o_dotpdf;

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

add_pdf_ps($msg, "pip",        "blastz output, PIP",     $o_pippdf, $o_pipps);
add_pdf_ps($msg, "dot",        "blastz output, Dotplot", $o_dotpdf, $o_dotps);
add_pdf_ps($msg, "ono",        "blastz output, Dotplot", $o_onopdf, $o_onops);
add_plain ($msg, "concise",    "blastz output, concise (text)", $o_pp);
add_plain ($msg, "text",       "blastz output, verbose (text)", $o_txt);
add_plain ($msg, "lav",        "blastz output, raw (text)",     $o_lav);
add_plain ($msg, "ono",        "order and orientation (text)",  $o_ono);
add_plain ($msg, "onoseq",     "reordered sequence2 (text)",    $o_onoseq);
add_plain ($msg, "analysis.txt",   "analysis of exons (text)",      $o_xan);
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
	
	if (usable_file $text) {
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
