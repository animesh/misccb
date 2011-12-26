#!/usr/bin/env perl
# $Id: texindy,v 1.8 2009/03/22 11:08:18 jschrod Exp $
#------------------------------------------------------------
# (history at end)

=head1 NAME

texindy - create sorted and tagged index from raw LaTeX index

=head1 SYNOPSIS

 texindy [-V?h] [-qv] [-iglr] [-d magic] [-o outfile.ind] [-t log] \
         [-L lang] [-C codepage] [-M module] [idx0 idx1 ...]

=head2 GNU-Style Long Options for Short Options:

 -V / --version
 -? / -h / --help
 -q / --quiet
 -v / --verbose
 -i / --stdin
 -g / --german
 -l / --letter-ordering
 -r / --no-ranges
 -d / --debug          (multiple times)
 -o / --out-file
 -t / --log-file
 -L / --language
 -C / --codepage
 -M / --module         (multiple times)
 -I / --input-markup   (supported: latex, omega)


=head1 DESCRIPTION

B<texindy> is the LaTeX-specific command of xindy, the flexible
indexing system. It takes a raw index as input, and produces a merged,
sorted and tagged index. Merging, sorting, and tagging is controlled
by xindy modules, with a convenient set already preloaded.

Files with the raw index are passed as arguments. If no arguments are
passed, the raw index will be read from standard input.

A good introductionary description of B<texindy> appears in the
indexing chapter of the LaTeX Companion (2nd ed.)

If you want to produce an index for LaTeX documents with special index
markup, the command xindy(1) is probably more of interest for you.

B<texindy> is an approach to merge support for the I<make-rules>
framework, own xindy modules (e.g., for special LaTeX commands in the
index), and a reasonable level of MakeIndex compatibility. There are
other older approaches, eventually they will get a description on the
xindy Web Site, http://www.xindy.org/.


=head1 OPTIONS

=over

=item C<--version> / B<-V>

output version numbers of all relevant components and exit.

=item C<--help> / B<-h> / B<-?>

output usage message with options explanation.

=item C<--quiet> / B<-q>

Don't output progress messages. Output only error messages.

=item C<--verbose> / B<-v>

Output verbose progress messages.

=item C<--debug> I<magic> / B<-d> I<magic>

Output debug messages, this option may be specified multiple times.
I<magic> determines what is output:

 magic          remark
 ------------------------------------------------------------
 script         internal progress messages of driver scripts
 keep_tmpfiles  don't discard temporary files
 markup         output markup trace, as explained in xindy manual
 level=n        log level, n is 0 (default), 1, 2, or 3

=item C<--out-file> F<outfile.ind> / B<-o> F<outfile.ind>

Output index to file F<outfile.ind>. If this option is not passed, the
name of the output file is the base name of the first argument and the
file extension F<ind>. If the raw index is read from standard input,
this option is mandatory.

=item C<--log-file> F<log.ilg> / B<-t> F<log.ilg>

Output log messages to file F<log.ilg>. These log messages are
independent from the progress messages that you can influence with
C<--debug> or C<--verbose>.

=item C<--language> I<lang> / B<-L> I<lang>

The index is sorted according to the rules of language I<lang>. These
rules are encoded in a xindy module created by I<make-rules>.

If no input encoding is specified via C<--codepage>, a xindy module
for that language is searched with a latin, a cp, an iso, or ascii
encoding, in that order.

=item C<--codepage> I<enc> / B <-C> I<enc>

The raw input is in input encoding I<enc>. This information is used to
select the correct xindy sort module and also the I<inputenc> target
encoding for C<latex> input markup.

When C<omega> input markup is used, C<utf8> is always used as the sort
codepage and no inputenc module is loaded. Then this option is
ignored.

=item C<--module> I<module> / B<-M> I<module>

Load the xindy module F<module.xdy>. This option may be specified
multiple times. The modules are searched in the xindy search path that
can be changed with the environment variable C<XINDY_SEARCHPATH>.

=item C<--input-markup> I<input> / B<-I> I<input>

Specifies the input markup of the raw index. Supported values for
I<input> are C<latex> and C<omega>.

C<latex> input markup is the one that is emitted by default from the
LaTeX kernel, or by the C<index> macro package of David Jones.
^^-notation of single byte characters is supported. Usage of LaTeX's
I<inputenc> package is assumed as well.

C<omega> input markup is like C<latex> input markup, but with Omega's
^^-notation as encoding for non-ASCII characters. LaTeX I<inputenc>
encoding is not used then, and C<utf8> is enforced to be the codepage
for sorting.

=back


=head1 SUPPORTED LANGUAGES / CODEPAGES

The following languages are supported:

=head2 Latin scripts

 albanian      gypsy             portuguese
 croatian      hausa		 romanian
 czech	       hungarian 	 russian-iso
 danish	       icelandic	 slovak-small
 english       italian		 slovak-large
 esperanto     kurdish-bedirxan  slovenian
 estonian      kurdish-turkish	 spanish-modern
 finnish       latin		 spanish-traditional
 french	       latvian		 swedish
 general       lithuanian	 turkish
 german-din    lower-sorbian	 upper-sorbian
 german-duden  norwegian	 vietnamese
 greek-iso     polish

German recognizes two different sorting schemes to handle umlauts:
normally, C<�> is sorted like C<ae>, but in phone books or
dictionaries, it is sorted like C<a>. The first scheme is known as
I<DIN order>, the second as I<Duden order>.

C<*-iso> language names assume that the raw index entries are in ISO
8859-9 encoding.

C<gypsy> is a northern Russian dialect.

=head2 Cyrillic scripts

 belarusian    mongolian  	 serbian
 bulgarian     russian    	 ukrainian
 macedonian

=head2 Other scripts

 greek         klingon

=head2 Available Codepages

This is not yet written. You can look them up in your xindy
distribution, in the F<modules/lang/language/> directory (where
I<language> is your language). They are named
F<variant-codepage-lang.xdy>, where F<variant-> is most often empty
(for german, it's C<din5007> and C<duden>; for spanish, it's C<modern>
and C<traditional>, etc.)

 < Describe available codepages for each language >

 < Describe relevance of codepages (as internal representation) for
   LaTeX inputenc >


=head1 TEXINDY STANDARD MODULES

There is a set of B<texindy> standard modules that help to process
LaTeX index files. Some of them are automatically loaded. Some of them
are loaded by default, this can be turned off with a B<texindy>
option. Others may be specified as C<--module> argument to achieve a
specific effect.

 xindy Module    Category  Description

=head2 Sorting

 word-order      Default   A space comes before any letter in the
                           alphabet: ``index style'' is listed before
                           ``indexing''. Turn it off with option -l.
 letter-order    Add-on    Spaces are ignored: ``index style''
                           is sorted after ``indexing''.
 keep-blanks     Add-on    Leading and trailing white space (blanks
                           and tabs) are not ignored; intermediate
                           white space is not changed.
 ignore-hyphen   Add-on    Hyphens are ignored:
			   ``ad-hoc'' is sorted as ``adhoc''.
 ignore-punctuation Add-on All kinds of punctuation characters are
			   ignored: hyphens, periods, commas, slashes,
			   parentheses, and so on.
 numeric-sort    Auto      Numbers are sorted numerically, not like
			   characters: ``V64'' appears before ``V128''.

=head2 Page Numbers

 page-ranges     Default   Appearances on more than two consecutive
			   pages are listed as a range: ``1--4''.
			   Turn it off with option -r.
 ff-ranges       Add-on    Uses implicit ``ff'' notation for ranges
			   of three pages, and explicit ranges
			   thereafter: 2f, 2ff, 2--6.
 ff-ranges-only  Add-on    Uses only implicit ranges: 2f, 2ff.
 book-order      Add-on    Sorts page numbers with common book numbering
			   scheme correctly -- Roman numerals first, then
			   Arabic numbers, then others: i, 1, A.

=head2 Markup and Layout

 tex             Auto      Handles basic TeX conventions.
 latex-loc-fmts  Auto	   Provides LaTeX formatting commands
		    	   for page number encapsulation.
 latex           Auto	   Handles LaTeX conventions, both in raw
		    	   index entries and output markup; implies
		    	   tex.
 makeindex       Auto	   Emulates the default MakeIndex input syntax
			   and quoting behavior.
 latin-lettergroups Auto   Layout contains a single Latin letter
			   above each group of words starting with the
			   same letter.
 german-sty      Add-on	   Handles umlaut markup of babel's german
			   and ngerman options.



=head1 ENVIRONMENT

=over

=item C<TEXINDY_AUTO_MODULE>

This is the name of the xindy module that loads all auto-loaded
modules. The default is C<texindy>.

=back


=head1 AUTHOR

Joachim Schrod


=head1 LEGALESE

B<texindy> is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.


=for Emacs
#'

=cut

use 5.006;
use strict;

BEGIN {
    use vars qw($Revision $VERSION);
    q$Revision: 1.8 $ =~ /: (\d+)\.(\d+)/ ;	# q wg. Emacs indent!
    my ($major, $minor) = ($1, $2);
    $VERSION = "$major." . ($minor<10 ? '0' : '') . $minor;
}


# Some common variables.
# Determine environment. Where is our library directory, and our modules?

use File::Basename;
our ($cmd_dir, $cmd);
BEGIN {
    $cmd_dir = dirname($0);
    $cmd = basename($0);
}


# Used modules.

use Getopt::Long qw(:config bundling);


# Check arguments, store them in proper variables.

my $usage_msg = <<_EOT_

usage: $cmd [-V?h] [-qv] [-iglr] [-d magic] [-o outfile.ind] [-t log] \\
            [-L lang] [-C codepage] [-M module] [-I input] [idx0 idx1 ...]

GNU-STYLE LONG OPTIONS FOR SHORT OPTIONS:

 -V / --version
 -? / -h / --help
 -q / --quiet
 -v / --verbose
 -i / --stdin
 -g / --german
 -l / --letter-ordering
 -r / --no-ranges
 -d / --debug          (multiple times)
                       (supported: script, keep_tmpfiles, markup, level=n)
 -o / --out-file
 -t / --log-file
 -L / --language
 -C / --codepage
 -M / --module         (multiple times)
 -I / --input-markup   (supported: latex, omega)

_EOT_
;

sub usage ()
{
    print STDERR $usage_msg;
    exit 1;
}

sub parse_options();
sub output_version();

our ($output_version, $quiet, $verbose, $stdin, @debug,
     $outfile, $logfile, $language, $codepage, @modules, $input_markup);
$language = 'general';
$codepage = 'latin';
$input_markup = 'latex';

our $xindy;
if ( -f "$cmd_dir/xindy" && -x _ ) {
    $xindy = "$cmd_dir/xindy";
} elsif ( -f "$cmd_dir/xindy.pl" && -x _ ) {
    $xindy = "$cmd_dir/xindy.pl";
} elsif ( -f "$ENV{SELFAUTOLOC}/xindy.bat" ) { # woe32
    $xindy = "$ENV{SELFAUTOLOC}/xindy.bat";
    $cmd_dir = $ENV{"SELFAUTOLOC"};
} else {
    die "$cmd: cannot locate xindy\n";
}

parse_options();
output_version()  if $output_version;	# will not return
usage()  if ( ! $stdin && @ARGV == 0 );	# brain damaged, but like makeindex


# Construct xindy options, and eventually switch to it.

my @opt;
push (@opt, '-q')  if $quiet;
push (@opt, '-v')  if $verbose;
push (@opt, map { ('-d', $_) } @debug)  if @debug;
push (@opt, '-o', $outfile)  if $outfile;
push (@opt, '-t', $logfile)  if $logfile;
push (@opt, '-L', $language);
push (@opt, '-C', $codepage)  if $codepage;
push (@opt, '-M', "tex/inputenc/$codepage")  if $codepage;
push (@opt, map { ('-M', $_) } ($ENV{TEXINDY_AUTO_MODULE} || 'texindy',
				@modules));
push (@opt, '-I', $input_markup);

print "Calling xindy as: $cmd_dir/xindy @opt @ARGV\n"  if (grep /^script$/, @debug);

if ($xindy =~ m,\.bat$,) {
  system($xindy, @opt, @ARGV);
  exit $? if $? != -1;
} else {
  exec $xindy, @opt, @ARGV;
}
die "$cmd: could not execute $xindy: $!\n";


# ------------------------------------------------------------


sub parse_options() {

    my ($german, $letter_ordering, $no_ranges);
    GetOptions(
	       'version|V'          => \$output_version,
	       'help|h|?'           => sub { print $usage_msg; exit 0; },
	       'quiet|q'            => \$quiet,
	       'verbose|v'          => \$verbose,
	       'stdin|i'            => \$stdin,
	       'german|g'           => \$german,
	       'letter-ordering|l'  => \$letter_ordering,
	       'no-ranges|r'        => \$no_ranges,
	       'debug|d=s'          => \@debug,
	       'out-file|o=s'       => \$outfile,
	       'log-file|t=s'       => \$logfile,
	       'language|L=s'       => \$language,
	       'codepage|C=s'       => \$codepage,
	       'module|M=s'         => \@modules,
	       'input-markup|I=s'   => \$input_markup,
	      )
      or  usage();

    if ( $german ) {
	unshift (@modules, 'german-sty');
	if ( $language eq 'general' ) {
	    $language = 'german-din';
	} elsif ( $language !~ /^german/ ) {
	    print STDERR "You cannot specify -g and -L at the same time.\n";
	    #print STDERR "NOTE: -g is obsolete anyhow.\n";
	    exit (1);
	}
    }
    unshift (@modules, ($letter_ordering ? 'letter-order' : 'word-order'));
    unshift (@modules, 'page-ranges')  unless $no_ranges;

    # Check that the input markup is known. omega markup implies
    # codepage utf8 for sorting, but no inputenc. We set the codepage
    # to undef to prevent loading of the inputenc module. Setting it
    # to utf8 for the sort encoding is done by the xindy script.
    if ( $input_markup ne 'latex' && $input_markup ne 'omega' ) {
	print STDERR "Unsupported input markup $input_markup.\n\n";
	usage();
    }
    if ( $input_markup eq 'omega' ) {
	$codepage = undef;
    }

}


sub output_version () {
    output_xindy_release();
    print "$cmd script version: $VERSION\n";
    my @xindy_cmd = ($xindy, '--internal-version');
    push (@xindy_cmd, qw(-d script --foobar))  if grep(/^script$/, @debug);
    exec @xindy_cmd;
}


sub output_xindy_release () {
    my $version = 'unknown';
    my $version_file;
    if ( -f "$cmd_dir/../VERSION" ) {
	$version_file = "$cmd_dir/../VERSION";
    } else {
	my $xindy_run;
	if ($^O eq "MSWin32" || $^O eq "cygwin") {
	  $xindy_run = "xindy-lisp.exe";
	} else {
	  $xindy_run = "xindy.run";
	}
	# Where is the library directory?
	my $lib_dir;
	if ( $ENV{XINDY_LIBDIR} ) {
	    $lib_dir = $ENV{XINDY_LIBDIR};
	} elsif ( -f "$cmd_dir/$xindy_run" ) { # texlive style
	    $lib_dir = $cmd_dir;
	} elsif ( '@libdir@' ne '@libdir' . '@' ) { # GNU configure at work?
	    if ( -d '@libdir@/xindy' ) { # /usr style
		$lib_dir = '@libdir@/xindy';
	    } else {
		$lib_dir = '@libdir@'; # /opt style
	    }
	} elsif ( -f "$cmd_dir/../lib/$xindy_run" ) { # /opt style
	    $lib_dir = "$cmd_dir/../lib";
	} elsif ( -d "$cmd_dir/../lib/xindy" ) { # /usr style
	    $lib_dir = "$cmd_dir/../lib/xindy";
	} else {
	    die "Cannot locate xindy library directory";
	}
	if ( -f "$lib_dir/VERSION" ) {
	    $version_file = "$lib_dir/VERSION";
	}
    }

    if ( $version_file ) {
	if ( open(VERSION, "<$version_file") ) {
	    while ( $version = <VERSION> ) {
		chomp ($version);
		$version =~ s/\#.*// ;
		$version =~ s/^\s+// ;
		$version =~ s/\s+$// ;
		last  if $version;
	    }
	    close (VERSION);
	}
    }
    print "xindy release: $version\n";
}



#======================================================================
#
# $Log: texindy,v $
# Revision 1.8  2009/03/22 11:08:18  jschrod
#     man page: --v is --verbose, not --version.
#
# Revision 1.7  2009/03/21 16:32:06  jschrod
#     Inputenc merge rules must be loaded before other texindy modules;
# otherwise inputenc markup would be discarded by tex.xdy.
#
# Revision 1.6  2008/02/17 14:55:32  jschrod
#     Use exitcode 0 when usage is explicitly demanded with --help et.al.
#
# Revision 1.5  2006/07/30 10:30:42  jschrod
#     Check if an exec() error happened and output an error message.
# (Ticket 1230801)
#
# Revision 1.4  2006/07/19 00:29:56  jschrod
#     Support for omega input markup.
#
# Revision 1.3  2004/11/01 22:48:51  jschrod
#     Locate xindy script.
#     Terminate on option error.
#     Fix up version output.
#
# Revision 1.2  2004/05/26 21:30:11  jschrod
#     Added POD documentation.
#
# Revision 1.1  2004/05/24 19:47:13  jschrod
#     Introduce new driver script, as part of the "Companion Release".
#
