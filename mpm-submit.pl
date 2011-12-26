#!/usr/local/bin/perl -wT
# $Id: mpm-submit.pl,v 1.22 2006/10/30 18:36:55 rico Exp $

BEGIN {
$| = 1;
umask(0002);
}

use 5.006_000;
use warnings;
use strict;

use CGI '-debug';
use CGI::Carp 'fatalsToBrowser';
use Net::DNS::Resolver;

my $prefix="/usr/local/align"; # XXX - config
my $piphome = "$prefix/pipmaker"; # XXX - config - must match pipenv
my $run     = "$piphome/run";
my $spool   = "$piphome/spool";
$ENV{'PATH'} = "$piphome/bin:/usr/local/align/bin:/usr/local/bin:/usr/bin:/bin";

delete @ENV{'IFS', 'CDPATH', 'ENV', 'BASH_ENV'};

# Note!  This form, %d.%d.%d, is known to other programs!
my $tmpname = sprintf("%d.%d.%d", time(), getppid(), $$); # XXX, mkdtemp
my $tmppath = "$spool/tmp/$tmpname";
my $newpath = "$spool/new/$tmpname";
my $errpath = "$spool/err/$tmpname";

# --------

$CGI::POST_MAX = 1024 * 1024 * 64;
my $debuglevel = 1;
my $q = new CGI;

# --------

sub cleanup { 
    if (defined($tmppath) && -d $tmppath) {
        rename($tmppath, $errpath) || die "Cannot rename tmp to err\n";
    }
}
sub plumber {  &cleanup; exit(0); }
$SIG{'HUP'} = \&plumber; 
$SIG{'INT'} = \&plumber;
$SIG{'TERM'} = \&plumber;
$SIG{'PIPE'} = \&plumber;
END { &cleanup; }

sub cgierr_exit {
    &cleanup;
    print $q->end_html;
    exit 0;
}

sub notify {
    my $s = shift;
    print $s;
    print STDERR $s;
}

sub err {
    my $q = shift;
    my $e = shift;

    # warn($e . "\n"); # for log
    print $q->header,
	$q->start_html('Problems'),
	$q->h2('Request not processed'),
	$q->strong($e);
    &cgierr_exit;
}

sub stash_err {
    my $q = shift;
    my $d = shift;
    my $s = shift;
    open(F, ">>$d/ERR0") or die "open: $!";
    print F "\n$s\n";
    close(F);
    err($q, $s);
}

sub cgierr {
    my $q = shift;
    my $e = $q->cgi_error;
    print $q->header,
	    $q->start_html('Problems'),
	    $q->h2('Request not processed'),
	    $q->strong($e);
    &cgierr_exit;
}
  
sub eupload {
	my $q = shift;
	my $a = shift;
	my $f = $q->upload($a);
	&cgierr($q) if (!$f && $q->cgi_error);
	 # linux/perl bug??
         if (defined($f) && -f $f && -B $f) {
             &err($q, "$a is not a plain text file");
         }
	return $f;
}

sub eparam {
	my $q = shift;
	my $a = shift;
	my $f = $q->param($a);
	&cgierr($q) if (!$f && $q->cgi_error);
	if ($f) {
		for ($f) {
			s/^\s+//;
			s/\s+$//;
		}
        }
	return $f;
}

sub canonicalize {
	# only handles non-pathalogical cases well.
	for (@_) { 
		s/\r\n/\n/g;                  # dos -> unix
		s/\r/\n/g;                    # mac -> unix
		s/[ \t]+$//gm;                        # trim trailing whitespace
		$_ .= "\n" unless $_ =~ /\n$/;  # make sure last line is ok
	}
}

sub save_query {
	my $q = shift;
	my $file = shift;

	die "Undefined file" unless $file;
	open(F, ">$file") or die "Cannot open $file: $!";
	$q->save(\*F);
	close(F);
}

sub copyfile {
	my $fh = shift;
	my $file = shift;

	die "Undefined file" unless $file;
	open(F, ">$file") or die "Cannot open $file: $!";
	if (defined($fh)) {
		seek($fh,0,0); # workaround bug in CGI::upload
		while (<$fh>) {
			canonicalize($_);
			print F or die "Cannot write $file: $!" 
		}
	}
	close(F) or die "Cannot close $file: $!";
}

sub copystring {
	my $str = shift;
	my $file = shift;

	die "Undefined file" unless $file;
	$str = "" unless defined($str);
	open(F, ">", $file) or die "Cannot open $file: $!";
	canonicalize($str);
	print F $str or die "Cannot write $file: $!";
	close(F) or die "Cannot close $file: $!";
}

sub copyfs {
    my $str = shift;
    my $fh = shift;
    my $file = shift;

    if (defined($str) && ($str !~ /^\s*$/)) {
        copystring($str, $file);
    } else {
        copyfile($fh, $file);
    }
}

sub nsubdirs {
  my($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
     $atime,$mtime,$ctime,$blksize,$blocks) = stat(shift);
  return (-d _ ? $nlink-2 : 0);
}

sub check_queue_size {
  my $q = shift;
  my $tmp = shift;
  err $q, "Too many pending submissions.\n"
    if nsubdirs("$tmp/tmp") > 50;
  err $q, "Too many submissions in the new-work queue.\n"
    if nsubdirs("$tmp/new") > 50;
}

sub valid_smtp_host {
  my $name = shift;
  my $resolver = new Net::DNS::Resolver;

  return 1 unless $resolver; # XXX - fail soft.
  return defined($resolver->query($name, "MX", "IN"))
      || defined($resolver->query($name, "A",  "IN"));
}

sub check_email {
    my $q = shift;
    my $n = shift;
    my $p = shift;
    err($q, "Parameter <i>$n</i> is empty.")
        if (!defined($p) || length($p) == 0);
    err($q, "Parameter <i>$n</i> seems to be malformed: <i>$p</i>.")
        unless $p =~ /[[:graph:]]+@([[:graph:]]+\.[[:graph:]]+)/;
    err($q, "Host <i>$1</i> doesn't have an A or MX record; is it misspelled?")
        unless valid_smtp_host $1;
}

sub check_strlen {
    my $q = shift;
    my $n = shift;
    my $p = shift;
    err($q, "Parameter <i>$n</i> is empty.") if (length($p) == 0);
}

sub check_nonempty {
    my $q = shift;
    my $d = shift;
    my $f = shift;
    my $g = "$d/$f";
    stash_err($q, $d, "Data file <i>$f</i> is empty.") if (-z $g);
}

sub check_lowercase {
    my $q = shift;
    my $d = shift;
    my $f = shift;
    my $g = "$d/$f";
    open(F, "$g") or die "Cannot open $g: $!";
    while (<F>) {
        stash_err($q, $d, "Data file $f contains lower case (masked out) letters.")
	    if (/^[A-Za-z]/ && /[a-z]/);
    }
    close(F) or die;
}

sub check_server_status {
    my $q = shift;
    my $run = shift;
    system("pipserver-status") == 0
	or err($q, "Server is down now.\n");
}

sub check_lint {
    my $q = shift;
    my $t = shift;
    my $res = qx{
      cd $t &&
      pipenv pip-lint \\
	--reqhdr \\
	--exon ./exons \\
	--rpts ./seq01mask \\
	--ulay ./underlay \\
	--anno ./annotation \\
	--nvce /dev/null -gxan /dev/null \\
	-- \\
	seq??data
    };
    $res = "status=$?:$!:$@:" unless defined($res);
    stash_err($q, $t, "<pre>$res</pre>") if ($? != 0);
}

sub file_lines {
    # man perlfaq5
    my $filename = shift;
    my $lines = 0;
    my $buffer = "";
    open(FILE, $filename) or die "Can't open `$filename': $!";
    while (sysread FILE, $buffer, 4096) {
        $lines += ($buffer =~ tr/\n//);
    }
    close FILE;
    return $lines;
}

sub V {
    my $v = shift;
    return $v if defined($v);
    return "";
} 

sub VP {
    return V(eparam($q, $_[0]));
}

sub untaint {
	$_[0] =~ m/^(.*)$/so;
	$1;
}

sub YN {
    return (V($_[0]) =~ /1/) ? "yes" : "no";
}

sub YNP {
    return YN(eparam($q, $_[0]));
}

# --- sub main ---
#check_server_status($q, $run);
#check_queue_size($q, $spool);

my $email = eparam($q, 'email');
check_email($q, "email", $email);

my $num = eparam($q, 'num');
   $num =~ /(\d+)/;
   $num = 0 + $1;
(1 < $num) or die "bad number num='$num'\n";
($num < 50) or die "number too large ($num)\n";
($num < 99) or die "number invalid ($num)\n"; # must match %02d elsewhere

mkdir $tmppath, 0775 or die "Cannot mkdir $tmppath: $!\n";

#save_query($q, "$tmppath/SAVE") if $debuglevel > 0;
copystring($email, "$tmppath/email");
copystring($num, "$tmppath/num");
copystring('', "$tmppath/app=mpip");
copystring('', "$tmppath/id");

sub save_param_str {
  copystring(eparam($_[0], $_[2]), "$_[1]/$_[2]");
}
sub save_param_fh {
  copyfile(eupload($_[0], $_[2]), "$_[1]/$_[2]");
}

# XXX - check validity?
save_param_str($q, $tmppath, 'genwww');
save_param_str($q, $tmppath, 'genpip');
save_param_str($q, $tmppath, 'genpdf');
save_param_str($q, $tmppath, 'gentext');
save_param_str($q, $tmppath, 'genuly');
save_param_str($q, $tmppath, 'genmaps');
save_param_str($q, $tmppath, 'piptitle');
save_param_str($q, $tmppath, 'dfltulay');

save_param_fh($q, $tmppath, 'exons');
save_param_fh($q, $tmppath, 'underlay');
save_param_fh($q, $tmppath, 'annotation');
save_param_fh($q, $tmppath, 'seq01mask');

for $_ (1..$num) {
  my $n = sprintf "seq%02d", $_;
  save_param_str($q, $tmppath, $n."name");
  save_param_str($q, $tmppath, $n."strand");
  save_param_str($q, $tmppath, $n."chain");
  save_param_str($q, $tmppath, $n."sensitivity");
  save_param_fh($q, $tmppath, $n."underlay");
  my $d = $n."data";
  save_param_fh($q, $tmppath, $d);
  # save_param_str($q, $tmppath, $d); # XXX - debugging
  check_nonempty($q, $tmppath, $d);
  unless (V(eparam($q,$n."name")) =~ /^[\w\.\-]*$/) {
    stash_err($q, $tmppath,
	"labels must be a single word (letters and numbers)");
  }
}

# my $syn = $q->Dump;
my $syn = summary($tmppath);
my $presyn = "<pre>\n$syn</pre>\n";
copystring($syn, "$tmppath/dump");

check_lint($q, "$tmppath");

print $q->header, $q->start_html('MultiPipMaker');

if (rename($tmppath, $newpath)) {
    print $q->h1("submitted"), $q->p(''), "Expect reply via email.", $q->p('');
} else {
    print $q->h1("submission failed");
    # system("rm -rf $tmppath");
}

print $presyn;
print $q->Dump;
print $q->end_html;
exit 0;

sub summary {
    my $tmp = shift;
    my $str = "email: " . VP('email') . "\n\n"
	. "exons: " . file_lines("$tmp/exons") . " lines \n"
	. "underlay: " . file_lines("$tmp/underlay")
	. "annotation: " . file_lines("$tmp/annotation") . " lines \n"
	. "seq01mask: " . file_lines("$tmp/seq01mask") . " lines \n"
	. "make a pip: " . YNP('genpip') . "\n"
	. "nucleotude level view: " . YNP('genmaps') . "\n"
	. "output pdf: " . YNP('genpdf') . "\n"
	. "output text: " . YNP('gentext') . "\n"
	. "get underlays created for overview: " . YNP('genuly') . "\n"
	. "retrieve with www: " . YNP('genwww') . "\n"
	. "number of sequences: " . VP('num') . "\n"
	;
    for $_ (1..$num) {
	my $n = sprintf "seq%02d", $_;
	my $id = untaint scalar qx{cd $tmp && seq_ident "$n"data 2>/dev/null};
	chomp($id);
	$str .= "\n" 
	     . $n."name: " . VP($n.'name') . "\n"
	     . $n."data: length $id\n"
	    ;
	if ($_ > 1) {
	     $str .= $n."strand: " .  (1+VP($n.'strand')) . "\n";
	     $str .= $n."chain: " .
		((V(eparam($q, $n.'chain')) =~ /0/)?"no":"yes")."\n";
	     $str .= $n."sensitivity: " .
		((V(eparam($q, $n.'sensitivity')) =~ /1/)?"high":"normal")."\n";
	     $str .= $n."underlay: " .
		file_lines("$tmp/$n".'underlay') . " lines\n";
	}
    }
    $str =~ s/[ \t]+/ /g;
    return $str;
}

