#!/usr/local/bin/perl -wT
# $Id: pipmaker3.cgi.pl,v 1.16 2006/10/30 17:50:28 rico Exp $

BEGIN {
 $| = 1;
 umask(0002);
}

use 5.006_001;
use warnings;
use strict;

BEGIN {
 $::prefix  = "/usr/local/align"; # XXX - config - must match pipenv
 $::piphome = "$::prefix/pipmaker"; # XXX - config - must match pipenv
 $::run     = "$::piphome/run";
 $::spool   = "$::piphome/spool";
 $ENV{'PATH'} = "$::piphome/bin:$::prefix/bin:/usr/local/bin:/usr/bin:/bin";
 delete @ENV{'IFS', 'CDPATH', 'ENV', 'BASH_ENV'};
}

use lib "$::piphome/lib";

#use CGI '-debug';
use CGI;
use CGI::Carp; # 'fatalsToBrowser';
use Net::DNS::Resolver;
use PSU_BIO_PIP;

# Note!  This form, %d.%d.%d, is known to other programs!
my $tmpname = sprintf("%d.%d.%d", time(), getppid(), $$); # XXX, mkdtemp
my $tmppath = "$::spool/tmp/$tmpname";
my $newpath = "$::spool/new/$tmpname";
my $errpath = "$::spool/err/$tmpname";

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
	 if (defined($f) && is_binary_fh($f)) {
	     &err($q, "$a is not a plain text file");
	 }
	# local($,)=" "; print "$a $f "; print stat $f; print "<br>\n";
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
                s/\r\n/\n/g;			# dos -> unix
                s/\r/\n/g;			# mac -> unix
                s/[ \t]+$//gm;			# trim trailing whitespace
		$_ .= "\n" unless $_ =~ /\n$/;	# make sure last line is ok
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
	open(F, ">$file") or die "Cannot open $file: $!";
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
  my $n = 0;
  err $q, "Too many pending submissions.\n"
    if ($n += nsubdirs("$tmp/tmp")) > 50;
  err $q, "Too many submissions in the new-work queue.\n"
    if ($n += nsubdirs("$tmp/new")) > 50;
  err $q, "Too many jobs in the work queue.\n"
    if ($n += nsubdirs("$tmp/work")) > 50;
  return $n;
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
    err($q, "Parameter <i>$n</i> isn't of the form user\@host.domain: <i>$p</i>.")
        unless $p =~ /^[[:graph:]]+@([[:graph:]]+\.[[:graph:]]+)$/;
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
	--seq1 ./seq1data \\
	--seq2 ./seq2data \\
	--exon ./exons \\
	--rpts ./seq1mask \\
	--ulay ./colors \\
	--nvce ./novice \\
	--gxan ./genxan \\
	--anno ./annotations \\
	--align ./ualign
    };
    $res = "status=$?:$!:$@:" unless defined($res);
    stash_err($q, $t, "<pre>$res</pre>") if ($? != 0);
}

sub client_id {
    my $ra = $ENV{'REMOTE_ADDR'} || "";  $ra =~ tr{\012\015\n\r}{    };
    my $rh = $ENV{'REMOTE_HOST'} || "";  $rh =~ tr{\012\015\n\r}{    };
    my $ru = $ENV{'REMOTE_USER'} || "";  $ru =~ tr{\012\015\n\r}{    };
    my $ri = $ENV{'REMOTE_IDENT'} || ""; $ri =~ tr{\012\015\n\r}{    };
    my $at = $ENV{'AUTH_TYPE'} || "";    $at =~ tr{\012\015\n\r}{    };

    my $id = "REMOTE_ADDR=$ra\n" 
           . "REMOTE_HOST=$rh\n"
           . "REMOTE_USER=$ru\n" 
           . "REMOTE_IDENT=$ri\n"
           . "AUTH_TYPE=$at\n";

    return $id;
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

sub untaint {
	$_[0] =~ m/^(.*)$/so;
	$1;
}

sub YN {
    return (V($_[0]) =~ /1/) ? "yes" : "no";
}

sub check_deny {
    my $s = shift;
    return 0 unless $s;
    open(F,"<","$::piphome/etc/deny") or die $!;
    while (<F>) {
	chomp;
	err($q, "Sorry, <i>$s</i> is disallowed from using the server.")
	    if ($s eq $_);
    }
    close(F);
    return 0;
}


# --- sub main ---
check_server_status($q, $::run);
my $queue_size = check_queue_size($q, $::spool);

my $seq1data_str = eparam($q, 'seq1data_str');
my $seq1mask_str = eparam($q, 'seq1mask_str');
my $seq2data_str = eparam($q, 'seq2data_str');
my $exons_str =    eparam($q, 'exons_str');
my $colors_str =   eparam($q, 'colors_str');
my $align_str =    eupload($q, 'alignment_str');

my $seq1datafh = eupload($q, 'seq1data');
my $seq1maskfh = eupload($q, 'seq1mask');
my $seq2datafh = eupload($q, 'seq2data');
my $exonsfh =    eupload($q, 'exons');
my $colorsfh =   eupload($q, 'colors');
my $annotfh =    eupload($q, 'annotations');
my $alignfh =    eupload($q, 'alignment');

my $email =      eparam($q, 'email');

my $format =     eparam($q, 'format');
my $strand =     eparam($q, 'strand');
my $chain =      eparam($q, 'chain');
my $sensitivity= eparam($q, 'sensitivity');

my $genpip =     eparam($q, 'genpip');
my $piptitle =   eparam($q, 'piptitle');
my $gendot =     eparam($q, 'gendot');
my $genpdf =     eparam($q, 'genpdf');
my $gentxt =     eparam($q, 'gentxt');
my $genlat =     eparam($q, 'genlat');
my $genxan =     eparam($q, 'genxan');
my $genlav =     eparam($q, 'genlav');
my $genono =     eparam($q, 'genono');
my $gentig =     eparam($q, 'gentig');
my $genuly =     eparam($q, 'genuly');
my $genwww =     eparam($q, 'genwww');

my $whichests =  eparam($q, 'whichests');
my $genestsum =  eparam($q, 'genestsum');

my $id = client_id();
my $novice = eparam($q, 'novice');
   $novice = !defined($novice) ? 1 : ($novice != 0 ? 1 : 0);

check_email($q, "email", $email);
check_deny($email);
check_deny($ENV{'REMOTE_HOST'});
check_deny($ENV{'REMOTE_ADDR'});

mkdir $tmppath, 0775 or die "Cannot mkdir $tmppath: $!\n";

save_query($q, "$tmppath/SAVE") if $debuglevel > 0;
copystring('',         "$tmppath/app=pip3");
copystring($id,        "$tmppath/id");
copystring($email,     "$tmppath/email");
copystring($format,    "$tmppath/format");
copystring($strand,    "$tmppath/strand");
copystring($chain,     "$tmppath/chain");
copystring($sensitivity,"$tmppath/sensitivity");
copystring($genpip,    "$tmppath/genpip");
copystring($gendot,    "$tmppath/gendot");
copystring($genpdf,    "$tmppath/genpdf");
copystring($gentxt,    "$tmppath/gentxt");
copystring($genlat,    "$tmppath/genlat");
copystring($genxan,    "$tmppath/genxan");
copystring($genlav,    "$tmppath/genlav");
copystring($genono,    "$tmppath/genono");
copystring($gentig,    "$tmppath/gentig");
copystring($genuly,    "$tmppath/genuly");
copystring($piptitle,  "$tmppath/piptitle");
copystring($whichests, "$tmppath/whichests");
copystring($genestsum, "$tmppath/genestsum");
copystring($novice,    "$tmppath/novice");
copystring($genwww,    "$tmppath/genwww");

copyfs($seq1data_str, $seq1datafh,  "$tmppath/seq1data");
copyfs($seq1mask_str, $seq1maskfh,  "$tmppath/seq1mask");
copyfs($seq2data_str, $seq2datafh,  "$tmppath/seq2data");
copyfs($exons_str,    $exonsfh,     "$tmppath/exons");
copyfs($colors_str,   $colorsfh,    "$tmppath/colors");
copyfs(undef,         $annotfh,    "$tmppath/annotations");
copyfs($align_str,    $alignfh,    "$tmppath/ualign");

# my $dump = $q->Dump;
# copystring("<html><body>\n$dump\n</body></html>\n",   "$tmppath/dump");

my $syn = summary($tmppath);
#my $presyn = "<html><body><pre>\n$syn</pre></body></html>\n";
my $presyn = "<pre>\n$syn</pre>\n";
copystring($syn, "$tmppath/dump");

check_nonempty($q, $tmppath, "seq1data");
check_nonempty($q, $tmppath, "seq2data");

# if ($novice) {
#     check_lowercase($q, $tmppath, "seq1data");
#     check_lowercase($q, $tmppath, "seq2data");
# }

# consistency check
if ($genono && !$strand) {
    err($q, "Order and Orient assumes both strands are searched.\n"); 
}

check_lint($q, "$tmppath");

print $q->header, $q->start_html('PipMaker');

if (rename($tmppath, $newpath)) {
    print $q->h1("submitted"), $q->p(''), "Expect reply via email.", $q->p('');
    print "There are $queue_size jobs in the queue ahead of you.", $q->p('');
} else {
    print $q->h1("submission failed");
    # system("rm -rf $tmppath");
}

print $presyn;
print $q->end_html;
exit 0;

sub summary {
    my $tmp = shift;
    my $x = qx{ cd $tmp && seq_ident seq1data 2>/dev/null };
    my $s1id = untaint scalar qx{ cd $tmp && seq_ident seq1data 2>/dev/null };
    my $s2id = untaint scalar qx{ cd $tmp && seq_ident seq2data 2>/dev/null };
    my $str = "email: $email\n"
	. "seq1data: bp "  . $s1id . "\n"
	. "seq2data: bp "  . $s2id . "\n"
	. "seq1mask: " . file_lines("$tmp/seq1mask") . " lines \n"
	. "exons: "    . file_lines("$tmp/exons") . " lines \n"
	. "underlay: " . file_lines("$tmp/colors")  . " lines \n"
	. "annotations: " . file_lines("$tmp/annotations")  . " lines \n"
	. "alignment: " . file_lines("$tmp/ualign")  . " lines \n"
	. "search strand: "
		. ((V($strand) =~ /0/) ? "one" : "both") . "\n"
	. "coverage: "
		. (V($chain) =~ /0/ ? "show all matches" :
		  (V($chain) =~ /2/ ? "use chaining" :
                                       "single coverage")) . "\n"
	. "high sensitivity mode: " . YN($sensitivity) . "\n"
	. "pip title: " . V($piptitle) . "\n"
	. "generate pip: " . YN($genpip) . "\n"
	. "generate dotplot: " . YN($gendot) . "\n"
	. "data format: " . (V($genpdf) =~ /0/ ? "PostScript" :
			    (V($genpdf) =~ /1/ ? "PDF" :
                                                  "PDF with annotations")) ."\n"
	. "generate concise text: " . YN($gentxt) . "\n"
	. "generate traditional text: " . YN($genlat) . "\n"
	. "generate analysis of exons: " . YN($genxan) . "\n"
	. "return raw blastz output: " . YN($genlav) . "\n"
	. "return order and orientation: " . YN($genono) . "\n"
	. "split seq2 into contigs: " . YN($gentig) . "\n"
	. "return underlay generated for the overview: " . YN($genuly) . "\n"
	. "get results via: " . (V($genwww) =~ /1/ ? "WWW" : "email") . "\n"
	;
   $str =~ s/\n+/\n/g;
   $str =~ s/[ \t]+/ /g;
   return $str;
}

