#!/usr/local/bin/perl -w
# -- make pip-generating commands for multiple blastz alignments 

use 5.006_000;
use strict;
use warnings;
#use diagnostics;

use Carp;
use Getopt::Long;

# -- Decl --
use subs qw(
    emit_species get_seqlen main
    max max_setrow max_url_annot
    sysf sysx total_height 
);

my $verbose = 0;
my $debug = 0;

my $TMP;
my $ROW_TAG_FILE;
my $tmpalign;
my $tmptags;
my $tmpundr;
my $tmp_pretag;
my $tmp_posttag;

# -- Parameters --
my $STDHDR = "/usr/local/align/pipmaker/lib/tag.style100";
my $ANNOT_MAGIC_STRING = "annot-link";

my $Title = "pip";
my $Author = "";

my $MAX_SPECIES = 55;

my $PAGE_HEIGHT = 11.0 * 72;
my $PAGE_WIDTH  =  8.5 * 72;
my $PAGE_ORIENT = 0;

# my $PPI         = 3_500; # pixels per inch in each pip element

my $LEFT_MARGIN  = 0.0; # pt
my $RIGHT_MARGIN = 0.0; # pt
my $TOP_MARGIN   = 0.0; # pt

my $ELT_WIDTH    = 20_000; # bp
my $ELT_HEIGHT   = 0.4 *72; # pt
my $INTR_ELT_GAP = 0.0 *72; # pt
my $TICS_HEIGHT  = 0.2 *72; # pt
#my $ANN0_HEIGHT  = 0.4 *72; # pt
my $ANN0_HEIGHT  = 0.3 *72; # pt
my $ANN1_HEIGHT  = 2.4; # pt
my $INTR_ANN_GAP = 0.0; # pt
#my $INTR_STANZA_GAP = 0.5 *72; # pt
my $INTR_STANZA_GAP = 0.0 *72; # pt

my $VTIC_MINPCT  = 50;
my $HTIC_STEP = 1000;
my $TLAB_MOD  = 2000;
my $PIP_START = 0;
my $PIP_END = 10e16;
my $PIP_ORIGIN = 0;

my $TOP_GAP    = $ELT_HEIGHT+$ANN0_HEIGHT;

exit &main;

# ---

sub sysx
{
    my $s = shift;
    print STDERR "+ $s\n" if $verbose;
    $s = "/usr/bin/time --format='? %er %Uu %Ss' $s" if $verbose;
    system($s) == 0 or croak "system '$s' == $?: $!";
}

sub sysf
{
    my ($f, @r) = @_;
    croak "bad format" unless @r;
    sysx sprintf($f, @r);
}

sub cat
{
    my $f = shift;
    open(F, "<", $f) or die "$! ($f)";
    print while (<F>);
    close F;
}

sub include_epsf
{
    my $f = shift;
    # not really complete
    print "%%BeginDocument: xxx\n";
    print "gsave\n";
    cat $f;
    print "grestore\n";
    print "%%EndDocument\n";
}

sub stanza_tophalf {
    my $na0 = shift;
    my $na1 = shift;
    return $INTR_STANZA_GAP+($ANN1_HEIGHT*$na1)+($ANN0_HEIGHT*$na0)+$TICS_HEIGHT;
}

sub stanza_height {
    my $nspecies = shift;
    my $na0 = shift;
    my $na1 = shift;
    return stanza_tophalf($na0,$na1)+$ELT_HEIGHT*($nspecies-1);
}

sub movedown
{
    my $n = shift;
    printf "0 %d translate\n", -$n;
    return $n;
}

sub startstanza
{
    my $h = shift;
    return movedown($h);
}

sub home
{
    my $lm = shift;
    my $ht = shift;
    printf "0 0 moveto   %d %d translate\n", $lm, $ht;
}

sub startdoc
{
    my $wd = shift;
    my $ht = shift;
    my $hdrs = shift;

    print  "%!PS-Adobe-3.0\n";
    printf "%%%%BoundingBox: 0 0 %d %d\n", $wd, $ht;
    print  "%%Title: $Title\n";
    print  "%%For: $Author\n";
    print  '%%Creator: mpip; $Id: mpip.pl,v 1.34 2003/06/11 20:02:05 schwartz Exp schwartz $', "\n";
    printf "%%%%CreationDate: %s\n", scalar localtime time;
    print  "%%Pages: (atend)\n";
    print  "%%EndComments\n";

    print  "%%BeginProlog\n";
    sysx   "pdf-header ";
    sysx   "pmps -procset ";
    foreach my $f (@{$hdrs}) { cat $f; }
    print  "%%EndProlog\n";

    print  "%%BeginSetup\n";
    print  "<< /PageSize [$wd $ht] >> setpagedevice\n";
    print  "[/Title (pip) /Dest [{ThisPage} /FitB] /OUT pdfmark\n";
    print  "%%EndSetup\n";
}

sub startpage
{
    my $num = shift;
    my $wd =  shift;
    my $ht =  shift;
    # print  "%!PS-Adobe-3.0 EPSF-3.0\n";
    # printf "%%%%BoundingBox: 0 0 %d %d\n", $wd, $ht;
    # print  "%%EndComments\n";

    print  "%%Page: $num $num\n";
    print  "gsave\n";
    home($LEFT_MARGIN, $ht);
    return movedown $TOP_MARGIN+$TOP_GAP;
}

sub endpage
{
    print  "grestore\n";
    print  "showpage\n";
}

sub enddoc
{
    print "%%Trailer\n";
    print "%%Pages: $_[0]\n";
    print "%%EOF\n";
}

my ($tag_file, $underlay_file);
my $legend_file;
my $legend_height = 72; # pt # XXX - should read bbox
my @header;
my @species;
my @align_arg;

sub species_name
{
    my $n = shift;
    die if ($n > $#species);
    die if ($n < 0);
    return $species[$n];
}

sub align_file
{
    my $n = shift;
    die if ($n-1 > $#align_arg);
    die if ($n-1 < 0);
    return $align_arg[$n-1];
}

my $collecting = "-";
my @preunderlay_arg = ();
my @postunderlay_arg = ();

sub collector
{
    if ($collecting) { # XXX -- undefined sometimes?
	print STDERR "collecting $collecting <- ", @_, "\n";
	if ($collecting eq "preunderlays") {
	    push @preunderlay_arg, @_;
	} elsif ($collecting eq "postunderlays") {
	    push @postunderlay_arg, @_;
	} else {
	    die("!FINISH");
	}
    }
}

sub collect_preunderlays
{
    $collecting = "preunderlays";
    @preunderlay_arg = ();
}

sub collect_postunderlays
{
    $collecting = "postunderlays";
    @postunderlay_arg = ();
}

sub preunderlay_files
{
    my $n = shift;
    die if ($n-1 < 0);
    return undef if ($n-1 > $#preunderlay_arg);
    return $preunderlay_arg[$n-1];
}

sub postunderlay_files
{
    my $n = shift;
    die if ($n-1 < 0);
    return undef if ($n-1 > $#postunderlay_arg);
    return $postunderlay_arg[$n-1];
}

sub dwim_align2species
{
    # XXX - DWIM
    my (@s, $n);
    #push @s, "0";
    push @s, "";
    foreach (@_) {
	++$n;
	if (/^(.+)\.blastz$/) {
	    push(@s,$1);
	} else {
	    #push(@s,$n);
	    push(@s,"");
	}
	
    }
    return @s;
}

sub dwim_species2align
{
    my @s;
    shift;
    foreach (@_) {
	push(@s,"$_.blastz");
    }
    return @s;
}



sub main
{
    my $id = time() . ".$$";
    my ($bp_on_page, $blocks_on_page);
    my $nspecies;
    my $tmp = "tmp.$id";

    push(@header,$STDHDR) if (-f $STDHDR);
    $tag_file = "/dev/null";
    $underlay_file = "/dev/null";

    die unless GetOptions(
	"debug" => \$debug,
	"verbose|v" => \$verbose,
	"tmpdir=s" => \$TMP,

	"title=s" => \$Title,
	"author=s" => \$Author,

	"pagewidth=f" => \$PAGE_WIDTH,
	"pageheight=f" => \$PAGE_HEIGHT,

	"topmargin=i" => \$TOP_MARGIN,
	"leftmargin=i" => \$LEFT_MARGIN,
	"rightmargin=i" => \$RIGHT_MARGIN,

	"pipwidth=i" => \$ELT_WIDTH,
	"pipstart=i" => \$PIP_START,
	"pipend=i" => \$PIP_END,
	"origin=i" => \$PIP_ORIGIN,

	"hticstep=i" => \$HTIC_STEP,
	"tlabmod=i" => \$TLAB_MOD,
	"vticminpct=i" => \$VTIC_MINPCT,

        "header|h|l=s" => \@header,

        "tag|t=s" => \$tag_file,
        "underlay|u=s" => \$underlay_file,
        "legend=s" => \$legend_file,
        "legendheight|lht=i" => \$legend_height,

        "speciesname|species|s=s" => \@species,
        "alignfile|align|a=s" => \@align_arg,

	"<>" => \&collector,
	"underlays|w" => \&collect_postunderlays,
	"preunderlays" => \&collect_preunderlays,
	"postunderlays" => \&collect_postunderlays,
    );

    die("require: -h header  -t tag  -u underlay") unless
        @header && $tag_file && $underlay_file;

# cases:
# . -s -s -s -s -a -a -a
# . -s -s -s -s
# . -a -a -a
# . f f f f
# . err

    @species = @ARGV unless @species;
    @species = dwim_align2species @align_arg unless @species;
    @align_arg = dwim_species2align @species unless @align_arg;
    $nspecies = $#species + 1;

    $TMP ||= ".";

{print STDERR "align_arg = ", join(",",@align_arg), "\n";
 print STDERR "species = ", join(",",@species), "\n";
 print STDERR "underlays = ", join(",",@preunderlay_arg), "\n";
 print STDERR "underlays = ", join(",",@postunderlay_arg), "\n";
 print STDERR "TMP = $TMP\n";
}
    die("require: species1..N") unless $nspecies > 1;
    die("too many species") if $nspecies > $MAX_SPECIES;
    die("nspecies != 1+nalign") if ($#species != 1+$#align_arg);

    $ROW_TAG_FILE = "$TMP/$tmp.vtag";

    my ($from,$to) = get_seqbounds(align_file(1));
    (undef,$from,$to,undef) = sort {$a <=> $b} ($from,$PIP_START,$PIP_END,$to);
    printf STDERR "bounds [$from,$to]\n";

    my $ht = stanza_height($nspecies, 1, 0);
    die("page too small ($PAGE_HEIGHT < $ht)") if
	$PAGE_HEIGHT < $ht;

# -- split up
    $tmpalign = "$TMP/$tmp.a";
    $tmptags = "$TMP/$tmp.t";
    $tmpundr = "$TMP/$tmp.u";
    $tmp_pretag = "$TMP/$tmp.v";
    $tmp_posttag = "$TMP/$tmp.w";

    for (my $i = 1; $i < $nspecies; ++$i) {
	mkdir "$tmpalign.$i" or die "$!: $tmpalign.$i\n";
        sysf("split-align <%s $tmpalign.$i $from $to $ELT_WIDTH",
            align_file($i));
	my $u = preunderlay_files($i);
	if ($u) {
            mkdir "$tmp_pretag.$i" or die "$!: $tmp_pretag.$i\n";
	    sysf("split-tags <%s $tmp_pretag.$i $from $to $ELT_WIDTH", $u);
	}
	$u = postunderlay_files($i);
	if ($u) {
            mkdir "$tmp_posttag.$i" or die "$!: $tmp_posttag.$i\n";
	    sysf("split-tags <%s $tmp_posttag.$i $from $to $ELT_WIDTH", $u);
	}
    }
    mkdir $tmpundr or die "$!: $tmpundr\n";
    sysx("split-tags <$underlay_file $tmpundr $from $to $ELT_WIDTH");
    mkdir $tmptags or die;
    sysx("group_tags <$tag_file |split-tags $tmptags $from $to $ELT_WIDTH");
	# XXX - pipe loses status
	# XXX - security
# --

    startdoc($PAGE_WIDTH, $PAGE_HEIGHT, \@header);
    my $avail = 0;
    my $stanza = 0;
    my $nb = 1;
    my $page = 0;
    my ($begin, $end);
    for ($begin = $from-1; $begin < $to; $begin = $end) {
    	++$stanza;
        $end = $begin + $ELT_WIDTH;
        $end = $to if ($end > $to);
	
	my $b1 = $begin+1;
	my $ms = max_setrow("$tmptags/$b1");
	my $mu = max_url_annot("$tmptags/$b1");

	my $a1 = $mu;
	my $a0 = $ms + 1;
	$a0 = 0.5 if ($a0 == 0 && $a1 == 0);
#	my $a1 = $mu > 0 ? 1 : 0;

        # need(enough for whole stanza);
	if ($avail <= stanza_height($nspecies, $a0, $a1)) {
	    endpage if ($page != 0);
            ++$page;
            $nb = 1;
            $avail = $PAGE_HEIGHT - startpage($page, $PAGE_WIDTH, $PAGE_HEIGHT);
	    # XXX - kludge
	    if ($legend_file) {
		$avail -= movedown($legend_height);
		include_epsf($legend_file);
		$legend_file = "";
	    }
	    print STDERR ".";
        }

#print STDERR "page $page; setrow $ms; urlannot $mu; a0 $a0; a1 $a1\n";

	$avail -= startstanza(stanza_tophalf($a0, $a1));
	printf "DOTPLOT begin /URL-annot-base %d def end\n", $ms+1;
	$avail -= emit_species($begin,$end,$nspecies,$id,$page,$stanza,$nb);
        ++$nb;
    }
    endpage;
    enddoc($page);

    if ($debug == 0) {
	unlink($ROW_TAG_FILE);
	sysf("rm -rf %s*", "$TMP/$tmp");
    }
    return 0;
}

sub file_or_devnull {
    my $f = shift;
    (-f $f) ? $f : "/dev/null";
}

sub emit_species
{
    my $avail = 0;
    my $begin = shift;
    my $end = shift;
    my $nspecies = shift;
    my $id = shift;
    my $page = shift;
    my $stanza = shift;
    my $nb = shift;
    my $b1 = $begin+1;

    for (my $i = 1; $i < $nspecies; ++$i) {
	#my $PPI  = int(0.5+$ELT_WIDTH/($PAGE_WIDTH/72-2.0)); # XXX 2.0?
	#my $PPI  = int(0.5+$ELT_WIDTH/($PAGE_WIDTH/72-2.0)); # XXX 2.0?
	my $width = $PAGE_WIDTH - $LEFT_MARGIN - $RIGHT_MARGIN - (2.0*72);
	my $PPI  = ($ELT_WIDTH / $width) * 72;
	my $Y100 = int(0.5+$PPI*$ELT_HEIGHT*1.5/72); # 100% mark
	my $Y50  = int(0.5+$Y100*$VTIC_MINPCT/100);

	print STDERR "= $id.$page-$stanza-$i\n" if $verbose;

	# mpm_rowtag, inline
	my $sp = species_name($i);
	$sp =~ tr/()[]\\/_____/;
	open(F, ">", $ROW_TAG_FILE) or die "$!: $ROW_TAG_FILE\n";
	print F "extra-labels\n";
	print F "[[($sp) /Times-Roman]] center-left-banner\n";
	close F;

	print "%%BeginDocument: $id.$page-$stanza-$i\n";

#	my $tmpalign = (-f $tmpalign.$i/$b1) ? $tmpalign.$i/$b1 : "/dev/null";
#	my $tmpundr = (-f $tmpundr/$b1) ? $tmpundr/$b1 : "/dev/null";
#	my $tmptags = (-f $tmptags/$b1) ? $tmptags/$b1 : "/dev/null";

	my $tmp_pretag = file_or_devnull "$tmp_pretag.$i/$b1";
	my $tmp_posttag = file_or_devnull "$tmp_posttag.$i/$b1";

	my $s = "pmps -Y $Y100 -w 1"
	    .   " -align $tmpalign.$i/$b1"
	    .   " -utag $tmp_pretag $tmpundr/$b1 $tmp_posttag"
	    .   (($i==1) ? " -htag $tmptags/$b1" : "")
	    .   " -vtag $ROW_TAG_FILE"
	    .   " -dot -eps -nb -ppi=$PPI"
	    .   (($i==1 && $nb==1) ? " -vt=50 -percent" : " -nv")
	    .   (($i==$nspecies-1) ? " -ht=$HTIC_STEP -tl=$TLAB_MOD" : " -nh")
	    .   " -noprocset -noshowpage -portrait -strictlabels"
	    .   " -xticbias=$PIP_ORIGIN"
	    .   " -ticfontsize=8 -labelfontsize=9 -bannerfontsize=10"
	    .   " $begin $end $Y50 $Y100 "
	    ;
	sysx $s;

	print "%%EndDocument: $id.$page-$stanza-$i\n";

	$avail += movedown($ELT_HEIGHT);
    }
    return $avail;
}

sub get_seqbounds
{
    my $path = shift;

    die "alignment file ($path) must be seekable; sorry!\n" if (-p $path);
    open(SF, '<', $path) or die "$! ($path)";

    while (<SF>) {
        next unless (/^s \{/);
        last unless defined($_ = <SF>); 
        return ($2,$3) if (/\s*("[^"]*")\s+(\d+)\s+(\d+)/);
    }
    close SF;
    warn("could not find sequence bounds in alignment file.");
    return (1,1);
}

sub get_seqlen
{
    my $path = shift;
    open(SF, '<', $path) or die "$! ($path)";

    while (<SF>) {
        next unless (/^s \{/);
        last unless defined($_ = <SF>); 
        return $3 if (/\s*("[^"]*")\s+(\d+)\s+(\d+)/);
        # XXX $3 <= 0?
    }
    close SF;
    die("could not find sequence length in alignment file.");
}

sub max_setrow
{
    my $f = shift;
    open(F, "<", $f) or die "$! ($f)";

    my $n = -1;
    while (<F>) {
	if (/^\s*([-.\d]*\d+)\s+setrow/) {
	    $n = max($n,$1);
	    #print STDERR "MAX SETROW $n : $f: $_";
	}
    }
    close F;
    return $n ;
}

sub max_url_annot
{
    my $f = shift;
    open(F, "<", $f) or die "$! ($f)";

    my $n = 0;
    while (<F>) {
	chomp;
	my @F = split;
	if ($#F > 1 && $F[$#F] =~ /$ANNOT_MAGIC_STRING/o) {
	    #my $r = $F[$#F-1];
	    my $r = $F[2];
	    $n = $r if $r > $n;
	}
    }
    close F;
    return $n;
}

sub max
{
    return $_[0] > $_[1] ?  $_[0] : $_[1] ;
}
