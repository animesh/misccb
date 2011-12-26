#!/usr/bin/perl

use File::Spec;

$MAXSEQ = 2048;
$MINSEQ = 64;
$MINVEC = 100;

use AMOS::AmosLib;
use AMOS::AmosFoundation;

my $base = new AMOS::AmosFoundation;

if (! defined $base){
    die ("Couldn't instantiate AMOS::AmosFoundation!!\n");
}

$base->setUsage("Usage: trimfrg.pl file.frg file.nmers [file.clip]\n");
$HELPTEXT = q~
    ktrimfrg.pl file.frg file.nmers [file.clip]
~;

# invocation:
#  trimfrg.pl file.frg file.nmers [file.clip]

$frgfile = $ARGV[0];
my $splicename = $ARGV[1];
my $clipname = $ARGV[2];

my %clipleft; my %clipright;

if (defined $clipname) {
    open(CLIP, "$clipname") || die ("Cannot open $clipname: $!\n");
    while (<CLIP>){
	chomp;
	my ($seqname, $clipl, $clipr) = split(/\s+/, $_);
	$clipleft{$seqname} = $clipl - 1;
	$clipright{$seqname} = $clipr;
    }
    close(CLIP);
}

my @splice;
open(SP, $splicename) || die ("Cannot open $splicename: $!\n");
while (<SP>){
    chomp;
    push(@splice, $_);
}
close(SP);

my $tmpdir  = $base->getTempDir();
my $tmpfile = File::Spec->catfile($tmpdir, "tmp.".$$.".ktrim");

my $isCA = undef;

open(FRG, $frgfile) || die ("Cannot open $frgfile\n");
while ($record = getRecord(\*FRG)){
    my ($rec, $fields, $recs) = parseRecord($record);
    if ($rec eq "BAT"){
	$isCA = 1;
    }
    if ($rec eq "UNV"){ # AMOS
	open(TMP, ">$tmpfile") || $base->bail("Cannot open $tmpfile: $!\n");
    }
    if (defined $isCA){
	if ($rec eq "FRG"){
	    my $seq = $$fields{seq};
	    my @lines = split('\n', $seq);
	    $seq = join('', @lines);
	    $seq = uc($seq);
	    my $seqname;
	    @lines = split('\n', $$fields{src});
	    $seqname = join('', @lines);
	    
	    my ($l, $r) = split(',', $$fields{clr});
	    
	    print STDERR "ORIG: $$fields{acc} $l $r\n";
	    
	    if (exists $clipleft{$seqname}){
		print STDERR "LUCY: $$fields{acc} $clipleft{$seqname} $clipright{$seqname}\n";
		my $oldl = $l; my $oldr = $r;
		$l = $clipleft{$seqname}  if ($l < $clipleft{$seqname});
		$r = $clipright{$seqname}; # if ($r > $clipright{$seqname});
		print STDERR "sequence $$fields{acc} was trimmed by lucy from $oldl,$oldr to $l,$r\n" unless ($l == $oldl && $r == $oldr);
	    } elsif (defined $clipname){
		print STDERR "sequence $$fields{acc} was rejected by lucy\n";
		next;
	    }
	    
	    foreach $splice (@splice) {
		$splice = uc($splice);
		my $ind = index($seq, $splice);
		my $lastind = -1;
		while ($ind > 0 && $ind < $MINVEC){
		    $lastind = $ind;
		    $ind = index($seq, $splice, $ind + 1);
		}
	    
		if ($lastind > 0){
		    if ($l < $lastind + length($splice)){
			print STDERR "trimming sequence $$fields{acc} due to vector ", $lastind + length($splice) - $l + 1, "\n";
			$l = $lastind + length($splice) + 1;
		    }
		}
	    } # for each splice
	    
	    if ($r - $l < $MINSEQ) {
		print STDERR "skipping short sequence $$fields{acc}\n";
		next;
	    }
	    
	    my $qual = $$fields{qlt};
	    @lines = split('\n', $qual);
	    $qual = join('', @lines);
	    
# change Ns to As
	    my @seq = split('', $seq);
	    my @qual = split('', $qual);
	    for (my $i = 0; $i <= $#seq; $i++){
		if ($seq[$i] eq "N"){
		    $seq[$i] = "A";
		    $qual[$i] = "6";
		}
	    }
	    
	    my $seq = join('', @seq);
	    my $qual = join('', @qual);
	    
	    print "{FRG\n";
	    print "act:$$fields{act}\n";
	    print "acc:$$fields{acc}\n";
	    print "typ:$$fields{typ}\n";
	    print "src:\n$$fields{src}.\n";
	    print "etm:$$fields{etm}\n";
	    print "seq:\n";
	    for (my $i = 0; $i <= $#seq; $i+=60){
		print substr($seq, $i, ($#seq - $i > 60)? 60: ($#seq - $i + 1)), "\n";
	    }
	    print ".\n";
	    print "qlt:\n";
	    for (my $i = 0; $i <= $#qual; $i+=60){
		print substr($qual, $i, ($#qual - $i > 60)? 60: ($#qual - $i + 1)), "\n";
	    }
	    print ".\n";
	    print "clr:$l,$r\n";
	    print "}\n";
	    
	    $seen{$$fields{acc}} = 1;
	    next;
	} # if rec eq FRG
	if ($rec eq "LKG"){
	    if (exists $seen{$$fields{fg1}} && exists $seen{$$fields{fg2}}){
		print $record;
	    }
	    next;
	}
    } else { # AMOS record
	if ($rec eq "RED"){
	    my $seq = $$fields{seq};
	    my @lines = split('\n', $seq);
	    $seq = join('', @lines);
	    $seq = uc($seq);
	    my $seqname = $$fields{eid};
	    
	    my ($l, $r) = split(',', $$fields{clr});
	    
	    print STDERR "ORIG: $$fields{eid} $l $r\n";
	    
	    if (exists $clipleft{$seqname}){
		print STDERR "LUCY: $$fields{eid} $clipleft{$seqname} $clipright{$seqname}\n";
		my $oldl = $l; my $oldr = $r;
		$l = $clipleft{$seqname}  if ($l < $clipleft{$seqname});
		$r = $clipright{$seqname}; # if ($r > $clipright{$seqname});
		print STDERR "sequence $$fields{eid} was trimmed by lucy from $oldl,$oldr to $l,$r\n" unless ($l == $oldl && $r == $oldr);
	    } elsif (defined $clipname){
		print STDERR "sequence $$fields{acc} was rejected by lucy\n";
		next;
	    }
	    
	    foreach $splice (@splice) {
		$splice = uc($splice);
		my $ind = index($seq, $splice);
		my $lastind = -1;
		while ($ind > 0 && $ind < $MINVEC){
		    $lastind = $ind;
		    $ind = index($seq, $splice, $ind + 1);
		}
	    
		if ($lastind > 0){
		    if ($l < $lastind + length($splice)){
			print STDERR "trimming sequence $$fields{acc} due to vector ", $lastind + length($splice) - $l + 1, "\n";
			$l = $lastind + length($splice) + 1;
		    }
		}
	    } # for each splice
	    
	    if ($r - $l < $MINSEQ) {
		print STDERR "skipping short sequence $$fields{eid}\n";
		next;
	    }
	    
	    my $qual = $$fields{qlt};
	    @lines = split('\n', $qual);
	    $qual = join('', @lines);
	    
# change Ns to As
	    my @seq = split('', $seq);
	    my @qual = split('', $qual);
	    for (my $i = 0; $i <= $#seq; $i++){
		if ($seq[$i] eq "N"){
		    $seq[$i] = "A";
		    $qual[$i] = "6";
		}
	    }
	    
	    my $seq = join('', @seq);
	    my $qual = join('', @qual);
	    
	    print TMP "{RED\n";
	    print TMP "iid:$$fields{iid}\n";
	    print TMP "eid:$$fields{eid}\n";
	    print TMP "seq:\n";
	    for (my $i = 0; $i <= $#seq; $i+=60){
		print TMP substr($seq, $i, ($#seq - $i > 60)? 60: ($#seq - $i + 1)), "\n";
	    }
	    print TMP ".\n";
	    print TMP "qlt:\n";
	    for (my $i = 0; $i <= $#qual; $i+=60){
		print TMP substr($qual, $i, ($#qual - $i > 60)? 60: ($#qual - $i + 1)), "\n";
	    }
	    print TMP ".\n";
	    print TMP "frg:$$fields{frg}\n";
	    print TMP "clr:$l,$r\n";
	    print TMP "}\n";
	}
	$seen{$$fields{iid}} = 1;
    } # if AMOS file
    if (defined $isCA){
	print $record;
    }
}

if (! defined $isCA){
    close(TMP);

    seek FRG, 0, 0;  # rewind FRG
    while ($record = getRecord(\*FRG)){
	my ($rec, $fields, $recs) = parseRecord($record);
	if ($rec eq "FRG"){
	    my ($rda, $rdb) = split(',', $$fields{rds});
	    print "{FRG\n";
	    print "iid:$$fields{iid}\n";
	    print "eid:$$fields{eid}\n";
	    print "lib:$$fields{lib}\n";
	    print "typ:$$fields{typ}\n";
	    if (exists $seen{$rda} && exists $seen{$rdb}){ # no mates
		print "rds:$$fields{rds}\n";
	    }
	    print "}\n";
	    next;
	}
	if ($rec eq "RED"){next;}
	print $record;
    } 
    open(TMP, $tmpfile) || $base->bail("Cannot open $tmpfile: $!\n");
    while (<TMP>){
	print;
    }
    close(TMP);
    unlink($tmpfile) || $base->bail("Cannot remove $tmpfile: $!\n");
} # if not CA

close(FRG);
exit(0);
