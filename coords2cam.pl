#!/usr/local/bin/perl
# coords2cam.pl - creates a celamy file from NUCmer .coords output

use strict;

my $MINPERC = 30; 
my $SEP = 500;

my @alignments;
my %query; 
my %ref;

my $HEAD = 
q~0refColor: CFF0000 T2 S # reference
0queryHangColor: C008000 T2 S # hang
0queryHitColor: C00FF00 T2 S # hit
~;

my $REFCOLOR = "A0refColor";
my $QUERYHANGCOLOR = "A0queryHangColor";
my $QUERYHITCOLOR = "A0queryHitColor";


print STDERR "reading input...\n";

while (<>){
    chomp;
    my @fields = split('\t');

    @{$alignments[++$#alignments]} = @fields;

    ${$query{$fields[12]}}[++$#{$query{$fields[12]}}] =  $#alignments;
}

print STDERR "done\n";

print STDERR "processing queries...\n";

while (my ($qid, $aligns) = each %query){
    
#    print STDERR "checking $qid: ", $#$aligns + 1, "\n";

    my @chosen;
    my @hits = sort {$alignments[$b][10] <=> $alignments[$a][10]} @{$aligns};

#    print STDERR "starting with ", $#hits + 1, " hits\n";
    # here we keep those alignments that do not overlap each other.
    for (my $h = 0; $h <= $#hits; $h++){
	my $clean = 1;
	if ($alignments[$hits[$h]][10] <= $MINPERC) { 
# only check the "short" hits
	    for (my $c = 0; $c <= $#chosen; $c++){
		if (ovl($chosen[$c], $hits[$h], "query")){
		    $clean = 0;
		    last;
		}
	    }
	}
	if ($clean) { # no overlap detected
	    push(@chosen, $hits[$h]); # add to the chosen overlaps
	    my $rname = $alignments[$hits[$h]][11];
	    ${$ref{$rname}}[++$#{$ref{$rname}}] =  $hits[$h];
	}
    }
#    print STDERR "ending with ", $#chosen + 1, " hits\n";

    if ($#chosen > 0) { # check if query bridges two references
	

    }
    
    @{$query{$qid}} = @chosen;
} # while each query

print STDERR "done\n";

print STDERR "processing references...\n";

print "$HEAD";

my $offset = 0;
my $id = 0;

foreach my $rid (sort {$alignments[${$ref{$b}}[0]][7] <=> $alignments[${$ref{$a}}[0]][7] } (keys %ref)) { 
    my $aligns = $ref{$rid};
#    print STDERR "$rid $alignments[${$ref{$rid}}[0]][7]\n";
# do reference hits in order of their coordinate
    my @hits = sort {$alignments[$a][0] <=> $alignments[$b][0]} @{$aligns};

    my @queries = ();
    my $minx;
    my $maxx;

    for (my $h = 0; $h <= $#hits; $h++){
	my $rl = $alignments[$hits[$h]][0];
	my $rr = $alignments[$hits[$h]][1];
	my $ql = $alignments[$hits[$h]][2];
	my $qr = $alignments[$hits[$h]][3];
	my $qlen = $alignments[$hits[$h]][8];

	$#queries++;
	
	if ($ql > $qr) { #sequence is reversed
	    $queries[$#queries][0] = $rl - $qlen + $ql;
	    $queries[$#queries][1] = $rl;
	    $queries[$#queries][2] = $rr;
	    $queries[$#queries][3] = $rr + $qr;
	} else {
	    $queries[$#queries][0] = $rl - $ql;
	    $queries[$#queries][1] = $rl;
	    $queries[$#queries][2] = $rr;
	    $queries[$#queries][3] = $rr + $qlen - $qr;
	}
	$queries[$#queries][4] = $alignments[$hits[$h]][12];

	if (! defined $minx || $minx > $queries[$#queries][0]){
	    $minx = $queries[$#queries][0];
	}
	if (! defined $maxx || $maxx < $queries[$#queries][3]){
	    $maxx = $queries[$#queries][3];
	}
    } # for each hit

    if ($minx < 0) {
	$offset -= $minx;
    }

    if ($maxx < $alignments[$hits[0]][7]){
	$maxx = $alignments[$hits[0]][7];
    }

    $id++;
    print "${id}ref: $offset $REFCOLOR ", $offset + $alignments[$hits[0]][7], 
    "R1 # $alignments[$hits[0]][11]\n";
    for (my $q = 0; $q <= $#queries; $q++){
	$id++;
	print "${id}query: ", $offset + $queries[$q][0], " $QUERYHANGCOLOR ",
	$offset + $queries[$q][1], " $QUERYHITCOLOR ", $offset + $queries[$q][2], 
	" $QUERYHANGCOLOR ", $offset + $queries[$q][3], " R2 # $queries[$q][4]\n";
    }

    $offset += $maxx + $SEP;
    
} # while each reference

print STDERR "done\n";


exit(0);

# this figures out if two alignment ranges overlap
sub ovl
{
    my $aid = shift;
    my $bid = shift;
    my $where = shift;

    my $L;
    my $R;

    if ($where eq "ref"){
	$L = 0;
	$R = 1;
    } elsif ($where eq "query"){
	$L = 2;
	$R = 3;
    } else {
	die ("Don't know where: $where\n");
    }

    my $al = ($alignments[$aid][$L] < $alignments[$aid][$R]) 
	? $alignments[$aid][$L] 
	: $alignments[$aid][$R];

    my $ar = ($alignments[$aid][$L] < $alignments[$aid][$R]) 
	? $alignments[$aid][$R] 
	: $alignments[$aid][$L];

    my $bl = ($alignments[$bid][$L] < $alignments[$bid][$R]) 
	? $alignments[$bid][$L] 
	: $alignments[$bid][$R];

    my $br = ($alignments[$bid][$L] < $alignments[$bid][$R]) 
	? $alignments[$bid][$R] 
	: $alignments[$bid][$L];

#    print STDERR "checking $al, $ar vs $bl, $br ";

    if ($al < $br && $bl < $ar) {
#	print STDERR "nogo\n";
	return 1;
    } else {
#	print STDERR "go\n";
	return 0;
    }
    
} # sub ovl
