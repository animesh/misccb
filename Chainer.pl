#!/usr/local/bin/perl

use TIGR::Foundation;
use strict;

my $VERSION = '$Revision: 1.2 $ ';
my $HELP = q~
    Joiner -ce contig_edge_file -co contig_overlap_file -c contig_file -o out

    assumes each pair of contigs occurs once in the contig_edge file
    assumes all overlaps are listed in contig_overlap file (more than one
   per contig pair)
~;

my $base = new TIGR::Foundation();
if (! defined $base) {
    die("A horrible death\n");
}

$base->setVersionInfo($VERSION);
$base->setHelpInfo($HELP);

my $edgefile;
my $ovlfile;
my $contigfile;
my $outfile;

my $err = $base->TIGR_GetOptions("ce=s"  => \$edgefile,
				 "co=s"  => \$ovlfile,
				 "c=s"   => \$contigfile,
                                 "o=s"   => \$outfile);

my %ctgpos;
my %ctglen;
my %ctgseq;
my $pos;

open(CTG, $contigfile) || $base->bail("Cannot open $contigfile: $!\n");

# first we build an index
while (<CTG>){
    if (/^\#\#(\S+) (\d+) (\d+)/){
	$ctglen{$1} = $3;
	$ctgseq{$1} = $2;
    }
}
close(CTG);

my %edges;

if (defined $outfile){
    open(STDOUT, ">$outfile") || $base->bail("cannot open $outfile: $!\n");
}

open(EDGE, $edgefile) || $base->bail("Cannot open $edgefile: $!\n");

while (<EDGE>){
    my ($ctgA, $ctgB, $ori, $mean, $sd, $count) = split('\t', $_);
    $edges{"$ctgA $ctgB $ori"} = "$mean $sd $count";
}

close(EDGE);

open(OVL, $ovlfile) || $base->bail("Cannot open $ovlfile: $!\n");

my $ctgA;
my $ctgB;
my %overlaps;
while (<OVL>){
    if (/^(\d+)\(\d+\) (\d+)\(\d+\)/) {
	$ctgA = $1;
	$ctgB = $2;
    }
    if (/^\t/){
	my @fields = split('\t', $_);
#	print "A: $fields[0] B: $fields[1] C: $fields[2]\n";
	$fields[2] =~ /(>|<) (>|<) (\d+) (\d+)/;
	my $ori; my $rori;
	my $ahang = $3;
	my $bhang = $4;
	my $dist; # distance between beginning of contigs

	if ($1 eq '<' && $2 eq '<'){
	    $ori = 'A'; # anti-normal
	    $rori = 'N';
	    $dist = $bhang;
	}
	if ($1 eq '<' && $2 eq '>'){
	    $ori = 'O'; # outie
	    $rori = 'O';
	    $dist = $ctglen{$ctgA} - $ahang;
	}
	if ($1 eq '>' && $2 eq '>'){
	    $ori = 'N'; # normal
	    $rori = 'A';
	    $dist = $ahang;
	}
	if ($1 eq '>' && $2 eq '<'){
	    $ori = 'I'; # innie
	    $rori = 'I';
	    $dist = $ctglen{$ctgA} + $bhang;
	}

	if (exists $edges{"$ctgA $ctgB $ori"}){
	    my ($mean, $sd, $count) = split(' ', $edges{"$ctgA $ctgB $ori"});
	    if (abs($mean - $dist) < 2 * $sd){ # accept overlap
		$overlaps{"$ctgA $ctgB $ori"} .= "$ahang,$bhang ";
	    } else {
		print STDERR "overlap and edge just don't agree: $ctgA $ctgB $ori $ahang $bhang\n";
	    }
	} elsif (exists $edges{"$ctgB $ctgA $rori"}){ # just the reverse
	    my ($mean, $sd, $count) = split(' ', $edges{"$ctgB $ctgA $rori"});
	    if (abs($mean - $dist) < 2 * $sd){ # accept overlap
		$overlaps{"$ctgB $ctgA $rori"} .= "$bhang,$ahang ";
	    } else {
		print STDERR "overlap and edge just don't agree: $ctgB $ctgA $rori $bhang $ahang\n";
	    }
	}
    } # if (^\t)
} # while <OVL>

close(OVL);

# now I need to check that overlaps are consistent
# essentially, if two or more contigs overlap the same
# end of a contig I throw away both

my %ends;
while (my ($adj, $hangs) = each %overlaps){
    my @hangs = split(' ', $hangs);
    
    my ($mean, $sd, $count) = split(' ', $edges{$adj});
    my ($ctgA, $ctgB, $ori) = split(' ', $adj);
    my ($ahang, $bhang) = split(',', $hangs[0]);
    # this I should be able to do better - make sure that 
    # all ahangs and bhangs are compatible and find the "consensus" set

    if (($ahang >= 0 && $bhang <= 0) ||
	($ahang <= 0 && $bhang >= 0)){
	print STDERR "CONTAINMENT $adj $ahang $bhang\n";
	# maybe get rid of this overlap for now??
	delete $overlaps{$adj};
	next;
    }

    # now I know that ahang and bhang have the same sign.
    if ($ahang > 0){
	if ($ori eq 'N'){
	    $ends{"$ctgA E"} .= "$adj:";
	    $ends{"$ctgB B"} .= "$adj:";
	} elsif ($ori eq 'A'){
	    $ends{"$ctgA B"} .= "$adj:";
	    $ends{"$ctgB E"} .= "$adj:";
	} elsif ($ori eq 'I'){
	    $ends{"$ctgA E"} .= "$adj:";
	    $ends{"$ctgB E"} .= "$adj:";
	} elsif ($ori eq 'O'){
	    $ends{"$ctgA B"} .= "$adj:";
	    $ends{"$ctgB B"} .= "$adj:";
	}
    } else { # ahang < 0
	if ($ori eq 'N'){
	    $ends{"$ctgA B"} .= "$adj:";
	    $ends{"$ctgB E"} .= "$adj:";
	} elsif ($ori eq 'A'){
	    $ends{"$ctgA E"} .= "$adj:";
	    $ends{"$ctgB B"} .= "$adj:";
	} elsif ($ori eq 'I'){
	    $ends{"$ctgA B"} .= "$adj:";
	    $ends{"$ctgB B"} .= "$adj:";
	} elsif ($ori eq 'O'){
	    $ends{"$ctgA E"} .= "$adj:";
	    $ends{"$ctgB E"} .= "$adj:";
	}
    } # if ahang > 0

    print STDERR "Contigs $adj have ", $#hangs + 1, 
    " overlaps supported by $count edges\n"; 
}

while (my ($end, $adj) = each %ends){
    my @adjs = split(':', $adj);
    if ($#adjs > 0){
	print STDERR "Contig $end has multiple overlaps: ";
	for (my $i = 0; $i <= $#adjs; $i++){
	    my @hangs = split(' ', $overlaps{$adjs[$i]});
	    my ($mean, $sd, $count) = split(' ', $edges{$adjs[$i]});	    
	    print STDERR "$adjs[$i] $count ", $#hangs + 1, "; ";
	    delete $overlaps{$adjs[$i]};
	}
	print STDERR "\n";
	delete $ends{$end};
    }
}

my @chains; # here we store chains of contigs in the format: id offset ori,...
my @chainlen;  # chain lengths
my @chainseq;  # sequences in chain


while (my ($adj, $hangs) = each %overlaps){
    my @hangs = split(' ', $hangs);
    
    my ($mean, $sd, $count) = split(' ', $edges{$adj});
    my ($ctgA, $ctgB, $ori) = split(' ', $adj);
    my ($ahang, $bhang) = split(',', $hangs[0]);
    print STDERR "Contigs $adj have ", $#hangs + 1, 
    " overlaps supported by $count edges\n"; 
}

my $done = 0;

while (! $done){
    $done = 1;
    while (my ($ctg, $len) = each %ctglen){
	my $myoffset;
	my $myori;
	my $nextadj;
	my @nextadj;
      
	$done = 0;
	if (! exists $ends{"$ctg B"}){
	    # start with the contig as forward
	    $myoffset = 0;
	    $myori = '>';
	    if (exists $ends{"$ctg E"}){
		@nextadj = split(':', $ends{"$ctg E"});
		$nextadj = $nextadj[0];
		if (! exists $overlaps{$nextadj}) { # we must have removed it
		    $nextadj = undef;
		}
	    }
	} elsif (! exists $ends{"$ctg E"}){
	    # start with the contig as reversed
	    $myoffset = 0;
	    $myori = '<';
	    if (exists $ends{"$ctg B"}){
		@nextadj = split(':', $ends{"$ctg B"});
		$nextadj = $nextadj[0];
		if (! exists $overlaps{$nextadj}) { # we must have removed it
		    $nextadj = undef;
		}
	    }
	} else {
	    next;
	}
	my @hangs = split(' ', $overlaps{$nextadj});
	my ($ahang, $bhang) = split(',', $hangs[0]);
	print STDERR "Starting with $ctg $myori and adjacency $nextadj: $ahang, $bhang\n ";
	print STDERR "$ctg B = ", $ends{"$ctg B"}, " $ctg E = ", $ends{"$ctg E"}, "\n";
	# now there can only be one contig off the other end
	$chains[++$#chains] .= "$ctg $myoffset $myori\n";
	$chainseq[++$#chainseq] += $ctgseq{$ctg};
	$#chainlen++;
	my $chainctg = 1;
	while (defined $nextadj){
	    my ($ctgA, $ctgB, $ori) = split(' ', $nextadj);
	    my @hangs = split(' ', $overlaps{$nextadj});
	    my ($ahang, $bhang) = split(',', $hangs[0]);
	    if ($ctg == $ctgA){
		# if ctg = > link can oly be N or I
		# if ctg = < link can only be A or O
		# ahang and bhang are both positive
		if ($ahang < 0 || $bhang < 0){
		    $base->bail("The hangs are all wrong ($ctg) - $nextadj, $ahang, $bhang\n");
		}
		if ($myori eq '>'){
		    if ($ori eq 'N'){
		    } elsif ($ori eq 'I'){
			$myori = '<';
		    } else {
			$base->bail("Wrong orientation $ctg $myori - $nextadj?");
		    }
		} else {
		    if ($ori eq 'A'){
			$myori = '<';
		    } elsif ($ori eq 'O'){
		    } else {
			$base->bail("Wrong orientation $ctg $myori - $nextadj?");
		    }
		}
		$myoffset += $ahang;
		delete $ctglen{$ctg};
		$ctg = $ctgB;
		$chains[$#chains] .= "$ctg $myoffset $myori\n";
		$chainseq[$#chainseq] += $ctgseq{$ctg};
		$chainctg++;
	    } elsif ($ctg == $ctgB){
		# if ahang is negative
		# if ctg = > link can only be N or O
		# if ctg = < link can only be A or I
		# if ahang is positive
		# if ctg = >  link can only be 
		# if ctg = < link can only be
		if ($ahang < 0){
		    if ($myori eq '>'){
			if ($ori eq 'N'){
			} elsif ($ori eq 'O'){
			    $myori = '<';
			} else {
			    $base->bail("Wrong orientation $ctg $myori - $nextadj?");
			}
		    } else {
			if ($ori eq 'A'){
			    $myori = '<';
			} elsif ($ori eq 'I'){
			} else {
			    $base->bail("Wrong orientation $ctg $myori - $nextadj?");
			}
		    }
		    if ($bhang > 0){
			$base->bail("The hangs are all wrong ($ctg) - $nextadj, $ahang, $bhang\n");
		    }
		    $myoffset -= $ahang;
		} else { #$ahang > 0
		    if ($myori eq '>'){
			if ($ori eq 'A'){
			} elsif ($ori eq 'I'){
			    $myori = '<';
			} else {
			    $base->bail("Wrong orientation $ctg $myori - $nextadj?");
			}
		    } else {
			if ($ori eq 'O'){
			    $myori = '>';
			} elsif ($ori eq 'N'){
			} else {
			    $base->bail("Wrong orientation $ctg $myori - $nextadj?");
			}
		    }
		    if ($bhang < 0){
			$base->bail("The hangs are all wrong ($ctg) - $nextadj, $ahang, $bhang\n");
		    }
		    $myoffset += $bhang;
		}
		delete $ctglen{$ctg};
		$ctg = $ctgA;
		$chains[$#chains] .= "$ctg $myoffset $myori\n";
		$chainseq[$#chainseq] += $ctgseq{$ctg};
		$chainctg++;
	    } else {
		$base->bail("$ctg is not in $nextadj\n");
	    }
	    if ($myori eq '<' && exists $ends{"$ctg B"}){
		@nextadj = split(':', $ends{"$ctg B"});
		$nextadj = $nextadj[0];
	    } elsif ($myori eq '>' && exists $ends{"$ctg E"}){
		@nextadj = split(':', $ends{"$ctg E"});
		$nextadj = $nextadj[0];
	    } else {
		$nextadj = undef;
	    }
	    if (! exists $overlaps{$nextadj}) { # we must have removed it
		$nextadj = undef;
	    }
	}
	$chainlen[$#chainlen] = $ctglen{$ctg} + $myoffset;  
	delete $ctglen{$ctg};
#	print STDERR "Chain ", $#chainlen, " is: $chains[$#chains]\n";
	print ">$#chainlen $chainctg $chainseq[$#chainseq] $chainlen[$#chainlen]\n";
	print $chains[$#chains];
    } # for each contig
} # while not done

exit(0);
