#!/usr/local/bin/perl

# $Id: toArachne.pl,v 1.2 2004/04/28 18:45:36 mpop Exp $

#  Copyright @ 2002, 2003, The Institute for Genomic Research (TIGR).  All
#  rights reserved.

use strict;
use DBI;
use TIGR::Foundation;

my $REVISION = '$Revision: 1.2 $ ';
our $VERSION = '1.0';
our $VERSION_STRING = "$VERSION (Build $REVISION)";

my $REPEAT_RANGE = 30;  # minimum overlap with the repeat region.

my $__HELP_INFO =
q~NAME
toArachne - generates inputs for GMOD

SYNOPSIS
toArachne [-D database]  [ -S server ] [ -U username ]
	 { { -a asm_file } | { -C contig_file }| {-m mate_file}
         } [-o out_prefix]
         [ -debug level ] [ -h ] [ -V ]

SUMMARY

PARAMETERS

OPTIONS

EXIT CODES
   0   Successful completion
   1   Failed completion


EXAMPLES

USAGE

SEE ALSO

~;

# initiate / define a TIGR Foundation object in the global scope
my $base = new TIGR::Foundation;
if (! defined $base){
    print STDERR "the sky is falling, run away!\n";
    exit(1);
}

$base->setVersionInfo($VERSION_STRING);
$base->setHelpInfo($__HELP_INFO);

# set up application defaults
my $SERVER          = "SYBTIGR";
my $USER            = "access";
my $PASSWD          = "access";
my $DBTYPE          = "Sybase";
my $dbproc;

my $db;
my $matefile;
my $asmfile;
my $acefile;
my $grpinp;
my $outFile = "assembly";
my $contigFile;    # contig file

# global variables
my $minSeqId = 1;  # for numbering sequences
my %seqids;        # correspondence between seq_names and ids
my %seqnames;
my %seq_range;     # range of sequences
my %contigs;       # correspondence between contigs and lengths
my %seqcontig;     # correspondence between sequences and contigs
my %contigseq;     # correspondence between contigs and sequences
my $matesDone = 0; # keep track of whether we found the mates
my %forw;          # forward end of insert
my %rev;           # reverse end of insert
my %insertlib;     # inserts for each library
my %seenlib;       # library for each insert
my %libraries;     # library parameter
my %libMap;        # mapping between 4 letter codes and actual libraries
my %contigcons;
my $scffile;
my @scaffolds;
my @scaffCtg;
my @scaffctgs;
my @scaffbases;
my %obssize;

# parse command line parameters
my $err = $base->TIGR_GetOptions("D=s"  => \$db,
  				 "S=s"  => \$SERVER,
  				 "U=s"  => \$USER,
				 "P=s"  => \$PASSWD,
  				 "m=s"  => \$matefile,
  				 "a=s"  => \$asmfile,
  				 "g=s"  => \$grpinp,
				 "o=s"  => \$outFile,
				 "C=s"  => \$contigFile,
				 "ace=s" => \$acefile,
				 "s=s" => \$scffile);

if ($err == 0){
    $base->bail("Command parsing failed.  See -h option");
}

if (! defined $matefile && 
    (defined $asmfile || defined $contigFile || defined $acefile)) {
    if (!defined $db){
	$base->bail("You must provide either a mate file or a database!");
    }
    $dbproc = DBI->connect("dbi:$DBTYPE:server=$SERVER", $USER, $PASSWD);
    if (! defined $dbproc){
	$base->bail("Connection to server \"$SERVER\" failed");
    }

    $dbproc->do("use $db") || 
	$base->bail("Failed to open database \"$db\"");
}

if (! defined $db && ! defined $matefile){
    $base->bail("Either -D or -m must be defined.  See -h option");
}

if (defined $asmfile){
    open(IN, $asmfile) || $base->bail("Cannot open \"$asmfile\" : $!");
    
    parseAsmFile(\*IN);

    close(IN);
}

if (defined $contigFile){
    open(IN, $contigFile) || 
	$base->bail("Cannot open \"$contigFile\" : $!");
    
    parseContigFile(\*IN);

    close(IN);
}

if (defined $acefile){
    open(IN, $acefile) ||
	$base->bail("Cannot open \"$contigFile\" : $!");
    
    parseACEFile(\*IN);
    
    close(IN);
}


# since some files contain the mating relationship we only do this step
# for those who don't.

my %seeninsert;
my %seqinsert;
# here we generate the insert hashes: %insertlib, %libraries, %forw, %rev
# %seqlib %seqmate;
if (! $matesDone){
    if (defined $db){ # do the TIGR thing
	# this is where the get_mates routines will go in
	
	while (my ($snm, $sid) = each %seqids){
	    if (length($snm) > 10){
		next; # get rid of closure seqs
	    }
	    my $insert = substr($snm, 0, 7);
	    my $library = substr($snm, 0, 4);
	    if (! exists $seeninsert{$insert}){
		$insertlib{$library} .= "$insert ";
		$seeninsert{$insert} = 1;
	    }
	    $seqinsert{$snm} = $insert;
	    $seenlib{$insert} = $library;
	    $libraries{$library} = "";
	    if (substr($snm, 7, 1) eq "F" ||
		substr($snm, 8, 1) eq "F"){
		if (! exists $forw{$insert} ||
		    $snm gt $forw{$insert}){
		    $forw{$insert} = $snm;
		}
	    } elsif (substr($snm, 7, 1) eq "R" ||
		     substr($snm, 8, 1) eq "R"){
		if (! exists $rev{$insert} ||
		    $snm gt $rev{$insert}) {
		    $rev{$insert} = $snm;
		}
	    }
	}
	while (my $lib = each %insertlib){
	    get_lib_info($lib);
	}
    } elsif (defined $matefile) { # the mate file contains either mates
                                  # or regular expressions defining them
	open(MATE, $matefile) || $base->bail("Cannot open \"$matefile\": $!\n");
	my @libregexp;
	my @libids;
	my @pairregexp;
	my $insname = 1;
	while (<MATE>){
	    chomp;
	    if (/^library/){
		my @recs = split('\t', $_);
		if ($#recs < 3|| $#recs > 4){
		    print STDERR "Only ", $#recs + 1, " fields\n";
		    $base->logError("Improperly formated line $. in \"$matefile\".\nMaybe you didn't use TABs to separate fields\n", 1);
		    next;
		}

		if ($#recs == 4){
		    $libregexp[++$#libregexp] = $recs[4];
		    $libids[++$#libids] = $recs[1];
		}
		$libraries{$recs[1]} = "$recs[2] $recs[3]";
		next;
	    } # if library
	    if (/^pair/){
		my @recs = split('\t', $_);
		if ($#recs != 2){
		    $base->logError("Improperly formated line $. in \"$matefile\".\nMaybe you didn't use TABs to separate fields\n");
		    next;
		}
		@pairregexp[++$#pairregexp] = "$recs[1] $recs[2]";
		next;
	    }
	    if (/^\#/) { # comment
		next;
	    }
	    if (/^\s*$/) { # empty line
		next;
	    }

	    # now we just deal with the pair lines
	    my @recs = split('\t', $_);
	    if ($#recs < 1 || $#recs > 2){
		$base->logError("Improperly formated line $. in \"$matefile\".\nMaybe you didn't use TABs to separate fields\n");
		next;
	    }

# make sure we've seen these sequences
	    if (! defined $seqids{$recs[0]}){
		$base->logError("No contig contains sequence $recs[0] at line $. in \"$matefile\"");
		next;
	    }
	    if (! defined $seqids{$recs[1]} ){
		$base->logError("No contig contains sequence $recs[1] at line $. in \"$matefile\"");
		next;
	    }

	    if (defined $recs[2]){
		$insertlib{$recs[2]} .= "$insname ";
		$seenlib{$insname} = $recs[2];
	    }

	    $forw{$insname} = $recs[0];
	    $rev{$insname} = $recs[1];

	    $seqinsert{$recs[0]} = $insname;
	    $seqinsert{$recs[1]} = $insname;

	    $insname++;
	} # while <MATE>
	close(MATE);

	# now we have to go through all the sequences and assign them to
	# inserts
	while (my ($nm, $sid) = each %seqids){
	    for (my $r = 0; $r <= $#pairregexp; $r++){
		my ($freg, $revreg) = split(' ', $pairregexp[$r]);
		$base->logLocal("trying $freg and $revreg on $nm\n", 2);
		if ($nm =~ /$freg/){
		    $base->logLocal("got forw $1\n", 2);
		    if (! exists $forw{$1}){
			$forw{$1} = $nm;
			$seqinsert{$nm} = $1;
		    }
		    last;
		}
		if ($nm =~ /$revreg/){
		    $base->logLocal("got rev $1\n", 2);
		    if (! exists $rev{$1}){
			$rev{$1} = $nm;
			$seqinsert{$nm} = $1;
		    }
		    last;
		}
	    } # for each pairreg
	} # while each %seqids

	while (my ($ins, $nm) = each %forw) {
	    if (! exists $seenlib{$ins}){
		my $found = 0;
		
		for (my $l = 0; $l <= $#libregexp; $l++){
		    $base->logLocal("Trying $libregexp[$l] on $nm\n", 2);
		    if ($nm =~ /$libregexp[$l]/){
			$base->logLocal("found $libids[$l]\n", 2);
			$insertlib{$libids[$l]} .= "$ins ";
			$seenlib{$ins} = $libids[$l];
			$found = 1;
			last;
		    }
		}
		if ($found == 0){
		    $base->logError("Cannot find library for \"$nm\"");
		    next;
		}
	    }
	}
    } # elsif
} # if ! matesDone

if (defined $scffile){
    open(IN, $scffile) || 
	$base->bail("Cannot open \"$scffile\" : $!");

    parseSCFile(\*IN);
    
    close(IN);

    open(SCF, ">$outFile.links") || $base->bail("Cannot open $outFile.links: $!\n");

    for (my $s = 0; $s <= $#scaffolds; $s++){
	my $sid = $scaffolds[$s];
	my $sctgs = $scaffctgs[$s];
	my $slen = $scaffbases[$s];

	my $coords = $scaffCtg[$s];
	my %starts; my %ends;
	my @scafctgs;
	while (my ($cid, $ccrds) = each %{$coords}){
	    my ($l, $r, $ori) = split(' ', $ccrds);
	    $starts{$cid} = $l;
	    $ends{$cid} = $r;
	    push(@scafctgs, $cid);
	    if ($ori eq "<"){ # need to reverse the contig
		$contigcons{$cid} = reverseComplement($contigcons{$cid});
		my @seqs = split(' ', $contigseq{$cid});
		for (my $seq = 0; $seq <= $#seqs; $seq++){
		    my ($al, $ar, $sl, $sr) = split(' ', $seq_range{$seqs[$seq]});
		    my $nal = $contigs{$cid} - $ar;
		    my $nar = $contigs{$cid} - $al;
		    my $nsl = $sr;
		    my $nsr = $sl;
		    $seq_range{$seqs[$seq]} = "$nal $nar $nsl $nsr";
		}
	    }
	}

	@scafctgs = sort {$starts{$a} <=> $starts{$b}} @scafctgs;
	my $last = 0;
	for (my $sc = 0; $sc <= $#scafctgs; $sc++){
	    print SCF 
		sprintf("%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\n", 
			$sid, $slen, $sctgs, $sc + 1, $scafctgs[$sc], 
			$contigs{$scafctgs[$sc]}, 
			$starts{$scafctgs[$sc]} - $last, 
			(($sc != $#scafctgs) ? ($starts{$scafctgs[$sc + 1]} - $ends{$scafctgs[$sc]}) : ($slen - $ends{$scafctgs[$sc]})));
	    $last = $ends{$scafctgs[$sc]};
	}

    }
    
    close(SCF);
}

open(XML, ">$outFile.reads.xml") || $base->bail("Cannot open \"$outFile.xml\": $!\n");

print XML "<?xml version=\"1.0\"?>\n";
print XML "<trace_volume>\n";

while (my ($nm, $sid) = each %seqids){
    my $ins = $seqinsert{$nm};
    my $lib = $seenlib{$ins};
    my ($min, $max) = split(' ', $libraries{$lib});
    my $mean = ($min + $max) / 2;
    my $stdev = ($max - $min) / 6;
    $stdev = sprintf("%d", $stdev);
    my $end;
    if ($nm eq $forw{$ins}) {
	$end = "F";
    } elsif ($nm eq $rev{$ins}){
        $end = "R";
    } else {
	$end = "C";
    }

    
    print XML "  <trace>\n";
    print XML "    <trace_name>$nm</trace_name>\n";
    print XML "    <template_id>$ins</template_id>\n";
    print XML "    <trace_end>$end</trace_end>\n";
    print XML "    <library_id>$lib</library_id>\n";
    print XML "    <insert_size>$mean</insert_size>\n";
    print XML "    <insert_stdev>$stdev</insert_stdev>\n";
    print XML "    <center_name>TIGR</center_name>\n";
    print XML "    <type>paired_production</type>\n";
    print XML "  </trace>\n";
}
print XML "</trace_volume>\n";

close(XML);

# now inserts should be defined we just pick those that connect distinct 
# contigs

open(RDS, ">$outFile.bases") || $base->bail("Cannot open \"$outFile.bases\": $!\n");

open(OUT, ">$outFile.reads") || $base->bail("Cannot open \"$outFile.reads\": $!\n");

while (my ($ctg, $len) = each %contigs){
    my @seqs = split(' ', $contigseq{$ctg});

    print RDS ">contig_$ctg\n";
    for (my $ll = 0; $ll < length($contigcons{$ctg}); $ll += 80){
	print RDS substr($contigcons{$ctg}, $ll, 80), "\n";
    }

    for (my $seq = 0; $seq <= $#seqs; $seq++){
	if (! exists $seqids{$seqnames{$seqs[$seq]}}){
	    $base->logError("Cannot find $seqnames{$seqs[$seq]}", 1);
	    next;
	}
	my ($al, $ar, $sl, $sr) = split(' ', $seq_range{$seqs[$seq]});
	my $ori = ($sl < $sr) ? "+" : "-";

	if ($ori eq "-"){
	    my $tmp = $sl;
	    $sl = $sr;
	    $sr = $tmp;
	}

	if (! exists $seqinsert{$seqnames{$seqs[$seq]}}){
	    $base->logError("Cannot find insert for $seqs[$seq]");
	    next;
	}
	my $ins = $seqinsert{$seqnames{$seqs[$seq]}};
	my $obs = $obssize{$ins};
	my $mate;

	if ($seqnames{$seqs[$seq]} eq $forw{$ins}) {
	    $mate = $rev{$ins};
	} elsif ($seqnames{$seqs[$seq]} eq $rev{$ins}){
	    $mate = $forw{$ins};
	} else {
	    $base->logError("$seqnames{$seqs[$seq]} is not in $ins\n");
	}

	if (defined $mate){
	    my $mateC = $seqcontig{$seqids{$mate}};
	    my ($mal, $mar, $msl, $msr) = split(' ', $seq_range{$seqids{$mate}});

	    if ($mateC == $ctg){
		my $low; my $high;
		if ($mal < $al){
		    $low = $mal;
		} else {
		    $low = $al;
		} 
		if ($mar > $ar){
		    $high = $mar;
		} else {
		    $high = $ar;
		}

		$obs = $high - $low;
	    }
	}

	my $lib = $seenlib{$ins};
	my ($min, $max) = split(' ', $libraries{$lib});
	my $mean = ($min + $max) / 2;
	my $stdev = ($max - $min) / 6;
	
	$base->logLocal("Insert: $ins library: $lib min: $min max: $max\n",0);

	if (! defined $mate){
	    print OUT 
		sprintf("%s\t\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%s\t\t\t\t\t\t\t\n", 
			$seqnames{$seqs[$seq]}, $sr, $sl, $sr - $sl, $ctg, $len, 
			$al, $ar, $ori);
	} else {
	    print OUT 
		sprintf("%s\t\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%s\t%s\t\t%d\t%d\t%d\t%d\t\n", 
			$seqnames{$seqs[$seq]}, $sr, $sl, $sr - $sl, $ctg, $len, 
			$al, $ar, $ori, $mate, $seqcontig{$seqids{$mate}}, 
			$obs, $mean, $stdev);
	}
    }
}


close(OUT);
close(RDS);
#I'm done, wshew

exit(0);
###############################################################

sub parseSCFile {
    my $IN = shift;

    while (<$IN>){
	if (/^Scaffold scaff_(\d+) (\d+) contigs \d+ bases (\d+) span/){
	    my $scaffId = $1;
	    push(@scaffolds, $scaffId);
	    push(@scaffctgs, $2);
	    push(@scaffbases, $3);
	    $#scaffCtg++;

	    $base->logLocal("Got $1 - $2 - $3\n", 1);
	}
	if (/contig_(\S+) \((\d+), (\d+)\) \S*(<|>)\S* .* \S*(<|>)\S* contig_(\S+) \((\d+), (\d+)\)/){
	    ${$scaffCtg[$#scaffCtg]}{$1} = "$2 $3 $4";
	    ${$scaffCtg[$#scaffCtg]}{$6} = "$7 $8 $5";
	    $base->logLocal("Got $1 $2 $3 $4 and $6 $7 $8 $5\n", 1);
	}
	if (/---/){
	    $_ =~ /\s+(\S+) .*\.\.\.\s+(\d+)/;
	    my $ins = $seqinsert{$1};
#	    print "insert for $1 is $ins with $2\n";
	    if (defined $ins){
		$obssize{$ins} = $2;
	    }
	}
	if (/UNUSED LINKS/){
	    last;
	}
    }
}


# PARSING FUNCTIONS
#
# Each function parses either a file or a database table and
# fills in the following hashes:
# 
# %contigs - contig_ids and sizes
# %seqids - seq_name to seq_id
# %seq_range - seq_id to seq_range and asm_range as blank delimited string
# %seqcontig - seq_id to contig
sub parseAsmFile {
    my $IN = shift;

    my $ctg; 
    my $len;
    my $sname;
    my $alend;
    my $arend;
    my $slend;
    my $srend;
    my $sid;
    while (<$IN>){
	if (/^sequence\s+(\w+)/){
	    $len = length($1);
	    next;
	}
	if (/^asmbl_id\s+(\w+)/){
	    $ctg = $1;
	    $contigs{$ctg} = $len;  # here we assume that length 
                                    # was already computed
	    next;
	}
	if (/^seq_name\s+(\S+)/){
	    $sname = $1;
	    $sid = $minSeqId++;
	    $seqcontig{$sid} = $ctg;
	    $contigseq{$ctg} .= "$sid ";
	    $seqids{$sname} = $sid;
	    $seqnames{$sid} = $sname;
	    next;
	}
	if (/^asm_lend\s+(\d+)/){
	    $alend = $1;
	    next;
	}
	if (/^asm_rend\s+(\d+)/){
	    $arend = $1;
	    next;
	}
	if (/^seq_lend\s+(\d+)/){
	    $slend = $1;
	    next;
	}
	if (/^seq_rend\s+(\d+)/){
	    $srend = $1;
	    next;
	}
	if (/^offset/){
	    $seq_range{$sid} = "$alend $arend $slend $srend";
	    next;
	}
    }
}


sub parseACEFile {
    my $IN = shift;
    
    my $ctg; 
    my $len;
    my $sname;
    my $alend;
    my $arend;
    my $slend;
    my $srend;
    my $sid;
    my $incontig = 0;
    my $consensus = "";
    while (<$IN>){
	if (/^CO (\S+) (\d+) (\d+)/){
	    $ctg = $1;
	    $contigs{$ctg} = $2;
	    $incontig = 1;
	    $consensus = "";
#	    %offset = ();
	    next;
	}
	
	if (/^AF (\S+) (\w) (-?\d+)/){

	}
	
	if (/^\#(\S+)\(\d+\) .*\{(\d+) (\d+)\} <(\d+) (\d+)>/){
	    $incontig = 0;
	    $sname = $1;
	    $sid = $minSeqId++;
	    $seqcontig{$sid} = $ctg;
	    $contigseq{$ctg} .= "$sid ";
#	    print STDERR "adding $sname to $ctg\n";
	    $seqids{$sname} = $sid;
	    $seqnames{$sid} = $sname;
	    $alend = $4;
	    $arend = $5;
	    $slend = $2;
	    $srend = $3;
	    $seq_range{$sid} = "$alend $arend $slend $srend";
	    next;
	}
	
	if ($incontig && /^\s*$/){
	    # here I try to get rid of dashes when computing contig sizes
	    my $ind = -1;
	    $incontig = 0;
	    
	    while (($ind = index($consensus ,"*", $ind + 1)) != -1){
		$contigs{$ctg}--;
	    }
	    $consensus =~ s/\*//g ;
	    $contigcons{$ctg} = $consensus;
	    next;
	}

	if ($incontig){
	    chomp;
	    $consensus .= $_;
	    next;
	}
	
    }
}



# parser for ace files
sub parseContigFile {
    my $IN = shift;

    my $ctg; 
    my $len;
    my $sname;
    my $alend;
    my $arend;
    my $slend;
    my $srend;
    my $sid;
    my $incontig = 0;
    my $consensus = "";
    while (<$IN>){
	if (/^\#\#(\S+) \d+ (\d+)/ ){
	    if (defined $consensus){
		$consensus =~ s/-//g;
		$contigcons{$ctg} = $consensus;
	    }
	    $consensus = "";
	    $ctg = $1;
	    $contigs{$ctg} = $2;
	    $incontig = 1;
	    next;
	}

	if (/^\#(\S+)\(\d+\) .*\{(\d+) (\d+)\} <(\d+) (\d+)>/){
	    $incontig = 0;
	    $sname = $1;
	    $sid = $minSeqId++;
	    $seqcontig{$sid} = $ctg;
	    $contigseq{$ctg} .= "$sid ";
#	    print STDERR "adding $sname to $ctg\n";
	    $seqids{$sname} = $sid;
	    $seqnames{$sid} = $sname;
	    $alend = $4;
	    $arend = $5;
	    $slend = $2;
	    $srend = $3;
	    $seq_range{$sid} = "$alend $arend $slend $srend";
	    next;
	}

	if ($incontig){
	    # here I try to get rid of dashes when computing contig sizes
	    my $ind = -1;
	    while (($ind = index($_ ,"-", $ind + 1)) != -1){
		$contigs{$ctg}--;
	    }
	    chomp;
	    $consensus .= $_;
	}
    }
    if (defined $consensus){
	$consensus =~ s/-//g;
	$contigcons{$ctg} = $consensus;
    }

}


# TIGR DB INTERACTION

# get_lib_info retrieves information from the database for a particular
# lib_id and writes it into the %libraries array.
# assumes $dbproc is already defined
sub get_lib_info
{
    my $lib = shift;

    my $query = 
qq~
select min_clone_len, max_clone_len, cat#
from track..library
where lib_id = "$lib"
~;

    my $rh = $dbproc->prepare($query) || 
	$base->bail("Cannot prepare query: \"$query\"");

    $rh->execute() || $base->bail("query \"$query\" failed");

    while (my $lineref = $rh->fetchrow_arrayref){
	$libraries{$lib} = "$$lineref[0] $$lineref[1]";
	$libMap{$lib} = $$lineref[2];
    }

    $rh->finish;
}

sub reverseComplement {
    my($string) = @_;

    $string = reverse($string);
    $string =~ tr/AaCcTtGgUuMmRrWwSsYyKkVvHhDdBbXxNn.-/TtGgAaCcAaKkYyWwSsRrMmBbD
dHhVvXxNn.-/;
    return $string;
} # reverseComplement
