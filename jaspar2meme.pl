#!@WHICHPERL@ -w

#**********************************************************************/
#* Copyright (c) University of Washington,                            */
#* Department of Genome Sciences, 2006. Written by Shobhit Gupta      */
#* and Timothy L. Bailey     					      */
#* All rights reserved.                                               */
#**********************************************************************/

#defaults:
my $pseudo_total = 1; 		# default total pseudocounts
my $alen = 4;
my @letters = ('A', 'C', 'G', 'T');
my %bg = ( 'A' => .25, 'C' => .25, 'G' => .25, 'T' => .25,);
my $ext = "sites";
my $STRANDS = 2;
my $use_numbers = 0;

my $usage = "USAGE: jaspar2meme [options] <Jaspar directory>

  Options: 
	   -pfm		    read JASPAR count files (.pfm); 
	   -cm		    read count file with line labels 'A|' etc. (.cm); 
			    default: site files (.sites)
           -numbers         use numbers instead of strings as motif IDs
           -strands [1|2]   print '+ -' '+' on the MEME strand line;
                            default: 2 (prints '+ -')
           -bg <bfile>      file with background frequencies in MEME
                            -bfile format; default: uniform frequencies
           -pseudo <A>      add <A> times background frequency to
                            each count when computing letter frequencies
                            default: $pseudo_total


  Convert a directory of JASPAR files into a MEME version 3 formatted 
  file suitable for use with MAST and other MEME Suite programs.

  A JASPAR '.sites' file describes a motif in terms of a multiple
  alignment of sites.  It contains a multiple alignment in modified 
  FASTA format.  Only capitalized sequence letters are part of the alignment.

  A JASPAR count file ('.pfm') contains a count matrix where the rows
  correspond to A, C, G and T, respectively.  

  A CM count file ('.cm') prefixes the rows with 'A| ', 'C| ', 'G| ' and 'T| '.

  A log-odds matrix and a probability matrix are output for each
  motif ('.sites') file.  The probability matrix is computed using
  pseudo-counts consisting of the background frequency (see -bg, above)
  multiplied by the total pseudocounts (see -pseudo, above).
  The log-odds matrix uses the background frequencies in the denominator
  and is log base 2.

  
  Writes standard output.
\n";

if (scalar(@ARGV) == 0) {
  printf(STDERR $usage);
  exit(1);
}

while($#ARGV > 0) {
    $arg = shift;
    if ($arg eq "-pfm") {
      $ext = "pfm";
    } elsif ($arg eq "-cm") {
      $ext = "cm";
    } elsif ($arg eq "-numbers") {
      $use_numbers = 1;
    } elsif ($arg eq "-strands") {
        $STRANDS = shift @ARGV;
    } elsif ($arg eq "-bg") {
      $bg_file = shift @ARGV;
    } elsif ($arg eq "-pseudo") {
      $pseudo_total = shift @ARGV;
    } else {
    print(STDERR "Illegal argument ($arg)\n");
    exit(1);
  }
}

while (scalar(@ARGV) > 0) {
    $JAS_DIR = shift(@ARGV);
}
$JAS_DIR =~ s/\/$//;

## Print usage if no argument
if ($JAS_DIR eq '') {
    print "\nJaspar directory missing\n";
    print "\nUsage:\n\n";
    print "jaspar2meme <Jaspar directory containing .sites files>\n\n";
    print "Meme version 3 format output file is written to stdout\n\n";
    exit (0);
}

if ($STRANDS == 2) {
    $strand_string = " \+ \-"
} elsif ($STRANDS == 1){
    $strand_string = " \+"
} else {
    print "value of \-strands not valid: should be 1 or 2 default is 2\n";
    exit (2);
}

#
# get the directory listing
#
my @files = `ls -1 $JAS_DIR`;

# read in the background frequencies
read_bg_file($bg_file, \%bg) if ($bg_file);

# 
# set up file header
#
my $file_header = "MEME version 3.0\n\nALPHABET\= ACGT\n\nstrands\: $strand_string\n\nBackground letter frequencies \(from dataset with add-one prior applied\)\:\nA $bg{A} C $bg{C} G $bg{G} T $bg{T}\n\n";

my $id = 0;				# motif ID number
foreach my $file (@files) {

    next unless ($file =~ m/(\S*)\.$ext/);

    my $jaspar_id = $1;
    chomp $file;
    open(IN, "<$JAS_DIR/$file") || die "Can't open file $JAS_DIR/$file\n";

    my $motif_length;
    my %counts = ();
    my $n_sites = 0;
    my $header = "";

    if ($ext eq "pfm" || $ext eq "cm") {	# read a counts file (.pfm, .cm)
       my $letter_index = -1;
       while (<IN>) {
            next if (/^#/ || /^\s*$/); 		# skip comment, blank lines
            my @words = split;
            shift @words if ($ext eq "cm");	# discard line labels
            $motif_length = @words;
	    $letter_index++;
            my $curr_letter = $letters[$letter_index];
            for (my $i=0; $i<$motif_length; $i++) {
              $counts{$i}{$curr_letter} = $words[$i]; 
            }
        }
    } elsif ($ext eq "sites") { 		# read a sites file
	my @seq = ();
	while (my $l = <IN>) {
	    chomp $l;
	    $l =~ s/\r//g;
	    if ($l =~ m/^\>/) {
		$header = $l;
		$seq[$n_sites] = <IN>;
		chomp $seq[$n_sites];
		$seq[$n_sites] =~s /\r//g;
		$seq[$n_sites] =~ s/[a-z]//g;
		$n_sites ++;
	    }
	} # End of file

	# convert sites to counts
	$motif_length = length $seq[0];
	for (my $query_child_index = 0; 
	     $query_child_index < $n_sites;
	     $query_child_index++) {
	    my $length_index;
	    for ($length_index = 0; 
		 $length_index < $motif_length;
		 $length_index++) {
		my $curr_letter = substr($seq[$query_child_index],
					 $length_index,1);
		$counts{$length_index}{$curr_letter}++;
	    }
	}
    } # read a file 

    #
    # print the MEME formatted output
    #
    print $file_header if ($id++ == 0);		# bump ID number
    $header =~ s/(\d+)$//;
    $header =~ s/^([^\s]*\s)//;
    if ($use_numbers) {
	print "MOTIF $id $header\($jaspar_id\)\n\n";
	print "BL   MOTIF $id width\=$motif_length seqs\=$n_sites\n";
    } else {
	print "MOTIF $jaspar_id $header\n\n";
	print "BL   MOTIF $jaspar_id width\=$motif_length seqs\=$n_sites\n";
    }
    my $log_odds = "log\-odds matrix\: alength= $alen w\= $motif_length n\= 0 bayes\= 0 E\= 0\n";
    my $letter_prob = "letter\-probability matrix\: alength= $alen w= $motif_length nsites= $n_sites E= 0\n";

    #
    # convert counts to frequencies and log-odds
    #
    #foreach my $key1 (sort numerically keys %counts) {
    foreach (my $key1=0; $key1<$motif_length; $key1++) {
	my %vals = ();
	my $sum_row = 0;
	foreach my $key2 (@letters) {
	    if (exists($counts{$key1}{$key2})){
		$sum_row += $vals{$key2} = $counts{$key1}{$key2} + ($pseudo_total * $bg{$key2});
	    } else {
		$sum_row += $vals{$key2} = $pseudo_total * $bg{$key2};
	    }
	}

	$log_odds .= " ";
	$letter_prob .= " ";
	foreach my $key (@letters) {
	    my $val1 = $vals{$key}/$sum_row;
	    #my $val2 = log($val1/$bg{$key}) / log(2.0);
	    #if ($val2 >= 0){
		#$log_odds .= " ";
	    #}
	    $val1 = sprintf ("%2.6f", $val1);
	    #$val2 = sprintf ("%2.6f", $val2); 
	    #$log_odds .= "$val2\t";
	    $letter_prob .= "$val1\t";
	}
	$letter_prob .= "\n";
	#$log_odds .= "\n";
    }
    $letter_prob .= "\n";

    #print $log_odds;
    print $letter_prob;

    close IN;
} # foreach file

#sub numerically { $a <=> $b; }

#
# Read a background file of frequencies in MEME -bfile format
# Input: 
#	$bg_file 	name of file
#	$bg_ptr		ptr to hash to receive frequencies
sub read_bg_file {
  my($bg_file, $bg_ptr) = @_;
  open($bg_file, "<$bg_file") || die("Can't open $bg_file.\n");
  my $total_bg = 0;
  while (<$bg_file>) {
    next if (/^#/);                     # skip comments
    my($a, $f) = split;
    if ($a eq "A" || $a eq "a") {
      $$bg_ptr{"A"} = $f;
      $total_bg += $f;
    } elsif ($a eq "C" || $a eq "c") {
      $$bg_ptr{"C"} = $f;
      $total_bg += $f;
    } elsif ($a eq "G" || $a eq "g") {
      $$bg_ptr{"G"} = $f;
      $total_bg += $f;
    } elsif ($a eq "T" || $a eq "t") {
      $$bg_ptr{"T"} = $f;
      $total_bg += $f;
    }
  }
  # make sure they sum to 1
  foreach my $key (keys %$bg_ptr) {
    $$bg_ptr{$key} /= $total_bg;
    #printf STDERR "$key $bg{$key}\n";
  }
} # read_bg_file
