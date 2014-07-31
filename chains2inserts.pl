#! /usr/bin/perl

if ($#ARGV < 0)
{
    print "Useage: $0  file1.chains  [file2.chains ...]\n";
    print "  Converts a sorted chains file into a file of insert lengths.\n";
    print "  The read-ids are assumed to be those generated by\n";
    print "  MakeRealInserts.cc  or  MakeFakeInserts.cc\n";
    exit(0);
}

# the mean fosmid insert length;
# this should be constant so long as
# the fosmid library construction technology does not change. 
$mean_length = 39400;   # not used except for experimental purposes

####################
# Iterate through the given list of files

$num_files = $#ARGV + 1;

for ($i = 0; $i < $num_files; $i++)
{
    $sorted_chains_file = $ARGV[$i];
    $sorted_chains_file =~ /(.+)\.chains/;
    $inserts_file = "$1.inserts";

    ####################
    # Load the chains file and parse it
    # into the data about the alignments

    $now_time = localtime;
    print "$now_time: Reading $sorted_chains_file...\n";

    $num_aligns = 0;
    @align_score = ();
    @align_chromosome = ();
    @align_start_coord = ();
    @align_stop_coord = ();
    @align_read_num = ();
    @align_read_AB = ();
    @align_orientation = ();

    open(SORTED_CHAINS_FILE, $sorted_chains_file) ||
	die "Can't open $sorted_chains_file\n";

    while ($line = <SORTED_CHAINS_FILE>)
    {
#    if ($line =~ /^chain \d+ (.+)_chr(.+)_\d+_\d+ \d+ . \d+ \d+ r(\d+)(A|B) \d+ . \d+ \d+/)
	if ( $line =~ /^chain (\d+) chr(.+)\.(\d+)-(\d+) (\d+) (.) (\d+) (\d+) (.+) (\d+) (.) (\d+) (\d+) /)
	{
	    $score = $1;

	    $chromosome = $2;
	    $chr_min = $3;
	    $chr_max = $4;
	    $chr_length = $5;
	    $chr_orien = $6;
	    $chr_start = $7;
	    $chr_stop = $8;

	    $read_id = $9;
	    $read_length = $10;
	    $read_orien = $11;
	    $read_start = $12;
	    $read_stop = $13;

	    if ($chr_orien eq "-")
	    {
		print "REVERSED: $line";
	    }

	    $read_id =~ /r(\d+)(A|B)/;
	    $read_num = $1;
	    $read_AB = $2;

	    push @align_score, $score;
	    push @align_chromosome, $chromosome;
	    push @align_start_coord, $chr_min+$chr_start;
	    push @align_stop_coord, $chr_min+$chr_stop;
	    push @align_read_num, $read_num;
	    push @align_read_AB, $read_AB;

	    if ( ($chr_orien eq "+" && $read_orien eq "+") ||
		 ($chr_orien eq "-" && $read_orien eq "-") )
	    {
		push @align_orientation, "+";
	    }
	    else
	    {
		push @align_orientation, "-";
	    }

	    $num_aligns++;

	}
	else
	{
	    print "UNMATCHED: $line";
	}

    } # while ($line = <SORTED_CHAINS_FILE>)

    close(SORTED_CHAINS_FILE);

    $now_time = localtime;
    print "$now_time: Reading $sorted_chains_file...done.\n";
    
    print "$num_aligns many alignments\n";

    if ($num_aligns == 0)
    {
	die "No alignments found in $sorted_chains_file\n";
    }

    ####################
    # Screen the alignments of pairs of reads
    # and select those for which both reads align
    # to the same chromosome and in the correct orientation

    $now_time = localtime;
    print "$now_time: Placing inserts...\n";

    @placement_nums = ();
    @placement_lengths = ();
    @placement_scores = ();

    $num_placements = 0;

    # array index for a block of consecutive entries with the same read_num
    $blk_index = 0;
    
    while ($blk_index < $num_aligns)
    {
	$curr_blk_index = $blk_index;
	$curr_placement_num = $align_read_num[$blk_index];
	
	while ($blk_index < $num_aligns &&
	       $align_read_num[$blk_index] == $curr_placement_num)
	{
	    $blk_index++;
	}

	# now the indices in the interval  [$curr_blk_index, $blk_index)
	# all have the same  $align_read_num[..] value, equal to $curr_placement_num
	
	$blkA_start = $curr_blk_index;
	$blkA_stop = $curr_blk_index;
	while ($blkA_stop < $blk_index &&
	       $align_read_AB[$blkA_stop] eq "A")
	{
	    $blkA_stop++;
	}

	if ($blkA_start == $blkA_stop)
	{
	    # there are no A-reads in this block
	    next;
	}

	$blkB_start = $blkA_stop;
	$blkB_stop = $blk_index;

	if ($blkB_start == $blkB_stop)
	{
	    # there are no B-reads in this block
	    next;
	}
	
	# now the indices in the interval  [$blkA_start, $blkA_stop)
	# all have the same  $align_read_AB[..] value, equal to "A",
	# while the indices in the interval  [$blkB_start, $blkB_stop)
	# all have the same  $align_read_AB[..] value, equal to "B"

	for ($a = $blkA_start; $a < $blkA_stop; $a++)
	{
	    for ($b = $blkB_start; $b < $blkB_stop; $b++)
	    {

		if ($align_chromosome[$a] != $align_chromosome[$b])
		{
		    # the reads lie on different chromosomes
		    next;
		}
		$chromosome = $align_chromosome[$a];

		if ($align_orientation[$a] != $align_orientation[$b])
		{
		    # the reads are oriented differently
		    next;
		}
		$orientation = $align_orientation[$a];


		if ($orientation eq "+")
		{
		    $start_coord = $align_start_coord[$a];
		    $stop_coord = $align_stop_coord[$b];

		    $start_score = $align_score[$a];
		    $stop_score = $align_score[$b];
		}
		else
		{
		    $start_coord = $align_start_coord[$b];
		    $stop_coord = $align_stop_coord[$a];

		    $start_score = $align_score[$b];
		    $stop_score = $align_score[$a];
		}

		# NOTE: Blastz somehow returns coordinates such that
		# the resulting insert lengths are 4 bases short;
		# so we compensate for that by adding 4 here.
		push @placement_lengths, $stop_coord - $start_coord + 4;
		push @placement_nums, $curr_placement_num;
		push @placement_scores, $start_score + $stop_score;

		$num_placements++;

	    } # for ($b = $blkB_start; $b < $blkB_stop; $b++)

	} # for ($a = $blkA_start; $a < $blkA_stop; $a++)

    } # while ($blk_index < $num_aligns)


    $now_time = localtime;
    print "$now_time: Placing inserts...done.\n";

    print "$num_placements many placements found in total\n";

    if ($num_placements == 0)
    {
	die "No placement found\n";
    }

    ####################
    # Filter the placements of read-pairs generated above
    # and pick those which have good scores and reasonable insert lengths
    # and write them to the output file.

    $now_time = localtime;
    print "$now_time: Writing to $inserts_file...\n";

    open(INSERTS_FILE, "> $inserts_file") ||
	die "Can't open $inserts_file for output\n";

    $num_ok_inserts = 0;

    $index = 0;
    while ($index < $num_placements)
    {
	$curr_index = $index;
	$num = $placement_nums[$index];

	while ($index < $num_placements &&
	       $placement_nums[$index] eq $num)
	{
	    $index++;
	}

	# now the indices in the interval  [$curr_index, $index)
	# all have the same  $placement_nums[..] value, equal to $num

	$best_line = "";
	$best_score = 0;
	for ($i = $curr_index; $i < $index; $i++)
	{
	    if ($placement_scores[$i] > $best_score &&
		$placement_lengths[$i] > 0 &&
		$placement_lengths[$i] < 200000)
		# $placement_lengths[$i] > $mean_length/2 &&
		# $placement_lengths[$i] < 2*$mean_length)
		# $placement_lengths[$i] > $mean_length - 2.5 * 1000 &&
		# $placement_lengths[$i] < $mean_length + 2.5 * 1000)
	    {
		$best_score = $placement_scores[$i];
		$best_line = sprintf "%6s %6s\n",
		( $placement_nums[$i], $placement_lengths[$i] );
	    }
	}

	if ($best_score > 1)
	{
	    print INSERTS_FILE $best_line;
	    $num_ok_inserts++;
	}
    }

    close(INSERTS_FILE);

    $now_time = localtime;
    print "$now_time: Writing to $inserts_file...done.\n";

    print "$num_ok_inserts many inserts accepted\n";

} # for ($i = 0; $i < $num_files; $i++)
