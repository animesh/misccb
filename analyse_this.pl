#! /usr/bin/perl

if ($#ARGV != 2)
{
    print "Useage: $0  lengths_file  chains_file  inserts_file\n";
    print "  Analyze the rate of alignment of reads in inserts.\n";
    exit(0);
}

$lengths_file = $ARGV[0];
$chains_file  = $ARGV[1];
$inserts_file = $ARGV[2];

####################
# Load the lengths file and count the number of given read-pairs

%zero_ended = ();

$now_time = localtime;
print "$now_time: Reading $lengths_file...\n";

open(LENGTHS_FILE, $lengths_file) ||
    die "Can't open $lengths_file\n";

$index = 0;

while (<LENGTHS_FILE>)
{
    $read = sprintf "%06d", $index;
    $zero_ended{$read} = 1;

    $index++;
}

close(LENGTHS_FILE);

$num_given_readpairs = $index;

$now_time = localtime;
print "$now_time: Reading $lengths_file...done.\n";


####################
# Load the chains file and parse it
# into an array of reads that align

$now_time = localtime;
print "$now_time: Reading $chains_file...\n";

%aligns = ();

open(CHAINS_FILE, $chains_file) ||
    die "Can't open $chains_file\n";


while ($line = <CHAINS_FILE>)
{
#    if ($line =~ /^chain \d+ (.+)_chr(.+)_\d+_\d+ \d+ . \d+ \d+ r(\d+)(A|B) \d+ . \d+ \d+/)
    if ($line =~ /^chain \d+ chr(.+)\.\d+-\d+ \d+ . \d+ \d+ r(\d+)(A|B) \d+ . \d+ \d+/)
    {
	$read = sprintf "r%06d%s", ($2,$3);
	$aligns{$read} = 1;

	$read = sprintf "%06d", $2;
	delete $zero_ended{$read};

    }
    else
    {
	print "UNMATCHED: $line";
    }

} # while ($line = <CHAINS_FILE>)

close(CHAINS_FILE);

$num_alignedreads = scalar(keys %aligns);

$now_time = localtime;
print "$now_time: Reading $chains_file...done.\n";



####################
# Load the inserts file and parse it

$now_time = localtime;
print "$now_time: Reading $inserts_file...\n";

open(INSERTS_FILE, $inserts_file) ||
    die "Can't open $inserts_file\n";

%two_ended = ();

while (<INSERTS_FILE>)
{
    /^\s*(\d+)\s+/;

    $insert = sprintf "%06d", $1;
    $two_ended{$insert} = 1;

    $readA = sprintf "r%06dA", $1;
    $readB = sprintf "r%06dB", $1;

    delete $aligns{$readA};
    delete $aligns{$readB};
}


$now_time = localtime;
print "$now_time: Reading $inserts_file...done.\n";

close (INSERTS_FILE);


####################
# Screen aligned reads for two-ended discards

$num_two_ended_discarded = 0;

foreach $readA (keys(%aligns))
{
    next if ($readA !~ /r(\d+)A/);

    $readB = sprintf "r%06d%s", ($1, "B");
    if (exists($aligns{$readB}))
    {
	$num_two_ended_discarded++;
	delete $aligns{$readA};
	delete $aligns{$readB};
    }
}



####################
# Analyze the rate of one-ended and two-ended alignments

$num_zero_ended = scalar(keys %zero_ended);
$num_one_ended = scalar(keys %aligns);
$num_two_ended = scalar(keys %two_ended);

print "\n\n";
print "----------------------------------------\n";

printf "Number of given inserts = %7u\n", $num_given_readpairs;
printf "Number of given reads   = %7u\n", 2*$num_given_readpairs;
printf "Number of aligned reads = %7u\n", $num_alignedreads;
print  "\n";

$p = $num_alignedreads / (2 * $num_given_readpairs);
$q = 1-$p;

printf "Probability of read alignment = %4.1f\%\n", $p * 100;
print  "\n";

print  "                           Expected  Observed\n";
printf "0-ended insert alignment :  %4.1f%%    %4.1f%% (%6u)\n",
    ($q*$q * 100,
     $num_zero_ended / $num_given_readpairs * 100,
     $num_zero_ended);
printf "1-ended insert alignment :  %4.1f%%    %4.1f%% (%6u)\n",
    (2*$p*$q * 100,
    $num_one_ended / $num_given_readpairs * 100,
     $num_one_ended);
printf "2-ended insert alignment :  %4.1f%%    %4.1f%% (%6u)\n",
    ($p*$p * 100,
    $num_two_ended / $num_given_readpairs * 100,
     $num_two_ended);
printf "2-ended insert alignment discarded : %4.1f%% (%6u)\n",
    ($num_two_ended_discarded / $num_given_readpairs * 100,
     $num_two_ended_discarded);
print  "\n";



# $exp_one_ended = 2 * $p * $q * $num_given_readpairs;
# $std_one_ended = sqrt((2*$p*$q - 4*$p*$p*$q*$q) * $num_given_readpairs);
# 
# $dev_one_ended = ($num_one_ended - $exp_one_ended) / $std_one_ended;
# 
# $exp_two_ended = $p * $p * $num_given_readpairs;
# $std_two_ended = sqrt(($p*$p - $p*$p*$p*$p) * $num_given_readpairs);
# 
# $dev_two_ended = ($num_two_ended - $exp_two_ended) / $std_two_ended;
# 
# printf "Expected number of 1-ended insert alignments =  %6u\n", $exp_one_ended;
# printf "Standard deviation                             =  %6u\n", $std_one_ended;
# printf "Actual   number of 1-ended insert alignments =  %6u  (deviation %+6.2f)\n",
#     ($num_one_ended, $dev_one_ended);
# print "\n";
# 
# printf "Expected number of 2-ended insert alignments =  %6u\n", $exp_two_ended;
# printf "Standard deviation                             =  %6u\n", $std_two_ended;
# printf "Actual   number of 2-ended insert alignments =  %6u  (deviation %+6.2f)\n",
#     ($num_two_ended, $dev_two_ended);
# print "\n";
# 
# $pp = 2 * $num_two_ended / $num_alignedreads;
# $qq = 1 - $pp;
# 
# printf "Probability of partner alignment          = %4.1f\%\n", $pp * 100;
# print  "\n";

print "----------------------------------------\n";

