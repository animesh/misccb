#! /util/bin/perl

# Useage: make_shortlist.pl wanted_file
#


if ($#ARGV < 1)
{
    print "Useage: $0  reads.ids_file  wanted.1  [wanted.2 ...]\n";
    print "   for each  wanted.i  file, writes to the file  wanted_names.i  all the\n";
    print "   read IDs  whose indices appear in  wanted.i\n";
    print "(NB: this should be run only after running  remove_discarded.pl)\n";
    exit(0);
}

@wanted_files = @ARGV;
$idfile = shift(@wanted_files);

open(IDFILE, $idfile);
@ids = <IDFILE>;
close(IDFILE);

$shown_length = shift(@ids);
chomp($shown_length);
$actual_size = @ids;
print "$idfile says it has $shown_length many entries\n";
print "and the actual number of entries found is $actual_size\n";


while (@wanted_files > 0)
{
    $wanted_file = shift(@wanted_files);
    $wanted_file =~ /.+\.(\d+)/;
    $out_file = "wanted_names.$1";

    print "Doing $wanted_file --> $out_file ...";

    @reads = ();

    open(WANTED_FILE, $wanted_file) || die "Can't open $wanted_file\n";
    @reads = <WANTED_FILE>;
    close(WANTED_FILE);

    open(OUT_FILE, "> $out_file") || die "Can't open $out_file for output\n";
    foreach (@reads)
    {
	/QUERY\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)/;

	$first_index = $1;
#	$first_start = $2;
#	$first_end = $3;
#	$first_length = $4;
#	$first_rc = $5;
#	$first_start_coord = $7;
#	$first_end_coord = $8;

	$first_id = $ids[$first_index];
	chomp($first_id);

	print OUT_FILE "$first_id\n";
    } # outer loop

    close(OUT_FILE);

    print "done.\n";
}

