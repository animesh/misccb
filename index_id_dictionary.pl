#! /util/bin/perl

# Useage: index_id_dictionary.pl  id_file_1  id_file_2  [project3 ...]";
#


if ($#ARGV != 1)
{
    print "Useage: $0  id_file_1  id_file_2\n";
    print "   provides translation of read indices and read IDs.\n";
    exit(0);;
}

$idfile1 = $ARGV[0];
$idfile2 = $ARGV[1];

######

%indices1 = ();
@ids1 = ();

open(IDFILE, $idfile1) || die "Can't open $idfile1\n";
$ids1_length = <IDFILE>;  # the first line is the number of reads
chomp $ids1_length;

$i = 0;
while (<IDFILE>)
{
    chomp;
    push @ids1, $_;
    $indices1{$_} = $i;
    $i++;
}

close(IDFILE);

if ($ids1_length != scalar(@ids1))
{
    print "The declared number $ids1_legnth of reads in $idfile1 is different\n";
    print "from the actual number ", scalar(@ids1), " found.\n";
    exit(-1);
}

$idfile1 =~ m|/([^/]+)/([^/]+)/work/reads.ids$|;
$proj1_name = $1;

######

%indices2 = ();
@ids2 = ();

open(IDFILE, $idfile2) || die "Can't open $idfile2\n";
$ids2_length = <IDFILE>;  # the first line is the number of reads

$i = 0;
while (<IDFILE>)
{
    chomp;
    push @ids2, $_;
    $indices2{$_} = $i;
    $i++;
}

close(IDFILE);

if ($ids2_length != scalar(@ids2))
{
    print "The declared number $ids2_legnth of reads in $idfile2 is different\n";
    print "from the actual number ", scalar(@ids2), " found.\n";
    exit(-1);
}

$idfile2 =~ m|/([^/]+)/([^/]+)/work/reads.ids$|;
$proj2_name = $1;

######

$prompt = "Enter read ID, or  <index2, or  index1>: ";
print $prompt;

while (<STDIN>)
{
    s/\s+//g;
    $input = $_;
    chomp $input;

    $id = "";
    $index1 = "";
    $index2 = "";

    if ($input =~ /^<(\d+)$/)
    {
	# translate a read index from project2
	# into a read ID and the read index in project1

	$index2 = $1;
	$id = $ids2[$1];
	$index1 = $indices1{$id};
    }
    elsif ($input =~ /^(\d+)>$/)
    {
	# translate a read index from project1
	# into a read ID and the read index in project2

	$index1 = $1;
	$id = $ids1[$1];
	$index2 = $indices2{$id};
    }
    else
    {
	# translate a read ID into read indices in the two projects

	$id = $input;
	$index1 = $indices1{$id};
	$index2 = $indices2{$id};
    }

    print "$id\n$proj1_name: $index1\n$proj2_name: $index2\n\n";
    print $prompt;
}

