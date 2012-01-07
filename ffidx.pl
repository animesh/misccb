#!/opt/local/bin/perl
#---------------------------------------------------------------------
# Generate an index file containing the sequence identifier and
# byte-offset of each record in a flatfile which contains biological
# sequence data.
#
# Generate some companion output files (primarily of use in the
# context of building complete GenBank releases).
#
# Author:  Mark L. Cavanaugh (cavanaug@ncbi.nlm.nih.gov)
# Created: 08/??/2001
# Updated: 02/28/2003
#
#---------------------------------------------------------------------
#
# TTD : Implement support for the SWISS-PROT flatfile format.
#
# TTD : Implement a new -sv argument, to support indexes based
#       on both the primary sequence identifier *and* the 
#	sequence version number.
#
# TTD : add checks that all values expected for a flatfile
# TTD : record were in fact parsed out successfully.
#
# TTD : ditto that the values make sense, eg, sequence lengths
# TTD : are numeric values, accessions alphanumeric, etc.
#
# TTD : Implement switches allowing the user to control what
#       happens when a 'duplicate' record is encountered. Eg,
#	index the record with the greatest update-date value, or
#	with the greatest/least byte-offset.
#
# TTD : When a 'duplicate' record is encountered, only ONE can
#       be represented in the index file. But what about the
#       many other output files? Should they reflect EXACTLY
#	what's in the input flatfile? Or should they reflect only
#       the records indexed? Hmmmm......
#
#---------------------------------------------------------------------

use IO::File;
use strict;

#---------------------------------------------------------------------
# Argument processing.
#---------------------------------------------------------------------

@ARGV || &Usage;

my ($input_file) = "";		# Input flatfile containing biological
				# sequence data.

my ($arg_idx_file) = "";	# User-supplied value for the -o argument,

my ($idx_file) = "";		# Output byte-offset index file for the
				# records in $input_file, based on their
				# primary sequence identifiers.

my ($idx_fh);			# File handle for the above.

my ($idx_incl_seqnm) = 0;	# True if the mnemonic name of each sequence
				# should be included in the index file.

my ($idx_incl_seqlen) = 0;	# True if the length of each sequence should
				# be included in the index file.

my ($idx_incl_divcode) = 0;	# True if the division code for each sequence
				# record should be included in the index file.

my ($idx_incl_udate) = 0;	# True if the update-date for each sequence
				# record should be included in the index file.

my ($psid_file) = "";		# Output file : list of all the primary
				# sequence identifiers for the records
				# in $input_file.

my ($psid_fh);			# File handle for the above.

my ($p2n_file) = "";		# Output file: table of primary
				# sequence identifiers and sequence names.

my ($p2n_fh);			# File handle for the above.

my ($s2p_file) = "";		# Output file: table of secondary sequence
				# identifiers and the primary sequence identifiers
				# of the records in which they appear.

my ($s2p_fh);			# File handle for the above.

my ($sbo_file) = "";		# Output file: table tallying number of sequences
				# and number of bases/residues for each organism name
				# encountered in $input_file

my ($sbo_fh);			# File handle for the above.

my ($sbt_file) = "";		# Output file: total number of sequences and total
				# number of bases/residues for all the records in
				# $input_file

my ($sbt_fh);			# File handle for the above.

my ($sdr_file) = "";		# Output file: 'Short Directory' summary for each
				# record in $input_file, including accession, defline
				# and number of basepairs.

my ($sdr_fh);			# File handle for the above.

my ($sdr_title) = "";		# 'Title' for the Short Directory

my ($allow_dups) = 0;		# True if duplicate records are allowed
				# in the input file. Fatal error if duplicates
				# are found and this switch isn't provided.

my ($min_parse) = 0;		# True if $input_file is to be minimally parsed,
				# in order to reduce execution time (at the
				# expense of not performing some data
				# validity checks)

my ($ver_div) = "";		# Division code that all records in $input_file
				# are supposed to be in. Fatal error if any are
				# found for which this is not true.

my ($no_ddbj) = 0;		# True if DDBJ records in $input_file should
				# *not* be processed.

my ($no_embl) = 0;		# True if EMBL records in $input_file should
				# *not* be processed.

my ($no_genbank) = 0;		# True if GenBank records in $input_file should
				# *not* be processed.

my ($dbx) = 0;			# Set to 1 to generate output useful for debugging.

while ($_ = $ARGV[0],/^-/) {

    shift;

    if ($dbx) {
	print STDOUT "$0 : DBX   : curr arg is $_\n";
	print STDOUT "$0 : DBX   : next arg is $ARGV[0]\n";
    }

    last if (/^--$/);

    if (/^-i$/) {
	if ($ARGV[0] && ($ARGV[0] =~ /^[^-]/)) {
	    $input_file = shift;
	}
	else {
	    print STDERR "$0 : USAGE : no value provided for $_ argument.\n\n";

	    # Flatfile data can be read from either stdin or
	    # a file. Seems safer to exit if the user specified
	    # -f, but provided no value.

	    exit 1;
	}
	next;
    }
    if (/^-o$/) {
        if ($ARGV[0] && ($ARGV[0] =~ /^[^-]/)) {
            $arg_idx_file = shift;
        }
        else {
            undef $arg_idx_file;
        }
        next;
    }
    if (/^-psi$/) {
	if ($ARGV[0] && ($ARGV[0] =~ /^[^-]/)) {
	    $psid_file = shift;
	}
	else {
	    print STDERR "$0 : USAGE : no value provided for $_ argument.\n\n";
	}
	next;
    }
    if (/^-p2n$/) {
	if ($ARGV[0] && ($ARGV[0] =~ /^[^-]/)) {
	    $p2n_file = shift;
	}
	else {
	    print STDERR "$0 : USAGE : no value provided for $_ argument.\n\n";
	}
	next;
    }
    if (/^-s2p$/) {
	if ($ARGV[0] && ($ARGV[0] =~ /^[^-]/)) {
	    $s2p_file = shift;
	}
	else {
	    print STDERR "$0 : USAGE : no value provided for $_ argument.\n\n";
	}
	next;
    }
    if (/^-sbo$/) {
	if ($ARGV[0] && ($ARGV[0] =~ /^[^-]/)) {
	    $sbo_file = shift;
	}
	else {
	    print STDERR "$0 : USAGE : no value provided for $_ argument.\n\n";
	}
	next;
    }
    if (/^-sbt$/) {
	if ($ARGV[0] && ($ARGV[0] =~ /^[^-]/)) {
	    $sbt_file = shift;
	}
	else {
	    print STDERR "$0 : USAGE : no value provided for $_ argument.\n\n";
	}
	next;
    }
    if (/^-sdr$/) {
	if ($ARGV[0] && ($ARGV[0] =~ /^[^-]/)) {
	    $sdr_file = shift;
	}
	else {
	    print STDERR "$0 : USAGE : no value provided for $_ argument.\n\n";
	}
	next;
    }
    if (/^-sdt$/) {
	while ($ARGV[0] && ($ARGV[0] =~ /^[^-]/)) {
	    if ($sdr_title) {
		$sdr_title .= ' ';
	    }
	    $sdr_title .= $ARGV[0];
	    shift;
	}
	unless ($sdr_title) {
	    print STDERR "$0 : USAGE : no value provided for -sdt argument.\n\n";
	}
	next;
    }
    if (/^-vdc$/) {
	if ($ARGV[0] && ($ARGV[0] =~ /^[^-]/)) {
	    $ver_div = shift;
	}
	else {
	    print STDERR "$0 : USAGE : no value provided for $_ argument.\n\n";
	}
	next;
    }

    /^-ad$/ && do { $allow_dups = 1; next; };
    /^-mp$/ && do { $min_parse = 1; next; };
    /^-isn$/ && do { $idx_incl_seqnm = 1; next; };
    /^-isl$/ && do { $idx_incl_seqlen = 1; next; };
    /^-idc$/ && do { $idx_incl_divcode = 1; next; };
    /^-iud$/ && do { $idx_incl_udate = 1; next; };
    /^-nod$/ && do { $no_ddbj = 1; next; };
    /^-noe$/ && do { $no_embl = 1; next; };
    /^-nog$/ && do { $no_genbank = 1; next; };
    /^-z$/ && do { $dbx = 1; next; };
    /^-h$/ && &Usage;
    /^-qh$/ && &Usage_Brief;

    &Usage;		# Bad argument
}			# Endwhile ARGV	

if ($input_file) {
    unless (-e $input_file) {
	print STDERR "$0 : ERROR : input flatfile >$input_file< not found\n";
	exit 2;
    }
    unless (-f _) {
	print STDERR "$0 : ERROR : input flatfile >$input_file< is not a plain file\n";
	exit 2;
    }
}
else {
    print STDOUT "$0 : WARN  : using STDIN for input : byte offsets in index file >$idx_file< will *NOT* be correct\n" if ($idx_file);
}

if ($ver_div) {
    # Strip any trailing digits from $ver_div and
    # convert to uppercase (pri7 -> PRI, vrt -> VRT, etc)
    $ver_div =~ s/\d+$//;
    $ver_div =~ tr/a-z/A-Z/;
}

if (defined($arg_idx_file)) {
    if ($arg_idx_file ne "") {
	$idx_file = $arg_idx_file;
	$idx_fh = IO::File->new("> $idx_file");
	unless (defined ($idx_fh)) {
	    print STDERR "$0 : FATAL : IO::File new() failure : file >$idx_file< : status $? : error $!\n";
	    exit 2;
	}
    }
    else {
	$idx_fh = "STDOUT";
	$idx_file = "to_stdout";
    }
}
else {
    # No value provided for -o, so do not output index-data, even to stdout
    $idx_fh = "";
    $idx_file = "";
}

unless ($idx_file || $psid_file || $p2n_file || $s2p_file || $sbo_file || $sbt_file || $sdr_file) {
    print STDERR "$0 : ERROR : at least one of -o, -psi, -p2n, -s2p, -sbo, -sbt, and -sdr must be provided\n";
    exit 2;
}

if ($psid_file) {
    $psid_fh = IO::File->new("> $psid_file");
    unless (defined ($psid_fh)) {
	print STDERR "$0 : FATAL : IO::File new() failure : file >$psid_file< : status $? : error $!\n";
	exit 2;
    }
}
if ($p2n_file) {
    $p2n_fh = IO::File->new("> $p2n_file");
    unless (defined ($p2n_fh)) {
	print STDERR "$0 : FATAL : IO::File new() failure : file >$p2n_file< : status $? : error $!\n";
	exit 2;
    }
}
if ($s2p_file) {
    $s2p_fh = IO::File->new("> $s2p_file");
    unless (defined ($s2p_fh)) {
	print STDERR "$0 : FATAL : IO::File new() failure : file >$s2p_file< : status $? : error $!\n";
	exit 2;
    }
}
if ($sbo_file) {
    $sbo_fh = IO::File->new("> $sbo_file");
    unless (defined ($sbo_fh)) {
	print STDERR "$0 : FATAL : IO::File new() failure : file >$sbo_file< : status $? : error $!\n";
	exit 2;
    }
}
if ($sbt_file) {
    $sbt_fh = IO::File->new("> $sbt_file");
    unless (defined ($sbt_fh)) {
	print STDERR "$0 : FATAL : IO::File new() failure : file >$sbt_file< : status $? : error $!\n";
	exit 2;
    }
}
if ($sdr_file) {
    $sdr_fh = IO::File->new("> $sdr_file");
    unless (defined ($sdr_fh)) {
	print STDERR "$0 : FATAL : IO::File new() failure : file >$sdr_file< : status $? : error $!\n";
	exit 2;
    }
}

#---------------------------------------------------------------------
# Variables and their usage.
#---------------------------------------------------------------------

my ($rec_no) = 0;		# Number of the current record within $input_file .

my ($pri_seqid) = "";		# Primary sequence identifier for a record read
				# from $input_file. Eg, the accession number.

my ($byte_offset) = 0;		# Byte offset of a record within $input_file .

my ($idx_data) = "";		# The data which will be stored in $idx_file,
				# in conjunction with the primary seqid .

my (@sec_seqid) = ();		# Secondary sequence identifiers for a record.
				# Eg, accession numbers (now "secondary") under
				# which the data for a record _used_ to be available.

my ($seq_name) = "";		# Mnemonic name for the sequence in a record
				# read from $input_file. Usually the same as
				# $pri_seqid if no distinctive name has been
				# given to the sequence.

my ($seq_len) = 0;		# Sequence length (number of bases/residues)
my ($div_code) = "";		# Division code 

my ($defline) = "";		# Definition line : descriptive text
				# summarizing the nature of a record.

my ($udate) = "";

my (@org_name) = ();		# Name(s) of the organism from which the sequence
				# in the record was obtained.

my (%IdxData) = ();		# Hash which stores the indexed data for each
				# record in $input_file. Its keys are the 
				# primary seqids (eg, accession numbers) of the
				# records.

my (%Seqs_ByOrgName) = ();	# Hash: tallies the number of sequences
				# per organism name.

my (%Bases_ByOrgName) = ();	# Hash: tallies the number of
				# basepairs/residues per organism name.

my ($format_nm) = "";		# Name of the flatfile format utilized by
				# the records in $input_file. The format
				# of the very first record encountered
				# is assumed to apply to *all* the records
				# in $input_file.

my ($prob_divcode) = 0;		# Set to true if a division code problem has
				# been detected for one or more records. Ie,
				# $ver_div != $divcode 

my ($total_bases) = 0;		# Total number of bases/residues for all the
				# sequence records in $input_file

my ($sdr_line_max) = 77;	# Max length of a line in the Short Directory
				# output file.
my ($sdr_name_max) = 11;	# Max length that the sequence name can occupy
				# at the start of a line in the Short Directory
my ($sdr_defline_max) = 0;	# Max length that the definition line can occupy
				# within the Short Directory.
my ($sdr_defline_start) = 13;	# Start position for the definition line.

my ($sdr_space) = 0;		# Temp var used to format Short Directory data
my ($temp1_sdr) = "";		# Ditto
my ($temp2_sdr) = "";		# Ditto

#---------------------------------------------------------------------
# 
#---------------------------------------------------------------------

my ($ffph) = MFFP_New($input_file);

unless($ffph) {
    print STDERR "$0 : FATAL : MFFP_New failure : file >$input_file<\n";
    exit 2;
}

##MFFP_Set_AllowJunk_Flag($ffph);	# Don't set this in production GbRelease contexts

$min_parse && MFFP_Set_MinParse_Flag($ffph);

while (! MFFP_At_EOF($ffph)) {

    $idx_data = "";
    $pri_seqid = "";
    @sec_seqid = ();
    $seq_name = "";
    $seq_len = "";
    $div_code = "";
    $udate = "";
    @org_name = ();

    MFFP_Parse_Record($ffph,\$pri_seqid,\$seq_name,\$seq_len,\$div_code,\$defline,\$udate,\@org_name,
		      \$rec_no,\$byte_offset,\@sec_seqid) || do {
       print STDERR "$0 : FATAL : MFFP_Parse_Record failure : at or near record >$rec_no< : sequence >$pri_seqid< : file >$input_file<\n";
       exit 2;
    };

    last if MFFP_At_EOF($ffph) && $min_parse;

    if ($dbx) {
	print STDOUT "$0 : DBX   : record number    = >$rec_no<\n";
	print STDOUT "$0 : DBX   : primary seqid    = >$pri_seqid<\n";
	print STDOUT "$0 : DBX   : secondary seqids = >@sec_seqid<\n";
	print STDOUT "$0 : DBX   : sequence name    = >$seq_name<\n";
	print STDOUT "$0 : DBX   : seq_len          = >$seq_len<\n";
	print STDOUT "$0 : DBX   : div_code         = >$div_code<\n";
	print STDOUT "$0 : DBX   : defline          = >$defline<\n";
	print STDOUT "$0 : DBX   : update-date      = >$udate<\n";
	print STDOUT "$0 : DBX   : organism names   = >@org_name<\n";
	print STDOUT "$0 : DBX   : byte_offset      = >$byte_offset<\n";
    }

    # Determine the format utilized by the records in the
    # input flatfile, based on the result of parsing
    # the first record.

    # Close and undef filehandles for any outputs requested
    # by the user that are not appropriate for that format.

    # Null out $ver_div if the format doesn't utilize
    # division codes. Reset the flags used to skip records
    # from specified data sources unless that format
    # is utilized by those sources.

    # TTD : ffidx.pl : Should these actions be silent or not?

    if ($rec_no == 1) {
	$format_nm = MFFP_Get_FF_Format_Name($ffph);
	unless ($format_nm) {
	    print STDERR "$0 : FATAL : no format name returned by MFFP_Get_FF_Format_Name : record number >$rec_no< : sequence >$pri_seqid< : file >$input_file<\n";
	    exit 2;
	}
	unless ($format_nm =~ /genbank/i || $format_nm =~ /embl/i) {
	    $psid_fh && do { close($psid_fh); undef $psid_fh; };
	    $p2n_fh  && do { close($p2n_fh); undef $p2n_fh; };
	    $s2p_fh  && do { close($s2p_fh); undef $s2p_fh; };
	    $sbo_fh  && do { close($sbo_fh); undef $sbo_fh; };
	    $sdr_fh  && do { close($sdr_fh); undef $sdr_fh; };
	    $ver_div = "";
	    $no_ddbj = 0;
	    $no_embl = 0;
	    $no_genbank = 0;
	}
    }

    # Skip records from data sources that the user has
    # indicated should not be processed.

    if ($no_ddbj || $no_embl || $no_genbank) {
	next if ( $no_ddbj && MFFP_IS_DDBJ_NtAcc($pri_seqid) );
	next if ( $no_embl && MFFP_IS_EMBL_NtAcc($pri_seqid) );
	next if ( $no_genbank && MFFP_IS_GenBank_NtAcc($pri_seqid) );
    }

    $total_bases += $seq_len if ($sbt_fh);

    if ($ver_div && ($div_code ne $ver_div)) {
	print STDERR "$0 : ERROR : divison code mismatch : expected >$ver_div< : encountered >$div_code< : record number >$rec_no< : sequence >$pri_seqid< : file >$input_file<\n";
	$prob_divcode = 1;
    }

    $idx_data = $byte_offset;
    $idx_data .= "|$seq_name" if $idx_incl_seqnm;
    $idx_data .= "|$seq_len" if $idx_incl_seqlen;
    $idx_data .= "|$div_code" if $idx_incl_divcode;
    $idx_data .= "|$udate" if $idx_incl_udate;

    if (exists($IdxData{$pri_seqid})) {
	if (! $allow_dups) {
	    print STDERR "$0 : FATAL : duplicate primary seqid : record number >$rec_no< : sequence >$pri_seqid< : file >$input_file<\n";
	    exit 2;
	}
	else {
	    print STDERR "$0 : WARN  : duplicate primary seqid : record number >$rec_no< : sequence >$pri_seqid< : file >$input_file< : only the record with greatest byte-offset in the input file will be indexed\n";
	}
    }

    $IdxData{$pri_seqid} = $idx_data;

    if ($psid_fh) {
	unless (print $psid_fh "$pri_seqid\n") {
	    print STDERR "$0 : FATAL : print failure : file >$psid_file< : status $? : error $!\n";
	    exit 2;
	}
    }
    if ($p2n_fh) {
	unless (print $p2n_fh "$pri_seqid|$seq_name\n") {
	    print STDERR "$0 : FATAL : print failure : file >$p2n_file< : status $? : error $!\n";
	    exit 2;
	}
    }
    if ($s2p_fh && scalar(@sec_seqid)) {
	foreach (@sec_seqid) {
	    unless (print $s2p_fh "$_|$pri_seqid\n") {
		print STDERR "$0 : FATAL : print failure : file >$s2p_file< : status $? : error $!\n";
		exit 2;
	    }
	}
    }
    if ($sbo_fh) {

	# If there are multiple organism names, only the first will be used for the
	# tally of the number of sequences and number of bases/residues per organism.

	if ($seq_len && @org_name) {
	    $Seqs_ByOrgName{$org_name[0]}++;
	    $Bases_ByOrgName{$org_name[0]} += $seq_len;
	}
	else {
	    print STDERR "$0 : WARN  : missing sequence length : record number >$rec_no< : sequence >$pri_seqid< : file >$input_file<\n" unless ($seq_len);
	    print STDERR "$0 : WARN  : missing organism name : record number >$rec_no< : sequence >$pri_seqid< : file >$input_file<\n" unless (@org_name);
	}
    }
    if ($sdr_fh) {

	if ($rec_no == 1) {
	    unless (print $sdr_fh " " x 20, "$sdr_title\n\n") {
		print STDERR "$0 : FATAL : print failure : file >$sdr_file< : status $? : error $!\n";
		exit 2;
	    }
	}

	# $seq_name must be printed as a string $sdr_name_max characters long.

	$temp1_sdr = sprintf "%-${sdr_name_max}.${sdr_name_max}s", $seq_name;

	# The maximum space that the definition line may occupy in the Short Directory
	# output file: subtract the string length of $seq_len, and one for the space
	# between the defline and $seq_len 

	$sdr_defline_max = ($sdr_line_max - $sdr_defline_start + 1) - length($seq_len) - 1;

	# See if $defline exceeds the max.

	# If not, sprintf $defline into a string forced to be the length of the max.

	# If so, find the rightmost space and break at that position,
	# then do the sprintf

	if (length($defline) > $sdr_defline_max) {
	    $temp2_sdr = substr($defline,0,$sdr_defline_max);
	    $sdr_space = rindex($temp2_sdr," ");
	    if ($sdr_space) {
		$temp2_sdr = substr($temp2_sdr,0,$sdr_space);
		$temp2_sdr = sprintf "%-${sdr_defline_max}.${sdr_defline_max}s", $temp2_sdr;
	    }
	    else {
		# Couldn't find a space.
		$temp2_sdr = sprintf "%-${sdr_defline_max}.${sdr_defline_max}s", $defline
		## chomp $temp2_sdr;
		## $temp2_sdr .= " ";
	    }
	}
	else {
	    $temp2_sdr = sprintf "%-${sdr_defline_max}.${sdr_defline_max}s", $defline;
	}
	unless (print $sdr_fh "$temp1_sdr $temp2_sdr $seq_len", "bp\n") {
	    print STDERR "$0 : FATAL : print failure : file >$sdr_file< : status $? : error $!\n";
	    exit 2;
	}
    }
}

BLOCK1: {

no strict 'refs';
if ($idx_fh) {
    while (($pri_seqid,$idx_data) = each %IdxData) {
	unless (print $idx_fh "$pri_seqid|$idx_data\n") {
	    print STDERR "$0 : FATAL : print failure : file >$idx_file< : status $? : error $!\n";
	    exit 2;
	}
    }
}

}

if ($sbo_fh) {
    foreach (keys (%Seqs_ByOrgName)) {
	unless (print $sbo_fh "$Seqs_ByOrgName{$_}|$Bases_ByOrgName{$_}|$_\n") {
	    print STDERR "$0 : FATAL : print failure : file >$sbo_file< : status $? : error $!\n";
	    exit 2;
	}
    }
}

if ($sbt_fh) {
    unless (print $sbt_fh "$rec_no|$total_bases\n") {
	print STDERR "$0 : FATAL : print failure : file >$sbt_file< : status $? : error $!\n";
	exit 2;
    }
}

# Output the delimiter that should appear at the end of the Short Directory
if ($sdr_fh) {
    unless (print $sdr_fh "ZZZZZZZZZZ\n\n") {
	print STDERR "$0 : FATAL : print failure : file >$sdr_file< : status $? : error $!\n";
	exit 2;
    }
}

#---------------------------------------------------------------------
# Close output files.
#---------------------------------------------------------------------

BLOCK2: {

no strict 'refs';

if ($idx_fh) {
    unless (close($idx_fh)) {
	print STDERR "$0 : FATAL : close failure : file >$idx_file< : status $? : error $!\n";
	exit 2;
    }
}
}

if ($psid_fh) {
    unless (close($psid_fh)) {
	print STDERR "$0 : FATAL : close failure : file >$psid_file< : status $? : error $!\n";
	exit 2;
    }
}
if ($p2n_fh) {
    unless (close($p2n_fh)) {
	print STDERR "$0 : FATAL : close failure : file >$p2n_file< : status $? : error $!\n";
	exit 2;
    }
}
if ($s2p_fh) {
    unless (close($s2p_fh)) {
	print STDERR "$0 : FATAL : close failure : file >$s2p_file< : status $? : error $!\n";
	exit 2;
    }
}
if ($sbo_fh) {
    unless (close($sbo_fh)) {
	print STDERR "$0 : FATAL : close failure : file >$sbo_file< : status $? : error $!\n";
	exit 2;
    }
}
if ($sbt_fh) {
    unless (close($sbt_fh)) {
	print STDERR "$0 : FATAL : close failure : file >$sbt_file< : status $? : error $!\n";
	exit 2;
    }
}
if ($sdr_fh) {
    unless (close($sdr_fh)) {
	print STDERR "$0 : FATAL : close failure : file >$sdr_file< : status $? : error $!\n";
	exit 2;
    }
}

if ($prob_divcode) {
    print STDERR "$0 : FATAL : one or more records in >$input_file< do not have a division code of >$ver_div<\n";
    exit 2;
}

#----------------------------------------------------------------
# Function:	Usage
#
# Description:	Output a usage message for this script.
#
# Arguments:	None
#
#----------------------------------------------------------------
sub Usage {
    select(STDOUT);
    print "\nUsage:\t$0 -i Input_Flatfile -o Output_Indexfile [options] > IndexData\n\n";
    print "  Create an index and a variety of other outputs for a flatfile\n";
    print "  that contains biological sequence data.\n\n";
    print "  Arguments:\n\n";
    print "  -i      Input flatfile of biological sequence data. Required.\n";
    print "  -o      Output index file, containing the primary sequence\n";
    print "          identifiers (eg, accession numbers) encountered in the\n";
    print "          input flatfile and their byte-offsets. Optional.\n";

##    print "  -sv     Utilize *both* the sequence version number and the primary\n";
##    print "          sequence identifier as the keys of the output index file\n";
##    print "          (see -o). Switch, default is OFF.\n";

    print "  -isn    Include sequence names in the index file (see -o).\n";
    print "          Switch, default is OFF.\n";
    print "  -isl    Include sequence lengths in the index file (see -o).\n";
    print "          Switch, default is OFF.\n";
    print "  -idc    Include division codes in the index file (see -o).\n";
    print "          Switch, default is OFF.\n";
    print "  -iud    Include update-dates in the index file (see -o).\n";
    print "          Switch, default is OFF.\n";
    print "  -psi    Output file containing just the primary sequence identifiers\n";
    print "          of the records in the flatfile. Optional.\n";
    print "  -s2p    Output table of secondary sequence identifiers and the\n";
    print "          primary identifiers of the sequences in which the secondaries\n";
    print "          are found. Optional.\n";
    print "  -p2n    Output table of primary sequence identifiers and sequence\n";
    print "          names. Optional.\n";
    print "  -sbo    Output table containing the number of sequences and the\n";
    print "          number of bases/residues for each organism name encountered\n";
    print "          in the input flatfile. Optional.\n";
    print "  -sbt    Output file for the total sequences and the total number of\n";
    print "          bases/residues for all the records in the flatfile. Optional.\n";
    print "  -sdr    Output 'Short Directory', containing a brief description of\n";
    print "          every record in the flatfile. Optional.\n";
    print "  -sdt    The 'Title' for the Short Directory. Optional.\n";
    print "  -ad     Allow 'duplicate' records in the input flatfile (based on\n";
    print "          primary sequence identifier). Switch, default is OFF.\n";
    print "  -mp     Minimally-parse the records in the flatfile, in order to\n";
    print "          reduce execution time. Switch, default is OFF.\n";
    print "  -vdc    Verify that the division code of *ALL* records in the input\n";
    print "          flatfile match this supplied division code value. Optional.\n";
    print "  -nod    Do not process DDBJ records that may be present in the input\n";
    print "          flatfile (see -i). Switch, default is OFF.\n";
    print "  -noe    Do not process EMBL records that may be present in the input\n";
    print "          flatfile (see -i). Switch, default is OFF.\n";
    print "  -nog    Do not process GenBank records that may be present in the input\n";
    print "          flatfile (see -i). Switch, default is OFF.\n";

##    print "  -ilbo   For non-unique records encountered in the input flatfile,\n";
##    print "          save the least of their byte offsets in the index file.\n";
##    print "          Switch, default is OFF: this means that the *greatest*\n";
##    print "          byte-offset is normally saved for non-unique records.\n";

    print "  -h      Output this usage message.\n\n";
    print "  -qh     Output a concise version of this usage message.\n\n";

    print "NOTES:\n";
    print "\n";
    print "When the -o argument is not provided, the output index data are directed\n";
    print "to STDOUT. Provide -o without an argument to prevent output to STDOUT.\n";
    print "\n";

##    print "Some sources of flatfile data (eg, SWISS-PROT) do not utilize sequence\n";
##    print "version numbers. Supplying the -sv switch for those sources will thus\n";
##    print "have no effect.\n";
##    print "\n";

    print "If the -ad switch is provided, then the presence of more than one record\n";
    print "with the same primary sequence identifier in the input flatfile is *NOT*\n";
    print "considered an error. Do not use -ad if you need to verify that all records\n";
    print "are unique, based solely on their primary sequence identifiers.\n";
    print "\n";

#    print "If the -sv switch is provided, then the combination of the primary sequence\n";
#    print "identifier (eg, accession number) and the sequence version number is treated\n";
#    print "as the identifier for a record. So if records with the same primary identifier\n";
#    print "but differing sequence version numbers are present in the input flatfile, they\n";
#    print "will *NOT* be treated as duplicates, and no error message will be generated\n";
#    print "even if the -ad switch is provided. Do not use -sv if you need to verify that\n";
#    print "all records are unique, based solely on their primary sequence identifiers.\n";
#    print "\n";

    print "If the -mp switch is provided, this utility will run about twice as fast.\n";
    print "HOWEVER! By using the switch, many potential data problems (invalid sequence\n";
    print "characters, invalid linetypes, data that lies between records, etc) will\n";
    print "*NOT* be detected. So do not provide -mp when using this utility as a\n";
    print "(half-hearted) validation tool for the contents of the input flatfile.\n";
    print "\n";
    exit 1;
}

#----------------------------------------------------------------
# Function:	Usage_Brief
#
# Description:	Print a brief usage message for this script.
#
# Arguments:	None
#
#----------------------------------------------------------------
sub Usage_Brief {
    select(STDOUT);
    print "\nUsage:\t$0 -i Input_Flatfile -o Output_Indexfile -isn -isl -idc -iud\n";
    print "  -h -qh -ad -mp -vdc -nod -noe -nog\n";
    print "  -psi File -s2p File -p2n File -sbo File -sbt File -sdr File\n\n";
    print "\n";
    exit 1;
}

#====================================================================================
#====================================================================================
#
# MFFP == Minimal Flatfile Parser
#
# The following MFFP_* functions provide support for *MINIMAL* parsing of flatfiles
# commonly used to represent of biological sequence data: GenBank flatfiles,
# EMBL flatfiles, SWISS-PROT flatfiles, etc.
#
# Since NCBI uses ASN.1 to store, manipulate, and transmit biological sequence data,
# the functions provided here support only basic tasks: parsing sequence identifiers,
# sequence lengths, and linetypes from sequence records; checking for illegal linetypes;
# checking the content of the sequence data for illegal characters; etc.
#
# A full-blown parser, perhaps implemented using object-oriented Perl, could
# conceivably be useful to some users. But we have decided to limit our efforts,
# focussing on just those tasks that we commonly perform with flatfiles ourselves.
#
# TTD : Add support for molecule topology and type of ID/LOCUS linetypes
#
# Author:  Mark L. Cavanaugh (cavanaug@ncbi.nlm.nih.gov)
# Created: 08/??/2001
# Updated: 11/09/2001
#
#====================================================================================
#====================================================================================


#----------------------------------------------------------------
# Function:	MFFP_Parse_GBFF_Locus
#
# Description:	Parse the LOCUS line of a GenBank flatfile,
#		returning the LOCUS name, sequence length,
#		division code, and update-date .
#
# Arguments:	line : the LOCUS line to be parsed
#
#		locus_nm : scalar ref for the LOCUS name
#		  parsed from line
#
#		locus_seqlen : scalar ref for the sequence
#		  length parsed from line
#
#		locus_divcode : scalar ref for the division
#		  code parsed from line
#
#		locus_udate : scalar ref for the update date
#		  parsed from line
#
#----------------------------------------------------------------
sub MFFP_Parse_GBFF_Locus {
    my($line, $locus_nm, $locus_seqlen, $locus_divcode, $locus_udate) = @_;
    my($sub_nm) = (caller(0))[3];

    my (@toks) = ();

    if (@_ != 5) {
	print STDERR "$0 : ERROR : $sub_nm : expected >5< arguments, called with >", scalar(@_), "<\n";
	return 0;
    }
    if (ref($locus_nm) ne 'SCALAR') {
	print STDERR "$0 : ERROR : $sub_nm : expected scalar reference for locus_nm arg, not >$locus_nm<\n";
	return 0;
    }
    if (ref($locus_seqlen) ne 'SCALAR') {
	print STDERR "$0 : ERROR : $sub_nm : expected scalar reference for locus_seqlen arg, not >$locus_seqlen<\n";
	return 0;
    }
    if (ref($locus_divcode) ne 'SCALAR') {
	print STDERR "$0 : ERROR : $sub_nm : expected scalar reference for locus_divcode arg, not >$locus_divcode<\n";
	return 0;
    }
    if (ref($locus_udate) ne 'SCALAR') {
	print STDERR "$0 : ERROR : $sub_nm : expected scalar reference for locus_udate arg, not >$locus_udate<\n";
	return 0;
    }

    $line || return 0;

    # Blow away the caller's refs

    $ {$locus_nm} = "";
    $ {$locus_seqlen} = "";
    $ {$locus_divcode} = "";
    $ {$locus_udate} = "";

    unless ($line =~ /^LOCUS\s+/) {
	print STDERR "$0 : ERROR : $sub_nm : line does not start with expected LOCUS token : line >$line<\n";
	return 0;
    }

    @toks = split(/\s+/,$line);
    if ($#toks == 8) {
	$ {$locus_nm} = $toks[1];
	$ {$locus_seqlen} = $toks[2];
	$ {$locus_divcode} = $toks[6];
	$ {$locus_udate} = $toks[7];
    }
    else {
	# support for old-style GenBank LOCUS line, which could be
	# either 7 *or* 8 tokens
	$ {$locus_nm} = $toks[1];
	$ {$locus_seqlen} = $toks[2];
	$ {$locus_udate} = pop(@toks);	# Update date is last.
	$ {$locus_divcode} = pop(@toks);# Divcode is next to last.
    }
    unless ( $ {$locus_nm} && $ {$locus_seqlen} && $ {$locus_divcode} && $ {$locus_udate} ) {
	print STDERR "$0 : ERROR : $sub_nm : one or more values parsed from LOCUS line is null : line >$line<\n";
	return 0;
    }

    # Success!
    1;
}
#----------------------------------------------------------------
# Function:	MFFP_Parse_EFF_Id
#
# Description:	Parse the ID line from an EMBL flatfile,
#		returning the entry name, sequence length,
#		and division code.
#
# Arguments:	line : the ID line to be parsed
#
#		id_name : scalar ref for the identifying name
#		  parsed from line
#
#		id_seqlen : scalar ref for the sequence
#		  length parsed from line
#
#		id_divcode : scalar ref for the division
#		  code parsed from line
#
#----------------------------------------------------------------
sub MFFP_Parse_EFF_Id {
    my($line, $id_name, $id_seqlen, $id_divcode) = @_;
    my($sub_nm) = (caller(0))[3];

    my (@toks) = ();

    if (@_ != 4) {
	print STDERR "$0 : ERROR : $sub_nm : expected >4< arguments, called with >", scalar(@_), "<\n";
	return 0;
    }
    if (ref($id_name) ne 'SCALAR') {
	print STDERR "$0 : ERROR : $sub_nm : expected scalar reference for id_name arg, not >$id_name<\n";
	return 0;
    }
    if (ref($id_seqlen) ne 'SCALAR') {
	print STDERR "$0 : ERROR : $sub_nm : expected scalar reference for id_seqlen arg, not >$id_seqlen<\n";
	return 0;
    }
    if (ref($id_divcode) ne 'SCALAR') {
	print STDERR "$0 : ERROR : $sub_nm : expected scalar reference for id_divcode arg, not >$id_divcode<\n";
	return 0;
    }

    $line || return 0;

    # Blow away the caller's refs

    $ {$id_name} = "";
    $ {$id_seqlen} = "";
    $ {$id_divcode} = "";

    unless ($line =~ /^ID\s+/) {
	print STDERR "$0 : ERROR : $sub_nm : line does not start with expected ID token : line >$line<\n";
	return 0;
    }

    @toks = split(/\s+/,$line);

    # Presence of topology token 'circular' is optional, so ID line can have
    # either 7 or 8 tokens.

    if (scalar(@toks) != 7) {
	if (scalar(@toks) != 8) {
	    print STDERR "$0 : WARN  : $sub_nm : unusual number of tokens on EMBL ID line : >", scalar(@toks), "< vs >7< : line >$line<\n";
	}
    }

    $ {$id_name} = $toks[1];

    pop(@toks);			# Ignore "BP."
    $ {$id_seqlen} = pop(@toks);
    $ {$id_divcode} = pop(@toks);

    $ {$id_divcode} =~ s/;$//;

    unless ( $ {$id_name} && $ {$id_seqlen} && $ {$id_divcode} ) {
	print STDERR "$0 : ERROR : $sub_nm : one or more values parsed from ID line is null : line >$line<\n";
	return 0;
    }

    # Success!
    1;
}
#----------------------------------------------------------------
# Function:	MFFP_New
#
# Description:	Return a hash reference that configures
#		the parsing of a flatfile
#
# Arguments:	filename : the name of the flatfile to be parsed
#
# Returns:	Hashref
#
#----------------------------------------------------------------
sub MFFP_New {
    my ($flatfile) = @_;
    my($sub_nm) = (caller(0))[3];

    my ($href) = {};
    my ($fh);

    $fh = IO::File->new("< $flatfile");

    if ($flatfile) {
	unless (defined ($fh)) {
	    print STDERR "$0 : ERROR : $sub_nm : IO::File new() failure : file >$flatfile< : status $? : error $!\n";
	    return undef;
	}
    }
    else {
	$fh = *STDIN;
	unless (defined ($fh)) {
	    print STDERR "$0 : ERROR : $sub_nm : stdin fh undefined\n";
	    return undef;
	}
    }

    $href->{file_handle} = $fh;

    # we don't yet know the name of the format utilized
    # by records in $filename

    $href->{format_nm} = "";

    # assume that the flatfile is *not* to be
    # minimally parsed

    $href->{minimal_parse} = 0;

    # start-of-record regexp; should match the
    # first line of any record in the supported
    # flatfile formats

    $href->{sor_regexp} = "(LOCUS|ID|ENTRY)\\s+";

    # end-of-record regexp; should match the
    # last line of any record in the supported
    # flatfile formats

    $href->{eor_regexp} = "(\/\/|\/\/\/)\$";

    # assume that junk which might exist between
    # records of the flatfile is NOT to be allowed
    # (the *much* safer course).

    # blank lines and whitespace-only lines are
    # considered to be junk

    $href->{allow_junk_btwn_rec} = 0;
    
    return $href;
}

#----------------------------------------------------------------
# Function:	MFFP_Init_By_FF_Format_Name
#
# Description:	Initialize format-specific fields of a hash reference
#		that is used to configure the parsing of a flatfile,
#		based on the name of the format utilized by the records
#		it contains.
#
# Arguments:	ffph : reference to a hash that configures parsing
#		  of a flatfile
#
#		format : flatfile format type (eg: GenBank, EMBL, etc)
#
# Returns:	0 on failure
#		1 on success
#
#----------------------------------------------------------------
sub MFFP_Init_By_FF_Format_Name {
    my ($ffph,$format) = @_;
    my($sub_nm) = (caller(0))[3];
    my(%ltypes) = {};

    if (ref($ffph) ne 'HASH') {
	print STDERR "$0 : ERROR : $sub_nm : expected hash reference for ffph arg, not >$ffph<\n";
	return 0;
    }

    unless ($format =~ /^genbank$/i || $format =~ /^embl$/i) {
	print STDERR "$0 : ERROR : $sub_nm : unsupported or invalid format argument : >$format<\n";
	return 0;
    }

    if ($format =~ /genbank/i) {
	$ffph->{format_nm} = "genbank";
	$ffph->{sor_delim} = "LOCUS\\s+";
	$ffph->{def_ptn} = "DEFINITION\\s+";
	$ffph->{acc_ptn} = "ACCESSION\\s+";
	$ffph->{sver_ptn} = "VERSION\\s+";
	$ffph->{date_ptn} = "";			# GBFF has no separate linetype for dates.
	$ffph->{org_ptn} = "  ORGANISM\\s+";
	$ffph->{seq_ptn} = "ORIGIN";
	$ffph->{eor_delim} = "\/\/";
	%ltypes = (
		   "LOCUS       ", 0,
		   "DEFINITION  ", 0,
		   "ACCESSION   ", 0,
		   "VERSION     ", 0,
		   "SEGMENT     ", 0,
		   "KEYWORDS    ", 0,
		   "SOURCE      ", 0,
		   "  ORGANISM  ", 0,
		   "REFERENCE   ", 0,
		   "  AUTHORS   ", 0,
		   "  CONSRTM   ", 0,
		   "  TITLE     ", 0,
		   "  JOURNAL   ", 0,
		   "  MEDLINE   ", 0,
		   "   PUBMED   ", 0,
		   "  REMARK    ", 0,
		   "COMMENT     ", 0,
		   "FEATURES    ", 0,
		   "BASE COUNT  ", 0,
		   "ORIGIN      ", 0,
		   "ORIGIN"      , 0,	# ORIGIN without any spaces after it
		   "            ", 0	# For lines within the feature table
		   );
	$ffph->{ltypes} = \%ltypes;
	$ffph->{max_line_length} = 79;
    }
    elsif ($format =~ /embl/i) {
	$ffph->{format_nm} = "embl";
	$ffph->{sor_delim} = "ID\\s+";
	$ffph->{acc_ptn} = "AC\\s+";
	$ffph->{sver_ptn} = "SV\\s+";
	$ffph->{date_ptn} = "DT\\s+";
	$ffph->{def_ptn} = "DE\\s+";
	$ffph->{org_ptn} = "OS\\s+";
	$ffph->{seq_ptn} = "SQ   ";
	$ffph->{eor_delim} = "\/\/";
	%ltypes = (
		   ID => 0,
		   AC => 0,
		   SV => 0,
		   DT => 0,
		   DE => 0,
		   KW => 0,
		   OS => 0,
		   OC => 0,
		   OG => 0,
		   RN => 0,
		   RP => 0,
		   RC => 0,
		   RA => 0,
		   RT => 0,
		   RL => 0,
		   DR => 0,
		   CC => 0,
		   FH => 0,
		   FT => 0,
		   SQ => 0,
		   XX => 0,
		   "     ", 0		# For lines within the feature table
		   );
	$ffph->{ltypes} = \%ltypes;
	$ffph->{max_line_length} = 79;
    }

    1;
}
#----------------------------------------------------------------
# Function:	MFFP_Get_FF_Format_Name
#
# Description:	Return the format name from a hash reference
#		that configures the parsing of a flatfile.
#
# Arguments:	ffph : reference to a hash that configures parsing
#		  of a flatfile
#
# Returns:	Format name that is stored in $ffph
#
#----------------------------------------------------------------
sub MFFP_Get_FF_Format_Name {
    my ($ffph,$format) = @_;
    my($sub_nm) = (caller(0))[3];
    my(%ltypes) = {};

    if (ref($ffph) ne 'HASH') {
	print STDERR "$0 : ERROR : $sub_nm : expected hash reference for ffph arg, not >$ffph<\n";
	return 0;
    }
    $ffph->{format_nm};
}
#----------------------------------------------------------------
# Function:	MFFP_IS_Valid_Linetype
#
# Description:	Return true if a line of input data read from
#               a flatfile begins with a legal linetype identifier
#
# Arguments:	ffph : reference to a hash that configures parsing
#		  of a flatfile
#		$line : input line from flatfile to be checked
#
# Returns:	1 if $line starts with a valid linetype identifier
#		0 if not
#
#----------------------------------------------------------------
sub MFFP_IS_Valid_Linetype {
    my ($ffph,$line) = @_;
    my($sub_nm) = (caller(0))[3];
    my($temp1);
    my($href);

    if (ref($ffph) ne 'HASH') {
	print STDERR "$0 : ERROR : $sub_nm : expected hash reference for ffph arg, not >$ffph<\n";
	return 0;
    }

    $line || return 0;

    if ($ffph->{format_nm} eq "genbank") {
	$temp1 = substr($line,0,12);
	##print STDOUT "$0 : DBX   : checking linetype >$temp1<\n";
	$href = $ffph->{ltypes};
	if (exists ($href->{$temp1})) {
	    return 1;
	}
	# For feature table lines.
	if ($line =~ /^     [\w-']+\s+/) {
	    return 1;
	}
	# For sequence data lines.
	if ($line =~ /^\s+\d+ /) {
	    return 1;
	}
    }
    elsif ($ffph->{format_nm} eq "embl") {
	$temp1 = substr($line,0,2);
	##print STDOUT "$0 : DBX   : checking linetype >$temp1<\n";
	$href = $ffph->{ltypes};
	if (exists ($href->{$temp1})) {
	    return 1;
	}
	# For sequence data lines.
	if ($line =~ /^     / && $line =~ /\s+\d+$/) {
	    return 1;
	}
    }
    0;
}
#----------------------------------------------------------------
# Function:	MFFP_Parse_Record
#
# Description:	Parse a biological sequence record represented
#		in a flatfile format, such as GenBank, EMBL, or
#		SWISS-PROT.
#
# Arguments:	ffph : hashref that configures the parsing of
#		  a flatfile which contains biological sequence
#		  data records
#
#		seqid_acc : scalar reference for the accession
#		   number identifier of a sequence record read
#		   from the flatfile; for return to caller
#
#		seqid_nm : scalar reference for the mnemonic
#		   name identifier of a sequence record read
#		   from the flatfile; for return to caller
#
#		seq_len : scalar reference for the length of
#		   the sequence data in the record read from
#		   the flatfile; for return to caller
#
#		divcode : scalar reference for the division code
#		   of the sequence record read from the flatfile
#
#		defline : scalar ref for the 'definition line'
#		   parsed from a sequence record.
#
#		udate : scalar reference for the update-date
#		   of the sequence record read from the flatfile;
#		   for return to caller
#
#		org_name : scalar reference for the organism name
#		   parsed from the organism linetype of a sequence
#		   record (ie, not from a feature table);
#		   for return to caller
#
#		rec_no : scalar reference for record count
#		   within the flatfile; for return to caller
#
#		byte_offset : scalar reference for the byte
#		   offset of the current record within the flatfile;
#		   for return to caller
#
#		sec_seqid : array reference for secondary accession
#		   number sequence identifiers parsed from a
#		   sequence record
#
# Notes:	You must first call MFFP_New to obtain a hashref used
#		to configure the parsing of the flatfile.
#
#		The format of the flatfile being parsed, once set,
#		is assumed to apply to all other records in the file.
#
#----------------------------------------------------------------

sub MFFP_Parse_Record {
    my($ffph,$seqid_acc,$seqid_nm,$seq_len,$divcode,$defline,$udate,$org_name,$rec_no,$byte_offset,$sec_seqid) = @_;
    my($sub_nm) = (caller(0))[3];

    my($sor_occ) = 0;		# Number of occurrences of the start-of-record
				# delimiter 

    my($acc_ptn_count) = 0;	# Number of times the pattern for the accession number
				# sequence identifier was encountered. A value >1 could
				# indicate a corrupted record.

    my ($seq_data_len) = 0;	# Actual length of the sequence data, as read from
				# the sequence data block. For comparison to the stated
				# $seq_len value.

    my ($temp1, $temp2);	# Scratch vars.

    my ($temp_def) = "";	# Scratch var.

    if (@_ != 11) {
	print STDERR "$0 : ERROR : $sub_nm : expected >11< arguments, called with >", scalar(@_), "<\n";
	return 0;
    }
    if (ref($ffph) ne 'HASH') {
	print STDERR "$0 : ERROR : $sub_nm : expected hash reference for ffph arg, not >$ffph<\n";
	return 0;
    }
    unless (defined($ffph->{format_nm})) {
	print STDERR "$0 : ERROR : $sub_nm : ffph hash element file_handle not defined : suspect hash!\n";
	return 0;
    }
    if (ref($seqid_acc) ne 'SCALAR') {
	print STDERR "$0 : ERROR : $sub_nm : expected scalar reference for seqid_acc arg, not >$seqid_acc<\n";
	return 0;
    }
    if (ref($seqid_nm) ne 'SCALAR') {
	print STDERR "$0 : ERROR : $sub_nm : expected scalar reference for seqid_nm arg, not >$seqid_nm<\n";
	return 0;
    }
    if (ref($seq_len) ne 'SCALAR') {
	print STDERR "$0 : ERROR : $sub_nm : expected scalar reference for seq_len arg, not >$seq_len<\n";
	return 0;
    }
    if (ref($divcode) ne 'SCALAR') {
	print STDERR "$0 : ERROR : $sub_nm : expected scalar reference for divcode arg, not >$divcode<\n";
	return 0;
    }
    if (ref($defline) ne 'SCALAR') {
	print STDERR "$0 : ERROR : $sub_nm : expected scalar reference for defline arg, not >$defline<\n";
	return 0;
    }
    if (ref($udate) ne 'SCALAR') {
	print STDERR "$0 : ERROR : $sub_nm : expected scalar reference for udate arg, not >$udate<\n";
	return 0;
    }
    if (ref($org_name) ne 'ARRAY') {
	print STDERR "$0 : ERROR : $sub_nm : expected array reference for org_name arg, not >$org_name<\n";
	return 0;
    }
    if (ref($rec_no) ne 'SCALAR') {
	print STDERR "$0 : ERROR : $sub_nm : expected scalar reference for rec_no arg, not >$rec_no<\n";
	return 0;
    }
    if (ref($byte_offset) ne 'SCALAR') {
	print STDERR "$0 : ERROR : $sub_nm : expected scalar reference for byte_offset arg, not >$byte_offset<\n";
	return 0;
    }
    if (ref($sec_seqid) ne 'ARRAY') {
	print STDERR "$0 : ERROR : $sub_nm : expected scalar reference for sec_seqid arg, not >$sec_seqid<\n";
	return 0;
    }

    my ($fh) = $ffph->{file_handle};

    # Blow away the caller's refs.
    # But leave $rec_no intact, since its value is incremented for every record.

    $ { $seqid_acc } = "";
    $ { $seqid_nm } = "";
    @ { $sec_seqid } = ();
    $ { $seq_len } = "";
    $ { $divcode } = "";
    $ { $defline } = "";
    $ { $udate } = "";
    @ { $org_name } = ();

    $ { $byte_offset } = tell;

    # If processing the very first record, read until a line
    # is encountered that looks like a start-of-record delimiter.
    # This skips any header that might exist at the start of
    # the flatfile.
    #
    # Do the same if the flatfile is only being minimally
    # parsed, since processing of the previous record
    # could have ceased *before* its end-of-record delimiter
    # was encountered.
    #
    # Otherwise, read just one line, because the previous
    # call to this routine presumably read up through the
    # end-of-record delimiter of the previous record.

    if ($ {$rec_no} == 0 || $ffph->{minimal_parse}) {
	while(<$fh>) {
	    chomp;

	    # if you encounter an end-of-record delimiter
	    # during this loop and the record counter is
	    # zero, then the first of the records in the
	    # flatfile must be partial

	    # would be more thorough to check for presence
	    # of a known linetype, but the format of the
	    # file is not yet known!

	    if (/^$ffph->{eor_regexp}/) {
		if ($ {$rec_no} == 0) {
		    print STDERR "$0 : ERROR : $sub_nm : partial record exists at the beginning of flatfile\n";
		    return 0;
		}
	    }
	    /^$ffph->{sor_regexp}/ && last;
	    $ { $byte_offset } = tell;
	}
	if (eof($fh)) {
	    if ($ffph->{minimal_parse}) {
		return 1;
	    }
	    else {
		print STDERR "$0 : ERROR : $sub_nm : end-of-file while looking for start of next record in flatfile\n";
		return 0;
	    }
	}
    }
    else {
	if ($ffph->{allow_junk_btwn_rec}) {
	    # Skip junk (including empty or whitespace-only lines) between records.
	    while(<$fh>) {
		/^$ffph->{sor_regexp}/ && last;
		$ { $byte_offset } = tell;
	    }
	}
	else {
	    $_ = <$fh>;
	}
	if (eof($fh) && $ffph->{allow_junk_btwn_rec}) {
	    return 1;
	}
	chomp;
    }

    # You should now be at the start of a record.
    # print STDERR "DBX : should be at record start : line is >$_<\n";

    unless (/^$ffph->{sor_regexp}/) {
	print STDERR "$0 : ERROR : $sub_nm : expected start-of-record delimiter was not found : line >$_<\n";
	return 0;
    }
    $ {$rec_no}++;

    # If first record, set the format of the file.

    if ($ {$rec_no} == 1) {
	if (/^LOCUS/) {
	    MFFP_Init_By_FF_Format_Name($ffph,"genbank");
	}
	elsif (/^ID/) {
	    MFFP_Init_By_FF_Format_Name($ffph,"embl");
	}
	else {
	    print STDERR "$0 : ERROR : $sub_nm : unrecognized linetype : cannot determine format of flatfile : line >$_<\n";
	    return 0;
	}
    }

    # while(1)
    #   last if EOR
    #   last if eof
    #   last if minimal_parse and you've gotten everything you need
    #   check for invalid linetype
    #   per-linetype if blocks
    #   each block must <$fh>, for next iteration
    # check that you got everything that you needed

    my ($done_scan) = 0;

    while (1) {
	## print STDOUT "DBX : processing line >$_<\n";

	last if (eof($fh));
	last if /^$ffph->{eor_delim}$/;
	last if ($done_scan);

	# HACK! Flatfile generator problem yields null lines
	# HACK! as of 11/2002. Skip such lines, for now. --MLC
	
	if ($_ eq "") {
	    print STDERR "$0 : WARN  : $sub_nm : null line within flatfile : record $ {$rec_no} : seqid >$ { $seqid_acc }< : line >$_<\n";
	    $_ = <$fh>;
	    chomp;
	    next;
	}

	unless (MFFP_IS_Valid_Linetype($ffph,$_)) {
	    print STDERR "$0 : ERROR : $sub_nm : invalid linetype : line >$_<\n";
	    return 0;
	}
	unless ($ffph->{minimal_parse}) {
	    if (length($_) > $ffph->{max_line_length}) {
		print STDERR "$0 : WARN  : $sub_nm : line exceeds maximum line length : record $ {$rec_no} : seqid >$ { $seqid_acc }< : line >$_<\n";
	    }
	}

	# Parse the start-of-record line.

	if (/^$ffph->{sor_delim}/) {
	    if ($ {$seqid_nm} ne "") {
		print STDERR "$0 : ERROR : $sub_nm : encountered start-of-record delimiter *within* record $ {$rec_no} : line >$<\n";
		return 0;
	    }
	    if ($ffph->{format_nm} eq "genbank") {
		MFFP_Parse_GBFF_Locus($_,$seqid_nm,$seq_len,$divcode,$udate) || do {
		    print STDERR "$0 : ERROR : $sub_nm : MFFP_Parse_GBFF_Locus failure : >line >$_<\n";
		    return 0;
		}
	    }
	    elsif ($ffph->{format_nm} eq "embl") {
		MFFP_Parse_EFF_Id($_,$seqid_nm,$seq_len,$divcode) || do {
		    print STDERR "$0 : ERROR : $sub_nm : MFFP_Parse_EFF_Id failure : line >$_<\n";
		    return 0;
		}
	    }
	    $_ = <$fh>;
	    chomp;
	}

	# Parse the accession number line.

	elsif (/^$ffph->{acc_ptn}/) {
	    if ($ {$seqid_acc} ne "") {
		print STDERR "$0 : ERROR : $sub_nm : multiple accession number linetypes : record $ {$rec_no} : line >$_<\n";
		return 0;
	    }
	    if ($ffph->{format_nm} eq "genbank") {
		($temp1, $$seqid_acc, $temp2) = split(/\s+/,$_,3);
		if ($temp2) {
		    push(@$sec_seqid,split(/\s+/,$temp2));
		}
	    }
	    elsif ($ffph->{format_nm} eq "embl") {
		($temp1, $$seqid_acc, $temp2) = split(/\s+/,$_,3);
		$ { $seqid_acc } =~ s/;$//;
		if ($temp2) {
		    push(@$sec_seqid,split(/\s*;\s*/,$temp2));
		}
	    }

	    # Secondary accessions can continue on additional lines
	    
	    while (<$fh>) {
		chomp;
		## print STDOUT "DBX : secondary accession while loop : line >$_<\n";
		last if (eof($fh));
		last if /^$ffph->{eor_delim}$/;

		unless (MFFP_IS_Valid_Linetype($ffph,$_)) {
		    print STDERR "$0 : ERROR : $sub_nm : invalid linetype : line >$_<\n";
		    return 0;
		}
		if ($ffph->{format_nm} eq "genbank") {
		    /^\s+/ || last;
		    s/\s+//;
		    push(@$sec_seqid,split(/\s+/,$_));
		}
		elsif ($ffph->{format_nm} eq "embl") {
		    /^$ffph->{acc_ptn}/ || last;
		    ($temp1, $temp2) = split(/\s+/,$_,2);
		    push(@$sec_seqid,split(/\s*;\s*/,$temp2));
		}
	    }

	    # No need to <$fh> here because of secondary acc while loop.
	}

	# Parse the 'definition' line. Definition line wraps
	# in GenBank format, with leading whitespace. In EMBL
	# format, every line of definition data starts with
	# $ffph->{def_ptn} .

	elsif (/^$ffph->{def_ptn}/) {

	    $ { $defline } = $';
	    $ { $defline } =~ s/\s+$//;

	    while (<$fh>) {
		chomp;
		## print STDOUT "DBX : defline while loop : line >$_<\n";
		last if (eof($fh));
		last if /^$ffph->{eor_delim}$/;

		unless (MFFP_IS_Valid_Linetype($ffph,$_)) {
		    print STDERR "$0 : ERROR : $sub_nm : invalid linetype : line >$_<\n";
		    return 0;
		}
		if ($ffph->{format_nm} eq "genbank") {
		    /^\s+/ || last;
		    $temp_def = $';
		}
		elsif ($ffph->{format_nm} eq "embl") {
		    /^$ffph->{def_ptn}/ || last;
		    $temp_def = $';
		}
		unless ( $ { $defline } =~ /-$/ ) {
		    $ { $defline } .= " ";
		}
		$ { $defline } .= $temp_def;
		$ { $defline } =~ s/\s+$//;
	    }
	    # No need to <$fh> here because of defline while loop.
	}

	# For EMBL format only, parse the DT linetype, in order to
	# obtain an update-date . Note that multiple DT linetypes
	# can be present.

	elsif ($ffph->{format_nm} eq "embl" && /^$ffph->{date_ptn}/ && /updated/i) {
	    if ($ { $udate } ne "") {
		print STDERR "$0 : ERROR : $sub_nm : multiple update dates : record $ {$rec_no} : using the first : $ { $udate } : >line >$_<\n";
	    }
	    else {
		($temp1, $ { $udate }, $temp2) = split(/\s+/,$_,3);
	    }
	    $_ = <$fh>;
	    chomp;
	}

	# Parse the organism line. Multiple organism lines possible in EMBL format.
	# If minimally parsing, set $done_scan since there's nothing after the
	# organism line that needs to be returned to the caller.

	elsif (/^$ffph->{org_ptn}/) {
	    if ($# {$org_name} >= 1 && $ffph->{format_nm} ne "embl") {
		print STDERR "$0 : ERROR : $sub_nm : multiple organism name linetypes : record $ {$rec_no} : line >$_<\n";
		return 0;
	    }
	    push(@$org_name,$');

	    if ($ffph->{minimal_parse}) {
		$done_scan = 1;
	    }
	    $_ = <$fh>;
	    chomp;
	}

	# Parse/validate the actual sequence data.
	# The data _follows_ the linetype used
	# to recognize its start. Not checking for
	# valid linetypes here because the sequence data
	# is in its own block, at the very end of
	# the record.

	elsif (/^$ffph->{seq_ptn}/) {
	    while (<$fh>) {
		chomp;
		## print STDOUT "DBX : sequence data while loop : line >$_<\n";
		last if (eof($fh));
		last if /^$ffph->{eor_delim}$/;

		if ($ffph->{format_nm} eq "genbank") {
		    s/^\s+\d+ //;
		    s/ //g;
		}
		elsif ($ffph->{format_nm} eq "embl") {
		    s/\s+\d+$//;
		    s/^\s+//;
		    s/ //g;
		}
		if (/[^abcdghkmnrstvwy]/) {
		    print STDERR "$0 : ERROR : $sub_nm : invalid sequence data : record $ {$rec_no} : seqid >$ { $seqid_acc }< : line >$_<\n";
		    return 0;
		}
		$seq_data_len += length($_);
	    }
	    # No need to <$fh> here because of sequence data while loop.
	}

	else {
	    # This else is important: this is where the read of the
	    # next line of a record occurs, if the previous line
	    # was not parsed in any way. Verification that the linetype
	    # is legal then occurs up at the top of the while() loop.

	    $_ = <$fh>;
	    chomp;
	}
    }

    unless ($ffph->{minimal_parse}) {
	if ($seq_data_len != $ { $seq_len } ) {
	    print STDERR "$0 : ERROR : $sub_nm : stated sequence length >$ { $seq_len }< disagrees with actual sequence data length >$seq_data_len< : record $ {$rec_no} : seqid >$ { $seqid_acc }<\n";
	    return 0;
	}
    }

    1;
}
#----------------------------------------------------------------
# Function:	MFFP_At_EOF
#
# Description:	Return true if at EOF of a flatfile being parsed.
#
# Arguments:	ffph : reference to a hash that configures parsing
#		  of a flatfile
#
# Returns:	1 on EOF
#		0 otherwise
#
#----------------------------------------------------------------
sub MFFP_At_EOF {
    my ($ffph) = @_;
    my($sub_nm) = (caller(0))[3];

    if (ref($ffph) ne 'HASH') {
	print STDERR "$0 : ERROR : $sub_nm : expected hash reference for ffph arg, not >$ffph<\n";
	return 0;
    }
    eof($ffph->{file_handle}) && return 1;
    return 0;
}
#----------------------------------------------------------------
# Function:	MFFP_Set_MinParse_Flag
#
# Description:	Turn on minimal parsing flag for a flatfile, in
#		order to reduce execution time.
#
# Arguments:	ffph : reference to a hash that configures parsing
#		  of a flatfile
#
# Returns:	1 on success
#		0 otherwise
#
#----------------------------------------------------------------
sub MFFP_Set_MinParse_Flag {
    my ($ffph) = @_;
    my($sub_nm) = (caller(0))[3];

    if (ref($ffph) ne 'HASH') {
	print STDERR "$0 : ERROR : $sub_nm : expected hash reference for ffph arg, not >$ffph<\n";
	return 0;
    }
    $ffph->{minimal_parse} = 1;
    return 1;
}
#----------------------------------------------------------------
# Function:	MFFP_Set_AllowJunk_Flag
#
# Description:	Turn on the flag that allows junk to exists between
#		records of a flatfile. This might be useful if records
#		are embedded within a file that contains other
#		information (eg, an email file).
#
# Arguments:	ffph : reference to a hash that configures parsing
#		  of a flatfile
#
# Returns:	1 on success
#		0 otherwise
#
#----------------------------------------------------------------
sub MFFP_Set_AllowJunk_Flag {
    my ($ffph) = @_;
    my($sub_nm) = (caller(0))[3];

    if (ref($ffph) ne 'HASH') {
	print STDERR "$0 : ERROR : $sub_nm : expected hash reference for ffph arg, not >$ffph<\n";
	return 0;
    }
    $ffph->{allow_junk_btwn_rec} = 1;
    return 1;
}
#----------------------------------------------------------------
# Function:	MFFP_IS_DDBJ_NtAcc
#
# Description:	Return TRUE if a token purported to be an
#		accession number could conceivably be an
#		accession that was issued by DDBJ for a
#		nucleotide sequence record.
#
# Arguments:	acc : accession number token
#
# Returns:	1 if accession appears to be from DDBJ
#		0 otherwise
#
#----------------------------------------------------------------
sub MFFP_IS_DDBJ_NtAcc {
    my ($acc) = @_;
    my($sub_nm) = (caller(0))[3];
    my($temp) = 0;

    $acc || return 0;

    # The length test saves unnecessary pattern matches
    # for clearly-invalid accessions, but at the cost
    # of the length() tests themselves for valid accessions. :(

    $temp = length($acc);

    if ($temp == 6) {
	if ($acc =~ /^[CDE]\d\d\d\d\d$/) {
	    return 1;
	}
	return 0;
    }
    elsif ($temp == 8) {
	# BR is for Third-Party Annotation and Consensus Sequence records (TPACS)
	if ($acc =~ /^(AB|AG|AK|AP|AT|AU|AV|BA|BB|BD|BJ|BP|BR|BS|BW|BY)\d\d\d\d\d\d$/) {
	    return 1;
	}
	return 0;
    }
    elsif ($temp == 12) {
	# Whole Genome Shotgun sequences.
	if ($acc =~ /^B[A-Z][A-Z][A-Z]\d\d\d\d\d\d\d\d$/) {
	    return 1;
	}
	return 0;
    }
    return 0;
}
#----------------------------------------------------------------
# Function:	MFFP_IS_EMBL_NtAcc
#
# Description:	Return TRUE if a token purported to be an
#		accession number could conceivably be an
#		accession that was issued by EBI for an EMBL
#		nucleotide sequence record.
#
# Arguments:	acc : accession number token
#
# Returns:	1 if accession appears to be from EMBL
#		0 otherwise
#
#----------------------------------------------------------------
sub MFFP_IS_EMBL_NtAcc {
    my ($acc) = @_;
    my($sub_nm) = (caller(0))[3];
    my($temp) = 0;

    $acc || return 0;

    # The length test saves unnecessary pattern matches
    # for clearly-invalid accessions, but at the cost
    # of the length() tests themselves for valid accessions :(

    $temp = length($acc);

    if ($temp == 6) {
	if ($acc =~ /^[AFVXYZ]\d\d\d\d\d$/) {
	    return 1;
	}
	return 0;
    }
    elsif ($temp == 8) {
	# BN is for Third-Party Annotation and Consensus Sequence records (TPACS)
	if ($acc =~ /^(AJ|AL|AM|AN|AX|BN|BX)\d\d\d\d\d\d$/) {
	    return 1;
	}
	return 0;
    }
    elsif ($temp == 12) {
	# Whole Genome Shotgun sequences.
	if ($acc =~ /^C[A-Z][A-Z][A-Z]\d\d\d\d\d\d\d\d$/) {
	    return 1;
	}
	return 0;
    }
    return 0;
}
#----------------------------------------------------------------
# Function:	MFFP_IS_GenBank_NtAcc
#
# Description:	Return TRUE if a token purported to be an
#		accession number could conceivably be an
#		accession that was issued by NCBI for a GenBank
#		nucleotide sequence record.
#
# Arguments:	acc : accession number token
#
# Returns:	1 if accession appears to be from GenBank
#		0 otherwise
#
#----------------------------------------------------------------
sub MFFP_IS_GenBank_NtAcc {
    my ($acc) = @_;
    my($sub_nm) = (caller(0))[3];
    my($temp) = 0;

    $acc || return 0;

    # The length test saves unnecessary pattern matches
    # for clearly-invalid accessions, but at the cost
    # of the length() tests themselves for valid accessions.

    $temp = length($acc);

    if ($temp == 6) {
	if ($acc =~ /^[JKLMNISTUGHRBW]\d\d\d\d\d$/) {
	    return 1;
	}
	return 0;
    }
    elsif ($temp == 8) {
	# BK and BL are for Third-Party Annotation and Consensus Sequence records (TPACS) and segmented TPACS records
	if ($acc =~ /^(AA|AC|AD|AE|AF|AH|AI|AQ|AR|AS|AW|AY|AZ|BC|BE|BF|BG|BH|BI|BK|BL|BM|BQ|BT|BU|BV|BZ|CA|CB|CC)\d\d\d\d\d\d$/) {
	    return 1;
	}
	return 0;
    }
    elsif ($temp == 12) {
	# Whole Genome Shotgun sequences.
	if ($acc =~ /^A[A-Z][A-Z][A-Z]\d\d\d\d\d\d\d\d$/) {
	    return 1;
	}
	return 0;
    }
    return 0;
}
