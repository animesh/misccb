# compare existing ensembl mappings with those generated by new xref mapper

# files should be in format
# xref_accession ensembl_type ensembl_id

# to dump this from an ensembl database use something like
# mysql -u ensro -h ecs2 -P 3365 -e "select x.dbprimary_acc, ox.ensembl_object_type, ox.ensembl_id from xref x, object_xref ox, external_db edb where edb.db_name like 'Uniprot/SWISSPROT' and x.external_db_id=edb.external_db_id and x.xref_id=ox.xref_id order by x.xref_id" homo_sapiens_core_25_34e > ensembl_swissprot_translations.txt


# ADDITIONALLY
# read in the synonym conversion. This is created with something like:-
#mysql -hecs4 -P3350 -uensadmin -pPASS -Dianl_xref_test -e"select x.accession, s.synonym from synonym s, xref x where x.xref_id = s.xref_id" > xref_synonym.txt

#ALSO get list of deleted accesions no need to look at these.
#down load wget http://us.expasy.org/txt/delac_tr.txt
#remove html rubbish and save.



use strict;

use Getopt::Long;

my ( $old, $new, $del, $syn, $pass, $port, $dbname  );

my %deleted;
my %synonym;


GetOptions( "old=s", \$old,
	    "new=s", \$new,
	    "del=s", \$del,
	    "syn=s", \$syn);

if( !($old && $new && $syn && $del) ) {
  usage();
  exit(1);
}

# read files into xref/object hashes
# TODO - more than one mapping?
get_deleted($del);
get_synonym($syn);


my ($new_x2e_r, $new_e2x_r) = read_mappings($new);
my ($old_x2e_r, $old_e2x_r) = read_mappings($old);

check_non_translations($new_e2x_r);

compare($old_x2e_r, $new_x2e_r, "xref");
compare($old_e2x_r, $new_e2x_r, "ensembl_object");


sub get_deleted {

  my ($file) = @_;

  open(FILE,"<".$file) || die "Could not open $file\n";

  while(<FILE>){
    chomp;
    $deleted{$_} = 1;
  }
  close FILE;
}

sub get_synonym {

  my ($file) = @_;

  open(FILE,"<".$file) || die "Could not open $file\n";

  <FILE>; #junk first line

  while(<FILE>){
    chomp;
    my @arr = split;
    $synonym{$arr[1]} = $arr[0];
  }
  close FILE;
}
    


# ----------------------------------------
# Compare mappings keyed on xref

sub compare {

  my ($old, $new, $desc) = @_;

  open(NEW_ONLY, ">${desc}_new_only.txt");
  open(OLD_ONLY, ">${desc}_old_only.txt");

  my ($matched, $mismatched, $new_only, $old_only, $total);

  foreach my $key (keys %$new) {

    # if a mapping exists, look for any matches
    my $found = 0;
    if (exists $old->{$key}) {
      foreach my $old_value (@{$old->{$key}}) {
	foreach my $new_value (@{$new->{$key}}) {
	  if ($old_value eq $new_value) {
	    $found = 1;
	  }
	}
      }
      if ($found) {
	$matched++;
      } else {
	$mismatched++;
      }
      $total++;

    } else {
      $new_only++;
      print NEW_ONLY join("\t", split(/\./, $key)) . "\n";
    }

  }

  foreach my $key (keys %$old) {

    if (!exists $new->{$key}) {
      $old_only++;
      print OLD_ONLY join("\t", split(/\./, $key)) . "\n";
    }

  }

  close(NEW_ONLY);
  close(OLD_ONLY);

  print "\nCompared " . scalar(keys %$new) . " new xref mappings with " . scalar(keys %$old) . " existing ensembl mappings\n";

  print "\nComparing keyed on $desc:\n\n";

  my $fmt = "%-30s\t%6d\t%5s%%\n";
  printf $fmt, "Mapped with same result:", $matched, pc($matched, $total);
  printf $fmt, "Mapped with different result:", $mismatched, pc($mismatched, $total);
  printf $fmt, "Total mapped:", $total, pc($total, $total);

  print "\nNumber mapped by new method only: $new_only\n";
  print "Number mapped by old method only: $old_only\n";

}

# ----------------------------------------------------------------------

sub pc {

  my ($a, $total) = @_;

  return "?" if (!$total);

  my $number = 100 * $a / $total;
  my $pad = "";
  $pad .= " " if ($number < 100);
  $pad .= " " if ($number < 10);

  return $pad . sprintf "%3.2f", $number;

}

sub usage {

print << "EOF";

Usage: compare_mapping.pl -old <old mapping file> -new <new mapping file>
       -del <deleted acc list file>  -syn <xref synonym file>

Mapping files should be in the following format:

xref_accession ensembl_type ensembl_id

EOF

}

# ----------------------------------------------------------------------

sub read_mappings {

  my $filename = shift;

  my %xref_to_ensembl;
  my %ensembl_to_xref;
  my $del=0;
  my $syn=0;

  open (FILE, $filename) || die "Can't read $filename\n";
  my $dummy = <FILE>; # skip first line

  while(<FILE>) {

    my ($xref_id, $type, $ensembl_id) = split;
    # TODO - better way of handling type?
    if(!defined($deleted{$xref_id})){
      if(defined($synonym{$xref_id})){
	$xref_id = $synonym{$xref_id};
	$syn++;
      }
      my $value = $ensembl_id . "." . $type;
      push @{$xref_to_ensembl{$xref_id}}, $value;
      push @{$ensembl_to_xref{$value}}, $xref_id;
    }
    else{
      $del++;
    }

  }

  close(FILE);

  my $i = 0;
  foreach my $xref_id (keys %xref_to_ensembl) {
    $i += @{$xref_to_ensembl{$xref_id}};
  }

  print "Read $i mappings from $filename\n";
  print "$del entries ignored due to deletions\n";
  print "$syn entries changed to synonym values\n";

  return (\%xref_to_ensembl, \%ensembl_to_xref);

}

# ----------------------------------------------------------------------

sub check_non_translations {

  my $new_e2x = shift;

  foreach my $key (keys %$new_e2x) {
    my ($ensembl_id, $type) = split(/\./, $key);
    if ($type !~ /Translation/i) {
      print "\n*Warning*: new xref file contains mappings to Ensembl " . lc($type) . "s; consider using convert_xrefs_to_all_translations.pl then comparing\n\n";
      return;
    }
  }

}

# ----------------------------------------------------------------------
