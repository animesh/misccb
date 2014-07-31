#!/usr/local/bin/perl

use SWISS::Entry;
use SWISS::KW;

# Read an entire record at a time
$/ = "\/\/\n";

while (<>){
  # Read the entry
  $entry = SWISS::Entry->fromText($_);

  # Print the primary accession number of each entry.
  print $entry->AC, "\n";

  # If the entry has a SWISS-2DPAGE crossreference
  if ($entry->DRs->get('SWISS-2DPAGE')) {
    
    # Add the pseudo-keyword 'Gelelectrophoresis'
    my $kw = new SWISS::KW;
    $kw->text('Gelelectrophoresis');
    $entry->KWs->add($kw);
  };
  
  # Print all keywords
  foreach my $kw ($entry->KWs->elements()) {
    print $kw->text(), ", ";
  }

  print "\n\n";
}
