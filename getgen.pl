perl -MBio::SeqIO -e ' $informat="genbank"; $outformat="fasta"; $count = 0; for $infile (@ARGV) { $in = Bio::SeqIO->newFh(-file => $infile , -format => $informat); $out = Bio::SeqIO->newFh(-format => $outformat); while (<$in>) { print $out $_; $count++; } } warn "Translated $count sequences from $informat to $outformat format\n" ' @ARGV[1] @ARGV[2]

