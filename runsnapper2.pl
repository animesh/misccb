@files=<s*-???.fasta>;
foreach $file (@files) {
        print "RunSnapper $file\n";
#       system("cp $file $file.cp");
#       system("sed 's/^PCUS/\>PCUS/' $file.cp  > $file"); 
        system("/usr/local/projects/BIOINFO/ASSEMBLY/bri/kmer/snapper/snapper2    -genomic Pwgs6dhmovlcod.scf.fasta -queries $file -positions PwgsScaf.posDB 
-verbose -numthreads 4 -ignore 100 -minmatchidentity 90 -minmatchcoverage 90  -output  $file.sim4db");
#       system ("rm $file.cp");-minmatchidentity 90 -minmatchcoverage 90
}

__END__
[animesh@astrakan 3355151.d]$ ls *-???.fasta | wc
    400     400    5800/usr/local/projects/BIOINFO/ASSEMBLY/bri/kmer/snapper/snapper2
[animesh@astrakan 3355151.d]$ /home/animesh/kmer/trunk/Linux-amd64/bin/snapper2 -genomic Pwgs6dhmovlcod.scf.fasta -queries ZZZ-001.fasta -positions X.posDB -
erbose -numthreads 16 -ignore 100 -minhitcoverage 0.5 -output  ZZZ-001.fasta.sim4db
  403   scp -r ash022@fimm.bccs.uib.no:/migrate/ash022/leaff .
  404  rsync --recursive ash022@fimm.bccs.uib.no:/migrate/ash022/leaff/ leaff/
  406  ./leaff/leaff --segment s1a 200 s_1_all.fasta
  410  ./leaff/leaff --segment s1a 200 s_1_all.fasta &
  411  ./leaff/leaff --segment s2ma 200 s_2m_all.fasta &

