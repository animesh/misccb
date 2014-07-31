print "File containing sequence names? ";
chomp ($file = <STDIN>);
open (FILEIN,"$file");
open (FILEOUT, ">blastx.bat");
while (chomp($seq = <FILEIN>)){
                $seqout = $seq.".out";
	print FILEOUT "/usr/local/blast/blastall -p blastx -d /disk1/gene-backup/database/nr -i $seq -o $seqout -T \n";
}
