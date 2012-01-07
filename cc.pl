system("ls -1 *.match > temp");
open(F,"temp");
while(<F>){
chomp;
system("clustalw2 $_");
}
	

