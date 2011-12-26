#!/util/bin/perl

$tabfilename = "/seq/tn02/tetraodon20010816/info_CD1/docs/TBR.table";
$renamefile = "/wga/data04/Tetraodon/fasta/genoscope.TBR.fasta";
$newfile = "/wga/data04/Tetraodon/fasta/genoscope.RENAMED.fasta";

open(INFILE, "$tabfilename");
open(RENAME, "$renamefile");
open(NEWFILE,">$newfile") or 
  die "ugh";


while (<RENAME>) 
{
  chomp;

  $name = $_;
  $t_name = $name;
  $t_name =~ /^(.)(.*)/;
  $tst = $1;
  if (  $tst eq ">" )
  {
    $new_name = $name;
    $correct_name = <INFILE>;   
    $correct_name =~ /^(.*)(\.SCF )(.*)$/;
    $fix=$3;

    $new_name =~ s/^(>).*(\.SCF)(.*)/\1$fix\2\3/;

    #print "$name $new_name\n";
    
  }
  else
  {
    $new_name = $name;
  }

  print(NEWFILE "$new_name\n"); 
 

}
close(INFILE);
close(NEWFILE);
close(RENAME);
