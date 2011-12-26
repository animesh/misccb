#!/usr/bin/perl
# fas2pair.pl     sharma.animesh@gmail.com     2009/03/22 01:04:42
#@SOLEXA16:0008:2:1:993:17470#0/2
#>F0008_993_17470        template=0008_993_17470 dir=F   library=0008
#>PCUS-319-EAS487:1:1:1:0:507#0/1
#>F1_0_507       template=1_0_507        dir=F   library=1
#>PCUS-319-EAS487:1:1:32:1789:1450#0/2
#>PCUS-319-EAS487:1:1:80:1789:1450#0/2
while ($line = <>) {
                chomp ($line);
                @tmp=split(/\:|\#|\//,$line);
                if ($line =~ /^>/){
                   $libstring="L@tmp[1]_@tmp[2]_@tmp[3]";
                   $template="T".$libstring."_@tmp[4]_@tmp[5]";
                   if($line=~/1$/){$dir="F";$name="F".$template}
                   else{$dir="R";$name="R".$template}
                   print ">$name\ttemplate=$template\tdir=$dir\tlibrary=$libstring\n";
                 } 
                 else {print "$line\n";}
}

