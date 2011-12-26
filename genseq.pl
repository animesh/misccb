@b=qw/A T G C N/;
$c=0;
$f=shift @ARGV;
$l=shift @ARGV;
$p=5;
open(FG,">$f.genome.fna");
print FG">Genome$l$f\n";
while($c<$l){
	$c++;
	$g.=$b[int(rand(5))];
}
print FG"$g\n";
close FG;
$c=0;
open(FCL,">$f.clc.fna");
open(FCE,">$f.cel.fna");
open(FNE,">$f.new.fna");
open(FEN,">$f.ens.fna");
open(FASM,">$f.asm.fna");
print FASM"CLC\t$f.clc.fna\t4\n";
print FASM"CEL\t$f.cel.fna\t3\n";
print FASM"NEW\t$f.new.fna\t2\n";
print FASM"ENS\t$f.ens.fna\t1";
close FASM;
while($c<$l-($l/$p)){
print "$c\t$p\t$l\t",$l/$p,"\n";
$s=rand();
if($s>0.5){
	$s1=substr($g,$c,$l/$p);
	$s2=substr($g,$c+int($l/4),$l/$p);
	$s3=substr($g,$c+int($l/2),$l/$p);
	$s4=substr($g,$c+int(3*$l/4),$l/$p);
	print FCL">CLCFS$c \n$s1\n";
	print FCE">CELFS$c \n$s2\n";
	print FNE">NEWFS$c \n$s3\n";
	print FEN">ENSFS$c \n$s4\n";
#>codbac-190o01.fb140_b1.SCF template=codbac-190o01 dir=F library=codbac-140 trim
	}
else{
        $s1=substr($g,$c,$l/$p);
        $s2=substr($g,$c+int($l/4),$l/$p);
        $s3=substr($g,$c+int($l/2),$l/$p);
        $s4=substr($g,$c+int(3*$l/4),$l/$p);
        $s1=~tr/ATGC/TACG/;
        $s1=reverse($s1);
        $s2=~tr/ATGC/TACG/;
        $s2=reverse($s2);
        $s3=~tr/ATGC/TACG/;
        $s3=reverse($s3);
	$s4=~tr/ATGC/TACG/;
        $s4=reverse($s4);
        print FEN">ENSS$c \n$s1\n";
        print FNE">NEWRS$c \n$s2\n";
        print FCE">CELRS$c \n$s3\n";
        print FCL">CLCRS$c \n$s4\n";

}
$c+=($l/$p);
}

