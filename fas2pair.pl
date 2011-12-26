#!/usr/bin/perl
# fas2pair.pl     sharma.animesh@gmail.com     2009/03/22 01:04:42
#Converts format >codbac-190o01.fb140_b1.SCF length=577 sp3=clipped to >DJS045A03F template=DJS054A03 dir=F library=DJS045
my $lthreshmax=15000;
my $lthreshmin=10000;
my $tcnt=1000;
my $seqlen=650;
my $tes=10;
my $filein=shift @ARGV;
open(F2,$filein);
        
while ($line = <F2>) {
                chomp ($line);
                if ($line =~ /^>/){
                        $snames=$line;
                        chomp $snames;
                     push(@seqname,$snames);
                        if ($seq ne ""){
                      push(@seq,$seq);
                      $seq = "";
                    }
              } else {$seq=$seq.$line;
              }
}push(@seq,$seq);
$seq="";
close F2;
open(FT,">$filein.artbac.txt");
while($tcnt>0){
	for($c=0;$c<=$#seq;$c++){
		$len=length(@seq[$c]);
		if($len>$lthreshmax && $tcnt>0){
		 for($ccc=0;$ccc<$tes;$ccc++){
		   $cnt++;
			$playlen=$len-$lthreshmin-$seqlen;
			$fpos=int(rand(1)*$playlen);
			$rpos=$fpos+$lthreshmin;
		   my @tmp=split(/\s+/,@seqname[$c]);
		   my $name=@seqname[$c];
		   $name=~s/\>|\s+//g;
		   my $namesubstr=@tmp[0];
		   $namesubstr=~s/\>|\s+//g;
		   my $template="$namesubstr-$fpos-$rpos";
		   my $dirf="F";
		   my $dirr="R";
		   my $libstring=$namesubstr;
		   $fseq=substr($seq[$c],$fpos-1,$seqlen);
		   $rseq=substr($seq[$c],$rpos-1,$seqlen);
		   print FT">$name\ttemplate=$template\tdir=$dirf\tlibrary=$libstring\n$fseq\n";    
		   print FT">$name\ttemplate=$template\tdir=$dirr\tlibrary=$libstring\n$rseq\n";   
			print "Wrote $cnt from $name\ttemplate=$template\tdir=$dirf\tlibrary=$libstring\n"; 
		   }
			$tcnt-=$tes;
		 }
		else{next}
	}
}
close FT;
