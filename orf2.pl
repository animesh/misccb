#!/usr/bin/perl

use Bio::SeqIO;
$ff=shift@ARGV;
sub ORF {
   $seqs=shift;
   $snames=shift;
   $rseqs=reverse($seqs);
   $strand{"\+"} = "$seqs";
   $strand{"\-"} = "$rseqs";
   #foreach (keys %strand) {print "$_ =>\n$strand{$_}\n";}
   foreach $direction (keys %strand) {$len=length($strand{$direction});
         for ( $frame=1;$frame<=3;$frame++) {
	$seqf{$frame}=substr($strand{$direction},$frame,$len);
	foreach (keys %seqf) {print "$_ $direction=>\n$strand{$direction}\n";}
         }
      while ($strand{$direction}=~m/(atg)/gi) {
         push @starts,pos($strand{$direction})-3;
      }

      while ($strand{$direction}=~m/(taa|tga|tag)/gi) {
         push @ends,pos($strand{$direction})-3;
      }
      push @ends,($dna->length-2,$dna->length-1,$dna->length);

      for my $s (@starts) {
         for my $e (@ends) {
            if ($e%3==$s%3 and $e>$s) {
   #            if ($e-$s>$best) {
                  $best=$e-$s;
                  ($bests,$beste,$beststrand)=($s,$e,$direction);
                  $bestorf=substr($seqc,$best,$s);
		     ($length,$start,$end,$direction,$sequence)=($best,$s,$e,$direction,$bestorf);
		     #print $dna->display_id," ",$dna->desc,": ";
                     print "$length, $start, $end ($direction)\n$sequence\n\n",Bio::Seq->new(-seq=>$sequence)->translate->seq,"\n\n--\n\n";

 #              }
               #last
            } else 	{
               next
            		}
         }
      }
   }

}
open(F2,$ff);
while ($line = <F2>) {
        chomp ($line);
        if ($line =~ /^>/){
             push(@seqname,$line);
                if ($seq ne ""){
              push(@seq,$seq);
              $seq = "";
            }
      } else {$seq=$seq.$line;
      }
}
push(@seq,$seq);
for($c1=0;$c1<=$#seq;$c1++){
	        @temp2=split(/\s+/,@seqname[$c1]);$seqc=uc(@seq[$c1]);
	        $t2=@temp2[0];
		ORF($seqc,$seqname);
		}
close F2;
