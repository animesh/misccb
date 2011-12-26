#!/usr/bin/perl
print "enter name of multiple sequences containing file in FASTA format : \n";
$file=<STDIN>;
chomp $file;   $least=1;
$seq="";
open (F,$file)||die "cant open  :$!";
open (FF,">tabb.txt");
while ($line = <F>)
{
        chomp ($line);
        if ($line =~ /^>/)
        {
            $line =~ s/>//;
            push(@seqname,$line);
            if ($seq ne "")
            {
              push(@seq,$seq);
              $seq = "";
            }
        }
        else
        {
            $seq=$seq.$line;
        }
}
push(@seq,$seq);
print FF"\t";
for($c=0;$c<$#seq;$c++){
	{for($cc=0;$cc<=$#seq;$cc++){
		if(@seq[$c] eq @seq[$cc]){last;}{
			$cont=alignment(@seq[$c],@seq[$cc]);print FF"$cont\t";
			 }
		}

#foreach $y (@seq){foreach $yy (@seq){if($y ne $yy){$cont=alignment($y,$yy);push(@contig,$cont);}}}
#foreach $ww(@contig){print "$ww\n";}
open f1,"tabb.txt" || die "can't open tabb.txt";
$lc=0;
while($line=<f1>)
{
chomp $line;
push(@seq,$line);
}
$lc=@seq;
#rint $lc;
for($lii=0;$lii<$lc;$lii++){$li=@seq[$lii];
$li =~ s/\s+/\t/g;
$li =~ s/\t//;
@elem=split(/\t/,$li);
$len=@elem;
	{for ($cc=0;$cc<$len;$cc++)
	{$tree[$lii][$cc]=@elem[$cc];
	}

}
#print "$tree[0][5]\t";
$row=1;$col=1;
$min=$tree[0][1];
for($lii=0;$lii<$lc;$lii++){#print "\n";
	for ($cc=0;$cc<$len;$cc++)
	{
                if($tree[$lii][$cc] ne "0" and $tree[$lii][$cc] <= $min){
                $min=$tree[$lii][$cc];$row=($lii+1);$col=($cc+1);
                }
	}

}
print "$row\t$col\n$min\n";
sub alignment {
$gap=-2;
$match=2;
$counter=0; @sequencecol=();@sequencerow=(); @point1=();@fscore1=(); @mysymbol=();
$point;
$foutputmat="";
$foutputrow="";
$foutputcol="";
$outputrow="";
$outputcol="";
$outputmat="";
$sequence=shift;
@sequencerow=split(//,$sequence);      #split each element and store in array
unshift(@sequencerow,0);                    #add 0 as first element
$sequence1=shift;
@sequencecol=split(//,$sequence1);
unshift(@sequencecol,0);
for ($row=0;$row<=$#sequencerow;$row++)        {
              for ($column=0;$column<=$#sequencecol;$column++){
                              if ($row==0 )
                                    {
                                            $fscore1[$row][$column]=$gap*$column;
                                             $point1[$row][$column]="h";
                                             #print "\t $fscore1[$row][$column]  ";
                                    }
                              else{
                                            if ($row>0 and $column==0)
                                                 {
                                                         $fscore1[$row][$column]=$gap*$row;
                                                         $point1[$row][$column]="v";
                                                    #     print "\t $fscore1[$row][$column]  ";

                                                 }
                                            else                                                                   #calculating values for the matching score for i,j
                                                {
                                                 if (($sequencecol[$column]) eq ($sequencerow[$row]))
                                                         {
                                                                 $score=$match;
                                                                 $mysymbol[$row][$column]="|";
                                                         }

                                                  else
                                                          {
                                                                                 $score=0;
                                                                                 $mysymbol[$row][$column]="X";
                                                           }

                                                 $scoreij=$score+$fscore1[$row-1][$column-1];                #calculating the final score for each cell and storing the pointer
                                                 if ($scoreij>(($fscore1[$row-1][$column])+$gap))
                                                         {
                                                                  $fscore=$scoreij;
                                                                  $point='d';
                                                         }
                                                 else
                                                         {
                                                                  $fscore=($fscore1[$row-1][$column])+$gap;
                                                                  $point="v";
                                                         }
                                                 if ($fscore>(($fscore1[$row][$column-1])+$gap))
                                                          {
                                                                 $fscore1[$row][$column]=$fscore;
                                                                 $point1[$row][$column]=$point;
                                                          }
                                                 else
                                                          {
                                                                 $fscore1[$row][$column]=($fscore1[$row][$column-  1])+$gap;
                                                                 $point1[$row][$column]="h";
                                                          }

                                             #    print "\t $fscore1[$row][$column]  ";

                                                 #print "$point1[$row][$column]  ";
                                          }
                                    }
                  }
               # print "\n";
               }


# TRACEBACK
$row--,$column--;
$scorealignment=$fscore1[$row][$column];
while ($row!=0 or $column!=0)
    {

       if ($point1[$row][$column] eq "d")
                 {
                         $outputrow=$outputrow.$sequencerow[$row];
                         $outputcol=$outputcol.$sequencecol[$column];
                         $outputmat=$outputmat.$mysymbol[$row][$column];
                         $row--;$column--;
                 }
       else
                 {
                         if ( $point1[$row][$column] eq "h")
                                 {
                                          $outputrow=$outputrow."-";
                                          $outputcol=$outputcol.$sequencecol[$column];
                                          $outputmat=$outputmat."X";
                                          $column--;
                                  }
                         else
                                  {
                                         $outputrow=$outputrow.$sequencerow[$row];
                                         $outputcol=$outputcol."-";
                                         $outputmat=$outputmat."X";
                                         $row--;
                                  }
                  }
         }
$foutputrow=uc(reverse($outputrow));
$foutputcol=uc(reverse($outputcol));
$foutputmat=reverse($outputmat);
$counter= $foutputmat =~ s/X/X/g;
return $counter;
}