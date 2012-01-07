
#!  usr/bin/perl
 print"enter the sequence\n";
 $file=<STDIN>;
 chomp($file);
my($starttime)=times();

use Math::Complex;
  $iota=sqrt(-1);
  $pi=3.14;
   $f=1/3;
$y =  exp((-$iota)*2*$pi*$f);
 for($j=1;$j<=300;$j++)
 {
 $e[$j]=($y**$j );
 }
#  print "What is the filename containing the sequence? ";
# $name = <STDIN>;
#chomp($name);
#print "The sequence filename is $name \n";

open (FILENAME, $file) || die "can't open $name: $!";
#open (OUT, ">out");
while ($line = <FILENAME>)
    {
    chomp($line);
    if ($line =~ /^>/)
        {
        $line =~ s/>//;
        $seqname = $line;
        }
    else {
        $seq = $seq.$line;
        }
    }

@base=('A','T','G','C') ;

$N=length($seq);
 print"MAXIMUM LENGTH OF THE SEQUENCE :$N\n\n";

print"k:\t\t\t\t\t p:\n";

           $winsize=300;
           $winsq=(1/$winsize+(1/$winsize**2));
           $wincube= ($winsize**3) ;
           @seq=split(//,$seq);
             $flag=0;  $i=0;

           for($k=1;$k<=$N-$winsize;$k++)
             {


             if($k==1)
             {
                 @window=@seq[1..$winsize];
                     $seq1= join "",@window;

                    foreach $b (@base){
                     $n{$b}=$seq1=~s/$b//g;

                     }
             }else{
               $oldres=shift(@window);
               $newres=$seq[$k+$winsize];
               push(@window,$newres);
               $n{$oldres}--;
               $n{$newres}++;
               }
               $sq=0;
               $s2=0;
             foreach $var('A','T','G','C'){

                  $sq=$sq+($n{$var}**2);
                     $s1 = 0;
                     $j=0;
                    foreach $base(@window)
                   {


                  $j++;
                      if ($base eq $var)
                      {
                        $s1=$s1+$e[$j];                                               	

                      } }
                     $s2=($s2+(abs$s1)**2); }

                      #to calculate $sbar
                                $sq=$sq/$wincube;
                               $sbar=($winsq-$sq);
                                $sq=$sbar*($wincube)/2  ;
                              #to calculate signal to noise ratio
                              $s2=$s2/$sq;
                              $p=($s2/$sbar);
                                             $s[$i]=$k;
                                             $x[$i]=$p;
                                             $i++;

                                       }
                                    $num_of_entries=$i;

                                   for($i=0;$i<$num_of_entries;$i++)
                                   {


                                    if($x[$i]>4)
                                      {
                                         if($flag==0)
                                           {
                                               print "\n\n\nSTART: $s[$i]\n";
                                               @start=(@start,$s[$i]);
                                               $flag=1;
                                           }
                                       }
                                      else
                                      {
                                          if($flag==1)
                                          {
                                              if($x[$i]<4)
                                                   {
                                                         print "\t\tEND: $s[$i-1]\n" ;
                                                         @end=(@end,$s[$i-1]);
                                                        $flag=0;
                                                    }
                                           }
                                       } # End else

                                     $i++;
                                     }#end for
                                       my $endtime=times();
                                         $progtime=$endtime-$starttime;

                                         print"time:$progtime\n";






























































