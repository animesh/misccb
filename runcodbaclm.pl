  use strict;
  use warnings;
  use lib '/usit/titan/u1/ash022/';
  use lib '/xanadu/home/ash022/libwww-perl-5.832/lib';
  use lib '/xanadu/home/ash022/URI-1.40';
  use LWP::Simple;
  use Parallel::ForkManager;
  #system("ls -1 *.fas > list.tmp");
  #my $command="est2genome";
  #my $genome="NC_010336.fna";
  my @tasks;
  #open(F,"list.tmp");
  #while(<F>){chomp;push(@tasks,$_);}
  my $tasksize= 16;
 for(my $c=2000;$c<=10000;$c+=$tasksize){
  print "There are #  $tasksize from $c \n";
  my $pm = new Parallel::ForkManager($tasksize);
   for(my $task=$c;$task<$c+$tasksize;$task++) {
    $pm->start and next; 
    system("/xanadu/project/codgenome/wgs/dCODBACWGS/0-overlaptrim-overlap/overlap.sh $task");
    $pm->finish; 
  }
  $pm->wait_all_children;
}

