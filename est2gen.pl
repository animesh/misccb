  use strict;
  use warnings;
  use lib '/home/animesh/export/pfm/lib/';
  use lib '/home/animesh/export/lwp/lib';
  use LWP::Simple;
  use Parallel::ForkManager;
  #system("ls -1 *.fasta > list.tmp");
 
  my $command="/home/animesh/export/EMBOSS-6.3.1/emboss/est2genome";
  my $genome= shift @ARGV; chomp $genome;
  my @tasks;
  open(F,"list.tmp");
  while(<F>){chomp;push(@tasks,$_);}
  close F;  
  my $tasksize= @tasks;
  print "There are #  $tasksize \n";
  my $pm = new Parallel::ForkManager($tasksize); 
  foreach my $task (@tasks) {
    $pm->start and next; 
    system("$command $task $genome $task.$genome.out");
    $pm->finish; 
  }
  $pm->wait_all_children;


