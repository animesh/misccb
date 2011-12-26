#!/usr/bin/perl
use strict;
my $main_file=shift @ARGV;
my $length=shift @ARGV;
my $window=shift @ARGV;;


my @gseq;
my @gseqname;
my $fot;
my $fas_file=$main_file.".L.$length.WS$window.read.fasta";
open(FT,">$fas_file");

get_other_source($main_file);

for($fot=0;$fot<=$#gseq;$fot++){
	my $slname=@gseqname[$fot];
	my $slseq=@gseq[$fot];
	my $wseqlen=length($gseq[$fot]);
	my $sno;
	my $totalcnt=int(($wseqlen-$length)/$window)+1;
	my $slnamews=$slname;$slnamews=~s/^>//;
	print "SL-$wseqlen SN-$slname TC-$totalcnt\n";
	for(my $c=0;$c<=$wseqlen-$window;$c+=$window){
   		$sno++;
 	       	my $start=$c;
		my $seqgen=substr($slseq,$start,$length);
		#if ($slseq =~ /$seqgen/) {
        	print FT">S.$sno.$start\t$length.$window.$wseqlen.$slnamews\n";
    		#print FT"$-[0]-$+[0]\n",
     		print FT "$seqgen\n";
		#}
	}
}
close FT;


sub get_other_source{
	my $other_file_pattern=shift;
	my $line;
	open(FO,$other_file_pattern)||die "can't open";
	my $seq;
	my $snames;
	while ($line = <FO>) {
        	chomp ($line);
       	if ($line =~ /^>/){
		$snames=$line;
		chomp $snames;
             push(@gseqname,$snames);
                	if ($seq ne ""){
              		push(@gseq,$seq);
              		$seq = "";
            	}
      	} 
		else {
			$seq=$seq.$line;
      	}
	}
	push(@gseq,$seq);
	$seq="";
	close FO;
	my $noseq=length(@gseq);
}


