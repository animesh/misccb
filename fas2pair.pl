#!/usr/bin/perl
use strict;
my $main_file=shift @ARGV;
my @gseq;
my @gseqname;
my $fot;
my $time=time;
my $fas_file=$main_file.".$time.pairedread.fasta";
open(FT,">$fas_file");

get_other_source($main_file);

my $sno=0;
my $basec=2;
my $per_win=shift @ARGV;

for($fot=0;$fot<=$#gseq;$fot++){
	my $slname=@gseqname[$fot];
	my $slseq=@gseq[$fot];
	my $wseqlen=length($gseq[$fot]);
	my $slnamews=$slname;$slnamews=~s/\s+/\./g;
	my $name2use=$slnamews;
	$name2use=~s/\s+|\_|\>//g;
	$name2use=$name2use.$per_win.$wseqlen;
	for(my $c=0;$c<$basec;$c++){
	if($wseqlen>2*$per_win){
   		$sno++;
		#my $chlen=$randy[int(rand(3))];
		my $fq=int($wseqlen/4);
		my $lq=int($wseqlen*3/4);
		my $chlen=$lq-$fq;
		#my $p=int(rand($fq));
		my $p=0;
 	       	my $p1=$p;
        	my $p2=$wseqlen-$per_win;
		my $mf=$main_file;
		$mf=~s/\.fna//;
		my $timecnt=time;
		my $template=$name2use."-".$timecnt.$sno;
		my $nameofseqF=$template."F";
		#>DJS045A03F template=DJS054A03 dir=F library=DJS045 trim=12-543
		print FT">$nameofseqF template=$template dir=F library=$name2use\n";
		print "SL-$wseqlen SN-$slname\n";
     		print FT substr($slseq,$p1+$c*$per_win,$per_win),"\n";
		my $nameofseqR=$template."R";
		print FT ">$nameofseqR template=$template dir=R library=$name2use\n";
		my $revstr=substr($slseq,$p2-$c*$per_win,$per_win);
		$revstr=reverse($revstr);
		$revstr=~tr/ATGCN/TACGN/;
     		print FT $revstr,"\n";
	}
	}
}


sub get_other_source{
	my $other_file_pattern=shift;
	my $line;
	open(FO,$other_file_pattern)||die "can't open";
	my $seq;
	my $snames;
	while ($line = <FO>) {
        	chomp ($line);
		$line=~s///g;
       	if ($line =~ /^>/){
		$snames=$line;
		chomp $snames;
             push(@gseqname,$snames);
                	if ($seq ne ""){
              		push(@gseq,uc($seq));
              		$seq = "";
            	}
      	} 
		else {
			$seq=$seq.$line;
      	}
	}
	push(@gseq,uc($seq));
	$seq="";
	close FO;
	my $noseq=length(@gseq);
}


