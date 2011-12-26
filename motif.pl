#!/usr/bin/perl
use strict;
my $main_file=shift @ARGV;
my $seq_i=shift @ARGV;
$seq_i=uc($seq_i);
my $per_win=20;
my @gseq;
my @gseqname;
my $fot;
my $fas_file=$main_file.".pairedread.fasta";
open(FT,">$fas_file");

get_other_source($main_file);
my $basec=4;
my $sno=0;

my($start_motif,$length_motif,$length_motif);

#while($sno<$coverage){
for($fot=0;$fot<=$#gseq;$fot++){
	my $slname=@gseqname[$fot];
	my $seq_o=@gseq[$fot];
	my $wseqlen=length($gseq[$fot]);
	my $slnamews=$slname;$slnamews=~s/\s+/\./g;
	my $name2use=$slnamews;
            while ($seq_o =~ /$seq_i/g) {
                my $posi= ((pos $seq_o) - length($&) +1);
                my $start_posi=$posi-$start_motif-1;
                my $moti = substr($seq_o,$start_posi,$length_motif);
                my $len=length($moti);
                (pos $seq_o)=(pos $seq_o)-length($&) +1;
                print ">Pos in Seq $slname - $posi - $start_posi ($length_motif)\t$moti\n$seq_i\n";
	}
}
#}

sub checkfile{


}

close FT;

sub gaussian_rand {
    my ($u1, $u2);  # uniformly distributed random numbers
    my $w;          # variance, then a weight
    my ($g1, $g2);  # gaussian-distributed numbers

    do {
        $u1 = 2 * rand() - 1;
        $u2 = 2 * rand() - 1;
        $w = $u1*$u1 + $u2*$u2;
    } while ( $w >= 1 );

    $w = sqrt( (-2 * log($w))  / $w );
    $g2 = $u1 * $w;
    $g1 = $u2 * $w;
         return wantarray ? ($g1, $g2) : $g1;
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
	push(@gseq,$seq);
	$seq="";
	close FO;
	my $noseq=length(@gseq);
}


