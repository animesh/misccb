#!/usr/bin/perl
#    Code base of Animesh Sharma [ sharma.animesh@gmail.com ]

use strict;

my $main_file_pattern=shift @ARGV;chomp $main_file_pattern;
my $other_file_pattern=shift @ARGV;chomp $other_file_pattern;

my $line;
my $seq;my @seq;
my @gseq;
my $seqname;my @seqname;my $snames;
my @gseqname;



open(F,$main_file_pattern)||die "can't open";
while ($line = <F>) {
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
close F;


open(FO,$other_file_pattern)||die "can't open";
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
      } else {$seq=$seq.$line;
      }
}push(@gseq,$seq);
$seq="";
close FO;

my $w;my %m;my $fot;my $t;my $fott;
my $fresall=$main_file_pattern.$other_file_pattern.".resall.txt";
open(FRA,">$fresall");



for($fott=0;$fott<=$#gseq;$fott++){
for($fot=0;$fot<=$#seq;$fot++){
@seqname[$fot]=~s/\s+/\_/g;
@gseqname[$fott]=~s/\s+/\_/g;
@seqname[$fot]=~s/\>//g;
@gseqname[$fott]=~s/\>//g;
my ($per_sim_res,$length_res,$other_start_res,$other_end_res,$dir,$onam)=seqcomp(@seq[$fot],@gseq[$fott],@seqname[$fot],@gseqname[$fott],$fot,$fott);
my $l=$per_sim_res;
$m{$l}+=1;
print FRA"@seqname[$fot]\t@gseqname[$fott]\t$per_sim_res\t$length_res\t$other_start_res\t$other_end_res\t$dir\t$onam\n";

}
}

close FRA;

my $fres=$main_file_pattern.$other_file_pattern.".res.txt";
open(FR,">$fres");


foreach $w (sort {$a<=>$b} keys %m){print FR"$w\t$m{$w}\n";$t+=$m{$w};}


sub seqcomp{
    my $o=shift;
    my $i=shift;
    my $o_n=shift;
    my $i_n=shift;
    my $on=shift;
    my $in=shift;
	my $length;
	my $lnoeg;
	my @tnote;
	my @t;
	my $length;
	my $per_sim;
	my $other_start;
	my $other_end;
	my $other_name;
	open(F1,">file1");
	open(F2,">file2");
	print F1">$o_n\n$o\n";
	print F2">$i_n\n$i\n";
	my $fileout="$o_n.$i_n.$on.$in.txt";
	print "Aligning seq $o_n and seq $i_n in $fileout with ";
	system("est2genome file1 file2 -outfile=$fileout");
	open(FN,$fileout);
	while(my $line=<FN>){
		chomp $line;
		$lnoeg++;
		if(($lnoeg==1) and ($line=~/^Note/)){
			@tnote=split(/\s+/,$line);
		}
		if($line=~/^Span/){
			@t=split(/\s+/,$line);
			$length=@t[7]-@t[6]+1;
			$per_sim=@t[2]+0;
			$other_start=@t[3]+0;
			$other_end=@t[4]+0;
			$other_name=@t[8];
		}
	}
	close FN;
	close F1;
	close F2;
	return($per_sim,$length,$other_start,$other_end,@tnote[5],$other_name);
}


