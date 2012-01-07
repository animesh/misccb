#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#    Code base of Animesh Sharma [ sharma.animesh@gmail.com ]

#!/usr/bin/perl
use Bio::SeqIO;
use strict;
my $main_file_pattern=shift @ARGV;chomp $main_file_pattern;
my $other_file_pattern=shift @ARGV;chomp $other_file_pattern;

my $l_utr=500;
my $l_dutr=500;
my $sim_thresh=90;
my $n_gene_threshold=5;
my $total_seq_cnt_threshold=$n_gene_threshold;#100;

my $n_gene;
my $select_utr_no;
my $select_rutr_no;
my $total_seq_cnt;
my $other_total_seq_cnt;
my $n_other_source;
my $tfl1;
my $tfl2;
my %h_seq;
my %h_seq_utr;
my %h_seq_dutr;
my %h_seq_name;
my %total_seq;
my %total_seq_utr;
my %total_seq_dutr;
my %total_seq_name;
my %other_source_sequence;
my %other_source_sequence_name;
my @other_file;

my $total_file_name="total.fas";
my $total_file_name_utr="total.utr.fas";
my $total_file_name_dutr="total.dutr.fas";
my $other_total_file_name="other_total.fas";
my $other_total_file_name_utr="other_total.utr.fas";
my $other_total_file_name_dutr="other_total.dutr.fas";
open(FT,">temp.txt");
open(FTTL,">$total_file_name");
open(FTTLUTR,">$total_file_name_utr");
open(FTTLDUTR,">$total_file_name_dutr");
open(OFTTL,">$other_total_file_name");
open(OFTTLUTR,">$other_total_file_name_utr");
open(OFTTLDUTR,">$other_total_file_name_dutr");

system("ls -1 $main_file_pattern*.gb* > tempfile1");
open(FT1,"tempfile1");
while($tfl1=<FT1>){
    chomp $tfl1;
	my $file=select_utr($tfl1);
	print "$tfl1 Total Gene, $n_gene out of $n_gene_threshold $file\n";
}
close FT1;

system("ls -1 $other_file_pattern*.gb* > tempfile1");
open(FT2,"tempfile1");
while($tfl2=<FT2>){
    chomp $tfl2;
	push(@other_file,$tfl2);
}
close FT2;

select_utr_threshold();

close FT;
close FTTL;

sub select_utr{
    my $gb_file=shift;
    my $seqio_object = Bio::SeqIO->new(-file => $gb_file, '-format' => 'GenBank');
    my $seq_object = $seqio_object->next_seq;
    print "$gb_file\n";
    for my $feat_object ($seq_object->get_SeqFeatures) {
		if ($feat_object->primary_tag eq "CDS") { 
		#if ($feat_object->primary_tag eq "gene") { 
			my $start = $feat_object->location->start;       
			my $end = $feat_object->location->end;
			my $strand = $feat_object->location->strand;$strand+=0;
			my $seq = $feat_object->spliced_seq->seq;
			my $sequence_string = $feat_object->entire_seq->seq;
			my $seq_utr;my $seq_tag;my $seq_dutr;
			my $l_seq_complete=length($sequence_string);
			my $l_seq=length($seq);
			my $seq_name;
			my $al_utr;
			my $al_dutr;
			my @product_name=$feat_object->get_tag_values('product');
			if(@product_name[0]=~/hypothetical/){
			    next;
			}
			    for my $tag ($feat_object->get_all_tags) {
					if(($tag eq "translation") or ($tag eq "codon_start")){
						next;
					}
					else{
						for my $value ($feat_object->get_tag_values($tag)){
						$seq_name.="$value ";
						}
					}
			    }       
			if(($strand == -1) && (($end+$l_utr)<$l_seq_complete) && (($start-$l_dutr)>0)){
				$n_gene++;
				$select_rutr_no++;
			    print "R->$n_gene\t$start-$end-$l_seq_complete-$l_utr-$l_dutr [$strand]\t";
			    $seq_utr = substr($sequence_string,$end,$l_utr);
			    $seq_utr=reverse($seq_utr);
			    $seq_utr=~tr/ATGC/TACG/d;
			    $seq_dutr = substr($sequence_string,($start-$l_dutr-1),$l_dutr);
			    $seq_dutr=reverse($seq_dutr);
			    $seq_dutr=~tr/ATGC/TACG/d;
			    $al_utr=length($seq_utr);
			    $al_dutr=length($seq_dutr);
				$seq_name.="$start-$end($l_seq) $strand UTR ($al_dutr-$al_utr)";
				$seq_name="$gb_file ($select_utr_no-$n_gene)".$seq_name;
				$h_seq_name{$n_gene}=$seq_name;
				$h_seq{$n_gene}=$seq;
				$h_seq_utr{$n_gene}=$seq_utr;
				$h_seq_dutr{$n_gene}=$seq_dutr;
			}
			elsif(($strand == 1) && (($start-$l_utr)>0) && (($end+$l_dutr)<$l_seq_complete)){
				$n_gene++;
				$select_utr_no++;
			    print "F->$n_gene\t$start-$end-$l_seq_complete-$l_utr-$l_dutr [$strand]\t";
			    $seq_utr = substr($sequence_string,($start-$l_utr-1),$l_utr);
			    $al_utr=length($seq_utr);
			    $seq_dutr = substr($sequence_string,$end,$l_dutr);
			    $al_dutr=length($seq_dutr);
				$seq_name.="$start-$end($l_seq) $strand UTR ($al_utr-$al_dutr)";
				$seq_name="$gb_file ($select_utr_no-$n_gene)".$seq_name;
				$h_seq_name{$n_gene}=$seq_name;
				$h_seq{$n_gene}=$seq;
				$h_seq_utr{$n_gene}=$seq_utr;
				$h_seq_dutr{$n_gene}=$seq_dutr;
			}
			if($n_gene >= $n_gene_threshold){
				print "\nTotal Gene, $n_gene, is > then $n_gene_threshold\n";
				return($gb_file);
			}
		}
	}
    print "\n";
}



sub select_utr_threshold{
	my $seqat;
	my $sutsim;
	for(my $o=1;$o<=$n_gene_threshold;$o++){
		for(my $i=$o+1;$i<=$n_gene_threshold;$i++){
			$sutsim=seqcomp($h_seq{$o},$h_seq{$i},$h_seq_name{$o},$h_seq_name{$i});
			if($sutsim>$sim_thresh){
				$seqat++;
			}
			print "$o\t$i\t$sutsim\t$seqat\n";
		}
	}
}

sub seqcomp{
    my $o=shift;
    my $i=shift;
    my $o_n=shift;
    my $i_n=shift;
	my $length;
	my $lnoeg;
	my @tnote;
	my @t;
	my $length;
	my $per_sim;
	my $other_start;
	my $other_end;
	open(F1,">file1");
	open(F2,">file2");
	print F1">$o_n\n$o\n";
	print F2">$i_n\n$i\n";
	print "Aligning seq $o_n and seq $i_n with ";
	system("est2genome file1 file2 -outfile=file3");
	open(FN,"file3");
	while(my $line=<FN>){
		chomp $line;
		$lnoeg++;
		if(($lnoeg==1) and ($line=~/^Note/)){
			@tnote=split(/\s+/,$line);
		}
		if($line=~/^Span/){
			@t=split(/\s+/,$line);
			$length=@t[1];
			$per_sim=@t[2]+0;
			$other_start=@t[3]+0;
			$other_end=@t[4]+0;
		}
	}
	close FN;
	close F1;
	close F2;
	return($per_sim);
}

sub get_other_source{
    my $foofile=shift;
    my $foofileno=shift;
    print "$foofile File Number $foofileno\n";
    my $seqio_object = Bio::SeqIO->new(-file => $foofile, '-format' => 'GenBank');
    my $seq_object = $seqio_object->next_seq;
    for my $feat_object ($seq_object->get_SeqFeatures) {
	if ($feat_object->primary_tag eq "source") { 
			my $start = $feat_object->location->start;       
			my $end = $feat_object->location->end;
			my $sequence = $feat_object->entire_seq->seq;
			my $length_sequence=length($sequence);
			my $seq_name;
			    for my $tag ($feat_object->get_all_tags) {
				    for my $value ($feat_object->get_tag_values($tag)){
					$seq_name.="$value ";
					}
			    }       
			$n_other_source++;
			$seq_name.="$start-$end($length_sequence)";
			$seq_name="$foofile ($n_other_source)".$seq_name;
			$other_source_sequence_name{$n_other_source}=$seq_name;
			$other_source_sequence{$n_other_source}=$sequence;
		    }
    }
}


sub other_seq_comp{
    my $foofile=shift;
    my $foonumber=shift;
    print "$foofile File Number $foonumber\n";
    my $foonw=$foofile."_".$foonumber."_total_other_utr_nw.txt";
    my $foosw=$foofile."_".$foonumber."_total_other_utr_sw.txt";
    my $fooms=$foofile."_".$foonumber."_total_other_utr_ms.txt";
    my $foobs=$foofile."_".$foonumber."_total_other_utr_bs.txt";
    my $fooeg=$foofile."_".$foonumber."_total_other_utr_eg.txt";
	open(FWOUTRN,">$foonw");
	open(FWOUTRS,">$foosw");
	open(FWOUTRM,">$fooms");
	open(FWOUTRB,">$foobs");
	open(FWOUTRE,">$fooeg");
	foreach my $o (sort {$a <=> $b} keys %total_seq) {
	   foreach my $i (sort {$a <=> $b} keys %other_source_sequence){
			#if($i==$o){
				map2gen($o,$i,$foofile);
			#}
		}
	}
	close FWOUTRN;
	close FWOUTRM;
	close FWOUTRB;
	close FWOUTRS;
	close FWOUTRE;
}

sub map2gen{
    my $o=shift;
    my $i=shift;
    my $max_seq_name;
    my $max=90;
    my $seq_o=$total_seq{$o};
    my $seq_i=$other_source_sequence{$i};
	my $other_sequence_string=$seq_i;
    $seq_i=~s/\-/N/g;
    $seq_o=~s/\-/N/g;
    my $seq_o_name=$total_seq_name{$o};
    my $seq_i_name=$other_source_sequence_name{$i};
    my $seq_o_length=length($total_seq{$o});
    my $seq_i_length=length($other_source_sequence{$i});
	my $ol_seq_complete=$seq_i_length;
	open(F1,">file1");
	open(F2,">file2");
	print F1">$seq_o_name\n$seq_o\n";
	print F2">$seq_i_name\n$seq_i\n";
	print "Aligning seq $o and seq $i with ";
	system("est2genome file1 file2 -outfile=file3");
	open(FN,"file3");
	my $length;
	my $lnoeg;
	my @tnote;
	my @t;
	my $length;
	my $per_sim;
	my $other_start;
	my $other_end;
	while(my $line=<FN>){
		chomp $line;
		$lnoeg++;
		if(($lnoeg==1) and ($line=~/^Note/)){
			@tnote=split(/\s+/,$line);
		}
		if($line=~/^Span/){
			@t=split(/\s+/,$line);
			$length=@t[1];
			$per_sim=@t[2]+0;
			$other_start=@t[3]+0;
			$other_end=@t[4]+0;
		   print FWOUTRE"$o-$i $seq_o_name\t$seq_i_name\t$per_sim\t$seq_o_length\t$seq_i_length\tLength-$length\tPer-$per_sim\tS-$other_start\tE-$other_end\n$line\n";
		}
	}
	close FN;
	close F1;
	close F2;
	if($per_sim>$sim_thresh and $seq_o_length==$length){
	    if($other_total_seq_cnt>=$total_seq_cnt_threshold){die"Other Sequence count has reached $other_total_seq_cnt";}
			if((@tnote[5] eq "reversed") && (($other_end+$l_utr)<$ol_seq_complete) && (($other_start-$l_dutr)>0)){
				$other_total_seq_cnt++;
			    my $oseq = substr($other_sequence_string,($other_start-1),($other_end-$other_start+1));
			    $oseq=reverse($oseq);
			    $oseq=~tr/ATGC/TACG/d;
			    my $oseq_utr = substr($other_sequence_string,$other_end,$l_utr);
			    $oseq_utr=reverse($oseq_utr);
			    $oseq_utr=~tr/ATGC/TACG/d;
			    my $oseq_dutr = substr($other_sequence_string,($other_start-$l_dutr-1),$l_dutr);
			    $oseq_dutr=reverse($oseq_dutr);
			    $oseq_dutr=~tr/ATGC/TACG/d;
			    my $oal_utr=length($oseq_utr);
			    my $oal_dutr=length($oseq_dutr);
			    my $oal=length($oseq);
				my $oseq_name_utr.="$seq_i_name $other_start-$other_end UTR [$oal_utr]";
				my $oseq_name_dutr.="$seq_i_name $other_start-$other_end DUTR [$oal_utr]";
			    print "R->$other_total_seq_cnt\t$other_start-$other_end-$ol_seq_complete-$l_utr-$l_dutr [@tnote[5]]\t";
			    print OFTTL">$seq_i_name\t$other_start-$other_end\t$ol_seq_complete [$oal]\n$oseq\n";
			    print OFTTLUTR">$oseq_name_utr\n$oseq_utr\n";
			    print OFTTLDUTR">$oseq_name_dutr\n$oseq_dutr\n";
				print FTTLUTR">$total_seq_name{$o}\tUTR [$l_utr]\n$total_seq_utr{$o}\n";
				print FTTLDUTR">$total_seq_name{$o}\tDUTR [$l_dutr]\n$total_seq_dutr{$o}\n";
				print FTTL">$total_seq_name{$o}\n$total_seq{$o}\n";
				last;
				return;
			}
			elsif((@tnote[5] eq "forward") && (($other_start-$l_utr)>0) && (($other_end+$l_dutr)<$ol_seq_complete)){
				$other_total_seq_cnt++;
			    my $oseq = substr($other_sequence_string,($other_start-1),($other_end-$other_start+1));
			    my $oseq_utr = substr($other_sequence_string,($other_start-$l_utr-1),$l_utr);
			    my $oseq_dutr = substr($other_sequence_string,$other_end,$l_dutr);
			    my $oal_utr=length($oseq_utr);
			    my $oal_dutr=length($oseq_dutr);
			    my $oal=length($oseq);
				my $oseq_name_utr.="$seq_i_name $other_start-$other_end UTR [$oal_utr]";
				my $oseq_name_dutr.="$seq_i_name $other_start-$other_end DUTR [$oal_dutr]";
			    print "F->$other_total_seq_cnt\t$other_start-$other_end-$ol_seq_complete-$l_utr-$l_dutr [@tnote[5]]\t";
			    print OFTTL">$seq_i_name\t$other_start-$other_end\t$ol_seq_complete [$oal]\n$oseq\n";
			    print OFTTLUTR">$oseq_name_utr\n$oseq_utr\n";
			    print OFTTLDUTR">$oseq_name_dutr\n$oseq_dutr\n";
				print FTTLUTR">$total_seq_name{$o}\tUTR [$l_utr]\n$total_seq_utr{$o}\n";
				print FTTLDUTR">$total_seq_name{$o}\tDUTR [$l_dutr]\n$total_seq_dutr{$o}\n";
				print FTTL">$total_seq_name{$o}\n$total_seq{$o}\n";
				last;
				return;
			}
	}
    #return($max,$max_seq_name);
}