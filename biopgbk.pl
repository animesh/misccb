#!/usr/bin/perl
use Bio::SeqIO;
use strict;
my $gb_file="CP258.gb";
my $l_utr=500;
my $sim_thresh=80;
#$gb_file="CP258m.gb";
my $seqio_object = Bio::SeqIO->new(-file => $gb_file);
my $seq_object = $seqio_object->next_seq;
#eval { my $seq_object = $seqio_object->next_seq; };       
      # if there's an error       
#print "Problem in $gb_file. Bad feature perhaps?\n" if $@;
my $n_gene=0;
my %h_seq_name;
my %h_seq;
my %h_seq_utr;
for my $feat_object ($seq_object->get_SeqFeatures) {
    #if ($feat_object->primary_tag eq "CDS") { 
    if ($feat_object->primary_tag eq "gene") { 
			my $start = $feat_object->location->start;       
			my $end = $feat_object->location->end;
			my $strand = $feat_object->location->strand;$strand+=0;
			my $seq = $feat_object->spliced_seq->seq;
			my $sequence_string = $feat_object->entire_seq->seq;
			my $seq_utr;my $seq_tag;
			my $l_seq_complete=length($sequence_string);
			my $l_seq=length($seq);
			my $seq_name;
			my $al_utr;
			#print $feat_object->spliced_seq->seq,"\n";
			$n_gene++;
			if(($strand == -1) && (($end+$l_utr)<$l_seq_complete)){
			    print "R->$n_gene\t";
			    for my $tag ($feat_object->get_all_tags) {
				$seq_name.="$tag:";
				for my $value ($feat_object->get_tag_values($tag)){
				    $seq_name.="$value\t";
				}          
			    }       
			    $seq_utr = substr($sequence_string,$end,$l_utr);
			    $seq_utr=reverse($seq_utr);
			    $seq_utr=~tr/ATGC/TACG/d;
			    $al_utr=length($seq_utr);
			    $seq_name.="S-$start E-$end L-$l_seq D-$strand\tUTR\tL-$al_utr";
			    $h_seq_name{$n_gene}=$seq_name;
			    $h_seq{$n_gene}=$seq;
			    $h_seq_utr{$n_gene}=$seq_utr;
			}
			elsif(($strand == 1) && (($start-$l_utr)>0) ){
			    print "F->$n_gene\t";
			    for my $tag ($feat_object->get_all_tags) {
				$seq_name.="$tag:";
				for my $value ($feat_object->get_tag_values($tag)){
				    $seq_name.="$value\t";
				}          
			    }       
			    $seq_utr = substr($sequence_string,($start-$l_utr-1),$l_utr);
			    $al_utr=length($seq_utr);
			    $seq_name.="S-$start E-$end L-$l_seq D-$strand\tUTR\tL-$al_utr";
			    $h_seq_name{$n_gene}=$seq_name;
			    $h_seq{$n_gene}=$seq;
			    $h_seq_utr{$n_gene}=$seq_utr;
			}
         }
}
print "\n";
my @t=split(/\./,$gb_file);
my $gb_file_out=@t[0].".utr.txt";
open(FW,">$gb_file_out");
open(FT,">temp.txt");
foreach my $o (sort {$a <=> $b} keys %h_seq_name) {
    my $per_sim; my $max;my $max_seq_name;
    foreach my $i (sort {$a <=> $b} keys %h_seq_name){
	if($i<$o){
	    open(F1,">file1");
	    open(F2,">file2");
	    print F1">$h_seq_name{$i}\n$h_seq{$i}\n";
	    print F2">$h_seq_name{$o}\n$h_seq{$o}\n";
	    print "Aligning seq $o and seq $i with ";
	    system("needle file1 file2 -gapopen=10 -gapext=0.5 -outfile=file3");
	    open(FN,"file3");
	    while(my $line=<FN>){
		if($line=~/^# Identity:     /){
		   @t=split(/\(|\)/,$line);
		   @t[1]=~s/\%|\s+//g;
		   $per_sim=@t[1]+0;
		   if($max<$per_sim){
		       $max=$per_sim;
		       $max_seq_name=$i;
		   }
		   print FT"$i-$o\t$per_sim\n";
		}
	    }
	    close FN;
	    close F1;
	    close F2;
	}
	if($max>$sim_thresh){
	    last;
	}
    }
    print FT"\n$o - $max_seq_name - $max - $per_sim - $sim_thresh\n";
    if($max<$sim_thresh){
	print FT"\t$o-$max_seq_name-$max-$per_sim\n";
	print FW">$o()$h_seq_name{$o}\n$h_seq_utr{$o}\n";
	next;
    }
}
close FW;
close FT;

#my @cds_features = grep { $_->primary_tag eq 'CDS' } Bio::SeqIO->new(-file => $gb_file)->next_seq->get_SeqFeatures;
#my %gene_sequences = map {$_->get_tag_values('gene'), $_->spliced_seq->seq } @cds_features;

