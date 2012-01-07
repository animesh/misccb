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

head	1.6;
access;
symbols;
locks
	krishna_bhakt:1.6; strict;
comment	@# @;


1.6
date	2006.09.03.15.18.50;	author krishna_bhakt;	state Exp;
branches;
next	1.5;

1.5
date	2006.09.03.15.18.15;	author krishna_bhakt;	state Exp;
branches;
next	1.4;

1.4
date	2006.09.03.15.17.43;	author krishna_bhakt;	state Exp;
branches;
next	1.3;

1.3
date	2006.09.03.15.17.15;	author krishna_bhakt;	state Exp;
branches;
next	1.2;

1.2
date	2006.09.03.15.16.13;	author krishna_bhakt;	state Exp;
branches;
next	1.1;

1.1
date	2006.09.03.15.14.49;	author krishna_bhakt;	state Exp;
branches;
next	;


desc
@@


1.6
log
@version 10
@
text
@#!/usr/bin/perl
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
#print "Problem in $gb_file. Bad feature perhaps?\n" if $@@;
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
my @@t=split(/\./,$gb_file);
my $gb_file_out=@@t[0].".utr.txt";
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
		   @@t=split(/\(|\)/,$line);
		   @@t[1]=~s/\%|\s+//g;
		   $per_sim=@@t[1]+0;
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

#my @@cds_features = grep { $_->primary_tag eq 'CDS' } Bio::SeqIO->new(-file => $gb_file)->next_seq->get_SeqFeatures;
#my %gene_sequences = map {$_->get_tag_values('gene'), $_->spliced_seq->seq } @@cds_features;

@


1.5
log
@version 9
@
text
@d4 1
a4 1
my $gb_file="CP258m.gb";
d6 2
a7 2
my $sim_thresh=50;
#$gb_file="D:\\animesh\\projects\\research\\ram_cd\\h37rv.gbk";
d18 2
a19 2
    #if ($feat_object->primary_tag eq "CDS") { # for H37Rv - 3918, 3916 for the inner loop
    if ($feat_object->primary_tag eq "gene") { # for H37Rv - 3922
a23 1
			#print $feat_object->spliced_seq->seq,"\n";
a24 2
			#my $complement_seq_string = reverse($sequence_string);
			#$complement_seq_string =~ t/ATGC/TACG/g;
d28 3
a32 1
			    my $seq_name;
a35 1
				#print "Tag: ", $tag, "\t";             
a37 1
				    #print "Value: ", $value, "\t";             
d43 1
a43 1
			    my $al_utr=length($seq_utr);
a44 1
			    #print "S-$start\tE-$end\tL-$l_seq\tD-$strand\n$seq\n>UTR\tL-$al_utr ($l_utr)\n$seq_utr\n";
a49 1
			    my $seq_name;
a52 1
				#print "Tag: ", $tag, "\t";             
a54 1
				    #print "Value: ", $value, "\t";             
d58 1
a58 1
			    my $al_utr=length($seq_utr);
a59 1
			    #print "S-$start\tE-$end\tL-$l_seq\tD-$strand\n$seq\n>UTR\tL-$al_utr ($l_utr)\n$seq_utr\n";
a63 10
			#$al_utr=length($seq_utr);
			#print ">S-$start\tE-$end\tL-$l_seq\tD-$strand\n$seq\n>UTR\tL-$al_utr ($l_utr)\n$seq_utr\n";

			# e.g. 'ATTATTTTCGCTCGCTTCTCGCGCTTTTTGAGATAAGGTCGCGT...'
			#if ($feat_object->has_tag('gene')) {
             #for my $val ($feat_object->get_tag_values('gene')){
               #print "gene: ",$val,"\n";
               # e.g. 'NDP', from a line like '/gene="NDP"'
             #}
           #}
d66 1
d71 2
a72 1
foreach my $o (sort {$a <=> $b} keys %h_seq_name) {my $per_sim; my $max;
d79 1
d89 1
a91 8
		   #if((@@t[1]+0)>$sim_thresh){
		   #    print "@@t[1]\n";
		       #print FW">$h_seq_name{$o}\n$h_seq_utr{$o}\n";
		       #last;
		   #}
		   #else{
		       #next;
		   #}
a100 1

d102 1
d104 1
a104 1
	print "\n$o-$max-$per_sim\n";
d110 2
a112 1

a114 7
#There are GENE with no CDS in H37Rv.gbk, for eg, 
#< gene: Rv0164
#< gene: Rv1395
#< gene: Rv1792
#< gene: Rv2160c
#< gene: Rv3128c
#< gene: lhr  ???
@


1.4
log
@version 8
@
text
@d126 1
a126 1
	print "$max-$per_sim\n";
@


1.3
log
@version 6
@
text
@d3 2
a4 1
my $gb_file="CP258.gb";
d6 1
d14 3
d29 2
a30 2
			$l_seq_complete=length($sequence_string);
			$l_seq=length($seq);
d33 5
a37 3
			    print ">$n_gene\t";
			    for my $tag ($feat_object->get_all_tags) {             
				print "Tag: ", $tag, "\t";             
d39 2
a40 1
				    print "Value: ", $value, "\t";             
d46 6
a51 2
			    $al_utr=length($seq_utr);
			    print "S-$start\tE-$end\tL-$l_seq\tD-$strand\n$seq\n>UTR\tL-$al_utr ($l_utr)\n$seq_utr\n";
d54 5
a58 3
			    print ">$n_gene\t"; 
			    for my $tag ($feat_object->get_all_tags) {             
				print "Tag: ", $tag, "\t";             
d60 2
a61 1
				    print "Value: ", $value, "\t";             
d65 6
a70 2
			    $al_utr=length($seq_utr);
			    print "S-$start\tE-$end\tL-$l_seq\tD-$strand\n$seq\n>UTR\tL-$al_utr ($l_utr)\n$seq_utr\n";
d84 39
d124 8
@


1.2
log
@version 5
@
text
@d11 3
a13 2

for my $feat_object ($seq_object->get_SeqFeatures) {#if ($feat_object->primary_tag eq "CDS") { # for H37Rv - 3918, 3916 for the inner loop
d23 1
a23 1
			my $seq_utr;
d26 9
a34 5
			if(($strand == -1) && (($end+$l_utr)>$l_seq_complete)){
			    #print "\nComplement Strand $strand\n";
			    #my $startutr = $end - ;
			    #my $endutr = $end;
			    #if(($end+$l_utr)>$l_seq_complete){next;}
d39 1
a39 1
			    print ">S-$start\tE-$end\tL-$l_seq\tD-$strand\n$seq\n>UTR\tL-$al_utr ($l_utr)\n$seq_utr\n";
d41 8
a48 3
			elsif(($strand == 1) && (($start-$l_utr)<=0) ){
			    #if(($start-$l_utr)<=0){next;}
			    #print "\n Strand $strand\n";
a49 3
			    #print "\n Strand $strand\n";
			    #$seq_utr = substr($sequence_string,1,100);
			    #print "\n Strand $strand\n$seq_utr\n";
d51 1
a51 1
			    print ">S-$start\tE-$end\tL-$l_seq\tD-$strand\n$seq\n>UTR\tL-$al_utr ($l_utr)\n$seq_utr\n";		  
@


1.1
log
@Initial revision
@
text
@d3 2
a4 1
$gb_file="CP258.gb";
d6 5
a10 2
       my $seqio_object = Bio::SeqIO->new(-file => $gb_file);
       my $seq_object = $seqio_object->next_seq;
d12 2
a13 3
       for my $feat_object ($seq_object->get_SeqFeatures) {
         #if ($feat_object->primary_tag eq "CDS") { # for H37Rv - 3918, 3916 for the inner loop
         if ($feat_object->primary_tag eq "gene") { # for H37Rv - 3922
d16 2
a17 1
			my $strand = $feat_object->location->strand;
d19 29
a47 1
			print "S-$start\tE-$end\tD-$strand\n";
d57 1
a57 1
       }
@
