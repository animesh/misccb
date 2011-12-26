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

use Bio::Seq;
use Bio::SeqIO;

$input_seqs = Bio::SeqIO->new ( '-format' => 'GenBank',
                                '-file'   => 'phix174.gbk'
                              );

while ( $s = $input_seqs->next_seq() )
   {	$seq1=$s->seq;
	#print( "sequence length is ", $s->length(), "\n" );print( "ac number is ", $s->accession(), "\n" );substr($s->seq,1,10);
    foreach $f ( $s->all_SeqFeatures() )
       {

	$st=$f->start;
	$sp=$f->end;
	$le=$sp-$st;
	$fe1=$f->primary_tag;
	$fe1=$f->top_SeqFeatures;
    	print "\>$st to $sp\t$fe1\n";
    	#" Primary tag  ", $f->primary_tag,
    	#" From ", $f->source_tag(),
	#"\n";
	$sequ=substr($seq1,($st-1),$le+1);
	print "$sequ\n";
        #print "Feature description is ", $f->gff_string(), "\n";
       }
   }


