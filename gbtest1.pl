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

         use Bio::Seq;
         use Bio::SeqIO;
	print "\nenter the gbk file";
	$f1=<>;chomp $f1;
        print "\nenter the output file";
        $f2=<>;chomp $f2;

         $seqin = Bio::SeqIO->new( format => 'genbank' , file => '$f1');
         $seqout= Bio::SeqIO->new( format => 'fasta', file =>'>$f2');

         while((my $seqobj = $seqin->next_seq())) {
             print "Seen sequence ",$seqobj->display_id,", start of seq ", substr($seqobj->seq,1,10),"\n";
             if( $seqobj->moltype eq 'dna') {
                   $rev = $seqobj->revcom;
                   $id  = $seqobj->display_id();
                   $id  = "$id.rev";
                   $rev->display_id($id);
                   $seqout->write_seq($rev);
             }

             foreach $feat ( $seqobj->top_SeqFeatures() ) {
                  if( $feat->primary_tag eq 'exon' ) {
                     print STDOUT "Location ",$feat->start,":",
                           $feat->end," GFF[",$feat->gff_string,"]\n";
                  }
             }
         }


