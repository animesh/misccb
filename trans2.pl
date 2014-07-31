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

use Bio::PrimarySeq;
use Bio::Seq;
use Bio::SeqIO;
use Bio::SeqIO::fasta;
print "\nenter Fasta File Name to be translated\t";
$file=<>;
chomp $file;
open(F,$file)||die "FILE with NAME $file doesn't exist in the Directory $!";
close F;
$fileo=$file."\.translated";
open FO,">$fileo";
$in  = Bio::SeqIO->new('-file' => $file,'-format' =>'Fasta');
#$out= Bio::SeqIO->new( -format => 'Fasta', -file => '>outfile');
while ( $seq = $in->next_seq() )
{
$seqname = $seq->display_id();
$translation  = $seq->translate();
$translated = $translation->seq();
#$out->write_seq($seq);
print FO"\>$seqname\n$translated\n";
}
