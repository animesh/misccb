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
print "\nenter Fasta File Name to be translated\t";
$file=<>;
chomp $file;
$seqin = Bio::SeqIO->new( -format => 'Fasta' , -file => $file);
$seqout= Bio::SeqIO->new( -format => 'Fasta', -file => '>$file.trans');
$seq=$seqin->seq();
print "$seq\n";