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


system("ls -1 > t");
system("mkdir test");
open F,"t";
while($l=<F>)
{

#if($l ~= /(txt$)/ ){
print $l;
$l=~s/\?//;
$file=$l;

$fn=$file.".fasta";
 use Bio::SeqIO;
  $in  = Bio::SeqIO->new('-file' => $file,
                         '-format' => 'GenBank');
  $out = Bio::SeqIO->new('-file' => ">$fn",
                         '-format' => 'Fasta');
  while ( my $seq = $in->next_seq() ) {$out->write_seq($seq); }
 # }
}
close F;