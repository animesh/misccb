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
#read in data for mw:
$allowed="ACDEFGHIKLMNPQRSTVWY";# Supported amino acids
$water=18.0152;                 # mol wt of H2O (added decimals, since
                                # it's multiplied by no_aa-1).
                                # molecular weights
%aawt=('A', 89.09, 'C', 121.16, 'D', 133.10, 'E', 147.13, 'F', 165.19,
       'G', 75.07, 'H', 155.16, 'I', 131.18, 'K', 146.19, 'L', 131.18,
       'M',149.21, 'N', 132.12, 'P', 115.13, 'Q', 146.15, 'R', 174.20,
       'S',105.09, 'T', 119.12, 'V', 117.15, 'W', 204.23, 'Y', 181.19);
print "What is the filename containing the sequences? ";
$name = <STDIN>;
chomp($name);
print "The sequence filename is $name \n";
open (FILENAME, $name) ||
       die "can't open $name: $!";
while ($line = <FILENAME>) {
	chomp ($line);
	if ($line =~ /^>/){
	    $line =~ s/>//;
	    $seqname=$line;
	} else {
            $seq=$seq.$line;
        }
        }
$protein=$seq;
$no_aa = length($protein);
foreach $aa (split(//, $allowed)) {
  $residue{$aa} = ($protein =~ s/$aa//g);
  $molwt += $residue{$aa}*$aawt{$aa};
  print "$protein\n\n";
}
     $molwt -= ($no_aa-1)*$water;
          print "the sequence name is $seqname\n";
     print "Sequence read from file is:\n$seq \n";                #starts reading the file line by line
     print "Molecular wt: $molwt\n";
