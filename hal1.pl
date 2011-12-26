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
if( @ARGV ne 1){die "\nUSAGE\t\"ProgName MultSeqFile\t\n\n\n";}
$file = shift @ARGV;
#$thr=20;
$amino_acid_order = "ABCDEFGHIKLMNPQRSTVWXYZ";
%h2a = (
	'I'		 => 		4.5,
	'V'		 => 		4.2,
	'L'		 => 		3.8,
	'F'		 => 		2.8,
	'C'		 => 		2.5,
	'M'		 => 		1.9,
	'A'		 => 		1.8,
	'G'		 => 		-0.4,
	'T'		 => 		-0.7,
	'W'		 => 		-0.9,
	'S'		 => 		-0.8,
	'Y'		 => 		-1.3,
	'P'		 => 		-1.6,
	'H'		 => 		-3.2,
	'E'		 => 		-3.5,
	'Q'		 => 		-3.5,
	'D'		 => 		-3.5,
	'N'		 => 		-3.5,
	'K'		 => 		-3.9,
	'R'		 => 		-4.5,
	'B'		 => 		-3.5,
	'Z'		 => 		-3.5,
	'X'		 => 		0,
	'*'		 => 		-4.5,
);

#4 5
#-1 -2

open (F, ">$file") || die "can't open \"$file\": $!";
$cnt1=0;
$date=time;
print F"#  Matrix made by PERL Code PROJECT_HYDRO $date\n";
print F"#  * column uses minimum score\n";
print F"#  Scoring Matrix in Hydro units\n";
print F"#  Blocks Database = None\n";
print F"#  Cluster Percentage: >= 10\n";
print F"#  Entropy =   0.6979, Expected =  -0.5209\n";
print F" ";
foreach $c1 (sort keys %h2a) {
	print F"$c1 ";
}
print F"\n";
foreach $c1 (sort keys %h2a) {
	$cnt1++;$cnt2=0;
	#print "$cnt1\t$c1\t$h2a{$c1}\t$h2a{$c2}\n";
#	print "$h2a{$c1}\t";
	print F"$c1 ";
		foreach $c2 (sort keys %h2a) {
			$cnt2++;
			if($c1 eq $c2){
					$val=exp(-(($h2a{$c1}-$h2a{$c2})**2));
					$val=int(10000*$val);
					#print "$cnt1\t$cnt2\t$c1\t$c2\t$h2a{$c1}\t$h2a{$c2}\n";
					print F"$val ";
					print "$val\t";
#
				#last;#print "$cnt\t$c1\t$h2a{$c1}\t$h2a{$c2}\n";
			}
			else{
#				if($h2a{$c1}<($h2a{$c2} and $h2a{$c1}<0){
					$val=exp(-(($h2a{$c1}-$h2a{$c2})**2));
					#$val=log((($h2a{$c1}/$h2a{$c2})));
					$val=int(10000*$val);
					#print "$cnt1\t$cnt2\t$c1\t$c2\t$h2a{$c1}\t$h2a{$c2}\n";
					print "$val [$h2a{$c1}-$h2a{$c2}]\t";
					print F"$val ";
#				}
#				else{
#					$val=exp(-(($h2a{$c1}-$h2a{$c2})**2));
#					$val=int(10*$val);
					#print "$cnt1\t$cnt2\t$c1\t$c2\t$h2a{$c1}\t$h2a{$c2}\n";
#					print "$val\t";
#				}
			}
		}
	print F"\n";
	print "\n";

}


#Group Residues Description
#1 C Cysteine, remains strongly during evolution
#2 M Hydrophobic, sulfur containing
#3 N, Q Amides, polar
#4 D, E Acids, polar, charged
#5 S, T Alcohols
#6 P, A, G Small
#7 I, V, L Aliphatic
#8 F, Y, W Aromatic
#9 H, K, R Bases, charged, positiv

#Amino Acid Name One Letter Code Hydropathy Score
 
#Isoleucine I 4.5 
#Valine V 4.2 
#Leucine L 3.8 
#Phenylalanine F 2.8 
#Cysteine C 2.5 
#Methionine M 1.9 
#Alanine A 1.8 
#Glycine G -0.4 
#Threonine T -0.7 
#Tryptophan W -0.9 
#Serine S -0.8 
#Tyrosine Y -1.3 
#Proline P -1.6 
#Histidine H -3.2 
#Glutamicacid E -3.5 
#Glutamine Q -3.5 
#Asparticacid D -3.5 
#Asparagine N -3.5 
#Lysine K -3.9 
#Arginine R -4.5 



#short blosum30mt[]={
#  4,
#  0,  5,
# -3, -2, 17,
#  0,  5, -3,  9,
#  0,  0,  1,  1,  6,
# -2, -3, -3, -5, -4, 10,
#  0,  0, -4, -1, -2, -3,  8,
# -2, -2, -5, -2,  0, -3, -3, 14,
#  0, -2, -2, -4, -3,  0, -1, -2,  6,
#  0,  0, -3,  0,  2, -1, -1, -2, -2,  4,
# -1, -1,  0, -1, -1,  2, -2, -1,  2, -2,  4,
#  1, -2, -2, -3, -1, -2, -2,  2,  1,  2,  2,  6,
#  0,  4, -1,  1, -1, -1,  0, -1,  0,  0, -2,  0,  8,
# -1, -2, -3, -1,  1, -4, -1,  1, -3,  1, -3, -4, -3, 11,
#  1, -1, -2, -1,  2, -3, -2,  0, -2,  0, -2, -1, -1,  0,  8,
# -1, -2, -2, -1, -1, -1, -2, -1, -3,  1, -2,  0, -2, -1,  3,  8,
#  1,  0, -2,  0,  0, -1,  0, -1, -1,  0, -2, -2,  0, -1, -1, -1,  4,
#  1,  0, -2, -1, -2, -2, -2, -2,  0, -1,  0,  0,  1,  0,  0, -3,  2,  5,
#  1, -2, -2, -2, -3,  1, -3, -3,  4, -2,  1,  0, -2, -4, -3, -1, -1,  1,  5,
# -5, -5, -2, -4, -1,  1,  1, -5, -3, -2, -2, -3, -7, -3, -1,  0, -3, -5, -3, 20,
#  0, -1, -2, -1, -1, -1, -1, -1,  0,  0,  0,  0,  0, -1,  0, -1,  0,  0,  0, -2, -1,
# -4, -3, -6, -1, -2,  3, -3,  0, -1, -1,  3, -1, -4, -2, -1,  0, -2, -1,  1,  5, -1,  9,
#  0,  0,  0,  0,  5, -4, -2,  0, -3,  1, -1, -1, -1,  0,  4,  0, -1, -1, -3, -1,  0, -2,  4};

