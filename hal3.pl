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
open (F, ">$file") || die "can't open \"$file\": $!";
$cnt1=0;
$date=time;
print F"#  Matrix made by PERL Code PROJECT_HYDRO $date\n";
print F"#  * column uses minimum score\n";
print F"#  Scoring Matrix in Hydro units\n";
print F"#  Blocks Database = None\n";
print F"#  Cluster Percentage: >= 10\n";
print F"#  Entropy =   0.6979, Expected =  -0.5209\n";
print F"\t";
foreach $c1 (sort keys %h2a) {
	print F"$c1\t";
}
print F"\n";
foreach $c1 (sort keys %h2a) {
	print F"$c1\t";
		foreach $c2 (sort keys %h2a) {
					$val=(-(abs(abs(10*$h2a{$c1})-abs(10*$h2a{$c2}))));
					$val=int($val/10);
					print F"$val\t";
					print "$val\t";
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

##  Matrix made by matblas from blosum62.iij
##  * column uses minimum score
##  BLOSUM Clustered Scoring Matrix in 1/2 Bit Units
##  Blocks Database = /data/blocks_5.0/blocks.dat
##  Cluster Percentage: >= 62
##  Entropy =   0.6979, Expected =  -0.5209
#   A  R  N  D  C  Q  E  G  H  I  L  K  M  F  P  S  T  W  Y  V  B  Z  X  *
#A  4 -1 -2 -2  0 -1 -1  0 -2 -1 -1 -1 -1 -2 -1  1  0 -3 -2  0 -2 -1  0 -4 
#R -1  5  0 -2 -3  1  0 -2  0 -3 -2  2 -1 -3 -2 -1 -1 -3 -2 -3 -1  0 -1 -4 
#N -2  0  6  1 -3  0  0  0  1 -3 -3  0 -2 -3 -2  1  0 -4 -2 -3  3  0 -1 -4 
#D -2 -2  1  6 -3  0  2 -1 -1 -3 -4 -1 -3 -3 -1  0 -1 -4 -3 -3  4  1 -1 -4 
#C  0 -3 -3 -3  9 -3 -4 -3 -3 -1 -1 -3 -1 -2 -3 -1 -1 -2 -2 -1 -3 -3 -2 -4 
#Q -1  1  0  0 -3  5  2 -2  0 -3 -2  1  0 -3 -1  0 -1 -2 -1 -2  0  3 -1 -4 
#E -1  0  0  2 -4  2  5 -2  0 -3 -3  1 -2 -3 -1  0 -1 -3 -2 -2  1  4 -1 -4 
#G  0 -2  0 -1 -3 -2 -2  6 -2 -4 -4 -2 -3 -3 -2  0 -2 -2 -3 -3 -1 -2 -1 -4 
#H -2  0  1 -1 -3  0  0 -2  8 -3 -3 -1 -2 -1 -2 -1 -2 -2  2 -3  0  0 -1 -4 
#I -1 -3 -3 -3 -1 -3 -3 -4 -3  4  2 -3  1  0 -3 -2 -1 -3 -1  3 -3 -3 -1 -4 
#L -1 -2 -3 -4 -1 -2 -3 -4 -3  2  4 -2  2  0 -3 -2 -1 -2 -1  1 -4 -3 -1 -4 
#K -1  2  0 -1 -3  1  1 -2 -1 -3 -2  5 -1 -3 -1  0 -1 -3 -2 -2  0  1 -1 -4 
#M -1 -1 -2 -3 -1  0 -2 -3 -2  1  2 -1  5  0 -2 -1 -1 -1 -1  1 -3 -1 -1 -4 
#F -2 -3 -3 -3 -2 -3 -3 -3 -1  0  0 -3  0  6 -4 -2 -2  1  3 -1 -3 -3 -1 -4 
#P -1 -2 -2 -1 -3 -1 -1 -2 -2 -3 -3 -1 -2 -4  7 -1 -1 -4 -3 -2 -2 -1 -2 -4 
#S  1 -1  1  0 -1  0  0  0 -1 -2 -2  0 -1 -2 -1  4  1 -3 -2 -2  0  0  0 -4 
#T  0 -1  0 -1 -1 -1 -1 -2 -2 -1 -1 -1 -1 -2 -1  1  5 -2 -2  0 -1 -1  0 -4 
#W -3 -3 -4 -4 -2 -2 -3 -2 -2 -3 -2 -3 -1  1 -4 -3 -2 11  2 -3 -4 -3 -2 -4 
#Y -2 -2 -2 -3 -2 -1 -2 -3  2 -1 -1 -2 -1  3 -3 -2 -2  2  7 -1 -3 -2 -1 -4 
#V  0 -3 -3 -3 -1 -2 -2 -3 -3  3  1 -2  1 -1 -2 -2  0 -3 -1  4 -3 -2 -1 -4 
#B -2 -1  3  4 -3  0  1 -1  0 -3 -4  0 -3 -3 -2  0 -1 -4 -3 -3  4  1 -1 -4 
#Z -1  0  0  1 -3  3  4 -2  0 -3 -3  1 -1 -3 -1  0 -1 -3 -2 -2  1  4 -1 -4 
#X  0 -1 -1 -1 -2 -1 -1 -1 -1 -1 -1 -1 -1 -1 -2  0  0 -2 -1 -1 -1 -1 -1 -4 
#* -4 -4 -4 -4 -4 -4 -4 -4 -4 -4 -4 -4 -4 -4 -4 -4 -4 -4 -4 -4 -4 -4 -4  1 


#Command line parameters 
#
#--------------------------------------------------------------------------------
#
#                DATA (sequences)
#
#/INFILE=file.ext                             :input sequences.
#/PROFILE1=file.ext  and  /PROFILE2=file.ext  :profiles (old alignment).
#--------------------------------------------------------------------------------
#        VERBS (do things)
#/OPTIONS	    :list the command line parameters
#/HELP  or /CHECK    :outline the command line params.
#/ALIGN              :do full multiple alignment 
#/TREE               :calculate NJ tree.
#/BOOTSTRAP(=n)      :bootstrap a NJ tree (n= number of bootstraps; def. = 1000).
#/CONVERT            :output the input sequences in a different file format.
#
#--------------------------------------------------------------------------------
#        PARAMETERS (set things)
#***General settings:****
#/INTERACTIVE :read command line, then enter normal interactive menus
#/QUICKTREE   :use FAST algorithm for the alignment guide tree
#/NEGATIVE    :protein alignment with negative values in matrix
#/OUTFILE=    :sequence alignment file name
#/OUTPUT=     :GCG, GDE, PHYLIP or PIR
#/OUTORDER=   :INPUT or ALIGNED
#/CASE        :LOWER or UPPER (for GDE output only)
#/SEQNOS=     :OFF or ON (for Clustal output only)
#
#***Fast Pairwise Alignments:***
#/KTUPLE=n      :word size                  /TOPDIAGS=n  :number of best diags.
#/WINDOW=n    :window around best diags.  /PAIRGAP=n   :gap penalty
#/SCORE       :PERCENT or ABSOLUTE
#
#***Slow Pairwise Alignments:***
#/PWMATRIX=    :Protein weight matrix=BLOSUM, PAM, GONNET, ID or filename
#/PWDNAMATRIX= :DNA weight matrix=IUB, CLUSTALW or filename²
#/PWGAPOPEN=f  :gap opening penalty        /PWGAPEXT=f  :gap extension penalty
#
#***Multiple Alignments:***
#/NEWTREE=    :file for new guide tree
#/USETREE=    :file for old guide tree
#/MATRIX=     :Protein weight matrix=BLOSUM, PAM, GONNET, ID or filename
#/DNAMATRIX=  :DNA weight matrix=IUB, CLUSTALW or filename
#/GAPOPEN=f   :gap opening penalty             /GAPEXT=f    :gap extension penalty
#/ENDGAPS     :no end gap separation pen.      /GAPDIST=n   :gap separation pen. range
#/NOPGAP      :residue-specific gaps off        /NOHGAP      :hydrophilic gaps off
#/HGAPRESIDUES= :list hydrophilic res.    /MAXDIV=n    :% ident. for delay
#/TYPE=       :PROTEIN or DNA             /TRANSWEIGHT :transitions weighted.
#
# ***Profile Alignments:***
#/PROFILE     :Merge two alignments by profile alignment
#/NEWTREE1=    :file for new guide tree for profile1
#/NEWTREE2=    :file for new guide tree for profile2
#/USETREE1=    :file for old guide tree for profile1
#/USETREE2=    :file for old guide tree for profile2
#
#***Sequence to Profile Alignments:***
#/SEQUENCES   :Sequentially add profile2 sequences to profile1 alignment
#/NEWTREE=    :file for new guide tree
#/USETREE=    :file for old guide tree
#
#***Structure Alignments:***
#/NOSECSTR1     :do not use secondary structure/gap penalty mask for profile 1 
#/NOSECSTR2     :do not use secondary structure/gap penalty mask for profile 2
#/SECSTROUT=    :STRUCTURE or MASK or BOTH or NONE  output in alignment file
#/HELIXGAP=n    :gap penalty for helix core residues 
#/STRANDGAP=n   :gap penalty for strand core residues
#/LOOPGAP=n     :gap penalty for loop regions
#/TERMINALGAP=n :gap penalty for structure termini
#/HELIXENDIN=n  :number of residues inside helix to be treated as terminal
#/HELIXENDOUT=n :number of residues outside helix to be treated as terminal
#/STRANDENDIN=n :number of residues inside strand to be treated as terminal
#/STRANDENDOUT=n:number of residues outside strand to be treated as terminal 
#
#***Trees:***
#/OUTPUTTREE=nj OR phylip OR dist
#/SEED=n      :seed number for bootstraps.
#/KIMURA      :use Kimura's correction.   /TOSSGAPS  :ignore positions with gaps.
#--------------------------------------------------------------------------------
#
#
