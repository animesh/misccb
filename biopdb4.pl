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

#!/usr/local/bin/perl
my %t2o = (
      'ALA' => 'A',
      'VAL' => 'V',
      'LEU' => 'L',
      'ILE' => 'I',
      'PRO' => 'P',
      'TRP' => 'W',
      'PHE' => 'F',
      'MET' => 'M',
      'GLY' => 'G',
      'SER' => 'S',
      'THR' => 'T',
      'TYR' => 'Y',
      'CYS' => 'C',
      'ASN' => 'N',
      'GLN' => 'Q',
      'LYS' => 'K',
      'ARG' => 'R',
      'HIS' => 'H',
      'ASP' => 'D',
      'GLU' => 'E',
    );


use Bio::Structure::IO;
use strict;
my $file=shift @ARGV;
my $structio = Bio::Structure::IO->new(-file => $file);
my $struc = $structio->next_structure;

for my $chain ($struc->get_chains) {
     my $chainid = $chain->id;
     my $fn=$file;
     $fn=~s/\.pdb|\.PDB//g;
     my $fnfas=$fn.".$chainid.fas";
     my $fnpdb=$fn.".$chainid.pdb";
     open(FO,">$fnfas");
     open(FPDB,">$fnpdb");
     print FO"\>$file\tChain-$chainid\n";
     #print FPDB"\>$file\tChain-$chainid\n";
     for my $res ($struc->get_residues($chain)) {
        my $resid = $res->id;
	my $resol = uc($resid);
	$resol=~s/[0-9]|-//g;
	$resol = $t2o{$resol};
	print FO"$resol";

        #my @atoms = $struc->get_atoms($res);
	#for(my $c2=0;$c2<=$#atoms;$c2++){
		#my @xyz = @atoms[$c2]->xyz; # the 3D coordinates of the atom
		#my $c3=$c2+1;
		#sprintf($c3,5,0);
		#print FPDB"@atoms[$c2]";
		#print "ATOM  $c333  CZ  TYR A   4       8.604  68.070  13.424  1.00 30.07           C\n";
		#for(my $c1=0;$c1<=$#xyz;$c1++){
		#	my $vari=sprintf(@xyz[$c1],3,3);
			#print FPDB"$vari\t";

		
		#}
		print FPDB"\n";
	#}
     }
     print FO"\n";
     close FO;
}


