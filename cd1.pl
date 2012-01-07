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
## Time-stamp: "5/1/2005" ##
$wfile='dna_chimes.mid';
 use MIDI::Simple;
 set_tempo 500000;  # 1 qn => .5 seconds (500,000 microseconds)
 patch_change 1, 8;  # Patch 8 = Celesta
 #$file="U00096.ffn";
 $file="s1.txt";
OPENFAS($file);
C2AT2O();
 #noop c1, f, o5;  # Setup
 # Now play
 #SAREGA;

for ($c1=0;$c1<=$#seq;$c1++) {
	$sn=@seqname[$c1];
	$se=@seq[$c1];
	#$se=~s/T/D/g;
	@t=split(//,$se);
	for ($c2=0;$c2<=$#t;$c2=$c2+3) {
		$co1=$c2a{@t[$c2].@t[$c2+1].@t[$c2+2]};
		#noop c1, f, o6;
   	    #n qn, @t[$c2];    
		print "$co1\n";
	}
}


sub SAREGA{$c=0;
 while($c<9){
	 $c++;
	 $t1="o".$c;$t2="o".($c+1);
		  noop c1, f, $t1; 
		  n qn, C;    
		  n qn, D;    
		  n qn, E;    
		  n qn, F;    
		  n qn, G;    
		  n qn, A;    
		  n qn, B;    
		  n $t2, qn, C;

		  n $t2, qn, C;
		  n $t1, qn, B;    
		  n qn, A;    
		  n qn, G;    
		  n qn, F;    
		  n qn, E;    
		  n qn, D;    
		  n qn, C;    
 }
}

write_score $wfile;
#system("wmplayer $wfile");

sub OPENFAS{
	$file = shift;
	open (F, $file) || die "can't open \"$file\": $!";
	$seq="";
	while ($line = <F>) {chomp $line;
		if ($line =~ /^>/){
			push(@seqname,$line);	
			if ($seq ne ""){
				push(@seq,$seq);
						$seq = "";
					}
			}
		 else {$seq=$seq.$line;
			}
	}
	push(@seq,$seq);
	close F;
}


sub C2AT2O{
	%c2a = (
			'TTT' => 'F','TTC' => 'F','TTA' => 'L','TTG' => 'L',
			'TCT' => 'S','TCC' => 'S','TCA' => 'S','TCG' => 'S',
			'TAT' => 'T','TAC' => 'T','TAA' => 'stop','TAG' => 'stop',
			'TGT' => 'C','TGC' => 'C','TGA' => 'stop','TGG' => 'W',
			
			'CTT' => 'L','CTC' => 'L','CTA' => 'L','CTG' => 'L',
			'CCT' => 'P','CCC' => 'P','CCA' => 'P','CCG' => 'P',
			'CAT' => 'H','CAC' => 'H','CAA' => 'Q','CAG' => 'Q',
			'CGT' => 'R','CGC' => 'R','CGA' => 'R','CGG' => 'R',
			
			'ATT' => 'I','ATC' => 'I','ATA' => 'I','ATG' => 'M',
			'ACT' => 'T','ACC' => 'T','ACA' => 'T','ACG' => 'T',
			'AAT' => 'N','AAC' => 'N','AAA' => 'K','AAG' => 'K',
			'AGT' => 'S','AGC' => 'S','AGA' => 'R','AGG' => 'R',
			
			'GTT' => 'V','GTC' => 'V','GTA' => 'V','GTG' => 'V',
			'GCT' => 'A','GCC' => 'A','GCA' => 'A','GCG' => 'A',
			'GAT' => 'D','GAC' => 'D','GAA' => 'E','GAG' => 'E',
			'GGT' => 'G','GGC' => 'G','GGA' => 'G','GGG' => 'G',
			'NNN' => 'none','gap' => 'gap'
	);
	%t2o = (
      'A' => 'C',
      'V' => 'D',
      'L' => 'E',
      'I' => 'F',
      'P' => 'G',
      'W' => 'A',
      'F' => 'B',
      'M' => 'c',
      'G' => 'd',
      'S' => 'e',
      'T' => 'f',
      'Y' => 'g',
      'C' => 'a',
      'N' => 'b',
      'Q' => 'c\'',
      'K' => 'd\'',
      'R' => 'e\'',
      'H' => 'f\'',
      'D' => 'g\'',
      'E' => 'a\'',
      'stop' => 'b\'',

    );

}