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
 set_tempo 500000;  
 patch_change 1, 8; 
 $file="s3.txt";
OPENFAS($file);
C2AT2O();
#CRTDNAMUS();

new_score;
@subs = ( \&measure_counter, \&psycho, \&boom, \&tboom, \&clap,\&CRTDNAMUS );
synch(@subs);
write_score("$wfile");
exit;

sub CRTDNAMUS{
	for ($c1=0;$c1<=$#seq;$c1++) {
		$sn=@seqname[$c1];
		$se=@seq[$c1];
		@t=split(//,$se);
		for ($c2=0;$c2<=$#t;$c2=$c2+3) {
			$co1=$c2a{@t[$c2].@t[$c2+1].@t[$c2+2]};
			noop c1, f, o6;
			n qn, $t2o{$co1};    
		}
	}
}

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
      'M' => 'Cs',
      'G' => 'Ds',
      'S' => 'Fs',
      'T' => 'Gs',
      'Y' => 'As',
      'C' => 'Cs1',
      'N' => 'Ds1',
      'Q' => 'Fs1',
      'K' => 'Gs1',
      'R' => 'As1',
      'H' => 'Cs2',
      'D' => 'Ds2',
      'E' => 'Fs2',
      'stop' => 'Gs2',

    );

}

sub MUSICBT {
sub measure_counter {
  my $it = shift;
  $it->r(wn); # a whole rest
  ++$measure;
}

sub boom {
  my $it = shift;
  return if $measure % 4 < 2;
  $it->n(c9, ff, n41, qn);  $it->r;
  $it->n(f);  r;
}

sub tboom {
  my $it = shift;
  return if $measure % 4 < 2;
  # 42 = 'Closed Hi-Hat' ; 43 = 'High Floor Tom'
  # In quick succession...
  $it->n( c9, ff, n43, sn); $it->n( n42 ); $it->r(dqn);
  # dqn = dotted quarter note/rest
  $it->r( c9, ff, n43, sn); $it->n( n42 ); $it->r(dqn);
}

sub clap {
  my $it = shift;
  return if  $measure < 4;
  $it->n(c9, ff, n39, sn); $it->n;
  $it->r(dqn);
  $it->r(hn);
}

sub psycho {
  my $it = shift;
  my $pattern =
    "  !.!.!.   !!!!!!   !.!.  " ;
  $pattern =~ tr<\cm\cj\t ><>d; # kill whitespace
  warn "<$pattern> doesn't add up to a whole measure\n"
    unless length($pattern) == 16;
  $it->noop(c9, mf, n37, sn);
  # setup: n37 on c9 = side stick
  foreach (split('', $pattern)) {
    if($_ eq '!') { $it->n }
    else { $it->r }
  }
}
}