#!/usr/local/bin/perl

$file=shift @ARGV;
chomp $file;
$wfile=$file.".midi";

use Math::Complex;
$pi=pi;
$i=sqrt(-1);
@base=qw/G T A C/;
$window=90;
$threshold=3;
use MIDI::Simple;
set_tempo 5000000;  
patch_change 1, 8; 
$thresh=1000;



OPENFAS($file);
new_score;
CRTDNAMUS();
write_score("$wfile");
exit;

sub CRTDNAMUS{
	for ($c1=0;$c1<=$#seq;$c1++) {
		$sn=@seqname[$c1];
		$se=@seq[$c1];
		$len=length($se);
		for ($c2=0;$c2<$thresh;$c2=$c2+$window) {
			$dnastr=substr($se,$c2,$window);
			undef @ptnr; undef $mash;
			$valptnr=FTT($dnastr);
			print "$file: $c1\t$c2\t$le\t$valptnr\n";
			for($c3=0;$c3<=$#ptnr;$c3++){
				@t1=split(/\./,@ptnr[$c3]);				
				$chann1=@t1[2]+1;$chann1="c".$chann1;
				@t2=split(//,@t1[1]);
				$note1=(@t1[0])+(@t2[1]*10);$note1="n".$note1;
				$octave1=(@t2[0]+1);$octave1="o".$octave1;
				noop $chann1, f, $octave1;
				n sn, $note1;
				#n(c9, ff, n41, qn);
			}
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



sub FTT {
	$dnastr=shift;$dnastr=uc($dnastr);
	until ($dnastr !~ /^G/){$dnastr =~s/^G//;}
	if((length($dnastr)%3)!=0){
		$dleng=length($dnastr);
		$dleng=$dleng-(length($dnastr)%3);
		$dnastr=substr($dnastr,0,$dleng);
		}
	$ws=length($dnastr);
	$subfftseq=$dnastr;
	$subs=$subfftseq;
	$c=$subfftseq=~s/C/C/g;$a=$subfftseq=~s/A/A/g;$g=$subfftseq=~s/G/G/g;$t=$subfftseq=~s/T/T/g;
	@subssplit=split(//,$subs);
	$sp=$ws;$le=$ws;
		for($k=1;$k<=($sp/2);$k++){	
				for($c6=0;$c6<=$#base;$c6++){
				$bvar=@base[$c6];
					for($c7=0;$c7<=$#subssplit;$c7++){
					$wsvar=@subssplit[$c7];
						if ($bvar eq $wsvar){
							$subsum+=exp(2*$pi*$i*($k/$le)*($c7+1));
						}
						else{
							$subsum+=0;
						}
					}
				$subsumtotal+=(((1/$le)**2)*(abs($subsum)**2));
				$subsum=0;
				}
				$atgcsq=((1/($ws**2))*($c**2+$a**2+$g**2+$t**2));
				$sbar=(1/$ws)*(1+(1/$ws)-$atgcsq);$atgcsq=0;
				$substss=$sbar;
				$subptnr1=$subsumtotal/$substss;
				$subsumtotal=0;
				$subptnr2=$subptnr1/($sp*$substss);
				$subptnr3=$subptnr2*2;
				$pp=($k)/$le;
				$sp3=$subptnr3;$sp3=sprintf (1,$subptnr3,2);$sp3=substr($subptnr3,0,3);
				push(@ptnr,$subptnr3);$mash{$subptnr3}=$pp;
				if(($le/$k)==3){$ptnr3=$subptnr3;}
		}
		return($ptnr3);
}
new_score;

@subs = ( \&measure_counter, \&psycho, \&boom, \&tboom, \&clap );
foreach (1 .. 24) { synch(@subs) }
write_score("base.midi");
exit;

# Subs
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
