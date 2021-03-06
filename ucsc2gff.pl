#!/usr/bin/perl

use strict;

use enum qw(:u_ refmethod refsource refgroup refseq refstart refstop refscore refstrand refphase qrystart qrystop sizes starts);
use enum qw(:v_ refmethod refsource refgroup refseq refstrand refscore refphase qrystart qrystop exonstarts exonstops);

use enum qw(:all_bacends__ x matches misMatches repMatches nCount qNumInsert tNumInsert tBaseInsert strand qName qSize qStart qEnd tName tSize tStart tEnd blockCount blockSizes qStarts tStarts);
use enum qw(:all_est__     bin matches misMatches repMatches nCount qNumInsert qBaseInsert tNumInsert tBaseInsert strand qName qSize qStart qEnd tName tSize tStart tEnd blockCount blockSizes qStarts tStarts);
use enum qw(:all_mrna__   bin matches misMatches repMatches nCount qNumInsert qBaseInsert tNumInsert tBaseInsert strand qName qSize qStart qEnd tName tSize tStart tEnd blockCount blockSizes qStarts tStarts);
use enum qw(:all_sts_primer__ matches misMatches repMatches nCount qNumInsert qBaseInsert tNumInsert tBaseInsert strand qName qSize qStart qEnd tName tSize tStart tEnd blockCount blockSizes qStarts tStarts);
use enum qw(:all_sts_seq__    matches misMatches repMatches nCount qNumInsert qBaseInsert tNumInsert tBaseInsert strand qName qSize qStart qEnd tName tSize tStart tEnd blockCount blockSizes qStarts tStarts);
use enum qw(:bacEndPairs__ bin chrom chromStart chromEnd name score strand pslTable lfCount lfStarts lfSizes lfNames);
use enum qw(:blatFish__  bin matches misMatches repMatches nCount qNumInsert qBaseInsert tNumInsert tBaseInsert strand qName qSize qStart qEnd tName tSize tStart tEnd blockCount blockSizes qStarts tStarts);
use enum qw(:gap__ bin chrom chromStart chromEnd ix n size type bridge);
use enum qw(:gl__ bin frag start end strand);
use enum qw(:gold__ bin chrom chromStart chromEnd ix type frag fragStart fragEnd strand);
use enum qw(:intronEst__ bin matches misMatches repMatches nCount qNumInsert qBaseInsert tNumInsert tBaseInsert strand qName qSize qStart qEnd tName tSize tStart tEnd blockCount blockSizes qStarts tStarts);
use enum qw(:mrna__      bin matches misMatches repMatches nCount qNumInsert qBaseInsert tNumInsert tBaseInsert strand qName qSize qStart qEnd tName tSize tStart tEnd blockCount blockSizes qStarts tStarts);
use enum qw(:rmsk__      bin swScore milliDiv milliDel milliIns genoName genoStart genoEnd genoLeft strand repName repClass repFamily repStart repEnd repLeft id);
use enum qw(:clonePos__ name seqSize phase chrom chromStart chromEnd stage faFile);
use enum qw(:ctgPos__ contig size chrom chromStart chromEnd);
use enum qw(:cytoBand__ chrom chromStart chromEnd name gieStain);
use enum qw(:fishClones__ chrom chromStart chromEnd name score placeCount bandStarts bandEnds labs placeType accCount accNames stsCount stsNames beCount beNames);
use enum qw(:gcPercent__ chrom chromStart chromEnd name gcPpt);
use enum qw(:genscan__ name chrom strand txStart txEnd cdsStart cdsEnd exonCount exonStarts exonEnds);
use enum qw(:genscanSubopt__ bin chrom chromStart chromEnd name score strand);
use enum qw(:jaxOrtholog__ humanSymbol humanBand mgiId mouseSymbol mouseChr mouseCm mouseBand);
use enum qw(:refGene__ name chrom strand txStart txEnd cdsStart cdsEnd exonCount exonStarts exonEnds);
use enum qw(:refLink__ name product mrnaAcc protAcc geneName prodName locusLinkID omimId);
use enum qw(:refSeqAli__    bin matches misMatches repMatches nCount qNumInsert qBaseInsert tNumInsert tBaseInsert strand qName qSize qStart qEnd tName tSize tStart tEnd blockCount blockSizes qStarts tStarts);
use enum qw(:simpleRepeat__ bin chrom chromStart chromEnd name period copyNum consensusSize perMatch perIndel score A C G T entropy sequence);
use enum qw(:stsAlias__ alias identNo trueName);
use enum qw(:stsInfo__ identNo name gbCount genbank gdbCount gdb nameCount otherNames dbSTSid otherDbstsCount otherDbSTS leftPrimer rightPrimer distance organism sequence otherUCSCcount otherUCSC mergeUCSCcount mergeUCSC genethonName genethonChr genethonPos genethonLOD marshfieldName marshfieldChr marshfieldPos marshfieldLOD wiyacName wiyacChr wiyacPos wiyacLOD wirhName wirhChr wirhPos wirhLOD gm99gb4Name gm99gb4Chr gm99gb4Pos gm99gb4LOD gm99g3Name gm99g3Chr gm99g3Pos gm99g3LOD tngName tngChr tngPos tngLOD);
use enum qw(:stsMap__ chrom chromStart chromEnd name score identNo ctgAcc otherAcc genethonChrom genethonPos marshfieldChrom marshfieldPos gm99Gb4Chrom gm99Gb4Pos shgcTngChrom shgcTngPos shgcG3Chrom shgcG3Pos wiYacChrom wiYacPos wiRhChrom wiRhPos fishChrom beginBand endBand lab);
###############################################
# end enum
###############################################

my %nolandmark = map {$_=>1} qw(gap cpgIsland recombRate_decode recombRate_marshfield recombRate_genethon
								humMusL zoom1_humMusL zoom50_humMusL zoom2500_humMusL
								genscanSubopt simpleRepeat snpNih snpTsc
								sanger22 sanger22_CDS sanger22pseudo sanger22pseudo_CDS
								softberryGene softberryGene_CDS
								twinscan twinscan_CDS
								refFlat_CDS
							   );
my %relativecoords = map {$_=>1} qw( uniGene_2 nci60 affyRatio );


foreach my $filename (@ARGV){
  warn $filename;
  next if $filename =~ /Pep\./;
  my $newfilename = $filename;
  $newfilename =~ s/txt\.gz/gff/;
  open(my $fhi, "zcat $filename |");
#  $newfilename =~ s/txt/gff/;
#  open(my $fhi, "$filename");

  open(my $fho, ">$newfilename");

  while(my $line = <$fhi>){

  # $filename =~ /rnaCluster/              ? toGFF($line,$fho,['rnaCluster',              '', 4, 1, 2, 3, 5, 6,-1,' ',' ',11,12]) :
  # $filename =~ /estOrientInfo/           ? toGFF($line,$fho,['estOrientInfo',           '',]) : wtf?

    $filename =~ /cpgIsland/               ? toGFF($line,$fho,['cpgIsland',               '', 'CpG', 0, 1, 2,-1,'+',-1]) :
    $filename =~ /all_bacends/             ? toGFF($line,$fho,['bacends',                 '', 9,13,15,16,-1, 8,-1,11,12,18,20]) :
    $filename =~ /all_est/                 ? toGFF($line,$fho,['est',                     '',10,14,16,17,-1, 9,-1,12,13,19,21]) :
    $filename =~ /all_mrna/                ? toGFF($line,$fho,['mrna',                    '',10,14,16,17,-1, 9,-1,12,13,19,21]) :
    $filename =~ /all_sts_primer/          ? toGFF($line,$fho,['sts_primer',              '', 9,13,15,16,-1, 8,-1,11,12,18,20]) :
    $filename =~ /all_sts_seq/             ? toGFF($line,$fho,['sts_seq',                 '', 9,13,15,16,-1, 8,-1,11,12,18,20]) :
    $filename =~ /blastzBestMouse/         ? toGFF($line,$fho,['blastzBestMouse',         '',10,14,16,17,-1, 9,-1,12,13,19,21]) :
    $filename =~ /blastzMm2/               ? toGFF($line,$fho,['blastzMm2',               '',10,14,16,17,-1, 9,-1,12,13,19,21]) :
    $filename =~ /blastzTightMouse/        ? toGFF($line,$fho,['blastzTightMouse',        '',10,14,16,17,-1, 9,-1,12,13,19,21]) :
    $filename =~ /blatFish/                ? toGFF($line,$fho,['blatFish',                '',10,14,16,17,-1, 9,-1,12,13,19,21]) :
    $filename =~ /chimpBac/                ? toGFF($line,$fho,['chimpBac',                '',10,14,16,17,-1, 9,-1,12,13,19,21]) :
    $filename =~ /chimpBlat/               ? toGFF($line,$fho,['chimpBlat',               '',10,14,16,17,-1, 9,-1,12,13,19,21]) :
    $filename =~ /clonePos/                ? toGFF($line,$fho,['clonePos',                '', 0, 3, 4, 5,-1,'+', 2]) :
    $filename =~ /ctgPos/                  ? toGFF($line,$fho,['ctgPos',                  '', 0, 2, 3, 4,-1,'+',-1]) :
    $filename =~ /cytoBand/                ? toGFF($line,$fho,['cytoBand',                '', 3, 0, 1, 2,-1,'+',-1]) :
    $filename =~ /est/                     ? toGFF($line,$fho,['est',                     '',10,14,16,17,-1,9,-1,12,13,19,21]) :
    $filename =~ /fishClones/              ? toGFF($line,$fho,['fishClones',              '', 3, 0, 1, 2, 4,-1,-1]) :
    $filename =~ /gap/                     ? toGFF($line,$fho,['gap',                     '', 7, 1, 2, 3,-1,'+',-1]) :
    $filename =~ /gcPercent/               ? toGFF($line,$fho,['gcPercent',               '', 3, 0, 1, 2, 4,'+',-1]) :
    $filename =~ /genMapDb/                ? toGFF($line,$fho,['genMapDb',                '', 3, 0, 1, 2, 4, 5,-1]) :
    $filename =~ /genscanSubopt/           ? toGFF($line,$fho,['genscanSubopt',           '', 4, 1, 2, 3, 5, 6,-1]) :
    $filename =~ /gold/                    ? toGFF($line,$fho,['gold',                    '', 6, 1, 2, 3,-1, 9,-1, 7, 8]) :
    $filename =~ /intronEst/               ? toGFF($line,$fho,['intron_est',              '',10,14,16,17,-1, 9,-1,12,13,19,21]) :
    $filename =~ /recombRate/              ? eval { toGFF($line,$fho,['recombRate_decode',       '', 3, 0, 1, 2, 4,'+',-1]);
                                                    toGFF($line,$fho,['recombRate_marshfield',   '', 3, 0, 1, 2, 7,'+',-1]);
                                                    toGFF($line,$fho,['recombRate_genethon',     '', 3, 0, 1, 2,10,'+',-1]); } :
    $filename =~ /refSeqAli/               ? toGFF($line,$fho,['refSeqAli',               '',10,14,16,17,-1, 9,-1,12,13,19,21]) :
    $filename =~ /rmsk/                    ? toGFF($line,$fho,['rmsk',                    '',10, 5, 6, 7,-1, 9,-1,13,14]) :
    $filename =~ /simpleRepeat/            ? toGFF($line,$fho,['simpleRepeat',            '', 4, 1, 2, 3,10,'+',-1]) :
    $filename =~ /snpNih/                  ? toGFF($line,$fho,['snpNih',                  '', 4, 1, 2, 3]) :
    $filename =~ /snpTsc/                  ? toGFF($line,$fho,['snpTsc',                  '', 4, 1, 2, 3]) :
    $filename =~ /stsMap/                  ? toGFF($line,$fho,['stsMap',                  '', 3, 0, 1, 2, 4,'+',-1]) :
    $filename =~ /xenoEst/                 ? toGFF($line,$fho,['xenoEst',                 '',10,14,16,17,-1, 9,-1,12,13,19,21]) :
    $filename =~ /xenoMrna/                ? toGFF($line,$fho,['xenoMrna',                '',10,14,16,17,-1, 9,-1,12,13,19,21]) :
    $filename =~ /zoom1_humMusL/           ? toGFF($line,$fho,['zoom1_humMusL',           '', 4, 1, 2, 3, 5, 6,-1]) :
    $filename =~ /zoom2500_humMusL/        ? toGFF($line,$fho,['zoom2500_humMusL',        '', 4, 1, 2, 3, 5, 6,-1]) :
    $filename =~ /zoom50_humMusL/          ? toGFF($line,$fho,['zoom50_humMusL',          '', 4, 1, 2, 3, 5, 6,-1]) :
    $filename =~ /humMusL/                 ? toGFF($line,$fho,['humMusL',                 '', 4, 1, 2, 3, 5, 6,-1]) :

    $filename =~ /nci60/                   ? toGFF($line,$fho,['nci60',                   '', 3, 0, 1, 2, 4, 5,-1,' ',' ',10,11]) :
    $filename =~ /affyRatio/               ? toGFF($line,$fho,['affyRatio',               '', 3, 0, 1, 2, 4, 5,-1,' ',' ',10,11]) :
    $filename =~ /uniGene_2/               ? toGFF($line,$fho,['uniGene_2',               '', 4, 1, 2, 3, 5, 6,-1,' ',' ',11,12]) :

    $filename =~ /refGene/                 ? eval { toGFF2($line,$fho,['refGene',                '', 0, 1, 2,-1,-1, 3, 4, 8, 9]);
                                                    toGFF2($line,$fho,['refGene_CDS',            '', 0, 1, 2,-1,-1, 5, 6, 8, 9]); } :
    $filename =~ /genscan/                 ? eval { toGFF2($line,$fho,['genscan',                '', 0, 1, 2,-1,-1, 3, 4, 8, 9]);
                                                    toGFF2($line,$fho,['genscan_CDS',            '', 0, 1, 2,-1,-1, 5, 6, 8, 9]); } :
    $filename =~ /acembly/                 ? eval { toGFF2($line,$fho,['acembly',                '', 0, 1, 2,-1,-1, 3, 4, 8, 9]);
                                                    toGFF2($line,$fho,['acembly_CDS',            '', 0, 1, 2,-1,-1, 5, 6, 8, 9]); } :
    $filename =~ /ensGene/                 ? eval { toGFF2($line,$fho,['ensGene',                '', 0, 1, 2,-1,-1, 3, 4, 8, 9]);
                                                    toGFF2($line,$fho,['ensGene_CDS',            '', 0, 1, 2,-1,-1, 5, 6, 8, 9]); } :
    $filename =~ /refFlat/                 ? eval { toGFF2($line,$fho,['refFlat',                '', 0, 2, 3,-1,-1, 4, 5, 9,10]);
                                                    toGFF2($line,$fho,['refFlat_CDS',            '', 0, 2, 3,-1,-1, 6, 7, 9,10]);
                                                    toGFF2($line,$fho,['refFlat',                '', 1, 2, 3,-1,-1, 4, 5, 9,10]);
                                                    toGFF2($line,$fho,['refFlat_CDS',            '', 1, 2, 3,-1,-1, 6, 7, 9,10]); } :
    $filename =~ /sanger22pseudo/          ? eval { toGFF2($line,$fho,['sanger22pseudo',         '', 0, 1, 2,-1,-1, 3, 4, 8, 9]);
                                                    toGFF2($line,$fho,['sanger22pseudo_CDS',     '', 0, 1, 2,-1,-1, 5, 6, 8, 9]); } :
    $filename =~ /sanger22/                ? eval { toGFF2($line,$fho,['sanger22',               '', 0, 1, 2,-1,-1, 3, 4, 8, 9]);
                                                    toGFF2($line,$fho,['sanger22_CDS',           '', 0, 1, 2,-1,-1, 5, 6, 8, 9]); } :
    $filename =~ /softberryGene/           ? eval { toGFF2($line,$fho,['softberryGene',          '', 0, 1, 2,-1,-1, 3, 4, 8, 9]);
                                                    toGFF2($line,$fho,['softberryGene_CDS',      '', 0, 1, 2,-1,-1, 5, 6, 8, 9]); } :
    $filename =~ /twinscan/                ? eval { toGFF2($line,$fho,['twinscan',               '', 0, 1, 2,-1,-1, 3, 4, 8, 9]);
                                                    toGFF2($line,$fho,['twinscan_CDS',           '', 0, 1, 2,-1,-1, 5, 6, 8, 9]); } :
    0;
  }

  close($fhi);
  close($fho);
}

###############################################
# begin filetype-specific subroutines
###############################################

sub toGFF2 {
  my($line,$fho,$maps) = @_;

  chomp $line; my @fields = split /\t/, $line;

  if(!$nolandmark{render($maps->[v_refmethod],\@fields)}){
	print $fho join "\t", map {render($maps->[$_],\@fields)} (v_refseq,
														 v_refsource,
														 v_refmethod,
														 v_qrystart,
														 v_qrystop,
														 u_refscore,
														 v_refstrand,
														 v_refphase,);
	print $fho "\t";
	print $fho "Sequence " . render($maps->[v_refgroup],\@fields);
	print $fho "\n";
  }

  if(defined($maps->[v_exonstarts]) and defined($maps->[v_exonstops])){
	my @starts = split /,/, render($maps->[v_exonstarts],\@fields);
	my @stops  = split /,/, render($maps->[v_exonstops],\@fields);

	while(my $start = shift @starts){
	  my $stop = shift @stops;
	  print $fho join "\t", (render($maps->[v_refseq],\@fields),
						render($maps->[v_refsource],\@fields),
						render($maps->[v_refmethod],\@fields),
						$start,
						$stop,
						render($maps->[v_refscore],\@fields),
						render($maps->[v_refstrand],\@fields),
						render($maps->[v_refphase],\@fields),
						render($maps->[v_refmethod],\@fields) . " " . render($maps->[v_refgroup],\@fields)
					   ), "\n";
	}
  }

}

sub toGFF {
  my($line,$fho,$maps) = @_;

  chomp $line; my @fields = split /\t/, $line;

  if(!$maps->[u_qrystart] and !$nolandmark{render($maps->[u_refmethod],\@fields)}){
	print $fho join "\t", map {render($maps->[$_],\@fields)} (u_refseq,
														 u_refsource,
														 u_refmethod,
														 u_refstart,
														 u_refstop,
														 u_refscore,
														 u_refstrand,
														 u_refphase);
	print $fho "\t";
	print $fho "Sequence " . render($maps->[u_refgroup],\@fields);
	print $fho "\n";
  }

  print $fho join "\t", map {render($maps->[$_],\@fields)} (u_refseq,
													   u_refsource,
													   u_refmethod,
													   u_refstart,
													   u_refstop,
													   u_refscore,
													   u_refstrand,
													   u_refphase);
  print $fho "\t";
  print $fho $maps->[u_qrystart] ? "Target " . render($maps->[u_refmethod],\@fields) . ':' : render($maps->[u_refmethod],\@fields) . " ";
  print $fho &render($maps->[u_refgroup],\@fields) . " " .
	         &render($maps->[u_qrystart],\@fields) . " " .
	         &render($maps->[u_qrystop], \@fields);
  print $fho "\n";

  if(defined($maps->[u_starts]) and defined($maps->[u_sizes])){
	my $offset = render($maps->[u_refstart],\@fields) if $relativecoords{render($maps->[u_refmethod],\@fields)};

	my @starts = split /,/, render($maps->[u_starts],\@fields);
	my @sizes = split /,/, render($maps->[u_sizes],\@fields);
	while(defined(my $start = shift @starts)){
	  my $size = shift @sizes;
	  print $fho join "\t", (render($maps->[u_refseq],\@fields),
							 render($maps->[u_refsource],\@fields),
							 render($maps->[u_refmethod],\@fields),
							 $offset + $start,
							 $offset + $start + $size,
							 render($maps->[u_refscore],\@fields),
							 render($maps->[u_refstrand],\@fields),
							 render($maps->[u_refphase],\@fields),
							 render($maps->[u_refmethod],\@fields) . " " . render($maps->[u_refgroup],\@fields)
							), "\n";
	}
  }
}

sub render {
  my($index,$fields) = @_;
  return '.' if $index == -1;
  return $index unless $index =~ /^\d+$/;

  $fields->[$index] =~ s/^chr([0-9XYM]+)$/$1/;
  $fields->[$index] = '+' if $fields->[$index] eq '+-' or $fields->[$index] eq '++';
  $fields->[$index] = '-' if $fields->[$index] eq '-+' or $fields->[$index] eq '--';

  return $fields->[$index];
}
