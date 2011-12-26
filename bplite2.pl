#!/usr/bin/perl

# PROGRAM  : bplite.pl ..modified from bptutorial.pl
# PURPOSE  : parses blast results and prints them in tabular format
# AUTHOR   : Andrew Lynn...modified from Peter Schattner schattner@alum.mit.edu
# CREATED  : June 2001

use strict;
use Bio::SimpleAlign;
use Bio::AlignIO;
use Bio::SeqIO;
use Bio::Seq;
use Bio::Tools::BPlite::Sbjct; # we want to use Sbjct
use Bio::Index::Fasta;
#use HTML::TreeBuilder;
#use HTML::FormatText;

my ($filename,$bp_parse_file1,$bp_parse_file2,$html,$formatter,$lth,$out,
     $query,$hit,$score,$P,$bits,$percent,$match,$positive);
print " Blast filename? ";
$filename = <STDIN>;
open (FILENAME,$filename) || die " cannot open $filename: $!";
print " O/P filename? ";
$out = <STDIN>;
chomp $out;
open (OUT,">$out") || die " cannot open $out: $!";
#write OUT; #header
# while ($bp_parse_file1 = <FILENAME>|| die "cannot append: #!")
 {
#some changes have to be made if your file is in html....to convert it to a format (text) that bplite.pm can parse
#ignore (comment out) this section if your blast o/p is in text
 #       chomp ($bp_parse_file1);
#	$bp_parse_file1=$bp_parse_file1.".blastx";
#	$bp_parse_file1=~s/ //g;
#	print " $bp_parse_file1,eol\n ";
#	$html = HTML::TreeBuilder->new->parse_file($bp_parse_file1);
#	print "the o/p of html is $html ";
#	open (TEMP,">temp");
  #       $formatter = HTML::FormatText->new(leftmargin => 0, rightmargin => 80);
#	 $bp_parse_file2 = $formatter->format($html);
#	 $_="$bp_parse_file2";
#	 $_ = tr/(2)// ;
#	$bp_parse_file2=~ s/Expect\(2\)/Expect/g;
  #      print TEMP $bp_parse_file2;
#	close (TEMP);
              bplite_parsing($filename);
                        $bp_parse_file2=''
 }
 close (FILENAME);

#########################################
#  bplite_parsing ():
#
 # arguments are sent into array @_
sub bplite_parsing {

    use Bio::Tools::BPlite;
    my ($file1, $file2, $file3, $report,$report2 , $report3 ,
	$last_iteration, $sbjct,  $total_iterations, $hsp,
	$matches, $loop, $hit,$score,$P,$bits,$percent,$match,$positive,
	$query,$query1,$namehandle,$start,$end);
     $file1 = shift;

#    print "\nBeginning bplite, bppsilite, bpbl2seq parsing example... \n";
#    open (OUT,">>$out") || die " cannot open $out: $!";

#    ($file1) = ("temp");
    #open FH, "t/blast.report";
#    print $bp_parse_file2;
    $report = Bio::Tools::BPlite->new(-file=>$file1);
#	use Bio::Tools::Blast::HTML qw(&strip_html);
#	&strip_html(\$report);

#     while($sbjct = $report->nextSbjct) {
#     print "$sbjct\n";
#     while($hsp = $sbjct->nextHSP) {
#                print "\t$hsp\n";
#     }
# }
   while ($sbjct = $report->nextSbjct) {
        $hit=$sbjct->name;
        $hit =~ s/ .*//;
#        print " Hit name is ", $sbjct->name, "\n";
   while ($hsp = $sbjct->nextHSP) {

 $score=$hsp->score;

 $P=$hsp->P;
 $bits=$hsp->bits;
 $percent=$hsp->percent;
 $match=$hsp->match;
 $positive=$hsp->positive;
 $start=$hsp->subject->start;
 $end=$hsp->subject->end;
 fetch_parsing( $hit,$start,$end);
 #print "score ",$hsp->score,"\n";
 #print "P ",$hsp->P,"\n";
 #print "bits ",$hsp->bits,"\n";
 #print "percent ",$hsp->percent,"\n";
 #print "match ",$hsp->match,"\n";
 #print "positive ",$hsp->positive,"\n";
 #print "length ",$hsp->length,"\n";
 #print "querySeq ",$hsp->querySeq,"\n"; #     $hsp->qs;
 #print "sbjctSeq ",$hsp->sbjctSeq,"\n"; #     $hsp->ss;
 #print "homologySeq ",$hsp->homologySeq,"\n"; #  $hsp->hs;
 #print "query->start ",$hsp->query->start,"\n";
 #print "query->end ",$hsp->query->end,"\n";
 #$query = $hsp->query->seqname;

#  $query =~ s/pfcon-//;
 #print "\n\n query  $query \n query1 $query1\n\n";
 #if ($query ne $query1){
 #$namehandle = select OUT;
 #$~="IN";
 #write OUT;
 #$query1 = $query;
 #$~ = "OUT";
 #}
# if ($P<= 0**-5){
#	write OUT;
#}
#
#else{
#     print " $query not eq to $query1 ";
#     $namehandle = select OUT;
#     $~ = "FIRST";
#     write OUT;
#     $query1 = $query;
#     $~ = "OUT";
#     $- = $- - 2;
#     select ($namehandle);
#}
#print "query->seqname ",$hsp->query->seqname,"\n\n\n";
# these commands have a different subroutine call (subject instead of sbjct..?)
## print "sbjct->primary_tag ",$hsp->sbjct->primary_tag,"\n"; # "similarity"
## print "sbjct->source_tag ",$hsp->sbjct->source_tag,"\n";  # "BLAST"
 ##print "sbjct->start ",$hsp->subject->start,"\n";
## print "sbjct->end ",$hsp->subject->end, "\n";
#format OUT_TOP =
#Page @<<
#$%
#
#Sequence                 HIT                        Score   E       bits  %    id     +    start   end
# ============ ====================================== ===== ====== ===== ==== ==== ==== ===== =====
#
#format IN =
#@<<<<<<<<<<<<<<<<<<<<<<<
#$query
#.
#format FIRST =
#
#@<<<<<<<<<<< ^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< @>>>> @>>>>> @>>>> @>>> @>>> @>>> @>>>> @>>>>
#$query,$hit,$score,$P,$bits,$percent,$match,$positive,$start,$end
#~~           ^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
 #            $hit
#.
#format OUT =
#             ^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< @>>>> @>>>>> @>>>> @>>> @>>> @>>>  @>>>> @>>>>
#$hit,$score,$P,$bits,$percent,$match,$positive,$start,$end
#~~            ^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#              $hit
#.
}
}
    return 1;
} ;


 #########################################
#  fetch_parsing:
#@_ contains the arguments
 sub fetch_parsing {
 my ( $Index_File_Name, $inx2, $id, $dir, $key,
	 $keyfound, $seq, $indexhash,$name,$start,$end,$trunc,$trans,$truncout);
#rint "fetch what? ";chomp($name=<STDIN>);
#id=$name;
$name = shift;
$start = shift;
$end = shift;
use Bio::Index::Fasta; # using fasta file format
$out = Bio::SeqIO->new('-fh' =>' ehptn.out ', -format => 'Fasta');
 $truncout = Bio::SeqIO->new('-fh' => 'ehnuc.out ', -format => 'Fasta');
    print "\nBeginning retrieving local_db example for <$name> start: $start end: $end... \n";
$id=$name;
    # then retrieve some files
    #$Index_File_Name = shift;
    $Index_File_Name = 'ehindex';

#    eval { use Cwd; $dir = cwd; };
    # CWD not installed, revert to unix behavior, best we can do
     $dir = '/home/anchal/download/blast/data';

    $inx2 = Bio::Index::Abstract->new
	('-FILENAME'  => "$dir/$Index_File_Name");

#    $indexhash = $inx2->db();
#    $keyfound = "";
#    foreach $key (sort keys %$indexhash) {
#	if($key =~ /$name/) {$keyfound = 'true' ; $id = $key; last};
 #   }
 #   if ($keyfound) {
#        $id = "excluded_7";
        $seq = $inx2->fetch($id);
	print "Sequence ", $seq->display_id();
	" has length  ", $seq->length()," \n";
         "complete sequence is...\n" ;
   $trunc = $seq->trunc($start,$end);
        $out->write_seq($trans);
# use subseq / trunc to generate a new seq object containing the required portion of the original sequence
   $trans= $trunc->translate();
     $truncout->write_seq($trunc );
         $out->write_seq($trans);
#    unlink "$dir/$Index_File_Name" ;
#  return $dir;
 # }
  }
