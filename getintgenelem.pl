#!/usr/bin/perl
use lib "/scratch/bioperl/";
use Bio::Perl;
use Bio::SeqIO;
my $window=200;
my $jump=180;
my $main_file=shift @ARGV;
#my $fas_file=$main_file."_in.fasta";
#open(FT,">$fas_file");
my ($wseqname,$wseq,$wseqlen)=get_other_source($main_file);
sub get_other_source{
    my $foofile=shift;
    my $seqio_object = Bio::SeqIO->new(-file => $foofile, '-format' => 'GenBank');
    my $seq_object = $seqio_object->next_seq;
    for my $feat_object ($seq_object->get_SeqFeatures) {
		if ($feat_object->primary_tag eq "source") { 
			my $start = $feat_object->location->start;       
			my $end = $feat_object->location->end;
			my $sequence = $feat_object->entire_seq->seq;
			my $length_sequence=length($sequence);
			my $seq_name;
			    for my $tag ($feat_object->get_all_tags) {
				    for my $value ($feat_object->get_tag_values($tag)){
					$seq_name.="$value ";
					}
			    }       
			$seq_name.="$start-$end($length_sequence)";
			$seq_name="$foofile\t".$seq_name;
			return ($seq_name,$sequence,$length_sequence);
			#print FT">$seq_name\n$sequence\n";
		}
    }

}
$file=shift @ARGV;
$fileout=$file.".int.fasta";
open(F,$file);
open(FO,">$fileout");
$lthresh=100;
$profile=$file;
$profile=~s/An$//;
$pro=$profile;
while(<F>){
	chomp;
	@t=split(/\s+/,$_);
	if(@t[3] ne "NA" and @t[4] ne "NA" and @t[3] ne "" and @t[4] ne ""){
		$c++;
		if(@t[3]<@t[4]){
			$mend=@t[3]+@t[2]-1;
			if(@t[2]>$lthresh){
				push(@start,@t[3]);
				push(@end,$mend);
			}
		}
                else{
			$mend=@t[4]+@t[2]-1;
			if(@t[2]>$lthresh){
				push(@start,@t[4]);
				push(@end,$mend);
			}
                }
 	}
	else{
		#print "$_\n";
	}

}


my $sno;
for(my $c=0;$c<=$#start;$c++){
    $sno++;
    $lenstr=length(substr($wseq,@start[$c]-1,abs(@start[$c]-@end[$c])+1));
    print FO">s.$sno.[@start[$c]-@end[$c]].$lenstr.$wseqname\n";
    print FO substr($wseq,@start[$c]-1,abs(@start[$c]-@end[$c])+1),"\n";
    print ">s.$sno.[@start[$c]-@end[$c]].$lenstr.$wseqname\n";
    #print FT substr($wseq,@start[$c]-1,abs(@start[$c]-@end[$c])),"\n";	
}
close FT;

