#!usr/bin/perl
#use strict;
use LWP::UserAgent;
#print "ENTER THE 4 LETTER PDB ID: ";
# my $pdb_id=<STDIN>;
open(F,"pdbid.txt");
while($line=<F>)
{
if($line ne ""){
$pdb_id=$line;
 chomp $pdb_id;
print $pdb_id; 
$pdb_id=~ tr/a-z/A-Z/;#CONVERTS TO UPPERCASE
 print "\nYOU HAVE ENTERED THE FOLLOWING PDB ID:$pdb_id\n";
 my $localfile=$pdb_id.'.pdb';
 unless (open(OUT,">$localfile"))
{print "ERROR IN OPENING FILE  $localfile FOR WRITING:
$!\n";exit;}
 my
$web_site='http://www.rcsb.org/pdb/cgi/export.cgi/';
 my
$full_path=$web_site.$pdb_id.'.pdb?format=PDB&pdbId='.$pdb_id.'&compression=None';
 
my $ua=new LWP::UserAgent;
#$ua->mirror($full_path,$localfile);
my $request = new HTTP::Request('GET', $full_path);
my $response = $ua->request($request);
my $content= $response->content;
unless($response->is_success)
    { die "$full_path,$response->error_as_HTML\n";}
 print OUT $content;
close (OUT);}
}
close F;
