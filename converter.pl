#!/ebi/extserv/bin/perl/bin/perl


########################################################
# Search the right lib directory
########################################################
my $PROG_DIR;
BEGIN
{
    unless($ENV{IPRSCAN_HOME}) {
$ENV{IPRSCAN_HOME} ='/scratch/tmp/iprscan';
    }

    unless($ENV{IPRSCAN_LIB}){
      $ENV{IPRSCAN_LIB} = "$ENV{IPRSCAN_HOME}/lib";
    }
}



########################################################
# GLOBAL VARIABLES, MODULES and LIBRARIES
########################################################
use strict;
use Getopt::Long;
use lib $ENV{IPRSCAN_LIB};
use Index::InterPro;
use Dispatcher::Tool::InterProScan;
use Data::Dumper;
use XML::Quote;
########################################################


my $databases = {
                 'fprintscan'  => 'PRINTS',
                 'profilescan' => 'PROFILE',
     'pfscan'      => 'PROFILE',
                 'blastprodom' => 'PRODOM',
                 'hmmsmart'    => 'SMART',
     'hmmpanther'  => 'PANTHER',
                 'hmmpfam'     => 'PFAM',
                 'hmmtigr'     => 'TIGRFAMs',
                 'scanregexp'  => 'PROSITE',
                 'coils'       => 'COIL',
                 'seg'         => 'SEG',
                 'tmhmm'       => 'TMHMM',
                 'signalp'     => 'SIGNALP',
                 'hmmpir'      => 'PIR',
                 'superfamily' => 'SUPERFAMILY',
     'gene3d'      => 'GENE3D',
     'hmmpanther'  => 'PANTHER'
                };
my $dbtype = {
        'fprintscan'  => 'matrix',
        'profilescan' => 'strings',
        'pfscan'      => 'strings',
        'blastprodom' => 'sequences',
        'hmmsmart'    => 'model',
        'hmmpanther'  => 'model',
        'hmmpfam'     => 'model',
        'hmmtigr'     => 'model',
        'scanregexp'  => 'strings',
        'coils'       => 'matrix',
        'seg'         => 'NA',
        'tmhmm'       => 'model',
        'signalp'     => 'model',
        'hmmpir'      => 'model',
        'superfamily' => 'model',
        'gene3d'      => 'model',
        'hmmpanther'  => 'model'
       };

my $iprscan;

########################################################
# sub usage
# Prints usage on STDERR and exit.
########################################################

sub usage {
  my $mess = shift;
  print STDERR "\n$0 Error: $mess\n\n" if($mess);
  print STDERR "Usage: $0 -format <xml|ebixml|raw|html|txt> -input <raw file> -jobid <iprscan-YYYYMMDD-hhmmssXX> > outfile\n\n";
  print STDERR "          -format Output file formats. ebixml format add an EBI header on the top of the xml file.\n";
  print STDERR "          -input  Input raw file to convert.\n";
  print STDERR "          -jobid  Jobid of the InterProScan run. Mandatory for html and ebixml output format.\n\n";
  exit 1;

}

########################################################
# sort the matches by ascending order of the start
# position (example of format: "T[374-388] 0.6224")
########################################################
sub sortByStartPosition
{
  my ($vara, $varb) = (0, 0);

  ($vara)=($a=~/^[^\d]+(\d+)-/);
  ($varb)=($b=~/^[^\d]+(\d+)-/);
  return($vara <=> $varb);
}  # end sortByStartPosition


########################################################
# PrintGOTerms
########################################################
sub PrintGOTerms
{
  local (*FILE) = shift;
  my ($goterms) = @_;   #eg: $goterms="#Molecular Function: structural constituent of ribosome (GO:0003735), Cellular Component: intracellular (GO:0005622)"

  my @F = split(",", $goterms);
  foreach my $goterm ( @F )
  {
    next if ( $goterm !~ /^\s*([^:]+)\s*:\s*([^\(]+)\s*\((GO:\w+)\)\s*$/ ); # unparsable go term (should not append!)
    my ($GO_category, $GO_descr, $GO_id) = ($1, $2, $3);
    #print STDERR "#$goterm# ($GO_category, $GO_descr, $GO_id)\n";
    $GO_category =~ s/^\s+//; $GO_category =~ s/\s+$//;
    $GO_descr =~ s/^\s+//; $GO_descr =~ s/\s+$//;
    $GO_id =~ s/^\s+//; $GO_id =~ s/\s+$//;
    
    print FILE "\t  <classification id=\"$GO_id\" class_type=\"GO\">\n";
    print FILE "\t    <category>$GO_category</category>\n";
    print FILE "\t    <description>$GO_descr</description>\n";
    print FILE "\t  </classification>\n";
  }
}  # end PrintGOTerms

########################################################
# getHeader
########################################################
sub getHeader{

  my $code = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n";
  $code .= "<EBIInterProScanResults xmlns=\"http://www.ebi.ac.uk/schema\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:noNamespaceSchemaLocation=\"http://www.ebi.ac.uk/schema/InterProScanResult.xsd\">\n";
  $code .= "\t<Header>\n";
  $code .= "\t\t<program name=\"InterProScan\" version=\"4.0\" citation=\"PMID:11590104\" />\n";

  my @appl = split(/,/, $iprscan->getParam('appl'));
  my $totaldb = scalar(@appl);
  my $nseqs = $iprscan->getParam('nseqs');
  $code .= "\t\t<parameters>\n";
  $code .= "\t\t\t<sequences total=\"$nseqs\" />\n";
  $code .= "\t\t\t<databases total=\"$totaldb\">\n";
  my $dbcnt = 0;
  for(@appl){
    $dbcnt++;
    my($name, $type) = ($databases->{$_}, $dbtype->{$_});
    $code .= "\t\t\t\t<database number=\"$dbcnt\" name=\"$name\" type=\"$type\" />\n";
  }
  $code .= "\t\t\t</databases>\n";
  $code .= "\t\t</parameters>\n";

  $code .= "\t</Header>\n";

  return $code;
}

########################################################
# getBottom
########################################################
sub getBottom{

  return "</EBIInterProScanResults>\n";
}


########################################################
# MAIN
########################################################
my ($format, $input, $jobid, $pfile, $res, $msg);
my $params = { };

unless(GetOptions('format:s' => \$format,
      'input:s'  => \$input,
      'jobid:s'  => \$jobid)){
  usage();
}

unless($format && $input && -f $input && $jobid){
    usage();
}

my %results;
my %seq_info;
my %appl;
my %ipr_classification;
my $go;


if($format eq 'ebixml' || $format eq 'html'){
  usage("Jobid not defined") unless($jobid);
  unless($jobid =~ /([A-Za-z]+-[\d]{8}-[\d]{8})/){
    die '"', __FILE__, '"', ", line \"", __LINE__, "\" Wrong jobid [$jobid].\nJobid must be like 'iprscan-YYYYDDMM-hhmmssXX.'";
  }

  $iprscan = new Dispatcher::Tool::InterProScan('iprscan');
  ($res, $msg) = $iprscan->parseConfig(1, {jobid => $jobid});
  die '"', __FILE__, '"', ", line \"", __LINE__, "\" $msg" unless $res;

  $pfile = $iprscan->findFile('toolparams');

  die '"', __FILE__, '"', ", line \"", __LINE__, "\" No toolparams configured" unless $pfile;
  die '"', __FILE__, '"', ", line \"", __LINE__, "\" $pfile does not exist" unless(-f $pfile);

  ($res, $msg) = $iprscan->readParamFile($pfile, $params);
  die '"', __FILE__, '"', ", line \"", __LINE__, $msg unless $res;

  $iprscan->setParams($params);
}

unless($format =~ /^htm/){
########################################################

# ori \S+
#reading input
open(IN,"< $input") || usage("can't open input file $input : $!\n");
while (<IN>) 
{
#EQ was my ($seq_ac, $seq_crc, $seq_len, $_raw, $meth, $beg, $ipr) = /(\S+)\s+(\S+)\s+(\d+)\s+((\S+)\s+\S+\s+\S+\s+(\d+)\s+\d+\s+\S*\s*\S+\s+\d+\-\S+\-\d+\s*(\S+)*\s*([^\n]+)?)/;
  my ($seq_ac, $seq_crc, $seq_len, $_raw, $meth, $beg, $ipr) = /(\S+)\s+(\S+)\s+(\d+)\s+((\S+)\s+\S+\t[\w\/\[\]\(\)\.\:\"\'\+\=\,\;\-\& ]*\t(\d+)\s+\d+\s+\S*\s*\S+\s+\d+\-\S+\-\d+\s*(\S+)*\s*([^\n]+)?)/;
  my $key;
  $appl{$seq_ac}{$meth} = 1 if(!exists($appl{$seq_ac}{$meth}));########### 07-10-03
  if (defined $ipr)
  {
    $key = $ipr;
  }
  else
  {
    $key = $meth;
  }
  
  if (defined $results{$seq_ac}{$key})
  {
    $results{$seq_ac}{$key} .= "\n". $_raw;
  }
  else
  {
    $results{$seq_ac}{$key} = $_raw ;
  }
  
  # $seq_ac is supposed to be unique
  $seq_info{$seq_ac}{'crc'} = $seq_crc;
  $seq_info{$seq_ac}{'len'} = $seq_len;
}
close(IN);


#parsing input
foreach my $ac (sort keys %results)
{
  foreach my $key (sort keys %{$results{$ac}})
  {
    if ($key =~ /^(IPR|NULL)/)  #if interpro
    {
      #EQ was $results{$ac}{$key} =~ /\S+\s+\S+\s+\S+\s+\d+\s+\d+\s+\S*\s*\S+\s+\d+\-\S+\-\d+\s+\S+\s+([^\n\t]+)(\t+([^\n\t]+))?/;
      $results{$ac}{$key} =~ /\S+\s+\S+\t[\w\/\[\]\(\)\,\;\.\:\"\'\+\=\-\& ]*\t\d+\s+\d+\s+\S*\s*\S+\s+\d+\-\S+\-\d+\s+\S+\s+([^\n\t]+)(\t+([^\n\t]+))?/;
      #print STDERR "\n\n##$1##  ##$2##  ##$3## (-1-$results{$ac}{$key})\n";
      my ($ipr_mapping, $go_mapping_long, $go_mapping_short) = ($1, $2, $3);
      
      my (%ipr) = ('name' => "$ipr_mapping");
      if ( defined($go_mapping_short) )
      {
        $ipr{'class'} = "$go_mapping_short";
        $go = 1;
      }
      
      my (%meth) = ();
      #EQ was ( $results{$ac}{$key} =~ /(\S+)\s+(\S+\s+\S+\s+\d+\s+\d+\s+\S*\s*\S+\s+\d+\-\S+\-\d+)/g )
      while ( $results{$ac}{$key} =~ /(\S+)\s+(\S+\t[\w\/\[\]\(\)\.\:\"\'\+\=\,\;\-\& ]*\t\d+\s+\d+\s+\S*\s*\S+\s+\d+\-\S+\-\d+)/g )
      {
        #FS ##HMMPfam##  ##PF00512  signal  230     296     1.1e-12 T        3-Oct-2002##
        #print STDERR "\n\n##$1##  ##$2## (-2-$results{$ac}{$key})\n";
        my ($method, $match) = ($1, $2);
        
        if ( defined($meth{$method}) )
        {
          $meth{$method} .= "\n". "$match";
        }
        else
        {
          $meth{$method} = "$match";
        }
      }
      
      foreach my $m (sort keys %meth)
      {
        my %hits;
        #print STDERR "---->#$m#\n";
        #EQ was $meth{$m} =~ /(\S+)\s+(\S+)\s+(\d+)\s+(\d+)\s+(\S*)\s*(\S+)\s+(\d+\-\S+\-\d+)/g)
        while ($meth{$m} =~ /(\S+)\t([\w\/\[\]\(\)\.\:\"\'\+\=\,\;\-\& ]*)\t(\d+)\s+(\d+)\s+(\S*)\s*(\S+)\s+(\d+\-\S+\-\d+)/g)
        {
          #FS ##PR00344##     ##BCTRLSENSOR##         ##406##         ##424##         ##0.000324##    ##T##   ##3-Oct-2002## 
          my ($methAC, $methName, $start, $end, $methScore, $status, $date) = ($1, $2, $3, $4, $5, $6, $7);
          #print STDERR " ##$methAC## \t ##$methName## \t ##$start## \t ##$end## \t ##$methScore## \t ##$status## \t ##$date## \n\n(-3-$meth{$m})\n\n";
          
          my $hit_desc = "${status}\[$start-$end]";
          $hit_desc .= " $methScore" if ( defined($methScore) && ($methScore !~ /^\s*$/) );
          if ( defined($hits{$methAC}) )  # hit_ac is uniq
          { 
            push(@{$hits{$methAC}{'loc'}}, "$hit_desc");
            #print STDERR "#$hit_desc#\n";
          }
          else
          {
            $hits{$methAC} = {'name' => "$methName", 'loc' => [$hit_desc]};
            #print STDERR "->method=$methName hit=#$hit_desc#\n";
          }
        }
        #print STDERR "#-#$m#-#\n";
        $ipr{'meth'}{$m} = \%hits;
      }
      #print STDERR "#*#$ac#*# ($key)\n";
      $results{$ac}{$key} = \%ipr;
    }
    else
    {
      my %hits;
      while ($results{$ac}{$key} =~ /(\S+)\s+(\S+)\t([\w\/\[\]\(\)\.\:\"\'\+\=\,\;\-\& ]*)\t(\d+)\s+(\d+)\s+(\S*)\s*(\S+)\s+(\d+\-\S+\-\d+)/g )
      {
        #EQ was $results{$ac}{$key} =~ /(\S+)\s+(\S+)\s+(\S+)\s+(\d+)\s+(\d+)\s+(\S*)\s*(\S+)\s+(\d+\-\S+\-\d+)/g)
        #FS 
        #print STDERR "NO IPR ##$1##  ##$2##  ##$3##  ##$4##  ##$5##  ##$6##  ##$7##  \n\n(-4-$results{$ac}{$key})\n\n\n";
        
        if (defined $hits{$2})  #hit_ac is uniq
        {
          push(@{$hits{$2}{'loc'}}, "${7}\[$4-$5\] $6");
        }
        else
        {
          $hits{$2} = {'name' => $3, 'loc' => ["${7}[$4-$5] $6"]};    
        }
      }
      $results{$ac}{$key} = \%hits;
    }
  }
      }

} #end unless($format =~ /^htm/)
##### done - all data in memmory.
# either:
#    'PPsearch' => {
#        'PS00017' => {
#           'name' => 'ATP_GTP_A',
#           'loc' => [
#                '?[144-151]'
#              ]
#         },
#        'PS00021' => {
#           'name' => 'KRINGLE_1',
#           'loc' => [
#                'T[82-87]'
#              ]
#         }
#      },
# or (linked to InterPro):
#    'IPR001687' => {
#         'name' => 'ATP/GTP-binding site motif A (P-loop)',
#         'meth' => {
#         'PPsearch' => {
#             'PS00017' => {
#                      'name' => 'ATP_GTP_A',
#                      'loc' => [
#                     '?[144-151]'
#                   ]
#                    }
#                 }
#             }
#       },
# 
# dumping out
#print STDERR Dumper(%results);
#exit;

#################################
if ( $format =~ /txt/ )
{
  #################################
  print "No significant hits reported.\n" unless (%results);
  
  foreach my $seq (sort keys %results)
  {
    my $onece = 0;
    print   "Sequence \"$seq\"".    
      " crc64 checksum: $seq_info{$seq}{'crc'}".
      " length: $seq_info{$seq}{'len'} aa.".
      "\n\n";

    foreach my $k (sort keys %{$results{$seq}})
    {
      if ($k =~ /^(IPR|NULL)/)
      {
        printf "%-15s%-15s%s\n", "InterPro", $k, $results{$seq}{$k}{'name'};
        printf "$results{$seq}{$k}{'class'}\n" if ($results{$seq}{$k}{'class'});
        printf "%-15s%-15s%-40s%s", "method", "AccNumber", "shortName", "location\n";
        foreach my $mm (sort keys %{$results{$seq}{$k}{'meth'}})
        {
          foreach my $hit (sort keys %{$results{$seq}{$k}{'meth'}{$mm}})
          {
            printf "%-15s%-15s%-40s%s\n", $mm, $hit, $results{$seq}{$k}{'meth'}{$mm}{$hit}{'name'}, join(" ",@{$results{$seq}{$k}{'meth'}{$mm}{$hit}{'loc'}});
          }
        }
        print "\n";
      }
      else
      {
        printf "%-15s%-15s%-40s%s", "method", "AccNumber", "shortName", "location\n" if ($onece ++ ==0);
        foreach my $hit (sort keys %{$results{$seq}{$k}})
        { 
          printf "%-15s%-15s%-40s%s\n", $k, $hit, $results{$seq}{$k}{$hit}{'name'}, join(" ",@{$results{$seq}{$k}{$hit}{'loc'}});
        }
        print "\n";
      }
    }
  }
  #################################
}
elsif ($format =~ /xml/)
{
  #################################

    print getHeader() if($format eq 'ebixml');

    print "<interpro_matches>\n\n";
    foreach my $seq (sort keys %results)
      {
    print   "   <protein id=\"$seq\" length=\"$seq_info{$seq}{'len'}\" crc64=\"$seq_info{$seq}{'crc'}\" >\n";

    foreach my $k (sort keys %{$results{$seq}})
      {
    if ($k =~ /^IPR/)
      {
          my($res, $msg, $inx, $entries, $type);

          ($res, $inx) = Index::InterPro->new(undef, 1);
          die '"', __FILE__, '"', ", line \"", __LINE__, "\" Index::InterPro : $inx" unless $res;

          ($res, $entries) = $inx->getEntry($k);
          die '"', __FILE__, '"', ", line \"", __LINE__, "\" $entries" unless $res;

          if($entries && ref($entries) eq 'ARRAY'){
        foreach my $entry (@$entries){

            my($childs, $contains, $founds, $parents);

            ($res, $msg) = $inx->parseFields(\$entry);
            die $msg unless $res;

            ($res, $type) = $inx->get_type();
            die '"', __FILE__, '"', ", line \"", __LINE__, "\" $type" unless $res;
            $type = $type->[0] if($type);   #getfield returns an array ref. 'type' field is unique in an interpro entry

            print "\t<interpro id=\"$k\" name=\"$results{$seq}{$k}{'name'}\" type=\"$type\"";
            ($res, $parents) = $inx->get_parent();
            die '"', __FILE__, '"', ", line \"", __LINE__, "\" $parents" unless $res;

            $parents = $parents->[0] if($parents);#  get_parent returns an array ref. 'parent' field is unique, an interpro entry cannot have more than one parent.
            if($parents ne ""){
          print " parent_id=\"$parents\"";
            }
            print ">\n";
            ######################################################################################
            ##### Display the children, found_in and contain for each InterPro entries.#####
            ###
            ($res, $childs) = $inx->get_child();
            die '"', __FILE__, '"', ", line \"", __LINE__, "\" $childs" unless $res;

            if($childs && ref($childs) eq "ARRAY"){
          print "\t  <child_list>\n";
          foreach my $child (@{$childs}){
              print "\t    <rel_ref ipr_ref=\"$child\"\/>\n";
          }
          print "\t  </child_list>\n";
            }
            ###
            ($res, $founds) = $inx->get_found();
            die '"', __FILE__, '"', ", line \"", __LINE__, "\" $founds" unless $res;

            if($founds && ref($founds) eq "ARRAY"){
          print "\t  <found_in>\n";
          foreach my $found (@{$founds}){
              print "\t    <rel_ref ipr_ref=\"$found\"\/>\n";
          }
          print "\t </found_in>\n";
            }
            ###
            ($res, $contains) = $inx->get_contains();
            die '"', __FILE__, '"', ", line \"", __LINE__, "\" $contains" unless $res;

            if($contains && ref($contains) eq "ARRAY"){
          print "\t  <contains>\n";
          foreach my $contain (@{$contains}){
              print "\t    <rel_ref ipr_ref=\"$contain\"\/>\n";
          }
          print "\t  </contains>\n";
            }
            ###
            ##############################################################################
            # Add GO classification to the xml output:
            &PrintGOTerms("STDOUT", $results{$seq}{$k}{'class'}) if ( $go );

            # vsi 2003-12-15 added XML::Quote::xml_quote call because name field
            # contains illegal characters < > ' " & which make XML parser fail.
            my $name;

            foreach my $mm (sort keys %{$results{$seq}{$k}{'meth'}})
        {
            foreach my $hit (sort keys %{$results{$seq}{$k}{'meth'}{$mm}})
              {
            $name = xml_quote($results{$seq}{$k}{'meth'}{$mm}{$hit}{'name'});
            print "\t  <match id=\"$hit\" name=\"$name\" dbname=\"",$inx->getDBname($mm),"\">\n";
            foreach my $l (@{$results{$seq}{$k}{'meth'}{$mm}{$hit}{'loc'}})
              {
            my ($sts,$beg,$end,$scr) = ($l =~ /(\S+)?\[(\d+)\-(\d+)\]\s*(\S+)?/);
            $end = "$end\" score=\"$scr" if ( defined($scr) );
            print "\t    <location start=\"$beg\" end=\"$end\" status=\"$sts\" evidence=\"$mm\" />\n";
              }
            print "\t  </match>\n";
              }
        }
            print "\t</interpro>\n";
        }
          }
      }
    elsif ($k =~ /^NULL/)
      {
          print "\t<interpro id=\"noIPR\" name=\"unintegrated\" type=\"unintegrated\">\n";

          # vsi 2003-12-15 added XML::Quote::xml_quote call for name
          my $name;

          foreach my $mm (sort keys %{$results{$seq}{$k}{'meth'}})
      {
          foreach my $hit (sort keys %{$results{$seq}{$k}{'meth'}{$mm}})
            {
          $name = xml_quote($results{$seq}{$k}{'meth'}{$mm}{$hit}{'name'});
          print "\t  <match id=\"$hit\" name=\"$name\" dbname=\"", Index::InterPro->getDBname($mm), "\">\n";
          foreach my $l (@{$results{$seq}{$k}{'meth'}{$mm}{$hit}{'loc'}})
            {
          my ($sts,$beg,$end,$scr) = ($l =~ /(\S+)?\[(\d+)\-(\d+)\]\s*(\S+)?/);
          $end = "$end\" score=\"$scr" if ( defined($scr) );
          print "\t    <location start=\"$beg\" end=\"$end\" status=\"$sts\" evidence=\"$mm\" />\n";
            }
          print "\t  </match>\n";
            }
      }
          print "\t</interpro>\n";
      }
    else
      {
          foreach my $hit (sort keys %{$results{$seq}{$k}})
      {
      		# fixed XML output by Martin Szugat on 06/06/2005.
          my $name;
          $name = xml_quote($results{$seq}{$k}{$hit}{'name'});
          print "\t  <match id=\"$hit\" name=\"$name\" dbname=\"", Index::InterPro->getDBname($k), "\">\n";
          foreach my $l (@{$results{$seq}{$k}{$hit}{'loc'}})
            {
          my ($sts,$beg,$end,$scr) = ($l =~ /(\S+)?\[(\d+)\-(\d+)\]\s*(\S+)?/);
          $end = "$end\" score=\"$scr" if ( defined($scr) );
          print "\t    <location start=\"$beg\" end=\"$end\" status=\"$sts\"  evidence=\"$k\" />\n";
            }
          print "\t  </match>\n";
      }
      }
      }
    print "   </protein>\n";
      }
    print "\n</interpro_matches>\n";
    print getBottom() if($format eq 'ebixml');
    #################################
}
elsif ($format =~  /^htm/) 
  {
      #################################

      #To create html view we use Dispatcher::Tool::InterProScan package which
      #parses the xml file.

      my $xfile = $iprscan->getConfigValue('toolxml');
      die '"', __FILE__, '"', ", line \"", __LINE__, "\" No toolxml configured" unless $xfile;
      die '"', __FILE__, '"', ", line \"", __LINE__, "\" $xfile does not exist" unless(-f $xfile);

      $iprscan->setParam('view', 'picture');
      $iprscan->setParam('cmd', 'check');

      ($res, $msg) = $iprscan->createResultPicture($xfile);
      die '"', __FILE__, '"', ", line \"", __LINE__, "\" $msg" unless $res;
      ($res, $msg) = $iprscan->createResultTable($xfile);
      die '"', __FILE__, '"', ", line \"", __LINE__, "\" $msg" unless $res;
  }
elsif($format =~ /^raw/){
    #do nothing...
}

exit;
