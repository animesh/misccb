
#!/util/bin/perl

$infilename = "/wga/data04/Tetraodon/genoscope.read.lst";
$outfilename = "/wga/data04/Tetraodon/traceinfo/genoscope.xml";

open(INFILE, "$infilename");
open(OUTFILE, ">$outfilename");


print OUTFILE "<?xml version=\"1.0\"?>\n";
print OUTFILE "\t<trace_volume>\n";
#print OUTFILE "\t<volume_name></volume_name>\n";
#print OUTFILE "\t<volume_date>`date`</volume_date>\n";
#print OUTFILE "\t<volume_version></volume_version>\n"; 

$fields{"species_code"}= "Tetraodon nigroviridis";
$fields{"center_name"} = "GSC";


while (<INFILE>) {
  
  chomp;
  
  $name = $_;
  $name =~ /([A-Z]+\d+[A-Z]+)(\d+)([A-DZY]?)([A-P])(\d+)([A-Z]+)(\d+)/;

  $trace_name = $name;
  $quad_name = (defined $3? $3 : "");
  $primer = $6;

  $fields{"trace_name"}  = $trace_name;
  $fields{"library_id"}  = $1;
  $fields{"plate_id"}    = $2;
  $fields{"well_id"}     = "$4$5";
  $fields{"template_id"} = "$1$2$3$4$5";


  #  get the trace end
  if ( $primer eq "C" or $primer eq "F" or $primer eq "XD" or $primer eq "LP" )
  {
    $fields{"trace_end"} = "F";
  }
  else
  {
    $fields{"trace_end"} = "R";
  }

  #  get the insert size and std    
  $1 =~/([A-Z]+)(\d+)([A-Z])([A-Z])/;
  $phys_lib = $4;

  if ( $phys_lib eq "B" ) 
  {
    $fields{"insert_size"} = 154000;
    $fields{"insert_stdev"} = 30000;
  } 
  if ( $phys_lib eq "C" ) 
  {
    $fields{"insert_size"} = 38000;
    $fields{"insert_stdev"} = 4000;
  } 
  if ( $phys_lib eq "A" ) 
  {
    $fields{"insert_size"} = 126000;
    $fields{"insert_stdev"} = 30000;
  } 
  if ( $phys_lib eq "G" ) 
  {
    $fields{"insert_size"} = 3800;
    $fields{"insert_stdev"} = 800;
  } 
  if ( $phys_lib eq "H" ) 
  {
    $fields{"insert_size"} = 2500;
    $fields{"insert_stdev"} = 500;
  } 
  if ( $phys_lib eq "K" ) 
  {
    $fields{"insert_size"} = 2100;
    $fields{"insert_stdev"} = 400;
  } 


  print OUTFILE "\t\t<trace>\n";
  foreach ( 'trace_name',
	    'well_id',
	    'plate_id',
	    'center_name',
	    'template_id',
	    'trace_end',
	    'insert_size',
	    'insert_stdev',
	    'library_id',
	    'species_code') {

    print OUTFILE "\t\t\t<$_>$fields{$_}</$_>\n";

  }
  print OUTFILE "\t\t</trace>\n";


}
print OUTFILE  "\t</trace_volume>";
close(OUTFILE);
close(INFILE);
