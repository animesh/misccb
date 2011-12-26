----bioajax.cgi----
#!/usr/bin/perl -w
use strict;
use warnings;
use CGI qw(:standard);  # then only CGI.pm defines a head()
use CGI::Carp 'fatalsToBrowser';
use CGI::Ajax;
use IO::String;
use Bio::SeqIO;
use Bio::DB::GenBank;
my $cgi  = CGI->new();
my $ajax = CGI::Ajax->new( get_sequence => \&get_sequence);
print $ajax->build_html( $cgi, \&main,{-expires=>'now'});

sub get_sequence{
    my ( $id ) = @_;
    my $str;
    my $io = IO::String->new(\$str);
    my $genBank = new Bio::DB::GenBank;
    my $seq = $genBank->get_Seq_by_id($id);
    my $seqOut = Bio::SeqIO->new(-format => 'fasta',-fh => $io );
    $seqOut->write_seq($seq);
    return $str;
}
sub main {
    my $html = <<HTML;
<html><head>
<title>Bioinformatics CGI-Ajax Example</title>
<script type="text/javascript" src="bioajax.js"></script>
</head><body>
<div id="message" style="position:absolute; top:5px; right:5px; font-size:15;border: 1px solid black; background-color: red;color: white;
          width: 175px; height: 20px; overflow: auto; visibility: hidden">
</div>
<h2>Bioinformatics CGI-Ajax Example</h2>
HTML
    my $url = $cgi->url(-relative => 1);
    $html .= <<HTML;
<form>

<table border=1 style="border: 1px solid black" BORDERCOLOR="#33CCFF" bgcolor="#CEE7FF"> 
<tr>
<td ><b>Enter the gi to retrieve sequence</b></td>
<td><input type="text" name="geneid" id="gi"/><button id="fetch">Fetch Sequence</button></td>
</tr>
<tr>
<td><b>Sequence in FASTA Format </b></td>
<td><textarea rows="10" cols="60" name="sequence" id="sequence" bgcolor = "#FFBE4A"/></textarea></td>
</tr>
</table>
</form>
<hr>
</body></html>
HTML
    return $html;
}

exit 0;
__END__

------------------------------------------------------------

-------------bioajax.js------------------------------------

// Run code when the page loads.  From
// http://simon.incutio.com/archive/2004/05/26/addLoadEvent
function addLoadEvent(func) {
  var oldonload = window.onload;
  if (typeof window.onload != 'function') {
    window.onload = func;
  } else {
    window.onload = function() {
      oldonload();
      func();
    }
  }
}

// Set up functions to run when events occur.
function installHandlers() {
  if (!document.getElementById) return;
  var idbox = document.getElementById('fetch');
  var tarea = document.getElementById('sequence');
  if (idbox) {
      // When the user leaves this element, call the server.
      idbox.onclick = function() {
           document.getElementById('message').innerHTML = 'Fetching Sequence...';
	     document.getElementById('message').style.visibility = 'visible'; 
           get_sequence(['gi'], ['sequence']);
           document.getElementById('sequence').style.backgroundColor = '#FFBE4A';
           setTimeout("document.getElementById('message').style.visibility = 'hidden';",2000);
           return false;          // Continue with default action.
      }
  }

}

addLoadEvent( installHandlers );
--------------------------------------------------------
