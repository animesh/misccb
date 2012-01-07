use Math::Matlab;
my $code = q/fprintf( 'Hello world!\n' )/;
if ( $matlab->execute($code) ) {
      print $matlab->fetch_result;
} else {
      print $matlab->err_msg;
}

