use CGI qw/:standard/;
print header(),
      start_html(-title=>'Wow!'),
      h1('Wow!'),
      'Look Ma, no hands!',
      end_html();

