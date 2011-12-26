#!/usr/bin/perl -w
#
# Calling a BioMoby services (with or without SOAP).
#
# $Id: testing-service.pl,v 1.8 2007/05/09 13:09:53 kawas Exp $
# Contact: Martin Senger <martin.senger@gmail.com>
# -----------------------------------------------------------

BEGIN {
    # some command-line options
    use Getopt::Std;
    use vars qw/ $opt_h $opt_d $opt_v $opt_l $opt_e /;
    getopts ('hdvl:e:');

    # usage
    if ($opt_h or @ARGV == 0) {
	print STDOUT <<'END_OF_USAGE';
Calling a BioMoby services (without using SOAP, just locally).
Usage: # calling a local module representing a service, without using SOAP
       [-vd] [-l <lib-location>] <package-name> [<input-file>]

       It also needs a location of a local cache (and potentially a
       BioMoby registry endpoint). It takes it from the
       'moby-service.cfg' configuration file.

       # calling a real service, using SOAP
       -e <service-url> <service-name> [<input-file>]

    <package-name> is a full name of a called module (service)
        e.g. Service::Mabuhay

    -l <lib-location>
        A directory where is called service stored.
        Default: src/Perl/services   

    -e <service-url>
        A service endpoint
        (e.g. http://localhost/cgi-bin/MobyServer.cgi)

    <input-file>
        A BioMoby XML file with input data.
        Default: an empty BioMoby request

    -v ... verbose
    -d ... debug
    -h ... help
END_OF_USAGE
    exit (0);
    }

    # load modules, depending on the mode of calling
    if ($opt_e) {
	# calling a real service, using SOAP
	eval "use SOAP::Lite; 1;"
	    or die "$@\n";

    } else {
	# calling a local service module, without SOAP
	require FindBin;
	require lib ; lib->import ("$FindBin::Bin/../Perl");
	# TBD: perhaps take this from the config file?
	require lib ; lib->import ("$FindBin::Bin/../Perl/services");
	require lib ; lib->import ("$FindBin::Bin/../Perl/generated");
	unshift (@INC, $opt_l) if $opt_l;
	eval "use MOSES::MOBY::Base; 1;";
	$LOG->level ('INFO') if $opt_v;
	$LOG->level ('DEBUG') if $opt_d;
    }
}

use strict;

sub _empty_input {
    return <<'END_OF_XML';
<?xml version="1.0" encoding="UTF-8"?>
<moby:MOBY xmlns:moby="http://www.biomoby.org/moby">
  <moby:mobyContent>
    <moby:mobyData moby:queryID="job_0"/>
  </moby:mobyContent>
</moby:MOBY>
END_OF_XML
}

# --- what service to call
my $module = shift;   # eg. Service::Mabuhay, or just Mabuhay
my $service;
($service = $module) =~ s/.*:://;

# --- call the service
if ($opt_e) {
    # calling a real service, using SOAP
    my $soap = SOAP::Lite
	-> uri ("http://biomoby.org/")
	-> proxy ($opt_e)
	-> on_fault (sub {
	    my $soap = shift;
	    my $res = shift;
	    my $msg =
		ref $res ? "--- SOAP FAULT ---\n" . $res->faultcode . " " . $res->faultstring
		: "--- TRANSPORT ERROR ---\n" . $soap->transport->status . "\n$res\n";
	    die $msg;
	});

    my $input = '';
    if (@ARGV > 0) {
	my $data = shift;     # a file name
	open INPUT, "<$data"
	    or die "Cannot read '$data': $!\n";
	while (<INPUT>) { $input .= $_; }
	close INPUT;
    } else {
	$input = _empty_input;
    }

    print $soap
	-> $service (SOAP::Data->type('string' => "$input"))
        -> result;

} else {
    # calling a local service module, without SOAP
    my $data;
    if (@ARGV > 0) {
	$data = shift;     # a file name
    } else {
	use File::Temp qw( tempfile );
	my $fh;
	($fh, $data) = tempfile (UNLINK => 1);
	print $fh _empty_input();
	close $fh;
    }
    eval "require $module" or croak $@;
    eval {
	my $target = new $module;
	print $target->$service ($data), "\n";
    } or croak $@;
}

__END__
