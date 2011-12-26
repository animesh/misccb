#!/util/bin/perl

use strict;

use LSFInterface;

# A convenience function to make this example resemble LSFExample.cc
sub getArgs {
	my ($argsref) = @_;
	my %args = (
		'HEAD' => undef,
		'OUT' => undef,
		'START' => undef,
		'END' => undef,
		'JOBS' => 10,
		'BATCH' => 0,
	);

	foreach my $arg (@{$argsref}) {
		my ($key, $value) = $arg =~ /^(.+)=(.+)$/;

		if ($value eq 'False' || $value eq 'false') { $value = 0; }
		elsif ($value eq 'True' || $value eq 'true') { $value = 1; }

		$args{$key} = $value;
	}

	my $undefinedArgs = 0;
	foreach my $key (keys(%args)) {
		if (!defined($args{$key})) {
			print "The argument '$key' is required!\n";
			$undefinedArgs = 1;
		}
	}

	if ($undefinedArgs) { exit(-1); }

	return %args;
}

my %args = getArgs(\@ARGV);

my $lsf = new LSFInterface('outputPath' => $args{'OUT'});
$lsf->setResourceRequirements("rusage[mem=1024]");

if ($args{'BATCH'}) {
	for (my $i = 0; $i < $args{'JOBS'}; $i++) {
		$lsf->submit("$0 HEAD=$args{'HEAD'} OUT=$args{'OUT'} START=" . ($args{'START'} + $i*(($args{'END'}-$args{'START'})/$args{'JOBS'})) . " END=" . ($args{'START'} + ($i+1)*(($args{'END'}-$args{'START'})/$args{'JOBS'}) - 1));
	}
	$lsf->wait();
} else {
	$lsf->execute("../ShortQueryLookup SEQS=$args{'HEAD'}.fastb L=$args{'HEAD'}.reference.lookup O=$args{'OUT'} START=$args{'START'} END=$args{'END'}");
}
