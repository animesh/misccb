#!/usr/bin/perl
##
#               A R C H I V E R . P L
#
# this script is forking deamon which accepts connections from
# perfcollect clients and generates html pages and graphs that
# display performance information collected by the perfcollect
# client
#
# Author: Stefano Santoro
##

use 5.005;
use strict;
use vars qw( $VERSION $DONE %CHILDREN);

use File::Basename;
use lib dirname($0), dirname($0) . "/../perlib";

use Carp;
use DB_File;
use Socket;
use IO::Socket::INET;
use IO::File;
use Getopt::Long;
use CGI::FastTemplate;
use POSIX qw(strftime setsid);

$VERSION = sprintf("%d.%02d", q$Revision: 1.5 $ =~ /(\d+)\.(\d+)/);

use RRDWrapper;
use SyslogTIE;

require "config.pl";

umask 002;

sub dbCompare { $_[1] <=> $_[0] };

$DB_BTREE->{compare} = \&dbCompare;

my $POPT  = {};
parseOptions( $POPT);

if ( $POPT->{file}) {
  my $logFH = new IO::File "<$POPT->{file}" or croak "`$POPT->{file}': $!";
  main( $logFH, "localhost");
  exit 0;
}

$SIG{CHLD} = \&REAPER;

my $pid = 0;
my $retries = 4;

dtchBLOCK: {
  $pid = fork;

  $pid == 0 && do {
    close STDOUT; close STDIN; close STDERR;

    tie *STDERR, "SyslogTIE", ident => basename($0);

    POSIX::setsid or croak "(lstn) process session: $!";

    @SIG{qw(INT HUP TERM)}     = ((\&HUNTER) x 3);
    @SIG{qw(__DIE__ __WARN__)} = ((\&TOSTDERR) x 2);

    my $pidFN = "/var/tmp/archiver.pid";

    open  PIDF, ">$pidFN" or croak "(dtch) `$pidFN': $!";
    print PIDF  "$$\n";
    close PIDF;

    listener();
  };

  # in parent process
  $pid > 0 && exit( 0);

  # EGAIN sleep one second and retry fork
  $! =~ /o more process/ && $retries > 0 && do {
    $retries--;
    sleep 1.0;
    redo dtchBLOCK;
  };

  # either no retries left, or some other error
  croak "dettach fork: $!";
}

sub listener {

  my $lstnSH = new IO::Socket::INET(
    'LocalPort' => $POPT->{'port'},
    'Proto'     => 'tcp',
    'Listen'    => 2,
    'Reuse'     => 1
  ) or croak("Socket: $@ (code: $!)");

  my $clientSH    = undef;
  my $pid         = 0;
  my $retries     = 4;
  my $sigset      = undef;

  $sigset = POSIX::SigSet->new(&POSIX::SIGINT);
  $sigset->addset(&POSIX::SIGTERM);
  $sigset->addset(&POSIX::SIGHUP);

  while ( $clientSH = $lstnSH->accept()) {

  acceptBLOCK: {
      POSIX::sigprocmask(&POSIX::SIG_BLOCK, $sigset)
	or croak "(lstn) sigset block: $!";

      $pid = fork;

      $pid == 0 && do {

	%CHILDREN = ();
	@SIG{qw(INT HUP TERM)} = (('DEFAULT') x 3);
	POSIX::sigprocmask(&POSIX::SIG_UNBLOCK, $sigset)
	  or croak "(srv) sigset unblock: $!";

	$lstnSH->close();
	$clientSH->autoflush(1);

        my ($clientPORT,$clientIADDR) =
	  (unpack_sockaddr_in( getpeername $clientSH));
	my $clientHN = gethostbyaddr( $clientIADDR, AF_INET);
	my $clientIP = inet_ntoa($clientIADDR);

	print STDERR "connection from $clientHN (IP: $clientIP)\n";

        main( $clientSH, $clientHN);
	exit(0);
      };

      # in parent process
      $pid > 0 && do {
	POSIX::sigprocmask(&POSIX::SIG_UNBLOCK, $sigset)
	  or croak "(lstn) sigset unblock: $!";

	$clientSH->close();
	$CHILDREN{$pid} = 1;
	last acceptBLOCK;
      };

      POSIX::sigprocmask(&POSIX::SIG_UNBLOCK, $sigset)
	or croak "(lstn) sigset unblock: $!";

      # EGAIN sleep one second and retry fork
      $! =~ /o more process/ && $retries > 0 && do {
	$retries--;
	sleep 1.0;
	redo acceptBLOCK;
      };

      # either no retries left, or some other error
      croak "accept fork: $!";
    }
    # AcceptBLOCK:
  }
  # while ( $clientSH = $lstnSH->accept())
}

sub main {
  my $clientSH  = $_[0];
  my $clientHN  = $_[1];

  my $logFN = $POPT->{db} . $clientHN . ".logdb";
  my (%log, $logDB);
  my $pageDN = $POPT->{page} . $clientHN . "/";

  $logDB = tie %log, "DB_File", $logFN, O_RDWR|O_CREAT, 0664, $DB_BTREE or
    croak "`$logFN': $!";

  my $tpl = new CGI::FastTemplate( dirname($0)."/template");
  my $buildURLlist = builURLlistGenerator($tpl, $logFN, $POPT->{'cgipfx'});

  $tpl->define(
     graph   => 'graph.tpl',
     arows   => 'arow.tpl',
     imgrows => 'imgrow.tpl'
  );

  my $rrdw = undef;
  my $assign = {};

  while (<$clientSH>) {
    chomp($_);

    $rrdw ||= new RRDWrapper
      \$_,
      rrdFN         => $POPT->{db} . $clientHN . ".rrd",
      graphFNPrefix => $pageDN . 'graphs',
      IMGTagPrefix  => 'graphs/'
    ;

    $rrdw->update( \$_);

    $log{ $rrdw->getLogTM() } = $_;
    $logDB->sync();

    &$buildURLlist( $rrdw->getLogTM());

    $assign->{IMGTAG} = $rrdw->graphCpuIMG();
    $tpl->assign( $assign);
    $tpl->parse( 'IMGROWS' => ".imgrows");
    $tpl->clear_href(1);

    $assign->{IMGTAG} = $rrdw->graphCtxSwitchesIMG();
    $tpl->assign( $assign);
    $tpl->parse( 'IMGROWS' => ".imgrows");
    $tpl->clear_href(1);

    $assign->{IMGTAG} = $rrdw->graphQueriesIMG();
    $tpl->assign( $assign);
    $tpl->parse( 'IMGROWS' => ".imgrows");
    $tpl->clear_href(1);

    $assign->{IMGTAG} = $rrdw->graphDbWritesIMG();
    $tpl->assign( $assign);
    $tpl->parse( 'IMGROWS' => ".imgrows");
    $tpl->clear_href(1);

    $assign->{IMGTAG} = $rrdw->graphSlowQueriesIMG();
    $tpl->assign( $assign);
    $tpl->parse( 'IMGROWS' => ".imgrows");
    $tpl->clear_href(1);

    $tpl->parse( 'IMGROWS' => "graph");

    renderTemplate($tpl, $pageDN."index.html" );

    $tpl->clear();
  }

  untie %log;
  $clientSH->close();
}

sub GONER {
  $DONE = 1;
  $SIG{ $_[0]} = \&GONER;
}

sub TOSTDERR {
  print STDERR @_;
}

sub REAPER {
  my $child;

  while (($child = waitpid(-1, &POSIX::WNOHANG)) != -1) {
    delete $CHILDREN{$child};
  }
  $SIG{CHLD} = \&REAPER;
}

sub HUNTER {
  local($SIG{CHLD} = 'IGNORE');

  kill 'INT' => keys %CHILDREN;
  exit (0)
}

sub parseOptions ($) {
  my $optsctl = shift(@_);
  my $usage = basename($0) . <<'EOU';
: [options]
  --once     run through the sampling loop once
  --file     file spec where log entries are read from [debug]
  --daemon   run as a daemon
  --port     port to listen to for perfcollect connections
  --help     this usage message
EOU
  %$optsctl = (
       "page"     => ".",
       "db"       => ".",
       "once"     => 0,
       "file"     => "",
       "daemon"   => 0,
       "help"     => 0,
       "cgipfx"   => "",
       "port"     => 61376
  );

  GetOptions( $optsctl, qw(once file=s daemon help port=i)) or croak $usage;

  @$optsctl{ qw( cgipfx db page)} =
    ($CFG::CgiPrefix, $CFG::CgiDN, $CFG::ArchiverDN);

  for ( keys %$optsctl) {
  optCASE: {
      m/help/ && do {
	print $usage if ($optsctl->{$_});
	last optCASE;
      };
      m/port/ && do {
	$optsctl->{$_} > 0 or croak "$_ $optsctl->{$_} must be positive";
	last optCASE;
      };
      m/page|db/ && do {
	my $fn = $optsctl->{$_};
        croak "$fn: cannot write to" unless( -w $fn && -r _ && -x _);
        $optsctl->{$_}  .= "/" if( substr($optsctl->{$_},-1,1) ne "/");
      };
      m/file/ && do {
	my $fn = $optsctl->{$_};
        croak "$fn: cannot create" unless( -w dirname($fn) && -r _ && -x _);
      };
      m/cgipfx/ && do {
	my $fn = $optsctl->{$_};
        croak "undefined CGI prefix" unless( $fn);
        $optsctl->{$_}  .= "/" if( substr($optsctl->{$_},-1,1) ne "/");
      };
    }
  }
  exit(0) if( $optsctl->{'help'});

  $optsctl->{'db'} .= "archiver/";
  mkdir $optsctl->{'db'}, 0775 unless (-e $optsctl->{'db'} );

  croak $optsctl->{'db'},": cannot access"
    unless (-w $optsctl->{'db'} && -x _ && -r _);

  return $optsctl;
}

sub builURLlistGenerator {
  my $tpl    = $_[0];
  my $dbf    = "archiver/" . basename($_[1]);
  my $cgipfx = $_[2];

  my $cnt = 0;
  my $samplesMAX = 60;
  my @samples = ();

  my $tms    = "";
  my $sample = undef;

  return sub {
    my $tm = $_[0];

    $tms = strftime( "%T", localtime( $tm));

    $sample = ( @samples < $samplesMAX ? {} : pop( @samples));
    @$sample{qw( CGIPREFIX DBTM DBTMS DBFILE) } = ($cgipfx, $tm, $tms, $dbf);
    unshift( @samples, $sample);

    foreach $sample ( @samples) {
      $tpl->assign( $sample);
      $tpl->parse(  AROWS => ".arows");
      $tpl->clear_href(1);
    }
  }
}

sub renderTemplate {
  my ($tpl, $outFN) = @_;
  my $oldFH;

  open TMPL,   ">$outFN" or croak "`$outFN': $!";
  open STDOUT ,">&TMPL"  or croak "dup STDOUT: $!";
  $oldFH = select STDOUT; $| = 1; select($oldFH);

  $tpl->print();

  close STDOUT or croak "STDOUT close: $!";
  close TMPL   or croak "TMPL close: $!";
}

