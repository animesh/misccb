#!/usr/local/bin/perl

# whisper_1_1.pl
# Scott Parks	code@levitator.org
# 01/26/00
# Modified: 09/28/00
# streaming mp3 server. Works with Winamp client. 

use Socket;
use Getopt::Std;

my $VERSION = 1.1;

getopt('plqsrd',\%args);
&help() if(!defined $args{l} && !defined $args{d});

$port = 2020 ;
$port=$args{p} if(defined $args{p});
&help() if($port < 1 || $port > 65535);

$quiet=0; $quiet=1 if(exists $args{q});
$loop=1; $loop=0 if(exists $args{s});

($name, $aliases, $protocol) = getprotobyname('tcp');
($name, $aliases, $port) = getservbyport($port, 'tcp') if ($port !~ /^\d+$/);

print "Listening on port $port...\n"  if(!$quiet);

socket(S,AF_INET,SOCK_STREAM,$protocol) || die "socket : $!";
($err |= setsockopt(S, SOL_SOCKET, SO_REUSEADDR, 1)) or die "Can't set sock option\n"; 

$sockaddr = 'S n a4 x8';
$this = pack($sockaddr, AF_INET, $port, "\0\0\0\0");
bind(S, $this) || die "bind : $!";

listen(S,10) || die "listen: $!";
select(S); $| = 1; select(STDOUT);

for ($con = 1; ; $con++) {
   printf("Waiting for connection %d....\n", $con)  if(!$quiet);
   ($addr = accept(NS,S)) || die $!;

   select(NS); $| = 1; select(STDOUT);

   if (($child = fork()) == 0) {
      @songs=();
      &make_list();
      &serv_client();   
   }
   close(NS);
} 

#### subs start here  ##########


sub clean_str() {
   local $str=@_[0];

   $str=~tr/\r|\n//d;
   $str=~tr/\t/ /;
   $str=~tr/ //s;
   return $str;
}

sub start_stream() {
   local $agent=@_[0], $ret="";

   local $buff="HTTP/1.1 200 OK\n\n";
   print NS $buff;
}

sub serv_client() {
   ($af,$port, $inetaddr) = unpack($sockaddr, $addr);
   @inetaddr = unpack('C4', $inetaddr);
   print "Connect: $con @inetaddr:$port\n" if(!$quiet);

   local $byts=0,$agent="",$buf="",$lin="";
   while (<NS>) {
      $buf=$_;
      $lin = &clean_str($buf);
      $agent="winamp" if($lin=~/winamp/gi);
      next if(length($lin));

      &start_stream($agent);
      if($loop) {
         local $cnt=1;
         do { 
            print "Pass: $cnt\n" if(!$quiet); 
            $cnt++;
         } while(&stream_data());
      } else {
         print "Single pass through $args{l}\n" if(!$quiet); 
         &stream_data();
      }
      print "Playlist done.\n" if(!$quiet);
      close(NS);
   }
   print "Client be gone .\n" if(!$quiet);
   close(NS);
   exit;
}

sub make_list() {
   local $fil, $idx=0;

   if(defined($args{l})) {
      local $playlist=$args{l};
      local @fils=qx{cat $playlist};
      foreach $fil(@fils) {
         $fil=~tr/\n|\r//d;
         @songs[$idx++]="$fil"; 
      } 
   } elsif (defined($args{d})) {
      local $dir=$args{d};
      local @fils=qx{ls $dir | grep .mp3};
      foreach $fil(@fils) {
         $fil=~tr/\n|\r//d;
         @songs[$idx++]="$dir\/$fil" if(length($fil) && length($dir)); 
      } 
   }
   &shuffle(\@songs) if(exists($args{r}));
}

sub stream_data() {
   local $sze=4096, $byts=0, $buf="", $song, $idx;

   foreach $song(@songs) {
      if(!open(FDS,"<$song")) {
         print "NOT FOUND, is path correct\?: $song\n";
         sleep(3);
         next;
      }
      print "Begin Streaming of -$song-\n" if(!$quiet); 
      $byts=$sze;
      while(($byts=read(FDS, $buf, $sze))) {
         print NS $buf if(length($buf) > 2) or return 0;
      }
      print "End Streaming of -$song-\n" if(!$quiet); 
      sleep 3;
   }
   return 1;
}

sub shuffle() {
   local $i, $j, $array = shift;

   srand(time());
   for($i = @$array; --$i; ) {
      $j = int rand($i+1);
      next if $i == $j;
      @$array[$i,$j] = @$array[$j,$i];
   }
}

sub help() {
   print "Usage: $0 [-l | -d] -p -q -s\n";
   print "\tNote: -l or -d must be specified\n";
   print "\t-l playlist, file containing full path to each .mp3 file. \n";
   print "\t-d full path to directory containing mp3 files\n";
   print "\t-p port number. Default is 2020\n";
   print "\t-q Quiet mode. No informative messages.\n";
   print "\t-s Single pass through playlist. Default is constant loop\n";
   print "\t-r Shuffle the playlist, don't play in listed order,\n\t   make sure it's the last switch on the command line.\n";
   print "\nExample:\n";
   print "\t$0 -l ./playlist\n";
   print "\tUsing default port (2020). Use your WinAmp player and 'Play Location'\n";
   print "\thost.domain.com:2020 and it will magically start streaming.\n\n";

   exit;
}

=head1 NAME
whisper_1_1.pl

=head1 DESCRIPTION
MP3 Server that works with the WinAmp client

=head1 README

whisper_1_1.pl - MP3 streaming server used with WinAmp client.
It'll run on any UNIX system with PERL. No special CPAN mods
required. WinAmp can be obtained at www.winamp.com

code@levitator.org
Scott Parks 01/26/00


whisper_1_1.pl - How to run it.

1. Use the WinAMP Client. That's what it's designed
   around.

2. Create a playlist
   This is a file with a complete path to a mp3 file.
   A playlist might look like this:

   /export/mp3/song1.mp3
   /export/mp3/song2.mp3
   /export/mp3/song3.mp3

3. Ensure the first line of whisper_1_1.pl points to your
   perl binary:

   #!/usr/local/bin/perl 

4. Run the script:
   
   ./whisper_1_1.pl -l playlist

5. Start your winamp client and 'Play Location' 
   http://your.server.dom:2020 

That'll get you running.

Options Summary
Run the program without any parameters to get the help 
message. Here's a summary of each option:
  
   -l the playlist file

   -d directory to look in for mp3's, non-recursive

   -p port to listen on. The default is 2020. Remeber, 
      you must use a port number above 1023 if you
      don't have root.

   -s Single pass through the playlist file. With this
      switch each song in the playlist will be played 
      once and the client will be dropped. The default
      is to continuosly loop through the playlist until
      the client bails or the server stops.

   -q Quiet mode. Turn off information messages.

   -r shuffle your playlist, be it a directory or file


That would be just about it.


=head1 PREQUISITES
PERL 5.004 or greater
Any Unix OS or more precisely, anything that has a fork system call
and runs PERL

=pod SCRIPT CATEGORIES
Audio/MP3

=cut



