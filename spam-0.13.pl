#!/usr/local/bin/perl
#
# Daniel Stenberg <Daniel.Stenberg@haxx.nu>
# December 3, 1999
# Version: 0.12
#

# Spam is an internet desease that's really hard to take down. I do my best to
# complain on all spam mails I receive. 1997 I wrote a script that does the
# complaining for me. This is that script, rewritten in perl with improved
# functionality and greatly enhanced speed. My original version was a bourne
# shell script.

#
# Use this script from within i.e elm or pine. Select 'show all headers' and
# then pipe the offending mail into this script.
#
# HISTORY:
# 0.6 (Sep 28, 1998)
#   - Made it deal with Received: lines with less spaces in the line
#   - Added -a support to complain through the abuse.net service.
#
# 0.7 (Feb 4, 1999)
#   - Made it work better on the first host within the first parenthesis
#   "Received: from mail3.selec.net (seawestb-205.az.com [208.222.191.205])"
#   - Made it stop at first space too when finding the end of that host.
#     It seems qmail add lines that look like:
#   "Received: from unknown (HELO selec.net) (192.168.101.16)"
#   - We should now be aware of "HELO" occurrences and get the next string
#     intead!
#   - Ignores IP-numbers in places where host names are expected better.
#
# 0.8 (March 15, 1999)
#   - -a now sends a single mail to each recipient.
#
# 0.9 (June 7, 1999)
#   - Presents the list of hosts better, even when using -a.
#
# 0.10 (August 18, 1999)
#   - AddHost() now skips hosts with @ in them.
#
# 0.11 (October 25, 1999)
#   - Bjorn@haxx.nu found out that this script didn't properly find headers
#     that contained underscores.
#
# 0.12 (December 3, 1999)
#   - James E. Weiss <wje@wje.net> convinced me that I really should include
#     the spammer's subject in the spam report subject. Now I check for MIME
#     gibberish and if there is such I don't do it, but otherwise I include
#     the offending subject in the complaint subject. I know some sites do
#     request this from reports as well, so this is a good thing to add.
#
# 0.13 (March 21, 2000)
#   - A tiny correction made it deal with received: lines better (the host
#     after 'by')

# You may need to change this if you're using anything else but Solaris:
$mailcmd="/usr/lib/sendmail";

# Linux example:
#$mailcmd="/usr/sbin/sendmail";


sub mailaway {
    my $mailto = $_[0];

    open(SENDIT, "|$mailcmd $mailto") ||
        die "Couldn't run mail program";

    if(($insubject =~ /\=\?iso/i) || ($insubject eq "")) {
        # MIME code in subject, just leave it out of the complaint subject
        print SENDIT "Subject: Spam Report!\n";
    }
    else {
        print SENDIT "Subject: Spam Report: $insubject\n";        
    }

    if($fromline ne "") {
        print SENDIT "From: $fromline\n";
    }
    if($mailto ne $receiver) {
        print SENDIT "To: $receiver\n";
    }
#    print SENDIT "X-mailer: spam.pl - a spam complaint script by Daniel Stenberg\n";

    print SENDIT "\n"; # header/body separator

    print SENDIT @body;

    print SENDIT "\n--- start of spam ---\n";
  
    for(@mail) {
        print SENDIT $_;
    }

    print SENDIT "\n--- end of spam ---\n";

    $sig="$home/.signature";
    open(SIG, "<$sig");
    while(<SIG>) {
        print SENDIT $_;
    }
    close(SIG);
    
    close(SENDIT);
}

argv:

if($ARGV[0] eq "-h") {
    print <<EOF;
Usage: $0 [-h][-d][single mail receiver]

 This program will try to extract information about which hosts that relayed
the abusive mail, and will sent off a complaint to the administrators of those
hosts. The mail should be sent to this program on stdin.

 -h displays this help

 -d runs in debug-mode (shows a lot of internal processing choices).
    debug-mode does not send any mail.

 -a makes the script use the report facility of abuse.net. This requires
    your From: address to be registered at that service prior to use.
    http://www.abuse.net/register.html

 If you supply an argument to this program, that will be used as the single
receiver of the complaint mail, which can be used to test the correctness of
this script before using for real.

 NOTE that you are not included among the receivers of the complaint.

 \$HOME/.spamcomplaint will be used as mailbody in the complaint mail if
 existant

 \$HOME/.spamfriends should be a list with friendly domains that shouldn\'t
 receive complaints. Most probably your own site and other related ones.

 \$HOME/.spamfrom should contain be the From: email address of the complainer.

 \$HOME/.signature will be included at the bottom of the generated mail.

     Short lesson in sendmail Received: headers
     ==========================================

Received: from host1 (host2 [ww.xx.yy.zz]) by host3 (8.7.5/8.7.3) with SMTP id
 MAA04298; Thu, 18 Jul 1996 12:18:06 -0600

host3 - The host that added the Received line
ww.xx.yy.zz - The IP address of the incoming SMTP connection
host2 - The reverse-DNS lookup of that IP address
host1 - The name the sender used in the SMTP HELO command when they connected

Author:  Daniel Stenberg <Daniel.Stenberg\@haxx.nu>
EOF
    exit;
}
elsif($ARGV[0] eq "-d") {
    $debug=1;
}
elsif($ARGV[0] eq "-dd") {
    $debug=1;
    $debug2=1; # extra detailed debug info
}
elsif($ARGV[0] eq "-a") {
    $abusenet = 1;
}

# This check must be after all switch checks:
elsif($ARGV[0] ne "") {
    $singlereceiver=$ARGV[0];
}

if($ARGV[0] ne "") {
    shift @ARGV;
    goto argv;
}

$inheader=0;

# we need $HOME at several places
$home=$ENV{'HOME'};

@mail=<STDIN>;


# We support a file with domains that are KNOWN to never spam.
# This list should also include your own domain and friendly domains
# through which your mail may have been forwarded through with your
# approval and permission
#
# If my home domain is frontec.se and I forward my mail to ericsson.se
# its pretty important that both of those domains are added to prevent
# this script to send complaints to them too!
#
$friends="$home/.spamfriends";

if(open(FRIENDS, "<$friends")) {
    @friends=<FRIENDS>;
    close(FRIENDS);
}
else {
    print "WARNING: You have no .spamfriends defined!\n";
    print "WARNING: This may generate complaints to friendly sites!\n";

    open(FRIENDS, ">$friends");
    print FRIENDS "";
    close(FRIENDS);
    print "WARNING: Exiting. Reinvoke if this is really what you want!\n";
    exit;
}

if($friends[0] eq "") {
    print "WARNING: You have no friendly domains entered in .spamfriends!\n";
    print "WARNING: This may generate complaints to friendly sites!\n";
}

if($debug) {
    for(@friends) {
        print "FRIEND: $_";
    }
}


sub AddHost {
    my $host = lc($_[0]);

    for(@friends) {
        $friendly=$_;
        chop $friendly;

        if($friendly =~ /\./) {
            # A domain requires a dot. It allows for empty lines in the
            # friends file
            if($host =~ /$friendly$/) {
                # known friendly domains are not added
                return;
            }
        }
    }

    # Ignore hosts that has a digit in the toplevel domain!
    if($host =~ /\.[^.]*[0-9][^.]*$/) {
        return;
    }

    if($host =~ /@/) {
        # @ in the host name looks like a fake
        return;
        # if this "host" has an @ letter, we cut it and everything to the
        # left of it
        # $host =~ s/([^@]*)@(.*)/$2/g;
    }
    
    # since it is quite impossible to get the full domain name
    # from a local name, we just skip names that have no dots
    if($host =~ /\./) {             
        if(!$offender{$host}) {
            $offender{$host}=1;
            push @uniqlist, $host;
            print "HOST: $host\n";
        }
    }
}

headers:
for(@mail) {
    if(!$inheader &&
       ($_ =~ /^From /)) {
        $inheader=1;
    }
    elsif($_ =~ /^[A-Za-z0-9-_]*:/) {
        $line = $_;

        chop $line;

        push @concat, $line;

        $inheader=1;
    }
    elsif($inheader &&
          ($_ =~ /^[ \t]/)) {
        $line = $_;
        chop $line;

        # replacing a sequence of beginning whitespaces with a single space
        $line =~ s/^[ \t]*/ /g;

        # append to previous line:
        $concat[$#concat]=$concat[$#concat].$line;
    }
    else {
        if($inheader) {
            $inheader=0;
            last headers;
        }
    }
#    print $_;
}

$rheaders=0;

for(@concat) {
    if($_ =~ /^Received: /) {
        # This is a received line, deal with it
        #
        # EXAMPLE:
        # Received: from gade.imada.ou.dk (root@gade.imada.ou.dk
        # [130.225.128.158]) by gatekeeper.frontec.se (8.8.2/8.8.2) with ESMTP
        # id OAA07503 for    <Daniel.Stenberg@sth.frontec.se>;
        # Sun, 16 Aug 1998 14:28:03 +0200 (MET DST)

        $rheaders++;

        $rest = substr($_, 10);

        if($debug2) {
            print "RECV: $rest\n";
        }
        if($rest =~ /^from ([^ ]*)/ ) {
            # Received: from gade.imada.ou.dk
            # add this host to the list of offenders

            $host = $1;

            # We have seen a from-host that looks like:
            # "pobox.com(c5800-1-188.225ohio.megsinet.net[216.214.20.188])"
            # we need to adjust to this weird thing. I.e no space between
            # the host and the next info part

            if( $host =~ /^([^(]*)/ ) {
                # get the "pobox.com" piece, get everything up to the first
                # parenthesis.
                $host = $1;
                $nextindex = 5;
            }
            else {
                $nextindex = 6;
            }

            AddHost($host);

            $rest=substr($rest, length($host)+ $nextindex);

            if($debug2) {
              print "MORE: $rest\n";
            }

            if($rest =~ /^ *\(([^\) ]*)/) {
                # (root@gade.imada.ou.dk [130.225.128.158])

                # we may get a "local" host name here, without full domain
                # we may also have a [ipnumber] to the right of the name
                # we may get an email address here like name@machine.domain
                # we may get an IP number only, like (192.192.192.192)
                # we may get weird stuff like this:
                #   "(root@[207.159.154.2])"
                # we may get the host and the IP-part merged like:
                #   "(c5800-1-188.225ohio.megsinet.net[216.214.20.188])"

                if($1 =~ /^HELO/) {
                    # qmail seems to add strings like this, and then the
                    # next string is the host instead!

                    $rest =~ /^ *\(HELO ([^\) ]*)/;

                    if($debug2) {
                        print "qmail HELO host!\n";
                    }
                    $host = $1;
                    $len = length($1);
                    $rest = substr($rest, $len+8);
                }
                else {
                    $host = $1;
                    $len = length($1);
                    $rest = substr($rest, $len+3);

                }

                if($debug2) {
                    print "ENTRY: $host\n";
                }
                # check for IP-only:
                if($host =~ /^\d+\.\d+\.\d+\.\d+$/) {
                    # HOST is nothing but IP!
                    if($debug2) {
                        print "$host is only IP address\n";
                    }
                    $ip = $host;
                    $host="";
                }

                # extract the possible IP number first:
                elsif($host=~ s/ *\[([^\]]*)\]//g) {
                    $ip = $1;
                    if($debug2) {
                        print "HOST: $host IP: $ip\n";
                    }
                }
                else {
                    $ip = "";
                }
                # haven't really decided what to do with the IP number yet

                # If we have DNS access, we could look it up and if it resolves
                # we could add that as a host too.

                # remove the possible name@ part of the host:
                $host=~ s/([^@]*)@//g;
                
                if($host ne "") {
                    # we've seen occurrences when $host now contains
                    # "[207.159.154.2]" !!

                    # we check if anything behind the last dot is numeric,
                    # then we don't add the host since that means IP number
                    # and most likely the effect of a creation that looked
                    # like: "root@[207.159.154.2]" style.
                    if($host =~ /\..*[0-9]+$/) {
                        if($debug2) {
                            print "BEEP: $host not a host name\n";
                        }
                    }
                    else {
                        AddHost($host);
                    }
                }

                if($debug2) {
                    print "REST: $rest\n";
                }

                # by gatekeeper.frontec.se (8.8.2/8.8.2)
                #  by pm1.contactor.se
                # [209.125.206.198]) by pm1.contactor.se (8.9.3/8.9.1)
                if($rest =~ /^.*by ([^ ]*)/ ) {
                    
                    AddHost($1);
                }

            }

        }
        elsif($rest =~ /^\(from ([^\)]*)/ ) {
            # Received: (from dast@localhost)
#           print "---- $1\n";
        }

    }
    elsif($_ =~ /^Subject: (.*)/) {
        $insubject=$1;
    }
    else {
        #print $_."\n";
    }
}

if($rheaders == 0) {
    print "No Received: headers were found!\n";
    exit;
}

if($uniqlist[0] eq "") {
    print "No unfriendly hosts were found. This cannot have been a spam!\n";
    exit;
}

# This creates a sequence of hosts from one.
#   penguin.wise.edt.ericsson.se
# will generate:
#
# penguin.wise.edt.ericsson.se
# wise.edt.ericsson.se
# edt.ericsson.se
# ericsson.se

sub generateaddress {
    $host = $_[0];

    @parts= split('\.', $host);

    while($#parts >= 1) {
        $new = join('.', @parts);
#       print "$new\n";
        AddHost($new);
        shift @parts;
    }
}

sub complainers {
    $host = $_[0];

    for('abuse', 'postmaster') {
        push @emails, $_."@".$host;
    }
}

sub abusecomplainers {
    $host = $_[0];

    # try to avoid building i.e first host.com@abuse.net
    # and then machine.host.com@abuse.net.
    # this hack attempts to skip the second action

    @p = split('\.', $host);

    $prts = $#p;

#    print "$host is $prts parts\n";

    $two=$p[$prts-1]."\.".$p[$prts];

    for(@emails) {
#       print "TRY $_ against $two\n";

        if($_ =~ /$two/) {
            # 2nd already present
            return;
        }
    }

    push @emails, $host."\@abuse.net";
}
# create all new combinations of hosts:
if(! $abusenet) {
    # if this is an abuse.net report, we don't do this
    for(@uniqlist) {
 #      if($debug) {
 #          print "HOST: $_\n";
 #      }
        generateaddress($_);
    }
}

# we sort the list 
@slist = sort { reverse($a) cmp reverse($b) } @uniqlist;

if(!$abusenet) {
    # make email addresses out of the hosts:
    for(@slist) {
        complainers($_);
    }
}
else {
    # the absue.net style of generating reports:
    for(@slist) {
        abusecomplainers($_);
    }
}

# @emails is how a complete list of email addresses to complain to
#
# Now, get the predefined complaint mail or use our built-in.

$body="$home/.spamcomplaint";

if(open(BODY, "<$body")) {
    @body=<BODY>;
    close(BODY);
}
else {
    @body=('I recently received the following message, which appears to be from one of
your users or was relayed by one of your machines.  It looks to me like a
spam, unsolicited commercial e-mail.  Such mail is very annoying and widely
considered to be abusive.

If your domain is the source for this E-mail, could you encourage him/her/it
to cut it out?  Thanks.

If you are one of the relays down the chain, maybe you should consider to
restrict the use of your mail-server? Thanks.

If your host was faked or in any way spoofed, I trust you would still like
to know about it.

If you know who to tell or inform about this in order to make it stop. Please
forward this information to whom it may concern.

(This mail was automatically generated.)
');
}


#
# Produce the complaint email
# 

for(@emails) {
  $receiver="$receiver$_ ";
}

# If a single argument was specified, we mail the stuff to that address only
if($singlereceiver ne "") {
  $mailto=$singlereceiver;
}
else {
  $mailto=$receiver;
}

if($debug) {
  print "Spam-subject: $insubject\n";
  print "When run for real, this ";
}
print "sends mail to $mailto\n";

  if(open(EMAIL, "<$home/.spamfrom")) {
    # we found a From: line file.
    $fromline=<EMAIL>;
    close(EMAIL);
  }
  else {
    print "WARNING: You have no .spamfrom defined!\n";
    print "WARNING: Your complaint mails don't get proper From: lines!\n";
    $fromline=""; # no from line
  }

if($debug eq "") {
    if($abusenet) {
      for(@emails) {
        # a single mail for each offender!
        $receiver = $_;
        if($singlereceiver ne "") {
          # for debugging, you can get it to mail a single specified addy
          $mailto=$singlereceiver;
        }
        else {
          $mailto=$receiver;
        }
        &mailaway($mailto);
      }
    }
    else {
      &mailaway($mailto);
    }
}

