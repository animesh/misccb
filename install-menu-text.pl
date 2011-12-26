#!/usr/bin/env perl
# $Id: install-menu-text.pl 12062 2009-02-03 12:56:24Z preining $
# install-menu-txt.pl
#
# Copyright 2007, 2008 Reinhard Kotucha, Norbert Preining
# This file is licensed under the GNU General Public License version 2
# or any later version.
#
# This file implements the text based menu system for the TeX Live installer.

use vars qw(@::end_install_hook $::opt_no_cls);

our %vars;
our $tlpdb;
our @collections_std;
our @collections_lang;
our @collections_lang_doc;
our $texlive_release;

my $MENU_CONTINUE = -1;
our $MENU_INSTALL = 0;
our $MENU_ABORT   = 1;
our $MENU_QUIT    = 2;


my $RETURN = $MENU_CONTINUE;


# issue welcome message on end of installation
push @::end_install_hook,
    sub { if (win32()) { print TeXLive::TLUtils::welcome(); }
          else { print TeXLive::TLUtils::welcome_paths(); } };

sub clear_screen {
  return 0 if ($::opt_no_cls);
  system (unix() ? 'clear' : 'cls');
}

sub string_to_list {
  my $string=shift;
  return split(//, $string);
}

sub button {
  my $val=shift;
  return ($val)? '[X]':'[ ]';
}

sub hbar {
  return '=' x79, "\n";
}

sub toggle {
  my $var=shift;
  my $val=$vars{$var};
  ++$val;
  $vars{$var}=$val%2;;
}

sub menu_head {
  my $text=shift;
  clear_screen;
  print "$text\n", hbar, "\n";
}

sub other_options {
  my @options=@_;
  my %opts=(
    '-' => 'deselect all',
    '+' => 'select all',
    'H' => 'help',
    'R' => 'return to main menu',
    'Q' => 'quit'
      );

  if ($vars{'from_dvd'}) {
    $opts{'I'}='start installation for running from DVD';
  } else {
    $opts{'I'}='start installation to hard disk';
  }

  if ($options[$#options] eq 'diskspace') {
    pop @options;
    calc_depends ();
    print "\nOther actions:                           ".
        "Disk space required: $vars{'total_size'} MB\n";
  } else {
    print "\nOther actions:\n";
  }
  
  for my $option (@options) {
    if (defined $opts{"$option"}) {
      printf " <%s> %s\n", $option, $opts{$option};
    } else {
      die "other_options: $opts{$option} undefined.\n";
    }
  }
}

sub prompt {
  my $prompt=shift;
  print "\n$prompt: ";
  my $answer = <STDIN>;
  $answer = "q" if !defined($answer);
  chomp($answer);
  return "$answer";
}

# The menu loop. A menu is a function.  Its return value is a
# reference to another menu or to itself.
sub run_menu_text {
  my $menu=\&main_menu;
  while ($RETURN == $MENU_CONTINUE) {
    $menu=$menu->();
  }
  return($RETURN);
}
$::run_menu = \&run_menu_text;

sub binary_menu {
  my %command=(
    'self' => \&binary_menu,
    'R' => \&main_menu,
    'Q' => \&quit
      );

  my @binaries;
  my @keys=string_to_list "abcdefghijklmopstuvwxyz";
  my $index=0;
  my %keyval;
  my $selected_platform;

  menu_head "Available sets of binaries:";

  foreach my $key (keys %vars) {
    if ($key =~ /binary_(.*)/) {
      push @binaries, $1;
    }
  }
  @binaries=sort(@binaries);

  foreach my $binary (@binaries) {
    printf "   %s %s %-16s %s\n", $keys[$index],
           button($vars{"binary_$binary"}),
           "$binary",
           platform_desc($binary);
    $keyval{"$keys[$index]"} = "binary_$binary";
    ++$index;
  }
  other_options qw(- + R Q diskspace);

  my $answer=prompt 'Press key to select/deselect binary systems';

  my @keystrokes=string_to_list $answer;

  foreach my $keystroke (@keystrokes) {
    if ($keystroke eq '-') {
      for my $binary (@binaries) {
        $vars{"binary_$binary"}=0 if defined $vars{"binary_$binary"};
      }
    }
    if ($keystroke eq '+') {
      for my $binary (@binaries) {
        $vars{"binary_$binary"}=1 if defined $vars{"binary_$binary"};
      }
    }
    if (defined $keyval{$keystroke}) {
      toggle "$keyval{$keystroke}";
    } elsif (!defined $command{"\u$answer"}) {
      print "Unknown command: $keystroke\n\n";
    }
  }
  if ($vars{"binary_win32"}) {
    $vars{"collection-wintools"} = 1;
  } else {
    $vars{"collection-wintools"} = 0;
  }
  if (defined $command{"\u$answer"}) {
    return $command{"\u$answer"}->();
  } else {
    return $command{'self'}->();
  }
}


sub scheme_menu {
  my %command=(
    'self' => \&scheme_menu,
    'R' => \&main_menu,
    'Q' => \&quit
      );

  my @schemes;
  my @keys=string_to_list "abcdefghijklmnopstuvwxyz";
  my %keyval;
  my $index=0;

  menu_head 'Select a scheme:';

  foreach my $pkg ($tlpdb->list_packages) {
    my $tlpobj = $tlpdb->{'tlps'}{$pkg};
    if ($tlpobj->category eq "Scheme") {
      push @schemes, $pkg;
      $vars{"$pkg"}=($vars{'selected_scheme'} eq $pkg)? 1:0;
    }
  }
  @schemes=sort @schemes;

  foreach my $scheme (@schemes) {
    $keyval{$keys[$index]}="$scheme";
    my $tlpobj = $tlpdb->get_package("$scheme");
    printf " %s %s %s\n", $keys[$index], button($vars{"$scheme"}),
    $tlpobj->shortdesc;
    ++$index;
  }

  foreach my $entry (keys %vars) {
    if ($entry=~/^(collection-.*)/) {
      $vars{"$1"}=0;
    }
  }

  my $scheme_tlpobj = $tlpdb->get_package($vars{'selected_scheme'});
  if (defined ($scheme_tlpobj)) {
    foreach my $dependent ($scheme_tlpobj->depends) {
      if ($dependent=~/^(collection-.*)/) {
        $vars{"$1"}=1;
      }
    }
  }
  if ($vars{"binary_win32"}) {
    $vars{"collection-wintools"} = 1;
  } else {
    $vars{"collection-wintools"} = 0;
  }

  other_options qw(R Q diskspace);
  my $answer=prompt 'Press key to select a scheme';

  if (defined $keyval{"$answer"}) {
    $vars{'selected_scheme'}=$keyval{"$answer"};
    return $command{'self'}->();
  }
  if (defined $command{"\u$answer"}) {
    return $command{"\u$answer"}->();
  } else {
    print "Unknown command: $answer\n\n";
    return $command{'self'}->();
  }
}


sub collection_menu {
  my %command=(
    'self' => \&collection_menu,
    'R' => \&main_menu,
    'Q' => \&quit
      );

  my @collections;
  my @keys=string_to_list "abcdefghijklmnopstuvwxyzABCDEFGHIJKLMNOPSTUVWXYZ";
  my %keyval;
  my $index=0;
  my @coll_short_desc;
  my @coll_long_desc;

  menu_head 'Select collections:';

  @collections=sort @collections_std;

  foreach my $collection (@collections) {
    next if ($collection eq 'collection-perl');
    my $tlpobj = $tlpdb->get_package("$collection");
    if (length $tlpobj->shortdesc>30) {
      push @coll_long_desc, $collection;
    } else {
      push @coll_short_desc, $collection;
    }
  }
  my $singlecolumn_index=@coll_short_desc-1;

##<cols=2>
  my $lines=@coll_short_desc/2;
  ++$lines if (@coll_short_desc%2);
  for (0..$lines-1) {
    $index=$_;
    my $collection=$coll_short_desc[$index];
    my $tlpobj = $tlpdb->get_package("$collection");
    $keyval{$keys[$index]}="$collection";
    printf " %s %s %-33s", $keys[$index], button($vars{"$collection"}),
    substr($tlpobj->shortdesc,0,33);
    if (defined $coll_short_desc[$index+$lines]) {
      my $collection=$coll_short_desc[$index+$lines];
      my $tlpobj=$tlpdb->get_package("$collection");
      $keyval{$keys[$index+$lines]}="$collection";
      printf " %s %s %-32s\n", $keys[$index+$lines],
      button($vars{"$collection"}), substr($tlpobj->shortdesc,0,32);
    } else {
      print "\n";
    }
  }
##</cols=2>
  $index=$singlecolumn_index;
#  print "\n$index\n\n";
  foreach my $collection (@coll_long_desc) {
    my $tlpobj=$tlpdb->get_package("$collection");
    $keyval{$keys[$index+1]}="$collection";
    printf " %s %s %s\n", $keys[$index+1], button($vars{"$collection"}),
    $tlpobj->shortdesc;
    ++$index;
  }
##</cols=1>

  other_options qw(- + R Q diskspace);
  my $answer=prompt 'Press key to select collections';

  my @keystrokes=string_to_list $answer;

  foreach my $keystroke (@keystrokes) {
    if ($keystroke eq '-') {
      for my $collection (@collections) {
        $vars{"$collection"}=0 if defined $vars{"$collection"};
      }
    }
    if ($keystroke eq '+') {
      for my $collection (@collections) {
        $vars{"$collection"}=1 if defined $vars{"$collection"};
      }
    }
    if (defined $keyval{$keystroke}) {
      toggle "$keyval{$keystroke}";
    }
  }

  if (defined $command{"\u$answer"}) {
    return $command{"\u$answer"}->();
  } else {
    print "Unknown command: $answer\n\n";
    return $command{'self'}->();
  }
}

sub language_menu {
  my %command=(
    'self' => \&language_menu,
    'R' => \&main_menu,
    'Q' => \&quit
      );

  my @languages;
  my @lang_docs;
  my @lang_keys=string_to_list "abcdefghijklmnopstuvwxyz0123456789";
  my @lang_doc_keys=string_to_list "ABCDEFGHIJKLMNOPSTUVWXYZ";
  my %keyval;
  my $lang_index=0;
  my $lang_doc_index=0;
  my $lines;

  menu_head 'Select language support:';

  @languages=sort @collections_lang;
  @lang_docs=sort @collections_lang_doc;

  $lines=@languages/2;
  ++$lines if (@languages%2);
  for my $index (0..$lines-1) {
    my $language=$languages[$index];
    my $tlpobj = $tlpdb->get_package("$language");
    $keyval{$lang_keys[$index]}="$language";
    printf " %s %s %-33s", $lang_keys[$index], button($vars{"$language"}),
    substr($tlpobj->shortdesc,0,33);
    if (defined $languages[$index+$lines]) {
      my $language=$languages[$index+$lines];
      my $tlpobj = $tlpdb->get_package("$language");
      $keyval{$lang_keys[$index+$lines]}="$language";
      printf " %s %s %-32s\n", $lang_keys[$index+$lines],
      button($vars{"$language"}), substr($tlpobj->shortdesc,0,32);
    } else {
      print "\n";
    }
  }

  print "\nSelect language-specific documentation:\n\n";

  $lines=@lang_docs/2;
  ++$lines if (@lang_docs%2);
  for my $index (0..$lines-1) {
    my $lang_doc=$lang_docs[$index];
    my $tlpobj = $tlpdb->get_package("$lang_doc");
    $keyval{$lang_doc_keys[$index]}="$lang_doc";
    printf " %s %s %-33s", $lang_doc_keys[$index], button($vars{"$lang_doc"}),
    substr($tlpobj->shortdesc,0,33);
    if (defined $lang_docs[$index+$lines]) {
      my $lang_doc=$lang_docs[$index+$lines];
      my $tlpobj = $tlpdb->get_package("$lang_doc");
      $keyval{$lang_doc_keys[$index+$lines]}="$lang_doc";
      printf " %s %s %-32s\n", $lang_doc_keys[$index+$lines],
      button($vars{"$lang_doc"}), substr($tlpobj->shortdesc,0,32);
    } else {
      print "\n";
    }
  }

  other_options qw (+ - R Q diskspace);
  my $answer=prompt 'Press key to select language';

  my @keystrokes=string_to_list $answer;

  foreach my $keystroke (@keystrokes) {
    if ($keystroke eq '-') {
      for my $collection (@languages,@lang_docs) {
        $vars{"$collection"}=0 if defined $vars{"$collection"};
      }
    }
    if ($keystroke eq '+') {
      for my $collection (@languages,@lang_docs) {
        $vars{"$collection"}=1 if defined $vars{"$collection"};
      }
    }
    if (defined $keyval{$keystroke}) {
      toggle "$keyval{$keystroke}";
    }
  }

  if (defined $keyval{"$answer"}) {
    # $vars{'selected_scheme'}=$keyval{"$answer"};
    return $command{'self'}->();
  }
  if (defined $command{"\u$answer"}) {
    return $command{"\u$answer"}->();
  } else {
    print "Unknown command: $answer\n\n";
    return $command{'self'}->();
  }
}


sub directories_menu
{
  my %command=(
    'self' => \&directories_menu,
    'R' => \&main_menu,
    'Q' => \&quit
      );

  my $maindir = $vars{'from_dvd'} ? 'TEXDIRW' : 'TEXDIR';

  menu_head "Current directories setup:";
  if (!TeXLive::TLUtils::texdir_check($vars{$maindir})) {
    print "!! The default location as given below can't be written to.
!! Either change the destination directory using <1> or create it
!! outside this script.
";
  }
  if ($vars{'from_dvd'}) {
  print <<"EOF";
 <1> TEXDIRW:      $vars{'TEXDIRW'}
     (root for configuration- and generated data)
EOF
  } else {
    print <<"EOF";
 <1> TEXDIR:       $vars{'TEXDIR'}
     support tree: $vars{'TEXDIR'}/texmf
EOF
  }
  print <<"EOF";

 <2> TEXMFLOCAL:     $vars{'TEXMFLOCAL'}
 <3> TEXMFSYSVAR:    $vars{'TEXMFSYSVAR'}
 <4> TEXMFSYSCONFIG: $vars{'TEXMFSYSCONFIG'}

 <5> TEXMFHOME:      $vars{'TEXMFHOME'}

EOF

  if (win32) {
    print " Note: ~ will expand to %USERPROFILE%\n";
  } else {
    print " Note: ~ will expand to \$HOME (or to %USERPROFILE% on Windows)\n";
  }

  other_options qw(R Q);
  my $answer = prompt 'Enter command';

  if ("\u$answer" eq '1') {
    print "New value for $maindir [$vars{$maindir}]: ";
    $answer = &input_dirname ();
    $vars{$maindir} = $answer if $answer ne "";
    if ($vars{$maindir}=~/^(.*)\/$texlive_release$/) {
      $vars{'TEXMFLOCAL'}="$1/texmf-local";
      $vars{'TEXMFSYSVAR'}="$1/$texlive_release/texmf-var";
      $vars{'TEXMFSYSCONFIG'}="$1/$texlive_release/texmf-config";
    } elsif ($vars{$maindir}=~/^(.*)$/) {
      $vars{'TEXMFLOCAL'}="$1/texmf-local";
      $vars{'TEXMFSYSVAR'}="$1/texmf-var";
      $vars{'TEXMFSYSCONFIG'}="$1/texmf-config";
    }
    $vars{'TEXDIRW'} = $vars{'TEXDIR'} unless $vars{'from_dvd'};
    return $command{'self'};

  } elsif ("\u$answer" eq '2') {
    print "New value for TEXMFLOCAL [$vars{'TEXMFLOCAL'}]: ";
    $answer = &input_dirname ();
    $vars{'TEXMFLOCAL'} = $answer if $answer ne "";
    return $command{'self'};

  } elsif ("\u$answer" eq '3') {
    print "New value for TEXMFSYSVAR [$vars{'TEXMFSYSVAR'}]: ";
    $answer = &input_dirname ();
    $vars{'TEXMFSYSVAR'} = $answer if $answer ne "";
    return $command{'self'};

  } elsif ("\u$answer" eq '4') {
    print "New value for TEXMFSYSCONFIG [$vars{'TEXMFSYSCONFIG'}]: ";
    $answer = &input_dirname ();
    $vars{'TEXMFSYSCONFIG'} = $answer if $answer ne "";
    return $command{'self'};

  } elsif ("\u$answer" eq '5') {
    print "New value for TEXMFHOME [$vars{'TEXMFHOME'}]: ";
    $answer = &input_dirname ();
    $vars{'TEXMFHOME'} = $answer if $answer ne "";
    return $command{'self'};
  }

  if (defined $command{"\u$answer"}) {
    return $command{"\u$answer"}->();
  } else {
    print "Unknown command: $answer\n\n";
    return $command{'self'}->();
  }
}


# Helper function to read a directory name and clean it up.
# 
sub input_dirname
{
  chomp (my $answer = <STDIN>);
  return "" if $answer eq "";
  
  $answer =~ s!\\!/!g if win32();  # switch to forward slashes

  my $home = getenv('HOME');
  $home = getenv('USERPROFILE') if win32();
  $home ||= '~';
  $answer =~ s/^~/$home/;          # $home expansion
  
  # relative paths are unlikely to work in texmf.cnf, et al.,
  # and don't have any apparent practical use.  Convert to absolute.
  if (! File::Spec->file_name_is_absolute($answer)) {
    $answer = Cwd::abs_path($answer);
  }
  return $answer;
}


$vars{'page'}=0;

sub html2text {
  my $filename=shift;
  my @text;
  open IN, "$filename";
  @all_lines=<IN>;
  close IN;
  chomp @all_lines;

  my $itemcnt;
  my $ordered_list=0;
  my $h1_indent=25;
  my $h2_indent=3;
  my $h3_indent=6;

  for (@all_lines) {
    next if /DOCTYPE/;
    next if /<!--/;
    next if /<title/i;
    next if /<\/?body/i;
    next if /<\/?html/i;
    next if /<\/?head/i;
    next if /<\/?meta/i;
    next if /^\s*$/; # ignore empty lines

    s/<i>/"/gi;  s/<\/i>/"/gi;  # italics
    s/<tt>/'/gi; s/<\/tt>/'/gi; # typewriter
    s/<p>.*//gi;                # paragraphs
    s/<\/ul>.*//gi;             # unsorted lists
    s/<\/ol>.*//gi;             # ordered lists
    s/&mdash;/--/gi;            # mdash
    s/&lt;/</gi; s/&gt;/>/gi;   # < and >
    if (/<h1>(.*?)<\/h1>/i) {
      push @text, " " x $h1_indent. "$1\n";
      push @text, " " x $h1_indent. "=" x (length $1). "\n";
      push @text, "\n";
    } elsif (/<h2>(.*?)<\/h2>/i) {
      push @text, "\n";
      push @text, " " x $h2_indent. "$1\n";
      push @text, " " x $h2_indent. "~" x (length $1). "\n";
      push @text, "\n";
    } elsif (/<h3>(.*?)<\/h3>/i) {
      push @text, "\n";
      push @text, " " x $h3_indent. "$1\n";
      push @text, " " x $h3_indent. "-" x (length $1). "\n";
      push @text, "\n";
    } elsif (/<ol>/i) {
      $ordered_list=1;
      $itemcnt=1;
    } elsif (/<ul>/i) {
      $ordered_list=0;
    } elsif (/^\s*<li>\s*(.*)/) {
      if ($ordered_list) {
        push @text, "\n";
        push @text, " $itemcnt. $1\n";
        ++$itemcnt;
      } else {
        push @text, "\n";
        push @text, " * $1\n";
      }
    } else {
      push @text, "$_\n";
    }
  }
  return @text;
}


sub help_menu {
  my %command=(
    'self' => \&help_menu,
    'R' => \&main_menu,
    'Q' => \&quit
      );
  my $installer_help="$installerdir/tlpkg/installer/install-tl.html";

  clear_screen;

  my @text=html2text "$installer_help";
  my $lines=(@text);
  my $overlap=3;
  my $lps=32; # lines per screen - overlap
  my $firstline=$vars{'page'}*$lps;
  my $lastline=$firstline+$lps+$overlap;
  my $line=0;
#  print "<<<$firstline>>> <<<$lastline>>>\n";
  for (@text) {
    print "$_" if ($line>=$firstline and $line<=$lastline);
    ++$line;
  }
  print "\n", hbar,
  "  <T> top  <N> next page  <P> previous page  <R> return"
      . "  <Q> quit         --", $vars{'page'}+1, "--\n";

  my $answer = prompt 'Enter command';

  if ("\u$answer" eq 'T') {
    $vars{'page'}=0;
    return $command{'self'};

  } elsif ("\u$answer" eq 'N') {
    $vars{'page'}+=1 unless $lastline>$lines;
    return $command{'self'};

  } elsif ("\u$answer" eq 'P') {
    $vars{'page'}-=1 if $vars{'page'}>=1;
    return $command{'self'};

  } elsif (defined $command{"\u$answer"}) {
    return $command{"\u$answer"};

  } else {
    print "Unknown command: $answer\n\n";
    return $command{'self'};
  }
}


sub options_menu {
  my $b_symlinks=button($vars{'option_symlinks'});
  my $b_doc=button($vars{'option_doc'});
  my $b_src=button($vars{'option_src'});
  my $b_fmt=button($vars{'option_fmt'});
  my $b_letter=button($vars{'option_letter'});

  my $sys_bin=$vars{'sys_bin'};
  my $sys_man=$vars{'sys_man'};
  my $sys_info=$vars{'sys_info'};

  my $t_sys_bin=($vars{'option_symlinks'})? $vars{'sys_bin'}:'';
  my $t_sys_man=($vars{'option_symlinks'})? $vars{'sys_man'}:'';
  my $t_sys_info=($vars{'option_symlinks'})? $vars{'sys_info'}:'';

  my %command=(
    'self' => \&options_menu,
    'R' => \&main_menu,
    'Q' => \&quit
      );

  clear_screen;
  menu_head "Current options setup:";

  print <<"EOF";
 <P> use letter size instead of A4 by default: $b_letter
 <F> create all format files:                  $b_fmt
EOF
;
  if ($vars{'doc_splitting_supported'} and !$vars{'from_dvd'}) {
    print " <D> install font/macro doc tree:              $b_doc\n";
  }
  if ($vars{'src_splitting_supported'} and !$vars{'from_dvd'}) {
    print " <S> install font/macro source tree:           $b_src\n";
  }
  if (unix() && !$vars{'from_dvd'}) {
    print <<"EOF";
 <L> create symlinks in standard directories:  $b_symlinks
            binaries to: $t_sys_bin
            manpages to: $t_sys_man
                info to: $t_sys_info
EOF
;
  }
  other_options qw(R Q diskspace);
  my $answer = prompt 'Enter command';

  if (unix()) {
    if (("\u$answer" eq 'L') and !$vars{'from_dvd'}) {
      my $home = getenv('HOME');
      $home = getenv('USERPROFILE') if (win32());
      $home ||= '~';
      toggle 'option_symlinks';
      if ($vars{'option_symlinks'}) {
        print "New value for binary directory [$sys_bin]: ";
        chomp($answer=<STDIN>);
        $vars{'sys_bin'} =  "$answer" if (length $answer);
        $vars{'sys_bin'} =~ s@\\@/@g if (win32());
        $vars{'sys_bin'} =~ s/^~/$home/;
        if ($vars{'sys_bin'}=~/^(.*)\/bin$/) {
          $vars{'sys_man'}="$1/man";
          $vars{'sys_info'}="$1/info";
        }
        print "New value for man directory    [$vars{'sys_man'}]: ";
        chomp($answer=<STDIN>);
        $vars{'sys_man'}="$answer" if (length $answer);
        $vars{'sys_man'} =~ s@\\@/@g if (win32());
        $vars{'sys_man'} =~ s/^~/$home/;

        print "New value for info directory   [$vars{'sys_info'}]: ";
        chomp($answer=<STDIN>);
        $vars{'sys_info'}="$answer" if (length $answer);
        $vars{'sys_info'} =~ s@\\@/@g if (win32());
        $vars{'sys_info'} =~ s/^~/$home/;
      }
      return $command{'self'};
    }
  }

  if ("\u$answer" eq 'P') {
    toggle 'option_letter';
    return $command{'self'};

  } elsif ("\u$answer" eq 'F') {
    toggle 'option_fmt';
    return $command{'self'};

  } elsif ("\u$answer" eq 'S' and !$vars{'from_dvd'}) {
    toggle 'option_src';
    return $command{'self'};

  } elsif ("\u$answer" eq 'D' and !$vars{'from_dvd'}) {
    toggle 'option_doc';
    return $command{'self'};

  } elsif (defined $command{"\u$answer"}) {
    return $command{"\u$answer"};

  } else {
    print "Unknown command: $answer\n\n";
    return $command{'self'};
  }
}


sub quit {
  exit 0;
  $RETURN = $MENU_QUIT;
}

sub do_install {
  $RETURN = $MENU_INSTALL;
}

sub dvd_hd_toggle {
  if ($vars{'from_dvd'}) {  # set up for hd install:
    $vars{'from_dvd'} = 0;
    $vars{'TEXDIR'} = $vars{'TEXDIRW'};
  } else {                  # set up for dvd install:
    $vars{'from_dvd'} = 1;
    $vars{'TEXDIR'} = abs_path($::installerdir);
  }
  main_menu;
}

sub main_menu {
  my $this_platform=platform_desc($vars{'this_platform'});

  my $b_symlinks=button($vars{'option_symlinks'});
  my $b_doc=button($vars{'option_doc'});
  my $b_src=button($vars{'option_src'});
  my $b_fmt=button($vars{'option_fmt'});
  my $b_letter=button($vars{'option_letter'});

  my $warn_nobin;

  $warn_nobin=set_install_platform;

  my $maindir = $vars{'from_dvd'} ? 'TEXDIRW' : 'TEXDIR';

  $vars{'n_systems_selected'}=0;
  $vars{'n_collections_selected'}=0;
  foreach my $key (keys %vars) {
    if ($key=~/^binary.*/) {
      ++$vars{'n_systems_selected'} if $vars{$key}==1;
    }
    if ($key=~/^collection/) {
      ++$vars{'n_collections_selected'} if $vars{$key}==1;
    }
  }
  calc_depends();

  my %command = (
    'self' => \&main_menu,
    'D' => \&directories_menu,
    'H' => \&help_menu,
    'I' => \&do_install,
    'O' => \&options_menu,
    'Q' => \&quit,
    'V' => \&dvd_hd_toggle,
  );
  if (!$vars{'from_dvd'}) {
    $command{'B'} = \&binary_menu if unix();
    $command{'C'} = \&collection_menu;
    $command{'L'} = \&language_menu;
    $command{'S'} = \&scheme_menu;
  }

  clear_screen;
  print <<"EOF";
======================> TeX Live installation procedure <=====================

=======> Note: Letters/digits in <angle brackets> indicate menu items <=======
=======>       for commands or configurable options                   <=======

EOF

  if (!$vars{'from_dvd'}) {
    print <<"EOF";
 Detected platform: $this_platform
 $warn_nobin
 <B> binary systems: $vars{'n_systems_selected'} out of $vars{'n_systems_available'}

 <S> Installation scheme ($vars{'selected_scheme'})
     $vars{'n_collections_selected'} collections out of $vars{'n_collections_available'}, disk space required: $vars{'total_size'} MB

 Customizing installation scheme:
   <C> standard collections
   <L> language collections
EOF

    if (!check_on_lang_collection_installed()) {
      print "     !! No language specific collection selected!\n";
      print "     !! If you only write English documents that is fine!\n";
    }

    print <<"EOF";

 <D> directories:
   TEXDIR (the main TeX directory):
EOF
  } else { # $vars{'from_dvd'}
    print <<"EOF";
 <D> directories:
   TEXDIRW (Writable root):
EOF
  }

  if (TeXLive::TLUtils::texdir_check($vars{$maindir})) {
    print "     $vars{$maindir}\n";
  } else {
    print "     !! default location: $vars{$maindir}\n";
    print "     !! is not writable, please select a different one!\n";
  }
  print <<"EOF";
   TEXMFLOCAL (directory for site-wide local files):
     $vars{'TEXMFLOCAL'}
   TEXMFSYSVAR (directory for variable and automatically generated data):
     $vars{'TEXMFSYSVAR'}
   TEXMFSYSCONFIG (directory for local config):
     $vars{'TEXMFSYSCONFIG'}
   TEXMFHOME (directory for user-specific files):
     $vars{'TEXMFHOME'}

EOF

print <<"EOF";
 <O> options:
   $b_letter use letter size instead of A4 by default
   $b_fmt create all format files
EOF

  if (!$vars{'from_dvd'}) {
    if ($vars{'doc_splitting_supported'}) {
      print "   $b_doc install macro/font doc tree\n";
    }
    if ($vars{'src_splitting_supported'}) {
      print "   $b_src install macro/font source tree\n";
    }
    if (unix()) {
      print "   $b_symlinks create symlinks in standard directories\n";
    }
  }

  if ($vars{'from_dvd'}) {
    print "\n <V> set up for installing to hard disk\n";
  } else {
    print "\n <V> set up for running from DVD\n";
  }

  other_options qw(I H Q);
  my $answer=prompt 'Enter command';

  if (defined $command{"\u$answer"}) {
    return $command{"\u$answer"};
  } else {
    print "Unknown command: $answer\n\n";
    return $command{'self'};
  }
}

# needs a terminal 1 for require to succeed!
1;

__END__

### Local Variables:
### perl-indent-level: 2
### tab-width: 2
### indent-tabs-mode: nil
### End:
# vim:set tabstop=2 expandtab: #
