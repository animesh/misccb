#!/usr/bin/env perl
# $Id: install-translations.pl 12147 2009-02-12 02:01:04Z karl $
#
# Copyright 2009 Norbert Preining
# This file is licensed under the GNU General Public License version 2
# or any later version.
#

use strict;
$^W = 1;

use utf8;
no utf8;

if (defined($::opt_lang)) {
  $::lang = $::opt_lang;
  if ($::lang eq "zh") {
    # set language to simplified chinese
    $::lang = "zh-cn";
  }
} else {
  if ($^O =~ /^MSWin(32|64)$/i) {
    # trying to deduce automatically the country code
    my $foo =  TeXLive::TLWinGoo::reg_country();
    if ($foo) {
      $::lang = $foo;
    } else {
      debug("Didn't get any usuful code from reg_country: $foo...\n");
    }
  } else {
    # we load POSIX and locale stuff
    require POSIX;
    import POSIX qw/locale_h/;
    # now we try to deduce $::lang
    my $loc = setlocale(&POSIX::LC_MESSAGES);
    my ($lang,$area,$codeset);
    if ($loc =~ m/^([^_.]*)(_([^.]*))?(\.([^@]*))?(@.*)?$/) {
      $lang = defined($1)?$1:"";
      $area = defined($3)?$3:"";
      if ($lang eq "zh") {
        if ($area =~ m/^(TW|HK)$/i) {
          $lang = "zh-tw";
        } else {
          # fallback to zh-cn for anything else, that is
          # zh-cn, zh-sg, zh, and maybe something else
          $lang = "zh-cn";
        }
      }
    }
    $::lang = $lang if ($lang);
  }
}


our %text = (title      => 'TeX Live 2008 Installation',
             basicinfo  => 'Basic Information',
             custom     => 'Further Customization',
             dirsetup   => "Directory setup",
             options    => 'Options',
             sysint     => 'System Integration',
             change     => 'Change',
             toggle     => 'Toggle',
             install    => 'Install TeX Live',
             finbut     => 'Finish',
             quit       => 'Quit',
             ok         => 'Ok',
             cancel     => 'Cancel',
             status     => 'Status output',
             changevar  => 'Change variable value',
             enterpath  => 'Enter path for',
             hinthome   => '(use ~ for %%%)',
             selectscheme => 'Select a scheme',
             selectstdcol => 'Select the collections to be installed',
             selectall  => 'Select All',
             selectnone => 'Deselect All',
             selectlang => 'Select language support',
             selectdoc  => 'Select language-specific documentation',
             createsym  => 'create symlinks in standard directories',
             binto      => 'binaries to',
             manto      => 'manpages to',
             infoto     => 'info to',
             selectsys  => 'Select arch-os',
             outof      => 'out of',
             collof     => 'collections out of',
             diskreq    => 'disk space required',
             yes        => 'Yes',
             no         => 'No',
             notwritable => '(default not writable - please change!)',
             changetexdir => '(please change TEXDIR first!)',
             nolangcol  => '(no language collection selected!)',
             finished   => 'See TEXDIR/index.html for links to documentation.\nThe TeX Live web site (http://tug.org/texlive/) contains any updates and corrections. TeX Live is a joint project of the TeX user groups around the world; please consider supporting it by joining the group best for you. The list of groups is available on the web at http://tug.org/usergroups.html.',
             finishedpath => 'Add TEXDIR/texmf/doc/man to MANPATH.\nAdd TEXDIR/texmf/doc/info to INFOPATH.\nMost importantly, add TEXDIR/bin/PLATFORM\nto your PATH for current and future sessions.',
             welcome => 'Welcome to TeX Live!',
             next       => 'Next >',
             prev       => '< Back',
             wizhello   => "Welcome to the installation of TeX Live 2008\n"
               . "http://tug.org/texlive\n\n"
               . "This wizard will guide you through the installation.\n\n"
               . "For an advanced, customizable installation, please consult\n"
               . "the web pages or installation guide.",
             destfolder => 'Destination folder:',
             advcustom  => 'Advanced customization',
             pathinfo   => "The destination folder will contain the installation.\nIt is strongly recommended to keep the year as the last component.",
             readyinst  => "We are ready to install TeX Live 2008.\nThe following settings will be used.\nIf you want to change something please go back,\notherwise press the \"Install\" button.",
             instshort  => 'Install',
             wizard     => 'Go to Wizard',
    );


our %labels = (binsys   => 'Binary System(s)',
               scheme   => 'Selected Scheme',
               stdcoll  => 'Standard Collections',
               langcoll => 'Language Collections',
               texdir   => 'TEXDIR (the main TeX directory)',
               localdir => 'TEXMFLOCAL (directory for site-wide local files)',
               sysvardir   => 'TEXMFSYSVAR (directory for autogenerated data)',
               sysconfigdir   => 'TEXMFSYSCONFIG (directory for local config)',
               texmfhome => 'TEXMFHOME (directory for user-specific files)',
               optletter => 'Use letter size instead of A4 by default',
               optfmt   => 'Create all format files',
               optdoc   =>  'Install font/macro doc tree',
               optsrc   =>  'Install font/macro source tree',
               symlink  =>  'Create symlinks in system directories',
    );

if (($::lang ne "en") && ($::lang ne "C")) {
  if (! -r "$::installerdir/tlpkg/installer/lang/perltk-$::lang") {
    tlwarn ("\n  Sorry, no translations available for $::lang; falling back to English.
  (If you'd like to help translate the installer's messages, please see
  http://tug.org/texlive/doc.html#install-tl-xlate for information.)\n\n");
  } else {
    # merge the translated strings into the text string
    open(LANG, "<$::installerdir/tlpkg/installer/lang/perltk-$::lang");
    my %trans;
    while (<LANG>) {
      chomp;
      next if m/^\s*#/;
      next if m/^\s*$/;
      my ($a,$b) = split(/:/,$_,2);
      $b =~ s/^\s*([^\s])/$1/;
      $b =~ s/\s*$//;
      if (!utf8::decode($b)) {
        warn("decoding string to utf8 didn't work:$b\n");
      }
      $b =~ s/\\n/\n/g;
      $trans{"$a"} = "$b";
    }
    close(LANG);
    foreach my $k (keys %text) {
      $text{$k} = $trans{"text.$k"} if defined($trans{"text.$k"});
    }
    foreach my $k (keys %labels) {
      $labels{$k} = $trans{"label.$k"} if defined($trans{"label.$k"});
    }
  }
}


1;

__END__

### Local Variables:
### perl-indent-level: 2
### tab-width: 2
### indent-tabs-mode: nil
### End:
# vim:set tabstop=2 expandtab: #

