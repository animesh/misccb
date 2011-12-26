# guinb1.pl
# $Id: do_listframe.pl 12151 2009-02-12 12:07:22Z preining $
#
# Copyright 2008 Tomasz Luczak, Norbert Preining
#
# GUI for tlmgr
#


sub validate_search_input {
  my ($w, $iw, $str, @l) = @_;
  my $found = -1;
  for (my $i = 0; $i < @l; $i++) {
    my $pkg = $l[$i];
    $pkg =~ s/^\s*(\(i\) )?//;
    if ($pkg =~ m;$str;i) {
      $found = $i;
      last;
    }
  }
  if ($found >= 0) {
    $w->selectionClear(0,"end");
    $w->yview($found);
    $w->selectionSet($found);
    update_info_window($w, $iw, @l);
  } else {
    $w->selectionClear(0,"end");
  }
  return(1);
}
 
sub find_next_search_match {
  my ($w, $e, $iw, @l) = @_;
  # first get the current selection
  my @cursel = $w->curselection;
  if (!@cursel) {
    push @cursel, 0;
  }
  my $str = $e->get;
  my $found = -1;
  for (my $i = $cursel[0] + 1; $i < @l; $i++) {
    my $pkg = $l[$i];
    $pkg =~ s/^\s*(\(i\) )?//;
    if ($pkg =~ m;$str;i) {
      $found = $i;
      last;
    }
  }
  if ($found >= 0) {
    $w->selectionClear(0,"end");
    $w->yview($found);
    $w->selectionSet($found);
    update_info_window($w, $iw, @l);
  }
}

sub update_info_window {
  my ($lb, $iw, @l) = @_;
  my @selind = $lb->curselection;
  return if (!@selind);
  my $pkgname = $l[$selind[0]];
  $pkgname =~ s/^\s*(\(i\) )?//;
  my $tlp;
  my $longdesc;
  my $shortdesc;
  if (defined($tlmediatlpdb)) {
    $tlp = $tlmediatlpdb->get_package($pkgname);
  } else {
    $tlp = $localtlpdb->get_package($pkgname);
  }
  if (defined($tlp)) {
    $longdesc = $tlp->longdesc;
    $shortdesc = $tlp->shortdesc;
  }
  my $text = "";
  if ($shortdesc) {
    $text .= $shortdesc;
    $text .= "\n\n";
  }
  if ($longdesc) {
    my @words = split /\s+/, $longdesc;
    my $i = 0;
    while (@words) {
      my $w = shift @words;
      my $l = length($w);
      if ($i + $l + 1 < 45) {
        $text .= " $w";
        $i += $l + 1;
      } else {
        $text .= "\n$w";
        $i = $l;
      }
    }
  }
  if ($text eq "") {
    $text = ___"nodescription";
  }
  $iw->delete("0.0", "end");
  $iw->insert("0.0", "$text");
  $iw->see("0.0");
}

sub do_listframe {
  my ($f, $title, $listref, $buttonsref, $with_force, $with_deps) = @_;

  # row 1, column 1-2
  my $f_title = $f->Frame(-relief => 'ridge', -borderwidth => 2);
  $f_title->Label( -text => $title,
                   -foreground => "blue", 
                   -font => "helvetica 10 bold"
                 )->pack(-side => "top");
  $f_title->Label( -text => ___"ctrlshift"
                 )->pack(-side => "top");
  $f_title->grid( -row => 1, -column => 1, -columnspan => 2,
                  -padx => "2m", -pady => "2m", -sticky => "we");

  # column 1, row 2-3

  my $f_listf = $f->Labelframe(-text => ___"selpkg");
  $f_listf->grid( -row => 2, -column => 1, -rowspan => 2,
                  -sticky => "nswe", -padx => "2m", -pady => "1m");

  my $f_listf_lb;
  my $f_textf_text;
  my $f_listf_searchf = $f_listf->Frame;
  $f_listf_searchf->pack(-pady => "1m");
  my $f_listf_searchf_label = 
    $f_listf_searchf->Label(-text => ___"search")->pack( -anchor => "w", 
                                                      -side => "left", 
                                                      -padx => "1m", 
                                                      -pady => "1m");
  my $f_listf_searchf_entry = $f_listf_searchf->Entry( -validate => "key", 
    -validatecommand => 
      sub { validate_search_input($f_listf_lb, 
                                  $f_textf_text, 
                                  $_[0], 
                                  @$listref); });
  my $f_listf_searchf_button = $f_listf_searchf->Button(-text => ___"next", 
      -command => sub { find_next_search_match($f_listf_lb, 
                                               $f_listf_searchf_entry, 
                                               $f_textf_text, 
                                               @$listref);  });
  $f_listf_searchf_entry->pack(
    -anchor => "w", -side => "left", -padx => "1m", -pady => "1m");
  $f_listf_searchf_button->pack(
    -anchor => "w", -side => "left", -padx => "1m", -pady => "1m");

  $f_listf_lb = $f_listf->Scrolled("Listbox",
    -listvariable => $listref,
    -selectmode => "extended",
    -scrollbars => "ose"
  );
  $f_listf_lb->bind('<<ListboxSelect>>', 
    sub { update_info_window ($f_listf_lb, $f_textf_text, @$listref); });
  $f_listf_lb->pack(-fill => "both", -expand => 1);

  # row 2 column 2
  my $f_textf = $f->Labelframe(-text => ___"infoitem");
  $f_textf->grid(-row => 2, -column => 2, -sticky => "nswe", 
                 -padx => "2m", -pady => "1m");

  # we would like to have -scrollbars => "oe" here so that it does disappear
  # if the text is not too long, but this doesn't work since Windows Perl/Tk
  # is broken and does not update the scrollbar properly, you have to first
  # select something while scrolling, then finally it works. Ok, so show
  # it all the time ...
  $f_textf_text = $f_textf->Scrolled("ROText", 
    -scrollbars => "e",
    -width => 45, -wrap => "word");

  $f_textf_text->pack(-expand => 1, -fill => "both");

  # row 3 column 2
  my $f_buttonf = $f->Labelframe();
  $f_buttonf->grid(-row => 3, -column => 2, 
      -padx => "2m", -pady => "2m", -sticky => "we");

  my $f_buttonf_optionsf = $f_buttonf->Frame();
  if ($with_force) {
    my $foo = $f_buttonf_optionsf->Checkbutton(-text => ___"force", 
                                     -variable => \$opts{"force"}
                                    )->pack(-side => 'left');
    $balloon->attach($foo,-balloonmsg => ___"forceballoon");
  }
  if ($with_deps) {
    my $foo = $f_buttonf_optionsf->Checkbutton(-text => ___"withoutdep", 
                                   -variable => \$opts{"no-depends"}
                                  )->pack(-side => 'left');
    $balloon->attach($foo,-balloonmsg => ___"nodepballoon");
  }
  $f_buttonf_optionsf->pack;
  foreach my $k (keys %$buttonsref) {
    $f_buttonf->Button(-text => $buttonsref->{$k}{'-text'},
      -command => sub { my @l = $f_listf_lb->curselection;
                        my @pl;
                        foreach my $i (@l) {
                          my @all = @$listref;
                          my $foo = $all[$i];
                          $foo =~ s/^\s*(\(i\) )?//;
                          push @pl, $foo;
                        }
                        my $coderef = $buttonsref->{$k}{'-command'};
                        my @allargs;
                        if (defined($buttonsref->{$k}{'-args'})) {
                          push @allargs, @{$buttonsref->{$k}{'-args'}};
                        }
                        push @allargs, @pl;
                        &$coderef(@allargs);
                        $f_listf_lb->configure(-listvariable => $listref);
                      })->pack( -expand => 0, -fill => "both",
                                -padx => "2m", -pady => "2m");
  }
  $f->gridRowconfigure(1,-weight => 0);
  $f->gridRowconfigure(2,-weight => 1);
  $f->gridRowconfigure(3,-weight => 0);
  $f->gridColumnconfigure(1,-weight => 1);
  $f->gridColumnconfigure(2,-weight => 1);
  return($f_textf_text, $f_listf_lb);
}

1;

### Local Variables:
### perl-indent-level: 2
### tab-width: 2
### indent-tabs-mode: nil
### End:
# vim:set tabstop=2 expandtab: #
