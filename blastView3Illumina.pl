#
# Author: Jarrod Chapmon, Isaac Ho
#
# Copyright 2011 The Regents of the University of California.
# All rights reserved.

# The United States Government has rights in this work pursuant
# to contracts DE-AC03-76SF00098, W-7405-ENG-36 and/or
# W-7405-ENG-48 between the United States Department of Energy
# and the University of California.

# Redistribution and use in source and binary forms are permitted
# provided that: (1) source distributions retain this entire
# copyright notice and comment, and (2) distributions including
# binaries display the following acknowledgement:  "This product
# includes software developed by the University of California,
# JGI-PSF and its contributors" in the documentation or other
# materials provided with the distribution and in all advertising
# materials mentioning features or use of this software.  Neither the
# name of the University nor the names of its contributors may be
# used to endorse or promote products derived from this software
# without specific prior written permission.

# THIS SOFTWARE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE.

#!/jgi/tools/bin//perl -w
#
# blastView3.pl by Jarrod Chapman <jchapman@lbl.gov> Fri Apr  8 14:13:14 PDT 2005
# Copyright 2005 Jarrod Chapman. All rights reserved.
#
#use strict;
#
#use Tk;
#use Tk::ROText;
#use Tk::Adjuster;

use Getopt::Std;

use Benchmark;

$, = "\t";

my %opts = ();
my $validLine = getopts('b:p:S:', \%opts);
my @required = ("b",);
my $nRequired = 0;
map {$nRequired += exists($opts{$_})} @required;
$validLine &= ($nRequired == @required);
if ($validLine != 1) {
    dumpUsage();
}

my @B62 = (  4, -1, -2, -2, 0, -1, -1, 0, -2, -1, -1, -1, -1, -2, -1, 1, 0, -3, -2, 0, -2, -1, 0, -4,
	     -1, 5, 0, -2, -3, 1, 0, -2, 0, -3, -2, 2, -1, -3, -2, -1, -1, -3, -2, -3, -1, 0, -1, -4,
	     -2, 0, 6, 1, -3, 0, 0, 0, 1, -3, -3, 0, -2, -3, -2, 1, 0, -4, -2, -3, 3, 0, -1, -4,
	     -2, -2, 1, 6, -3, 0, 2, -1, -1, -3, -4, -1, -3, -3, -1, 0, -1, -4, -3, -3, 4, 1, -1, -4,
	     0, -3, -3, -3, 9, -3, -4, -3, -3, -1, -1, -3, -1, -2, -3, -1, -1, -2, -2, -1, -3, -3, -2, -4,
	     -1, 1, 0, 0, -3, 5, 2, -2, 0, -3, -2, 1, 0, -3, -1, 0, -1, -2, -1, -2, 0, 3, -1, -4,
	     -1, 0, 0, 2, -4, 2, 5, -2, 0, -3, -3, 1, -2, -3, -1, 0, -1, -3, -2, -2, 1, 4, -1, -4,
	     0, -2, 0, -1, -3, -2, -2, 6, -2, -4, -4, -2, -3, -3, -2, 0, -2, -2, -3, -3, -1, -2, -1, -4,
	     -2, 0, 1, -1, -3, 0, 0, -2, 8, -3, -3, -1, -2, -1, -2, -1, -2, -2, 2, -3, 0, 0, -1, -4,
	     -1, -3, -3, -3, -1, -3, -3, -4, -3, 4, 2, -3, 1, 0, -3, -2, -1, -3, -1, 3, -3, -3, -1, -4,
	     -1, -2, -3, -4, -1, -2, -3, -4, -3, 2, 4, -2, 2, 0, -3, -2, -1, -2, -1, 1, -4, -3, -1, -4,
	     -1, 2, 0, -1, -3, 1, 1, -2, -1, -3, -2, 5, -1, -3, -1, 0, -1, -3, -2, -2, 0, 1, -1, -4,
	     -1, -1, -2, -3, -1, 0, -2, -3, -2, 1, 2, -1, 5, 0, -2, -1, -1, -1, -1, 1, -3, -1, -1, -4,
	     -2, -3, -3, -3, -2, -3, -3, -3, -1, 0, 0, -3, 0, 6, -4, -2, -2, 1, 3, -1, -3, -3, -1, -4,
	     -1, -2, -2, -1, -3, -1, -1, -2, -2, -3, -3, -1, -2, -4, 7, -1, -1, -4, -3, -2, -2, -1, -2, -4,
	     1, -1, 1, 0, -1, 0, 0, 0, -1, -2, -2, 0, -1, -2, -1, 4, 1, -3, -2, -2, 0, 0, 0, -4,
	     0, -1, 0, -1, -1, -1, -1, -2, -2, -1, -1, -1, -1, -2, -1, 1, 5, -2, -2, 0, -1, -1, 0, -4,
	     -3, -3, -4, -4, -2, -2, -3, -2, -2, -3, -2, -3, -1, 1, -4, -3, -2, 11, 2, -3, -4, -3, -2, -4,
	     -2, -2, -2, -3, -2, -1, -2, -3, 2, -1, -1, -2, -1, 3, -3, -2, -2, 2, 7, -1, -3, -2, -1, -4,
	     0, -3, -3, -3, -1, -2, -2, -3, -3, 3, 1, -2, 1, -1, -2, -2, 0, -3, -1, 4, -3, -2, -1, -4,
	     -2, -1, 3, 4, -3, 0, 1, -1, 0, -3, -4, 0, -3, -3, -2, 0, -1, -4, -3, -3, 4, 1, -1, -4,
	     -1, 0, 0, 1, -3, 3, 4, -2, 0, -3, -3, 1, -1, -3, -1, 0, -1, -3, -2, -2, 1, 4, -1, -4,
	     0, -1, -1, -1, -2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -2, 0, 0, -2, -1, -1, -1, -1, -1, -4,
	     -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, 1 );

my @AAsyms = ("A","R","N","D","C","Q","E","G","H","I","L","K","M","F","P","S","T","W","Y","V","B","Z","X","*");

my %BLOSUM62 = ();
for (my $i = 0; $i < 24; $i++) {
    for (my $j = 0; $j < 24; $j++) {
	$BLOSUM62{$AAsyms[$i] . $AAsyms[$j]} = $B62[$i*24+$j];
    }
}

my %hitFieldIndex = ("length",0,"nHSPs",1,"annotation",2,"startIndex",3,
		     "minStart",4,"maxStop",5,"maxLength",6);


my %alignmentFieldIndex = ("alignID",0,"score",1,"eValue",2,"strand",3,
			   "matchesover",4,"identities",5,"qStart",6,
			   "qStop",7,"sStart",8,"sStop",9,"sSeq",10,"qSeq",11);

my %HSPArrowFieldIndex = ("arrowID",0,"depth",1,"alignmentInfo",2);

#Parse options

my $blastFile = $opts{"b"};
open (F,$blastFile) || die "Error: Couldn't open $blastFile for reading\n";

my %validPrintOptions = ("hits"=>"all|best",
			 "HSPs"=>"all|best",
			 "semiglobal"=>"0|1",
			 "sequence"=>"0|1",
			 );
my %printOptions = ("hits"=>"all","HSPs"=>"all","semiglobal"=>0,"sequence"=>1);
if (exists($opts{"p"})) {
    my @printOptions = split(/\,/,$opts{"p"});
    foreach my $option (@printOptions) {
	dumpUsage() unless ($option =~ /\S+=\S+/);
	my ($key,$value) = $option =~ /(\S+)=(\S+)/;
	my $validValues = $validPrintOptions{$key};
	$validValues =~ s/\|/\$\|\^/g;
	dumpUsage() unless (exists($validPrintOptions{$key}) && $value =~ /^$validValues$/);
	$printOptions{$key} = $value;
    }    
}

my $minSeed = 0;
if(exists($opts{"S"})) {
    $minSeed = $opts{ "S" };
}

my $fileFormat = undef;
my $firstLine = <F>;
if ($firstLine =~ /^T?BLAST[PXN]/) {
    $fileFormat = "ncbi";
} elsif ($firstLine =~ /^BLAST_TYPE/ ) {
    $fileFormat = "bv3";
} else {
    die "\nError: $blastFile not a recognized format (NCBI or bv3)\n";
}
seek(F,0,0);

my $mode = "viewing";
if (exists($opts{"p"})) {
    $mode = "parsing";
}
print STDERR "Welcome to blastView3!\nUsing file format \"$fileFormat\"\nOpening $blastFile for $mode";

# Find beginning of each query 
my @queryNames = ();
my @jumpPoints = ();
my $nQueries = 0;
my $filePos = 0;
if ($fileFormat eq "ncbi") {
    while (my $line = <F>) {
	if ($line =~ /^T?BLAST[PXN]/) {
	    $jumpPoints[$nQueries] = $filePos;
	    print STDERR ".";
	} elsif ($line =~ /^Query=/) {
	    my ($queryName) = $line =~ /^Query=\s+(.+)/;	
	    $queryNames[$nQueries] = $queryName;
	    $nQueries++;
	}
	$filePos = tell(F);
    }
    print STDERR "Done\nFound $nQueries queries.\n";
} elsif ($fileFormat eq "bv3") {
    my $line = <F>;
    unless ($line =~ /^BLAST_TYPE\tQUERY\tQ_START\tQ_STOP\tQ_LENGTH\tSUBJECT\tS_START\tS_STOP\tS_LENGTH\tSTRAND\tSCORE\tE_VALUE\tIDENTITIES\tALIGN_LENGTH\tQ_SEQUENCE\tS_SEQUENCE$/) {
	die "\nError: $blastFile not a valid bv3 format\n";
    }
    $filePos = tell(F);
    my $lastQuery = undef;
    while (my $line = <F>) {
	my @cols = split(/\t/,$line);
	if ((!defined($lastQuery)) || ($cols[1] ne $lastQuery)) {
	    my $queryName = $cols[1];
	    $jumpPoints[$nQueries] = $filePos;
	    print STDERR ".";
	    $queryNames[$nQueries] = $queryName;
	    $nQueries++;
	    $lastQuery = $queryName; 
	}
	$filePos = tell(F);
    }
    print STDERR "Done\nFound $nQueries queries.\n";
}
seek(F,$jumpPoints[0],0);

my $currentQuery = -1;
my $blastVersion = undef;
my $blastType = undef;
my $queryUnits = undef;
my $subjectUnits = undef;
my $queryName = undef;
my $queryLength = undef;
my $nHits = undef;
my $nAlignments = undef;
my %hitInfo = ();
my %cloneIDs = ();
my %HSPArrows = ();
my $colorScheme = "Score";
my %colorSettings = ("%ID",[0.93,0.95,0.97,0.99],"%Aligned",[0.25,0.50,0.75,1.0]);

if (exists($opts{"p"})) {

    printBV3Header(*STDOUT);
    
    for (my $queryIndex = 0; $queryIndex < $nQueries; $queryIndex++) {
	seek(F,$jumpPoints[$queryIndex],0);
	parseBlast($fileFormat);
	printBlast(\%printOptions, *STDOUT);
    }
    shutItDown();
}

# --- WIDGETS ---

# --- WIDGET-PARAMS ---
my $canvasWidth = 1000;
my $canvasHeight = 400;
my $charWidth = 9;
my $textLines = 8;
my $spacer = 10;
my $binResolution = 1;
my $arrowWidth = $spacer/2;
my $maxTicks = 10;

my $xScale = 1.0;
my $maxXScale = 1.0;
my $minXScale = 1.0;
my $yScale = 1.0;
my $maxYScale = 1.0;
my $minYScale = 1.0;
my $zoomResolution = log(sqrt(sqrt(2)));

my $hbins = undef;
my $vbins = undef;
my @binFilled = ();
resetBins();

# The top level window
# --------------------
my $main = new MainWindow;
$main->resizable(1,1);
$main->focusFollowsMouse;

#main screen areas
my $menuFrame = $main->Frame(-relief => 'ridge', -borderwidth => 2);
my $queryListFrame = $main->Frame();
my $adjuster1 = $main->Adjuster();
my $multiViewFrame = $main->Frame();
my $adjuster2 = $main->Adjuster();
my $HSPInfoFrame = $main->Frame();

#pack them
$menuFrame->pack(-side => 'top', -anchor => 'n', -fill => 'x', -expand => 0);
$HSPInfoFrame->pack(-side => 'top', -fill => 'both', -expand => 1);
$adjuster1->packAfter($HSPInfoFrame, -side => 'top');
$multiViewFrame->pack(-side => 'top', -fill => 'both', -expand => 1);
$adjuster2->packAfter($multiViewFrame, -side => 'top');
$queryListFrame->pack(-side => 'top', -fill => 'both', -expand=>1);

# Some menu items
my @menus = ();
foreach my $item (qw/File Colors HSPInfo/) {
    push (@menus, $menuFrame->Menubutton(-text => $item)->pack(-side=>'left'));
}
$menus[0]->command(-label => "Save Alignments", -command => \&saveBV3);
$menus[0]->command(-label => "Save Postscript", -command => \&savePostscript);
$menus[0]->command(-label => "Author", -command => sub { 
    $main->messageBox(-title => "Author",
		      -message => "Thanks for asking!\n Jarrod Chapman\n <jchapman\@lbl.gov>\n wrote blastView3.pl\n",
		      -type => 'OK',
		      -icon => 'info',
	);
		   });
$menus[0]->command(-label => "Exit", -command => sub{shutItDown()});

foreach my $scheme (qw/Orientation %ID %Aligned Score IsSemiglobal SubCoord/) {
    $menus[1]->radiobutton(-label => $scheme,
			   -command => sub {
			       updateColorValues('load');
			       foreach my $arrowRef (values(%HSPArrows)) {
				   setArrowColor($arrowRef);
			       }
			   }, 
			   -variable => \$colorScheme,
			   -value => $scheme);
}

my @HSPPrintFields = qw(Identity Accuracy Extents Alignment);
my %HSPPrintFields = ();
foreach my $field (@HSPPrintFields) {
    $HSPPrintFields{$field} = 1;
    $menus[2]->checkbutton(-label => $field,
			   -variable => \$HSPPrintFields{$field},
			   );
}

# A canvas
my $multiViewScroll = $multiViewFrame->Scrolled("Canvas", -scrollbars => 'se')->pack(-expand => 1, 
										     -fill =>'both');

my $multiViewCanvas = $multiViewScroll->Subwidget("canvas");
$multiViewCanvas->configure( -width => $canvasWidth, -height => $canvasHeight,
			     -background => 'black',
			     -xscrollincrement => 10, -yscrollincrement => 10
			     );

my $queryStatusIndicator = "";
my $HSPArrowStatusIndicator = "";
my $mouseOverStatusIndicator = "";
my $mouseOverLabel = $multiViewFrame->Label(-anchor=>'w',
					    -borderwidth=>2, 
					    -relief=>'groove',
					    -textvariable => \$mouseOverStatusIndicator,
					    -background=>'white')->pack(-expand=>0,
									   -fill=>'x');

my $multiViewUtilityFrame = $multiViewFrame->Frame(-relief => 'ridge', -borderwidth => 2)->pack(-expand => 0,
											       -fill => 'x');

my $colorFrame = $multiViewUtilityFrame->Frame(-relief => 'ridge', -borderwidth => 2)->pack(-side => 'left');

my $colorEntryWidth = 5;
my $colorChangeButton = $colorFrame->Button(-text => "Update colors",
					    -command => sub {
						updateColorValues('update');
						foreach my $arrowRef (values(%HSPArrows)) {
						    setArrowColor($arrowRef);
						}
					    })->pack(-side=>'left');
						
my %colorEntryValues = (); 
for my $color ('blue','green','yellow','red') {
    $colorEntryValues{$color} = "";
    $colorFrame->Entry(-background=>$color,
		       -width=>$colorEntryWidth,
		       -textvariable=>\$colorEntryValues{$color})->pack(-side=>'left');
}

my $zoomFrame = $multiViewUtilityFrame->Frame(-relief => 'ridge', -borderwidth => 2)->pack(-side => 'right',
											   -fill => 'both',
											   -expand => 0);

my $xZoomLabel = $zoomFrame->Label(-text => "H-Zoom:")->pack(-side=>'left');
my $zoomWidth = 7;
my $xZoomValue = 0.0;
my $xZoomScale = $zoomFrame->Scale(-orient => 'horizontal',
				  -from => 0, -to => 10, -resolution => $zoomResolution, 
				  -showvalue => 0,
				  -variable => \$xZoomValue,
				  -width => $zoomWidth,
				  -sliderlength => $zoomWidth,
				  -command => sub {zoom('X')}, 
				  )->pack(-side=>'left',
					  -fill => 'x',
					  -expand => 1);

my $yZoomLabel = $zoomFrame->Label(-text => "V-Zoom:")->pack(-side=>'left');
my $yZoomValue = 0.0;
my $yZoomScale = $zoomFrame->Scale(-orient => 'horizontal',
				  -from => 0, -to => 10, -resolution => $zoomResolution, 
				  -showvalue => 0,
				  -variable => \$yZoomValue,
				  -width => $zoomWidth,
				  -sliderlength => $zoomWidth,
				  -command => sub {zoom('Y')}, 
				  )->pack(-side=>'left',
					  -fill => 'x',
					  -expand => 1);

# A Read-Only-Text box for HSP Info
my $HSPInfoROT = $HSPInfoFrame->Scrolled("ROText", 
					 -width => $canvasWidth/$charWidth, 
					 -height => $textLines, 
					 -background => 'white', 
					 -scrollbars => 'osoe',
					 -wrap => 'none',
					 -relief => 'sunken')->pack(-expand => 1, -fill =>'both');


my $searchWidth = 25;
my $searchFrame = $queryListFrame->Frame()->pack(-side=>'top',-anchor=>'w',-fill=>'x');

my $querySearchString = "";
my $querySearchLabel = $searchFrame->Label(-text => "Find Query:")->pack(-side=>'left');
my $querySearchEntry = $searchFrame->Entry(-width => $searchWidth,
						-background => 'white',
						-textvariable => \$querySearchString)->pack(-side=>'left');

my $subjectSearchString = "";
my $subjectSearchEntry = $searchFrame->Entry(-width => $searchWidth,
						-background => 'white',
						-textvariable => \$subjectSearchString)->pack(-side=>'right');
my $subjectSearchLabel = $searchFrame->Label(-text => "Find Subject:")->pack(-side=>'right');

# A listbox containing query names
my $queryLines = 3;
my $queryListbox = $queryListFrame->Scrolled("Listbox", 
					     -scrollbars => 'osoe',
					     -background => 'white',
					     -borderwidth => 4,
					     -selectmode => 'single',
					     -selectbackground=> 'yellow',
					     -exportselection => 0,
					     -height=>$queryLines,
					     -width => $canvasWidth/$charWidth)->pack(-expand => 1, -fill =>'both');

$queryListbox->insert("end", @queryNames);

# ---- BINDINGS ---- 

#For the top level window
#------------------------
# Control-q key
$main->bind("<Control-Key-q>", sub {shutItDown();});

# Control-r to return to original window size
$main->bind("<Control-Key-r>", sub { $main->geometry(""); });


#For the search entries
#--------------------------
# Enter a keystoke
$querySearchEntry->bind('<KeyPress>',[\&searchQueryList, Ev('K')]);
$subjectSearchEntry->bind('<KeyPress>',[\&searchSubjectInfo, Ev('K')]);

#For the zoom scales
#-------------------
$xZoomScale->bind('<Button-3>',
		 sub {
		     $main->Busy(-recurse => 1);
		     my $self = 0; #$Tk::widget;
		     my ($x,$y) = ($Tk::event->x,$Tk::event->y);
		     my $value = $self->get($x,$y);
		     $self->set($value);
		     zoom('X'); 
		     $main->Unbusy();
		 }
);
$yZoomScale->bind('<Button-3>',
		 sub {
		     $main->Busy(-recurse => 1);
		     my $self = 0; #$Tk::widget;
		     my ($x,$y) = (0,0); #($Tk::event->x,$Tk::event->y);
		     my $value = $self->get($x,$y);
		     $self->set($value);
		     zoom('Y'); 
		     $main->Unbusy();
		 }
);


#For the query listbox
#---------------------
#left click on an entry
$queryListbox->bind('<Button-1>',
		    sub { my @selected = $queryListbox->curselection();
			  changeQuery($selected[0]);
		      }
);

# up arrow key
$queryListbox->bind("<Key-Up>", sub { changeQuery(($currentQuery-1)%$nQueries);});
# down arrow key
$queryListbox->bind("<Key-Down>", sub { changeQuery(($currentQuery+1)%$nQueries);});

#For the multiViewCanvas
#-----------------------

# left arrow
#$multiViewCanvas->CanvasBind("<Key-Left>", sub {$multiViewCanvas->xviewScroll(-5,'units');});
# right arrow
#$multiViewCanvas->CanvasBind("<Key-Right>", sub {$multiViewCanvas->xviewScroll(5,'units');});
# up arrow
#$multiViewCanvas->CanvasBind("<Key-Up>", sub {$multiViewCanvas->yviewScroll(-1,'units');});
# down arrow
#$multiViewCanvas->CanvasBind("<Key-Down>", sub {$multiViewCanvas->yviewScroll(1,'units');});

$multiViewCanvas->bind("Query", "<Enter>",
    sub {
	$mouseOverStatusIndicator = $queryStatusIndicator;
	drawHighlightShape('Query');
    }
);

$multiViewCanvas->bind("HSPArrow", "<Enter>",
    sub {
	my $tags = $multiViewCanvas->itemcget("current", -tags);
	my ($hitID,$index) = $tags->[0] =~ /^ID_(.+)\#(\d+)$/;
	my ($length, $nAligns, $annotation, $startIndex) = 
	    @{$hitInfo{$hitID}}[@hitFieldIndex{'length','nHSPs','annotation','startIndex'}];

	$HSPArrowStatusIndicator = "SUBJECT: $hitID [$annotation]\t$length $subjectUnits\tHSP " . ($index+1) ." of $nAligns";
	$mouseOverStatusIndicator = $HSPArrowStatusIndicator;
	drawHighlightShape($tags->[0]);

    }
);

$multiViewCanvas->CanvasBind('<Button-3>', 
    sub {
	my ($x,$y) = (0,0); #($Tk::event->x,$Tk::event->y);
	mouseZoom('init',$x,$y);
	$main->configure(-cursor => 'sb_h_double_arrow');
    }
);

$multiViewCanvas->CanvasBind('<B3-ButtonRelease>', 
    sub {
	$main->configure(-cursor => 'left_ptr');
    }
);

$multiViewCanvas->CanvasBind('<B3-Motion>', 
    sub {
	my ($x,$y) = (0,0); #($Tk::event->x,$Tk::event->y);
	mouseZoom('zoom',$x,$y);
    }
);

$multiViewCanvas->CanvasBind('<Button-1>', 
    sub {
	my ($x,$y) = (0,0); #($Tk::event->x,$Tk::event->y);
	mouseScan('init',$x,$y);
	$main->configure(-cursor => 'fleur');
    }
);

$multiViewCanvas->CanvasBind('<B1-ButtonRelease>', 
    sub {
	$main->configure(-cursor => 'left_ptr');
    }
);

$multiViewCanvas->CanvasBind('<B1-Motion>', 
    sub {
	my ($x,$y) = (0,0); #($Tk::event->x,$Tk::event->y);
	mouseScan('scan',$x,$y);
    }
);


$multiViewCanvas->bind("HSPArrow", "<Button-1>",
    sub {
	my $tags = $multiViewCanvas->itemcget("current", -tags);
	my ($alignID) = $tags->[0] =~ /^ID_(.+)$/;

	my $alignmentRef = $HSPArrows{$alignID}->[$HSPArrowFieldIndex{'alignmentInfo'}];

	my $text = "";
	foreach my $field (@HSPPrintFields) {
	    my $fieldValue;
	    if ($HSPPrintFields{$field}) {
		$text .= printHSPInfo($field,$alignmentRef);
	    }
	}
	my @lines = split(/\n/,$text);
	my $maxLine = 0;
	foreach my $line (@lines) {
	    my $len = length($line);
	    if ($len > $maxLine) {
		$maxLine = $len;
	    }
	}
	my $pad = "+" x ($maxLine+10) . "\n"; 
	$text = $text . $pad;
	$HSPInfoROT->insert('1.0',$text);

	drawSelectionShape($tags->[0]);

    }
);

my $multiViewCanvasGeometry = $multiViewCanvas->geometry;
$main->bind('<Configure>',
    sub { my $oldGeometry = $multiViewCanvasGeometry;
	  my $newGeometry = $multiViewCanvas->geometry;
	  $multiViewCanvasGeometry = $newGeometry;
	  
	  unless (($oldGeometry ne $newGeometry) &&
		  ($oldGeometry !~ /^1x1\+/)) {
	      return;
	  }

	  my (@oldGeometry) = $oldGeometry =~ /^(\d+)x(\d+)\+/;
	  my (@newGeometry) = $newGeometry =~ /^(\d+)x(\d+)\+/;
	  my (@delta) = ($newGeometry[0]-$oldGeometry[0],$newGeometry[1]-$oldGeometry[1]);

	  my ($width) = $multiViewCanvas->cget(-width);
	  my ($height) = $multiViewCanvas->cget(-height);
	  $canvasWidth = $width+$delta[0];
	  $canvasHeight = $height+$delta[1];

	  $multiViewCanvas->configure(-width=>$canvasWidth);
	  $multiViewCanvas->configure(-height=>$canvasHeight);

	  zoom('X',$canvasWidth/$width);
     }
);

updateColorValues('load'); 
changeQuery(0);
$main->title("blastView3.pl: [$blastVersion] $blastFile"); 
MainLoop;

# ------- END of MAIN ----------- 

# SUBROUTINES 

sub shutItDown {
    close F;
    exit;
}

sub dumpUsage {
    print "Usage: ./blastView3.pl <-b(last) output file>\n";
    print "\t<<-p(rint) hits=[(all)|best],HSPs=[(all)|best],semiglobal=[(0)|1],sequence=[0|(1)]>>\n";
    shutItDown();
}

sub isValidRegexp {
    my ($regexp) = @_;
    return eval { "" =~ /$regexp/; 1 } || 0;
}

sub printHSPInfo {
    my ($infoType,$alignmentRef) = @_;
    my $text = "";
    if ($infoType eq "Alignment") {
	my ($sSeq,$qSeq) = @{$alignmentRef}[@alignmentFieldIndex{'sSeq','qSeq'}];
	my $matchString = "";
	my @qAA = split(/ */,$qSeq);
	my @sAA = split(/ */,$sSeq);
	while (@qAA) {
	    my $q = shift(@qAA);
	    my $s = shift(@sAA);
	    if ($q eq $s) {
		$matchString .= "|";
	    } elsif ( ($blastType eq "Protein") && 
		      (exists($BLOSUM62{$q . $s}) && ($BLOSUM62{$q . $s} > 0))) {
		$matchString .= "+";
	    } else {
		$matchString .= " ";
	    }
	}
	my $pad = " " x (1+length($infoType));
	$text = "$infoType:\tQ: $qSeq\n$pad\t   $matchString\n$pad\tS: $sSeq\n";

    } elsif ($infoType eq "Extents") {

	my ($alignID, $qStart, $qStop, $sStart, $sStop) =
	    @{$alignmentRef}[@alignmentFieldIndex{"alignID","qStart","qStop","sStart","sStop"}];

	my ($hitID) = $alignID =~ /(.+)\#\d+$/;
	my $subjectLength = $hitInfo{$hitID}->[$hitFieldIndex{'length'}];

	my $tValueQ = 100*($qStop - $qStart + 1)/($queryLength);
	my $tValueS = 100*($sStop - $sStart + 1)/($subjectLength);

	$text = sprintf("$infoType:\tquery: $qStart-$qStop (%.2f%%)\tsubject: $sStart-$sStop (%.2f%%)\n",$tValueQ,$tValueS);

    } elsif ($infoType eq "Identity") {

	my ($alignID, $strand) = @{$alignmentRef}[@alignmentFieldIndex{'alignID','strand'}];
	my ($hitID,$index) = $alignID =~ /(.+)\#(\d+)$/;
	my ($length,$nAligns,$annotation) = @{$hitInfo{$hitID}}[@hitFieldIndex{'length','nHSPs','annotation'}];

	$text = "$infoType:\t$hitID [$annotation]\t$strand strand\t$length $subjectUnits\tHSP " . ($index+1) ." of $nAligns\n";

    } elsif ($infoType eq "Accuracy") {
	my ($score,$eValue,$matchesover,$identities) =
	    @{$alignmentRef}[@alignmentFieldIndex{'score','eValue','matchesover','identities'}];

	my $pID = "NA";
	if ($matchesover > 0) {
	    $pID = 100*$identities/$matchesover;
	}	

	$text = sprintf("$infoType:\tscore: $score\teValue: $eValue\tmatches: $identities/$matchesover (%.2f%%)\n",$pID);
    }

    return $text;
}

BEGIN {
    my $oldXScale = 1.0;
    my $oldYScale = 1.0;

    sub zoom {
	
	my ($mode,$newScale) = @_;

	if ($mode eq "init") {
	    $oldXScale = 1.0;
	    $xScale = 1.0;
	    $xZoomValue = 0.0;
	    $maxXScale = sprintf("%.1f",$queryLength/$canvasWidth);
	    if ($maxXScale == 0) {
		$maxXScale = 1.0;
	    }
	    $xZoomScale->configure(-to => log($maxXScale));
	    
	    $oldYScale = 1.0;
	    $yScale = 1.0;
	    $yZoomValue = 0.0;
	    $maxYScale = 10.0;
	    $yZoomScale->configure(-to => log($maxYScale));

	} elsif ($mode eq "X") {

	    my $scaleFactor;
	    
	    if (defined($newScale)) {

		$scaleFactor = $newScale;

	    } else {

		if (exp($xZoomValue) > $maxXScale) {
		    $xZoomValue = log($maxXScale);
		} elsif (exp($xZoomValue) < $minXScale) {
		    $xZoomValue = log($minXScale);
		}
		$xScale = exp($xZoomValue);
		$scaleFactor = $xScale/$oldXScale;
		$oldXScale = $xScale;
	    }	    

	    $multiViewCanvas->delete('tick','ticklabel');
	    my $middle = $multiViewCanvas->canvasx($canvasWidth/2);
	    $multiViewCanvas->scale('all', $middle, 0, $scaleFactor, 1.0);
	    drawTicks();
	    drawSubjectSearchShape();
	    drawSelectionShape();
	    drawHighlightShape();
	    $multiViewCanvas->configure(-scrollregion => [ $multiViewCanvas->bbox('all') ]); 

	} elsif ($mode eq "Y") {

	    my $scaleFactor;
	    
	    if (defined($newScale)) {

		$scaleFactor = $newScale;

	    } else {

		if (exp($yZoomValue) > $maxYScale) {
		    $yZoomValue = log($maxYScale);
		} elsif (exp($yZoomValue) < $minYScale) {
		    $yZoomValue = log($minYScale);
		}
		$yScale = exp($yZoomValue);
		$scaleFactor = $yScale/$oldYScale;
		$oldYScale = $yScale;
	    }	    

	    $multiViewCanvas->scale('HSPArrow', 0, 5*$spacer, 1.0, 1.0/$scaleFactor);
	    $multiViewCanvas->scale('HSPConnector', 0, 5*$spacer, 1.0, 1.0/$scaleFactor);
	    drawSubjectSearchShape();
	    drawSelectionShape();
	    drawHighlightShape();
	    $multiViewCanvas->configure(-scrollregion => [ $multiViewCanvas->bbox('all') ]); 

	} else {
	    warn "zoom: wtf?!\n";
	}
    }
}
    
BEGIN {
    my $oldXZoomValue;
    my $oldXCoord;
    my $oldYZoomValue;
    my $oldYCoord;
    my $sensitivity = 200;

    sub mouseZoom {
	my ($mode,$xCoord,$yCoord) = @_;
	if ($mode eq 'init') {
	    $oldXCoord = $xCoord;
	    $oldXZoomValue = $xZoomValue;
	    $oldYCoord = $yCoord;
	    $oldYZoomValue = $yZoomValue;
	}
	my $deltaX = ($xCoord-$oldXCoord)/$sensitivity;
	$xZoomValue = $oldXZoomValue + $deltaX;
	zoom('X');
	my $deltaY = ($yCoord-$oldYCoord)/$sensitivity;
	$yZoomValue = $oldYZoomValue + $deltaY;
	zoom('Y');
    }
}

BEGIN {
    my ($oldCoordX,$oldCoordY);
    my $sensitivity = 2;

    sub mouseScan {
	my ($mode,$coordX,$coordY) = @_;
	if ($mode eq 'init') {
	    $oldCoordX = $coordX;
	    $oldCoordY = $coordY;
	}
	my $deltaX = ($coordX-$oldCoordX)/$sensitivity;
	my $deltaY = ($coordY-$oldCoordY)/$sensitivity;
	$multiViewCanvas->xviewScroll(-$deltaX,'units');
	$multiViewCanvas->yviewScroll(-$deltaY,'units');
	$oldCoordX = $coordX;
	$oldCoordY = $coordY;
    }
}

sub updateColorValues {
    my ($mode) = @_;

    if ($mode eq "load") {
	if (($colorScheme eq "%ID") || ($colorScheme eq "%Aligned")) {
	    @colorEntryValues{'blue','green','yellow','red'} = @{$colorSettings{$colorScheme}};
	    foreach my $child ($colorFrame->children) {
		$child->configure(-state=>'normal');
	    }
	} else {
	    @colorEntryValues{'blue','green','yellow','red'} = ("") x 4;
	    foreach my $child ($colorFrame->children) {
		$child->configure(-state=>'disabled');
	    }
	}
    } elsif ($mode eq "update") {
	if (validateColorValues()) {
	    @{$colorSettings{$colorScheme}} = @colorEntryValues{'blue','green','yellow','red'};
	} else {
	    $main->bell;
	    warn "Invalid color parameters: @colorEntryValues{'blue','green','yellow','red'}\n";
	}
    }
}

sub validateColorValues {
    my ($blue,$green,$yellow,$red) = @colorEntryValues{"blue","green","yellow","red"};

    if (!(isNumeric($blue)) ||  ($blue < 0) || ($blue > 1)) {
	return 0;
    }
    if (!(isNumeric($green)) ||  ($green < 0) || ($green > 1)) {
	return 0;
    }
    if (!(isNumeric($yellow)) ||  ($yellow < 0) || ($yellow > 1)) {
	return 0;
    }
    if (!(isNumeric($red)) ||  ($red < 0) || ($red > 1)) {
	return 0;
    }
    
    if (($yellow >= $red) || ($green >= $yellow) || ($blue >= $green))  {
	return 0;
    }

    return 1;

}

sub isNumeric {
    my ($val) = @_;
    if ($val =~ /^([+-]?)(?=\d|\.\d)\d*(\.\d*)?([Ee]([+-]?\d+))?$/) {
	return 1;
    } else {
	return 0;
    }
}

sub searchQueryList {
    my ($caller, $keysym) = @_;
    
    unless ($querySearchString) {
	$caller->configure(-background=>'white');
	$queryListbox->see($currentQuery);
	return;
    }

    unless (isValidRegexp($querySearchString)) {
	$caller->configure(-background=>'yellow');
	return;
    }

    my $foundMatch = 0;
    my $q = ($currentQuery+1)%$nQueries;
    my $nTested = 0;
    while (($foundMatch==0) && ($nTested < $nQueries)) {
	$nTested++;
	if ($queryNames[$q] =~ /$querySearchString/) {
	    $foundMatch = 1;
	} else { 
	    $q = ($q+1)%$nQueries;
	}
    }

    if ($foundMatch == 1) {
	$caller->configure(-background=>'green');
	$queryListbox->see($q);
    } else {
	$caller->configure(-background=>'red');
	$queryListbox->see($currentQuery);
    }

    if (($keysym eq "Return") && ($foundMatch==1)) {
	changeQuery($q);
    }
}

sub searchSubjectInfo {
    my ($caller, $keysym) = @_;
    
    unless ($subjectSearchString) {
	$caller->configure(-background=>'white');
	return;
    }

    unless (isValidRegexp($subjectSearchString)) {
	$caller->configure(-background=>'yellow');
	return;
    }

    my @matches = ();

    while (my ($ID,$info) = each(%hitInfo)) {
	my $annotation = ${$info}[$hitFieldIndex{'annotation'}];
	
	if ( ($ID =~ /$subjectSearchString/i) || ($annotation =~ /$subjectSearchString/i)) {
	    push(@matches,$ID);
	}
    }

    if (scalar(@matches) > 0) {
	$caller->configure(-background=>'green');
    } else {
	$caller->configure(-background=>'red');
    }

    if (($keysym eq "Return") && (scalar(@matches)>0)) {
	drawSubjectSearchShape(\@matches);
    }
}

sub changeQuery {

    my ($newQueryID) = @_;

    if ($newQueryID == $currentQuery) {
	return;
    }

    $main->Busy(-recurse => 1);

    $HSPInfoROT->delete("1.0",'end');
    $queryListbox->selectionClear(0,'end');
    $queryListbox->selectionSet($newQueryID);
    $queryListbox->activate($newQueryID);
    $queryListbox->see($newQueryID);
    seek(F,$jumpPoints[$newQueryID],0);
    parseBlast($fileFormat);

    $currentQuery=$newQueryID;
    $queryStatusIndicator = "QUERY: $queryName\t$queryLength $queryUnits\t$nHits aligned sequences\t$nAlignments HSPs";
    $mouseOverStatusIndicator = $queryStatusIndicator;

    zoom('init');
    redraw();
    drawHighlightShape('Query');

    $main->Unbusy();

}

BEGIN {
    my %timers = ();

    sub timeReport {
	my ($cmd,$timerID) = @_;

	if ($cmd eq "reset") {
	    $timers{$timerID} = new Benchmark;
	} elsif ($cmd eq "report") {
	    my $newTimer = new Benchmark;
	    if (exists($timers{$timerID})) {
		my $diff = timediff($newTimer,$timers{$timerID});
		$timers{$timerID} = $newTimer;
		print "$timerID took ", timestr($diff, 'all'), " seconds\n";
	    } else {
		warn "Warning: (timeReport) Timer must be reset before report\n";
	    }
	}
    }
}

sub resetBins {
    $hbins = sprintf("%d",$canvasWidth/$binResolution);
    $vbins = sprintf("%d",$canvasHeight/$binResolution);

    @binFilled = ();
    for (my $i = 0; $i < $hbins*$vbins; $i++) {
	$binFilled[$i] = 0;
    }
}

sub redraw {
    $multiViewCanvas->delete('all');
    %HSPArrows = ();
    resetBins();
    drawQuery();
    drawTicks();
    drawSubjects();
    drawConnectors();
}

# draw the query arrow
sub drawQuery {
    my $queryArrow = $multiViewCanvas->create ('line', 
					       $spacer, 2*$spacer, $canvasWidth-$spacer, 2*$spacer, 
					       -fill => 'red', 
					       -width => $arrowWidth,
					       -tags => 'Query');
}

BEGIN {
    my $selected;
    
    sub drawSelectionShape {
	my ($newSelection) = @_;
	if ($newSelection) {
	    $selected = $newSelection;
	}

	if ($newSelection || ($multiViewCanvas->type('selectionShape'))) {

	    my @HSPBBox = $multiViewCanvas->bbox($selected);
	    $multiViewCanvas->delete('selectionShape');
	    $multiViewCanvas->createRectangle(@HSPBBox,
					      -outline=>'green',
					      -width => 1,
					      -tags => 'selectionShape');
	}
    }
}

BEGIN {
    my @matches = ();
    
    sub drawSubjectSearchShape {
	my ($newMatchRef) = @_;
	my @newMatches = ();
	if ($newMatchRef) {
	    @newMatches = @{$newMatchRef};
	    @matches = @newMatches;
	}

	if (scalar(@newMatches) || ($multiViewCanvas->type('subjectSearchShape'))) {
	    
	    $multiViewCanvas->delete('subjectSearchShape');
	    foreach my $ID (@matches) {
		my @HSPTags = ();
		my ($nHSPs) = @{$hitInfo{$ID}}[$hitFieldIndex{'nHSPs'}];
		for (my $i = 0;$i<$nHSPs;$i++) {
		    push(@HSPTags,"ID_" . $ID . "#" . $i);
		}
		my @HSPBBox = $multiViewCanvas->bbox(@HSPTags);
		$multiViewCanvas->createRectangle(@HSPBBox,
						  -outline=>'gold',
						  -width => 1,
						  -tags => 'subjectSearchShape');
	    }
	}
    }
}

BEGIN {
    my $highlighted;

    sub drawHighlightShape {
	my ($newHighlighted) = @_;
	if ($newHighlighted) {
	    $highlighted = $newHighlighted;
	}

	if ($newHighlighted || ($multiViewCanvas->type('highlightShape'))) {

	    if ($highlighted eq "Query") {
		my @HSPBBox = $multiViewCanvas->bbox('Query','tick','ticklabel');
		$multiViewCanvas->delete('highlightShape');
		$multiViewCanvas->createRectangle(@HSPBBox,
						  -outline=>'white',
						  -tags => 'highlightShape');
	    } else {

		my @HSPBBox = $multiViewCanvas->bbox($highlighted);
		$multiViewCanvas->delete('highlightShape');
		$multiViewCanvas->createOval(@HSPBBox,
					     -outline=>'white',
					     -tags => 'highlightShape');
	    }
	}
    }
}


#draw the tick marks 
sub drawTicks {
    my $basesOnScreen = $queryLength/$xScale;
    my $baseStep = (10**sprintf("%.0f", log($basesOnScreen)/log(10)))/$maxTicks;
    my $nTicks = sprintf("%d",$queryLength/$baseStep);
    
    while ($nTicks > $maxTicks*$xScale) {
	$baseStep *= 2;
	$nTicks = sprintf("%d",$queryLength/$baseStep);
    }
    
    my $screenStep = $xScale*($canvasWidth-2*$spacer)*($baseStep/$queryLength);

    my $nLittleSteps = 8;
    my $littleScreenStep = $screenStep/$nLittleSteps;
    my $littleBaseStep = $baseStep/$nLittleSteps;
    my @queryCoords = $multiViewCanvas->coords('Query'); 
    my $queryStart = $queryCoords[0];
    my $queryDepth = $queryCoords[1];

    for (my $i = 0; $i <= $nTicks; $i++) { 
	if ($i*$baseStep <= $queryLength) {

	    my $place = $queryStart + $i*$screenStep;
	    my $text = $i*$baseStep;

	    my $tickLine = $multiViewCanvas->create ('line', 
#						     $place, $queryDepth-0.5*$spacer*$yScale, 
#						     $place, $queryDepth+0.5*$spacer*$yScale, 
						     $place, $queryDepth-0.5*$spacer, 
						     $place, $queryDepth+0.5*$spacer, 
						     -fill => 'white',
						     -tags => 'tick'
						     );

	    for (my $j = 1; $j<$nLittleSteps; $j++) {
		if ($i*$baseStep+$j*$littleBaseStep <= $queryLength) {
		    my $tickLine = $multiViewCanvas->create ('line', 
							     $place + $j*$littleScreenStep, 
#							     $queryDepth-0.25*$spacer*$yScale, 
							     $queryDepth-0.25*$spacer, 
							     $place + $j*$littleScreenStep, 
#							     $queryDepth+0.25*$spacer*$yScale, 
							     $queryDepth+0.25*$spacer, 
							     -fill => 'white',
							     -tags => 'tick'
							     );
		}
	    }
	    
	    my $tickLabel = $multiViewCanvas->createText( $place, 
#							  $queryDepth+$spacer*$yScale, 
							  $queryDepth+$spacer, 
							  -text => $text, 
							  -fill => 'white',
							  -tags => 'ticklabel'
							  ); 
	}
    }
}

#draw the subject arrows 

sub drawSubjects { 
    my @sortedHits = sort byScore keys(%hitInfo);

    foreach my $hitID (@sortedHits) {
	my ($nAligns, $annotation, $startIndex, $hitFirst, $hitLast) = 
	    @{$hitInfo{$hitID}}[@hitFieldIndex{'nHSPs','annotation','startIndex','minStart','maxStop'}];

	my $depth = findDepth($hitFirst, $hitLast);

	for (my $i = $startIndex; $i < $startIndex+$nAligns; $i++) {

	    my ($alignID, $strand, $qStart, $qStop, $sStart, $sStop) = 
		@{$alignments[$i]}[@alignmentFieldIndex{'alignID','strand','qStart','qStop','sStart','sStop'}]; 
	    
	    my $dir;
	    if ($strand eq "Minus") {$dir = 'first';}
	    elsif ($strand eq "Plus") {$dir = 'last';}
	    else {$dir = 'none';}

	    my $leftPixel = ($canvasWidth-2*$spacer)*($qStart)/($queryLength);
	    $leftPixel += $spacer;
	    my $rightPixel = ($canvasWidth-2*$spacer)*($qStop)/($queryLength);
	    $rightPixel += $spacer;
	
#	    if ($leftPixel == $rightPixel) {$rightPixel++;}
	    
	    my $subjectArrow = $multiViewCanvas->create ('line', 
							 $leftPixel, $depth*$spacer, 
							 $rightPixel, $depth*$spacer, 
							 -width => $arrowWidth,
							 -arrow => $dir, -arrowshape => [ 3, 4, 2],
							 -tags => ["ID_" . $alignID , "HSPArrow"]);

	    $HSPArrows{$alignID} = [];


	    @{$HSPArrows{$alignID}}[@HSPArrowFieldIndex{'arrowID','depth','alignmentInfo'}] =
		($subjectArrow,$depth,$alignments[$i]);

	    setArrowColor($HSPArrows{$alignID});
	

	}
    }
    $multiViewCanvas->configure(-scrollregion => [ $multiViewCanvas->bbox("all") ]);

}

sub drawConnectors {

    while (my ($hitID, $hitInfo) = each(%hitInfo)) {
	
	my ($length, $nAligns, $annotation, $startIndex) =
	    @{$hitInfo}[@hitFieldIndex{'length','nHSPs','annotation','startIndex'}];

	if ($nAligns > 1) {

	    my @HSPs = sort { $a->[$alignmentFieldIndex{'qStart'}] <=> $b->[$alignmentFieldIndex{'qStart'}] } (@alignments[$startIndex .. ($startIndex+$nAligns-1)]);

	    my @connector = (0,0,0,0);

	    my $firstAlignID = $HSPs[0]->[$alignmentFieldIndex{'alignID'}];
	    my $lastAlignID = $HSPs[$nAligns-1]->[$alignmentFieldIndex{'alignID'}];

	    my @arrowList = $multiViewCanvas->find("withtag", "ID_$firstAlignID");
	    @hitCoords = $multiViewCanvas->coords($arrowList[0]);
	    
#	    if (!scalar(@hitCoords)) {
#		print "[@arrowList] $firstAlignID $lastAlignID\n";
#	    }

	    $connector[0] = ($hitCoords[0]+$hitCoords[2])/2; 
	    $connector[1] = ($hitCoords[1]+$hitCoords[3])/2; 
	    @arrowList = $multiViewCanvas->find("withtag", "ID_$lastAlignID");
	    @hitCoords = $multiViewCanvas->coords($arrowList[0]);
	    $connector[2] = ($hitCoords[0]+$hitCoords[2])/2; 
	    $connector[3] = ($hitCoords[1]+$hitCoords[3])/2; 
	    my $connectorLine = $multiViewCanvas->create ('line', 
							  @connector,
							  -fill => 'SlateGray',
							  -tags => 'HSPConnector',
							  -width => 1
							  );
	    $multiViewCanvas->lower($connectorLine);
	}
    }
}

sub parseBlast {
    my ($format) = @_;
    
    @alignments = ();
    %hitInfo = ();
    %cloneIDs = ();
    $queryLength = undef; 
    $queryName = undef;
    $nHits = 0;
    $nAlignments = 0;

    if ($format eq "ncbi") {
	parseNCBI();
    } elsif ($format eq "bv3") {
	parseBV3();
    }
}

sub setUnits {
    if ($blastVersion eq "BLASTN") {
	$blastType = "Nucleotide";
	$queryUnits = "bases";
	$subjectUnits = "bases";
    } elsif ($blastVersion eq "BLASTX") {
	$blastType = "Protein";
	$queryUnits = "bases";
	$subjectUnits = "amino acids";
    } elsif ($blastVersion eq "BLASTP") {
	$blastType = "Protein";
	$queryUnits = "amino acids";
	$subjectUnits = "amino acids";
    } elsif ($blastVersion eq "TBLASTN") {
	$blastType = "Protein";
	$queryUnits = "amino acids";
	$subjectUnits = "bases"; 
    } elsif ($blastVersion eq "TBLASTX") {
	$blastType = "Protein";
	$queryUnits = "bases";
	$subjectUnits = "bases"; 
    } else {
	$blastType = "Nucleotide";
	$queryUnits = "bases";
	$subjectUnits = "bases";
	warn "Warning: Unknown blast type: $blastVersion\n";
    }
}

sub parseBV3 {

    my $line = <F>;
    chomp $line;

    my @cols = split(/\t/,$line);
    ($blastVersion,$queryName,$queryLength) = @cols[0,1,4]; 

    setUnits();

    my $currentSubject = undef;
    my $HSPIndex = 0;
    my $minQStart = undef;
    my $maxQStop = undef;
    my $maxQFootprint = undef;
    my $hitID = undef;
    my $annotation = undef;
    while ($cols[1] eq $queryName) {
	my ($qStart,$qStop,$subjectName,$sStart,$sStop,$sLength,
	    $strand,$score,$eValue,$identities,$alignLength,$qSeq,$sSeq) = @cols[2,3,5..15];

	if (!defined($currentSubject) || ($currentSubject ne $subjectName)) {
	    
	    if (defined($currentSubject)) {
		@{$hitInfo{$hitID}}[@hitFieldIndex{'nHSPs','minStart','maxStop','maxLength'}] =
						   ($HSPIndex,$minQStart,$maxQStop,$maxQFootprint);
	    }

	    ($hitID) = $subjectName =~ /^(\S+)/;
	    if ($subjectName =~ /\S+\s+.+/) {
		($annotation) = $subjectName =~ /\S+(\s+.+)/;
	    } else {
		$annotation = "";
	    }
	    $nHits++;
	    $hitInfo{$hitID} = [];
	    @{$hitInfo{$hitID}}[@hitFieldIndex{'length','nHSPs','annotation','startIndex',
					       'minStart','maxStop','maxLength'}] =
						   ($sLength,undef,$annotation,$nAlignments,undef,undef,undef);

	    $currentSubject = $subjectName;
	    $HSPIndex = 0;
	    $minQStart = undef;
	    $maxQStop = undef;
	    $maxQFootprint = undef;
	} 
	
	$alignments[$nAlignments] = [];
	@{$alignments[$nAlignments]}[@alignmentFieldIndex{'alignID','score','eValue','strand',
							    'matchesover','identities',
							    'qStart','qStop','sStart','sStop',
							    'qSeq','sSeq'}] = 
								($hitID . "#" . $HSPIndex,
								 $score,$eValue,$strand,
								 $alignLength,$identities,
								 $qStart,$qStop,
								 $sStart,$sStop,
								 $qSeq,$sSeq);

	if ((!defined($minQStart)) || ($qStart < $minQStart)) {
	    $minQStart = $qStart;
	}
	if ((!defined($maxQStop)) || ($qStop > $maxQStop)) {
	    $maxQStop = $qStop;
	}
	if ((!defined($maxQFootprint)) || (($qStop-$qStart+1) > $maxQFootprint)) {
	    $maxQFootprint = $qStop-$qStart+1;
	}
	$nAlignments++;
	$HSPIndex++;

	if (eof(F)) {
	    last;
	} 
	$line = <F>;
	chomp $line;
	@cols = split(/\t/,$line);
    }

    @{$hitInfo{$hitID}}[@hitFieldIndex{'nHSPs','minStart','maxStop','maxLength'}] =
	($HSPIndex,$minQStart,$maxQStop,$maxQFootprint);

}

sub parseNCBI {

    my $line = <F>;

    #Determine the type of blast

    if ($line =~ /^T?BLAST[PXN]/) {
	($blastVersion) = $line =~ /(^\S*)/;
	setUnits();
    } else {
	die "Error: parseNCBI could not determine blast version.\n";
    }
    $line = <F>;

# if we ever get a line with '^T?BLAST[PXN]' we're done
    while (!($line =~ /^T?BLAST[PXN]/) && !(eof(F))) {

	# Get the query name

	if ($line =~ /^Query=/) {
	    ($queryName) = $line =~ /^Query=\s+(.+)/;

	    # Get the query length

	} elsif (($line =~ /\([\d\,]+\s+letters\)/) && (!defined($queryLength))) {
	    ($queryLength) = $line =~ /([\d\,]+)/;
	    $queryLength =~ s/,//g;

	    # Find the start of the sequence list

	} elsif ($line =~ /^Sequences\s+producing/) {

	    my $nextLine = <F>;

	    #keep going until the start of the first alignment
	    
	    while (!($nextLine =~ /^\>/)) {
		chomp $nextLine;

		#if the line wasn't blank
		
		if ($nextLine) {
		    my ($hitID, $annotation, $score, $eValue) = 
			$nextLine =~ /(\S+)\s+(.+)\s+([\d\+\-\.e]+)\s+([\d\+\-\.e]+)/;
		    #$hitID =~ s/[^a-zA-Z0-9_\+\-\.\s\|\:]/_/g;
		    #$hitID =~ s/\|{2,}/\|/g;
		    $annotation =~ s/^\s+//;
		    $annotation =~ s/\s+$//;
		    $annotation =~ s/\s+/ /g;

		    if (exists($hitInfo{$hitID})) {
			warn "Warning: $hitID listed multiply in aligned sequences.\n";
		    } else {
			$hitInfo{$hitID} = [];
			@{$hitInfo{$hitID}}[@hitFieldIndex{'length','nHSPs','annotation','startIndex',
							   'minStart','maxStop','maxLength'}] =
							       (undef,undef,$annotation,undef,undef,undef,undef);

		    } 

#		    print "[$hitID] [$annotation] [$score] [$eValue]\n";

		    my $cloneID = $hitID;
		    if ($hitID =~ /(\S+)\./) {
			($cloneID) = $hitID =~ /(\S+)\./;
		    } else {
			($cloneID) = $hitID =~ /(\S+)/;
		    }

		    if (exists($cloneIDs{$cloneID})) {
			push( @{$cloneIDs{$cloneID}}, $hitID);
		    } else {
			$cloneIDs{$cloneID} = [$hitID];
		    }

		}
		$nextLine = <F>;
	    }
	    $line=$nextLine;
	}
	
	chomp $line;
    
	# if we are at a new subject hit
	if ($line =~ /^\>/) {
	    $nHits++;
	    my $nSpans = 0;
	    my $minQuery = undef;
	    my $maxQuery = undef;
	    my $minSubj = undef;
	    my $maxSubj = undef;
	    my $querySequence = "";
	    my $subjectSequence = "";
	    my $minStart = undef;
	    my $maxStop = undef;
	    my $maxLength = undef;
	    my ($hitID) = $line =~ /^\>(\S+)/;
	    #$hitID =~ s/[^a-zA-Z0-9_\+\-\.\s\|\:]/_/g;
	    #$hitID =~ s/\|{2,}/\|/g;

	    if (!exists($hitInfo{$hitID})) {
		warn "Warning: $hitID not listed in aligned sequences. Ignoring all occurrences\n";
	    } else {
		$hitInfo{$hitID}->[$hitFieldIndex{'startIndex'}] = $nAlignments;
		# get the length of the subject

		$line = <F>;
		chomp $line;
		while (!($line =~ /Length\s+=\s+\d+/)) {
		    $line = <F>;
		    chomp $line;
		}

		my ($hitLength) = $line =~ /(\d+)/;
		$hitInfo{$hitID}->[$hitFieldIndex{'length'}] = $hitLength;

		$line = <F>;
		chomp $line;
		$line =~ s/^\s+$//g;

		my ($score, $expect, $strand, $matchesover, $identities) = (0, 0, 'Unspecified', 0, 0);

		#get all the info for all alignments

		while (!($line =~ /^\>/) && !($line =~ /^T?BLAST[PXN]/)) {
		    if (eof(F)) {
#			warn "Warning: Unexpected end of file!\n";
			last;
		    }		

		    #get the blast scores
		    
		    if ($line =~ /Score\s+=\s+/) {
			if ($nSpans > 0) {

			    if ((!defined($minStart)) || ($minQuery < $minStart)) {
				$minStart = $minQuery;
			    }
			    if ((!defined($maxStop)) || ($maxQuery > $maxStop)) {
				$maxStop = $maxQuery;
			    }
			    if ((!defined($maxLength)) || (($maxQuery-$minQuery+1) > $maxLength)) {
				$maxLength = $maxQuery-$minQuery+1;
			    }

			    $alignments[$nAlignments-1] = [];
			    @{$alignments[$nAlignments-1]}[@alignmentFieldIndex{'alignID','score','eValue','strand',
										'matchesover','identities',
										'qStart','qStop','sStart','sStop',
										'qSeq','sSeq'}] = 
										    ($hitID . "#" . ($nSpans-1),
										     $score,$expect,$strand,
										     $matchesover,$identities,
										     $minQuery,$maxQuery,
										     $minSubj,$maxSubj,
										     $querySequence,$subjectSequence);
			    
			}

			$nSpans++;
			$nAlignments++;
			$minQuery = undef;
			$maxQuery = undef;
			$minSubj = undef;
			$maxSubj = undef;
			$querySequence = "";
			$subjectSequence = "";

			($score,$expect) = $line =~ /Score.+=\s+([\d\.\+\-e]+)\s+.+Expect.+=\s+([\d\.e\+\-]+)/;
			if ($expect =~ /^e/) {
			    $expect = "1" . $expect;
			}

		    } elsif ($line =~ /Identities\s+=\s+/) {			#get the % identities, etc..
			($identities,$matchesover) = $line =~ /Identities\s+=\s+(\d+)\/(\d+)/;

		    } elsif ($line =~ /Strand\s+=\s+Plus/) {			#get the Strand/Frame
			($strand) = $line =~ /Plus\s+\/\s+(\w+)/;

		    } elsif ($line =~ /Frame\s+=\s+[+-][123]\s*$/) {
			my ($frame) = $line =~ /Frame\s+=\s+(.+\S)/;
			my $sign = substr($frame,0,1);
			$strand = 'Minus';
			if ($sign eq '+') {$strand = 'Plus';}

		    } elsif ($line =~ /Frame\s+=\s+[+-][123]\s+\/\s+[+-][123]\s*$/) {
			my ($frame1,$frame2) = $line =~ /Frame\s+=\s+([+-])[123]\s+\/\s+([+-])[123]/;
			my $sign1 = substr($frame1,0,1);
			my $sign2 = substr($frame2,0,1);
			$strand = 'Minus';
			if ($sign1 eq $sign2) {$strand = 'Plus';}
			
		    } elsif ($line =~ /Query/) {			# get the sequence/starts/stops
			my ($qleft,$queryseq,$qright) = $line =~ /^Query:\s+(\d+)\s+(\S+)\s+(\d+)/;
			if ((!defined($minQuery)) || ($qleft < $minQuery)) {$minQuery = $qleft;}
			if ((!defined($minQuery)) || ($qright < $minQuery)) {$minQuery = $qright;}
			if ((!defined($maxQuery)) || ($qleft > $maxQuery)) {$maxQuery = $qleft;}
			if ((!defined($maxQuery)) || ($qright > $maxQuery)) {$maxQuery = $qright;}
			$querySequence .= $queryseq;

			$line = <F>; $line = <F>; 

			my ($sleft,$subseq,$sright) = $line =~ /^Sbjct:\s*(\d+)\s*(\S+)\s*(\d+)$/;
#			print "[$sleft] [$subseq] [$sright]\n";
#			if (!($line=~ /^Sbjct:\s+(\d+)\s+(\S+)\s+(\d+)/)) {print "$line\n";}
			if ((!defined($minSubj)) || ($sleft < $minSubj)) {$minSubj = $sleft;}			
			if ((!defined($minSubj)) || ($sright < $minSubj)) {$minSubj = $sright;}
			if ((!defined($maxSubj)) || ($sleft > $maxSubj)) {$maxSubj = $sleft;}
			if ((!defined($maxSubj)) || ($sright > $maxSubj)) {$maxSubj = $sright;}
			$subjectSequence .= $subseq;
		    }
		    
		    $line = <F>;
		}

		if ((!defined($minStart)) || ($minQuery < $minStart)) {
		    $minStart = $minQuery;
		}
		if ((!defined($maxStop)) || ($maxQuery > $maxStop)) {
		    $maxStop = $maxQuery;
		}
		if ((!defined($maxLength)) || (($maxQuery-$minQuery+1) > $maxLength)) {
		    $maxLength = $maxQuery-$minQuery+1;
		}


		@{$hitInfo{$hitID}}[@hitFieldIndex{'nHSPs','minStart','maxStop','maxLength'}] = 
		    ($nSpans,$minStart,$maxStop,$maxLength);

		$alignments[$nAlignments-1] = [];
		@{$alignments[$nAlignments-1]}[@alignmentFieldIndex{'alignID','score','eValue','strand',
								    'matchesover','identities',
								    'qStart','qStop','sStart','sStop',
								    'qSeq','sSeq'}] = 
									($hitID . "#" . ($nSpans-1),
									 $score,$expect,$strand,
									 $matchesover,$identities,
									 $minQuery,$maxQuery,
									 $minSubj,$maxSubj,
									 $querySequence,$subjectSequence);
	    }
	} else {
	    $line = <F>;
	}
    }

    # BEGIN BUG FIX: Added Feb 1, 2007 to fix parsing errors when -b != -v (JAC)
    # Clean up hits with no alignment info
    while (my ($hitID, $hitInfo) = each(%hitInfo)) {
	unless(defined($hitInfo{$hitID}->[$hitFieldIndex{'startIndex'}])) {
	    delete $hitInfo{$hitID};
	}
    }
   # END BUG FIX

    my $debug = 0;
    if ($debug == 1) {
	while (my ($hitID, $hitInfo) = each(%hitInfo)) {
	    print "$hitID\n";
	    printInfo($hitInfo,\%hitFieldIndex,0,"keys|values");

	    my ($nHSPs,$startIndex) = 
		@{$hitInfo}[@hitFieldIndex{'nHSPs','startIndex'}];

	    printInfo(0,\%alignmentFieldIndex,0,"keys");
	    for (my $i = 0; $i < $nHSPs; $i++) {
		my $alignment = $alignments[$startIndex+$i];
		printInfo($alignment,\%alignmentFieldIndex,0,"values");
	    }
	}
    }
}

sub byScore {

    return ($alignments[$hitInfo{$b}->[$hitFieldIndex{'startIndex'}]]->[$alignmentFieldIndex{'score'}] <=>
	    $alignments[$hitInfo{$a}->[$hitFieldIndex{'startIndex'}]]->[$alignmentFieldIndex{'score'}]);
		
}

sub findDepth {

    my ($start,$stop) = @_;

    my $leftEnd = sprintf("%d",(($canvasWidth-2*$spacer)/($binResolution))*(($start)/($queryLength)));
    $leftEnd += 1;
    my $rightEnd = sprintf("%d",(($canvasWidth-2*$spacer)/($binResolution))*(($stop)/($queryLength)));
    $rightEnd += 1;
    
    my $j = $leftEnd;
    my $l = 5;
    while ($j < $rightEnd+1)  {
	if ($l >= $vbins) {
	    my $newVbins = $l+1;
	    for (my $k = $vbins*$hbins; $k < $newVbins*$hbins; $k++) {$binFilled[$k] = 0;}
	    $vbins = $newVbins;
	}
	if ($binFilled[$l*$hbins+$j] == 1) {
	    $l += 1;
	    $j = $leftEnd;
	} else {
	    $j += 1;
	}
    }
	    
    my $depth = $l;
    for ($j = $leftEnd; $j <= $rightEnd; $j++) {
	$binFilled[$depth*$hbins + $j] = 1;
    }
	
    return $depth;
}    

sub fileOpenFailure {
    my ($fileName,$outcome) = @_;

    my $answer = $main->messageBox(-title => "$outcome: File open failure",
                                   -message => "Could not open file\n\"$fileName\"",
                                   -type => 'OK',
                                   -icon => $outcome,
                                   );

    if ($outcome eq "error") {
        shutItDown();
    }
}

sub savePostscript {

    my $types = [
                 ['Postscript', '*.ps']
                 ];

    my @bbox = $multiViewCanvas->bbox('all');
    $multiViewCanvas->createRectangle(@bbox,
				      -fill => 'black',
				      -tags => 'backPlane');
    $multiViewCanvas->lower('backPlane');
    my $postscript = $multiViewCanvas->postscript();
    $multiViewCanvas->delete('backPlane');

    my $saveFile = $main->getSaveFile(-filetypes => $types);
    if (defined $saveFile) {
        if (defined(open (SAVE_FILE, ">$saveFile"))) {
            print SAVE_FILE $postscript;
            close SAVE_FILE;
        } else {
            fileOpenFailure($saveFile,"warning");
        }
    }
}

sub saveBV3 {

    my $types = [
                 ['BlastView3', '*.bv3']
                 ];

    my $saveFile = $main->getSaveFile(-filetypes => $types);
    if (defined $saveFile) {
        if (defined(open (SAVE_FILE, ">$saveFile"))) {
	    printBV3Header(*SAVE_FILE);
	    printBlast(\%printOptions,*SAVE_FILE);
            close SAVE_FILE;
        } else {
            fileOpenFailure($saveFile,"warning");
        }
    }
}

sub printBV3Header {
    my ($fileHandle) = @_;

    print $fileHandle "BLAST_TYPE\tQUERY\tQ_START\tQ_STOP\tQ_LENGTH\tSUBJECT\tS_START\tS_STOP\tS_LENGTH\t";
    print $fileHandle "STRAND\tSCORE\tE_VALUE\tIDENTITIES\tALIGN_LENGTH";

    if ($printOptions{"sequence"} == 1) {
	print $fileHandle "\tQ_SEQUENCE\tS_SEQUENCE";
    }
    print $fileHandle "\n";
}

#Print alignment information
sub printBlast { 

    my ($optionsHashRef, $fileHandle) = @_;
    my %options = %{$optionsHashRef};

    my @sortedHits = sort byScore keys(%hitInfo);

    my $printedSomething = 0;

    foreach my $hitID (@sortedHits) {

	if (($options{"hits"} eq "best") && $printedSomething) {
	    last;
	}

	my ($length, $nAligns, $annotation, $startIndex, $hitFirst, $hitLast) = 
	    @{$hitInfo{$hitID}}[@hitFieldIndex{'length','nHSPs','annotation','startIndex','minStart','maxStop'}];

	for (my $i = $startIndex; $i < $startIndex+$nAligns; $i++) {

	    my ($alignID, $score, $evalue, $strand, $matchesover, 
		$identities, $qStart, $qStop, $sStart, $sStop, $qSeq, $sSeq) = 
		    @{$alignments[$i]}[@alignmentFieldIndex{'alignID','score','eValue','strand',
							    'matchesover','identities','qStart','qStop',
							    'sStart','sStop','qSeq','sSeq'}]; 

	    my $acceptable = 1;
	    if ($options{"semiglobal"} == 1) {
		$acceptable = isSemiGlobal($qStart,$qStop,$sStart,$sStop,$queryLength,$length,$strand);
	    }

            if ($minSeed) {                                   
                my $matchString = "";                         
                my @q = split(/ */,$qSeq);                    
                my @s = split(/ */,$sSeq);                    
                for (my $j = 0; $j < $matchesover; $j++) {    
                    if ($q[$j] eq $s[$j]) {                   
                        $matchString .= "|";                  
                    } else {                                  
                        $matchString .= " ";                  
                    }                                         
                }                                             
                my @matches = split(/\s+/,$matchString);      
                my @matchLens = map {length($_)} @matches;    
                my @sortedLens = sort {$b <=> $a} @matchLens; 
                my $maxMatch = $sortedLens[0];                
                unless ($maxMatch >= $minSeed) {              
                    $acceptable = 0;                          
                }                                             
            }                                     

	    if ($acceptable) {
		if (($annotation) && ($annotation !~ /^\s+/)) {
		    $annotation = " " . $annotation;
		}
		    
		print $fileHandle "$blastVersion\t$queryName\t$qStart\t$qStop\t$queryLength\t$hitID$annotation\t$sStart\t$sStop\t$length\t";
		print $fileHandle "$strand\t$score\t$evalue\t$identities\t$matchesover";

		if ($options{"sequence"} == 1) {
		    print $fileHandle "\t$qSeq\t$sSeq";
		}
		print $fileHandle "\n";

		$printedSomething = 1;

		if ($options{"HSPs"} eq "best") {
		    last;
		}
	    }
	}
    }
}

sub isSemiGlobal {
    my ($qStart,$qStop,$sStart,$sStop,$qLen,$sLen,$strand) = @_;

    my $wiggleRoom = 5;
    my $semiGlobal = 0;

    if ( (($qStart < $wiggleRoom) && ($qStop > $qLen-$wiggleRoom)) ||
	 (($sStart < $wiggleRoom) && ($sStop > $sLen-$wiggleRoom)) ) {
	$semiGlobal = 1;
    } else {
	if ($strand eq "Plus") {
	    if ( (($sStart < $wiggleRoom) && ($qStop > $qLen-$wiggleRoom)) ||
		 (($qStart < $wiggleRoom) && ($sStop > $sLen-$wiggleRoom)) ) {
		$semiGlobal = 1;
	    }
	} elsif ($strand eq "Minus") {
	    if ( (($qStart < $wiggleRoom) && ($sStart < $wiggleRoom)) ||
		 (($qStop > $qLen-$wiggleRoom) && ($sStop > $sLen-$wiggleRoom)) ) {
		$semiGlobal = 1;
	    }
	}
    }

    return $semiGlobal;
}

sub printInfo {
    my ($infoRef,$fieldHashRef,$printFieldsRef,$printCommand) = @_;

    my %fieldIndex = %{$fieldHashRef};

    my @keys = ();

    if ($printFieldsRef) {
	@keys = @{$printFieldsRef};
    } else {
	@keys = keys(%fieldIndex);
    }
    my $nKeys = scalar(@keys);
    my @indices = @fieldIndex{@keys};

    my @values = ();
    if ($infoRef) {
	@values = @{$infoRef}[@indices];
    }

    if ($printCommand =~ /keys/) {
	print @keys,"\n";
    }

    if ($printCommand =~ /values/) {
	print @values,"\n";
    }

}

sub setArrowColor {
    my ($arrowRef) = @_;

    my ($arrowID,$alignmentInfo) = @{$arrowRef}[@HSPArrowFieldIndex{'arrowID','alignmentInfo'}];

    my ($score, $strand, $matchesover, $identities, $qStart, $qStop, $sStart, $sStop, $alignID) = 
	@{$alignmentInfo}[@alignmentFieldIndex{'score','strand','matchesover','identities',
					       'qStart', 'qStop', 'sStart','sStop','alignID'}];
	    
    my $color = 'red';

    if ($colorScheme eq "Orientation") {
	if ($strand eq "Minus") {$color = 'gold';}
	elsif ($strand eq "Plus") {$color = 'blue';}
	else {$color = 'gray';}

    } elsif ($colorScheme eq "IsSemiglobal") {
	my ($hitID) = $alignID =~ /(.+)\#\d+$/;
	my $subjectLength = $hitInfo{$hitID}->[$hitFieldIndex{'length'}];
	my $isSemiglobal = isSemiGlobal($qStart,$qStop,$sStart,$sStop,$queryLength,$subjectLength,$strand);
	if ($isSemiglobal) {
	    $color = 'green';
	}

    } elsif ($colorScheme eq "SubCoord") {
	my ($hitID) = $alignID =~ /(.+)\#\d+$/;
	my $subjectLength = $hitInfo{$hitID}->[$hitFieldIndex{'length'}];

	my $coordValue = (($sStop+$sStart)/2)/$subjectLength;
	$color = colorValue(0,1./3.,2./3.,1, $coordValue);

    } elsif ($colorScheme eq "%ID") {

	my $pID = 'Undefined';
	if ($matchesover > 0) {
	    $pID = $identities/$matchesover;
	}

	if ($pID ne 'Undefined') {
	    $color = colorValue(@{$colorSettings{$colorScheme}}, $pID);
	} else {
	    $color = 'gray';
	}

    } elsif ($colorScheme eq "%Aligned") {
	my ($hitID) = $alignID =~ /(.+)\#\d+$/;
	my $subjectLength = $hitInfo{$hitID}->[$hitFieldIndex{'length'}];
	my $tValue = ($sStop - $sStart + 1)/($subjectLength);

	$color = colorValue(@{$colorSettings{$colorScheme}}, $tValue);

    } elsif ($colorScheme eq "Score") {
	$color = blastColorValue(40, 50, 80, 200, $score);

    } else {
	warn "Color scheme not defined! - using red for all arrows\n";
    }

    $multiViewCanvas->itemconfigure($arrowID, -fill=>$color);
    return $color;
}

sub blastColorValue {
    my ($t1,$t2,$t3,$t4,$value) = @_;

    my $color;
    if ($value <= $t1) {
	$color = "Gray";
    } elsif ($value < $t2) {
	$color = "Blue";
    } elsif ($value < $t3) {
	$color = "Green";
    } elsif ($value < $t4) {
	$color = "Magenta";
    } else {
	$color = "Red";
    }
    return $color;
}


sub colorValue {
    my ($t1,$t2,$t3,$t4,$value) = @_;

    my $maxValue = 255;

    my $red = "00";
    my $green = "00";
    my $blue = "00";
    if ($value <= $t1) {
	$blue = "ff";
    } elsif ($value < $t2) {
	$green = sprintf("%02x",$maxValue*($value - $t1)/($t2 - $t1));
	$blue = sprintf("%02x",$maxValue*($t2 - $value)/($t2 - $t1));
    } elsif ($value < $t3) {
	$red = sprintf("%02x",$maxValue*($value - $t2)/($t3 - $t2));
	$green = "ff";
    } elsif ($value < $t4) {
	$red = "ff";
	$green = sprintf("%02x",$maxValue*($t4 - $value)/($t4 - $t3));
    } else {
	$red = "ff";
    }
    my $color = "#" . $red . $green . $blue;

    return $color;
}



#Documentation

=pod

=head1 NAME

    blastView3 - Parse and/or visualize NCBI BLAST results

=head1 SYNOPSIS

    blastView3.pl -b BLASTOutputFile [ -p printCommand ]

=head2 Options

=over 4

=item * -b(last) BLASTOutputFile 

Currently only standard NCBI BLAST output is supported.

=item * -p(rint) printCommand

printCommand is of the format: hits=[(all)|best],HSPs=[(all)|best],semiglobal=[(0)|1],sequence=[(0)|1].  At least
one I<field>=I<value> pair is required.

=back

=head1 DESCRIPTION

=over 4 

=item Interactive mode

The default mode of operation (in the absence of the I<-p> option) is interactive visualization.  In this mode,
all B<Subject> sequences aligning to the currently selected B<Query> sequence are visualized in the B<Display Canvas>
in the central panel of the blastView3 window. The B<Query> seqeuence is represented by a red bar at the top of the
B<Display Canvas> with nucleotide (or amino acid) coordinates.  Aligning B<Subject> sequences are depicted as arrows
below the query.  Multiple HSPs from a single subject are shown connected by gray lines.

=back

=over 4

=item * Choosing a B<Query>

The currently selected B<Query> sequence is chosen through the B<Query List> in the lower panel of the
blastView3 window.  The query may be selected directly by left-clicking on an entry in the list.  When the 
B<Query List> has mouse focus, the up- and down-arrow keys may be used to navigate the list.  Finally, the
B<Query List> may be searched via the B<Find Query Entry> just above the B<Query List>.  Typing in this 
entry will perform a regular expression search of the list of query sequence names.  Any valid regular expression
may be used.  The entry will turn green and the list will scan to the first matching query name if a match exists.
Hitting I<ENTER> will change the selected query to the next match.  The entry will turn red if no query
name matches the entered regular expression.  The entry will turn yellow if the regular expression is not a valid
one. The search will wrap around the end of the B<Query List>.

=item * Accessing HSPInfo

Mousing over the B<Query> or B<Subject arrows> will provide a one-line description of the highlighted object in
the B<Highlight Window> just below the B<Display Canvas>.  The highlighted object will be indicated by a white
outline shape (either oval or rectangular depending on whether the query or a subject was highlighted).  Clicking
with the left mouse button on a B<Subject arrow> will provide a detailed description of the HSP selected in the
B<HSPInfo Window> in the top panel of the blastView3 display.  The Identity, Accuracy, Extents, and Alignment of
the HSP will be displayed by default.  This information may be configured via the B<HSPInfo Menu>.  A green rectangle
identifies the currently selected HSP.

=item * Navigating the Display Canvas 

The alignment view may be adjusted by sliding the scrollbars to the right and below the B<Display Canvas> or by
dragging the left-mouse button across the B<Display Canvas>.  The 
horizontal scale of the display may be adjusted either by directly scrolling the B<H-Zoom Slider> at the right side
of the blastView3 window between the B<Display Canvas> and the B<Query List> or by dragging the right-mouse 
button across the B<Display Canvas> (right to zoom-in, left to zoom-out).  The display may be "zoomed-out" to show 
the entire B<Query> or "zoomed-in" as far as single-base per pixel resolution.  (Warning:  very high resolutions of 
large query sequences may cause significant redraw latency).  The B<Display Canvas> may be interactively resized by 
either globally resizing the blastView3 window or by adjusting the separators between the B<Display Canvas> and either
the B<HSPInfo Window> or the B<Query List>.  Hitting I<Alt-r> will resize the blastView3 window to its original size.

=item * HSP Colors

The colors of the B<Subject Arrows> may be set to reflect their Orientation, Score, %ID, or %Aligned via the 
B<Colors Menu>.  The values associated with different colors may be chosen for
the %ID and %Aligned color schemes via the B<Update Colors Entry> at the left side of the blastView3 window 
between the B<Display Canvas> and the B<Query List>.  The values entered must be between 0 and 1 (inclusive) 
and must be increasing in magnitude (blue < green < yellow < red).

=item * File Menu

Alignment information for the currently selected B<Query> may be printed to STDOUT by selecting the 
B<Print Alignments> option from the B<File Menu>.  The user may exit blastView3 either by selecting the B<Exit>
option from this menu or by hitting I<Alt-q>.

=back

=over 4 

=item Print mode

If a I<-p> command line option is provided, the BLAST file will be parsed and 
printed in tab-delimited format to STDOUT (and blastView3 will not enter interactive mode).  
The parameter passed to the I<-p> option is expected to be a
comma-separated list of I<field>=I<value> pairs including at least one of B<hits>=B<[(all)|best]>, 
B<HSPs>=B<[(all)|best]>, B<semiglobal>=B<[(0)|1]>, B<sequence>=B<[(0)|1]>.  The default values are indicated 
parenthetically here.  If B<hits> is set to B<best> only the highest scoring Subject sequence for each Query
 will have its HSPs printed. If B<HSPs> is set to B<best> only the highest scoring HSP from any Subject will be 
printed.  If B<semiglobal> is set to B<1>, only semi-global HSPs will be printed (if you don't know what that 
means, you probably don't want that).  If B<sequence> is set to B<1>, the gapped sequences of the Query and 
Subject will be returned.
These parameters may be set in any combination, providing 16 different styles of output.  The first line
of the output is a tab-delimited descriptor of each column's contents. 

=back 

=head1 AUTHOR
    
    blastView3.pl by Jarrod Chapman <jchapman@lbl.gov> Fri Apr  8 14:13:14 PDT 2005
    Copyright 2005 Jarrod Chapman. All rights reserved.

=cut
