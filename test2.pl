#!/usr/bin/perl

use lib '.','../..','./blib/lib','../../blib/lib','../..';
use strict;
use Bio::Graphics::Panel;
use Bio::Graphics::Feature;

chomp (my $CLASS = shift);
$CLASS or die "\nUsage: lots_of_glyphs IMAGE_CLASS
\t- where IMAGE_CLASS is one of GD or GD::SVG
\t- GD generate png output; GD::SVG generates SVG.\n";

my $ftr = 'Bio::Graphics::Feature';
my $segment = $ftr->new(-start=>1,-end=>2000000,-name=>'Cjejuni',-type=>'clone');

my $panel = Bio::Graphics::Panel->new(
#				      -grid => [50,100,150,200,250,300,310,320,330],
				      -gridcolor => 'lightcyan',
				      -grid => 1,
				      -segment => $segment,
#				      -offset => 300,
#				      -length  => 1700000,
				      -spacing => 15,
				      -width   => 600,
				      -pad_top  => 20,
				      -pad_bottom  => 20,
				      -pad_left => 20,
				      -pad_right=> 20,
#				      -bgcolor => 'teal',
#				      -key_style => 'between',
				      -key_style => 'bottom',
				      -image_class => $CLASS,
				     );
my @colors = $panel->color_names();
$panel->add_track($segment,
		  -glyph => 'arrow',
		  -label => 'base pairs',
		  -double => 1,
		  -bump => 0,
		  -height => 10,
		  -arrowstyle=>'regular',
		  -linewidth=>1,
		  -tkcolor => $colors['yellow'],
		  -tick => 2,
		 );


my $zk154_1 = $ftr->new(-start=>50,-end=>800000,-name=>'ZK154.1',-type=>'gene');
my $zk154_2 = $ftr->new(-start=>1000000,-end=>1100000,-name=>'ZK154.2',-type=>'gene');
my $t = $panel->add_track(
			  transcript => [$zk154_1,$zk154_2],
			  -label => 1,
			  -bump => 1,
			  -key => 'MIRA',
			  -tkcolor => $colors['blue'],
			 );



my $gd    = $panel->gd;

my $type = ($CLASS eq 'GD') ? 'png' : 'svg';
print $gd->$type;

