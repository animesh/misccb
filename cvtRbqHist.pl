#!/usr/bin/perl

use strict;

	my $sFile = "history.txt";
	open( FIN, "<$sFile" ) or die;
	my $sLine;
	while( $sLine = <FIN>)
	{
		$sLine =~ tr/\r\n//d;
		$sLine =~ s/^\/\///;
		$sLine =~ s/&/&amp;/g;
		$sLine =~ s/</&lt;/g;
		$sLine =~ s/>/&gt;/g;
#print( $sLine );
#		my ( $sMm, $sDd, $sYy, $sInit, $sVer, $sRest ) = ( $sLine =~ /(\d+)\/(\d+)\/(\d+) (\w+) (\w+) (\w+)/ );
		my ( $sMm, $sDd, $sYy, $sInit, $sVer, $sRest ) = ( $sLine =~ /(\d+)[\/\-](\d+)[\/\-](\d+)\s+(\w+)\s+(\w+\.\w+)\s+(.+)/ );
		if( !$sMm )
		{ 
			( $sMm, $sDd, $sYy, $sInit, $sRest ) = ( $sLine =~ /(\d+)[\/\-](\d+)[\/\-](\d+)\s+(\w+)\s+(.+)/ );
			$sVer = undef;
		}
		if( $sMm )
		{
			if( $sMm >= 2000 )
			{
				my $sTmp = $sYy;
				$sYy = $sMm;
				$sMm = $sDd;
				$sDd = $sTmp;	
			}
			print( "\n<h2>$sYy-$sMm-$sDd</h2>\n" );
			print( "\n" );
#			if( $sYy < 2003 )
#			{
				print( "(rbq log, history.txt)<br>\n" );
#			}

#			print( "$sInit\n" );
			if( $sVer )
			{
				print( "$sVer<br>\n" );
			}
			print( "$sRest<br>\n" );
		}
		else
		{
			$sLine =~ s/^\s+//;
			if( length( $sLine ) > 0 )
			{
				print( "$sLine<br>\n" );
			}
#print( "month not found\n" );
		}
#exit( 0 );
	}
	close( FIN );
