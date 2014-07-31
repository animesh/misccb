#!/usr/bin/perl

use strict;

	my $sFile = "rb.log";
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
		my ( $sMm, $sDd, $sYy, $sInit, $sVer, $sRest ) = ( $sLine =~ /(\d+)\/(\d+)\/(\d+)\s+(\w+)\s+(\w+\.\w+)\s+(.+)/ );
		if( !$sMm )
		{ 
			( $sMm, $sDd, $sYy, $sInit, $sRest ) = ( $sLine =~ /(\d+)\/(\d+)\/(\d+)\s+(\w+)\s+(.+)/ ); 
			$sVer = undef;
		}
		if( $sMm )
		{
			if( $sYy < 2000 )
				{ $sYy = 2000 + $sYy; }

			print( "\n<h2>$sYy-$sMm-$sDd</h2>\n" );
			print( "\n" );
			if( $sYy >= 2002 && $sMm >= 6 )
			{
				print( "(rb log, rb.cpp)<br>\n" );
			}

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
