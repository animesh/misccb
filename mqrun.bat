set MAXQUANTDIR=C:\Users\animeshs\SkyDrive\MaxQuant
set DATADIR=M:\RAW\P
set PREFIXRAW=npq
set PARAMFILE=mqpar.xml
set SEARCHTEXT=TestFile

DIR /B %DATADIR%\%PREFIXRAW%*.raw > %DATADIR%\tempfile.txt

FOR /F "eol=  tokens=1,2 delims=." %%i in (%DATADIR%\tempfile.txt) do  ( 
	if not %%i ==   "" (
		call :Change %%i
		:: %MAXQUANTDIR%\bin\MaxQuantCmd.exe %DATADIR%\%%i.xml 4
		if exist %DATADIR%\%%i.txt del %DATADIR%\%%i.txt
		copy %DATADIR%\combined\txt\proteinGroups.txt %DATADIR%\%%i.txt
	)
)


GOTO :Source 

:Change

	setlocal enabledelayedexpansion

	set FileN=%~1
	set INTEXTFILE=%DATADIR%\%PARAMFILE%
	set OUTTEXTFILE=%DATADIR%\%FileN%.xml
	set REPLACETEXT=%FileN%
	set OUTPUTLINE=

	if exist %OUTTEXTFILE% del %OUTTEXTFILE%
	for /f "tokens=1,* delims=¶" %%A in ( '"type %INTEXTFILE%"') do (
	SET string=%%A
	SET modified=!string:%SEARCHTEXT%=%REPLACETEXT%!

	echo !modified! >> %OUTTEXTFILE%
	)

:Source 

:: https://irfanview-forum.de/showthread.php?t=3263 
:: http://stackoverflow.com/questions/5273937/how-to-replace-substrings-in-windows-batch-file
:: http://www.pcreview.co.uk/forums/delims-t1466398.html
:: http://www.robvanderwoude.com/for.php
:: http://stackoverflow.com/questions/3713601/subroutines-in-batch-files
