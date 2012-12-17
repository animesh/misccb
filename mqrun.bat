DIR /B M:\RAW\KA\P\12*.raw > tempfile.txt

FOR /F "tokens=1,2 delims=." %%i in (tempfile.txt) do call :Change %%i

:: FOR %%A IN (DEL) DO %%A tempfile.txt

:: C:\Users\animeshs\SkyDrive\MaxQuant\bin\MaxQuantCmd.exe C:\Users\animeshs\Desktop\check.xml 4




:Change
setlocal enabledelayedexpansion
set FileN=%~1
set INTEXTFILE=M:\RAW\KA\P\KAParCp.xml
set OUTTEXTFILE=M:\RAW\KA\P\%FileN%.xml
set SEARCHTEXT=TestFile
set REPLACETEXT=%FileN%
set OUTPUTLINE=

if exist %OUTTEXTFILE% del %OUTTEXTFILE%
for /f "tokens=1,* delims=¶" %%A in ( '"type %INTEXTFILE%"') do (
SET string=%%A
SET modified=!string:%SEARCHTEXT%=%REPLACETEXT%!

echo !modified! >> %OUTTEXTFILE%
)


:: Source 
:: https://irfanview-forum.de/showthread.php?t=3263 
:: http://stackoverflow.com/questions/5273937/how-to-replace-substrings-in-windows-batch-file
:: http://www.pcreview.co.uk/forums/delims-t1466398.html
:: http://www.robvanderwoude.com/for.php 
