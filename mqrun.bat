DIR /B M:\RAW\KA\*.raw > tempfile.txt


FOR /F "tokens=1,2 delims=." %%i in (tempfile.txt) do @echo %%i

:: FOR %%A IN (DEL) DO %%A tempfile.txt

:: C:\Users\animeshs\SkyDrive\MaxQuant\bin\MaxQuantCmd.exe C:\Users\animeshs\Desktop\check.xml 4

:: Source http://www.robvanderwoude.com/for.php 

    
  
  
if exist M:\RAW\KA\KAParCpN.xml del M:\RAW\KA\KAParCpN.xml
for /f "tokens=2 delims= " %%a in (M:\RAW\KA\KAParCp.xml) do call :Change "%%a"
exit /b

:Change
set Text=%~1
if "%Text%"=="Karpas_4%_2_101212_121213103116" (
(@echo %%i)>> M:\RAW\KA\KAParCpN.xml
) else (
(echo %Text%)>> M:\RAW\KA\KAParCpN.xml
)
exit /b

:: Source https://irfanview-forum.de/showthread.php?t=3263 