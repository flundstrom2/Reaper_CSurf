@echo off
echo MUST be run as Admin!
:: BatchGotAdmin
:: from http://stackoverflow.com/questions/1894967/how-to-request-administrator-access-inside-a-batch-file

:-------------------------------------
REM  --> Check for permissions
>nul 2>&1 "%SYSTEMROOT%\system32\cacls.exe" "%SYSTEMROOT%\system32\config\system"

REM --> If error flag set, we do not have admin.
if '%errorlevel%' NEQ '0' (
    echo Requesting administrative privileges...
    goto UACPrompt
) else ( goto gotAdmin )

:UACPrompt
    echo Set UAC = CreateObject^("Shell.Application"^) > "%temp%\getadmin.vbs"
    set params = %*:"=""
    echo UAC.ShellExecute "cmd.exe", "/c %~s0 %params%", "", "runas", 1 >> "%temp%\getadmin.vbs"

    "%temp%\getadmin.vbs"
    del "%temp%\getadmin.vbs"
    exit /B

:gotAdmin
    pushd "%CD%"
    CD /D "%~dp0"
:--------------------------------------

cd "C:\Users\Fredrik\Documents\Visual Studio 2008\Projects\reaper_extension_sdk\jmde\csurf"
echo Shutting down REAPER (if it exist)...
taskkill 2>&1 >NUL /IM reaper.exe
set rc=%ERRORLEVEL%
sleep 3s
if %rc% EQU 128 goto cont

:retry
tasklist /FI "IMAGENAME eq REAPER.exe" | find >NUL /i "REAPER.exe"
set rc=%ERRORLEVEL%
IF %rc% EQU 1 GOTO cont
echo REAPER seems to request your attention...
sleep 1s
goto retry

:cont
echo Copying...
xcopy ..\Debug\Plugins\reaper_csurf_mf8_debug.dll "C:\Program Files (x86)\REAPER\Plugins" /Y
xcopy ..\Release\Plugins\reaper_csurf_mf8.dll "C:\Program Files (x86)\REAPER\Plugins" /Y
echo Starting REAPER...
start /B startreaper.bat
