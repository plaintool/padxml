@echo off
setlocal

::Build 32-bit Lazarus project "padxml" using lazbuild
SET PROJECT_PATH=padxml.lpi
SET BUILD_MODE=Release

::Path to 32-bit FPC compiler
SET FPC32="C:\lazarus\fpc\3.2.2\bin\i386-win32\fpc.exe"

::Rename existing 64-bit exe to padxml64.exe to avoid overwriting
if exist "padxml.exe" (
    echo Renaming existing 64-bit executable...
    ren "padxml.exe" "padxml64.exe"
)

echo Building 32-bit project: %PROJECT_PATH%
"C:\Lazarus\lazbuild.exe" %PROJECT_PATH% --cpu=i386 --ws=win32 --build-mode=%BUILD_MODE% --compiler=%FPC32%

IF %ERRORLEVEL% NEQ 0 (
    echo 32-bit build failed!
    ::Restore 64-bit exe back
    if exist "padxml64.exe" ren "padxml64.exe" "padxml.exe"
    pause
    exit /b %ERRORLEVEL%
)

::Rename output to padxml32.exe to distinguish from 64-bit
if exist "padxml.exe" (
    echo Renaming 32-bit executable...
    if exist "padxml32.exe" del /F /Q "padxml32.exe"
    ren "padxml.exe" "padxml32.exe"
)

::Restore 64-bit exe back to original name
if exist "padxml64.exe" (
    echo Restoring 64-bit executable name...
    if exist "padxml.exe" del /F /Q "padxml.exe"
    ren "padxml64.exe" "padxml.exe"
)

echo 32-bit build completed successfully

::Wait 2 seconds to ensure file is free
timeout /t 2 /nobreak >nul

::Certificate settings
SET SIGNTOOL="C:\Program Files (x86)\Windows Kits\10\bin\10.0.26100.0\x64\signtool.exe"
SET CERTFILE=%~dp0installer\AlexanderT.pfx
SET CERTPASS=1234
SET TIMESTAMP_URL=http://timestamp.digicert.com

::Sign the 32-bit executable
if exist "padxml32.exe" (
    echo Signing 32-bit executable...
    %SIGNTOOL% sign /f "%CERTFILE%" /p "%CERTPASS%" /fd SHA256 /tr %TIMESTAMP_URL% /td SHA256 "padxml32.exe"
    IF %ERRORLEVEL% EQU 0 (
        echo Signing completed successfully
    ) else (
        echo Signing failed
    )
) else (
    echo Executable not found: padxml32.exe
)