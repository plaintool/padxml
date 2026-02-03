@echo off
setlocal

:: Define paths
SET "SOURCE_DIR=%~dp0"
SET "VERSION=%VERSION%"
IF "%~2" NEQ "" (
    SET "VERSION=%~2"
)
IF "%VERSION%"=="" (
    SET "VERSION=0.0.0"
)

:: Check if platform parameter is provided
IF "%1"=="" (
    SET "PLATFORM=x64"
) ELSE (
    SET "PLATFORM=%1"
)

:: Validate platform parameter
IF NOT "%PLATFORM%"=="x64" IF NOT "%PLATFORM%"=="x86" (
    echo Error: Invalid platform parameter. Use x64 or x86.
    echo Example: %0 x64
    echo Example: %0 x86
    exit /b 1
)

:: --- Build peruser ---
echo Compiling msisetup_peruser.wxs with candle for %PLATFORM%...
candle -nologo "%SOURCE_DIR%\msisetup_peruser.wxs" -out "%SOURCE_DIR%\peruser.wixobj" -ext WixUIExtension -dPlatform=%PLATFORM%
echo Linking peruser.wixobj into padxml-%VERSION%-%PLATFORM%.msi with light...
light -nologo "%SOURCE_DIR%\peruser.wixobj" -out "%SOURCE_DIR%\padxml-%VERSION%-%PLATFORM%.msi" -ext WixUIExtension
echo File created: padxml-%VERSION%-%PLATFORM%.msi
echo.

:: --- Build permachine ---
echo Compiling msisetup_permachine.wxs with candle for %PLATFORM%...
candle -nologo "%SOURCE_DIR%\msisetup_permachine.wxs" -out "%SOURCE_DIR%\permachine.wixobj" -ext WixUIExtension -dPlatform=%PLATFORM%
echo Linking permachine.wixobj into padxml-%VERSION%-%PLATFORM%-allusers.msi with light...
light -nologo "%SOURCE_DIR%\permachine.wixobj" -out "%SOURCE_DIR%\padxml-%VERSION%-%PLATFORM%-allusers.msi" -ext WixUIExtension
echo File created: padxml-%VERSION%-%PLATFORM%-allusers.msi
echo.

:: --- Clean temporary files ---
echo Deleting temporary .wixobj and .wixpdb files...
del /q "%SOURCE_DIR%\*.wixobj" >nul
del /q "%SOURCE_DIR%\*.wixpdb" >nul
echo Cleanup completed.
echo.

:: --- Sign installers ---
IF "%SIGNTOOL%"=="" (
    SET "SIGNTOOL=C:\Program Files (x86)\Windows Kits\10\bin\10.0.26100.0\x64\signtool.exe"
)
IF "%CERTFILE%"=="" (
    SET "CERTFILE="
)
SET "CERTPASS=1234"
SET "TIMESTAMP_URL=http://timestamp.digicert.com"

if not "%CERTFILE%"=="" (
    if exist "%CERTFILE%" (
        if exist "%SIGNTOOL%" (
            echo Signing MSI files...
            "%SIGNTOOL%" sign /f "%CERTFILE%" /p "%CERTPASS%" /fd SHA256 /tr %TIMESTAMP_URL% /td SHA256 "%SOURCE_DIR%\padxml-%VERSION%-%PLATFORM%.msi"
            IF %ERRORLEVEL% EQU 0 (
                echo Signing of padxml-%VERSION%-%PLATFORM%.msi completed successfully
            ) else (
                echo Signing failed for padxml-%VERSION%-%PLATFORM%.msi
            )

            "%SIGNTOOL%" sign /f "%CERTFILE%" /p "%CERTPASS%" /fd SHA256 /tr %TIMESTAMP_URL% /td SHA256 "%SOURCE_DIR%\padxml-%VERSION%-%PLATFORM%-allusers.msi"
            IF %ERRORLEVEL% EQU 0 (
                echo Signing of padxml-%VERSION%-%PLATFORM%-allusers.msi completed successfully
            ) else (
                echo Signing failed for padxml-%VERSION%-%PLATFORM%-allusers.msi
            )
        ) else (
            echo Skipping signing (signtool not found).
        )
    ) else (
        echo Skipping signing (cert file not found).
    )
) else (
    echo Skipping signing (CERTFILE not set).
)

:: --- Portable ---
if "%BUILD_PORTABLE%"=="1" (
    powershell -Command ^
    "Compress-Archive ^
     -Force ^
     -Path ..\padxml.exe,..\padxml32.exe, .\form_settings.json ^
     -DestinationPath padxml-%VERSION%-x86-x64-portable.zip"
)

echo Build and signing completed successfully!
