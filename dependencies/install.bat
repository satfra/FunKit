@echo off
setlocal enabledelayedexpansion

set wolfram_app_dir=%1
if not defined wolfram_app_dir (
    echo Usage: install.bat [wolfram_app_directory]
    exit /b 1
)

:: Create the Wolfram application directory if it doesn't exist
if not exist "%wolfram_app_dir%" mkdir "%wolfram_app_dir%"

:: Get the path where this script is located
set script_path=%~dp0

:: Install QMeS
if not exist "%wolfram_app_dir%\QMeSderivation" (
    echo Installing QMeS to %wolfram_app_dir%\QMeSderivation
    if exist "%script_path%QMeS-Derivation-main" rd /s /q "%script_path%QMeS-Derivation-main"
    powershell -Command "Expand-Archive -Path '%script_path%QMeS.zip' -DestinationPath '%script_path%' -Force" >nul 2>&1
    move "%script_path%QMeS-Derivation-main" "%wolfram_app_dir%\QMeSderivation" >nul
)

:: Install FormTracer
if not exist "%wolfram_app_dir%\FormTracer" (
    echo Installing FormTracer to %wolfram_app_dir%\FormTracer
    if exist "%script_path%FormTracer" rd /s /q "%script_path%FormTracer"
    powershell -Command "Expand-Archive -Path '%script_path%FormTracer.zip' -DestinationPath '%script_path%' -Force" >nul 2>&1
    move "%script_path%FormTracer" "%wolfram_app_dir%\FormTracer" >nul
)

:: Install TensorBases
if not exist "%wolfram_app_dir%\TensorBases" (
    echo Installing TensorBases to %wolfram_app_dir%\TensorBases
    if exist "%script_path%TensorBases-main" rd /s /q "%script_path%TensorBases-main"
    powershell -Command "Expand-Archive -Path '%script_path%TensorBases.zip' -DestinationPath '%script_path%' -Force" >nul 2>&1
    move "%script_path%TensorBases-main" "%wolfram_app_dir%\TensorBases" >nul
)

echo Installation completed successfully!
endlocal