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

:: Install FormTracer.
:: FunKit requires the FormTracer bundled here, which carries fixes that upstream
:: does not (getOpenIndices index census, leftover Levi-Civita contraction)
:: without a version bump. So an installation that is already there but differs
:: from the bundle is replaced, not kept -- a stale copy aborts FORM traces on
:: well-formed expressions (see tests/CrossTests/GhostGluonFormTests.m). The
:: comparison is on FormTracer.m alone: it is the entire package, everything else
:: is documentation. Keep in step with install.sh.
set ft_dir=%wolfram_app_dir%\FormTracer
set ft_install=1

if exist "%ft_dir%" (
    if exist "%script_path%FormTracer" rd /s /q "%script_path%FormTracer"
    powershell -Command "Expand-Archive -Path '%script_path%FormTracer.zip' -DestinationPath '%script_path%' -Force" >nul 2>&1
    fc /b "%script_path%FormTracer\FormTracer.m" "%ft_dir%\FormTracer.m" >nul 2>&1
    if !errorlevel! equ 0 (
        set ft_install=0
        rd /s /q "%script_path%FormTracer"
    ) else (
        echo The FormTracer at %ft_dir% differs from the one bundled with FunKit.
        for /f "delims=" %%l in ('powershell -NoProfile -Command "if((Get-Item -LiteralPath '%ft_dir%' -Force).LinkType){'link'}else{'dir'}"') do set ft_link=%%l
        if "!ft_link!"=="link" (
            :: A linked development checkout. Renaming it would move the user's own
            :: working tree out from under them, so refuse and let them update it.
            echo   %ft_dir% is a link, so it is left untouched. 1>&2
            echo   Update the FormTracer checkout it points at to match dependencies\FormTracer.zip. 1>&2
            exit /b 1
        )
        echo   Moving the old installation to %ft_dir%.bak
        if exist "%ft_dir%.bak" rd /s /q "%ft_dir%.bak"
        move "%ft_dir%" "%ft_dir%.bak" >nul
    )
)

if "!ft_install!"=="1" (
    echo Installing FormTracer to %ft_dir%
    if not exist "%script_path%FormTracer" (
        powershell -Command "Expand-Archive -Path '%script_path%FormTracer.zip' -DestinationPath '%script_path%' -Force" >nul 2>&1
    )
    move "%script_path%FormTracer" "%ft_dir%" >nul
)

:: Install TensorBases.
:: TensorBases carries a minimum version, unlike the other two dependencies.
:: From 1.3.0 on, TBMakePropagator expands the inverse propagator with all
:: momenta incoming; older versions return every momentum-odd propagator
:: dressing -- for instance the pslash dressing of the quark propagator -- with
:: the wrong sign relative to the momentum-even ones, and do so silently. So
:: this block, unlike the two above, also replaces an installation that is
:: already there but too old. Keep in step with install.sh.
set tb_required=1.3.0
set tb_dir=%wolfram_app_dir%\TensorBases
set tb_install=1

if exist "%tb_dir%" (
    for /f "delims=" %%v in ('powershell -NoProfile -Command "$m=Select-String -Path '%tb_dir%\PacletInfo.m' -Pattern 'Version[^0-9]*([0-9]+\.[0-9]+\.[0-9]+)'; if($m){$m.Matches[0].Groups[1].Value}else{'0.0.0'}" 2^>nul') do set tb_have=%%v
    if not defined tb_have set tb_have=0.0.0
    for /f "delims=" %%r in ('powershell -NoProfile -Command "if([version]'!tb_have!' -ge [version]'%tb_required%'){'ok'}else{'old'}"') do set tb_state=%%r
    if "!tb_state!"=="ok" (
        set tb_install=0
    ) else (
        echo TensorBases !tb_have! is installed at %tb_dir%, but FunKit requires %tb_required% or newer.
        for /f "delims=" %%l in ('powershell -NoProfile -Command "if((Get-Item -LiteralPath '%tb_dir%' -Force).LinkType){'link'}else{'dir'}"') do set tb_link=%%l
        if "!tb_link!"=="link" (
            :: A linked development checkout. Renaming it would move the user's own
            :: working tree out from under them, so refuse and let them update it.
            echo   %tb_dir% is a link, so it is left untouched. 1>&2
            echo   Update the TensorBases checkout it points at to %tb_required% or newer. 1>&2
            exit /b 1
        )
        echo   Moving the old installation to %tb_dir%.!tb_have!.bak
        if exist "%tb_dir%.!tb_have!.bak" rd /s /q "%tb_dir%.!tb_have!.bak"
        move "%tb_dir%" "%tb_dir%.!tb_have!.bak" >nul
        :: The basis cache is deliberately not carried over: its serialisation
        :: format changed between 1.1.6 and 1.3.0, so the first load rebuilds it.
        echo   The basis cache is not carried over; the first load will rebuild it.
    )
)

if "!tb_install!"=="1" (
    if exist "%script_path%TensorBases-main" rd /s /q "%script_path%TensorBases-main"
    powershell -Command "Expand-Archive -Path '%script_path%TensorBases.zip' -DestinationPath '%script_path%' -Force" >nul 2>&1
    for /f "delims=" %%v in ('powershell -NoProfile -Command "$m=Select-String -Path '%script_path%TensorBases-main\PacletInfo.m' -Pattern 'Version[^0-9]*([0-9]+\.[0-9]+\.[0-9]+)'; if($m){$m.Matches[0].Groups[1].Value}else{'0.0.0'}" 2^>nul') do set tb_bundled=%%v
    if not defined tb_bundled set tb_bundled=0.0.0
    for /f "delims=" %%r in ('powershell -NoProfile -Command "if([version]'!tb_bundled!' -ge [version]'%tb_required%'){'ok'}else{'old'}"') do set tb_zipstate=%%r
    if "!tb_zipstate!"=="old" (
        echo ERROR: dependencies\TensorBases.zip bundles TensorBases !tb_bundled!, which is below 1>&2
        echo        the required %tb_required%. Run dependencies/update_to_latest.sh to refresh it. 1>&2
        exit /b 1
    )
    echo Installing TensorBases !tb_bundled! to %tb_dir%
    move "%script_path%TensorBases-main" "%tb_dir%" >nul
)

echo Installation completed successfully!
endlocal