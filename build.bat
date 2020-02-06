@echo off
set rsvars="d:\Program Files (x86)\Embarcadero\Studio\19.0\bin\rsvars.bat"
call %rsvars%
call cleanup.bat
call copyresources.bat

set project=.\Packages\DDuce.Core.dproj
msbuild %project% /t:make /p:config=Release /p:platform=Win32
msbuild %project% /t:make /p:config=Debug /p:platform=Win32
msbuild %project% /t:make /p:config=Release /p:platform=Win64
msbuild %project% /t:make /p:config=Debug /p:platform=Win64

set project=.\Packages\DDuce.Components.dproj
msbuild %project% /t:make /p:config=Release /p:platform=Win32
msbuild %project% /t:make /p:config=Debug /p:platform=Win32
msbuild %project% /t:make /p:config=Release /p:platform=Win64
msbuild %project% /t:make /p:config=Debug /p:platform=Win64

set project=.\Packages\DDuce.Modules.dproj
msbuild %project% /t:make /p:config=Release /p:platform=Win32
msbuild %project% /t:make /p:config=Debug /p:platform=Win32
msbuild %project% /t:make /p:config=Release /p:platform=Win64
msbuild %project% /t:make /p:config=Debug /p:platform=Win64

set project=.\Demos\DDuce.Demos.dproj
msbuild %project% /t:make /p:config=Debug /p:platform=Win32
msbuild %project% /t:make /p:config=Debug /p:platform=Win64

set project=.\Tests\DDuce.Tests.dproj
msbuild %project% /t:make /p:config=Debug /p:platform=Win32
msbuild %project% /t:make /p:config=Debug /p:platform=Win64
pause
