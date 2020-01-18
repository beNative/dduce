@echo off
set source=.\Source\Modules\*.dfm
set dest=.\Lib\Win32\Debug
copy %source% %dest%
set dest=.\Lib\Win32\Release
copy %source% %dest%
set dest=.\Lib\Win64\Debug
copy %source% %dest%
set dest=.\Lib\Win64\Release
copy %source% %dest%

set source=.\Source\Modules\Editor\*.dfm
set dest=.\Lib\Win32\Debug
copy %source% %dest%
set dest=.\Lib\Win32\Release
copy %source% %dest%
set dest=.\Lib\Win64\Debug
copy %source% %dest%
set dest=.\Lib\Win64\Release
copy %source% %dest%

set source=.\Source\Modules\ObjectInspector\*.dfm
set dest=.\Lib\Win32\Debug
copy %source% %dest%
set dest=.\Lib\Win32\Release
copy %source% %dest%
set dest=.\Lib\Win64\Debug
copy %source% %dest%
set dest=.\Lib\Win64\Release
copy %source% %dest%

set source=.\Source\Modules\RTTEye\*.dfm
set dest=.\Lib\Win32\Debug
copy %source% %dest%
set dest=.\Lib\Win32\Release
copy %source% %dest%
set dest=.\Lib\Win64\Debug
copy %source% %dest%
set dest=.\Lib\Win64\Release
copy %source% %dest%

set source=.\Source\Components\*.dfm
set dest=.\Lib\Win32\Debug
copy %source% %dest%
set dest=.\Lib\Win32\Release
copy %source% %dest%
set dest=.\Lib\Win64\Debug
copy %source% %dest%
set dest=.\Lib\Win64\Release
copy %source% %dest%

set source=.\Source\Components\*.res
set dest=.\Lib\Win32\Debug
copy %source% %dest%
set dest=.\Lib\Win32\Release
copy %source% %dest%
set dest=.\Lib\Win64\Debug
copy %source% %dest%
set dest=.\Lib\Win64\Release
copy %source% %dest%
pause
