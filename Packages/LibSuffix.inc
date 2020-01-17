{
  Copyright (C) 2013-2020 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

{$IFNDEF LIBSUFFIX_INC}
{$DEFINE LIBSUFFIX_INC}

{$IFDEF VER230}
  {$LIBSUFFIX '.DelphiXE2'}
{$ENDIF VER230}

{$IFDEF VER240}
  {$LIBSUFFIX '.DelphiXE3'}
{$ENDIF VER240}

{$IFDEF VER250}
  {$LIBSUFFIX '.DelphiXE4'}
{$ENDIF VER250}

{$IFDEF VER260}
  {$LIBSUFFIX '.DelphiXE5'}
{$ENDIF VER260}

{$IFDEF VER270}
  {$LIBSUFFIX '.DelphiXE6'}
{$ENDIF VER270}

{$IFDEF VER280}
  {$LIBSUFFIX '.DelphiXE7'}
{$ENDIF VER280}

{$IFDEF VER290}
  {$LIBSUFFIX '.DelphiXE8'}
{$ENDIF VER290}

{$IFDEF VER300}
  {$LIBSUFFIX '.Delphi10Seattle'}
{$ENDIF VER300}

{$IFDEF VER310}
  {$LIBSUFFIX '.Delphi10Berlin'}
{$ENDIF VER310}

{$IFDEF VER320}
  {$LIBSUFFIX '.Delphi10Tokyo'}
{$ENDIF VER320}

{$IFDEF VER330}
  {$LIBSUFFIX '.Delphi10Rio'}
{$ENDIF VER330}

{$ENDIF ~LIBSUFFIX_INC}
