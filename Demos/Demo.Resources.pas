{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Demo.Resources;

interface

const
  TEXTEDITOR_SETTINGS_FILE = 'settings.texteditor.json';

  LOREM_IPSUM =
    'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod ' +
    'tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim '    +
    'veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea ' +
    'commodo consequat. Duis aute irure dolor in reprehenderit in voluptate '  +
    'velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint '       +
    'occaecat cupidatat non proident, sunt in culpa qui officia deserunt '     +
    'mollit anim id est laborum.';

  EXAMPLE_SQL =
    '/* Chinook database example query */'    + sLineBreak +
    'select'                                  + sLineBreak +
    '  *'                                     + sLineBreak +
    'from '                                   + sLineBreak +
    '  InvoiceLine il'                        + sLineBreak +
    '  inner join Invoice i'                  + sLineBreak +
    '    on (i.InvoiceId = il.InvoiceId)'     + sLineBreak +
    '  inner join Customer c'                 + sLineBreak +
    '    on (c.CustomerId = i.CustomerId)'    + sLineBreak +
    '  inner join Employee e'                 + sLineBreak +
    '    on (e.EmployeeId = c.SupportRepId)'  + sLineBreak +
    '  inner join Track t'                    + sLineBreak +
    '    on (il.TrackId = t.TrackId)'         + sLineBreak +
    '  inner join Album al'                   + sLineBreak +
    '    on (al.AlbumId = t.AlbumId)'         + sLineBreak +
    '  inner join MediaType mt'               + sLineBreak +
    '    on (mt.MediaTypeId = t.MediaTypeId)' + sLineBreak +
    '  inner join Genre g'                    + sLineBreak +
    '    on (g.GenreId = t.GenreId)';

  VT_VISIBLE_PROPERTIES : array of string = [
    'Color',
    'Colors',
    'ColorSettings',
    'DefaultNodeHeight',
    'DefaultText',
    'DragImageKind',
    'DragKind',
    'DragMode',
    'DragOperations',
    'DragType',
    'DragWidth',
    'DrawSelectionMode',
    'EmptyListMessage',
    'Enabled',
    'Font',
    'Header',
    'Hint',
    'HintMode',
    'Indent',
    'LineMode',
    'LineStyle',
    'Margin',
    'NodeAlignment',
    'ShowHint',
    'TextMargin',
    'TreeOptions',
    'Visible'
  ];

{$REGION 'EXAMPLE_XML_DOCUMENT'}
  EXAMPLE_XML_DOCUMENT = '''
  <?xml version="1.0" encoding="UTF-8"?>
  <library xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
           xsi:noNamespaceSchemaLocation="library.xsd"
           xmlns:meta="http://example.org/meta">

    <!-- Book with multiple authors, attributes, and CDATA -->
    <book id="b101" language="en" available="yes">
      <title>Learning XML</title>
      <author>Jane Doe</author>
      <author>John Smith</author>
      <year>2024</year>
      <genre>Technical</genre>
      <price currency="USD">39.95</price>
      <summary><![CDATA[
        Learn XML from scratch. Covers elements, attributes, CDATA, schemas,
        namespaces, and more. "Great for beginners!"
      ]]></summary>
    </book>

    <!-- Book with escaped characters and a namespaced tag -->
    <book id="b102" available="no">
      <title>Escape &amp; Encode: The &lt;XML&gt; Way</title>
      <author>Alice Walker</author>
      <year>2023</year>
      <genre>Education</genre>
      <price currency="EUR">29.99</price>
      <summary>This book explains how to use &amp;, &lt;, &gt; properly in XML.</summary>
      <meta:note>European Bestseller</meta:note>
    </book>

    <!-- A simpler magazine entry -->
    <magazine id="m200" frequency="monthly">
      <title>XML Monthly</title>
      <issue>2025-05</issue>
      <editor>Bob Marley</editor>
    </magazine>

    <!-- Optional element not present in all entries -->
    <book id="b103">
      <title>XML Databases</title>
      <author>Dr. XMLstein</author>
      <year>2022</year>
      <genre>Data</genre>
      <price currency="GBP">24.50</price>
      <summary>A bridge between structured documents and relational storage.</summary>
    </book>

  </library>
  ''';
{$ENDREGION}

{$REGION 'EXAMPLE_JSON_DOCUMENT'}
  EXAMPLE_JSON_DOCUMENT = '''
  {
    "DebugMode": false,
    "EmitLogMessages": true,
    "FormSettings": {
      "Left": 1036,
      "Top": 414,
      "Width": 1023,
      "Height": 499,
      "FormStyle": 0,
      "WindowState": 0
    },
    "MessageListSettings": {
      "AutoScrollMessages": true,
      "AutoFilterMessages": true,
      "ColumnHeadersVisible": true,
      "DynamicAutoSizeColumns": true,
      "MessageDetailsVisible": true,
      "SmartTimeStamps": true,
      "HorizontalPanelPositions": [
        0.232743867655448,
        0.836328125,
        1
      ],
      "LeftVerticalPanelPositions": [
        0.204040404040404,
        1
      ]
    },
    "WinodsSettings": {
      "Enabled": true
    },
    "WinipcSettings": {
      "Enabled": false,
      "PollingInterval": 100
    },
    "ZmqSettings": {
      "Enabled": false,
      "PollingInterval": 100,
      "PollingTimeout": 10,
      "Endpoints": "New=tcp://SBVCTITPOA8:5555\r\n"
    },
    "FileSystemSettings": {
      "Enabled": false,
      "PathNames": ""
    },
    "ComPortSettings": {
      "ComPorts": "GPS=com30:115200\r\n"
    },
    "DisplayValuesSettings": {
      "Id": {
        "BackgroundColor": 16777215,
        "FontName": "Segoe UI",
        "FontColor": 8421504,
        "FontSize": 8,
        "FontStyle": "",
        "WordWrap": false,
        "HorizontalAlignment": 0,
        "VerticalAlignment": 0
      },
      "Info": {
        "BackgroundColor": 16711680,
        "FontName": "Segoe UI",
        "FontColor": 16777215,
        "FontSize": 8,
        "FontStyle": "fsBold",
        "WordWrap": false,
        "HorizontalAlignment": 0,
        "VerticalAlignment": 0
      },
      "Warning": {
        "BackgroundColor": 42495,
        "FontName": "Segoe UI",
        "FontColor": 16777215,
        "FontSize": 8,
        "FontStyle": "fsBold",
        "WordWrap": false,
        "HorizontalAlignment": 0,
        "VerticalAlignment": 0
      },
      "Error": {
        "BackgroundColor": 255,
        "FontName": "Segoe UI",
        "FontColor": 16777215,
        "FontSize": 8,
        "FontStyle": "fsBold",
        "WordWrap": false,
        "HorizontalAlignment": 0,
        "VerticalAlignment": 0
      },
      "TimeStamp": {
        "BackgroundColor": 15724527,
        "FontName": "Segoe UI",
        "FontColor": 16711680,
        "FontSize": 8,
        "FontStyle": "",
        "WordWrap": false,
        "HorizontalAlignment": 0,
        "VerticalAlignment": 0
      },
      "ValueName": {
        "BackgroundColor": 16777215,
        "FontName": "Segoe UI",
        "FontColor": 128,
        "FontSize": 8,
        "FontStyle": "",
        "WordWrap": false,
        "HorizontalAlignment": 0,
        "VerticalAlignment": 0
      },
      "ValueType": {
        "BackgroundColor": 16777215,
        "FontName": "Consolas",
        "FontColor": 8388608,
        "FontSize": 8,
        "FontStyle": "",
        "WordWrap": false,
        "HorizontalAlignment": 0,
        "VerticalAlignment": 0
      },
      "Value": {
        "BackgroundColor": 16777215,
        "FontName": "Consolas",
        "FontColor": 0,
        "FontSize": 8,
        "FontStyle": "fsBold",
        "WordWrap": false,
        "HorizontalAlignment": 0,
        "VerticalAlignment": 0
      },
      "CheckPoint": {
        "BackgroundColor": 32768,
        "FontName": "Segoe UI",
        "FontColor": 16777215,
        "FontSize": 8,
        "FontStyle": "fsBold",
        "WordWrap": false,
        "HorizontalAlignment": 0,
        "VerticalAlignment": 0
      },
      "Counter": {
        "BackgroundColor": 16777215,
        "FontName": "Segoe UI",
        "FontColor": 8388736,
        "FontSize": 8,
        "FontStyle": "",
        "WordWrap": false,
        "HorizontalAlignment": 0,
        "VerticalAlignment": 0
      },
      "Tracing": {
        "BackgroundColor": 15724527,
        "FontName": "Segoe UI",
        "FontColor": 32896,
        "FontSize": 8,
        "FontStyle": "fsBold,fsUnderline",
        "WordWrap": false,
        "HorizontalAlignment": 0,
        "VerticalAlignment": 0
      },
      "Enter": {
        "BackgroundColor": 15724527,
        "FontName": "Segoe UI",
        "FontColor": 32896,
        "FontSize": 8,
        "FontStyle": "fsBold",
        "WordWrap": false,
        "HorizontalAlignment": 0,
        "VerticalAlignment": 0
      },
      "Leave": {
        "BackgroundColor": 15724527,
        "FontName": "Segoe UI",
        "FontColor": 32896,
        "FontSize": 8,
        "FontStyle": "fsBold",
        "WordWrap": false,
        "HorizontalAlignment": 0,
        "VerticalAlignment": 0
      },
      "Conditional": {
        "BackgroundColor": 16777215,
        "FontName": "Segoe UI",
        "FontColor": 8421376,
        "FontSize": 8,
        "FontStyle": "",
        "WordWrap": false,
        "HorizontalAlignment": 0,
        "VerticalAlignment": 0
      }
    },
    "WatchSettings": {
      "ColumnHeadersVisible": false,
      "OnlyTrackChanges": true,
      "WatchHistoryVisible": true,
      "SyncWithSelection": true
    },
    "CallStackSettings": {
      "ColumnHeadersVisible": false
    },
    "LogLevelSettings": {
      "LogLevels": [
        {
          "Alias": "",
          "Color": 15793151,
          "Level": 0
        },
        {
          "Alias": "",
          "Color": 32768,
          "Level": 1
        },
        {
          "Alias": "",
          "Color": 32896,
          "Level": 2
        },
        {
          "Alias": "",
          "Color": 8454143,
          "Level": 3
        },
        {
          "Alias": "",
          "Color": 8453888,
          "Level": 4
        },
        {
          "Alias": "",
          "Color": 65535,
          "Level": 5
        },
        {
          "Alias": "",
          "Color": 8388736,
          "Level": 8
        },
        {
          "Alias": "",
          "Color": 536870911,
          "Level": 7
        },
        {
          "Alias": "",
          "Color": 8454143,
          "Level": 8
        },
        {
          "Alias": "",
          "Color": 536870911,
          "Level": 9
        },
        {
          "Alias": "",
          "Color": 65535,
          "Level": 10
        },
        {
          "Alias": "",
          "Color": 8421376,
          "Level": 11
        },
        {
          "Alias": "",
          "Color": 536870911,
          "Level": 12
        },
        {
          "Alias": "",
          "Color": 65535,
          "Level": 13
        },
        {
          "Alias": "",
          "Color": 65535,
          "Level": 14
        },
        {
          "Alias": "",
          "Color": 536870911,
          "Level": 15
        },
        {
          "Alias": "",
          "Color": 536870911,
          "Level": 16
        },
        {
          "Alias": "",
          "Color": 536870911,
          "Level": 17
        },
        {
          "Alias": "",
          "Color": 536870911,
          "Level": 18
        },
        {
          "Alias": "",
          "Color": 536870911,
          "Level": 19
        },
        {
          "Alias": "",
          "Color": 536870911,
          "Level": 20
        },
        {
          "Alias": "",
          "Color": 536870911,
          "Level": 21
        },
        {
          "Alias": "",
          "Color": 536870911,
          "Level": 22
        },
        {
          "Alias": "",
          "Color": 536870911,
          "Level": 23
        },
        {
          "Alias": "",
          "Color": 536870911,
          "Level": 24
        },
        {
          "Alias": "",
          "Color": 536870911,
          "Level": 25
        },
        {
          "Alias": "",
          "Color": 536870911,
          "Level": 26
        },
        {
          "Alias": "",
          "Color": 536870911,
          "Level": 27
        },
        {
          "Alias": "",
          "Color": 536870911,
          "Level": 28
        },
        {
          "Alias": "",
          "Color": 536870911,
          "Level": 29
        },
        {
          "Alias": "",
          "Color": 536870911,
          "Level": 30
        },
        {
          "Alias": "",
          "Color": 536870911,
          "Level": 31
        }
      ]
    }
  }
  ''';
{$ENDREGION}

{$REGION 'EXAMPLE_INI_DOCUMENT'}
  EXAMPLE_INI_DOCUMENT = '''
  ; -------------------------------
  ; Application Configuration File
  ; -------------------------------

  [General]
  AppName=MyCoolApp
  Version=1.2.3
  LicenseAccepted=True

  [Window]
  Width=1024
  Height=768
  Fullscreen=False
  PositionX=100
  PositionY=200

  [UserSettings]
  Username=guest_user
  Language=en
  Theme=dark
  ShowTipsOnStartup=True

  [RecentFiles]
  File1=C:\Projects\App\main.dpr
  File2=D:\Media\song.mp3
  File3=C:\Temp\todo.txt

  [Paths]
  DataDir=C:\ProgramData\MyCoolApp\
  LogDir=C:\Logs\
  BackupDir=D:\Backups\MyCoolApp\

  [Advanced]
  EnableDebugMode=1
  MaxThreads=8
  Timeout=30
  CustomOptions="fast;safe;verbose"

  [Network]
  ServerIP=192.168.1.50
  ServerPort=8080
  UseSSL=true
  ProxyEnabled=false
  ProxyAddress=
  ''';


{$ENDREGION}

implementation

end.
