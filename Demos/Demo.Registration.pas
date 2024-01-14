{
  Copyright (C) 2013-2024 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Demo.Registration;

interface

procedure RegisterDemos;

implementation

uses
  //Demo.DDuce.XMLTree,
  Demo.DDuce.JsonTree,
  Demo.DDuce.IniTree,
  Demo.DDuce.PropertyInspector,
  Demo.DDuce.Reflect,
  Demo.DDuce.DynamicRecord,
  Demo.DDuce.DBGridView,
  Demo.DDuce.Logger,
  Demo.DDuce.Inspector,
  Demo.DDuce.GridView,
  Demo.DDuce.Editor,
  Demo.DDuce.VirtualTrees,
  Demo.DDuce.ValueList,
  Demo.DDuce.VTNode,
  Demo.DDuce.EditList,
  Demo.DDuce.Dialogs,
  Demo.Manager;

{$REGION 'interfaced routines'}
procedure RegisterDemos;
begin
  DemoManager.Register(TfrmEditor, 'Editor');

  //Exit;
  DemoManager.Register(TfrmLogger, 'Logger');
  DemoManager.Register(TfrmGridView, 'GridView');
  DemoManager.Register(TfrmDBGridView, 'DBGridView');
  DemoManager.Register(TfrmValueListDemo, 'ValueList');
  DemoManager.Register(TfrmDynamicRecords, 'Dynamic record');
  DemoManager.Register(TfrmVirtualTrees, 'VirtualTrees factories');
  DemoManager.Register(TfrmVTNode, 'VTNode');
  DemoManager.Register(TfrmEditList, 'EditList');
  DemoManager.Register(TfrmDialogs, 'Dialogs');
  DemoManager.Register(TfrmReflect, 'Reflect');
  DemoManager.Register(TfrmJsonTree, 'JsonTree');
  DemoManager.Register(TfrmIniTree, 'IniTree');
//  DemoManager.Register(TfrmInspector, 'Inspector');
//  DemoManager.Register(TfrmPropertyInspector, 'Property Inspector');
  // not working yet
  //DemoManager.Register(TfrmXMLTree, 'XMLTree');
end;
{$ENDREGION}

end.

