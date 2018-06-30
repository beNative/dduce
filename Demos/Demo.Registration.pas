{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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
  Demo.DDuce.ScopedReference,
  Demo.DDuce.XMLTree,
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

  Demo.Manager;

{$REGION 'interfaced routines'}
procedure RegisterDemos;
begin
  DemoManager.Register(TfrmLogger, 'Logger');
  Exit;
  DemoManager.Register(TfrmScopedReferences, 'Scoped references');
  DemoManager.Register(TfrmDBGridView, 'DBGridView');
  DemoManager.Register(TfrmPropertyInspector, 'Property Inspector');
  DemoManager.Register(TfrmReflect, 'Reflect');
  DemoManager.Register(TfrmEditor, 'Editor');


  DemoManager.Register(TfrmDynamicRecords, 'Dynamic record');
  DemoManager.Register(TfrmInspector, 'Inspector');
  DemoManager.Register(TfrmGridView, 'GridView');
  DemoManager.Register(TfrmVirtualTrees, 'VirtualTrees');
  DemoManager.Register(TfrmValueListDemo, 'ValueList');
  // not working yet
  //DemoManager.Register(TfrmXMLTree, 'XMLTree');
end;
{$ENDREGION}

end.

