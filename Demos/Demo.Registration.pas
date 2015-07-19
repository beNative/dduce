{
  Copyright (C) 2013-2015 Tim Sinaeve tim.sinaeve@gmail.com

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
  Demo.DDuce.ListDataSet,
  Demo.DDuce.ScopedReference,
  Demo.DDuce.XMLTree,
  Demo.DDuce.VirtualDBGrid,
  Demo.DDuce.PropertyInspector,
  Demo.DDuce.Reflect,
  Demo.DDuce.DynamicRecord,

  Demo.Manager;

{$REGION 'interfaced routines'}
procedure RegisterDemos;
begin
  DemoManager.Register(TfrmListDataSet, 'ListDataSet');
  DemoManager.Register(TfrmXMLTree, 'XMLTree');
  DemoManager.Register(TfrmReflect, 'Reflect');
  DemoManager.Register(TfrmVirtualDBGrid, 'VirtualDBGrid');
  DemoManager.Register(TfrmScopedReferences, 'Scoped references');
  //DemoManager.Register(TfrmDBGridView, 'DBGridView');
  DemoManager.Register(TfrmDynamicRecords, 'Dynamic record');
  DemoManager.Register(TfrmPropertyInspector, 'Property Inspector');
end;
{$ENDREGION}

end.
