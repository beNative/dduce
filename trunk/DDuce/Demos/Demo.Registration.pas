{
  Copyright (C) 2013-2014 Tim Sinaeve tim.sinaeve@gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit Demo.Registration;

interface

procedure RegisterDemos;

implementation

uses
  SysUtils,

  Demo.DDuce.ListDataSet,
  Demo.DDuce.ScopedReferences,
  Demo.DDuce.DynamicRecord,
  Demo.DDuce.XMLTree,
  Demo.DDuce.VirtualDBGrid,
  Demo.DDuce.DBGridView,
  Demo.DDuce.PropertyInspector,
  Demo.DDuce.Reflect,
  Demo.Manager;

{$REGION 'interfaced routines'}
procedure RegisterDemos;
begin
  DemoManager.Register(TfrmListDataSet, 'ListDataSet');
  DemoManager.Register(TfrmXMLTree, 'XMLTree');
  DemoManager.Register(TfrmReflect, 'Reflect');
  DemoManager.Register(TfrmVirtualDBGrid, 'VirtualDBGrid');
  DemoManager.Register(TfrmScopedReferences, 'Scoped references');
//  DemoManager.Register(TfrmDBGridView, 'DBGridView');
//  DemoManager.Register(TfrmDynamicRecords, 'Dynamic records');
  DemoManager.Register(TfrmPropertyInspector, 'Property Inspector');
end;
{$ENDREGION}

end.

