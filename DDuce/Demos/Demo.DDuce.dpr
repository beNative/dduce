program Demo.DDuce;

uses
  Demo.DDuce.XMLTree in 'Demo.DDuce.XMLTree.pas' {frmXMLTree},
  Demo.DDuce.DynamicRecord in 'Demo.DDuce.DynamicRecord.pas' {frmDynamicRecords},
  Demo.DDuce.ScopedReference in 'Demo.DDuce.ScopedReference.pas' {frmScopedReferences},
  Demo.DDuce.ListDataSet in 'Demo.DDuce.ListDataSet.pas' {frmListDataSet},
  Demo.MainForm in 'Demo.MainForm.pas' {frmMainMenu},
  Demo.Contact in 'Demo.Contact.pas',
  Demo.Data in 'Demo.Data.pas' {dmData: TDataModule},
  Demo.Utils in 'Demo.Utils.pas',
  Demo.Helpers in 'Demo.Helpers.pas',
  Demo.Registration in 'Demo.Registration.pas',
  Demo.Manager in 'Demo.Manager.pas',
  Demo.DDuce.DBGridView in 'Demo.DDuce.DBGridView.pas' {frmDBGridView},
  DDuce.DynamicRecord in '..\Source\DDuce.DynamicRecord.pas',
  DDuce.Logger in '..\Source\DDuce.Logger.pas',
  DDuce.RandomData in '..\Source\DDuce.RandomData.pas',
  DDuce.Reflect in '..\Source\DDuce.Reflect.pas',
  DDuce.ScopedReference in '..\Source\DDuce.ScopedReference.pas',
  DDuce.Components.DBGridView in '..\Source\Components\DDuce.Components.DBGridView.pas',
  DDuce.Components.GridView in '..\Source\Components\DDuce.Components.GridView.pas',
  DDuce.Components.Inspector in '..\Source\Components\DDuce.Components.Inspector.pas',
  DDuce.Components.ListDataSet in '..\Source\Components\DDuce.Components.ListDataSet.pas',
  DDuce.Components.LogTree in '..\Source\Components\DDuce.Components.LogTree.pas',
  DDuce.Components.PropertyInspector.CollectionEditor in '..\Source\Components\DDuce.Components.PropertyInspector.CollectionEditor.pas' {frmCollectionEditor},
  DDuce.Components.PropertyInspector in '..\Source\Components\DDuce.Components.PropertyInspector.pas',
  DDuce.Components.PropertyInspector.StringsEditor in '..\Source\Components\DDuce.Components.PropertyInspector.StringsEditor.pas' {StringsEditorDialog},
  DDuce.Components.VirtualDataSet in '..\Source\Components\DDuce.Components.VirtualDataSet.pas',
  DDuce.Components.VirtualDBGrid in '..\Source\Components\DDuce.Components.VirtualDBGrid.pas',
  DDuce.Components.XMLTree.Editors in '..\Source\Components\DDuce.Components.XMLTree.Editors.pas',
  DDuce.Components.XMLTree.NodeAttributes in '..\Source\Components\DDuce.Components.XMLTree.NodeAttributes.pas',
  DDuce.Components.XMLTree in '..\Source\Components\DDuce.Components.XMLTree.pas',
  Demo.DDuce.PropertyInspector in 'Demo.DDuce.PropertyInspector.pas' {frmPropertyInspector},
  Demo.DDuce.VirtualDBGrid in 'Demo.DDuce.VirtualDBGrid.pas' {frmVirtualDBGrid},
  Demo.DDuce.Reflect in 'Demo.DDuce.Reflect.pas' {frmReflect},
  Demo.Factories in 'Demo.Factories.pas';

{$R *.res}

begin
//  ReportMemoryLeaksOnShutdown := DebugHook > 0;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  RegisterDemos;
  if DemoManager.ItemList.Count = 1 then
  begin
    DemoManager.Execute(DemoManager.ItemList[0].AsObject);
  end
  else
  begin
    Application.Title := 'DDuce Demos';
    Application.CreateForm(TfrmMainMenu, frmMainMenu);
  end;
  Application.Run;
end.
