program DDuceDemos;

uses
  Vcl.Forms,
  DDuce.Demos.XMLTree in 'DDuce.Demos.XMLTree.pas' {frmXMLTree},
  DDuce.Demos.DynamicRecord in 'DDuce.Demos.DynamicRecord.pas' {frmDynamicRecords},
  DDuce.Demos.ScopedReferences in 'DDuce.Demos.ScopedReferences.pas' {frmScopedReferences},
  DDuce.Demos.ListDataSet in 'DDuce.Demos.ListDataSet.pas' {frmListDataSet},
  DDuce.Demos.MainForm in 'DDuce.Demos.MainForm.pas' {frmMainMenu},
  DDuce.ScopedReference in '..\Source\DDuce.ScopedReference.pas',
  DDuce.DynamicRecord in '..\Source\DDuce.DynamicRecord.pas',
  DDuce.Logger in '..\Source\DDuce.Logger.pas',
  DDuce.Reflect in '..\Source\DDuce.Reflect.pas',
  DDuce.Components.XMLTree in '..\Source\Components\DDuce.Components.XMLTree.pas',
  DDuce.Components.DBGridView in '..\Source\Components\DDuce.Components.DBGridView.pas',
  DDuce.Components.GridView in '..\Source\Components\DDuce.Components.GridView.pas',
  DDuce.Components.Inspector in '..\Source\Components\DDuce.Components.Inspector.pas',
  DDuce.Components.ListDataSet in '..\Source\Components\DDuce.Components.ListDataSet.pas',
  DDuce.Components.PropertyInspector.CollectionEditor in '..\Source\Components\DDuce.Components.PropertyInspector.CollectionEditor.pas' {frmCollectionEditor},
  DDuce.Components.PropertyInspector in '..\Source\Components\DDuce.Components.PropertyInspector.pas',
  DDuce.Components.PropertyInspector.StringsEditor in '..\Source\Components\DDuce.Components.PropertyInspector.StringsEditor.pas' {StringsEditorDialog},
  DDuce.Components.VirtualDataSet in '..\Source\Components\DDuce.Components.VirtualDataSet.pas',
  DDuce.Demos.Contact in 'DDuce.Demos.Contact.pas',
  DDuce.Demos.Data in 'DDuce.Demos.Data.pas' {dmData: TDataModule},
  DDuce.Demos.Utils in 'DDuce.Demos.Utils.pas',
  DDuce.Demos.Helpers in 'DDuce.Demos.Helpers.pas',
  DDuce.Demos.Registration in 'DDuce.Demos.Registration.pas',
  DDuce.Demos.DemoManager in 'DDuce.Demos.DemoManager.pas',
  DDuce.Components.LogTree in '..\Source\Components\DDuce.Components.LogTree.pas',
  DDuce.Demos.DBGridView in 'DDuce.Demos.DBGridView.pas' {frmDBGridView},
  DDuce.RandomData in '..\Source\DDuce.RandomData.pas',
  Vcl.Themes,
  Vcl.Styles,
  DDuce.Components.XMLTree.NodeAttributes in '..\Source\Components\DDuce.Components.XMLTree.NodeAttributes.pas',
  DDuce.Components.XMLTree.Editors in '..\Source\Components\DDuce.Components.XMLTree.Editors.pas',
  DDuce.Components.VirtualDBGrid in '..\Source\Components\DDuce.Components.VirtualDBGrid.pas',
  DDuce.Demos.VirtualDBGrid in 'DDuce.Demos.VirtualDBGrid.pas' {frmVirtualDBGrid};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := DebugHook > 0;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Smokey Quartz Kamri');
  RegisterDemos;
  if DemoManager.ItemList.Count = 1 then
  begin
    DemoManager.Execute(DemoManager.ItemList.First.AsObject);
  end
  else
  begin
    Application.Title := 'DDuce Demos';
    Application.CreateForm(TfrmMainMenu, frmMainMenu);
  end;
  Application.Run;
end.
