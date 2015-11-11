program DDuce.Demos;

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  Demo.Contact in 'Demo.Contact.pas',
  Demo.Data in 'Demo.Data.pas' {dmData: TDataModule},
  Demo.DDuce.DBGridView in 'Demo.DDuce.DBGridView.pas' {frmDBGridView},
  Demo.DDuce.DynamicRecord in 'Demo.DDuce.DynamicRecord.pas' {frmDynamicRecords},
  Demo.DDuce.PropertyInspector in 'Demo.DDuce.PropertyInspector.pas' {frmPropertyInspector},
  Demo.DDuce.Reflect in 'Demo.DDuce.Reflect.pas' {frmReflect},
  Demo.DDuce.ScopedReference in 'Demo.DDuce.ScopedReference.pas' {frmScopedReferences},
  Demo.DDuce.XMLTree in 'Demo.DDuce.XMLTree.pas' {frmXMLTree},
  Demo.Factories in 'Demo.Factories.pas',
  Demo.MainForm in 'Demo.MainForm.pas' {frmMainMenu},
  Demo.Manager in 'Demo.Manager.pas',
  Demo.Registration in 'Demo.Registration.pas',
  Demo.Utils in 'Demo.Utils.pas',
  Demo.DDuce.Logger in 'Demo.DDuce.Logger.pas' {frmLogger},
  Demo.DDuce.Inspector in 'Demo.DDuce.Inspector.pas' {frmInspector},
  Demo.DDuce.GridView in 'Demo.DDuce.GridView.pas' {frmGridView},
  DDuce.DynamicRecord in '..\Source\DDuce.DynamicRecord.pas',
  DDuce.Logger in '..\Source\DDuce.Logger.pas',
  DDuce.RandomData in '..\Source\DDuce.RandomData.pas',
  DDuce.Reflect in '..\Source\DDuce.Reflect.pas',
  DDuce.ScopedReference in '..\Source\DDuce.ScopedReference.pas',
  DDuce.Components.DBGridView in '..\Source\Components\DDuce.Components.DBGridView.pas',
  DDuce.Components.GridView in '..\Source\Components\DDuce.Components.GridView.pas',
  DDuce.Components.Inspector in '..\Source\Components\DDuce.Components.Inspector.pas',
  DDuce.Components.LogTree in '..\Source\Components\DDuce.Components.LogTree.pas',
  DDuce.Components.PropertyInspector.CollectionEditor in '..\Source\Components\DDuce.Components.PropertyInspector.CollectionEditor.pas' {frmCollectionEditor},
  DDuce.Components.PropertyInspector in '..\Source\Components\DDuce.Components.PropertyInspector.pas',
  DDuce.Components.PropertyInspector.StringsEditor in '..\Source\Components\DDuce.Components.PropertyInspector.StringsEditor.pas' {StringsEditorDialog},
  DDuce.Components.XMLTree.Editors in '..\Source\Components\DDuce.Components.XMLTree.Editors.pas',
  DDuce.Components.XMLTree.NodeAttributes in '..\Source\Components\DDuce.Components.XMLTree.NodeAttributes.pas',
  DDuce.Components.XMLTree in '..\Source\Components\DDuce.Components.XMLTree.pas',
  DSharp.Bindings.Collections in '..\Source\Dependencies\DSharp\DSharp.Bindings.Collections.pas',
  DSharp.Bindings.CollectionView in '..\Source\Dependencies\DSharp\DSharp.Bindings.CollectionView.pas',
  DSharp.Bindings.Notifications in '..\Source\Dependencies\DSharp\DSharp.Bindings.Notifications.pas',
  DSharp.Collections.ObservableCollection in '..\Source\Dependencies\DSharp\DSharp.Collections.ObservableCollection.pas',
  DSharp.Core.Collections in '..\Source\Dependencies\DSharp\DSharp.Core.Collections.pas',
  DSharp.Core.DataTemplates.Default in '..\Source\Dependencies\DSharp\DSharp.Core.DataTemplates.Default.pas',
  DSharp.Core.DataTemplates in '..\Source\Dependencies\DSharp\DSharp.Core.DataTemplates.pas',
  DSharp.Core.DependencyProperty in '..\Source\Dependencies\DSharp\DSharp.Core.DependencyProperty.pas',
  DSharp.Core.Expressions in '..\Source\Dependencies\DSharp\DSharp.Core.Expressions.pas',
  DSharp.Core.Framework in '..\Source\Dependencies\DSharp\DSharp.Core.Framework.pas',
  DSharp.Core.PropertyChangedBase in '..\Source\Dependencies\DSharp\DSharp.Core.PropertyChangedBase.pas',
  DSharp.Core.Reflection in '..\Source\Dependencies\DSharp\DSharp.Core.Reflection.pas',
  DSharp.Core.Utils in '..\Source\Dependencies\DSharp\DSharp.Core.Utils.pas',
  DSharp.Windows.ColumnDefinitions.ControlTemplate in '..\Source\Dependencies\DSharp\DSharp.Windows.ColumnDefinitions.ControlTemplate.pas',
  DSharp.Windows.ColumnDefinitions in '..\Source\Dependencies\DSharp\DSharp.Windows.ColumnDefinitions.pas',
  DSharp.Windows.ControlTemplates in '..\Source\Dependencies\DSharp\DSharp.Windows.ControlTemplates.pas',
  DSharp.Windows.CustomPresenter in '..\Source\Dependencies\DSharp\DSharp.Windows.CustomPresenter.pas',
  DSharp.Windows.CustomPresenter.Types in '..\Source\Dependencies\DSharp\DSharp.Windows.CustomPresenter.Types.pas',
  DSharp.Windows.TreeViewPresenter in '..\Source\Dependencies\DSharp\DSharp.Windows.TreeViewPresenter.pas',
  NativeXml in '..\Source\Dependencies\NativeXml\NativeXml.pas',
  NativeXmlCodepages in '..\Source\Dependencies\NativeXml\NativeXmlCodepages.pas',
  NativeXmlNodes in '..\Source\Dependencies\NativeXml\NativeXmlNodes.pas',
  sdDebug in '..\Source\Dependencies\NativeXml\sdDebug.pas',
  sdStreams in '..\Source\Dependencies\NativeXml\sdStreams.pas',
  sdStringTable in '..\Source\Dependencies\NativeXml\sdStringTable.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  RegisterDemos;
  if DemoManager.ItemList.Count = 1 then
  begin
    DemoManager.Execute(DemoManager.ItemList.Single);
  end
  else
  begin
    Application.CreateForm(TfrmMainMenu, frmMainMenu);
  end;
  Application.Run;
end.
