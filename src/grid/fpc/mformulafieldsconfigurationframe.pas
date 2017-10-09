unit mformulafieldsconfigurationframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BufDataset, db, memds, FileUtil, Forms, Controls, DBGrids, Grids,
  StdCtrls, ExtCtrls;

type

  { TFormulaFieldsConfFrame }

  TFormulaFieldsConfFrame = class(TFrame)
    AddButton: TButton;
    RemoveButton: TButton;
    TopPanel: TPanel;
    procedure AddButtonClick(Sender: TObject);
    procedure RemoveButtonClick(Sender: TObject);
  private
    FDataSource: TDataSource;
    FDataset: TMemDataset;
    FGrid : TDBGrid;
    procedure OnSelectEditor (Sender: TObject; Column: TColumn; var Editor: TWinControl);
    procedure OnEditButtonClick (Sender : TObject);
    procedure OnEditingDone(Sender : TObject);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

uses
  mDBGrid;

{$R *.lfm}

{ TFormulaFieldsConfFrame }

procedure TFormulaFieldsConfFrame.AddButtonClick(Sender: TObject);
begin
  FDataset.DisableControls;
  try
    FDataset.Append;
    FDataset.FieldByName('FldName').AsString := 'NEWFIELD';
    FDataset.FieldByName('FldType').AsString := 'DOUBLE';
    FDataset.FieldByName('FldFormula').AsString := '1';
    FDataset.Post;
  finally
    FDataset.EnableControls;
  end;
end;

procedure TFormulaFieldsConfFrame.RemoveButtonClick(Sender: TObject);
begin
  if FDataset.IsEmpty then
    exit;

  FDataset.DisableControls;
  try
    FDataset.Delete;
  finally
    FDataset.EnableControls;
  end;
end;

procedure TFormulaFieldsConfFrame.OnSelectEditor(Sender: TObject; Column: TColumn; var Editor: TWinControl);
begin
  if Column.FieldName = 'FldType' then
  begin
    if (Editor is TCustomComboBox) then
    begin
      (Editor as TCustomComboBox).Style:= csDropDownList;
      (Editor as TCustomComboBox).Items.CommaText:= 'DOUBLE,STRING';
    end;
  end;
end;

procedure TFormulaFieldsConfFrame.OnEditButtonClick(Sender: TObject);
begin
  if FGrid.SelectedColumn.FieldName = 'FldFormula' then
  begin
    // todo
  end;
end;

procedure TFormulaFieldsConfFrame.OnEditingDone(Sender: TObject);
begin
  if FGrid.SelectedColumn.FieldName = 'FldName' then
  begin
    FDataset.DisableControls;
    try
      FDataset.Edit;
      FDataset.FieldByName('FldName').AsString := StringReplace(Uppercase(Trim(FDataset.FieldByName('FldName').AsString)), ' ', '_', [rfReplaceAll]);
      FDataset.Post;
    finally
      FDataset.EnableControls;
    end;
  end
  else
  if (FGrid.SelectedColumn.FieldName = 'FldSize') or (FGrid.SelectedColumn.FieldName = 'FldType') then
  begin
    if FDataset.FieldByName('FldType').AsString = 'DOUBLE' then
    begin
      FDataset.DisableControls;
      try
        FDataset.Edit;
        FDataset.FieldByName('FldSize').Clear;
        FDataset.Post;
      finally
        FDataset.EnableControls;
      end;
    end;
  end;

end;

constructor TFormulaFieldsConfFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FDataSource := TDataSource.Create(Self);
  FDataset := TMemDataset.Create(Self);

  with FDataset.FieldDefs.AddFieldDef do
  begin
    Name := 'FldName';
    DataType:= ftString;
    Size := 100;
  end;
  with FDataset.FieldDefs.AddFieldDef do
  begin
    Name := 'FldType';
    DataType:= ftString;
    Size := 20;
  end;
  with FDataset.FieldDefs.AddFieldDef do
  begin
    Name := 'FldSize';
    DataType:= ftInteger;
  end;
  with FDataset.FieldDefs.AddFieldDef do
  begin
    Name := 'FldFormula';
    DataType:= ftString;
    Size := 500;
  end;
  FDataset.Active:= true;

  FDatasource.DataSet := FDataset;

  FGrid:= TmDBGrid.Create(Self);
  FGrid.Parent := Self;
  FGrid.Align:= alClient;

  FGrid.Options:= [dgEditing,dgTitles,dgIndicator,dgColumnResize,dgColumnMove,dgColLines,dgRowLines,dgTabs,dgAlwaysShowSelection,dgConfirmDelete,dgCancelOnExit];
  FGrid.OptionsExtra:=[dgeAutoColumns,dgeCheckboxColumn];
  FGrid.OnSelectEditor:= @OnSelectEditor;
  FGrid.OnEditButtonClick:= @OnEditButtonClick;
  FGrid.OnEditingDone:=@OnEditingDone;
  with FGrid.Columns.Add do
  begin
    // ReadOnly := True;
    Title.Caption := 'Name';
    Width := 250;
    FieldName := 'FldName';
  end;
  with FGrid.Columns.Add do
  begin
    Title.Caption := 'Type';
    FieldName := 'FldType';
    Width := 100;
    ButtonStyle:= cbsPickList;
  end;
  with FGrid.Columns.Add do
  begin
    Title.Caption := 'Size';
    Width := 70;
    FieldName := 'FldSize';
  end;
  with FGrid.Columns.Add do
  begin
    Title.Caption := 'Formula';
    FieldName := 'FldFormula';
    Width := 250;
    ButtonStyle:= cbsEllipsis;
  end;
  FGrid.DataSource := FDataSource;
end;

end.

