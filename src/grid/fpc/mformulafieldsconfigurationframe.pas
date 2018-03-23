unit mformulafieldsconfigurationframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BufDataset, db, memds, FileUtil, Forms, Controls, DBGrids, Grids,
  StdCtrls, ExtCtrls,Dialogs,
  mVirtualDatasetFormulas;

resourcestring
  SErrorMessageCaption = 'Error';
  SErrorEmptyName = 'Wrong name: name cannot be blank or null.';
  SErrorDuplicateName = 'Wrong name: cannot assign the same name to two or more fields.';
  SErrorWrongType = 'Wrong type: type must be STRING or DOUBLE or DATE.';
  SErrorWrongSize = 'Wrong size: size must have a value between 1 and 1000.';
  SErrorWrongFormula = 'Wrong formula: formula cannot be blank or null.';

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
    FFormulas : TmFormulaFields;
    procedure OnSelectEditor (Sender: TObject; Column: TColumn; var Editor: TWinControl);
    procedure OnEditButtonClick (Sender : TObject);
    procedure OnEditingDone(Sender : TObject);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    function Check : boolean;
    procedure Init (aFormulas : TmFormulaFields);
    procedure UpdateFormulaFields;

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
      (Editor as TCustomComboBox).Items.CommaText:= 'DOUBLE,STRING,DATE';
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
    FDataset.Edit;
    FDataset.FieldByName('FldName').AsString := StringReplace(Uppercase(Trim(FDataset.FieldByName('FldName').AsString)), ' ', '_', [rfReplaceAll]);
    FDataset.Post;
  end
  else
  if (FGrid.SelectedColumn.FieldName = 'FldSize') or (FGrid.SelectedColumn.FieldName = 'FldType') then
  begin
    if (FDataset.FieldByName('FldType').AsString = 'DOUBLE') or (FDataset.FieldByName('FldType').AsString = 'DATE') then
    begin
      FDataset.Edit;
      FDataset.FieldByName('FldSize').Clear;
      FDataset.Post;
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

function TFormulaFieldsConfFrame.Check: boolean;
var
  tmpString, tmpType : String;
  tmpNames : TStringList;
begin
  tmpNames := TStringList.Create;
  try
    FDataset.DisableControls;
    try
      FDataset.First;
      while not FDataset.Eof do
      begin
        tmpString := Uppercase(Trim(FDataset.FieldByName('FldName').AsString));
        if tmpString  = '' then
        begin
          MessageDlg(SErrorMessageCaption, SErrorEmptyName, mtInformation, [mbOK], 0);
          Result := false;
          exit;
        end;
        if tmpNames.IndexOf(tmpString) >= 0 then
        begin
          MessageDlg(SErrorMessageCaption, SErrorDuplicateName, mtInformation, [mbOK], 0);
          Result := false;
          exit;
        end;

        tmpType := Uppercase(Trim(FDataset.FieldByName('FldType').AsString));
        if (tmpType <> 'DOUBLE') and (tmpType <> 'STRING') and (tmpType <> 'DATE') then
        begin
          MessageDlg(SErrorMessageCaption, SErrorWrongType, mtInformation, [mbOK], 0);
          Result := false;
          exit;
        end;
        if tmpType = 'STRING' then
        begin
          if (FDataset.FieldByName('FldSize').IsNull) or (FDataset.FieldByName('FldSize').AsInteger <= 0) or (FDataset.FieldByName('FldSize').AsInteger > 1000) then
          begin
            MessageDlg(SErrorMessageCaption, SErrorWrongSize,mtInformation, [mbOK], 0);
            Result := false;
            exit;
          end;
        end;

        if (FDataset.FieldByName('FldFormula').IsNull) or (trim(FDataset.FieldByName('FldFormula').AsString) = '') then
        begin
          MessageDlg(SErrorMessageCaption, SErrorWrongFormula, mtInformation, [mbOK], 0);
          Result := false;
          exit;
        end;

        tmpNames.Add(tmpString);

        FDataset.Next;
      end;
    finally
      FDataset.EnableControls;
    end;
  finally
    tmpNames.Free;
  end;
  Result := true;
end;

procedure TFormulaFieldsConfFrame.Init(aFormulas: TmFormulaFields);
var
  i : integer;
begin
  FDataset.DisableControls;
  try
    FDataset.Clear(false);
    for i := 0 to aFormulas.Count - 1 do
    begin
      FDataset.Append;
      FDataset.FieldByName('FldName').AsString := aFormulas.Get(i).Name;
      if aFormulas.Get(i).DataType = fftString then
      begin
        FDataset.FieldByName('FldType').AsString := 'STRING';
        FDataset.FieldByName('FldSize').AsInteger:= aFormulas.Get(i).Size;
      end
      else if aFormulas.Get(i).DataType = fftFloat then
        FDataset.FieldByName('FldType').AsString := 'DOUBLE'
      else
        FDataset.FieldByName('FldType').AsString := 'DATE';
      FDataset.FieldByName('FldFormula').AsString := aFormulas.Get(i).Formula;
      FDataset.Post;
    end;
  finally
    FDataset.EnableControls;
  end;
  FFormulas := aFormulas;
end;

procedure TFormulaFieldsConfFrame.UpdateFormulaFields;
var
  i : integer;
  tmpNames, tmpToBeDeleted : TStringList;
  tmpString : String;
  tmpFormula : TmFormulaField;

begin
  if Self.Check then
  begin
    tmpNames := TStringList.Create;
    try
      FDataset.DisableControls;
      try
        FDataset.First;
        while not FDataset.Eof do
        begin
          tmpString := UpperCase(Trim(FDataset.FieldByName('FldName').AsString));
          tmpNames.Add(tmpString);

          tmpFormula := FFormulas.FindByName(tmpString);
          if not Assigned(tmpFormula) then
            tmpFormula := FFormulas.Add;
          tmpFormula.Name:= tmpString;
          if FDataset.FieldByName('FldType').AsString = 'DOUBLE' then
            tmpFormula.DataType:= fftFloat
          else if FDataset.FieldByName('FldType').AsString = 'DATE' then
            tmpFormula.DataType:= fftDateTime
          else
          begin
            tmpFormula.DataType:= fftString;
            tmpFormula.Size:= FDataset.FieldByName('FldSize').AsInteger;
          end;

          tmpFormula.Formula:= trim(FDataset.FieldByName('FldFormula').AsString);

          FDataset.Next;
        end;

        tmpToBeDeleted := TStringList.Create;
        try
          for i := 0 to FFormulas.Count - 1 do
          begin
            if tmpNames.IndexOf(FFormulas.Get(i).Name) < 0 then
              tmpToBeDeleted.Add(FFormulas.Get(i).Name);
          end;
          for i := 0 to tmpToBeDeleted.Count - 1 do
          begin
            FFormulas.Delete(tmpToBeDeleted.Strings[i]);
          end;

        finally
          tmpToBeDeleted.Free;
        end;
      finally
        FDataset.EnableControls;
      end;
    finally
      tmpNames.Free;
    end;
  end;
end;

end.

