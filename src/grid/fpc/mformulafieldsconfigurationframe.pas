// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mformulafieldsconfigurationframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BufDataset, FileUtil, Forms, Controls, DBGrids, Grids,
  StdCtrls, ExtCtrls,Dialogs, DB,

  mVirtualDatasetFormulas, mFields;

resourcestring
  SErrorMessageCaption = 'Error';
  SErrorEmptyName = 'Wrong name: name cannot be blank or null.';
  SErrorDuplicateName = 'Wrong name: cannot assign the same name to two or more fields.';
  SErrorWrongType = 'Wrong type: type must be STRING or DOUBLE or DATE.';
  SErrorWrongSize = 'Wrong size: size must have a value between 1 and 1000.';
  SErrorWrongFormula = 'Wrong formula: formula cannot be blank or null.';
  SErrorStillEditingFormula = 'Please save or discard formula.';

  SBtnSaveFormula = 'Save';
  SBtnDiscardFormula = 'Discard';

type

  { TFormulaFieldsConfFrame }

  TFormulaFieldsConfFrame = class(TFrame)
    AddButton: TButton;
    RemoveButton: TButton;
    TopPanel: TPanel;
    procedure AddButtonClick(Sender: TObject);
    procedure RemoveButtonClick(Sender: TObject);
  private
    const IDX_NAME = 0;
    const IDX_TYPE = 1;
    const IDX_SIZE = 2;
    const IDX_FORMULA = 3;
  private
    FGridPanel : TPanel;
    FGrid : TStringGrid;
    FFormulas : TmFormulaFields;
    FFieldsList : TStringList;
    FSomethingChanged : boolean;
    procedure OnSelectEditor (Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
    procedure OnEditButtonClick (Sender: TObject; aCol, aRow: Integer);
    procedure OnEditingDone(Sender : TObject);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    function Check : boolean;
    procedure Init (aFormulas : TmFormulaFields; const aFields : TmFields); overload;
    procedure Init (aFormulas : TmFormulaFields; const aFields : TStringList); overload;
    procedure UpdateFormulaFields;

    property SomethingChanged : boolean read FSomethingChanged;
  end;

implementation

uses
  Graphics,
  mUtility, mMathUtility, mFormulaEditForm;

{$R *.lfm}

{ TFormulaFieldsConfFrame }

procedure TFormulaFieldsConfFrame.AddButtonClick(Sender: TObject);
begin
  FGrid.InsertRowWithValues(FGrid.RowCount, ['NEWFIELD', 'DOUBLE', '', '1']);
  FSomethingChanged:= true;
end;

procedure TFormulaFieldsConfFrame.RemoveButtonClick(Sender: TObject);
begin
  if FGrid.RowCount = 0 then
    exit;

  if FGrid.Row >= 0 then
  begin
    FGrid.DeleteRow(FGrid.Row);
    FSomethingChanged:= true;
  end;
end;

procedure TFormulaFieldsConfFrame.OnSelectEditor (Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
begin
  if aCol = IDX_TYPE then
  begin
    if (Editor is TCustomComboBox) then
    begin
      (Editor as TCustomComboBox).Style:= csDropDownList;
      (Editor as TCustomComboBox).Items.CommaText:= 'DOUBLE,STRING,DATE';
    end;
  end;
end;

procedure TFormulaFieldsConfFrame.OnEditButtonClick(Sender: TObject; aCol, aRow: Integer);
var
  dlg : TFormulaEditForm;
begin
  if FGrid.SelectedColumn.Index = IDX_FORMULA then
  begin
    dlg := TFormulaEditForm.Create(Self);
    try
      dlg.Init(trim(FGrid.Cells[IDX_FORMULA, aRow]), FFieldsList);
      if dlg.ShowModal = mrOK then
      begin
        FGrid.Cells[IDX_FORMULA, aRow] := trim(dlg.GetFormula);
        FSomethingChanged:= true;
      end;
    finally
      dlg.Free;
    end;
  end;
end;

procedure TFormulaFieldsConfFrame.OnEditingDone(Sender: TObject);
var
  newSize : integer;
begin
  FSomethingChanged:= true;
  if FGrid.SelectedColumn.Index = IDX_NAME then
  begin
    FGrid.Cells[IDX_NAME, FGrid.Row] := StringReplace(UpperCase(Trim(FGrid.Cells[IDX_NAME, FGrid.Row])), ' ', '_', [rfReplaceAll]);
  end
  else
  if (FGrid.SelectedColumn.Index = IDX_SIZE) or (FGrid.SelectedColumn.Index = IDX_TYPE) then
  begin
    if (FGrid.Cells[IDX_TYPE, FGrid.Row] = 'DOUBLE') or (FGrid.Cells[IDX_TYPE, FGrid.Row] = 'DATE') then
    begin
      FGrid.Cells[IDX_SIZE, FGrid.Row] := '';
    end
    else
    begin
      if (not TryToConvertToInteger(FGrid.Cells[IDX_SIZE, FGrid.Row], newSize)) or (newSize <= 0) or (newSize > 1000) then
        FGrid.Cells[IDX_SIZE, FGrid.Row] := '';
    end;
  end;
end;

constructor TFormulaFieldsConfFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FSomethingChanged:= false;

  FFieldsList := TStringList.Create;

  FGridPanel := TPanel.Create(Self);
  FGridPanel.Parent := Self;
  FGridPanel.Align:= alClient;

  FGrid:= TStringGrid.Create(FGridPanel);
  FGrid.Parent := FGridPanel;
  FGrid.Align:= alClient;

  FGrid.Options := [goEditing,goTabs, goVertLine, goHorzLine, goDblClickAutoSize];
  FGrid.FixedCols:= 0;
  FGrid.RowCount:= 1;
  FGrid.FixedRows := 1;
  FGrid.AlternateColor:= clMoneyGreen;

  FGrid.OnSelectEditor:= @OnSelectEditor;
  FGrid.OnButtonClick:= @OnEditButtonClick;
  FGrid.OnEditingDone:=@OnEditingDone;
  with FGrid.Columns.Add do
  begin
    // ReadOnly := True;
    Title.Caption := 'Name';
    Width := 250;
  end;
  with FGrid.Columns.Add do
  begin
    Title.Caption := 'Type';
    Width := 100;
    ButtonStyle:= cbsPickList;
  end;
  with FGrid.Columns.Add do
  begin
    Title.Caption := 'Size';
    Width := 70;
  end;
  with FGrid.Columns.Add do
  begin
    Title.Caption := 'Formula';
    Width := 250;
    ButtonStyle:= cbsEllipsis;
  end;
  FGrid.RowCount:= 1;
end;

destructor TFormulaFieldsConfFrame.Destroy;
begin
  FFieldsList.Free;
  inherited Destroy;
end;

function TFormulaFieldsConfFrame.Check: boolean;
var
  tmpString, tmpType : String;
  tmpNames : TStringList;
  i, newSize : integer;
begin
  tmpNames := TStringList.Create;
  try
    for i := 1 to FGrid.RowCount - 1 do
    begin
      tmpString := Uppercase(Trim(FGrid.Cells[IDX_NAME, i]));
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

      tmpType := Uppercase(Trim(FGrid.Cells[IDX_TYPE, i]));
      if (tmpType <> 'DOUBLE') and (tmpType <> 'STRING') and (tmpType <> 'DATE') then
      begin
        MessageDlg(SErrorMessageCaption, SErrorWrongType, mtInformation, [mbOK], 0);
        Result := false;
        exit;
      end;
      if tmpType = 'STRING' then
      begin
        if (not (TryToConvertToInteger(FGrid.Cells[IDX_SIZE, i], newSize))) or (newSize <= 0) or (newSize > 1000) then
        begin
          MessageDlg(SErrorMessageCaption, SErrorWrongSize,mtInformation, [mbOK], 0);
          Result := false;
          exit;
        end;
      end;

      if (trim(FGrid.Cells[IDX_FORMULA, i]) = '') then
      begin
        MessageDlg(SErrorMessageCaption, SErrorWrongFormula, mtInformation, [mbOK], 0);
        Result := false;
        exit;
      end;

      tmpNames.Add(tmpString);

    end;
  finally
    tmpNames.Free;
  end;
  Result := true;
end;

procedure TFormulaFieldsConfFrame.Init(aFormulas: TmFormulaFields; const aFields : TmFields);
var
  i : integer;
  tmpList : TStringList;
begin
  tmpList := TStringList.Create;
  try
    for i := 0 to aFields.Count - 1 do
      tmpList.Add(aFields.Get(i).FieldName);

    Self.Init(aFormulas, tmpList);
  finally
    tmpList.Free;
  end;
end;

procedure TFormulaFieldsConfFrame.Init(aFormulas: TmFormulaFields; const aFields: TStringList);
var
  i : integer;
begin
  for i := 0 to aFormulas.Count - 1 do
  begin
    FGrid.InsertColRow(false, i + 1);
    FGrid.Cells[IDX_NAME, i + 1] := aFormulas.Get(i).Name;
    if aFormulas.Get(i).DataType = fftString then
    begin
      FGrid.Cells[IDX_TYPE, i + 1] := 'STRING';
      FGrid.Cells[IDX_SIZE, i + 1] := IntToStr(aFormulas.Get(i).Size);
    end
    else if aFormulas.Get(i).DataType = fftFloat then
      FGrid.Cells[IDX_TYPE, i + 1] := 'DOUBLE'
    else
      FGrid.Cells[IDX_TYPE, i + 1] := 'DATE';
    FGrid.Cells[IDX_FORMULA, i + 1] := aFormulas.Get(i).Formula;
    FGrid.Objects[IDX_NAME, i+1] := aFormulas.Get(i);
  end;

  for i := 0 to aFields.Count - 1 do
  begin
    if (not Assigned(aFormulas.FindByName(aFields.Strings[i]))) and (not IsSystemField(aFields.Strings[i])) then
        FFieldsList.Add(aFields.Strings[i]);
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
      for i := 1 to FGrid.RowCount - 1 do
      begin
        tmpString := UpperCase(Trim(FGrid.Cells[IDX_NAME, i]));
        tmpNames.Add(tmpString);

        tmpFormula := FFormulas.FindByName(tmpString);
        if not Assigned(tmpFormula) then
          tmpFormula := FFormulas.Add;
        tmpFormula.Name:= tmpString;
        if FGrid.Cells[IDX_TYPE, i] = 'DOUBLE' then
          tmpFormula.DataType:= fftFloat
        else if FGrid.Cells[IDX_TYPE, i] = 'DATE' then
          tmpFormula.DataType:= fftDateTime
        else
        begin
          tmpFormula.DataType:= fftString;
          tmpFormula.Size:= StrToInt(FGrid.Cells[IDX_SIZE, i]);
        end;

        tmpFormula.Formula:= trim(FGrid.Cells[IDX_FORMULA, i]);
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
      tmpNames.Free;
    end;
  end;
end;

end.

