// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mcelldecorationsconfigurationframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BufDataset, FileUtil, Forms, Controls, DBGrids, Grids,
  StdCtrls, ExtCtrls,Dialogs, DB,
  mCellDecorations, mFields;

resourcestring
  SErrorMessageCaption = 'Error';
  SErrorEmptyName = 'Wrong field name: field name cannot be blank or null.';
  //SErrorDuplicateName = 'Wrong name: cannot assign the same name to two or more fields.';
  //SErrorWrongType = 'Wrong type: type must be STRING or DOUBLE or DATE.';
  //SErrorWrongSize = 'Wrong size: size must have a value between 1 and 1000.';
  SErrorWrongCondition = 'Wrong condition: formula cannot be blank or null.';
  SErrorWrongDecoration = 'Wrong decoration: decoration cannot be empty.';
  SErrorStillEditingFormula = 'Please save or discard formula.';

  SBtnSaveFormula = 'Save';
  SBtnDiscardFormula = 'Discard';

  SColumnFieldName = 'Fieldname';
  SColumnCondition = 'Condition';
  SColumnDecoration = 'Decoration';

type

  { TCellDecorationsConfFrame }

  TCellDecorationsConfFrame = class(TFrame)
    AddButton: TButton;
    RemoveButton: TButton;
    TopPanel: TPanel;
    procedure AddButtonClick(Sender: TObject);
    procedure RemoveButtonClick(Sender: TObject);
  private
    const IDX_FIELDNAME = 0;
    const IDX_CONDITION = 1;
    const IDX_DECORATION = 2;
  private
    FGridPanel : TPanel;
    FGrid : TStringGrid;
    FCellDecorations, FOriginalCellDecorations : TmCellDecorations;
    FFields : TStringList;
    procedure OnSelectEditor (Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
    procedure OnEditButtonClick (Sender: TObject; aCol, aRow: Integer);
    procedure OnEditingDone(Sender : TObject);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    function Check : boolean;
    procedure Init (aCellDecorations : TmCellDecorations; const aFields : TmFields);
    procedure UpdateCellDecorations;
  end;

implementation

uses
  Graphics,
  mUtility, mMathUtility, mFormulaEditForm, mDecorationEditForm;

{$R *.lfm}

{ TCellDecorationsConfFrame }

procedure TCellDecorationsConfFrame.AddButtonClick(Sender: TObject);
var
  i : integer;
  newDec : TmCellDecoration;
begin
  i := FGrid.RowCount;
  newDec := FCellDecorations.Add;
  FGrid.InsertRowWithValues(i, ['', '1', '']);
  FGrid.Objects[IDX_FIELDNAME, i] := newDec;
end;

procedure TCellDecorationsConfFrame.RemoveButtonClick(Sender: TObject);
begin
  if FGrid.RowCount = 0 then
    exit;

  if FGrid.Row >= 0 then
  begin
    FGrid.DeleteRow(FGrid.Row);
  end;
end;


procedure TCellDecorationsConfFrame.OnSelectEditor (Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
begin
  if aCol = IDX_FIELDNAME then
  begin
    if (Editor is TCustomComboBox) then
    begin
      (Editor as TCustomComboBox).Style:= csDropDownList;
      if (Editor as TCustomComboBox).Items.Count = 0 then
      begin
        (Editor as TCustomComboBox).Items.Add(DECORATE_ALL_FIELDS_FIELDNAME);
        (Editor as TCustomComboBox).Items.AddStrings(FFields);
      end;
    end;
  end;
end;

procedure TCellDecorationsConfFrame.OnEditButtonClick(Sender: TObject; aCol, aRow: Integer);
var
  dlg : TFormulaEditForm;
  dlgDec : TDecorationEditForm;
begin
  if FGrid.SelectedColumn.Index = IDX_CONDITION then
  begin
    dlg := TFormulaEditForm.Create(Self);
    try
      dlg.Init(trim(FGrid.Cells[IDX_CONDITION, aRow]), FFields);
      if dlg.ShowModal = mrOK then
      begin
        FGrid.Cells[IDX_CONDITION, aRow] := trim(dlg.GetFormula);
      end;
    finally
      dlg.Free;
    end;
  end else if FGrid.SelectedColumn.Index = IDX_DECORATION then
  begin
    dlgDec := TDecorationEditForm.Create(Self);
    try
      dlgDec.Init(FGrid.Objects[IDX_FIELDNAME, aRow] as TmCellDecoration);
      if dlgDec.ShowModal = mrOk then
      begin
        dlgDec.ApplyChanges;
        FGrid.Cells[IDX_DECORATION, aRow] := (FGrid.Objects[IDX_FIELDNAME, aRow] as TmCellDecoration).DecorationAsString;
      end;
    finally
      dlgDec.Free;
    end;
  end;

end;

procedure TCellDecorationsConfFrame.OnEditingDone(Sender: TObject);
begin
  if FGrid.SelectedColumn.Index = IDX_FIELDNAME then
    FGrid.Cells[IDX_FIELDNAME, FGrid.Row] := StringReplace(UpperCase(Trim(FGrid.Cells[IDX_FIELDNAME, FGrid.Row])), ' ', '_', [rfReplaceAll])
  else if FGrid.SelectedColumn.Index = IDX_DECORATION then
    FGrid.Cells[IDX_DECORATION, FGrid.Row] := (FGrid.Objects[IDX_FIELDNAME, FGrid.Row] as TmCellDecoration).DecorationAsString;

  (*
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
  *)
end;


constructor TCellDecorationsConfFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FFields := TStringList.Create;
  FCellDecorations := TmCellDecorations.Create;

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
    Title.Caption := SColumnFieldName;
    Width := 200;
    ButtonStyle:= cbsPickList;
  end;
  with FGrid.Columns.Add do
  begin
    Title.Caption := SColumnCondition;
    Width := 250;
    ButtonStyle:= cbsEllipsis;
  end;
  with FGrid.Columns.Add do
  begin
    Title.Caption := SColumnDecoration;
    Width := 350;
    ButtonStyle:= cbsEllipsis;
  end;
  FGrid.RowCount:= 1;
end;

destructor TCellDecorationsConfFrame.Destroy;
begin
  FFields.Free;
  FCellDecorations.Free;
  inherited Destroy;
end;

function TCellDecorationsConfFrame.Check: boolean;
var
  tmpString: String;
  i: integer;
begin
  for i := 1 to FGrid.RowCount - 1 do
  begin
    tmpString := Uppercase(Trim(FGrid.Cells[IDX_FIELDNAME, i]));
    if tmpString  = '' then
    begin
      MessageDlg(SErrorMessageCaption, SErrorEmptyName, mtInformation, [mbOK], 0);
      Result := false;
      exit;
    end;

    if (trim(FGrid.Cells[IDX_CONDITION, i]) = '') then
    begin
      MessageDlg(SErrorMessageCaption, SErrorWrongCondition, mtInformation, [mbOK], 0);
      Result := false;
      exit;
    end;

    if (trim(FGrid.Cells[IDX_DECORATION, i]) = '') then
    begin
      MessageDlg(SErrorMessageCaption, SErrorWrongDecoration, mtInformation, [mbOK], 0);
      Result := false;
      exit;
    end;

  end;
  Result := true;
end;

procedure TCellDecorationsConfFrame.Init(aCellDecorations : TmCellDecorations; const aFields : TmFields);
var
  i : integer;
  tmpCellDec : TmCellDecoration;
begin
  FOriginalCellDecorations := aCellDecorations;

  for i := 0 to aCellDecorations.Count - 1 do
  begin
    tmpCellDec := FCellDecorations.Add;
    tmpCellDec.Assign(aCellDecorations.Get(i));

    FGrid.InsertColRow(false, i + 1);
    FGrid.Cells[IDX_FIELDNAME, i + 1] := tmpCellDec.FieldName;
    FGrid.Cells[IDX_CONDITION, i + 1] := tmpCellDec.Condition.AsString;
    FGrid.Cells[IDX_DECORATION, i + 1] := tmpCellDec.DecorationAsString;
    FGrid.Objects[IDX_FIELDNAME, i + 1] := tmpCellDec;
  end;

  for i := 0 to aFields.Count - 1 do
  begin
    if (not IsSystemField(aFields.Get(i).FieldName)) then
        FFields.Add(aFields.Get(i).FieldName);
  end;
end;

procedure TCellDecorationsConfFrame.UpdateCellDecorations;
var
  i : integer;
  newCelDec : TmCellDecoration;
begin
  if Self.Check then
  begin
    FOriginalCellDecorations.Clear;
    for i := 1 to FGrid.RowCount - 1 do
    begin
      newCelDec := FOriginalCellDecorations.Add;
      newCelDec.Assign((FGrid.Objects[IDX_FIELDNAME, i] as TmCellDecoration));
      newCelDec.Condition.Assign(FGrid.Cells[IDX_CONDITION, i], false);
      newCelDec.FieldName:= FGrid.Cells[IDX_FIELDNAME, i];
    end;
  end;
end;

end.

