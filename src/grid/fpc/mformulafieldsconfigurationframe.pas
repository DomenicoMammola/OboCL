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
  SynEdit, SynCompletion,

  OMultiPanel, oMultiPanelSetup,

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
    FRootPanel : TOMultiPanel;
    FGridPanel : TPanel;
    FGrid : TStringGrid;
    FFormulaPanel : TPanel;
    FLeftEditorPanel : TPanel;
    FFormulas : TmFormulaFields;
    FEditor : TSynEdit;
    FEditorCompletion: TSynCompletion;
    FFieldsList : TStringList;
    FSaveFormulaBtn : TButton;
    FDiscardFormulaBtn : TButton;
    FCurRow : integer;
    procedure OnSelectEditor (Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
    procedure OnEditButtonClick (Sender: TObject; aCol, aRow: Integer);
    procedure OnEditingDone(Sender : TObject);
    procedure OnEditorCompletionExecute(Sender: TObject);
    procedure OnEditorCompletionSearchPosition(var APosition: integer);
    procedure OnClickSaveFormula(Sender : TObject);
    procedure OnClickDiscardFormula(Sender : TObject);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    function Check : boolean;
    procedure Init (aFormulas : TmFormulaFields; const aFields : TmFields);
    procedure UpdateFormulaFields;
  end;

implementation

uses
  Graphics,
  mUtility, mMathUtility, KAParser;

{$R *.lfm}

{ TFormulaFieldsConfFrame }

procedure TFormulaFieldsConfFrame.AddButtonClick(Sender: TObject);
begin
  FGrid.InsertRowWithValues(FGrid.RowCount, ['NEWFIELD', 'DOUBLE', '', '1']);
end;

procedure TFormulaFieldsConfFrame.RemoveButtonClick(Sender: TObject);
begin
  if FGrid.RowCount = 0 then
    exit;

  if FGrid.Row >= 0 then
  begin
    FGrid.DeleteRow(FGrid.Row);
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
begin
  if FGrid.SelectedColumn.Index = IDX_FORMULA then
  begin
    FCurRow := aRow;
    FEditor.Text:= trim(FGrid.Cells[IDX_FORMULA, FCurRow]);
    FSaveFormulaBtn.Enabled:= true;
    FDiscardFormulaBtn.Enabled:= true;
    FGrid.Enabled:= false;
    AddButton.Enabled:= false;
    RemoveButton.Enabled:= false;
    FEditor.SetFocus;
    FEditor.CaretX:= 0;
    FEditor.CaretY:= 0;
  end;
end;

procedure TFormulaFieldsConfFrame.OnEditingDone(Sender: TObject);
var
  newSize : integer;
begin
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

procedure TFormulaFieldsConfFrame.OnEditorCompletionExecute(Sender: TObject);
var
  i : integer;

  procedure Add(s: String);
  begin
    if (FEditorCompletion.CurrentString = '') or (pos(lowercase(FEditorCompletion.CurrentString), lowercase(s)) = 1) then
      FEditorCompletion.ItemList.Add(s);
  end;

begin
  FEditorCompletion.ItemList.Clear;
  for i := 0 to FFieldsList.Count - 1 do
    Add(FFieldsList.Strings[i]);
end;

procedure TFormulaFieldsConfFrame.OnEditorCompletionSearchPosition(var APosition: integer);
var
  i : integer;

  procedure Add(s: String);
  begin
    if (FEditorCompletion.CurrentString = '') or (pos(lowercase(FEditorCompletion.CurrentString), lowercase(s)) = 1) then
      FEditorCompletion.ItemList.Add(s);
  end;
begin
  FEditorCompletion.ItemList.Clear;
  for i := 0 to FFieldsList.Count - 1 do
    Add(FFieldsList.Strings[i]);

  if FEditorCompletion.ItemList.Count > 0 then
    APosition := 0
  else
    APosition := -1;
end;

procedure TFormulaFieldsConfFrame.OnClickSaveFormula(Sender: TObject);
begin
  FGrid.Cells[IDX_FORMULA, FCurRow] := trim(FEditor.Text);
  FSaveFormulaBtn.Enabled:= false;
  FDiscardFormulaBtn.Enabled:= false;
  FGrid.Enabled:= true;
  FEditor.Text:= '';
  AddButton.Enabled:= true;
  RemoveButton.Enabled:= true;
end;

procedure TFormulaFieldsConfFrame.OnClickDiscardFormula(Sender: TObject);
begin
  FEditor.Text:= '';
  FSaveFormulaBtn.Enabled:= false;
  FDiscardFormulaBtn.Enabled:= false;
  FGrid.Enabled:= true;
  AddButton.Enabled:= true;
  RemoveButton.Enabled:= true;
end;

constructor TFormulaFieldsConfFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FFieldsList := TStringList.Create;

  FRootPanel := TOMultiPanel.Create(Self);
  FRootPanel.Parent := Self;
  FRootPanel.PanelType:= ptVertical;
  FRootPanel.Align:= alClient;

  FGridPanel := TPanel.Create(FRootPanel);
  FGridPanel.Parent := FRootPanel;
  FRootPanel.PanelCollection.AddControl(FGridPanel);

  FFormulaPanel := TPanel.Create(FRootPanel);
  FFormulaPanel.Parent := FRootPanel;
  FRootPanel.PanelCollection.AddControl(FFormulaPanel);

  FRootPanel.PanelCollection.Items[1].Position:= 1;
  FRootPanel.PanelCollection.Items[0].Position:= 0.4;

  FLeftEditorPanel := TPanel.Create(FFormulaPanel);
  FLeftEditorPanel.Parent := FFormulaPanel;
  FLeftEditorPanel.BevelInner:= bvNone;
  FLeftEditorPanel.BevelOuter:= bvNone;
  FLeftEditorPanel.Caption:= '';
  FLeftEditorPanel.Align:= alRight;
  FLeftEditorPanel.Width:= 100;

  FSaveFormulaBtn := TButton.Create(FLeftEditorPanel);
  FSaveFormulaBtn.Parent := FLeftEditorPanel;
  FSaveFormulaBtn.Top:= 4;
  FSaveFormulaBtn.Left:= 4;
  FSaveFormulaBtn.Width:= FLeftEditorPanel.Width - 8;
  FSaveFormulaBtn.Caption:= SBtnSaveFormula;
  FSaveFormulaBtn.OnClick:= @OnClickSaveFormula;
  FSaveFormulaBtn.Enabled:= false;

  FDiscardFormulaBtn := TButton.Create(FLeftEditorPanel);
  FDiscardFormulaBtn.Parent := FLeftEditorPanel;
  FDiscardFormulaBtn.Top:= 4 + 4 + FSaveFormulaBtn.Height;
  FDiscardFormulaBtn.Left:= 4;
  FDiscardFormulaBtn.Width:= FLeftEditorPanel.Width - 8;
  FDiscardFormulaBtn.Caption:= SBtnDiscardFormula;
  FDiscardFormulaBtn.OnClick:= @OnClickDiscardFormula;
  FDiscardFormulaBtn.Enabled:= false;

  FEditor := TSynEdit.Create(FFormulaPanel);
  FEditor.Parent := FFormulaPanel;
  FEditor.Align:= alClient;

  FGrid:= TStringGrid.Create(FGridPanel);
  FGrid.Parent := FGridPanel;
  FGrid.Align:= alClient;

  FGrid.Options := [goEditing,goTabs, goVertLine, goHorzLine, goDblClickAutoSize];
  FGrid.FixedCols:= 0;
  FGrid.RowCount:= 0;
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

  FEditorCompletion:= TSynCompletion.Create(Self);
  FEditorCompletion.Editor := FEditor;
  //FEditorCompletion.EndOfTokenChr:= '()[].';
  FEditorCompletion.OnExecute:= @OnEditorCompletionExecute;
  FEditorCompletion.OnSearchPosition:= @OnEditorCompletionSearchPosition;
  FEditorCompletion.LinesInWindow:= 12;
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
  if FSaveFormulaBtn.Enabled then
  begin
    MessageDlg(SErrorMessageCaption, SErrorStillEditingFormula, mtInformation, [mbOK], 0);
    Result := false;
    exit;
  end;
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

  GetFunctionsList(FFieldsList, false);
  for i := 0 to aFields.Count - 1 do
  begin
    if (not Assigned(aFormulas.FindByName(aFields.Get(i).FieldName))) and (not IsSystemField(aFields.Get(i).FieldName)) then
        FFieldsList.Add(aFields.Get(i).FieldName);
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

