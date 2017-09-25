// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
//
// This can be uses as a workaround for this problem:
// https://forum.lazarus.freepascal.org/index.php?topic=16308.0
//
unit mEditingFrame;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, Controls, Forms, ValEdit, Graphics, Grids, contnrs, ExtCtrls,
  SysUtils, variants, StdCtrls,
  oMultiPanelSetup, OMultiPanel,
  mGridEditors, mMaps, mCalendarDialog, mUtility, mMathUtility, mLookupForm,
  mQuickReadOnlyVirtualDataSet, mVirtualDataSet, mVirtualFieldDefs;

resourcestring
  SPropertyColumnTitle = 'Property';
  SValueColumnTitle = 'Value';

type
  TmEditingFrameEditorKind = (ekInteger, ekFloat, ekDate, ekLookup, ekText);

  { TmEditingFrame }

  TmEditingFrame = class(TCustomPanel)
  strict private
    FRootPanel : TOMultiPanel;
    FValueListEditor: TValueListEditor;
    FCustomDateEditor : TmExtStringCellEditor;
    FCustomEditor : TmExtStringCellEditor;
    FLinesByName : TmStringDictionary;
    FLinesByIndex : TmIntegerDictionary;
    FMemosByName : TmStringDictionary;
    FLines : TObjectList;

    function GetAlternateColor: TColor;
    procedure SetAlternateColor(AValue: TColor);

    procedure OnValueListEditorPrepareCanvas(sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
    procedure OnValueListEditorSelectEditor(Sender: TObject; aCol,  aRow: Integer; var Editor: TWinControl);
    procedure OnValueListEditorValidateEntry(sender: TObject; aCol, aRow: Integer; const OldValue: string; var NewValue: String);
    function OnValueListEditorEditValue  (const aCol, aRow : integer; var aNewValue : string): boolean;
  protected
    procedure AddLine (const aName : string; const aCaption : string; const aDefaultValue : string; const aEditorKind : TmEditingFrameEditorKind);
    procedure AddMemo (const aName : string; const aCaption : string; const aDefaultValue : string; const aMemoHeightPercent : double);
    procedure ExtractFields (aVirtualFields : TVirtualFieldDefs; aList : TStringList);
    function GetValue (const aName : string) : String;
    // override these:
    procedure OnEditValue(const aName : string; const aNewValue : variant); virtual;
    procedure OnValidateValue(const aName : string; const aOldValue : String; var aNewValue : String); virtual;
    procedure InitProviderForLookup (const aName : string; aDatasetProvider : TReadOnlyVirtualDatasetProvider; aFieldsList : TStringList; out aKey: string); virtual;
    function GetValueFromLookupKeyValue (const aName : string; const aLookupKeyValue : string) : string; virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetFocusInEditor;

    property AlternateColor : TColor read GetAlternateColor write SetAlternateColor;
  end;

implementation

type
  TEditorLine = class
  public
    Name : String;
    EditorKind : TmEditingFrameEditorKind;
    Index : integer;
  end;

{ TmEditingFrame }

function TmEditingFrame.GetAlternateColor: TColor;
begin
  Result := FValueListEditor.AlternateColor;
end;

procedure TmEditingFrame.SetAlternateColor(AValue: TColor);
begin
  FValueListEditor.AlternateColor:= aValue;
end;

procedure TmEditingFrame.OnValueListEditorPrepareCanvas(sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
begin
  if aCol = 0 then
    FValueListEditor.Canvas.Font.Style := FValueListEditor.Canvas.Font.Style + [fsBold];
end;

procedure TmEditingFrame.OnValueListEditorSelectEditor(Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
var
  curLine : TEditorLine;
begin
  if aCol <> 1 then
    exit;

  curLine := FLinesByIndex.Find(aRow) as TEditorLine;
  if not Assigned(curLine) then
    exit;

  if (curLine.EditorKind = ekDate) then
  begin
    FCustomDateEditor.Text := FValueListEditor.Cells[FValueListEditor.Col, FValueListEditor.Row];
    Editor := FCustomDateEditor;
  end
  else if (curLine.EditorKind = ekLookup) then
  begin
    FCustomEditor.Text := FValueListEditor.Cells[FValueListEditor.Col, FValueListEditor.Row];
    Editor := FCustomEditor;
  end;
end;

procedure TmEditingFrame.OnValueListEditorValidateEntry(sender: TObject; aCol, aRow: Integer; const OldValue: string; var NewValue: String);
var
  tmp : String;
  vDate : TDate;
  curLine : TEditorLine;
  tmpDouble : Double;
begin
  tmp := trim(NewValue);

  curLine := FLinesByIndex.Find(aRow) as TEditorLine;

  if curLine.EditorKind = ekDate then
  begin
    vDate := 0;
    if TryToUnderstandDateString(tmp, vDate) then
      NewValue := DateToStr(vDate)
    else
      NewValue := '';
  end else if curLine.EditorKind = ekInteger then
  begin
    if IsNumeric(tmp, false) then
      NewValue := tmp
    else
      NewValue := '';
  end else if curLine.EditorKind = ekFloat then
  begin
    if TryToConvertToDouble(tmp, tmpDouble) then
      NewValue := FloatToStr(tmpDouble)
    else
      NewValue := '';
  end;

  Self.OnValidateValue(curLine.Name, OldValue, NewValue);

  if NewValue <> OldValue then
    Self.OnEditValue(curLine.Name, NewValue);
end;

function TmEditingFrame.OnValueListEditorEditValue(const aCol, aRow: integer; var aNewValue: string): boolean;
var
  calendarFrm : TmCalendarDialog;
  str, tmpKey : String;
  curLine : TEditorLine;
  lookupFrm : TmLookupWindow;
  tmpDatasetProvider : TReadOnlyVirtualDatasetProvider;
  tmpDataset : TVirtualDataset;
  tmpFieldsList : TStringList;
begin
  curLine := FLinesByIndex.Find(aRow) as TEditorLine;

  if curLine.EditorKind = ekDate then
  begin
    calendarFrm := TmCalendarDialog.Create;
    try
      if FValueListEditor.Cells[aCol, aRow] <> '' then
      begin
        try
          calendarFrm.Date := StrToDate(FValueListEditor.Cells[aCol, aRow]);
        except
          // ignored
        end;
      end;
      if calendarFrm.Execute then
      begin
        str := DateToStr(calendarFrm.Date);
        FValueListEditor.Cells[aCol, aRow] := str;
        Result := true;
        aNewValue := str;

        Self.OnEditValue(curLine.Name, calendarFrm.Date);
      end;
    finally
      calendarFrm.Free;
    end;
  end
  else if curLine.EditorKind = ekLookup then
  begin
    tmpFieldsList := TStringList.Create;
    try
      lookupFrm := TmLookupWindow.Create(Self);
      try
        tmpDatasetProvider := TReadOnlyVirtualDatasetProvider.Create;
        tmpDataset := TVirtualDataset.Create(Self);
        try
          tmpDataset.DatasetDataProvider := tmpDatasetProvider;

          InitProviderForLookup(curLine.Name, tmpDatasetProvider, tmpFieldsList, tmpKey);

          tmpDataset.Active:= true;
          tmpDataset.Refresh;
          lookupFrm.Init(tmpDataset, tmpFieldsList, tmpKey);
          if lookupFrm.ShowModal = mrOk then
          begin
            str := GetValueFromLookupKeyValue (curLine.Name, lookupFrm.Selected);

            FValueListEditor.Cells[aCol, aRow] := str;
            aNewValue := str;
            Result := true;
          end;
        finally
          tmpDataset.Free;
          tmpDatasetProvider.Free;
        end;

      finally
        lookupFrm.Free;
      end;
    finally
      tmpFieldsList.Free;
    end;
  end;
end;

procedure TmEditingFrame.AddLine(const aName: string; const aCaption: string; const aDefaultValue: string; const aEditorKind : TmEditingFrameEditorKind);
var
  tmp : TEditorLine;
begin
  tmp := TEditorLine.Create;
  FLines.Add(tmp);
  FLinesByName.Add(aName, tmp);
  tmp.EditorKind:= aEditorKind;
  tmp.Name := aName;
  tmp.Index:= FValueListEditor.InsertRow(aCaption, aDefaultValue, true);
  FLinesByIndex.Add(tmp.Index + 1, tmp);
  if (aEditorKind = ekDate) or (aEditorKind = ekLookup) then
    FValueListEditor.ItemProps[tmp.Index].EditStyle:=esEllipsis;
end;

procedure TmEditingFrame.AddMemo(const aName: string; const aCaption: string;const aDefaultValue: string; const aMemoHeightPercent : double);
var
  tmpPanel1, tmpPanel2 : TPanel;
  tmpMemo : TMemo;
  i : integer;
  position : double;
begin
  tmpPanel1 := TPanel.Create(FRootPanel);
  tmpPanel1.Parent := FRootPanel;
  tmpPanel1.BevelInner:= bvNone;
  tmpPanel1.BevelOuter:= bvNone;
  FRootPanel.PanelCollection.AddControl(tmpPanel1);

  tmpPanel2 := TPanel.Create(tmpPanel1);
  tmpPanel2.Parent := tmpPanel1;
  tmpPanel2.Align:= alLeft;
  tmpPanel2.BevelInner:= bvNone;
  tmpPanel2.BevelOuter:= bvNone;
  tmpPanel2.Width:= FValueListEditor.DefaultColWidth;
  tmpPanel2.Caption:= aCaption;
  tmpPanel2.Font.Style:=[fsBold];
  tmpPanel2.BorderWidth:= 1;
  tmpPanel2.BorderStyle:=bsSingle;
  tmpMemo:= TMemo.Create(tmpPanel1);
  tmpMemo.Parent := tmpPanel1;
  tmpMemo.Align:= alClient;
  tmpMemo.ScrollBars:= ssVertical;
  tmpMemo.WantReturns:= true;
  tmpMemo.Text:= aDefaultValue;

  FMemosByName.Add(aName, tmpMemo);

  FRootPanel.PanelCollection.Items[FRootPanel.PanelCollection.Count - 1].Position:= 1;
  position := 1 - aMemoHeightPercent;
  for i := FRootPanel.PanelCollection.Count -2 downto 0 do
  begin
   FRootPanel.PanelCollection.Items[i].Position := position;
   position := position - aMemoHeightPercent;
  end;
end;

procedure TmEditingFrame.ExtractFields(aVirtualFields: TVirtualFieldDefs; aList: TStringList);
var
  i : integer;
begin
  aList.Clear;
  for i := 0 to aVirtualFields.Count -1 do
    aList.Add(aVirtualFields.VirtualFieldDefs[i].Name);
end;

function TmEditingFrame.GetValue(const aName: string): String;
var
  curLine : TEditorLine;
begin
  curLine := FLinesByName.Find(aName) as TEditorLine;
  Result := FValueListEditor.Rows[curLine.Index + 1].Strings[1];
end;

procedure TmEditingFrame.OnEditValue(const aName: string; const aNewValue: variant);
begin
  //
end;

procedure TmEditingFrame.OnValidateValue(const aName: string; const aOldValue: String; var aNewValue: String);
begin
  //
end;

procedure TmEditingFrame.InitProviderForLookup(const aName : string; aDatasetProvider: TReadOnlyVirtualDatasetProvider; aFieldsList: TStringList; out aKey: string);
begin
  aKey := '';
end;

function TmEditingFrame.GetValueFromLookupKeyValue(const aName: string; const aLookupKeyValue: string): string;
begin
  Result := '';
end;

constructor TmEditingFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Self.BevelInner:= bvNone;
  Self.BevelOuter:= bvNone;
  Self.BorderStyle:= bsNone;
  Self.Caption:= '';
  FRootPanel := TOMultiPanel.Create(Self);
  FRootPanel.Parent := Self;
  FRootPanel.PanelType:= ptVertical;
  FRootPanel.Align:= alClient;

  FValueListEditor:= TValueListEditor.Create(FRootPanel);
  FValueListEditor.Parent := FRootPanel;
  FValueListEditor.Align:= alClient;
  FRootPanel.PanelCollection.AddControl(FValueListEditor);
  FValueListEditor.Height:= 200;
  FValueListEditor.AlternateColor := clMoneyGreen;
  FValueListEditor.AutoAdvance := aaDown;
  FValueListEditor.DefaultColWidth := 230;
  FValueListEditor.FixedCols := 0;
  FValueListEditor.Flat := True;
  FValueListEditor.RowCount := 2;
  FValueListEditor.TabOrder := 0;
  FValueListEditor.OnPrepareCanvas := Self.OnValueListEditorPrepareCanvas;
  FValueListEditor.OnSelectEditor := Self.OnValueListEditorSelectEditor;
  FValueListEditor.OnValidateEntry := Self.OnValueListEditorValidateEntry;
  FValueListEditor.TitleCaptions.Add(SPropertyColumnTitle);
  FValueListEditor.TitleCaptions.Add(SValueColumnTitle);
  FValueListEditor.ColWidths[0] := 230;
  FValueListEditor.ColWidths[1] := 370;

  FCustomEditor := TmExtStringCellEditor.Create(Self);
  FCustomEditor.Visible := false;
  FCustomEditor.ReadOnly := true;
  FCustomEditor.OnShowEditorEvent:= Self.OnValueListEditorEditValue;
  FCustomEditor.ParentGrid := FValueListEditor;

  FCustomDateEditor := TmExtStringCellEditor.Create(Self);
  FCustomDateEditor.Visible := false;
  FCustomDateEditor.ReadOnly := false;
  FCustomDateEditor.OnShowEditorEvent:= Self.OnValueListEditorEditValue;
  FCustomDateEditor.ParentGrid := FValueListEditor;

  FLinesByName := TmStringDictionary.Create();
  FLinesByIndex := TmIntegerDictionary.Create();
  FLines := TObjectList.Create(true);
  FMemosByName := TmStringDictionary.Create();
end;

destructor TmEditingFrame.Destroy;
begin
  FLinesByName.Free;
  FLinesByIndex.Free;
  FLines.Free;
  FMemosByName.Free;
  inherited Destroy;
end;

procedure TmEditingFrame.SetFocusInEditor;
begin
  FValueListEditor.SetFocus;
  FValueListEditor.Row:= 1;
  FValueListEditor.Col:= 1;
  FValueListEditor.EditorMode:= true;
end;

end.
