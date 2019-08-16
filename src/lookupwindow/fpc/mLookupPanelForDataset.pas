// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mLookupPanelForDataset;

{$mode objfpc}
{$H+}

interface

uses
  Classes, Controls, ExtCtrls, ComCtrls, DB, contnrs,
  ListViewFilterEdit,
  mLookupWindowEvents, mMaps;

type

  { TmLookupPanel }

  TmLookupPanel = class (TCustomPanel)
  private
    const BLANK_PLACEHOLDER = '*BLANK*';
  private
    LValues: TListView;
    LValuesFilter: TListViewFilterEdit;
    FOnSelectAValue : TOnSelectAValue;
    FFieldsList : TStringList;
    FIdxKeyFieldName : integer;
    FValuesIndex : TmStringDictionary;
    FGarbage: TObjectList;

    procedure LValuesDblClick (Sender : TObject);
    procedure LValuesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure AdjustColumnsWidth;
  protected
    procedure DoOnResize; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init(aValues : TDataset; aFieldNames : TStringList; aKeyFieldName, aDisplayLabelFieldName : string);
    procedure SetFocusOnFilter;
    procedure GetSelectedValues (out aKeyValue: variant; out aDisplayLabel: string);

    property OnSelectAValue : TOnSelectAValue read FOnSelectAValue write FOnSelectAValue;
  end;

implementation

uses
  SysUtils;

type
  TResultValues = class
    ValueAsVariant : variant;
    DisplayLabel : string;
  end;

{ TmLookupPanel }

procedure TmLookupPanel.LValuesDblClick(Sender: TObject);
var
  tmpDisplayLabel: string;
  tmpKeyValue: variant;
begin
  if (LValues.SelCount = 1) and (Assigned(FOnSelectAValue)) then
  begin
    Self.GetSelectedValues(tmpKeyValue, tmpDisplayLabel);
    FOnSelectAValue(tmpKeyValue, tmpDisplayLabel);
  end;
end;

procedure TmLookupPanel.LValuesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  tmpDisplayLabel: string;
  tmpKeyValue: variant;
begin
  if (Key = 13) and (LValues.SelCount = 1) and (Assigned(FOnSelectAValue)) then
  begin
    Self.GetSelectedValues(tmpKeyValue, tmpDisplayLabel);
    FOnSelectAValue(tmpKeyValue, tmpDisplayLabel);
  end;
end;

procedure TmLookupPanel.AdjustColumnsWidth;
var
  ColWidth : integer;
  i : integer;
begin
  if FFieldsList.Count > 0 then
  begin
    ColWidth := (Self.Width - 30) div FFieldsList.Count;
    LValues.BeginUpdate;
    try
      for i := 0 to LValues.Columns.Count -1 do
        LValues.Columns[i].Width:= ColWidth;
    finally
      LValues.EndUpdate;
    end;
  end;
end;

procedure TmLookupPanel.DoOnResize;
begin
  inherited DoOnResize;
  AdjustColumnsWidth;
end;

procedure TmLookupPanel.GetSelectedValues (out aKeyValue: variant; out aDisplayLabel: string);
var
  tmp : TResultValues;
  tmpKey : string;
begin
  if (LValues.SelCount = 1) and (FIdxKeyFieldName >= 0) then
  begin
    if FIdxKeyFieldName = 0 then
      tmpKey := LValues.Selected.Caption
    else
      tmpKey := LValues.Selected.SubItems[FIdxKeyFieldName - 1];
    if tmpKey = '' then
      tmp := FValuesIndex.Find(BLANK_PLACEHOLDER) as TResultValues
    else
      tmp := FValuesIndex.Find(tmpKey) as TResultValues;
    aKeyValue := tmp.ValueAsVariant;
    aDisplayLabel:= tmp.DisplayLabel;
  end
  else
  begin
    aDisplayLabel:= '';
    aKeyValue:= null;
  end;
end;

constructor TmLookupPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Self.FOnSelectAValue:= nil;

  Self.BevelInner:= bvNone;
  Self.BevelOuter:= bvNone;
  LValuesFilter := TListViewFilterEdit.Create(Self);
  LValuesFilter.PArent := Self;
  LValuesFilter.Align:= alTop;
  LValuesFilter.ByAllFields:= true;
  LValues:= TListView.Create(Self);
  LValues.Parent := Self;
  LValues.Align:= alClient;
  LValues.OnDblClick:= @LValuesDblClick;
  LValues.GridLines := True;
  LValues.HideSelection := False;
  LValues.ReadOnly := True;
  LValues.RowSelect := True;
  LValues.ViewStyle := vsReport;
  FFieldsList := TStringList.Create;
  FIdxKeyFieldName := -1;
  FValuesIndex := TmStringDictionary.Create();
  FGarbage := TObjectList.Create(true);
end;

destructor TmLookupPanel.Destroy;
begin
  FGarbage.Free;
  FValuesIndex.Free;
  FFieldsList.Free;
  inherited Destroy;
end;

procedure TmLookupPanel.Init(aValues: TDataset; aFieldNames: TStringList;aKeyFieldName, aDisplayLabelFieldName: string);
var
  i : integer;
  col : TListColumn;
  item : TListItem;
  tmpField : TField;
  str : String;
  valueShell : TResultValues;
begin
  LValues.BeginUpdate;
  try
    for i := 0 to aFieldNames.Count -1 do
    begin
      tmpField := aValues.FindField(aFieldNames.Strings[i]);
      if Assigned(tmpField) then
      begin
        col := LValues.Columns.Add;
        col.Caption:= tmpField.DisplayLabel;
        col.Width:= 200;
        FFieldsList.Add(tmpField.FieldName);
        if CompareText(aFieldNames.Strings[i], aKeyFieldName) = 0 then
          FIdxKeyFieldName := FFieldsList.Count - 1;
      end;
    end;

    if FIdxKeyFieldName < 0 then
      raise Exception.Create('Key fieldname is missing!');

    aValues.DisableControls;
    try
      aValues.First;
      while not aValues.EOF do
      begin
        item := LValues.Items.Add;
        for i := 0 to FFieldsList.Count - 1 do
        begin
          tmpField := aValues.FieldByName(FFieldsList.Strings[i]);
          if tmpField.IsNull then
            str := ''
          else
            str := tmpField.AsString;
          if i = 0 then
            item.Caption:= str
          else
            item.SubItems.Add(str);
        end;
        tmpField := aValues.FieldByName(aKeyFieldName);
        valueShell := TResultValues.Create();
        valueShell.ValueAsVariant:= tmpField.Value;
        if tmpField.IsNull then
          FValuesIndex.Add(BLANK_PLACEHOLDER, valueShell)
        else
          FValuesIndex.Add(tmpField.AsString, valueShell);
        tmpField := aValues.FieldByName(aDisplayLabelFieldName);
        valueShell.DisplayLabel:= tmpField.AsString;
        FGarbage.Add(valueShell);

        aValues.Next;
      end;
    finally
      aValues.EnableControls;
    end;
  finally
    LValues.EndUpdate;
  end;
  LValuesFilter.FilteredListview := LValues;
end;

procedure TmLookupPanel.SetFocusOnFilter;
begin
  LValuesFilter.SetFocus;
end;

end.
