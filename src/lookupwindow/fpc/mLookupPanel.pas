// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mLookupPanel;

{$mode objfpc}
{$H+}

interface

uses
  Classes, Controls, ExtCtrls, ComCtrls, DB, contnrs,
  Variants,
  ListViewFilterEdit,
  mLookupWindowEvents, mMaps,
  mDatasetStandardSetup, mDataProviderInterfaces, mDataProviderFieldDefs;

type

  { TmLookupPanel }

  TmLookupPanel = class (TCustomPanel)
  private
    LValues: TListView;
    LValuesFilter: TListViewFilterEdit;
    FOnSelectAValue : TOnSelectAValue;
    FFieldsList : TStringList;
    FFieldDefs : TmVirtualFieldDefs;

    FKeyFieldName : String;
    FDataProvider : IVDDataProvider;
    FDisplayFieldNames : TStringList;

    procedure LValuesDblClick (Sender : TObject);
    procedure LValuesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure AdjustColumnsWidth;
  protected
    procedure DoOnResize; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init(const aDataProvider : IVDDataProvider; const aFieldNames : TStringList; const aKeyFieldName : string; const aDisplayFieldNames : TStringList); overload;
    procedure Init(const aDataProvider : IVDDataProvider); overload;
    procedure SetFocusOnFilter;
    procedure GetSelectedValues (out aKeyValue: variant; out aDisplayLabel: string);

    property OnSelectAValue : TOnSelectAValue read FOnSelectAValue write FOnSelectAValue;
  end;

implementation

uses
  SysUtils, Math;

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
  // ColWidth : integer;
  i, eqWidth, tmp : integer;
  curFieldDef : TmVirtualFieldDef;
  flex : double;
begin
  if FFieldsList.Count > 0 then
  begin
    eqWidth:= 0;
    tmp := 0;
    for i := 0 to FFieldsList.Count - 1 do
    begin
      curFieldDef := FFieldDefs.FindByName(FFieldsList.Strings[i]);
      if (curFieldDef.DataType = vftString) or (curFieldDef.DataType = vftWideString) then
      begin
        tmp := min(curFieldDef.Size div 4, 12) + max((curFieldDef.Size - 50) div 14, 0) + max((curFieldDef.Size - 100) div 28, 0);
        eqWidth:= eqWidth + tmp
      end
      else if curFieldDef.DataType = vftBoolean then
        eqWidth:= eqWidth + 2
      else if curFieldDef.DataType = vftInteger then
        eqWidth:= eqWidth + 4
      else if curFieldDef.DataType = vftFloat then
        eqWidth:= eqWidth + 7
      else
        eqWidth:= eqWidth + 6;
    end;

    flex := (Self.Width - 30) / eqWidth;
    LValues.BeginUpdate;
    try
      for i := 0 to LValues.Columns.Count -1 do
      begin
        curFieldDef := FFieldDefs.FindByName(FFieldsList.Strings[i]);
        if (curFieldDef.DataType = vftString) or (curFieldDef.DataType = vftWideString) then
        begin
          tmp := min(curFieldDef.Size div 4, 12) + max((curFieldDef.Size - 50) div 14, 0) + max((curFieldDef.Size - 100) div 28, 0);
          LValues.Columns[i].Width:= trunc(tmp * flex);
        end
        else if curFieldDef.DataType = vftBoolean then
          LValues.Columns[i].Width:= trunc(flex * 2)
        else if curFieldDef.DataType = vftInteger then
          LValues.Columns[i].Width:= trunc(flex * 4)
        else if curFieldDef.DataType = vftFloat then
          LValues.Columns[i].Width:= trunc(flex * 7)
        else
          LValues.Columns[i].Width:= trunc(flex * 6);
      end;
    finally
      LValues.EndUpdate;
    end;

    (*
    ColWidth := (Self.Width - 30) div FFieldsList.Count;
    LValues.BeginUpdate;
    try
      for i := 0 to LValues.Columns.Count -1 do
        LValues.Columns[i].Width:= ColWidth;
    finally
      LValues.EndUpdate;
    end;
    *)
  end;
end;

procedure TmLookupPanel.DoOnResize;
begin
  inherited DoOnResize;
  AdjustColumnsWidth;
end;

procedure TmLookupPanel.GetSelectedValues (out aKeyValue: variant; out aDisplayLabel: string);
var
  i : integer;
begin
  if (LValues.SelCount = 1) then
  begin
    i := UIntPtr(LValues.Selected.Data);
    aKeyValue := FDataProvider.GetDatum(i).GetPropertyByFieldName(FKeyFieldName);
    aDisplayLabel:= ConcatenateFieldValues(FDataProvider.GetDatum(i), FDisplayFieldNames);
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
  FDisplayFieldNames := TStringList.Create;
  FFieldDefs := TmVirtualFieldDefs.Create;
end;

destructor TmLookupPanel.Destroy;
begin
  FFieldsList.Free;
  FDisplayFieldNames.Free;
  FFieldDefs.Free;
  inherited Destroy;
end;

procedure TmLookupPanel.Init(const aDataProvider : IVDDataProvider;  const aFieldNames : TStringList; const aKeyFieldName : string; const aDisplayFieldNames : TStringList);
var
  k, i : integer;
  ptr : UIntPtr;
  col : TListColumn;
  item : TListItem;
  str : String;
  curValue : Variant;
  curDatum : IVDDatum;
begin
  FDataProvider := aDataProvider;
  FDataProvider.FillVirtualFieldDefs(FFieldDefs, '');

  FKeyFieldName:= aKeyFieldName;
  FDisplayFieldNames.Clear;
  if Assigned(aDisplayFieldNames) then
    FDisplayFieldNames.AddStrings(aDisplayFieldNames)
  else
    FDataProvider.GetMinimumFields(FDisplayFieldNames);

  LValues.BeginUpdate;
  try
    FFieldsList.Clear;
    if Assigned(aFieldNames) then
      FFieldsList.AddStrings(aFieldNames)
    else
      FDataProvider.GetMinimumFields(FFieldsList);

    for i := 0 to FFieldsList.Count -1 do
    begin
      col := LValues.Columns.Add;
      col.Caption:= GenerateDisplayLabel(FFieldsList.Strings[i]);
      col.Width:= 200;
    end;

    ptr := 0;
    for i := 0 to FDataProvider.Count - 1 do
    begin
      item := LValues.Items.Add;
      item.Data:= pointer(ptr);
      ptr := ptr + 1;

      for k := 0 to FFieldsList.Count - 1 do
      begin
        curDatum := FDataProvider.GetDatum(i);
        curValue := curDatum.GetPropertyByFieldName(FFieldsList.Strings[k]);

        if VarIsNull(curValue) then
          str := ''
        else
          str := VarToStr(curValue);
        if k = 0 then
          item.Caption:= str
        else
          item.SubItems.Add(str);
      end;
    end;
  finally
    LValues.EndUpdate;
  end;
  LValuesFilter.FilteredListview := LValues;
end;

procedure TmLookupPanel.Init(const aDataProvider: IVDDataProvider);
begin
  Self.Init(aDataProvider, nil, aDataProvider.GetKeyFieldName, nil);
end;

procedure TmLookupPanel.SetFocusOnFilter;
begin
  LValuesFilter.SetFocus;
end;

end.
