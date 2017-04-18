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
  Classes, Controls, ExtCtrls, ComCtrls, DB,
  ListViewFilterEdit,
  LookupWindowEvents,
  mMaps;

type

  { TmLookupPanel }

  TmLookupPanel = class (TCustomPanel)
  private
    LValues: TListView;
    LValuesFilter: TListViewFilterEdit;
    FOnSelectAValue : TOnSelectAValue;
    FFieldsList : TStringList;
    FIdxKeyFieldName : integer;

    procedure LValuesDblClick (Sender : TObject);
    procedure LValuesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure AdjustColumnsWidth;
  protected
    procedure DoOnResize; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init(aValues : TDataset; aFieldNames : TStringList; aKeyFieldName : string);
    procedure SetFocusOnFilter;
    function GetSelectedRowKey : String;

    property OnSelectAValue : TOnSelectAValue read FOnSelectAValue write FOnSelectAValue;
  end;

implementation

uses
  SysUtils;

{ TmLookupPanel }

procedure TmLookupPanel.LValuesDblClick(Sender: TObject);
begin
  if (LValues.SelCount = 1) and (Assigned(FOnSelectAValue)) then
    FOnSelectAValue(Self.GetSelectedRowKey);
end;

procedure TmLookupPanel.LValuesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = 13) and (LValues.SelCount = 1) and (Assigned(FOnSelectAValue)) then
    FOnSelectAValue(Self.GetSelectedRowKey);
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

function TmLookupPanel.GetSelectedRowKey: String;
begin
  if (LValues.SelCount = 1) and (FIdxKeyFieldName >= 0) then
  begin
    if FIdxKeyFieldName = 0 then
      Result := LValues.Selected.Caption
    else
      Result := LValues.Selected.SubItems[FIdxKeyFieldName - 1];
  end
  else
    Result := '';
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
end;

destructor TmLookupPanel.Destroy;
begin
  FFieldsList.Free;
  inherited Destroy;
end;

procedure TmLookupPanel.Init(aValues: TDataset; aFieldNames: TStringList;aKeyFieldName: string);
var
  i : integer;
  col : TListColumn;
  item : TListItem;
  tmpField : TField;
  str : String;
begin
  LValues.BeginUpdate;
  try
    for i := 0 to aFieldNames.Count -1 do
    begin
      col := LValues.Columns.Add;
      tmpField := aValues.FieldByName(aFieldNames.Strings[i]);
      col.Caption:= tmpField.DisplayLabel;
      col.Width:= 200;
//      col.AutoSize:= true;
      FFieldsList.Add(tmpField.FieldName);
      if CompareText(aFieldNames.Strings[i], aKeyFieldName) = 0 then
      begin
        FIdxKeyFieldName := FFieldsList.Count - 1;
      end;
    end;
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
