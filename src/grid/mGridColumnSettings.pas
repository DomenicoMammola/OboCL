// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mGridColumnSettings;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Contnrs, Grids, DBGrids,
  {$IFDEF DEBUG_COL_SET}LazLogger,{$ENDIF}
  mMaps, mNullables;

const
  MINIMUM_GRID_COLUMN_WIDTH = 10;

type

  { TmGridColumnSettings }

  TmGridColumnSettings = class
  strict private
    FFieldName : String;
    FDisplayFormat: TNullableString;
    FDisplayLabel : TNullableString;
    FVisible : TNullableBoolean;
    FWidth : TNullableInteger;
    FSortOrder : TNullableInteger;
  public
    constructor Create(aFieldName : String);
    destructor Destroy; override;

    procedure Clear;

    property FieldName : String read FFieldName;
    property DisplayLabel : TNullableString read FDisplayLabel;
    property Visible : TNullableBoolean read FVisible;
    property DisplayFormat : TNullableString read FDisplayFormat;
    property Width : TNullableInteger read FWidth;
    property SortOrder : TNullableInteger read FSortOrder;
  end;

  { TmGridColumnsSettings }

  TmGridColumnsSettings = class
  strict private
    FMap : TmStringDictionary;
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    function AddSettingsForField (aFieldName : String) : TmGridColumnSettings;
    function GetSettingsForField (aFieldName : String) : TmGridColumnSettings;
    function Count : integer;
    function Get (aIndex : integer): TmGridColumnSettings;

    procedure Clear;
  end;

implementation

uses
  SysUtils, Dialogs, Math;

{ TmGridColumnsSettings }

constructor TmGridColumnsSettings.Create;
begin
  FList := TObjectList.Create(true);
  FMap := TmStringDictionary.Create();
end;

destructor TmGridColumnsSettings.Destroy;
begin
  FMap.Free;
  FList.Free;
  inherited Destroy;
end;

function TmGridColumnsSettings.AddSettingsForField(aFieldName: String): TmGridColumnSettings;
var
  tmp : String;
  oldOptions : TmGridColumnSettings;
begin
  tmp := Uppercase(aFieldName);
  oldOptions := FMap.Find(tmp) as TmGridColumnSettings;
  if Assigned(oldOptions) then
  begin
    FMap.Remove(tmp);
    FList.Remove(oldOptions);
  end;
  Result := TmGridColumnSettings.Create(aFieldName);
  FList.Add(Result);
  FMap.Add(tmp, Result);
end;

function TmGridColumnsSettings.GetSettingsForField(aFieldName: String): TmGridColumnSettings;
begin
  Result := FMap.Find(Uppercase(aFieldName)) as TmGridColumnSettings;
end;

function TmGridColumnsSettings.Count: integer;
begin
  Result := FList.Count;
end;

function TmGridColumnsSettings.Get(aIndex: integer): TmGridColumnSettings;
begin
  Result := FList.Items[aIndex] as TmGridColumnSettings;
end;


procedure TmGridColumnsSettings.Clear;
begin
  FList.Clear;
  FMap.Clear;
end;

{ TmGridColumnSettings }

constructor TmGridColumnSettings.Create(aFieldName : String);
begin
  FFieldName:= aFieldName;
  FDisplayFormat := TNullableString.Create();
  FDisplayLabel := TNullableString.Create();
  FSortOrder := TNullableInteger.Create();
  FVisible := TNullableBoolean.Create();
  FWidth := TNullableInteger.Create();
end;

destructor TmGridColumnSettings.Destroy;
begin
  FDisplayFormat.Free;
  FDisplayLabel.Free;
  FVisible.Free;
  FWidth.Free;
  FSortOrder.Free;

  inherited Destroy;
end;

procedure TmGridColumnSettings.Clear;
begin
  FWidth.IsNull:= true;
  FVisible.IsNull := true;
  FSortOrder.IsNull := true;
  FDisplayLabel.IsNull := true;
  FDisplayFormat.IsNull:= true;
end;

end.
