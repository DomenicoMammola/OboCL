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
  mMaps, mXML, mNullables;

const
  MINIMUM_GRID_COLUMN_WIDTH = 4;

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

    procedure SaveToXmlElement (aElement : TmXmlElement);
    procedure LoadFromXmlElement(aElement : TmXmlElement);

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

    procedure SaveToXmlElement (aElement : TmXmlElement);
    procedure LoadFromXmlElement (aElement : TmXmlElement);

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

procedure TmGridColumnsSettings.SaveToXmlElement(aElement: TmXmlElement);
var
  i : integer;
  op : TmGridColumnSettings;
begin
  for i := 0 to FList.Count - 1 do
  begin
    op := FList.Items[i] as TmGridColumnSettings;
    op.SaveToXmlElement(aelement.AddElement('column'));
  end;
end;

procedure TmGridColumnsSettings.LoadFromXmlElement(aElement: TmXmlElement);
var
  tmpCursor : TmXmlElementCursor;
  i : integer;
  op : TmGridColumnSettings;
begin
  Self.Clear;
  tmpCursor := TmXmlElementCursor.Create(aElement, 'column');
  try
    for i := 0 to tmpCursor.Count - 1 do
    begin
      op := Self.AddSettingsForField(tmpCursor.Elements[i].GetAttribute('fieldName'));
      op.LoadFromXmlElement(tmpCursor.Elements[i]);
    end;
  finally
    tmpCursor.Free;
  end;

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


procedure TmGridColumnSettings.SaveToXmlElement(aElement: TmXmlElement);
begin
  if FVisible.NotNull then
    aElement.SetAttribute('visible', BoolToStr(FVisible.Value, true));
  if FDisplayFormat.NotNull then
    aElement.SetAttribute('displayFormat', FDisplayFormat.Value);
  if FDisplayLabel.NotNull then
    aElement.SetAttribute('displayLabel', FDisplayLabel.Value);
  if FWidth.NotNull then
    aElement.SetIntegerAttribute('width', FWidth.Value);
  if FSortOrder.NotNull then
    aElement.SetIntegerAttribute('sortOrder', FSortOrder.Value);
  aElement.SetAttribute('fieldName', FFieldName);
end;

procedure TmGridColumnSettings.LoadFromXmlElement(aElement: TmXmlElement);
begin
  Self.Clear;
  if aElement.HasAttribute('visible') then
    FVisible.Value := StrToBool(aElement.GetAttribute('visible'));
  if aElement.HasAttribute('displayFormat') then
    FDisplayFormat.Value := aElement.GetAttribute('displayFormat');
  if aElement.HasAttribute('displayLabel') then
  begin
    FDisplayLabel.Value := aElement.GetAttribute('displayLabel');
    {$IFDEF DEBUG_COL_SET}DebugLn('[TmGridColumnSettings.LoadFromXmlElement] ' + FieldName + ' displayLabel:' + FDisplayLabel.Value);{$ENDIF}
  end;

  if aElement.HasAttribute('width') then
    FWidth.Value := aElement.GetIntegerAttribute('width');
  if aElement.HasAttribute('sortOrder') then
    FSortOrder.Value := aElement.GetIntegerAttribute('sortOrder');
end;


end.
