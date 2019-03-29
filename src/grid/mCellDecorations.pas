// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mCellDecorations;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, Graphics, contnrs,
  mNullables, mMaps;

resourcestring
  SDecorationBackgroundColor = 'background-color';
  SDecorationTextColor = 'text-color';
  SDecorationBold = 'bold';
  SDecorationItalic = 'italic';

const
  DECORATE_ALL_FIELDS_FIELDNAME = '*';

type

  { TmCellDecoration }

  TmCellDecoration = class
  strict private
    FFieldName: string;
    FCondition: TNullableString;
    FBackgroundColor: TNullableColor;
    FTextColor: TNullableColor;
    FTextBold: TNullableBoolean;
    FTextItalic: TNullableBoolean;
  private
    FOnChangeFieldName : TNotifyEvent;
    procedure SetFieldName(AValue: string);
  public
    constructor Create;
    destructor Destroy; override;

    function DecorationAsString: String;
    procedure Assign(const aSource : TmCellDecoration);

    property FieldName: string read FFieldName write SetFieldName;
    property Condition: TNullableString read FCondition;
    property BackgroundColor: TNullableColor read FBackgroundColor;
    property TextColor: TNullableColor read FTextColor;
    property TextBold: TNullableBoolean read FTextBold;
    property TextItalic: TNullableBoolean read FTextItalic;
  end;

  { TmListOfDecorations }

  TmListOfDecorations = class
  strict private
    FList : TList;
  public
    constructor Create;
    destructor Destroy; override;
    function Count : integer;
    function Get(const aIndex : integer) : TmCellDecoration;
    procedure Add(aCellDecoration: TmCellDecoration);
  end;

  { TmCellDecorations }

  TmCellDecorations = class
  strict private
    FList: TObjectList;
    FIndex: TmStringDictionary;
    procedure OnChangeFieldName (aSender : TObject);
  public
    constructor Create;
    destructor Destroy; override;
    function Add: TmCellDecoration;
    function Count: integer;
    function Get(const aIndex: integer): TmCellDecoration;
    procedure Clear;
    procedure FindByFieldName (const aFieldName: string; out aList : TmListOfDecorations);
  end;

implementation

uses
  SysUtils;

{ TmListOfDecorations }

constructor TmListOfDecorations.Create;
begin
  FList := TList.Create;
end;

destructor TmListOfDecorations.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TmListOfDecorations.Count: integer;
begin
  Result := FList.Count;
end;

function TmListOfDecorations.Get(const aIndex: integer): TmCellDecoration;
begin
  Result := TmCellDecoration(FList.Items[aIndex]);
end;

procedure TmListOfDecorations.Add(aCellDecoration: TmCellDecoration);
begin
  FList.Add(aCellDecoration);
end;

{ TmCellDecorations }

procedure TmCellDecorations.OnChangeFieldName(aSender: TObject);
begin
  FIndex.Clear;
end;

constructor TmCellDecorations.Create;
begin
  FList:= TObjectList.Create(true);
  FIndex:= TmStringDictionary.Create(true);
end;

destructor TmCellDecorations.Destroy;
begin
  FList.Free;
  FIndex.Free;
  inherited Destroy;
end;

function TmCellDecorations.Add: TmCellDecoration;
begin
  Result:= TmCellDecoration.Create;
  FList.Add(Result);
  Result.FOnChangeFieldName:= Self.OnChangeFieldName;
  FIndex.Clear;
end;

function TmCellDecorations.Count: integer;
begin
  Result:= FList.Count;
end;

function TmCellDecorations.Get(const aIndex: integer): TmCellDecoration;
begin
  Result:= FList.Items[aIndex] as TmCellDecoration;
end;

procedure TmCellDecorations.Clear;
begin
  FList.Clear;
  FIndex.Clear;
end;

procedure TmCellDecorations.FindByFieldName (const aFieldName: string; out aList : TmListOfDecorations);
var
  i : integer;
  tmpList : TmListOfDecorations;
begin
  if FIndex.Count = 0 then
  begin
    for i := 0 to Self.Count -1 do
    begin
      tmpList := FIndex.Find(Self.Get(i).FieldName) as TmListOfDecorations;
      if not Assigned(tmpList) then
      begin
        tmpList := TmListOfDecorations.Create;
        FIndex.Add(Self.Get(i).FieldName, tmpList);
      end;
      tmpList.Add(Self.Get(i));
    end;
  end;
  aList := FIndex.Find(aFieldName) as TmListOfDecorations;
end;

{ TmCellDecoration }

procedure TmCellDecoration.SetFieldName(AValue: string);
var
  str: string;
begin
  str:= Uppercase(AValue);
  if FFieldName=str then Exit;
  FFieldName:=str;
  if Assigned(FOnChangeFieldName) then
    FOnChangeFieldName(Self);
end;

constructor TmCellDecoration.Create;
begin
  FFieldName:= '';
  FCondition:= TNullableString.Create;
  FBackgroundColor:= TNullableColor.Create();
  FTextColor:= TNullableColor.Create();
  FTextBold:= TNullableBoolean.Create();
  FTextItalic:= TNullableBoolean.Create();
end;

destructor TmCellDecoration.Destroy;
begin
  FCondition.Free;
  FBackgroundColor.Free;
  FTextColor.Free;
  FTextBold.Free;
  FTextItalic.Free;

  inherited Destroy;
end;

function TmCellDecoration.DecorationAsString: String;
var
  sep : String;
begin
  Result := '';
  sep := '';
  if FBackgroundColor.NotNull then
  begin
    Result := Result + sep + SDecorationBackgroundColor + ': ' + FBackgroundColor.AsString;
    sep := '; ';
  end;
  if FTextColor.NotNull then
  begin
    Result := Result + sep + SDecorationTextColor + ': ' + FTextColor.AsString;
    sep := '; ';
  end;
  if FTextBold.AsBoolean then
  begin
    Result := Result + sep + SDecorationBold;
    sep := '; ';
  end;
  if FTextItalic.AsBoolean then
  begin
    Result := Result + sep + SDecorationItalic;
    sep := '; ';
  end;
end;

procedure TmCellDecoration.Assign(const aSource: TmCellDecoration);
begin
  FieldName := aSource.FieldName;
  Condition.Assign(aSource.Condition);
  BackgroundColor.Assign(aSource.BackgroundColor);
  TextColor.Assign(aSource.TextColor);
  TextBold.Assign(aSource.TextBold);
  TextItalic.Assign(aSource.TextItalic);
end;

end.
