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
  mNullables, StrHashMap;

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

    property FieldName: string read FFieldName write SetFieldName;
    property Condition: TNullableString read FCondition;
    property BackgroundColor: TNullableColor read FBackgroundColor;
    property TextColor: TNullableColor read FTextColor;
    property TextBold: TNullableBoolean read FTextBold;
    property TextItalic: TNullableBoolean read FTextItalic;
  end;

  { TmCellDecorations }

  TmCellDecorations = class
  strict private
    FList: TObjectList;
    FIndex: TStringHashMap;
    procedure OnChangeFieldName (aSender : TObject);
  public
    constructor Create;
    destructor Destroy; override;
    function Add: TmCellDecoration;
    function Count: integer;
    function Get(const aIndex: integer): TmCellDecoration;
    procedure Clear;
    function FindByFieldName (const aFieldName: string): TmCellDecoration;
  end;

implementation

uses
  SysUtils;

{ TmCellDecorations }

procedure TmCellDecorations.OnChangeFieldName(aSender: TObject);
begin
  FIndex.Clear;
end;

constructor TmCellDecorations.Create;
begin
  FList:= TObjectList.Create(true);
  FIndex:= TStringHashMap.Create();
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

function TmCellDecorations.FindByFieldName(const aFieldName: string): TmCellDecoration;
var
  i : integer;
  tmp : pointer;
begin
  if FIndex.Count = 0 then
  begin
    for i := 0 to Self.Count -1 do
      FIndex.Add(Self.Get(i).FieldName, Self.Get(i));
  end;
  if FIndex.Find(Uppercase(aFieldName), tmp) then
    Result := TmCellDecoration(tmp)
  else
    Result := nil;
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

end.
