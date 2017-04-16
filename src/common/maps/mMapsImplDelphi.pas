// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mMapsImplDelphi;

interface

uses
  Generics.Collections,
  mMapsImpl;

type

  TmStringDictionaryImplDelphi = class(TmStringDictionaryImpl)
  strict private
    FInternalDictionary : TObjectDictionary<String, TObject>;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure _Add(const aStringKey : String; aObject : TObject); override;
    procedure _Clear; override;
    function _Find(const aStringKey : String) : TObject; override;
    function _Count : integer; override;
    procedure _Remove(const aStringKey : String); override;
  end;

  TmIntegerDictionaryImplDelphi = class (TmIntegerDictionaryImpl)
  strict private
    FInternalDictionary : TObjectDictionary<Integer,TObject>;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure _Add(const aIntegerKey : integer; aObject : TObject); override;
    procedure _Clear; override;
    function _Find(const aIntegerKey : integer) : TObject; override;
    function _Count : integer; override;
    procedure _Remove(const aIntegerKey : integer); override;
  end;

implementation

{ TmStringDictionaryImplDelphi }

constructor TmStringDictionaryImplDelphi.Create;
begin
  inherited;
  FInternalDictionary := TObjectDictionary<String,TObject>.Create();
end;

destructor TmStringDictionaryImplDelphi.Destroy;
begin
  FInternalDictionary.Free;
  inherited;
end;

procedure TmStringDictionaryImplDelphi._Add(const aStringKey: String; aObject: TObject);
begin
  FInternalDictionary.Add(aStringKey, aObject);

end;

procedure TmStringDictionaryImplDelphi._Clear;
begin
  FInternalDictionary.Clear;
end;

function TmStringDictionaryImplDelphi._Count: integer;
begin
  Result := FInternalDictionary.Count;
end;

function TmStringDictionaryImplDelphi._Find(const aStringKey: String): TObject;
var
  tmp : TObject;
begin
  if FInternalDictionary.TryGetValue(aStringKey, tmp) then
    Result := tmp
  else
    Result := nil;
end;

procedure TmStringDictionaryImplDelphi._Remove(const aStringKey: String);
begin
  FInternalDictionary.Remove(aStringKey);
end;

{ TmIntegerDictionaryImplDelphi }

constructor TmIntegerDictionaryImplDelphi.Create;
begin
  inherited;
  FInternalDictionary := TObjectDictionary<integer,TObject>.Create;
end;

destructor TmIntegerDictionaryImplDelphi.Destroy;
begin
  FInternalDictionary.Free;
  inherited;
end;

procedure TmIntegerDictionaryImplDelphi._Add(const aIntegerKey: integer; aObject: TObject);
begin
  FInternalDictionary.Add(aIntegerKey, aObject);
end;

procedure TmIntegerDictionaryImplDelphi._Clear;
begin
  FInternalDictionary.Clear;
end;

function TmIntegerDictionaryImplDelphi._Count: integer;
begin
  Result := FInternalDictionary.Count;
end;

function TmIntegerDictionaryImplDelphi._Find(const aIntegerKey: integer): TObject;
var
  tmp : TObject;
begin
  if FInternalDictionary.TryGetValue(aIntegerKey, tmp) then
    Result := tmp
  else
    Result := nil;
end;

procedure TmIntegerDictionaryImplDelphi._Remove(const aIntegerKey: integer);
begin
  FInternalDictionary.Remove(aIntegerKey);
end;

end.
