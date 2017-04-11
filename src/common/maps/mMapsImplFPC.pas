// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mMapsImplFPC;

interface

uses
  fgl,
  mMapsImpl;

type

  TmStringDictionary = specialize TFPGMap<string, TObject>;

  { TmStringDictionaryImplFPC }

  TmStringDictionaryImplFPC = class(TmStringDictionaryImpl)
  strict private
    FInternalDictionary : TmStringDictionary;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure _Add(const aStringKey : String; aObject : TObject); override;
    procedure _Clear; override;
    function _Find(const aStringKey : String) : TObject; override;
    function _Count: integer; override;
  end;

  TmIntegerDictionary = specialize TFPGMap<integer, TObject>;

  { TmIntegerDictionaryImplFPC }

  TmIntegerDictionaryImplFPC = class (TmIntegerDictionaryImpl)
  strict private
    FInternalDictionary : TmIntegerDictionary;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure _Add(const aIntegerKey : integer; aObject : TObject); override;
    procedure _Clear; override;
    function _Find(const aIntegerKey : integer) : TObject; override;
    function _Count : integer; override;
  end;

implementation

{ TmIntegerDictionaryImplFPC }

constructor TmIntegerDictionaryImplFPC.Create;
begin
  FInternalDictionary := TmIntegerDictionary.Create;
end;

destructor TmIntegerDictionaryImplFPC.Destroy;
begin
  FInternalDictionary.Free;
  inherited Destroy;
end;

procedure TmIntegerDictionaryImplFPC._Add(const aIntegerKey: integer; aObject: TObject);
begin
  FInternalDictionary.Add(aIntegerKey, aObject);
end;

procedure TmIntegerDictionaryImplFPC._Clear;
begin
  FInternalDictionary.Clear;
end;

function TmIntegerDictionaryImplFPC._Find(const aIntegerKey: integer): TObject;
var
  tmpIndex : integer;
begin
  tmpIndex := FInternalDictionary.IndexOf(aIntegerKey);
  if tmpIndex >= 0 then
  begin
    Result := FInternalDictionary.Data[tmpIndex];
  end
  else
    Result := nil;
end;

function TmIntegerDictionaryImplFPC._Count: integer;
begin
  Result := FInternalDictionary.Count;
end;

{ TmStringDictionaryImplFPC }

constructor TmStringDictionaryImplFPC.Create;
begin
  FInternalDictionary := TmStringDictionary.Create;
end;

destructor TmStringDictionaryImplFPC.Destroy;
begin
  FInternalDictionary.Free;
  inherited Destroy;
end;

procedure TmStringDictionaryImplFPC._Add(const aStringKey: String; aObject: TObject);
begin
  FInternalDictionary.Add(aStringKey, aObject);
end;

procedure TmStringDictionaryImplFPC._Clear;
begin
  FInternalDictionary.Clear;
end;

function TmStringDictionaryImplFPC._Find(const aStringKey: String): TObject;
var
  tmpIndex : integer;
begin
  tmpIndex := FInternalDictionary.IndexOf(aStringKey);
  if tmpIndex >= 0 then
    Result := FInternalDictionary.Data[tmpIndex]
  else
    Result := nil;
end;

function TmStringDictionaryImplFPC._Count: integer;
begin
  Result := FInternalDictionary.Count;
end;

end.
