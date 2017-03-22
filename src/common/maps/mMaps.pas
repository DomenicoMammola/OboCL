// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mMaps;

interface

uses
  mMapsImpl;

type

  { TmStringDictionary }

  TmStringDictionary = class
    strict private
      FImpl : TmStringDictionaryImpl;
    public
      constructor Create();
      destructor Destroy; override;

      procedure Add(const aStringKey : String; aObject : TObject);
      function Find(const aStringKey : String) : TObject;
      procedure Clear;
  end;




implementation

{ TmStringDictionary }

constructor TmStringDictionary.Create;
begin
  FImpl := CreateTmStringDictionary;
end;

destructor TmStringDictionary.Destroy;
begin
  FImpl.Free;
  inherited Destroy;
end;

procedure TmStringDictionary.Add(const aStringKey: String; aObject: TObject);
begin
  FImpl._Add(aStringKey, aObject);
end;

function TmStringDictionary.Find(const aStringKey: String): TObject;
begin
  Result := FImpl._Find(aStringKey);
end;

procedure TmStringDictionary.Clear;
begin
  FImpl._Clear;
end;

end.
