// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mBaseClassesAsObjects;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}


interface

type

  { TIntegerObject }

  TIntegerObject = class
  strict private
    FValue : Integer;
  public
    constructor Create(); overload;
    constructor Create(aValue : integer); overload;

    property Value : Integer read FValue write FValue;
  end;

  { TAnsiStringObject }

  TAnsiStringObject = class
  strict private
    FValue : AnsiString;
  public
    constructor Create(); overload;
    constructor Create(aValue : AnsiString); overload;

    property Value : AnsiString read FValue write FValue;
  end;

  { TUnicodeStringObject }

  TUnicodeStringObject = class
  strict private
    FValue : UnicodeString;
  public
    constructor Create(); overload;
    constructor Create(aValue : UnicodeString); overload;

    property Value : UnicodeString read FValue write FValue;
  end;

  { TStringObject }

  TStringObject = class
  strict private
    FValue : String;
  public
    constructor Create(); overload;
    constructor Create(aValue : String); overload;

    property Value : String read FValue write FValue;
  end;

implementation

{ TStringObject }

constructor TStringObject.Create;
begin
  FValue := '';
end;

constructor TStringObject.Create(aValue: String);
begin
  FValue := aValue;
end;

{ TUnicodeStringObject }

constructor TUnicodeStringObject.Create;
begin
  FValue := '';
end;

constructor TUnicodeStringObject.Create(aValue: UnicodeString);
begin
  FValue := aValue;
end;

{ TAnsiStringObject }

constructor TAnsiStringObject.Create;
begin
  FValue := '';
end;

constructor TAnsiStringObject.Create(aValue: AnsiString);
begin
  FValue := aValue;
end;

{ TIntegerObject }

constructor TIntegerObject.Create();
begin
  FValue := 0;
end;

constructor TIntegerObject.Create(aValue: integer); overload;
begin
  FValue := aValue;
end;

end.
