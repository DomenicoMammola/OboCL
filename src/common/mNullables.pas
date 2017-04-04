// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mNullables;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

type

  { TAbstractNullable }

  TAbstractNullable = class abstract
  protected
    FIsNull : Boolean;
    function GetNotNull : Boolean;
  public
    constructor Create(); virtual;
    property IsNull : Boolean read FIsNull write FIsNull;
    property NotNull : Boolean read GetNotNull;
  end;

  { TNullableString }

  TNullableString = class(TAbstractNullable)
  private
    FValue : String;
    function GetValue: String;
    procedure SetValue(AValue: String);
  public
    class function CreateNew(aValue: String): TNullableString; overload;
    class function CreateNew(): TNullableString; overload;

    procedure Assign(aSource : TNullableString);

    property Value : String read GetValue write SetValue;
  end;

  { TNullableUnicodeString }

  TNullableUnicodeString = class (TAbstractNullable)
    private
      FValue : UnicodeString;
      function GetValue: UnicodeString;
      procedure SetValue(AValue: UnicodeString);
    public
      class function CreateNew(aValue: UnicodeString): TNullableUnicodeString; overload;
      class function CreateNew(): TNullableUnicodeString; overload;

      procedure Assign(aSource : TNullableUnicodeString);

      property Value : UnicodeString read GetValue write SetValue;
  end;

  { TNullableAnsiString }

  TNullableAnsiString = class (TAbstractNullable)
    private
      FValue : AnsiString;
      function GetValue: AnsiString;
      procedure SetValue(AValue: AnsiString);
    public
      class function CreateNew(aValue: AnsiString): TNullableAnsiString; overload;
      class function CreateNew(): TNullableAnsiString; overload;

      procedure Assign(aSource : TNullableAnsiString);

      property Value : AnsiString read GetValue write SetValue;
  end;


  { TNullableDateTime }

  TNullableDateTime = class(TAbstractNullable)
    private
      FValue : TDateTime;
      function GetValue : TDateTime;
      procedure SetValue (AValue : TDateTime);
    public
      class function CreateNew(aValue : TDateTime) : TNullableDateTime; overload;
      class function CreateNew() : TNullableDateTime; overload;

      procedure Assign(aSource : TNullableDateTime);

      property Value : TDateTime read GetValue write SetValue;
  end;

  { TNullableDouble }

  TNullableDouble = class (TAbstractNullable)
  private
    FValue : Double;
    function GetValue : Double;
    procedure SetValue (AValue : Double);
  public
    class function CreateNew(aValue : Double) : TNullableDouble; overload;
    class function CreateNew() : TNullableDouble; overload;

    procedure Assign(aSource : TNullableDouble);

    property Value : Double read GetValue write SetValue;
  end;

  { TNullableBoolean }

  TNullableBoolean = class (TAbstractNullable)
  private
    FValue : Boolean;
    function GetValue : Boolean;
    procedure SetValue (AValue : Boolean);
  public
    class function CreateNew(aValue : Boolean): TNullableBoolean; overload;
    class function CreateNew(): TNullableBoolean; overload;

    procedure Assign(aSource : TNullableBoolean);

    property Value : Boolean read GetValue write SetValue;
  end;


implementation

{ TNullableBoolean }

function TNullableBoolean.GetValue: Boolean;
begin
  Result := FValue;
end;

procedure TNullableBoolean.SetValue(AValue: Boolean);
begin
  FValue:= aValue;
  FIsNull := false;
end;

class function TNullableBoolean.CreateNew(aValue: Boolean): TNullableBoolean;
var
  tmp : TNullableBoolean;
begin
  tmp := TNullableBoolean.Create();
  tmp.Value := aValue;
  Result := tmp;
end;

class function TNullableBoolean.CreateNew: TNullableBoolean;
begin
  Result := TNullableBoolean.Create();
end;

procedure TNullableBoolean.Assign(aSource: TNullableBoolean);
begin
  if not aSource.IsNull then
    Self.Value := aSource.Value
  else
    Self.IsNull := true;
end;

{ TNullableString }

function TNullableString.GetValue: String;
begin
  if (FIsNull) then
       Result := null
    else
      Result := FValue;
end;

procedure TNullableString.SetValue(AValue: String);
begin
  FValue:= aValue;
  FIsNull := False;
end;

class function TNullableString.CreateNew(aValue: String): TNullableString;
var
  tmp : TNullableString;
begin
  tmp := TNullableString.Create();
  tmp.Value := aValue;
  Result := tmp;
end;

class function TNullableString.CreateNew: TNullableString;
begin
  Result := TNullableString.Create();
end;

procedure TNullableString.Assign(aSource: TNullableString);
begin
  if not aSource.IsNull then
    Self.Value := aSource.Value
  else
    Self.IsNull := true;
end;

{ TNullableDouble }

function TNullableDouble.GetValue: Double;
begin
  Result := FValue;
end;

procedure TNullableDouble.SetValue(AValue: Double);
begin
  FValue:= aValue;
  FIsNull := false;
end;

class function TNullableDouble.CreateNew(aValue: Double): TNullableDouble;
var
  tmp : TNullableDouble;
begin
  tmp := TNullableDouble.Create();
  tmp.Value := aValue;
  Result := tmp;
end;

class function TNullableDouble.CreateNew: TNullableDouble;
begin
  Result := TNullableDouble.Create();
end;

procedure TNullableDouble.Assign(aSource: TNullableDouble);
begin
  if not aSource.IsNull then
    Self.Value := aSource.Value
  else
    Self.IsNull := true;
end;

{ TNullableAnsiString }

function TNullableAnsiString.GetValue: AnsiString;
begin
  if (FIsNull) then
     Result := null
  else
    Result := FValue;
end;

procedure TNullableAnsiString.SetValue(AValue: AnsiString);
begin
  FValue:= aValue;
  FIsNull := false;
end;

class function TNullableAnsiString.CreateNew(aValue: AnsiString): TNullableAnsiString;
var
  tmp : TNullableAnsiString;
begin
  tmp := TNullableAnsiString.Create();
  tmp.Value := aValue;
  Result := tmp;
end;

class function TNullableAnsiString.CreateNew: TNullableAnsiString;
begin
  Result := TNullableAnsiString.Create();
end;

procedure TNullableAnsiString.Assign(aSource: TNullableAnsiString);
begin
  if not aSource.IsNull then
    Self.Value := aSource.Value
  else
    Self.IsNull := true;
end;

{ TNullableDateTime }

function TNullableDateTime.GetValue: TDateTime;
begin
  Result := FValue;
end;

procedure TNullableDateTime.SetValue(AValue: TDateTime);
begin
  FValue:= aValue;
  FIsNull := false;
end;

class function TNullableDateTime.CreateNew(aValue: TDateTime): TNullableDateTime;
var
  tmp : TNullableDateTime;
begin
  tmp := TNullableDateTime.Create();
  tmp.Value := aValue;
  Result := tmp;
end;

class function TNullableDateTime.CreateNew: TNullableDateTime;
begin
  Result := TNullableDateTime.Create();
end;

procedure TNullableDateTime.Assign(aSource: TNullableDateTime);
begin
  if not aSource.IsNull then
    Self.Value := aSource.Value
  else
    Self.IsNull := true;
end;

{ TAbstractNullable }

function TAbstractNullable.GetNotNull: Boolean;
begin
  Result := not FIsNull;
end;

constructor TAbstractNullable.Create;
begin
  FIsNull := true;
end;

{ TNullableUnicodeString }


function TNullableUnicodeString.GetValue: UnicodeString;
begin
  if (FIsNull) then
     Result := null
  else
    Result := FValue;
end;

procedure TNullableUnicodeString.SetValue(AValue: UnicodeString);
begin
  FValue:= aValue;
  FIsNull := False;
end;


class function TNullableUnicodeString.CreateNew(aValue: UnicodeString): TNullableUnicodeString;
var
  tmp : TNullableUnicodeString;
begin
  tmp := TNullableUnicodeString.Create();
  tmp.Value := aValue;
  Result := tmp;
end;

class function TNullableUnicodeString.CreateNew: TNullableUnicodeString;
begin
  Result := TNullableUnicodeString.Create();
end;

procedure TNullableUnicodeString.Assign(aSource: TNullableUnicodeString);
begin
  if not aSource.IsNull then
    Self.Value := aSource.Value
  else
    Self.IsNull := true;
end;

end.
