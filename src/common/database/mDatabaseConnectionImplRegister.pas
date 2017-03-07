// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mDatabaseConnectionImplRegister;

interface

uses
  Contnrs,
  mDatabaseConnectionImpl, mDatabaseConnectionClasses;

type
  { TmDatabaseConnectionImplRegister }

  TmDatabaseConnectionImplRegister = class
  strict private
    FImplementationsList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterImplementations (const AName : String; const VendorType : TmDatabaseVendor; const ConnectionImplementationClass : TmDatabaseConnectionImplClass;
      const QueryImplementationClass : TmDatabaseQueryImplClass);


    function GetConnectionImpl (const VendorType : TmDatabaseVendor) : TmDatabaseConnectionImpl;
    function GetQueryImpl(const AName : String): TmDatabaseQueryImpl;
  end;

  function GetDataConnectionClassesRegister : TmDatabaseConnectionImplRegister;

implementation

uses
  SysUtils;

type
  TImplementationClassesShell = class
  public
    Name : String;
    VendorType : TmDatabaseVendor;
    ConnectionClass : TmDatabaseConnectionImplClass;
    QueryClass : TmDatabaseQueryImplClass;
  end;

var
  _Register : TmDatabaseConnectionImplRegister;

function GetDataConnectionClassesRegister : TmDatabaseConnectionImplRegister;
begin
  if _Register = nil then
    _Register := TmDatabaseConnectionImplRegister.Create;
  Result := _Register;
end;

{ TmDatabaseConnectionImplRegister }

constructor TmDatabaseConnectionImplRegister.Create;
begin
  FImplementationsList := TObjectList.Create(true);
end;

destructor TmDatabaseConnectionImplRegister.Destroy;
begin
  FImplementationsList.Free;
  inherited Destroy;
end;

procedure TmDatabaseConnectionImplRegister.RegisterImplementations (const AName : String; const VendorType : TmDatabaseVendor; const ConnectionImplementationClass : TmDatabaseConnectionImplClass;
    const QueryImplementationClass : TmDatabaseQueryImplClass);
var
  temp : TImplementationClassesShell;
begin
  temp := TImplementationClassesShell.Create;
  temp.Name := AName;
  temp.VendorType:= VendorType;
  temp.ConnectionClass:= ConnectionImplementationClass;
  temp.QueryClass:= QueryImplementationClass;
  FImplementationsList.Add(temp);
end;


function TmDatabaseConnectionImplRegister.GetConnectionImpl(const VendorType: TmDatabaseVendor): TmDatabaseConnectionImpl;
var
  i : integer;
  TempShell : TImplementationClassesShell;
begin
  for i := 0 to FImplementationsList.Count -1 do
  begin
    TempShell := FImplementationsList[i] as TImplementationClassesShell;
    if TempShell.VendorType = VendorType then
    begin
      Result := TempShell.ConnectionClass.Create;
      exit;
    end;
  end;
  Result := nil;
end;

function TmDatabaseConnectionImplRegister.GetQueryImpl(const AName: String): TmDatabaseQueryImpl;
var
  i : integer;
  TempShell : TImplementationClassesShell;
begin
  for i := 0 to FImplementationsList.Count -1 do
  begin
    TempShell := FImplementationsList[i] as TImplementationClassesShell;
    if CompareText(TempShell.Name, AName) = 0 then
    begin
      Result := TempShell.QueryClass.Create;
      exit;
    end;
  end;
  Result := nil;
end;

initialization
_Register := nil;

finalization
if Assigned(_Register) then
  FreeAndNil(_Register);

end.
