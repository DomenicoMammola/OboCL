// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mSQLDialectExpertImplRegister;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  Contnrs,
  mSQLDialectExpertImpl, mDatabaseConnectionClasses;

type

  { TmSQLDialectExpertImplRegister }

  TmSQLDialectExpertImplRegister = class
  strict private
    FImplementationsList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterImplementations (const VendorType : TmDatabaseVendor; const SQLExpertImplementationClass: TSQLDialectExpertImplementationClass);

    function GetImpl (const VendorType : TmDatabaseVendor) : TSQLDialectExpertImplementationClass;
  end;

  function GetSQLDialectExpertsRegister : TmSQLDialectExpertImplRegister;

implementation

uses
  SysUtils;

type
  TImplementationClassesShell = class
  public
    VendorType : TmDatabaseVendor;
    ExpertClass : TSQLDialectExpertImplementationClass;
  end;

var
  _Register : TmSQLDialectExpertImplRegister;

function GetSQLDialectExpertsRegister: TmSQLDialectExpertImplRegister;
begin
  if _Register = nil then
    _Register := TmSQLDialectExpertImplRegister.Create;
  Result := _Register;
end;

{ TmSQLDialectExpertImplRegister }

constructor TmSQLDialectExpertImplRegister.Create;
begin
  inherited Create;
  FImplementationsList := TObjectList.Create(true);
end;

destructor TmSQLDialectExpertImplRegister.Destroy;
begin
  FImplementationsList.Free;
  inherited Destroy;
end;

procedure TmSQLDialectExpertImplRegister.RegisterImplementations(const VendorType: TmDatabaseVendor;
  const SQLExpertImplementationClass: TSQLDialectExpertImplementationClass);
var
  Shell : TImplementationClassesShell;
begin
  Shell := TImplementationClassesShell.Create;
  Shell.VendorType:= VendorType;
  Shell.ExpertClass:= SQLExpertImplementationClass;
  FImplementationsList.Add(Shell);
end;

function TmSQLDialectExpertImplRegister.GetImpl(const VendorType: TmDatabaseVendor): TSQLDialectExpertImplementationClass;
var
  i : integer;
begin
  for i := 0 to FImplementationsList.Count - 1 do
  begin
    if (FImplementationsList.Items[i] as TImplementationClassesShell).VendorType = VendorType then
    begin
      Result := (FImplementationsList.Items[i] as TImplementationClassesShell).ExpertClass;
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
