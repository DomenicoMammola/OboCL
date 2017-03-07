// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
//
// To enable database access throw the SDAC components, you must purchase
// the SDAC components library from Devart here:
// https://www.devart.com/sdac/

unit mDatabaseConnectionImplOnSdac;

interface

{$MODE DELPHI}

uses
  Classes, DB,

  MSAccess, MsClasses,

  mDatabaseConnectionImpl;

type

  { TSdacDatabaseConnectionImpl }

  TSdacDatabaseConnectionImpl = class(TmDatabaseConnectionImpl)
  private
    FConnection : TMSConnection;
  public
    constructor Create(); override;
    destructor Destroy; override;

    procedure Connect; override;
    procedure Close; override;
    function GetName : String; override;
    class function GetImplementationName : String;
  end;

  { TSdacDatabaseQueryImpl }

  TSdacDatabaseQueryImpl = class (TmDatabaseQueryImpl)
  private
    FConnectionImpl : TSdacDatabaseConnectionImpl;
    FQuery: TMSQuery;
  protected
    procedure SetDatabaseConnectionImpl (value : TmDatabaseConnectionImpl); override;
    function GetDatabaseConnectionImpl : TmDatabaseConnectionImpl; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure SetSQL (aValue : TStringList); override;
    function SameSQL (aValue : TStringList): boolean; override;

    procedure Open; override;
    procedure Close; override;
    procedure Prepare; override;
    procedure Unprepare; override;
    procedure First; override;
    procedure Next; override;
    function Eof : boolean; override;
    function AsDataset : TDataset; override;
    function ParamByName(const Value: string): TParam; override;
    function Prepared : boolean; override;
  end;



implementation

uses
  mDatabaseConnectionClasses, mDatabaseConnectionImplRegister,
  SysUtils;

{ TSdacDatabaseQueryImpl }

procedure TSdacDatabaseQueryImpl.SetDatabaseConnectionImpl(value: TmDatabaseConnectionImpl);
begin
  FConnectionImpl := value as TSdacDatabaseConnectionImpl;
  FQuery.Connection := FConnectionImpl.FConnection;
end;

function TSdacDatabaseQueryImpl.GetDatabaseConnectionImpl: TmDatabaseConnectionImpl;
begin
  Result := FConnectionImpl;
end;

constructor TSdacDatabaseQueryImpl.Create;
begin
  FQuery :=  TMSQuery.Create(nil);
end;

destructor TSdacDatabaseQueryImpl.Destroy;
begin
  FQuery.Free;
  inherited Destroy;
end;

procedure TSdacDatabaseQueryImpl.SetSQL(aValue: TStringList);
begin
  FQuery.SQL.Clear;
  FQuery.SQL.AddStrings(aValue);
end;

function TSdacDatabaseQueryImpl.SameSQL(aValue: TStringList): boolean;
begin
  Result := FQuery.SQL.Count <> aValue.Count;
  if (not Result) then
    Result := (CompareStr(FQuery.SQL.Text, aValue.Text) = 0);
end;

procedure TSdacDatabaseQueryImpl.Open;
begin
  FQuery.Open;
end;

procedure TSdacDatabaseQueryImpl.Close;
begin
  FQuery.Close;
end;

procedure TSdacDatabaseQueryImpl.Prepare;
begin
  FQuery.Prepare;;
end;

procedure TSdacDatabaseQueryImpl.Unprepare;
begin
  FQuery.UnPrepare;
end;

procedure TSdacDatabaseQueryImpl.First;
begin
  FQuery.First;
end;

procedure TSdacDatabaseQueryImpl.Next;
begin
  FQuery.Next;
end;

function TSdacDatabaseQueryImpl.Eof: boolean;
begin
  Result := FQuery.EOF;
end;

function TSdacDatabaseQueryImpl.AsDataset: TDataset;
begin
  Result := FQuery;
end;

function TSdacDatabaseQueryImpl.ParamByName(const Value: string): TParam;
begin
  Result := FQuery.ParamByName(Value);
end;

function TSdacDatabaseQueryImpl.Prepared: boolean;
begin
  Result := FQuery.Prepared;
end;


{ TSdacDatabaseConnection }

constructor TSdacDatabaseConnectionImpl.Create;
begin
  FConnection := TMSConnection.Create(nil);
  FConnection.Options.DefaultLockTimeout := 60000;
end;

destructor TSdacDatabaseConnectionImpl.Destroy;
begin
  FConnection.Free;
  inherited Destroy;
end;

procedure TSdacDatabaseConnectionImpl.Connect;
begin
  if (not FConnection.Connected) then
  begin
    FConnection.Server:= FConnectionInfo.Server;
    FConnection.Database:= FConnectionInfo.DatabaseName;
    FConnection.Options.Provider:= MSClasses.prAuto;
    if (FConnectionInfo.WindowsIntegratedSecurity) then
    begin
      FConnection.Options.PersistSecurityInfo:= true;
      FConnection.Authentication:= auWindows;
    end
    else
    begin
      FConnection.Username := FConnectionInfo.UserName;
      FConnection.Password := FConnectionInfo.Password;
    end;

    FConnection.Connect;
  end;
end;

procedure TSdacDatabaseConnectionImpl.Close;
begin
  if FConnection.Connected then
    FConnection.Close();
end;

function TSdacDatabaseConnectionImpl.GetName: String;
begin
  Result := TSdacDatabaseConnectionImpl.GetImplementationName;
end;

class function TSdacDatabaseConnectionImpl.GetImplementationName: String;
begin
  Result := 'sdac';
end;

initialization

  GetDataConnectionClassesRegister.RegisterImplementations(TSdacDatabaseConnectionImpl.GetImplementationName, dvSQLServer, TSdacDatabaseConnectionImpl, TSdacDatabaseQueryImpl);

end.
