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
// To enable database access throw the sqldb components to MsSqlServer
// please check:
// http://wiki.freepascal.org/mssqlconn

unit mDatabaseConnectionImplOnSqldb;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, DB,

  sqldb,

  mDatabaseConnectionClasses,
  mDatabaseConnectionImpl;

type


  { TSqldbDatabaseConnectionImpl }

  TSqldbDatabaseConnectionImpl = class(TmDatabaseConnectionImpl)
  private
    FConnection : TSQLConnection;
    FTransaction : TSQLTransaction;
  protected
    procedure SetConnectionInfo(AValue: TmDatabaseConnectionInfo); override;
  public
    constructor Create(); override;
    destructor Destroy; override;

    procedure Connect; override;
    procedure Close; override;
    function GetName : String; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;

    class function GetImplementationName : String;
  end;

  { TSqldbDatabaseQueryImpl }

  TSqldbDatabaseQueryImpl = class (TmDatabaseQueryImpl)
  private
    FConnectionImpl : TSqldbDatabaseConnectionImpl;
    FQuery: TSQLQuery;
    FPrepared : boolean;
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
    function ParamCount : integer; override;
    procedure SetParamValue(aParam : TmQueryParameter); override;
    function GetParam (aIndex : integer) : TParam; override;
    function Prepared : boolean; override;
  end;

  { TSdacDatabaseCommandImpl }

  { TSqldbDatabaseCommandImpl }

  TSqldbDatabaseCommandImpl = class (TmDatabaseCommandImpl)
  private
    FConnectionImpl : TSqldbDatabaseConnectionImpl;
    FCommand : TSQLQuery;
    FPrepared : boolean;
  protected
    procedure SetDatabaseConnectionImpl (value : TmDatabaseConnectionImpl); override;
    function GetDatabaseConnectionImpl : TmDatabaseConnectionImpl; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure SetSQL (aValue : TStringList); override;
    function SameSQL (aValue : TStringList): boolean; override;

    function Execute : integer; override;

    procedure Prepare; override;
    procedure Unprepare; override;
    function ParamCount : integer; override;
    procedure SetParamValue(aParam : TmQueryParameter); override;
    function GetParam (aIndex : integer) : TParam; override;

    function Prepared : boolean; override;
  end;



implementation

uses
  mssqlconn,
  mDatabaseConnectionImplRegister, mSQLServerSQLDialect,
  SysUtils;

{ TSqldbDatabaseCommandImpl }

procedure TSqldbDatabaseCommandImpl.SetDatabaseConnectionImpl(value: TmDatabaseConnectionImpl);
begin
  FConnectionImpl := value as TSqldbDatabaseConnectionImpl;
  FCommand.DataBase := FConnectionImpl.FConnection;
end;

function TSqldbDatabaseCommandImpl.GetDatabaseConnectionImpl: TmDatabaseConnectionImpl;
begin
  Result := FConnectionImpl;
end;

constructor TSqldbDatabaseCommandImpl.Create;
begin
  FCommand := TSQLQuery.Create(nil);
  FPrepared := false;
end;

destructor TSqldbDatabaseCommandImpl.Destroy;
begin
  FreeAndNil(FCommand);
  inherited Destroy;
end;

procedure TSqldbDatabaseCommandImpl.SetSQL(aValue: TStringList);
begin
  FCommand.SQL.Clear;
  FCommand.SQL.AddStrings(aValue);
  FPrepared := false;
end;

function TSqldbDatabaseCommandImpl.SameSQL(aValue: TStringList): boolean;
begin
  Result := FCommand.SQL.Count <> aValue.Count;
  if (not Result) then
    Result := (CompareStr(FCommand.SQL.Text, aValue.Text) = 0);
end;

function TSqldbDatabaseCommandImpl.Execute: integer;
begin
  FCommand.ExecSQL();
  Result := FCommand.RowsAffected;
  FPrepared := true;
end;

procedure TSqldbDatabaseCommandImpl.Prepare;
begin
  FCommand.Prepare;
  FPrepared := true;
end;

procedure TSqldbDatabaseCommandImpl.Unprepare;
begin
  FCommand.Unprepare;
  FPrepared := false;
end;

function TSqldbDatabaseCommandImpl.ParamCount: integer;
begin
  Result := FCommand.Params.Count;
end;

procedure TSqldbDatabaseCommandImpl.SetParamValue(aParam: TmQueryParameter);
var
  TmpParam : TParam;
begin
  TmpParam := FCommand.ParamByName(aParam.Name);

  if aParam.IsNull then
  begin
    TmpParam.DataType:= ParameterDataTypeToDataType(aParam.DataType);
    TmpParam.Clear;
    exit;
  end;

  case aParam.DataType of
    ptDate:
      TmpParam.AsDate := aParam.AsDate;
    ptDateTime:
      TmpParam.AsDateTime := aParam.AsDateTime;
    ptString:
      TmpParam.AsString := aParam.AsString;
    ptWideString:
      TmpParam.AsWideString:= aParam.AsWideString;
    ptInteger:
      TmpParam.AsInteger:= aParam.AsInteger;
    ptFloat:
      TmpParam.AsFloat:= aParam.AsFloat;
    else
      raise TmDataConnectionException.Create('Unknown parameter type');
  end;
end;

function TSqldbDatabaseCommandImpl.GetParam(aIndex: integer): TParam;
begin
  Result := FCommand.Params[aIndex];
end;

function TSqldbDatabaseCommandImpl.Prepared: boolean;
begin
  Result := FPrepared;
end;

{ TSqldbDatabaseQueryImpl }

procedure TSqldbDatabaseQueryImpl.SetDatabaseConnectionImpl(value: TmDatabaseConnectionImpl);
begin
  FConnectionImpl := value as TSqldbDatabaseConnectionImpl;
  FQuery.DataBase := FConnectionImpl.FConnection;
end;

function TSqldbDatabaseQueryImpl.GetDatabaseConnectionImpl: TmDatabaseConnectionImpl;
begin
  Result := FConnectionImpl;
end;

constructor TSqldbDatabaseQueryImpl.Create;
begin
  FQuery := TSQLQuery.Create(nil);
  FPrepared := false;
end;

destructor TSqldbDatabaseQueryImpl.Destroy;
begin
  inherited Destroy;
end;

procedure TSqldbDatabaseQueryImpl.SetSQL(aValue: TStringList);
begin
  FQuery.SQL.Clear;
  FQuery.SQL.AddStrings(aValue);
end;

function TSqldbDatabaseQueryImpl.SameSQL(aValue: TStringList): boolean;
begin
  Result := FQuery.SQL.Count <> aValue.Count;
  if (not Result) then
    Result := (CompareStr(FQuery.SQL.Text, aValue.Text) = 0);
end;

procedure TSqldbDatabaseQueryImpl.Open;
begin
  if not FPrepared then
    FPrepared := true;
  FQuery.Open;
end;

procedure TSqldbDatabaseQueryImpl.Close;
begin
  FQuery.Close;
end;

procedure TSqldbDatabaseQueryImpl.Prepare;
begin
  FQuery.Prepare;
  FPrepared := true;
end;

procedure TSqldbDatabaseQueryImpl.Unprepare;
begin
  FQuery.UnPrepare;
  FPrepared := false;
end;

procedure TSqldbDatabaseQueryImpl.First;
begin
  FQuery.First;
end;

procedure TSqldbDatabaseQueryImpl.Next;
begin
  FQuery.Next;
end;

function TSqldbDatabaseQueryImpl.Eof: boolean;
begin
  Result := FQuery.EOF;
end;

function TSqldbDatabaseQueryImpl.AsDataset: TDataset;
begin
  Result := FQuery;
end;

function TSqldbDatabaseQueryImpl.ParamCount: integer;
begin
  Result := FQuery.Params.Count;
end;

procedure TSqldbDatabaseQueryImpl.SetParamValue(aParam: TmQueryParameter);
var
  TmpParam : TParam;
begin
  TmpParam := FQuery.ParamByName(aParam.Name);

  if aParam.IsNull then
  begin
    TmpParam.DataType:= ParameterDataTypeToDataType(aParam.DataType);
    TmpParam.Clear;
    exit;
  end;

  case aParam.DataType of
    ptDate:
      TmpParam.AsDate:= aParam.AsDate;
    ptDateTime:
      TmpParam.AsDateTime := aParam.AsDateTime;
    ptString:
      TmpParam.AsString := aParam.AsString;
    ptWideString:
      TmpParam.AsWideString:= aParam.AsWideString;
    ptInteger:
      TmpParam.AsInteger:= aParam.AsInteger;
    ptFloat:
      TmpParam.AsFloat:= aParam.AsFloat;
  else
    raise TmDataConnectionException.Create('Unknown parameter type');
  end;
end;

function TSqldbDatabaseQueryImpl.GetParam(aIndex: integer): TParam;
begin
  Result := FQuery.Params[aIndex];
end;

function TSqldbDatabaseQueryImpl.Prepared: boolean;
begin
  Result := FPrepared;
end;

{ TSqldbDatabaseConnectionImpl }

procedure TSqldbDatabaseConnectionImpl.SetConnectionInfo(AValue: TmDatabaseConnectionInfo);
begin
  inherited SetConnectionInfo(AValue);
  if Assigned(FConnection) then
    FreeAndNil(FConnection);
  if (FConnectionInfo.VendorType = dvSQLServer) then
  begin
    FConnection := TMSSQLConnection.Create(nil);
    FTransaction.DataBase := FConnection;
  end;
end;

constructor TSqldbDatabaseConnectionImpl.Create;
begin
  FConnection := nil;
  FTransaction := TSQLTransaction.Create(nil);
end;

destructor TSqldbDatabaseConnectionImpl.Destroy;
begin
  FreeAndNil(FTransaction);
  FreeAndNil(FConnection);
  inherited Destroy;
end;

procedure TSqldbDatabaseConnectionImpl.Connect;
begin
  if (not FConnection.Connected) then
  begin
    FConnection.HostName:= FConnectionInfo.Server;
    FConnection.DatabaseName:= FConnectionInfo.DatabaseName;
    if (not FConnectionInfo.WindowsIntegratedSecurity) then
    begin
      FConnection.Username := FConnectionInfo.UserName;
      FConnection.Password := FConnectionInfo.Password;
    end;

    FConnection.Open;
  end;
end;

procedure TSqldbDatabaseConnectionImpl.Close;
begin
  if FConnection.Connected then
    FConnection.Close(false);
end;

function TSqldbDatabaseConnectionImpl.GetName: String;
begin
  Result := TSqldbDatabaseConnectionImpl.GetImplementationName;
end;

procedure TSqldbDatabaseConnectionImpl.StartTransaction;
begin
  FTransaction.StartTransaction;
end;

procedure TSqldbDatabaseConnectionImpl.Commit;
begin
  FTransaction.Commit;
end;

procedure TSqldbDatabaseConnectionImpl.Rollback;
begin
  FTransaction.Rollback;
end;

class function TSqldbDatabaseConnectionImpl.GetImplementationName: String;
begin
  Result := 'sqldb';
end;


initialization

  GetDataConnectionClassesRegister.RegisterImplementations(TSqldbDatabaseConnectionImpl.GetImplementationName, dvSQLServer, TSqldbDatabaseConnectionImpl, TSqldbDatabaseQueryImpl, TSqldbDatabaseCommandImpl);

end.
