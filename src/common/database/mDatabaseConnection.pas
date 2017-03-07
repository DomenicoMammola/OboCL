// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mDatabaseConnection;

{$MODE DELPHI}
// {$MODE DELPHIUNICODE}

interface

uses
  Classes, DB,
  mDatabaseConnectionClasses, mDatabaseConnectionImpl;

type
  { TmDatabaseConnection }

  TmDatabaseConnection = class
    private
      FConnectionInfo : TmDatabaseConnectionInfo;

      FImplementation : TmDatabaseConnectionImpl;
      procedure CreateImplementation;
    public
      constructor Create;
      destructor Destroy; override;

      procedure Connect;
      procedure Close;

      procedure StartTransaction;
      procedure Commit;
      procedure Rollback;

      property ConnectionInfo : TmDatabaseConnectionInfo read FConnectionInfo write FConnectionInfo;
  end;


  { TmDatabaseQuery }

  TmDatabaseQuery = class
  private
    FDatabaseConnection : TmDatabaseConnection;
    FSQL : TStringList;

    FImplementation : TmDatabaseQueryImpl;
    procedure CreateImplementation;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Open;
    procedure Close;
    procedure First;
    procedure Next;
    function Eof : boolean;
    function AsDataset : TDataset;
    procedure Prepare;
    function Prepared : boolean;
    function ParamByName(const Value: string): TParam;

    property DatabaseConnection : TmDatabaseConnection read FDatabaseConnection write FDatabaseConnection;
    property SQL : TStringList read FSQL;
  end;

  { TmDatabaseCommand }

  TmDatabaseCommand = class abstract
  private
    FDatabaseConnection : TmDatabaseConnection;
    FSQL : TStringList;

    FImplementation : TmDatabaseCommandImpl;
    procedure CreateImplementation;
  public
    constructor Create;
    destructor Destroy; override;

    function Execute : integer; virtual; abstract;
    procedure Prepare;
    function Prepared : boolean;
    function ParamByName(const Value: string): TParam;

    property DatabaseConnection : TmDatabaseConnection read FDatabaseConnection write FDatabaseConnection;
    property SQL : TStringList read FSQL;
  end;

implementation

uses
  SysUtils, mDatabaseConnectionImplRegister;

  { TmDatabaseCommand }

  procedure TmDatabaseCommand.CreateImplementation;
  begin
    if not Assigned(FImplementation) then
    begin
      FImplementation := GetDataConnectionClassesRegister.GetCommandImpl(FDatabaseConnection.FImplementation.GetName);
      FImplementation.DatabaseConnectionImpl := FDatabaseConnection.FImplementation;
    end;
  end;

  constructor TmDatabaseCommand.Create;
  begin
    FImplementation := nil;
    FSQL := TStringList.Create;
  end;

  destructor TmDatabaseCommand.Destroy;
  begin
    if Assigned(FImplementation) then
      FreeAndNil(FImplementation);
    FSQL.Free;
    inherited Destroy;
  end;

  procedure TmDatabaseCommand.Prepare;
  begin
    CreateImplementation;
    if FImplementation.Prepared then
      FImplementation.Unprepare;
    FImplementation.SetSQL(FSQL);
    FImplementation.Prepare;
  end;

  function TmDatabaseCommand.Prepared: boolean;
  begin
    CreateImplementation;
    Result := FImplementation.Prepared;
  end;

  function TmDatabaseCommand.ParamByName(const Value: string): TParam;
  begin
    CreateImplementation;
    Result := FImplementation.ParamByName(Value);
  end;

  { TmDatabaseQuery }

  procedure TmDatabaseQuery.CreateImplementation;
  begin
    if not Assigned(FImplementation) then
    begin
      FImplementation := GetDataConnectionClassesRegister.GetQueryImpl(FDatabaseConnection.FImplementation.GetName);
      FImplementation.DatabaseConnectionImpl := FDatabaseConnection.FImplementation;
    end;
  end;

  constructor TmDatabaseQuery.Create;
  begin
    FImplementation := nil;
    FSQL := TStringList.Create;
  end;

  destructor TmDatabaseQuery.Destroy;
  begin
    if Assigned(FImplementation) then
      FreeAndNil(FImplementation);
    FSQL.Free;
    inherited Destroy;
  end;

  procedure TmDatabaseQuery.Open;
  begin
    CreateImplementation;
    if (not FImplementation.Prepared) or (not FImplementation.SameSQL(FSQL)) then
      Self.Prepare;
    FImplementation.Open;
  end;

  procedure TmDatabaseQuery.Close;
  begin
    CreateImplementation;
    FImplementation.Close;
  end;

  procedure TmDatabaseQuery.First;
  begin
    CreateImplementation;
    FImplementation.First;
  end;

  procedure TmDatabaseQuery.Next;
  begin
    CreateImplementation;
    FImplementation.Next;
  end;

  function TmDatabaseQuery.Eof: boolean;
  begin
    CreateImplementation;
    Result := FImplementation.Eof;
  end;

  function TmDatabaseQuery.AsDataset: TDataset;
  begin
    CreateImplementation;
    Result := FImplementation.AsDataset;
  end;

  procedure TmDatabaseQuery.Prepare;
  begin
    CreateImplementation;
    if FImplementation.Prepared then
      FImplementation.Unprepare;
    FImplementation.SetSQL(FSQL);
    FImplementation.Prepare;
  end;

  function TmDatabaseQuery.Prepared: boolean;
  begin
    CreateImplementation;
    Result := FImplementation.Prepared;
  end;

  function TmDatabaseQuery.ParamByName(const Value: string): TParam;
  begin
    CreateImplementation;
    Result := FImplementation.ParamByName(Value);
  end;

  { TmDatabaseConnection }

  procedure TmDatabaseConnection.CreateImplementation;
  begin
    if Assigned(FConnectionInfo) then
    begin
      if not Assigned(FImplementation) then
      begin
        FImplementation := GetDataConnectionClassesRegister.GetConnectionImpl(FConnectionInfo.VendorType);
        FImplementation.ConnectionInfo := FConnectionInfo;
      end;
    end
    else
    begin
      raise TmDataConnectionException.Create('Connection info is unavailable.');
    end;
  end;

  constructor TmDatabaseConnection.Create;
  begin
    FImplementation := nil;
  end;

  destructor TmDatabaseConnection.Destroy;
  begin
    if Assigned(FImplementation) then
      FreeAndNil(FImplementation);
    inherited Destroy;
  end;

  procedure TmDatabaseConnection.Connect;
  begin
    CreateImplementation;
    FImplementation.Connect;
  end;

  procedure TmDatabaseConnection.Close;
  begin
    CreateImplementation;
    FImplementation.Close;
  end;

  procedure TmDatabaseConnection.StartTransaction;
  begin
    CreateImplementation;
    FImplementation.StartTransaction;
  end;

  procedure TmDatabaseConnection.Commit;
  begin
    CreateImplementation;
    FImplementation.Commit;
  end;

  procedure TmDatabaseConnection.Rollback;
  begin
    CreateImplementation;
    FImplementation.Rollback;
  end;

end.
