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

{$IFDEF FPC}
{$MODE DELPHI}
// {$MODE DELPHIUNICODE}
{$ENDIF}

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

  { TmAbstractDatabaseCommand }

  TmAbstractDatabaseCommand = class abstract
  protected
    FImplementation : TmAbstractDatabaseCommandImpl;
    FDatabaseConnection : TmDatabaseConnection;
    FSQL : TStringList;
    FParameters : TmQueryParameters;
    procedure CreateImplementation; virtual; abstract;
    function PrepareIfNecessary : Boolean;
    procedure ReloadParameters;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Prepare;
    procedure Unprepare;
    function Prepared : boolean;
    function ParamByName(const Value: string): TmQueryParameter;

    property DatabaseConnection : TmDatabaseConnection read FDatabaseConnection write FDatabaseConnection;
    property SQL : TStringList read FSQL;
  end;


  { TmDatabaseQuery }

  TmDatabaseQuery = class (TmAbstractDatabaseCommand)
  protected
    procedure CreateImplementation; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Open;
    procedure Close;
    procedure First;
    procedure Next;
    function Eof : boolean;
    function AsDataset : TDataset;
  end;

  { TmDatabaseCommand }

  TmDatabaseCommand = class (TmAbstractDatabaseCommand)
  protected
    procedure CreateImplementation; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Execute : integer;
  end;

implementation

uses
  SysUtils, mDatabaseConnectionImplRegister;

  { TmAbstractDatabaseCommand }

  function TmAbstractDatabaseCommand.PrepareIfNecessary: Boolean;
  begin
    Result := false;
    if (not FImplementation.Prepared) then
    begin
      Self.Prepare;
      Result := true;
    end
    else
    begin
      if (not FImplementation.SameSQL(FSQL)) then
      begin
        Self.Unprepare;
        Self.Prepare;
        Result := true;
      end;
    end;
  end;

  procedure TmAbstractDatabaseCommand.ReloadParameters;
  var
    i : integer;
  begin
    FParameters.Clear;
    for i := 0 to FImplementation.ParamCount - 1 do
    begin
      FParameters.Add(FImplementation.GetParam(i));
    end;
  end;

  constructor TmAbstractDatabaseCommand.Create;
  begin
    FSQL := TStringList.Create;
    FParameters := TmQueryParameters.Create;
  end;

  destructor TmAbstractDatabaseCommand.Destroy;
  begin
    FreeAndNil(FSQL);
    FreeAndNil(FParameters);
    inherited Destroy;
  end;

  procedure TmAbstractDatabaseCommand.Prepare;
  begin
    CreateImplementation;
    if FImplementation.Prepared then
      FImplementation.Unprepare;
    FImplementation.SetSQL(FSQL);
    FImplementation.Prepare;
    ReloadParameters;
  end;

  procedure TmAbstractDatabaseCommand.Unprepare;
  begin
    CreateImplementation;
    FImplementation.Unprepare;
    FParameters.Clear;
  end;

  function TmAbstractDatabaseCommand.Prepared: boolean;
  begin
    CreateImplementation;
    Result := FImplementation.Prepared;
  end;

  function TmAbstractDatabaseCommand.ParamByName(const Value: string): TmQueryParameter;
  var
    i : integer;
    TempParam : TmQueryParameter;
  begin
    CreateImplementation;
    if PrepareIfNecessary then
    begin
      // must reload FParameters
      ReloadParameters;
    end;
    Result := FParameters.FindByName(Value);
  end;

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
    inherited;
    FImplementation := nil;
  end;

  destructor TmDatabaseCommand.Destroy;
  begin
    if Assigned(FImplementation) then
      FreeAndNil(FImplementation);
    inherited Destroy;
  end;

  function TmDatabaseCommand.Execute: integer;
  var
    i : integer;
  begin
    CreateImplementation;
    PrepareIfNecessary;
    for i := 0 to FParameters.Count -1 do
    begin
      FImplementation.SetParamValue(FParameters.GetParam(i));
    end;
    Result := (FImplementation as TmDatabaseCommandImpl).Execute;
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
    inherited;
    FImplementation := nil;
  end;

  destructor TmDatabaseQuery.Destroy;
  begin
    if Assigned(FImplementation) then
      FreeAndNil(FImplementation);
    inherited Destroy;
  end;

  procedure TmDatabaseQuery.Open;
  var
    i : integer;
  begin
    CreateImplementation;
    PrepareIfNecessary;
    for i := 0 to FParameters.Count -1 do
    begin
      FImplementation.SetParamValue(FParameters.GetParam(i));
    end;
    (FImplementation as TmDatabaseQueryImpl).Open;
  end;

  procedure TmDatabaseQuery.Close;
  begin
    CreateImplementation;
    (FImplementation as TmDatabaseQueryImpl).Close;
  end;

  procedure TmDatabaseQuery.First;
  begin
    CreateImplementation;
    (FImplementation as TmDatabaseQueryImpl).First;
  end;

  procedure TmDatabaseQuery.Next;
  begin
    CreateImplementation;
    (FImplementation as TmDatabaseQueryImpl).Next;
  end;

  function TmDatabaseQuery.Eof: boolean;
  begin
    CreateImplementation;
    Result := (FImplementation as TmDatabaseQueryImpl).Eof;
  end;

  function TmDatabaseQuery.AsDataset: TDataset;
  begin
    CreateImplementation;
    Result := (FImplementation as TmDatabaseQueryImpl).AsDataset;
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
