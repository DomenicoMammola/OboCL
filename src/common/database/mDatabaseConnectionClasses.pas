// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mDatabaseConnectionClasses;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  DB, Contnrs, SysUtils;

type
  TmDataConnectionException = class (Exception);

  TmDatabaseVendor = (dvUnknown, dvSQLServer);

  TmParameterDataType = (ptUnknown, ptDate, ptDateTime, ptInteger, ptFloat, ptString, ptWideString);

{ TmQueryParameter }

  TmQueryParameter = class
  strict private
    FName : String;
    FValue : Variant;
    FDataType : TmParameterDataType;
    function GetAsDate: TDate;
    function GetAsDateTime: TDateTime;
    function GetAsFloat: Double;
    function GetAsInteger: Integer;
    function GetAsString: String;
    function GetAsWideString: WideString;
    procedure SetAsDate(AValue: TDate);
    procedure SetAsDateTime(AValue: TDateTime);
    procedure SetAsFloat(AValue: Double);
    procedure SetAsInteger(AValue: Integer);
    procedure SetAsString(AValue: String);
    procedure SetAsWideString(AValue: WideString);
    procedure SetParameterDataType (value : TmParameterDataType);
    function GetParameterDataType : TmParameterDataType;
    function ValueAsDouble : Double;
  public
    constructor Create;
    procedure ImportFromParam (aSource : TParam);

    procedure SetNull;
    function IsNull : boolean;
    property Name : String read FName write FName;
    property DataType : TmParameterDataType read GetParameterDataType write SetParameterDataType;
    property AsString : String read GetAsString write SetAsString;
    property AsWideString : WideString read GetAsWideString write SetAsWideString;
    property AsInteger : Integer read GetAsInteger write SetAsInteger;
    property AsFloat : Double read GetAsFloat write SetAsFloat;
    property AsDateTime : TDateTime read GetAsDateTime write SetAsDateTime;
    property AsDate : TDate read GetAsDate write SetAsDate;
  end;

  { TmQueryParameters }

  TmQueryParameters = class
  strict private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add (aSource : TParam); overload;
    procedure Add (aParam : TmQueryParameter); overload;
    function FindByName (const aName : String): TmQueryParameter;
    procedure Clear;
    function Count : integer;
    function GetParam (aIndex : integer) : TmQueryParameter;
  end;

  { TmDatabaseConnectionInfo }

  TmDatabaseConnectionInfo = class
  strict private
    FVendorType : TmDatabaseVendor;
    FServer : String;
    FDatabaseName : String;
    FUserName : String;
    FPassword : String;
    FWindowsIntegratedSecurity : Boolean;

    function GetDatabaseName: String;
    function GetPassword: String;
    function GetServer: String;
    function GetUserName: String;
    function GetWindowsIntegratedSecurity: Boolean;
    procedure SetDatabaseName(AValue: String);
    procedure SetPassword(AValue: String);
    procedure SetServer(AValue: String);
    procedure SetUserName(AValue: String);
    procedure SetWindowsIntegratedSecurity(AValue: Boolean);
  public
    constructor Create;
    property VendorType : TmDatabaseVendor read FVendorType write FVendorType;
    property Server : String read GetServer write SetServer;
    property DatabaseName : String read GetDatabaseName write SetDatabaseName;
    property UserName : String read GetUserName write SetUserName;
    property Password : String read GetPassword write SetPassword;
    property WindowsIntegratedSecurity : Boolean read GetWindowsIntegratedSecurity write SetWindowsIntegratedSecurity;
  end;


function DataTypeToParameterDataType (aValue : TFieldType) : TmParameterDataType;
function ParameterDataTypeToDataType(aValue : TmParameterDataType) : TFieldType;

implementation

function DataTypeToParameterDataType(aValue: TFieldType): TmParameterDataType;
begin
  case aValue of
    ftUnknown:
      Result := ptUnknown;
    ftInteger:
      Result := ptInteger;
    ftFloat:
      Result := ptFloat;
    ftDate:
      Result := ptDate;
    ftDateTime:
      Result := ptDateTime;
    ftWideString:
      Result := ptWideString;
    else
      Result := ptString;
  end;
end;

function ParameterDataTypeToDataType(aValue: TmParameterDataType): TFieldType;
begin
  case aValue of
    ptUnknown:
      Result := ftUnknown;
    ptInteger:
      Result := ftInteger;
    ptFloat:
      Result := ftFloat;
    ptDate:
      Result := ftDate;
    ptDateTime:
      Result := ftDateTime;
    ptWideString:
      Result := ftWideString;
    else
      Result := ftString;
    end;
end;

{ TmQueryParameters }

constructor TmQueryParameters.Create;
begin
  FList:= TObjectList.Create;

end;

destructor TmQueryParameters.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TmQueryParameters.Add(aSource: TParam);
var
  TempParam : TmQueryParameter;
begin
  TempParam := TmQueryParameter.Create;
  FList.Add(TempParam);
  TempParam.ImportFromParam(aSource);
end;

procedure TmQueryParameters.Add(aParam: TmQueryParameter);
begin
  FList.Add(aParam);
end;

function TmQueryParameters.FindByName(const aName: String): TmQueryParameter;
var
  i : integer;
begin
  Result := nil;
  for i := 0 to FList.Count - 1 do
  begin
    if CompareText((FList.Items[i] as TmQueryParameter).Name, aName) = 0 then
    begin
      Result := FList.Items[i] as TmQueryParameter;
      exit;
    end;
  end;
end;

procedure TmQueryParameters.Clear;
begin
  FList.Clear;
end;

function TmQueryParameters.Count: integer;
begin
  Result := FList.Count;
end;

function TmQueryParameters.GetParam(aIndex: integer): TmQueryParameter;
begin
  Result := FList.Items[aIndex] as TmQueryParameter;
end;

{ TmDatabaseConnectionInfo }

constructor TmDatabaseConnectionInfo.Create;
begin
  FVendorType:= dvUnknown;
end;


{ TmQueryParameter }

procedure TmQueryParameter.SetParameterDataType(value: TmParameterDataType);
begin
  FDataType:= value;
end;

function TmQueryParameter.GetAsDateTime: TDateTime;
begin
  if (FDataType = ptDate) or (FDataType = ptDateTime) then
  begin
    Result := ValueAsDouble;
  end
  else
  begin
    raise TmDataConnectionException.Create('Datatype of parameter is not date or dateTime');
  end;
end;

function TmQueryParameter.GetAsDate: TDate;
begin
  if (FDataType = ptDate) or (FDataType = ptDateTime) then
  begin
    Result := trunc(ValueAsDouble);
  end
  else
  begin
    raise TmDataConnectionException.Create('Datatype of parameter is not date or dateTime');
  end;
end;

function TmQueryParameter.GetAsFloat: Double;
begin
  if (FDataType = ptFloat) then
  begin
    Result := ValueAsDouble;
  end
  else
  begin
    raise TmDataConnectionException.Create('Datatype of parameter is not float');
  end;
end;

function TmQueryParameter.GetAsInteger: Integer;
begin
  if (FDataType = ptInteger) then
  begin
    if (FValue = Null) then
      Result := 0
    else
      Result := FValue;
  end
  else
  begin
    raise TmDataConnectionException.Create('Datatype of parameter is not integer');
  end;
end;

function TmQueryParameter.GetAsString: String;
begin
  if (FDataType = ptString) then
  begin
    if (FValue = Null) then
      Result := ''
    else
      Result := FValue;
  end
  else
  begin
    raise TmDataConnectionException.Create('Datatype of parameter is not string');
  end;
end;

function TmQueryParameter.GetAsWideString: WideString;
begin
  if (FDataType = ptString) or (FDataType = ptWideString) then
  begin
    if (FValue = Null) then
      Result := ''
    else
      Result := FValue;
  end
  else
  begin
    raise TmDataConnectionException.Create('Datatype of parameter is not wideString or string');
  end;
end;

procedure TmQueryParameter.SetAsDate(AValue: TDate);
begin
  FValue := AValue;
  FDataType:= ptDate;
end;

procedure TmQueryParameter.SetAsDateTime(AValue: TDateTime);
begin
  FValue := AValue;
  FDataType:= ptDateTime;
end;

procedure TmQueryParameter.SetAsFloat(AValue: Double);
begin
  FValue := AValue;
  FDataType:= ptFloat;
end;

procedure TmQueryParameter.SetAsInteger(AValue: Integer);
begin
  FValue := AValue;
  FDataType:= ptInteger;
end;

procedure TmQueryParameter.SetAsString(AValue: String);
begin
  FValue := AValue;
  FDataType := ptString;
end;

procedure TmQueryParameter.SetAsWideString(AValue: WideString);
begin
  FValue := AValue;
  FDataType := ptWideString;
end;

function TmQueryParameter.GetParameterDataType: TmParameterDataType;
begin
  Result := FDataType;
end;

function TmQueryParameter.ValueAsDouble: Double;
begin
  if (FValue = Null) then
    Result := 0
  else
    Result := FValue;
end;

constructor TmQueryParameter.Create;
begin
  FDataType:= ptUnknown;
  FValue:= Null;
end;

procedure TmQueryParameter.ImportFromParam(aSource: TParam);
begin
  Self.Name:= aSource.Name;
  Self.DataType:= DataTypeToParameterDataType(aSource.DataType);
end;

procedure TmQueryParameter.SetNull;
begin
  FValue := Null;
end;

function TmQueryParameter.IsNull: boolean;
begin
  Result := (FValue = Null);
end;


function TmDatabaseConnectionInfo.GetDatabaseName: String;
begin
  Result := FDatabaseName;
end;

function TmDatabaseConnectionInfo.GetPassword: String;
begin
  Result := FPassword;
end;

function TmDatabaseConnectionInfo.GetServer: String;
begin
  Result := FServer;
end;

function TmDatabaseConnectionInfo.GetUserName: String;
begin
  Result := FUserName;
end;

function TmDatabaseConnectionInfo.GetWindowsIntegratedSecurity: Boolean;
begin
  Result := FWindowsIntegratedSecurity;
end;

procedure TmDatabaseConnectionInfo.SetDatabaseName(AValue: String);
begin
  FDatabaseName:= AValue;
end;

procedure TmDatabaseConnectionInfo.SetPassword(AValue: String);
begin
  FPassword:= AValue;
end;

procedure TmDatabaseConnectionInfo.SetServer(AValue: String);
begin
  FServer := AValue;
end;

procedure TmDatabaseConnectionInfo.SetUserName(AValue: String);
begin
  FUserName := AValue;
end;

procedure TmDatabaseConnectionInfo.SetWindowsIntegratedSecurity(AValue: Boolean);
begin
  FWindowsIntegratedSecurity:= AValue;
end;

end.
