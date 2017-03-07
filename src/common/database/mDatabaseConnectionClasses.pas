unit mDatabaseConnectionClasses;

{$MODE DELPHI}

interface

uses
  DB, SysUtils;

type
  TmDataConnectionException = class (Exception);

  TmDatabaseVendor = (dvUnknown, dvSQLServer);

{ TmQueryParameter }

  TmQueryParameter = class
  strict private
    FValue : Variant;
    FFieldType : TFieldType;
    function GetAsDateTime: TDateTime;
    function GetAsDouble: Double;
    function GetAsInteger: Integer;
    function GetAsString: String;
    function GetAsWideString: WideString;
    procedure SetAsDateTime(AValue: TDateTime);
    procedure SetAsDouble(AValue: Double);
    procedure SetAsInteger(AValue: Integer);
    procedure SetAsString(AValue: String);
    procedure SetAsWideString(AValue: WideString);
    procedure SetFieldType (value : TFieldType);
    function GetFieldType : TFieldType;
    function ValueAsDouble : Double;
  public
    constructor Create;

    procedure SetNull;
    property FieldType : TFieldType read GetFieldType write SetFieldType;
    property AsString : String read GetAsString write SetAsString;
    property AsWideString : WideString read GetAsWideString write SetAsWideString;
    property AsInteger : Integer read GetAsInteger write SetAsInteger;
    property AsDouble : Double read GetAsDouble write SetAsDouble;
    property AsDateTime : TDateTime read GetAsDateTime write SetAsDateTime;
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


implementation

{ TmDatabaseConnectionInfo }

constructor TmDatabaseConnectionInfo.Create;
begin
  FVendorType:= dvUnknown;
end;


{ TmQueryParameter }

procedure TmQueryParameter.SetFieldType(value: TFieldType);
begin
  FFieldType:= value;
end;

function TmQueryParameter.GetAsDateTime: TDateTime;
begin
  if (FFieldType = ftDate) or (FFieldType = ftDateTime) then
  begin
    Result := ValueAsDouble;
  end
  else
  begin
    raise TmDataConnectionException.Create('Fieldtype of parameter is not ftDate or ftDateTime');
  end;
end;

function TmQueryParameter.GetAsDouble: Double;
begin
  if (FFieldType = ftFloat) then
  begin
    Result := ValueAsDouble;
  end
  else
  begin
    raise TmDataConnectionException.Create('Fieldtype of parameter is not ftFloat');
  end;
end;

function TmQueryParameter.GetAsInteger: Integer;
begin
  if (FieldType = ftInteger) then
  begin
    if (FValue = Null) then
      Result := 0
    else
      Result := FValue;
  end
  else
  begin
    raise TmDataConnectionException.Create('Fieldtype of parameter is not ftInteger');
  end;
end;

function TmQueryParameter.GetAsString: String;
begin
  if (FieldType = ftString) then
  begin
    if (FValue = Null) then
      Result := ''
    else
      Result := FValue;
  end
  else
  begin
    raise TmDataConnectionException.Create('Fieldtype of parameter is not ftString');
  end;
end;

function TmQueryParameter.GetAsWideString: WideString;
begin
  if (FieldType = ftString) or (FieldType = ftWideString) then
  begin
    if (FValue = Null) then
      Result := ''
    else
      Result := FValue;
  end
  else
  begin
    raise TmDataConnectionException.Create('Fieldtype of parameter is not ftWideString or ftString');
  end;
end;

procedure TmQueryParameter.SetAsDateTime(AValue: TDateTime);
begin
  FValue := AValue;
  FFieldType:= ftDateTime;
end;

procedure TmQueryParameter.SetAsDouble(AValue: Double);
begin
  FValue := AValue;
  FFieldType:= ftFloat;
end;

procedure TmQueryParameter.SetAsInteger(AValue: Integer);
begin
  FValue := AValue;
  FFieldType:= ftInteger;
end;

procedure TmQueryParameter.SetAsString(AValue: String);
begin
  FValue := AValue;
  FFieldType := ftString;
end;

procedure TmQueryParameter.SetAsWideString(AValue: WideString);
begin
  FValue := AValue;
  FFieldType := ftWideString;
end;

function TmQueryParameter.GetFieldType: TFieldType;
begin
  Result := FFieldType;
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
  FFieldType:= ftUnknown;
  FValue:= Null;
end;

procedure TmQueryParameter.SetNull;
begin
  FValue := Null;
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
