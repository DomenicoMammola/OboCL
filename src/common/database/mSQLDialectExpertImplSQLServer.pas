// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mSQLDialectExpertImplSQLServer;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  mDatabaseConnectionClasses,
  mSQLDialectExpertImpl, mSQLDialectExpertImplRegister;

type

  { TSQLDialectExpertImplSQLServer }

  TSQLDialectExpertImplSQLServer = class (TSQLDialectExpertImpl)
  public
    constructor Create; override;
    function GetSQLForParameter (aParam : TmQueryParameter) : string; override;
  end;


implementation

uses
  SysUtils,
  mSQLServerSQLDialect;

{ TSQLDialectExpertImplSQLServer }

constructor TSQLDialectExpertImplSQLServer.Create;
begin
  // do nothing
end;

function TSQLDialectExpertImplSQLServer.GetSQLForParameter(aParam: TmQueryParameter): string;
begin
  if aParam.IsNull then
  begin
    Result := 'NULL';
  end
  else
  begin
    case aParam.DataType of
      ptDate:
        Result := DateToSQLString(aParam.AsDate);
      ptDateTime:
        Result := DateTimeToSQLString(aParam.AsDateTime);
      ptString:
        Result := StringToSQLString(aParam.AsString);
      ptWideString:
        Result := StringToSQLString(aParam.AsWideString);
      ptInteger:
        Result := IntToStr(aParam.AsInteger);
      ptFloat:
        Result := FloatToSQLString(aParam.AsFloat);
    end;
  end;
end;

initialization
  GetSQLDialectExpertsRegister.RegisterImplementations(dvSQLServer, TSQLDialectExpertImplSQLServer);

end.
