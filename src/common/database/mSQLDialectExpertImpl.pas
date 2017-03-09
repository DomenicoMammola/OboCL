// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mSQLDialectExpertImpl;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  mDatabaseConnectionClasses;

type
  TSQLDialectExpertImpl = class abstract
  public
    constructor Create; virtual; abstract;
    function GetSQLForParameter (aParam : TmQueryParameter) : string; virtual; abstract;
  end;

  TSQLDialectExpertImplementationClass = class of TSQLDialectExpertImpl;

implementation


end.
