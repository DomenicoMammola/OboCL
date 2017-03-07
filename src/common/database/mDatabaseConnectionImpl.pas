// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mDatabaseConnectionImpl;

{$MODE DELPHI}

interface

uses
  Classes, DB,
  mDatabaseConnectionClasses;

type

  TmDatabaseConnectionImpl = class abstract
    protected
      FConnectionInfo : TmDatabaseConnectionInfo;
    public
      constructor Create; virtual; abstract;
      procedure Connect; virtual; abstract;
      procedure Close; virtual; abstract;
      function GetName : String; virtual; abstract;
      procedure StartTransaction; virtual; abstract;
      procedure Commit; virtual; abstract;
      procedure Rollback; virtual; abstract;

      property ConnectionInfo : TmDatabaseConnectionInfo read FConnectionInfo write FConnectionInfo;
  end;

  TmDatabaseConnectionImplClass = class of TmDatabaseConnectionImpl;


  { TmDatabaseQueryImpl }

  TmDatabaseQueryImpl = class abstract
  protected
    procedure SetDatabaseConnectionImpl (value : TmDatabaseConnectionImpl); virtual; abstract;
    function GetDatabaseConnectionImpl : TmDatabaseConnectionImpl; virtual; abstract;
  public
    constructor Create; virtual; abstract;

    procedure Open; virtual; abstract;
    procedure Close; virtual; abstract;
    procedure First; virtual; abstract;
    procedure Next; virtual; abstract;
    function Eof : boolean; virtual; abstract;
    function AsDataset : TDataset; virtual; abstract;
    procedure SetSQL (aValue : TStringList); virtual; abstract;
    function SameSQL (aValue : TStringList): boolean; virtual; abstract;
    procedure Prepare; virtual; abstract;
    function ParamByName(const Value: string): TParam; virtual; abstract;
    function Prepared : boolean; virtual; abstract;
    procedure Unprepare; virtual; abstract;

    property DatabaseConnectionImpl : TmDatabaseConnectionImpl read GetDatabaseConnectionImpl write SetDatabaseConnectionImpl;
  end;

  TmDatabaseQueryImplClass = class of TmDatabaseQueryImpl;

  TmDatabaseCommandImpl = class abstract
  protected
    procedure SetDatabaseConnectionImpl (value : TmDatabaseConnectionImpl); virtual; abstract;
    function GetDatabaseConnectionImpl : TmDatabaseConnectionImpl; virtual; abstract;
  public
    constructor Create; virtual; abstract;

    procedure Execute; virtual; abstract;
    procedure SetSQL (aValue : TStringList); virtual; abstract;
    function SameSQL (aValue : TStringList): boolean; virtual; abstract;
    procedure Prepare; virtual; abstract;
    function ParamByName(const Value: string): TParam; virtual; abstract;
    function Prepared : boolean; virtual; abstract;
    procedure Unprepare; virtual; abstract;

    property DatabaseConnectionImpl : TmDatabaseConnectionImpl read GetDatabaseConnectionImpl write SetDatabaseConnectionImpl;
  end;

  TmDatabaseCommandImplClass = class of TmDatabaseCommandImpl;

implementation

end.
