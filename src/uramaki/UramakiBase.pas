// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit UramakiBase;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface
uses
  Classes, SysUtils,
  UramakiFrameworkConnector;

type
  TUramakiException = class (Exception);

  TUramakiPlate = class;

  TUramakiStreamFormat = (usfXML, usfJSON);

  TUramakiRoll = class abstract
  public
    const NULL_URAMAKI_ID = '**NULL**';
  public
    function GetMyId : string; virtual; abstract;
    function GetDescription : string; virtual; abstract;
    function CanBeCached : boolean; virtual; abstract;

    procedure Init; virtual; abstract;
    procedure BeforeRead; virtual; abstract;
    procedure AfterRead; virtual; abstract;
  end;

 // TUramakiPlate = class;

  IUramakiFrameworkConnector = interface
    procedure PleaseRefreshMyChilds (aPlate : TUramakiPlate);
  end;

  { TUramakiPlate }

  TUramakiPlate = class abstract
  private
    FInstanceIdentifier : TGuid;
  protected
    FFrameworkConnector : IUramakiFrameworkConnector;
  public
    constructor Create (aFrameworkConnector : IUramakiFrameworkConnector); virtual;
    destructor Destroy; override;

    function GetUramaki(const aUramakiId: String) : TUramakiRoll; virtual; abstract;
    procedure StartTransaction(const aTransactionId : TGuid); virtual; abstract;
    procedure EndTransaction(const aTransactionId: TGuid); virtual; abstract;

    property InstanceIdentifier : TGuid read FInstanceIdentifier write FInstanceIdentifier;
  end;

  TUramakiPublicationContext = class abstract
  public
    procedure SaveTo (aStream : TStream; const aFormat: TUramakiStreamFormat); virtual; abstract;
    procedure LoadFrom (aStream : TStream; const aFormat: TUramakiStreamFormat); virtual; abstract;
  end;

  { TUramakiPublisher }

  TUramakiPublisher = class abstract
  public
    function GetMyId : String; virtual; abstract;
    function GetDescription : String; virtual; abstract;
    function GetHelp : String; virtual; abstract;

    function GetInputUramakiId : String; virtual; abstract;

    function CreatePlate (aFrameworkConnector : TUramakiFrameworkConnector) : TUramakiPlate; virtual; abstract;
    function CreatePublicationContext : TUramakiPublicationContext; virtual; abstract;
    procedure StartTransaction(const aTransactionId : TGuid); virtual; abstract;
    procedure EndTransaction(const aTransactionId: TGuid); virtual; abstract;

    procedure Publish(aInput : TUramakiRoll; aPlate : TUramakiPlate; aContext : TUramakiPublicationContext); virtual; abstract;
  end;

  TUramakiTransformationContext = class abstract
  public
    procedure SaveTo (aStream : TStream; const aFormat: TUramakiStreamFormat); virtual; abstract;
    procedure LoadFrom (aStream : TStream; const aFormat: TUramakiStreamFormat); virtual; abstract;
  end;

  TUramakiTransformer = class abstract
  public
    function GetMyId : String; virtual; abstract;
    function GetDescription : String; virtual; abstract;
    function GetHelp : String; virtual; abstract;

    function GetInputUramakiId : String; virtual; abstract;
    function GetOutputUramakiId : String; virtual; abstract;

    function CreateTransformationContext : TUramakiTransformationContext; virtual; abstract;

    procedure Configure (aInput : TUramakiRoll; aContext : TUramakiTransformationContext); virtual; abstract;
    procedure Transform (aInput : TUramakiRoll; aContext : TUramakiTransformationContext); virtual; abstract;

    procedure StartTransaction(const aTransactionId : TGuid); virtual; abstract;
    procedure EndTransaction(const aTransactionId: TGuid); virtual; abstract;
  end;


implementation


{ TUramakiPlate }

constructor TUramakiPlate.Create(aFrameworkConnector: IUramakiFrameworkConnector);
begin
  FFrameworkConnector:= aFrameworkConnector;
end;

destructor TUramakiPlate.Destroy;
begin
  inherited Destroy;
end;

end.
