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
  Classes, SysUtils, contnrs,
  mXML;

const
  NULL_URAMAKI_ID = '**NULL**';

type
  TUramakiException = class (Exception);

  TUramakiPlate = class;

  TUramakiRoll = class abstract
  public
    function GetMyId : string; virtual; abstract;
    function GetDescription : string; virtual; abstract;
    function CanBeCached : boolean; virtual; abstract;

    procedure Init; virtual; abstract;
    procedure BeforeRead; virtual; abstract;
    procedure AfterRead; virtual; abstract;
  end;

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
    procedure SaveToXML (aXMLElement: TmXmlElement); virtual; abstract;
    procedure LoadFromXML (aXMLElement : TmXmlElement); virtual; abstract;
  end;

  { TUramakiPublisher }

  TUramakiPublisher = class abstract
  public
    function GetMyId : String; virtual; abstract;
    function GetDescription : String; virtual; abstract;
    function GetHelp : String; virtual; abstract;

    function GetInputUramakiId : String; virtual; abstract;

    function CreatePlate (aFrameworkConnector : IUramakiFrameworkConnector) : TUramakiPlate; virtual; abstract;
    function CreatePublicationContext : TUramakiPublicationContext; virtual; abstract;
    procedure StartTransaction(const aTransactionId : TGuid); virtual; abstract;
    procedure EndTransaction(const aTransactionId: TGuid); virtual; abstract;

    procedure Publish(aInput : TUramakiRoll; aPlate : TUramakiPlate; aContext : TUramakiPublicationContext); virtual; abstract;
  end;

  { TUramakiPublishers }

  TUramakiPublishers = class
  strict private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    function Get(index : integer) : TUramakiPublisher;
    function Count : integer;
    procedure Add(aPublisher : TUramakiPublisher);
    procedure Clear;
  end;

  TUramakiTransformationContext = class abstract
  public
    procedure SaveToXML (aXMLElement: TmXmlElement); virtual; abstract;
    procedure LoadFromXML (aXMLElement : TmXmlElement); virtual; abstract;
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

  { TUramakiTransformers }

  TUramakiTransformers = class
  strict private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    function Get(index : integer) : TUramakiTransformer;
    function Count : integer;
    procedure Add(aTransformer : TUramakiTransformer);
    procedure Clear;
  end;


implementation

{ TUramakiTransformers }

constructor TUramakiTransformers.Create;
begin
  FList := TObjectList.Create(false);
end;

destructor TUramakiTransformers.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TUramakiTransformers.Get(index: integer): TUramakiTransformer;
begin
  Result := FList.Items[index] as TUramakiTransformer;
end;

function TUramakiTransformers.Count: integer;
begin
  Result := FList.Count;
end;

procedure TUramakiTransformers.Add(aTransformer: TUramakiTransformer);
begin
  FList.Add(aTransformer);
end;

procedure TUramakiTransformers.Clear;
begin
  FList.Clear;
end;

{ TUramakiPublishers }

constructor TUramakiPublishers.Create;
begin
  FList := TObjectList.Create;
end;

destructor TUramakiPublishers.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TUramakiPublishers.Get(index: integer): TUramakiPublisher;
begin
  Result := FList.Items[index] as TUramakiPublisher;
end;

function TUramakiPublishers.Count: integer;
begin
  Result := FList.Count;
end;

procedure TUramakiPublishers.Add(aPublisher: TUramakiPublisher);
begin
  FList.Add(aPublisher);
end;

procedure TUramakiPublishers.Clear;
begin
  FList.Clear;
end;


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
