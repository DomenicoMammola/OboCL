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
  Classes, SysUtils, contnrs, Controls, ExtCtrls, db,
  mXML;

const
  NULL_URAMAKI_ID = '**NULL**';

type
  TUramakiException = class (Exception);

  TUramakiPlate = class;

  { TUramakiRoll }

  TUramakiRoll = class abstract
  public
    function GetMyId : string; virtual; abstract;
    function GetDescription : string; virtual; abstract;
    function CanBeCached : boolean; virtual;

    procedure Init; virtual;
    procedure BeforeRead; virtual;
    procedure AfterRead; virtual;
  end;

  TUramakiAbstractEngineMediator = class abstract
  public
    procedure PleaseRefreshMyChilds (aPlate : TUramakiPlate); virtual; abstract;
    procedure PleaseClearMyChilds (aPlate : TUramakiPlate); virtual; abstract;
    procedure PleaseAskMyFatherToRefreshItsChilds(aPlate : TUramakiPlate); virtual; abstract;
    function GetInstanceIdentifier (aPlate : TUramakiPlate) : TGuid; virtual; abstract;
  end;

  { TUramakiPlate }

  TUramakiPlate = class (TPanel)
  strict private
    FEngineMediator : TUramakiAbstractEngineMediator;
    FOnUramakiPlateDestroy : TNotifyEvent;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    function GetUramakiRoll(const aUramakiRollId: String) : TUramakiRoll; virtual; abstract;
    procedure GetAvailableUramakiRolls (aUramakiRollIdList : TStringList); virtual; abstract;
    procedure StartTransaction(const aTransactionId : TGuid); virtual;
    procedure EndTransaction(const aTransactionId: TGuid); virtual;
    procedure LoadConfigurationFromXML (aXMLElement : TmXmlElement); virtual;
    procedure SaveConfigurationToXML (aXMLElement : TmXmlElement); virtual;
    procedure Clear; virtual; abstract;

    property EngineMediator : TUramakiAbstractEngineMediator read FEngineMediator write FEngineMediator;
    property OnDestroy : TNotifyEvent read FOnUramakiPlateDestroy write FOnUramakiPlateDestroy;
  end;

  TUramakiPublicationContext = class abstract
  public
    procedure SaveToXML (aXMLElement: TmXmlElement); virtual; abstract;
    procedure LoadFromXML (aXMLElement : TmXmlElement); virtual; abstract;
  end;

  { TUramakiPublisher }

  TUramakiPublisher = class abstract
  protected
    function GenerateStandardDescription (const aDescriptionOfSourceData, aDescriptionOfPlateData, aDescriptionOfRepresentation : string) : string; overload;
    function GenerateStandardDescription (const aDescriptionOfPlateData, aDescriptionOfRepresentation : string) : string; overload;
  public
    function GetMyId : String; virtual; abstract;
    function GetDescription : String; virtual; abstract;
    function GetCategory : String; virtual;
    function GetHelp : String; virtual;

    function GetInputUramakiId : String; virtual; abstract;

    function CreatePlate (aParent : TPanel) : TUramakiPlate; virtual; abstract;

    function CreatePublicationContext : TUramakiPublicationContext; virtual;
    procedure StartTransaction(const aTransactionId : TGuid); virtual;
    procedure EndTransaction(const aTransactionId: TGuid); virtual;

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
    function FindById (aId : String) : TUramakiPublisher;
  end;

  TUramakiTransformationContext = class abstract
  public
    procedure SaveToXML (aXMLElement: TmXmlElement); virtual; abstract;
    procedure LoadFromXML (aXMLElement : TmXmlElement); virtual; abstract;
  end;

  { TUramakiTransformer }

  TUramakiTransformer = class
  public
    function GetMyId : String; virtual; abstract;
    function GetDescription : String; virtual; abstract;
    function GetHelp : String; virtual;

    function GetInputUramakiId : String; virtual; abstract;
    function GetOutputUramakiId : String; virtual; abstract;

    function CreateTransformationContext : TUramakiTransformationContext; virtual;

    procedure Configure (aInput : TUramakiRoll; aContext : TUramakiTransformationContext); virtual;
    function Transform (aInput : TUramakiRoll; aContext : TUramakiTransformationContext) : TUramakiRoll; virtual; abstract;

    procedure StartTransaction(const aTransactionId : TGuid); virtual;
    procedure EndTransaction(const aTransactionId: TGuid); virtual;
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
    function FindById (aId : String) : TUramakiTransformer;
  end;


implementation

{ TUramakiPublisher }

function TUramakiPublisher.GenerateStandardDescription(const aDescriptionOfSourceData, aDescriptionOfPlateData, aDescriptionOfRepresentation: string): string;
begin
  if (aDescriptionOfSourceData <> aDescriptionOfPlateData) then
    Result := aDescriptionOfSourceData + ' -> ' + aDescriptionOfPlateData + ' [' + aDescriptionOfRepresentation + ']'
  else
    Result := GenerateStandardDescription(aDescriptionOfSourceData, aDescriptionOfRepresentation);
end;

function TUramakiPublisher.GenerateStandardDescription(const aDescriptionOfPlateData, aDescriptionOfRepresentation: string): string;
begin
  Result := aDescriptionOfPlateData + ' [' + aDescriptionOfRepresentation + ']'
end;

function TUramakiPublisher.GetCategory: String;
begin
  Result := '';
end;

function TUramakiPublisher.GetHelp: String;
begin
  Result := GetDescription;
end;

function TUramakiPublisher.CreatePublicationContext: TUramakiPublicationContext;
begin
  Result := nil;
end;

procedure TUramakiPublisher.StartTransaction(const aTransactionId: TGuid);
begin
  //
end;

procedure TUramakiPublisher.EndTransaction(const aTransactionId: TGuid);
begin
  //
end;

{ TUramakiPlate }

constructor TUramakiPlate.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TUramakiPlate.Destroy;
begin
  if Assigned(FOnUramakiPlateDestroy) then
    FOnUramakiPlateDestroy(Self);
  inherited Destroy;
end;

procedure TUramakiPlate.StartTransaction(const aTransactionId: TGuid);
begin
  //
end;

procedure TUramakiPlate.EndTransaction(const aTransactionId: TGuid);
begin
  //
end;

procedure TUramakiPlate.LoadConfigurationFromXML(aXMLElement: TmXmlElement);
begin
  //
end;

procedure TUramakiPlate.SaveConfigurationToXML(aXMLElement: TmXmlElement);
begin
  //
end;

{ TUramakiRoll }

function TUramakiRoll.CanBeCached: boolean;
begin
  Result := false;
end;

procedure TUramakiRoll.Init;
begin
  //
end;

procedure TUramakiRoll.BeforeRead;
begin
  //
end;

procedure TUramakiRoll.AfterRead;
begin
  //
end;

{ TUramakiTransformer }

function TUramakiTransformer.GetHelp: String;
begin
  Result := GetDescription;
end;

function TUramakiTransformer.CreateTransformationContext: TUramakiTransformationContext;
begin
  Result := nil;
end;

procedure TUramakiTransformer.Configure(aInput: TUramakiRoll; aContext: TUramakiTransformationContext);
begin
  //
end;

procedure TUramakiTransformer.StartTransaction(const aTransactionId: TGuid);
begin
  //
end;

procedure TUramakiTransformer.EndTransaction(const aTransactionId: TGuid);
begin
  //
end;

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

function TUramakiTransformers.FindById(aId: String): TUramakiTransformer;
var
  i : integer;
begin
  Result := nil;
  for i := 0 to Self.Count - 1 do
  begin
    if CompareText(aId, Self.Get(i).GetMyId) = 0 then
    begin
      Result := Self.Get(i);
      exit;
    end;
  end;
end;

{ TUramakiPublishers }

constructor TUramakiPublishers.Create;
begin
  FList := TObjectList.Create(false);
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

function TUramakiPublishers.FindById(aId: String): TUramakiPublisher;
var
  i : integer;
begin
  Result := nil;
  for i := 0 to Self.Count - 1 do
  begin
    if CompareText(Self.Get(i).GetMyId, aId) = 0 then
    begin
      Result := Self.Get(i);
      exit;
    end;
  end;
end;


end.
