// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit UramakiFrameworkClasses;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes,
  mXML,
  UramakiBase;

type

  { TUramakiActualTransformation }

  TUramakiActualTransformation = class (TCollectionItem)
  strict private
    FTransformer : TUramakiTransformer;
    FTransformationContext : TUramakiTransformationContext;
  public
    constructor Create(Collection: TCollection); override;

    property Transformer : TUramakiTransformer read FTransformer write FTransformer;
    property TransformationContext : TUramakiTransformationContext read FTransformationContext write FTransformationContext;
  end;

  { TUramakiActualTransformations }

  TUramakiActualTransformations = class(TCollection)
  strict private
    function GetItem (index : integer): TUramakiActualTransformation;
  public
    constructor Create; reintroduce;
    function Add : TUramakiActualTransformation;
    property Items[index:integer]: TUramakiActualTransformation read GetItem; default;
  end;

  { TUramakiActualPublication }

  TUramakiActualPublication = class (TCollectionItem)
  strict private
    FPublisher : TUramakiPublisher;
    FPublicationContext : TUramakiPublicationContext;
  public
    constructor Create(Collection : TCollection); override;

    property Publisher : TUramakiPublisher read FPublisher write FPublisher;
    property PublicationContext : TUramakiPublicationContext read FPublicationContext write FPublicationContext;
  end;

  { TUramakiActualPublications }

  TUramakiActualPublications = class (TCollection)
  strict private
    function GetItem(index: integer): TUramakiActualPublication;
  public
    constructor Create; reintroduce;
    function Add: TUramakiActualPublication;
    property Items[index : integer] : TUramakiActualPublication read GetItem; default;
  end;

  { TUramakiLivingPlate }

  TUramakiLivingPlate = class
  strict private
    FIsNullPlate : Boolean;
    FParentPlate : TUramakiLivingPlate;
    FPublisher : TUramakiPublisher;
    FPublicationContext : TUramakiPublicationContext;
    FPlate : TUramakiPlate;
    FChildPlates : TList;
    FTransformations : TUramakiActualTransformations;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveToXml (aXMLElement : TmXmlElement);
    procedure LoadFromXml (aXMLElement : TmXmlElement);

    property IsNullPlate : Boolean read FIsNullPlate write FIsNullPlate;
    property ParentPlate : TUramakiLivingPlate read FParentPlate write FParentPlate;
    property Publisher : TUramakiPublisher read FPublisher write FPublisher;
    property PublicationContext : TUramakiPublicationContext read FPublicationContext write FPublicationContext;
    property Plate : TUramakiPlate read FPlate write FPlate;
    property ChildPlates : TList read FChildPlates;
    property Transformations : TUramakiActualTransformations read FTransformations;
  end;



implementation

uses
  SysUtils;

{ TUramakiLivingPlate }

constructor TUramakiLivingPlate.Create;
begin
  FIsNullPlate := false;
  FParentPlate := nil;
  FPublisher := nil;
  FPublicationContext := nil;
  FPlate := nil;
  FChildPlates := TList.Create;
  FTransformations := TUramakiActualTransformations.Create;
end;

destructor TUramakiLivingPlate.Destroy;
begin
  FChildPlates.Free;
  FTransformations.Free;
  inherited Destroy;
end;

procedure TUramakiLivingPlate.SaveToXml(aXMLElement: TmXmlElement);
var
  tmpElement : TmXmlElement;
  i : integer;
begin
  aXMLElement.SetAttribute('instance', GUIDToString(Self.FPlate.InstanceIdentifier));
  aXMLElement.SetBooleanAttribute('isNullPlate', FIsNullPlate);
  aXMLElement.SetAttribute('publisherId', FPublisher.GetMyId);
  if Assigned(FPublicationContext) then
  begin
    FPublicationContext.SaveToXML(aXMLElement.AddElement('publicationContext'));
  end;
  tmpElement := aXMLElement.AddElement('transformations');
  for i := 0 to FTransformations.Count -1 do
  begin
    tmpElement.SetAttribute('transformerId', FTransformations.Items[i].Transformer.GetMyId);
    FTransformations.Items[i].TransformationContext.SaveToXML(tmpElement.AddElement('transformationContext'));
  end;
end;

procedure TUramakiLivingPlate.LoadFromXml(aXMLElement: TmXmlElement);
begin
  Self.FPlate.InstanceIdentifier := StringToGUID(aXMLElement.GetAttribute('instance'));
  Self.FIsNullPlate:= aXMLElement.GetBooleanAttribute('isNullPlate');
  // to do
end;

{ TUramakiActualPublications }

function TUramakiActualPublications.GetItem(index: integer): TUramakiActualPublication;
begin
  Result := TUramakiActualPublication(inherited Items[index]);
end;

constructor TUramakiActualPublications.Create;
begin
  inherited Create(TUramakiActualPublication);
end;

function TUramakiActualPublications.Add: TUramakiActualPublication;
begin
  Result := TUramakiActualPublication(inherited Add);
end;

{ TUramakiActualPublication }

constructor TUramakiActualPublication.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FPublicationContext := nil;
  FPublisher := nil;
end;

{ TUramakiActualTransformations }

function TUramakiActualTransformations.GetItem(index: integer): TUramakiActualTransformation;
begin
  Result := TUramakiActualTransformation(inherited Items[index]);
end;

constructor TUramakiActualTransformations.Create;
begin
  inherited Create(TUramakiActualTransformation);
end;

function TUramakiActualTransformations.Add: TUramakiActualTransformation;
begin
  Result := TUramakiActualTransformation(inherited Add);
end;

{ TUramakiActualTransformation }

constructor TUramakiActualTransformation.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FTransformationContext := nil;
  FTransformer := nil;
end;

end.
