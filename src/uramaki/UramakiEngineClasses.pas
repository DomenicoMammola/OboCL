// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit UramakiEngineClasses;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes,
  mXML, mMaps,
  UramakiBase;

type

  { TUramakiActualTransformation }

  TUramakiActualTransformation = class (TCollectionItem)
  strict private
    FTransformer : TUramakiTransformer;
    FTransformationContext : TUramakiTransformationContext;
    procedure SetTransformer(AValue: TUramakiTransformer);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    property Transformer : TUramakiTransformer read FTransformer write SetTransformer;
    property TransformationContext : TUramakiTransformationContext read FTransformationContext;
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

  TUramakiActualPublication = class
  strict private
    FPublisher : TUramakiPublisher;
    FPublicationContext : TUramakiPublicationContext;
    procedure SetPublisher(AValue: TUramakiPublisher);
  public
    constructor Create;
    destructor Destroy; override;

    property Publisher : TUramakiPublisher read FPublisher write SetPublisher;
    property PublicationContext : TUramakiPublicationContext read FPublicationContext;
  end;


  { TUramakiLivingPlate }

  TUramakiLivingPlate = class
  strict private
    FIsNullPlate : Boolean;
    FParentIdentifier : TGuid;
    FParent : TUramakiLivingPlate;
    FInstanceIdentifier : TGuid;

    FPlate : TUramakiPlate;
    FTransformations : TUramakiActualTransformations;
    FPublication : TUramakiActualPublication;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveToXml (aXMLElement : TmXmlElement);
    procedure BuildFromXml (aXMLElement : TmXmlElement; aPublishers : TUramakiPublishers; aTransformers : TUramakiTransformers; aBuiltPlates : TmStringDictionary);
    procedure RefreshData;

    property IsNullPlate : Boolean read FIsNullPlate write FIsNullPlate;
    property Plate : TUramakiPlate read FPlate write FPlate;
    property Transformations : TUramakiActualTransformations read FTransformations;
    property Publication : TUramakiActualPublication read FPublication;
    property InstanceIdentifier : TGuid read FInstanceIdentifier;
    property ParentIdentifier : TGuid read FParentIdentifier write FParentIdentifier;
    property Parent : TUramakiLivingPlate read FParent;
  end;



implementation

uses
  SysUtils;

{ TUramakiLivingPlate }

constructor TUramakiLivingPlate.Create;
begin
  FIsNullPlate := false;
  FParentIdentifier := GUID_NULL;
  FInstanceIdentifier := TGuid.NewGuid;
  FPlate := nil;

  FTransformations := TUramakiActualTransformations.Create;
  FPublication := TUramakiActualPublication.Create();
end;

destructor TUramakiLivingPlate.Destroy;
begin
  FTransformations.Free;
  FPublication.Free;
  FreeAndNil(FPlate);
  inherited Destroy;
end;

procedure TUramakiLivingPlate.SaveToXml(aXMLElement: TmXmlElement);
var
  tmpElement : TmXmlElement;
  i : integer;
begin
  aXMLElement.SetAttribute('instance', GUIDToString(Self.InstanceIdentifier));
  aXMLElement.SetAttribute('parent', GUIDToString(Self.ParentIdentifier));
  aXMLElement.SetBooleanAttribute('isNullPlate', FIsNullPlate);
  aXMLElement.SetAttribute('publisherId', FPublication.Publisher.GetMyId);
  FPublication.PublicationContext.SaveToXML(aXMLElement.AddElement('publicationContext'));
  tmpElement := aXMLElement.AddElement('transformations');
  for i := 0 to FTransformations.Count -1 do
  begin
    tmpElement.SetAttribute('transformerId', FTransformations.Items[i].Transformer.GetMyId);
    FTransformations.Items[i].TransformationContext.SaveToXML(tmpElement.AddElement('transformationContext'));
  end;
end;

procedure TUramakiLivingPlate.BuildFromXml(aXMLElement: TmXmlElement; aPublishers : TUramakiPublishers; aTransformers : TUramakiTransformers; aBuiltPlates : TmStringDictionary);
var
  tmpId : string;
  cursor, cursor2: TmXmlElementCursor;
  i : integer;
  tmpTransformation : TUramakiActualTransformation;
begin
  Self.FInstanceIdentifier := StringToGUID(aXMLElement.GetAttribute('instance'));
  Self.FIsNullPlate:= aXMLElement.GetBooleanAttribute('isNullPlate');
  tmpId := aXMLElement.GetAttribute('parent');
  Self.FParentIdentifier := StringToGUID(tmpId);
  if not FIsNullPlate then
    Self.FParent := aBuiltPlates.Find(tmpId) as TUramakiLivingPlate;

  tmpId := aXMLElement.GetAttribute('publisherId');
  assert (tmpId <> '');
  Self.Publication.Publisher := aPublishers.FindById(tmpId);
  cursor := TmXmlElementCursor.Create(aXMLElement, 'publicationContext');
  try
    assert (cursor.Count = 1);
    Self.Publication.PublicationContext.LoadFromXML(cursor.Elements[0]);
  finally
    cursor.Free;
  end;
  cursor := TmXmlElementCursor.Create(aXMLElement, 'transformations');
  try
    for i := 0 to cursor.Count - 1 do
    begin
      tmpId := cursor.Elements[i].GetAttribute('transformerId');
      tmpTransformation := Self.Transformations.Add;
      tmpTransformation.Transformer := aTransformers.FindById(tmpId);
      cursor2 := TmXmlElementCursor.Create(cursor.Elements[i], 'transformationContext');
      try
        assert (cursor2.Count = 1);
        tmpTransformation.TransformationContext.LoadFromXML(cursor2.Elements[0]);
      finally
        cursor2.Free;
      end;
    end;
  finally
    cursor.Free;
  end;

end;

procedure TUramakiLivingPlate.RefreshData;
begin

end;


{ TUramakiActualPublication }

procedure TUramakiActualPublication.SetPublisher(AValue: TUramakiPublisher);
begin
  if FPublisher = AValue then Exit;
  FPublisher := AValue;
  FPublicationContext := FPublisher.CreatePublicationContext;
end;

constructor TUramakiActualPublication.Create;
begin
  FPublicationContext := nil;
  FPublisher := nil;
end;

destructor TUramakiActualPublication.Destroy;
begin
  FreeAndNil(FPublicationContext);
  inherited Destroy;
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

procedure TUramakiActualTransformation.SetTransformer(
  AValue: TUramakiTransformer);
begin
  if FTransformer=AValue then Exit;
  FTransformer:=AValue;
  FTransformationContext := FTransformer.CreateTransformationContext;
end;

constructor TUramakiActualTransformation.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FTransformationContext := nil;
  FTransformer := nil;
end;

destructor TUramakiActualTransformation.Destroy;
begin
  FreeAndNil(FTransformationContext);
  inherited Destroy;
end;

end.
