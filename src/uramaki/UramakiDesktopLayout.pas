// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit UramakiDesktopLayout;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  contnrs, ExtCtrls, Graphics,
  mXML;

type

  TContainerType = (ctVertical, ctHorizontal, ctTabbed);

  TDoLinkLayoutPanelToPlate = procedure (aItem : TPanel; aLivingPlateInstanceIdentificator : TGuid) of object;

  { TUramakiDesktopLayoutConfItem }

  TUramakiDesktopLayoutConfItem = class
  strict private
    FPosition : Double;
    FCaption : String;
    FColor : TColor;
  protected
    procedure InternalAssign(aSource : TUramakiDesktopLayoutConfItem);
  public
    constructor Create; virtual;

    function GetDescription : String; virtual; abstract;
    procedure SaveToXMLElement (aElement : TmXmlElement); virtual;
    procedure LoadFromXMLElement (aElement : TmXmlElement); virtual;

    property Position : Double read FPosition write FPosition;
    property Caption : String read FCaption write FCaption;
    property Color : TColor read FColor write FColor;
  end;

  { TUramakiDesktopLayoutConfSimpleItem }

  TUramakiDesktopLayoutConfSimpleItem = class (TUramakiDesktopLayoutConfItem)
  strict private
    FLivingPlateIdentifier : TGuid;
  public
    function GetDescription : String; override;
    procedure Assign(aSource : TUramakiDesktopLayoutConfItem);
    procedure SaveToXMLElement (aElement : TmXmlElement); override;
    procedure LoadFromXMLElement (aElement : TmXmlElement); override;

    property LivingPlateIdentifier : TGuid read FLivingPlateIdentifier write FLivingPlateIdentifier;
  end;

  { TUramakiDesktopLayoutConfContainerItem }

  TUramakiDesktopLayoutConfContainerItem = class (TUramakiDesktopLayoutConfItem)
  strict private
    FContainerType : TContainerType;
    FChilds : TObjectList;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure AddItem (aItem : TUramakiDesktopLayoutConfItem);
    function Count : integer;
    function Get(aIndex : integer) : TUramakiDesktopLayoutConfItem;
    procedure Clear;

    function GetDescription : String; override;
    procedure Assign(aSource : TUramakiDesktopLayoutConfItem; const aAssignChilds : boolean);
    procedure SaveToXMLElement (aElement : TmXmlElement); override;
    procedure LoadFromXMLElement (aElement : TmXmlElement); override;

    property ContainerType : TContainerType read FContainerType write FContainerType;
  end;

  function TContainerTypeToString (aValue : TContainerType) : String;
  function StringToTContainerType (aValue : String) : TContainerType;

implementation

uses
  SysUtils;

function TContainerTypeToString(aValue: TContainerType): String;
begin
  case aValue of
    ctVertical : Result := 'vertical';
    ctHorizontal : Result := 'horizontal';
  else
    Result := 'tabbed';
  end;
end;

function StringToTContainerType(aValue: String): TContainerType;
begin
  if aValue = 'vertical' then
    Result := ctVertical
  else if aValue = 'horizontal' then
    Result := ctHorizontal
  else
    Result := ctTabbed;
end;

{ TUramakiDesktopLayoutConfItem }

procedure TUramakiDesktopLayoutConfItem.InternalAssign(aSource: TUramakiDesktopLayoutConfItem);
begin
  Self.FPosition:= aSource.FPosition;
end;

constructor TUramakiDesktopLayoutConfItem.Create;
begin
  FPosition := -1;
  FCaption:= 'report';
  FColor := $808080;
end;

procedure TUramakiDesktopLayoutConfItem.SaveToXMLElement(aElement: TmXmlElement);
begin
  aElement.SetFloatAttribute('position', FPosition);
  aElement.SetAttribute('caption', FCaption);
  aElement.SetAttribute('color', ColorToString(FColor));
end;

procedure TUramakiDesktopLayoutConfItem.LoadFromXMLElement(aElement: TmXmlElement);
begin
  FPosition := aElement.GetFloatAttribute('position', -1);
  FCaption := aElement.GetAttribute('caption', 'report');
  if aElement.HasAttribute('color') then
    FColor := StringToColor(aElement.GetAttribute('color'));
end;

{ TUramakiDesktopLayoutConfSimpleItem }

function TUramakiDesktopLayoutConfSimpleItem.GetDescription: String;
begin
  Result := GUIDToString(Self.LivingPlateIdentifier);
end;

procedure TUramakiDesktopLayoutConfSimpleItem.Assign(aSource: TUramakiDesktopLayoutConfItem);
begin
  assert (aSource is TUramakiDesktopLayoutConfSimpleItem);
  InternalAssign(aSource);
  Self.LivingPlateIdentifier := (aSource as TUramakiDesktopLayoutConfSimpleItem).LivingPlateIdentifier;
end;

procedure TUramakiDesktopLayoutConfSimpleItem.SaveToXMLElement(aElement: TmXmlElement);
begin
  inherited;
  aElement.SetAttribute('livingPlateInstanceIdenfier', GUIDToString(FLivingPlateIdentifier));
end;

procedure TUramakiDesktopLayoutConfSimpleItem.LoadFromXMLElement(aElement: TmXmlElement);
begin
  inherited;
  Self.FLivingPlateIdentifier := StringToGUID(aElement.GetAttribute('livingPlateInstanceIdenfier'));
end;

{ TUramakiDesktopLayoutConfContainerItem }

constructor TUramakiDesktopLayoutConfContainerItem.Create;
begin
  inherited;
  FChilds := TObjectList.Create(true);
end;

destructor TUramakiDesktopLayoutConfContainerItem.Destroy;
begin
  FChilds.Free;
  inherited Destroy;
end;

procedure TUramakiDesktopLayoutConfContainerItem.AddItem(aItem: TUramakiDesktopLayoutConfItem);
begin
  FChilds.Add(aItem);
end;

function TUramakiDesktopLayoutConfContainerItem.Count: integer;
begin
  Result := FChilds.Count;
end;

function TUramakiDesktopLayoutConfContainerItem.Get(aIndex: integer): TUramakiDesktopLayoutConfItem;
begin
  Result := FChilds.Items[aIndex] as TUramakiDesktopLayoutConfItem;
end;

procedure TUramakiDesktopLayoutConfContainerItem.Clear;
begin
  FChilds.Clear;
end;

function TUramakiDesktopLayoutConfContainerItem.GetDescription: String;
begin
  Result := TContainerTypeToString(Self.ContainerType);
end;

procedure TUramakiDesktopLayoutConfContainerItem.Assign(aSource: TUramakiDesktopLayoutConfItem; const aAssignChilds : boolean);
var
  tmpSource : TUramakiDesktopLayoutConfContainerItem;
  i : integer;
  newItem : TUramakiDesktopLayoutConfItem;
begin
  assert (aSource is TUramakiDesktopLayoutConfContainerItem);
  InternalAssign(aSource);
  tmpSource := aSource as TUramakiDesktopLayoutConfContainerItem;
  Self.ContainerType:= tmpSource.ContainerType;

  FChilds.Clear;

  if aAssignChilds then
  begin
    for i := 0 to tmpSource.Count - 1 do
    begin
      if tmpSource.Get(i) is TUramakiDesktopLayoutConfContainerItem then
      begin
        newItem := TUramakiDesktopLayoutConfContainerItem.Create;
        Self.AddItem(newItem);
        (newItem as TUramakiDesktopLayoutConfContainerItem).Assign(tmpSource.Get(i), aAssignChilds);
      end
      else
      begin
        newItem := TUramakiDesktopLayoutConfSimpleItem.Create;
        Self.AddItem(newItem);
        (newItem as TUramakiDesktopLayoutConfSimpleItem).Assign(tmpSource.Get(i));
      end;
    end;
  end;
end;

procedure TUramakiDesktopLayoutConfContainerItem.SaveToXMLElement(aElement: TmXmlElement);
var
  i : integer;
  tmpElement : TmXmlElement;
begin
  inherited;
  aElement.SetAttribute('containerType', TContainerTypeToString(Self.ContainerType));
  for i := 0 to Count - 1 do
  begin
    tmpElement := aElement.AddElement('layoutItem');
    if Self.Get(i) is TUramakiDesktopLayoutConfContainerItem then
      tmpElement.SetAttribute('type', 'container')
    else
      tmpElement.SetAttribute('type', 'simple');
    Self.Get(i).SaveToXMLElement(tmpElement);
  end;
end;

procedure TUramakiDesktopLayoutConfContainerItem.LoadFromXMLElement(aElement: TmXmlElement);
var
  tmpContainerType : TContainerType;
  cursor : TmXmlElementCursor;
  i : integer;
  tmpItem : TUramakiDesktopLayoutConfItem;
begin
  inherited;
  Self.ContainerType := StringToTContainerType(aElement.GetAttribute('containerType'));
  cursor := TmXmlElementCursor.Create(aElement, 'layoutItem');
  try
    for i := 0 to cursor.Count - 1 do
    begin
      if cursor.Elements[i].GetAttribute('type') = 'container' then
        tmpItem := TUramakiDesktopLayoutConfContainerItem.Create
      else
        tmpItem := TUramakiDesktopLayoutConfSimpleItem.Create;
      Self.AddItem(tmpItem);
      tmpItem.LoadFromXMLElement(cursor.Elements[i]);
    end;
  finally
    cursor.Free;
  end;
end;

end.
