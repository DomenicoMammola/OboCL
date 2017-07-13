unit UramakiDesktopLayout;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  contnrs, ExtCtrls,
  mXML;

type

  TContainerType = (ctVertical, ctHorizontal, ctTabbed);

  TDoLinkLayoutPanelToPlate = procedure (aItem : TPanel; aLivingPlateInstanceIdentificator : TGuid) of object;

  TUramakiDesktopLayoutConfItem = class
  public
    function GetDescription : String; virtual; abstract;
    procedure SaveToXMLElement (aElement : TmXmlElement); virtual; abstract;
    procedure LoadFromXMLElement (aElement : TmXmlElement); virtual; abstract;
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
    constructor Create;
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

{ TUramakiDesktopLayoutConfSimpleItem }

function TUramakiDesktopLayoutConfSimpleItem.GetDescription: String;
begin
  Result := GUIDToString(Self.LivingPlateIdentifier);
end;

procedure TUramakiDesktopLayoutConfSimpleItem.Assign(aSource: TUramakiDesktopLayoutConfItem);
begin
  assert (aSource is TUramakiDesktopLayoutConfSimpleItem);
  Self.LivingPlateIdentifier := (aSource as TUramakiDesktopLayoutConfSimpleItem).LivingPlateIdentifier;
end;

procedure TUramakiDesktopLayoutConfSimpleItem.SaveToXMLElement(aElement: TmXmlElement);
begin
  aElement.SetAttribute('livingPlateInstanceIdenfier', GUIDToString(FLivingPlateIdentifier));
end;

procedure TUramakiDesktopLayoutConfSimpleItem.LoadFromXMLElement(aElement: TmXmlElement);
begin
  Self.FLivingPlateIdentifier := StringToGUID(aElement.GetAttribute('livingPlateInstanceIdenfier'));
end;

{ TUramakiDesktopLayoutConfContainerItem }

constructor TUramakiDesktopLayoutConfContainerItem.Create;
begin
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
