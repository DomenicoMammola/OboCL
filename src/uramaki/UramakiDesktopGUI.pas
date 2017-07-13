unit UramakiDesktopGUI;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  Controls, Classes, StdCtrls, ExtCtrls, ComCtrls, contnrs,
  Graphics,
  oMultiPanelSetup, OMultiPanel,
  mXML,
  UramakiDesktopLayout;

type

  TDoLinkLayoutPanelToPlate = procedure (aItem : TPanel; aLivingPlateInstanceIdentificator : TGuid) of object;

  TUramakiDesktopPanel = class (TPanel)
  public
    procedure SaveToXMLElement (aElement : TmXmlElement); virtual; abstract;
    procedure LoadFromXMLElement (aElement : TmXmlElement; aDoLinkCallback: TDoLinkLayoutPanelToPlate); virtual; abstract;
  end;



  { TUramakiDesktopSimplePanel }

  TUramakiDesktopSimplePanel = class(TUramakiDesktopPanel)
  strict private
    FTitleBar : TPanel;
    FContentPanel : TPanel;
    FLivingPlateInstanceIdentifier : TGuid;
  public
    constructor Create(TheOwner: TComponent); override;

    procedure SaveToXMLElement (aElement : TmXmlElement); override;
    procedure LoadFromXMLElement (aElement : TmXmlElement; aDoLinkCallback: TDoLinkLayoutPanelToPlate); override;

    property TitleBar : TPanel read FTitleBar;
    property ContentPanel : TPanel read FContentPanel;
    property LivingPlateInstanceIdentifier : TGuid read FLivingPlateInstanceIdentifier write FLivingPlateInstanceIdentifier;
  end;



  { TUramakiDesktopContainerPanel }

  TUramakiDesktopContainerPanel = class (TUramakiDesktopPanel)
  strict private
    FContainerType : TContainerType;
    FRootPanel : TOMultiPanel;
    FPageControl : TPageControl;
    FItems : TObjectList;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init(aContainerType : TContainerType);
    function Count : integer;
    function Get (aIndex : integer) : TUramakiDesktopPanel;

    procedure SaveToXMLElement (aElement : TmXmlElement); override;
    procedure LoadFromXMLElement (aElement : TmXmlElement; aDoLinkCallback: TDoLinkLayoutPanelToPlate); override;

    function AddItem : TUramakiDesktopSimplePanel;
    function AddContainer : TUramakiDesktopContainerPanel;
    property ContainerType : TContainerType read FContainerType;
  end;

implementation

uses
  SysUtils;

{ TUramakiDesktopContainerPanel }

constructor TUramakiDesktopContainerPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FItems := TObjectList.Create(false);
end;

destructor TUramakiDesktopContainerPanel.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TUramakiDesktopContainerPanel.Init(aContainerType: TContainerType);
begin
  FContainerType:= aContainerType;
  FreeAndNil(FPageControl);
  FreeAndNil(FRootPanel);
  if FContainerType = ctTabbed then
  begin
    FPageControl := TPageControl.Create(Self);
    FPageControl.Parent := Self;
    FPageControl.Align:= alClient;
  end
  else
  begin
    FRootPanel := TOMultiPanel.Create(Self);
    FRootPanel.Parent := Self;
    FRootPanel.Align:= alClient;
    if FContainerType = ctHorizontal then
      FRootPanel.PanelType:= ptHorizontal
    else
      FRootPanel.PanelType:= ptVertical;
  end;
end;

function TUramakiDesktopContainerPanel.Count: integer;
begin
  Result := FItems.Count;
end;

function TUramakiDesktopContainerPanel.Get(aIndex: integer): TUramakiDesktopPanel;
begin
  Result := FItems.Items[aIndex] as TUramakiDesktopPanel;
end;

procedure TUramakiDesktopContainerPanel.SaveToXMLElement(aElement: TmXmlElement);
var
  i : integer;
  tmpElement : TmXmlElement;
begin
  aElement.SetAttribute('containerType', TContainerTypeToString(Self.ContainerType));
  for i := 0 to Count - 1 do
  begin
    tmpElement := aElement.AddElement('layoutItem');
    if Self.Get(i) is TUramakiDesktopContainerPanel then
      tmpElement.SetAttribute('type', 'container')
    else
      tmpElement.SetAttribute('type', 'simple');
    Self.Get(i).SaveToXMLElement(tmpElement);
  end;
end;

procedure TUramakiDesktopContainerPanel.LoadFromXMLElement(aElement: TmXmlElement; aDoLinkCallback: TDoLinkLayoutPanelToPlate);
var
  tmpContainerType : TContainerType;
  cursor : TmXmlElementCursor;
  i : integer;
  simpleItem : TUramakiDesktopSimplePanel;
begin
  tmpContainerType := StringToTContainerType(aElement.GetAttribute('containerType'));
  Self.Init(tmpContainerType);
  cursor := TmXmlElementCursor.Create(aElement, 'layoutItem');
  try
    for i := 0 to cursor.Count - 1 do
    begin
      if cursor.Elements[i].GetAttribute('type') = 'container' then
      begin
        Self.AddContainer.LoadFromXMLElement(cursor.Elements[i], aDoLinkCallback);
      end
      else
      begin
        simpleItem := Self.AddItem;
        simpleItem.LoadFromXMLElement(cursor.Elements[i], aDoLinkCallback);
        aDoLinkCallback(simpleItem, simpleItem.LivingPlateInstanceIdentifier);
      end;
    end;
  finally
    cursor.Free;
  end;
end;

function TUramakiDesktopContainerPanel.AddItem : TUramakiDesktopSimplePanel;
var
  ts : TTabSheet;
begin
  if FContainerType = ctTabbed then
  begin
    ts := FPageControl.AddTabSheet;
    Result := TUramakiDesktopSimplePanel.Create(ts);
    Result.Parent := ts;
    Result.Align := alClient;
  end
  else
  begin
    Result := TUramakiDesktopSimplePanel.Create(FRootPanel);
    Result.Parent := FRootPanel;
    Result.Align:= alClient;
    FRootPanel.PanelCollection.AddControl(Result);
  end;
  FItems.Add(Result);
end;

function TUramakiDesktopContainerPanel.AddContainer: TUramakiDesktopContainerPanel;
var
  ts : TTabSheet;
begin
  if FContainerType = ctTabbed then
  begin
    ts := FPageControl.AddTabSheet;
    Result := TUramakiDesktopContainerPanel.Create(ts);
    Result.Parent := ts;
    Result.Align := alClient;
  end
  else
  begin
    Result := TUramakiDesktopContainerPanel.Create(FRootPanel);
    Result.Parent := FRootPanel;
    Result.Align:= alClient;
    FRootPanel.PanelCollection.AddControl(Result);
  end;
  FItems.Add(Result);
end;

{ TUramakiDesktopSimplePanel }

constructor TUramakiDesktopSimplePanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FTitleBar := TPanel.Create(Self);
  FTitleBar.Color:= clBlue;
  FTitleBar.Parent := Self;
  FTitleBar.Align:= alTop;
  FTitleBar.BorderStyle:= bsNone;
  FTitleBar.BevelInner:= bvNone;
  FTitleBar.BevelOuter:= bvNone;
  FTitleBar.Height:= 30;

  FContentPanel := TPanel.Create(Self);
  FContentPanel.Parent := Self;
  FContentPanel.BorderStyle:= bsNone;
  FContentPanel.BevelOuter:= bvNone;
  FContentPanel.BevelInner:= bvNone;
  FContentPanel.Align:= alClient;
end;

procedure TUramakiDesktopSimplePanel.SaveToXMLElement(aElement: TmXmlElement);
begin
  aElement.SetAttribute('livingPlateInstanceIdenfier', GUIDToString(FLivingPlateInstanceIdentifier));
end;

procedure TUramakiDesktopSimplePanel.LoadFromXMLElement(aElement: TmXmlElement; aDoLinkCallback: TDoLinkLayoutPanelToPlate);
begin
  Self.FLivingPlateInstanceIdentifier := StringToGUID(aElement.GetAttribute('livingPlateInstanceIdenfier'));
end;

end.
