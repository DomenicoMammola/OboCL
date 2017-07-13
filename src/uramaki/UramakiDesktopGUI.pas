unit UramakiDesktopGUI;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  Controls, Classes, StdCtrls, ExtCtrls, ComCtrls, contnrs,
  Graphics,
  oMultiPanelSetup, OMultiPanel,
  UramakiDesktopLayout;

type

  TUramakiDesktopPanel = class (TPanel)
  public
    function ExportAsConfItem : TUramakiDesktopLayoutConfItem; virtual; abstract;
    procedure ImportFromConfItem (aSource : TUramakiDesktopLayoutConfItem; aDoLinkCallback: TDoLinkLayoutPanelToPlate); virtual; abstract;
  end;

  { TUramakiDesktopSimplePanel }

  TUramakiDesktopSimplePanel = class(TUramakiDesktopPanel)
  strict private
    FTitleBar : TPanel;
    FContentPanel : TPanel;
    FLivingPlateInstanceIdentifier : TGuid;
  public
    constructor Create(TheOwner: TComponent); override;

    function ExportAsConfItem : TUramakiDesktopLayoutConfItem; override;
    procedure ImportFromConfItem (aSource : TUramakiDesktopLayoutConfItem; aDoLinkCallback: TDoLinkLayoutPanelToPlate); override;

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

    function ExportAsConfItem : TUramakiDesktopLayoutConfItem; override;
    procedure ImportFromConfItem (aSource : TUramakiDesktopLayoutConfItem; aDoLinkCallback: TDoLinkLayoutPanelToPlate); override;

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
  FItems := TObjectList.Create(true);
end;

destructor TUramakiDesktopContainerPanel.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TUramakiDesktopContainerPanel.Init(aContainerType: TContainerType);
begin
  FContainerType:= aContainerType;
  FItems.Clear;
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

function TUramakiDesktopContainerPanel.ExportAsConfItem: TUramakiDesktopLayoutConfItem;
var
  i : integer;
begin
  Result := TUramakiDesktopLayoutConfContainerItem.Create;
  (Result as TUramakiDesktopLayoutConfContainerItem).ContainerType:= Self.ContainerType;
  for i := 0 to Count - 1 do
  begin
    if Self.Get(i) is TUramakiDesktopContainerPanel then
      (Result as TUramakiDesktopLayoutConfContainerItem).AddItem((Self.Get(i) as TUramakiDesktopContainerPanel).ExportAsConfItem)
    else
      (Result as TUramakiDesktopLayoutConfContainerItem).AddItem((Self.Get(i) as TUramakiDesktopSimplePanel).ExportAsConfItem)
  end;
end;

procedure TUramakiDesktopContainerPanel.ImportFromConfItem(aSource: TUramakiDesktopLayoutConfItem; aDoLinkCallback: TDoLinkLayoutPanelToPlate);
var
  tmpSource : TUramakiDesktopLayoutConfContainerItem;
  i : integer;
  simpleItem : TUramakiDesktopSimplePanel;
begin
  assert (tmpSource is TUramakiDesktopLayoutConfContainerItem);
  tmpSource := aSource as TUramakiDesktopLayoutConfContainerItem;
  Self.Init(tmpSource.ContainerType);
  for i := 0 to tmpSource.Count - 1 do
  begin
    if (tmpSource.Get(i) is TUramakiDesktopLayoutConfContainerItem) then
    begin
      Self.AddContainer.ImportFromConfItem(tmpSource.Get(i), aDoLinkCallback)
    end
    else
    begin
      simpleItem := Self.AddItem;
      simpleItem.ImportFromConfItem(tmpSource.Get(i), aDoLinkCallback);
      aDoLinkCallback(simpleItem, simpleItem.LivingPlateInstanceIdentifier);
    end;
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
  Self.BorderStyle:= bsNone;
  Self.BevelInner:= bvNone;
  Self.BevelOuter:= bvNone;

  FTitleBar := TPanel.Create(Self);
  FTitleBar.Color:= clBlue;
  FTitleBar.Parent := Self;
  FTitleBar.Align:= alTop;
  FTitleBar.BorderStyle:= bsNone;
  FTitleBar.BevelInner:= bvNone;
  FTitleBar.BevelOuter:= bvNone;
  FTitleBar.Height:= 20;

  FContentPanel := TPanel.Create(Self);
  FContentPanel.Parent := Self;
  FContentPanel.BorderStyle:= bsNone;
  FContentPanel.BevelOuter:= bvNone;
  FContentPanel.BevelInner:= bvNone;
  FContentPanel.Align:= alClient;
end;


function TUramakiDesktopSimplePanel.ExportAsConfItem: TUramakiDesktopLayoutConfItem;
begin
  Result := TUramakiDesktopLayoutConfSimpleItem.Create;
  (Result as TUramakiDesktopLayoutConfSimpleItem).LivingPlateIdentifier:= Self.FLivingPlateInstanceIdentifier;
end;

procedure TUramakiDesktopSimplePanel.ImportFromConfItem(aSource: TUramakiDesktopLayoutConfItem; aDoLinkCallback: TDoLinkLayoutPanelToPlate);
begin
  Self.FLivingPlateInstanceIdentifier := (aSource as TUramakiDesktopLayoutConfSimpleItem).LivingPlateIdentifier;
end;

end.
