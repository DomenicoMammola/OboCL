unit UramakiDesktopGUI;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  Controls, Classes, StdCtrls, ExtCtrls, ComCtrls, contnrs,
  Graphics, Menus,
  oMultiPanelSetup, OMultiPanel,
  ATTabs,
  UramakiDesktopLayout;

type

  { TUramakiDesktopPanel }

  TUramakiDesktopPanel = class (TPanel)
  protected
    FTabs : TATTabs;
    function CreateTabs : TATTabs;
  public
    function ExportAsConfItem : TUramakiDesktopLayoutConfItem; virtual; abstract;
    procedure ImportFromConfItem (aSource : TUramakiDesktopLayoutConfItem; aDoLinkCallback: TDoLinkLayoutPanelToPlate); virtual; abstract;
  end;

  { TUramakiDesktopSimplePanel }

  TUramakiDesktopSimplePanel = class(TUramakiDesktopPanel)
  strict private
//    FTitleBar : TPanel;
    FLivingPlateInstanceIdentifier : TGuid;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure AddTab;

    function ExportAsConfItem : TUramakiDesktopLayoutConfItem; override;
    procedure ImportFromConfItem (aSource : TUramakiDesktopLayoutConfItem; aDoLinkCallback: TDoLinkLayoutPanelToPlate); override;

//    property TitleBar : TPanel read FTitleBar;
    property LivingPlateInstanceIdentifier : TGuid read FLivingPlateInstanceIdentifier write FLivingPlateInstanceIdentifier;
  end;



  { TUramakiDesktopContainerPanel }

  TUramakiDesktopContainerPanel = class (TUramakiDesktopPanel)
  strict private
    FContainerType : TContainerType;
    FRootPanel : TOMultiPanel;
    FPageControl : TPageControl;
    FItems : TObjectList;
    FPopupMenu : TPopupMenu;
    procedure OnTabClick (aSender : TObject);
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

{ TUramakiDesktopPanel }

function TUramakiDesktopPanel.CreateTabs: TATTabs;
begin
  Result := TATTabs.Create(Self);
  Result.Parent := Self;
  Result.Align:= alTop;
  //FTabs.TabAngle:= 4;
  //FTabs.Height:= 56;
  Result.TabDoubleClickClose:= false;
  Result.TabDoubleClickPlus:= false;
  Result.TabShowClose:= tbShowNone;
  Result.TabShowPlus:= false;
  Result.Height:= 24;
  Result.TabHeight:= 18;
end;


{ TUramakiDesktopContainerPanel }

procedure TUramakiDesktopContainerPanel.OnTabClick(aSender: TObject);
begin
  if (Self.FContainerType = ctTabbed) then
    FPageControl.ActivePageIndex:= FTabs.TabIndex;
end;

constructor TUramakiDesktopContainerPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Self.BorderStyle:= bsNone;
  Self.BevelInner:= bvNone;
  Self.BevelOuter:= bvNone;
  Self.ParentColor:= true;

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
  FreeAndNil(FTabs);
  if FContainerType = ctTabbed then
  begin
    FPageControl := TPageControl.Create(Self);
    FPageControl.Parent := Self;
    FPageControl.Align:= alClient;
    FPageControl.BorderWidth:= 0;
    FPageControl.BorderSpacing.InnerBorder := 0;
    FPageControl.ShowTabs:= false;
    FTabs := CreateTabs;
    FTabs.OnTabClick:= Self.OnTabClick;
    FTabs.BorderWidth:= 0;
    FTabs.BorderStyle:= bsNone;
  end
  else
  begin
    FRootPanel := TOMultiPanel.Create(Self);
    FRootPanel.Parent := Self;
    FRootPanel.Align:= alClient;
    FRootPanel.BorderWidth:= 0;
    FRootPanel.BorderStyle:= bsNone;
    FRootPanel.BevelInner:= bvNone;
    FRootPanel.BevelOuter:= bvNone;
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
  tmp : TUramakiDesktopLayoutConfItem;
begin
  Result := TUramakiDesktopLayoutConfContainerItem.Create;
  (Result as TUramakiDesktopLayoutConfContainerItem).ContainerType:= Self.ContainerType;
  for i := 0 to Count - 1 do
  begin
    if Self.Get(i) is TUramakiDesktopContainerPanel then
    begin
      tmp := (Self.Get(i) as TUramakiDesktopContainerPanel).ExportAsConfItem;
      if Assigned(FRootPanel) then
        tmp.Position := FRootPanel.PanelCollection.Items[i].Position;
      (Result as TUramakiDesktopLayoutConfContainerItem).AddItem(tmp)
    end
    else
    begin
      tmp := (Self.Get(i) as TUramakiDesktopSimplePanel).ExportAsConfItem;
      if Assigned(FRootPanel) then
        tmp.Position := FRootPanel.PanelCollection.Items[i].Position;
      (Result as TUramakiDesktopLayoutConfContainerItem).AddItem(tmp)
    end;
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
  // recover positions
  if Assigned(FRootPanel) then
  begin
    for i := Self.Count - 1 downto 0 do
    begin
      if tmpSource.Get(i).Position >= 0 then
        FRootPanel.PanelCollection.Items[i].Position:= tmpSource.Get(i).Position;
    end;
  end;
end;

function TUramakiDesktopContainerPanel.AddItem : TUramakiDesktopSimplePanel;
var
  ts : TTabSheet;
begin
  if FContainerType = ctTabbed then
  begin
    FTabs.AddTab(-1, 'report ' + IntToStr(FTabs.TabCount));
    ts := FPageControl.AddTabSheet;
    ts.BorderWidth:= 0;
    Result := TUramakiDesktopSimplePanel.Create(ts);
    Result.Parent := ts;
    Result.Align := alClient;
  end
  else
  begin
    Result := TUramakiDesktopSimplePanel.Create(FRootPanel);
    Result.AddTab;
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
    FTabs.AddTab(FTabs.TabCount, 'report ' + IntToStr(FTabs.TabCount));
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
  Self.ParentColor:= true;


(*  FTitleBar := TPanel.Create(Self);
  FTitleBar.Color:= clBlue;
  FTitleBar.Parent := Self;
  FTitleBar.Align:= alTop;
  FTitleBar.BorderStyle:= bsNone;
  FTitleBar.BevelInner:= bvNone;
  FTitleBar.BevelOuter:= bvNone;
  FTitleBar.Height:= 20;*)
end;

procedure TUramakiDesktopSimplePanel.AddTab;
begin
  FTabs := CreateTabs;
  FTabs.AddTab(-1, 'report');
  FTabs.TabWidthMax:= 3000;
  FTabs.TabShowMenu := false;
  FTabs.TabAngle:= 0;
  FTabs.Height:= FTabs.TabHeight;
  FTabs.TabIndentTop:= 0;
  FTabs.TabIndentInit:= 0;
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
