// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit UramakiDesktopGUI;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  Controls, Classes, StdCtrls, ExtCtrls, contnrs,
  Graphics, Menus, Dialogs,
  oMultiPanelSetup, OMultiPanel,
  ATTabs,
  UramakiDesktopLayout, UramakiDesktopPanelConfigForm, PilePanel;

type

  { TUramakiDesktopPanel }

  TUramakiDesktopPanel = class (TPanel)
  protected
    FTabs : TATTabs;
    FTabData : TATTabData;
    FPopupMenu : TPopupMenu;
    FConfigureMenuItem : TMenuItem;

    procedure CreateTabs; virtual;
    procedure OnConfigurePanel (Sender : TObject);
    procedure SetTabData(AValue: TATTabData); virtual;
  public
    procedure CreatePopupMenu; virtual;

    function ExportAsConfItem : TUramakiDesktopLayoutConfItem; virtual; abstract;
    procedure ImportFromConfItem (aSource : TUramakiDesktopLayoutConfItem; aDoLinkCallback: TDoLinkLayoutPanelToPlate); virtual; abstract;
    function HowManySubReports : integer; virtual; abstract;

    property TabData: TATTabData read FTabData write SetTabData;
  end;

  { TUramakiDesktopSimplePanel }

  TUramakiDesktopSimplePanel = class(TUramakiDesktopPanel)
  strict private
    FLivingPlateInstanceIdentifier : TGuid;
    FAddMenuItem : TMenuItem;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure CreateCaptionPanel;
    procedure CreatePopupMenu; override;

    function ExportAsConfItem : TUramakiDesktopLayoutConfItem; override;
    procedure ImportFromConfItem (aSource : TUramakiDesktopLayoutConfItem; aDoLinkCallback: TDoLinkLayoutPanelToPlate); override;
    function HowManySubReports : integer; override;

    property LivingPlateInstanceIdentifier : TGuid read FLivingPlateInstanceIdentifier write FLivingPlateInstanceIdentifier;
    property AddMenuItem : TMenuItem read FAddMenuItem;
  end;



  { TUramakiDesktopContainerPanel }

  TUramakiDesktopContainerPanel = class (TUramakiDesktopPanel)
  strict private
    FContainerType : TContainerType;
    FRootPanel : TOMultiPanel;
    FPageControl : TPilePanel;
    FItems : TObjectList;
    FPopupMenu : TPopupMenu;

    procedure OnTabClick (aSender : TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init(aContainerType : TContainerType);
    function Count : integer;
    function Get (aIndex : integer) : TUramakiDesktopPanel;
    function HowManySubReports : integer; override;

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

procedure TUramakiDesktopPanel.SetTabData(AValue: TATTabData);
begin
  if FTabData=AValue then Exit;
  FTabData:=AValue;
  if Assigned(FPopupMenu) then
    FTabData.TabPopupMenu := Self.FPopupMenu;
end;

procedure TUramakiDesktopPanel.CreatePopupMenu;
begin
  FPopupMenu := TPopupMenu.Create(Self);
  FConfigureMenuItem := TMenuItem.Create(FPopupMenu);
  FConfigureMenuItem.Caption := 'Configure header';
  FConfigureMenuItem.OnClick:= Self.OnConfigurePanel;
  FPopupMenu.Items.Add(FConfigureMenuItem);
  if Assigned(FTabData) then
    FTabData.TabPopupMenu := Self.FPopupMenu;
end;

procedure TUramakiDesktopPanel.CreateTabs;
begin
  FTabs := TATTabs.Create(Self);
  FTabs.Parent := Self;
  FTabs.Align:= alTop;
  //FTabs.TabAngle:= 4;
  //FTabs.Height:= 56;
  FTabs.TabDoubleClickClose:= false;
  FTabs.TabDoubleClickPlus:= false;
  FTabs.TabShowClose:= tbShowNone;
  FTabs.TabShowPlus:= false;
  FTabs.TabShowEntireColor:= true;
  FTabs.Height:= 24;
  FTabs.TabHeight:= 18;
  FTabs.TabWidthMax:= 200;
  FTabs.ColorBg:= clMenu;
end;

(*
procedure TUramakiDesktopPanel.OnPopupMenu(Sender: TObject);
begin
  if Self is TUramakiDesktopContainerPanel then
  begin
    if (Self as TUramakiDesktopContainerPanel).ContainerType = ctTabbed then
    begin
      if Self.FTabs.TabIndex >= 0 then
      begin
        FAddMenuItem.Visible := (Self as TUramakiDesktopContainerPanel).Get(Self.FTabs.TabIndex) is TUramakiDesktopSimplePanel;
      end;
    end
  end;
end;*)

procedure TUramakiDesktopPanel.OnConfigurePanel(Sender: TObject);
var
  Dlg : TUramakiPanelConfigurationForm;
  Ref : TATTabData;
begin
  if Assigned(FTabData) then
    Ref := FTabData
  else
  begin
    if Assigned(FTabs) then
    begin
      Ref := FTabs.GetTabData(FTabs.TabIndex);
    end;
  end;
  if not Assigned(Ref) then
    exit;

  Dlg := TUramakiPanelConfigurationForm.Create(nil);
  try
    Dlg.Init(Ref.TabCaption, Ref.TabColor);
    if Dlg.ShowModal = mrOk then
    begin
      Dlg.GetValues(Ref.TabCaption, Ref.TabColor);
      if Assigned(FTabs) then
        FTabs.Invalidate;
    end;
  finally
    Dlg.Free;
  end;
end;

{ TUramakiDesktopContainerPanel }

procedure TUramakiDesktopContainerPanel.OnTabClick(aSender: TObject);
begin
  if (Self.FContainerType = ctTabbed) then
    FPageControl.ActivePanelIndex:= FTabs.TabIndex;
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
    FPageControl := TPilePanel.Create(Self);
    FPageControl.Parent := Self;
    FPageControl.Align:= alClient;
    //FPageControl.BorderWidth:= 0;
    //FPageControl.BorderSpacing.InnerBorder := 0;
    //FPageControl.ShowTabs:= false;
    CreateTabs;
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

function TUramakiDesktopContainerPanel.HowManySubReports : integer;
var
  i, c : integer;
begin
  c := 0;
  for i := 0 to Self.Count - 1 do
  begin
    if (Self.Get(i) is TUramakiDesktopContainerPanel) then
    begin
      c := c + (Self.Get(i) as TUramakiDesktopContainerPanel).HowManySubReports;
    end
    else
    begin
      inc (c);
    end;
  end;
  Result := c;
end;

function TUramakiDesktopContainerPanel.ExportAsConfItem: TUramakiDesktopLayoutConfItem;
var
  i : integer;
  tmp : TUramakiDesktopLayoutConfItem;
begin
  Result := TUramakiDesktopLayoutConfContainerItem.Create;
  (Result as TUramakiDesktopLayoutConfContainerItem).ContainerType:= Self.ContainerType;
  if Assigned(FTabData) then
  begin
    (Result as TUramakiDesktopLayoutConfContainerItem).Caption:= Self.FTabData.TabCaption;
    (Result as TUramakiDesktopLayoutConfContainerItem).Color:= Self.FTabData.TabColor;
  end;
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
  assert (aSource is TUramakiDesktopLayoutConfContainerItem);
  tmpSource := aSource as TUramakiDesktopLayoutConfContainerItem;
  if Assigned(FTabData) then
  begin
    FTabData.TabCaption := tmpSource.Caption;
    FTabData.TabColor := tmpSource.Color;
  end;

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

  if Assigned(FTabs) and (FTabs.TabCount > 0) then
    FTabs.TabIndex := 0;
  if Assigned(FPageControl) and (FPageControl.Count > 0) then
    FPageControl.ActivePanelIndex:= 0;
end;

function TUramakiDesktopContainerPanel.AddItem : TUramakiDesktopSimplePanel;
var
  ts : TPanel;
begin
  if FContainerType = ctTabbed then
  begin
    FTabs.AddTab(-1, 'report ' + IntToStr(FTabs.TabCount));
    ts := FPageControl.AddPanel;
    ts.BorderWidth:= 0;
    Result := TUramakiDesktopSimplePanel.Create(ts);
    Result.Parent := ts;
    Result.Align := alClient;
    Result.TabData := FTabs.GetTabData(FTabs.TabCount - 1);
    Result.CreatePopupMenu;
    FTabs.TabIndex:= FTabs.TabCount - 1;
  end
  else
  begin
    Result := TUramakiDesktopSimplePanel.Create(FRootPanel);
    Result.CreateCaptionPanel;
    Result.CreatePopupMenu;
    Result.Parent := FRootPanel;
    Result.Align:= alClient;
    FRootPanel.PanelCollection.AddControl(Result);
  end;
  FItems.Add(Result);
end;

function TUramakiDesktopContainerPanel.AddContainer: TUramakiDesktopContainerPanel;
var
  ts : TPanel;
begin
  if FContainerType = ctTabbed then
  begin
    ts := FPageControl.AddPanel;
    Result := TUramakiDesktopContainerPanel.Create(ts);
    Result.Parent := ts;
    Result.Align := alClient;
    FTabs.AddTab(-1, 'report ' + IntToStr(FTabs.TabCount));
    Result.TabData := FTabs.GetTabData(FTabs.TabCount - 1);
    Result.CreatePopupMenu;
    FTabs.TabIndex:= FTabs.TabCount - 1;
  end
  else
  begin
    Result := TUramakiDesktopContainerPanel.Create(FRootPanel);
    Result.Parent := FRootPanel;
    Result.Align:= alClient;
    FRootPanel.PanelCollection.AddControl(Result);
    Result.TabData := Self.TabData;
  end;
  FItems.Add(Result);
end;

{ TUramakiDesktopSimplePanel }


procedure TUramakiDesktopSimplePanel.CreatePopupMenu;
begin
  inherited;
  FAddMenuItem := TMenuItem.Create(FPopupMenu);
  FAddMenuItem.Caption:= 'Add a new child to report..';
  FPopupMenu.Items.Add(FAddMenuItem);

//  FPopupMenu.OnPopup:= Self.OnPopupMenu;
end;


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

procedure TUramakiDesktopSimplePanel.CreateCaptionPanel;
begin
  CreateTabs;
  FTabs.AddTab(-1, 'report');
  FTabs.TabWidthMax:= 3000;
  FTabs.TabShowMenu := false;
  FTabs.TabAngle:= 0;
  FTabs.Height:= FTabs.TabHeight;
  FTabs.TabIndentTop:= 0;
  FTabs.TabIndentInit:= 0;
  Self.TabData := FTabs.GetTabData(0);
  FTabs.TabIndex:= FTabs.TabCount - 1;
end;


function TUramakiDesktopSimplePanel.ExportAsConfItem: TUramakiDesktopLayoutConfItem;
begin
  Result := TUramakiDesktopLayoutConfSimpleItem.Create;
  (Result as TUramakiDesktopLayoutConfSimpleItem).LivingPlateIdentifier:= Self.FLivingPlateInstanceIdentifier;
  if Assigned(FTabData) then
  begin
    (Result as TUramakiDesktopLayoutConfSimpleItem).Caption:= Self.FTabData.TabCaption;
    (Result as TUramakiDesktopLayoutConfSimpleItem).Color:= Self.FTabData.TabColor;
  end;
end;

procedure TUramakiDesktopSimplePanel.ImportFromConfItem(aSource: TUramakiDesktopLayoutConfItem; aDoLinkCallback: TDoLinkLayoutPanelToPlate);
begin
  Self.FLivingPlateInstanceIdentifier := (aSource as TUramakiDesktopLayoutConfSimpleItem).LivingPlateIdentifier;
  if Assigned(FTabData) then
  begin
    FTabData.TabCaption:= (aSource as TUramakiDesktopLayoutConfSimpleItem).Caption;
    FTabData.TabColor:= (aSource as TUramakiDesktopLayoutConfSimpleItem).Color;
  end;
end;

function TUramakiDesktopSimplePanel.HowManySubReports: integer;
begin
  Result := 1;
end;

end.
