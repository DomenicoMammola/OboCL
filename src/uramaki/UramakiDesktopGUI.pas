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
  Controls, Classes, StdCtrls, ExtCtrls, contnrs, Forms,
  Graphics, Menus, Dialogs,
  oMultiPanelSetup, OMultiPanel,
  ATTabs,
  UramakiDesktopLayout, UramakiDesktopPanelConfigForm, PilePanel;

resourcestring
  SMenuItemConfigureHeader = 'Configure header';
  SMenuItemAddChild = 'Add a child widget...';
  SLabelUpdatingCaption = 'Updating...';
  SMenuItemShowNote = 'Note...';

type

  { TUramakiDesktopPanel }

  TUramakiDesktopPanel = class (TPanel)
  protected
    FTabs : TATTabs;
    FTabData : TATTabData;
    FUpdatingLabel : TLabel;
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
    FShowNoteMenuItem : TMenuItem;
    FNote : String;
    procedure OnShowNote (Sender : TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure CreateCaptionPanel;
    procedure CreatePopupMenu; override;

    function ExportAsConfItem : TUramakiDesktopLayoutConfItem; override;
    procedure ImportFromConfItem (aSource : TUramakiDesktopLayoutConfItem; aDoLinkCallback: TDoLinkLayoutPanelToPlate); override;
    function HowManySubReports : integer; override;
    procedure StartShining;
    procedure StopShining;

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
    //FPopupMenu : TPopupMenu;

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
  SysUtils,
  mGraphicsUtility, mMemoDialog;

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
  FConfigureMenuItem.Caption := SMenuItemConfigureHeader;
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
  FTabs.OptMouseDoubleClickClose:= false;
  FTabs.OptShowPlusTab:= false;
  FTabs.OptShowXButtons:= atbxShowNone;
  FTabs.OptMouseDoubleClickClose:= false;
  FTabs.OptShowEntireColor:= true;
  FTabs.Height:= ScaleForDPI(24);
  FTabs.OptTabHeight:= ScaleForDPI(18);
  FTabs.OptTabWidthNormal:= ScaleForDPI(200);
  FTabs.ColorBg:= clMenu;
  FTabs.OptMouseDragEnabled:= false; //enable drag-drop
  FTabs.OptMouseDragOutEnabled:= false; //also enable drag-drop to another controls
  FTabs.OptShowArrowsNear:= false;
  FTabs.OptShowDropMark:= false;
  FTabs.OptShowScrollMark:= false;
  FTabs.OptButtonLayout:= '';
  FTabs.OptShowFlat:= false;
  FTabs.OptShowAngled:= true;
  FTabs.OptSpaceBetweenTabs:= 5;
  FUpdatingLabel := TLabel.Create(Self);
  FUpdatingLabel.Parent:= Self;
  FUpdatingLabel.Align:= alTop;
  FUpdatingLabel.Height:= 10;
  FUpdatingLabel.Visible:= false;
  FUpdatingLabel.Caption:= SLabelUpdatingCaption;
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
  newCaption : String;
  newColor : TColor;
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
      Dlg.GetValues(newCaption, newColor);
      Ref.TabCaption := newCaption;
      Ref.TabColor := newColor;
      if IsDark(newColor) then
        FTabs.Font.Color:= clWhite
      else
        FTabs.Font.Color:= clDefault;
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
  FShowNoteMenuItem := TMenuItem.Create(FPopupMenu);
  FShowNoteMenuItem.Caption:= SMenuItemShowNote;
  FShowNoteMenuItem.OnClick:= OnShowNote;
  FPopupMenu.Items.Add(FShowNoteMenuItem);
  FAddMenuItem := TMenuItem.Create(FPopupMenu);
  FAddMenuItem.Caption:= SMenuItemAddChild;
  FPopupMenu.Items.Add(FAddMenuItem);

//  FPopupMenu.OnPopup:= Self.OnPopupMenu;
end;

procedure TUramakiDesktopSimplePanel.OnShowNote(Sender: TObject);
begin
  FNote := TmMemoForm.ShowAndEdit(Self, FNote);
end;

constructor TUramakiDesktopSimplePanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Self.BorderStyle:= bsNone;
  Self.BevelInner:= bvNone;
  Self.BevelOuter:= bvNone;
  Self.ParentColor:= true;
  Self.FNote:= '';
end;

procedure TUramakiDesktopSimplePanel.CreateCaptionPanel;
begin
  CreateTabs;
  FTabs.AddTab(-1, 'report', nil, false, DEFAULT_TAB_COLOR);
  FTabs.OptTabWidthMinimal:= 3000;
  FTabs.OptShowDropMark := false;
  FTabs.OptShowArrowsNear:= false;
  FTabs.OptShowScrollMark:= false;
  FTabs.OptButtonLayout:= '';
  FTabs.OptShowFlat:= false;
  FTabs.Height:= FTabs.OptTabHeight;
  FTabs.OptSpacer:= 0;
  FTabs.OptSpaceInitial:= 0;
  FTabs.OptSpaceBeforeText:= 3;
  Self.TabData := FTabs.GetTabData(0);
  FTabs.TabIndex:= FTabs.TabCount - 1;
end;


function TUramakiDesktopSimplePanel.ExportAsConfItem: TUramakiDesktopLayoutConfItem;
begin
  Result := TUramakiDesktopLayoutConfSimpleItem.Create;
  (Result as TUramakiDesktopLayoutConfSimpleItem).LivingPlateIdentifier:= Self.FLivingPlateInstanceIdentifier;
  Result.Note:= FNote;
  if Assigned(FTabData) then
  begin
    (Result as TUramakiDesktopLayoutConfSimpleItem).Caption:= Self.FTabData.TabCaption;
    (Result as TUramakiDesktopLayoutConfSimpleItem).Color:= Self.FTabData.TabColor;
  end;
end;

procedure TUramakiDesktopSimplePanel.ImportFromConfItem(aSource: TUramakiDesktopLayoutConfItem; aDoLinkCallback: TDoLinkLayoutPanelToPlate);
begin
  Self.FLivingPlateInstanceIdentifier := (aSource as TUramakiDesktopLayoutConfSimpleItem).LivingPlateIdentifier;
  Self.FNote:= aSource.Note;
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

procedure TUramakiDesktopSimplePanel.StartShining;
begin
  FUpdatingLabel.Visible:= true;
  Application.ProcessMessages;
end;

procedure TUramakiDesktopSimplePanel.StopShining;
begin
  FUpdatingLabel.Visible:= false;
end;

end.
