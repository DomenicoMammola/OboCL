// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mGridColumnsSettingsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BufDataset, FileUtil, Forms, Controls,
  Menus, ExtCtrls, StdCtrls, EditBtn,

  OMultiPanel, oMultiPanelSetup,

  mGridColumnSettings, mUtility;

resourcestring
  SLabelHiddenCols = 'Hidden columns';
  SLabelVisibleCols = 'Visible columns';
  SLabelProperties = 'Properties of column';
  SLabelLabel = 'Label:';
  SLabelFormat = 'Display format:';
  SButtonFind = 'Find..';
  SHideAllCommand = 'Hide all';

type

  { TGridColumnsSettingsFrame }

  TGridColumnsSettingsFrame = class(TFrame)
  strict private
    FSettings : TmGridColumnsSettings;
    FRootPanel : TOMultiPanel;
    FHiddenColsPanel, FVisibleColsPanel, FPropertiesPanel,
      FLabelPanel, FFormatPanel: TPanel;
    FHiddenColFindBtn, FVisibleColFindBtn : TEditButton;
    FLabelHidden, FLabelVisible, FPropertiesLabel: TLabel;
    FListBoxHiddenColumns, FListBoxVisibleColumns : TListBox;
    FLELabel, FLEFormat: TLabeledEdit;
    FVisibleColsMenu : TPopupMenu;

    FModifiedSettings : TmGridColumnsSettings;
    FCurrentProperties: TmGridColumnSettings;
    procedure ClearProperties;
    procedure CreateHiddenColsPanel;
    procedure CreateVisibleColsPanel;
    procedure CreatePropertiesPanel;
    procedure LBDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);

    procedure LBVisibleColumnsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LBVisibleColumnsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure LBVisibleColumnsSelectionChange(Sender: TObject; User: boolean);
    procedure LBVisibleColumnsStartDrag(Sender: TObject; var DragObject: TDragObject);

    procedure LBHiddenColumnsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LBHiddenColumnsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure LBHiddenColumnsStartDrag(Sender: TObject; var DragObject: TDragObject);

    procedure LEFormatEditingDone(Sender: TObject);
    procedure LELabelEditingDone(Sender: TObject);
    procedure OnFindHiddenCol(Sender: TObject);
    procedure OnFindVisibleCol(Sender: TObject);
    procedure OnHideAllFields(Sender : TObject);
    function GetLBLine(const aColSettings: TmGridColumnSettings) : String;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init (aSettings : TmGridColumnsSettings);
    procedure UpdateSettings;
  end;

implementation

uses
  Graphics, LCLType,
  mDBGrid;

{$R *.lfm}

{ TGridColumnsSettingsFrame }

procedure TGridColumnsSettingsFrame.LBVisibleColumnsSelectionChange(Sender: TObject; User: boolean);
begin
  ClearProperties;
  if (FListBoxVisibleColumns.SelCount = 1) and (FListBoxVisibleColumns.ItemIndex >= 0) then
  begin
    FCurrentProperties:= FListBoxVisibleColumns.Items.Objects[FListBoxVisibleColumns.ItemIndex] as TmGridColumnSettings;
    FLELabel.Text:= FCurrentProperties.DisplayLabel.AsString;
    FLEFormat.Text:= FCurrentProperties.DisplayFormat.AsString;
  end;
end;

procedure TGridColumnsSettingsFrame.LBDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  aColor: TColor;
  tmpListBox : TListBox;
  tmpProperties : TmGridColumnSettings;
begin
  tmpListBox := Control as TListBox;
  if odSelected in State then
  begin
    if tmpListBox.Focused then
      tmpListBox.Canvas.Brush.Color := clHighlight
    else
      tmpListBox.Canvas.Brush.Color := clGray;
    tmpListBox.Canvas.Font.Color := clHighlightText;
  end
  else
  begin
    if (Index mod 2 = 0) then
      aColor:= clWhite
    else
      aColor := clMoneyGreen;
    tmpListBox.Canvas.Brush.Color:=aColor;
  end;

  tmpListBox.Canvas.FillRect(ARect);
  if Assigned(tmpListBox.Items.Objects[index]) then
  begin
    tmpProperties:= tmpListBox.Items.Objects[index] as TmGridColumnSettings;
    tmpListBox.Canvas.TextRect(ARect, 2, ARect.Top+2, GetLBLine(tmpProperties));
  end
  else
    tmpListBox.Canvas.TextRect(ARect, 2, ARect.Top+2, tmpListBox.Items[Index]);
end;

procedure TGridColumnsSettingsFrame.LBVisibleColumnsDragDrop(Sender,Source: TObject; X, Y: Integer);
var
  dest, prev : integer;
  old: String;
  oldObj : TObject;
  pt : TPoint;
begin
  if Source = FListBoxVisibleColumns then
  begin
    prev := FListBoxVisibleColumns.ItemIndex;
    if prev < 0 then
      exit;
    pt.x:= X;
    pt.y:= Y;
    dest := FListBoxVisibleColumns.ItemAtPos(pt, true);
    old := FListBoxVisibleColumns.Items[prev];
    oldObj := FListBoxVisibleColumns.Items.Objects[prev];

    if dest >= 0 then
    begin
      FListBoxVisibleColumns.DeleteSelected;
      if dest < prev then
        FListBoxVisibleColumns.Items.InsertObject(dest, old, oldObj)
      else
        FListBoxVisibleColumns.Items.InsertObject(dest - 1, old, oldObj);
    end;
  end
  else if Source = FListBoxHiddenColumns then
  begin
    prev := FListBoxHiddenColumns.ItemIndex;
    if prev < 0 then
      exit;
    pt.x:= X;
    pt.y:= Y;
    dest := FListBoxVisibleColumns.ItemAtPos(pt, true);
    old := FListBoxHiddenColumns.Items[prev];
    oldObj := FListBoxHiddenColumns.Items.Objects[prev];

    FListBoxHiddenColumns.DeleteSelected;
    if dest >= 0 then
      FListBoxVisibleColumns.Items.InsertObject(dest, old, oldObj)
    else
      FListBoxVisibleColumns.AddItem(old, oldObj);
  end;
  LBVisibleColumnsSelectionChange(nil, true);
end;

procedure TGridColumnsSettingsFrame.LBVisibleColumnsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = FListBoxVisibleColumns) or (Source = FListBoxHiddenColumns);
end;

procedure TGridColumnsSettingsFrame.LBVisibleColumnsStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  //
end;

procedure TGridColumnsSettingsFrame.LBHiddenColumnsDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  dest, prev : integer;
  old: String;
  oldObj : TObject;
  pt : TPoint;
begin
  prev := FListBoxVisibleColumns.ItemIndex;
  if prev < 0 then
    exit;
  pt.x:= X;
  pt.y:= Y;
  dest := FListBoxHiddenColumns.ItemAtPos(pt, true);
  old := FListBoxVisibleColumns.Items[prev];
  oldObj := FListBoxVisibleColumns.Items.Objects[prev];

  FListBoxVisibleColumns.DeleteSelected;
  if dest >= 0 then
    FListBoxHiddenColumns.Items.InsertObject(dest, old, oldObj)
  else
    FListBoxHiddenColumns.AddItem(old, oldObj);
  LBVisibleColumnsSelectionChange(nil, true);
end;

procedure TGridColumnsSettingsFrame.LBHiddenColumnsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = FListBoxVisibleColumns);
end;

procedure TGridColumnsSettingsFrame.LBHiddenColumnsStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  //
end;

procedure TGridColumnsSettingsFrame.LEFormatEditingDone(Sender: TObject);
begin
  if Assigned(FCurrentProperties) then
  begin
    if Trim(FLEFormat.Text) <> '' then
      FCurrentProperties.DisplayFormat.Value:= Trim(FLEFormat.Text)
    else
      FCurrentProperties.DisplayFormat.IsNull:= true;
  end;
end;

procedure TGridColumnsSettingsFrame.LELabelEditingDone(Sender: TObject);
begin
  if Assigned(FCurrentProperties) then
  begin
    if Trim(FLELabel.Text) <> '' then
      FCurrentProperties.DisplayLabel.Value:= Trim(FLELabel.Text)
    else
      FCurrentProperties.DisplayLabel.IsNull:= true;
    FListBoxVisibleColumns.Invalidate;
  end;
end;

procedure TGridColumnsSettingsFrame.OnFindHiddenCol(Sender: TObject);
var
  start, i: integer;
begin
  if FHiddenColFindBtn.Text = '' then
    exit;

  if (FListBoxHiddenColumns.ItemIndex >= 0) then
    start := FListBoxHiddenColumns.ItemIndex
  else
    start := -1;
  for i := start + 1 to FListBoxHiddenColumns.Count - 1 do
  begin
    if Pos(Uppercase(FHiddenColFindBtn.Text), Uppercase(FListBoxHiddenColumns.Items[i])) > 0 then
    begin
      FListBoxHiddenColumns.ItemIndex:= i;
      exit;
    end;
  end;
  for i := 0 to start do
  begin
    if Pos(Uppercase(FHiddenColFindBtn.Text), Uppercase(FListBoxHiddenColumns.Items[i])) > 0 then
    begin
      FListBoxHiddenColumns.ItemIndex:= i;
      exit;
    end;
  end;
end;

procedure TGridColumnsSettingsFrame.OnFindVisibleCol(Sender: TObject);
var
  start, i: integer;
begin
  if FVisibleColFindBtn.Text = '' then
    exit;

  if (FListBoxVisibleColumns.ItemIndex >= 0) then
    start := FListBoxVisibleColumns.ItemIndex
  else
    start := -1;
  for i := start + 1 to FListBoxVisibleColumns.Count - 1 do
  begin
    if Pos(Uppercase(FVisibleColFindBtn.Text), Uppercase(FListBoxVisibleColumns.Items[i])) > 0 then
    begin
      FListBoxVisibleColumns.ItemIndex:= i;
      exit;
    end;
  end;
  for i := 0 to start do
  begin
    if Pos(Uppercase(FVisibleColFindBtn.Text), Uppercase(FListBoxVisibleColumns.Items[i])) > 0 then
    begin
      FListBoxVisibleColumns.ItemIndex:= i;
      exit;
    end;
  end;
end;

procedure TGridColumnsSettingsFrame.OnHideAllFields(Sender: TObject);
var
  i : integer;
  op : TmGridColumnSettings;
begin
  for i := FListBoxVisibleColumns.Count - 1 downto 0 do
  begin
    op := FListBoxVisibleColumns.Items.Objects[i] as TmGridColumnSettings;
    FListBoxHiddenColumns.Items.InsertObject(0, FListBoxVisibleColumns.Items[i], op);
  end;
  FListBoxVisibleColumns.Clear;
end;

function TGridColumnsSettingsFrame.GetLBLine(const aColSettings: TmGridColumnSettings): String;
begin
  Result := aColSettings.DisplayLabel.AsString + ' ['+ aColSettings.FieldName + ']';
end;

procedure TGridColumnsSettingsFrame.CreateHiddenColsPanel;
begin
  FHiddenColsPanel.BevelOuter:= bvNone;

  FHiddenColFindBtn:= TEditButton.Create(FHiddenColsPanel);
  FHiddenColFindBtn.Parent:= FHiddenColsPanel;
  FHiddenColFindBtn.ButtonWidth:= 50;
  FHiddenColFindBtn.Align:= alTop;
  FHiddenColFindBtn.OnButtonClick:= @OnFindHiddenCol;
  FHiddenColFindBtn.ButtonCaption:= SButtonFind;

  FLabelHidden:= TLabel.Create(FHiddenColsPanel);
  FLabelHidden.Parent:= FHiddenColsPanel;
  FLabelHidden.Alignment:= taCenter;
  FLabelHidden.Align:= alTop;
  FLabelHidden.Caption:= SLabelHiddenCols;

  FListBoxHiddenColumns:= TListBox.Create(FHiddenColsPanel);
  FListBoxHiddenColumns.Parent:= FHiddenColsPanel;
  FListBoxHiddenColumns.Align:= alClient;
  FListBoxHiddenColumns.DragMode:= dmAutomatic;
  FListBoxHiddenColumns.OnDragDrop:= @LBHiddenColumnsDragDrop;
  FListBoxHiddenColumns.OnDragOver:= @LBHiddenColumnsDragOver;
  FListBoxHiddenColumns.OnStartDrag:= @LBHiddenColumnsStartDrag;
  FListBoxHiddenColumns.OnDrawItem:= @Self.LBDrawItem;
  FListBoxHiddenColumns.Style:= lbOwnerDrawFixed;
  FListBoxHiddenColumns.ItemHeight:= 20;
end;

procedure TGridColumnsSettingsFrame.CreateVisibleColsPanel;
var
  tmpMenuItem : TMenuItem;
begin
  FVisibleColsPanel.BevelOuter:= bvNone;

  FVisibleColFindBtn:= TEditButton.Create(FVisibleColsPanel);
  FVisibleColFindBtn.Parent:= FVisibleColsPanel;
  FVisibleColFindBtn.ButtonWidth:= 50;
  FVisibleColFindBtn.Align:= alTop;
  FVisibleColFindBtn.OnButtonClick:= @OnFindVisibleCol;
  FVisibleColFindBtn.ButtonCaption:= SButtonFind;

  FLabelVisible:= TLabel.Create(FVisibleColsPanel);
  FLabelVisible.Parent:= FVisibleColsPanel;
  FLabelVisible.Alignment:= taCenter;
  FLabelVisible.Align:= alTop;
  FLabelVisible.Caption:= SLabelVisibleCols;

  FListBoxVisibleColumns:= TListBox.Create(FVisibleColsPanel);
  FListBoxVisibleColumns.Parent:= FVisibleColsPanel;
  FListBoxVisibleColumns.Align:= alClient;
  FListBoxVisibleColumns.DragMode:= dmAutomatic;
  FListBoxVisibleColumns.OnDragDrop:= @LBVisibleColumnsDragDrop;
  FListBoxVisibleColumns.OnDragOver:= @LBVisibleColumnsDragOver;
  FListBoxVisibleColumns.OnSelectionChange:= @LBVisibleColumnsSelectionChange;
  FListBoxVisibleColumns.OnStartDrag:= @LBVisibleColumnsStartDrag;
  FListBoxVisibleColumns.OnDrawItem:= @Self.LBDrawItem;
  FListBoxVisibleColumns.Style:= lbOwnerDrawFixed;
  FListBoxVisibleColumns.ItemHeight:= 20;

  FVisibleColsMenu := TPopupMenu.Create(FListBoxVisibleColumns);
  FListBoxVisibleColumns.PopupMenu := FVisibleColsMenu;
  tmpMenuItem := TMenuItem.Create(FVisibleColsMenu);
  FVisibleColsMenu.Items.Add(tmpMenuItem);
  tmpMenuItem.Caption:= SHideAllCommand;
  tmpMenuItem.OnClick:= @OnHideAllFields;
end;

procedure TGridColumnsSettingsFrame.CreatePropertiesPanel;
begin
  FPropertiesPanel.BevelOuter := bvNone;

  FFormatPanel:= TPanel.Create(FPropertiesPanel);
  FFormatPanel.Parent:= FPropertiesPanel;
  FFormatPanel.Height:= 40;
  FFormatPanel.Align:= alTop;
  FFormatPanel.BevelOuter:= bvNone;

  FLabelPanel:= TPanel.Create(FPropertiesPanel);
  FLabelPanel.Parent:= FPropertiesPanel;
  FLabelPanel.Height:= 40;
  FLabelPanel.Align:= alTop;
  FLabelPanel.BevelOuter:= bvNone;

  FPropertiesLabel:= TLabel.Create(FPropertiesPanel);
  FPropertiesLabel.Parent:= FPropertiesPanel;
  FPropertiesLabel.Alignment:= taCenter;
  FPropertiesLabel.Align:= alTop;
  FPropertiesLabel.Caption:= SLabelProperties;

  FLEFormat:= TLabeledEdit.Create(FFormatPanel);
  FLEFormat.Parent:= FFormatPanel;
  FLEFormat.EditLabel.Caption:= SLabelFormat;
  FLEFormat.OnEditingDone:= @LEFormatEditingDone;
  FLEFormat.Align:= alBottom;

  FLELabel:= TLabeledEdit.Create(FLabelPanel);
  FLELabel.Parent:= FLabelPanel;
  FLELabel.EditLabel.Caption:= SLabelLabel;
  FLELabel.OnEditingDone:= @LELabelEditingDone;
  FLELabel.Align:= alBottom;

end;

procedure TGridColumnsSettingsFrame.ClearProperties;
begin
  FCurrentProperties:= nil;
  FLELabel.Text:= '';
  FLEFormat.Text:= '';
end;

constructor TGridColumnsSettingsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FModifiedSettings := TmGridColumnsSettings.Create();

  FRootPanel := TOMultiPanel.Create(Self);
  FRootPanel.Parent := Self;
  FRootPanel.PanelType:= ptHorizontal;
  FRootPanel.Align:= alClient;

  FHiddenColsPanel := TPanel.Create(FRootPanel);
  FHiddenColsPanel.Parent := FRootPanel;
  FRootPanel.PanelCollection.AddControl(FHiddenColsPanel);

  FVisibleColsPanel := TPanel.Create(FRootPanel);
  FVisibleColsPanel.Parent := FRootPanel;
  FRootPanel.PanelCollection.AddControl(FVisibleColsPanel);

  FPropertiesPanel := TPanel.Create(FRootPanel);
  FPropertiesPanel.Parent := FRootPanel;
  FRootPanel.PanelCollection.AddControl(FPropertiesPanel);

  CreateHiddenColsPanel;
  CreateVisibleColsPanel;
  CreatePropertiesPanel;

  FRootPanel.PanelCollection.Items[2].Position:= 1;
  FRootPanel.PanelCollection.Items[1].Position:= 0.8;
  FRootPanel.PanelCollection.Items[0].Position:= 0.4;
end;

destructor TGridColumnsSettingsFrame.Destroy;
begin
  FModifiedSettings.Free;
  inherited Destroy;
end;

procedure TGridColumnsSettingsFrame.Init(aSettings: TmGridColumnsSettings);
var
  i : integer;
  op : TmGridColumnSettings;
  k : string;
  tmpList : TStringList;
begin

  FSettings := aSettings;
  FModifiedSettings.Assign(aSettings);
  tmpList := TStringList.Create;
  try
    tmpList.Sorted:= true;

    for i := 0 to FModifiedSettings.Count -1 do
    begin
      op := FModifiedSettings.Get(i);
      if op.Visible.NotNull and op.Visible.Value then
      begin
        if op.SortOrder.IsNull then
          k := '99999'
        else
          k := AddZerosFront(op.SortOrder.Value, 5);
        tmpList.AddObject(k, op);
      end
      else
        FListBoxHiddenColumns.AddItem(GetLBLine(op), op);
    end;
    for i := 0 to tmpList.Count - 1 do
    begin
      op := tmpList.Objects[i] as TmGridColumnSettings;
      FListBoxVisibleColumns.AddItem(GetLBLine(op), op);
    end;
  finally
    tmpList.Free;
  end;

end;

procedure TGridColumnsSettingsFrame.UpdateSettings;
var
  source, dest : TmGridColumnSettings;
  i : integer;
  newWidth : integer;
begin
  for i := 0 to FListBoxHiddenColumns.Count - 1 do
  begin
    source := TmGridColumnSettings(FListBoxHiddenColumns.Items.Objects[i]);
    dest := FSettings.GetSettingsForField(source.FieldName);
    dest.Assign(source);
    dest.Visible.Value:= false;
  end;
  for i := 0 to FListBoxVisibleColumns.Count - 1 do
  begin
    source := TmGridColumnSettings(FListBoxVisibleColumns.Items.Objects[i]);
    dest := FSettings.GetSettingsForField(source.FieldName);
    if not dest.Visible.AsBoolean then
      newWidth:= 100
    else
      newWidth := source.Width.AsInteger;
    dest.Assign(source);
    dest.Visible.Value:= true;
    dest.Width.Value:= newWidth;
    dest.SortOrder.Value:= i;
  end;
end;

end.

