// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mPivotSettingsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BufDataset, FileUtil, Forms, Controls,
  Menus, ExtCtrls, StdCtrls, EditBtn, contnrs,

  OMultiPanel, OMultiPanelSetup,

  mPivoter, mSummary, mDataProviderFieldDefs;

resourcestring
  SLabelFields = 'Fields';
  SButtonFind = 'Find..';
  SLabelHorizontalFields = 'Horizontal';
  SLabelVerticalFields = 'Vertical';
  SLabelDataFields = 'Data';
  SLabelGroupByDefOperator = 'Group-by operator:';


  SLabelHiddenCols = 'Hidden columns';
  SLabelProperties = 'Properties of column';
  SLabelLabel = 'Label:';
  SLabelFormat = 'Display format:';
  SHideAllCommand = 'Hide all';

type

  { TPivotFieldsSettingsFrame }

  TPivotFieldsSettingsFrame = class(TFrame)
  strict private
    FVerticalGroupByDefs : TmGroupByDefs;
    FHorizontalGroupByDefs : TmGroupByDefs;
    FSummaryDefinitions : TmSummaryDefinitions;
    FFieldDefs: TmVirtualFieldDefs;

    FRootPanel : TOMultiPanel;
    FLeftPanel, FRightPanel : TOMultiPanel;
    FFieldsPanel, FFilteredFieldsPanel, FHorizontalFieldsPanel, FVerticalFieldsPanel, FDataFieldsPanel, FPropertiesPanel: TPanel;
    FGroupDefPropertiesPanel, FSummaryPropertiesPanel : TPanel;
    FGroupDefPropertiesOperatorLabel : TLabel;
    FGroupDefPropertiesOperatorCB : TComboBox;

    FListBoxFields, FListBoxHorizontalFields, FListBoxVerticalFields, FListBoxDataFields : TListBox;
    FFieldsFindBtn : TEditButton;
    FLabelFields, FLabelHorizontalFields, FLabelVerticalFields, FLabelDataFields : TLabel;

    FHiddenColFindBtn, FVisibleColFindBtn : TEditButton;
    FLabelHidden, FLabelVisible, FPropertiesLabel: TLabel;
    FListBoxHiddenColumns, FListBoxVisibleColumns : TListBox;
    FLELabel, FLEFormat: TLabeledEdit;
    FVisibleColsMenu : TPopupMenu;

    FCurrentField : TmVirtualFieldDef;
    FCurrentGroupByDef : TmGroupByDef;
    FGarbageGroupByDefs : TmGroupByDefs;
    FGarbage : TObjectList;
    procedure ClearProperties;

    procedure CreateFieldsPanel;
    procedure CreatePropertiesPanel;
    procedure UpdatePropertiesPanel;
    procedure CreateFilteredFieldsPanel;
    procedure CreateHorizontalFieldsPanel;
    procedure CreateVerticalFieldsPanel;
    procedure CreateDataFieldsPanel;
    procedure LBDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);

    procedure OnFindField(Sender: TObject);

    procedure LBFieldsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LBFieldsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure LBFieldsSelectionChange(Sender: TObject; User: boolean);

    procedure LBHorizontalFieldsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LBHorizontalFieldsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure LBHorizontalFieldsSelectionChange(Sender: TObject; User: boolean);

    procedure LBVerticalFieldsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LBVerticalFieldsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure LBVerticalFieldsSelectionChange(Sender: TObject; User: boolean);

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
    function GetLBFieldLine (const aFieldDef : TmVirtualFieldDef) : String;
    function GetLBDataFieldLine (const aSummaryDef : TmSummaryDefinition) : String;
    function GetLBVertHorizFieldLine(const aGroupByDef : TmGroupByDef) : String;

    procedure ImportGroupByFromField (const aSource : TmVirtualFieldDef; aDestination : TmGroupByDef);
    procedure OnGroupDefPropertiesOperatorCBChange (aSender : TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init (aPivoter : TmPivoter);
    procedure UpdateSettings;

    property VerticalGroupByDefs : TmGroupByDefs read FVerticalGroupByDefs;
    property HorizontalGroupByDefs : TmGroupByDefs read FHorizontalGroupByDefs;
    property SummaryDefinitions : TmSummaryDefinitions read FSummaryDefinitions;
  end;

implementation

uses
  Graphics, LCLType,
  mUtility, mDatasetStandardSetup;

{$R *.lfm}

type

  { TGroupByOperationKindShell }

  TGroupByOperationKindShell = class
  public
    op : TmGroupByOperationKind;
    constructor Create(const aOperation : TmGroupByOperationKind);
  end;

{ TGroupByOperationKindShell }

constructor TGroupByOperationKindShell.Create(const aOperation: TmGroupByOperationKind);
begin
  op := aOperation;
end;


procedure TPivotFieldsSettingsFrame.LBVisibleColumnsSelectionChange(Sender: TObject; User: boolean);
begin
  ClearProperties;
  if (FListBoxVisibleColumns.SelCount = 1) and (FListBoxVisibleColumns.ItemIndex >= 0) then
  begin
(*    FCurrentProperties:= FListBoxVisibleColumns.Items.Objects[FListBoxVisibleColumns.ItemIndex] as TmGridColumnSettings;
    FLELabel.Text:= FCurrentProperties.DisplayLabel.AsString;
    FLEFormat.Text:= FCurrentProperties.DisplayFormat.AsString;
    *)
  end;
end;

procedure TPivotFieldsSettingsFrame.LBDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  aColor: TColor;
  tmpListBox : TListBox;
//  tmpProperties : TmGridColumnSettings;
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
    if tmpListBox.Items.Objects[index] is TmVirtualFieldDef then
      tmpListBox.Canvas.TextRect(ARect, 2, ARect.Top + 2, GetLBFieldLine(tmpListBox.Items.Objects[index] as TmVirtualFieldDef))
    else if tmpListBox.Items.Objects[index] is TmSummaryDefinition then
      tmpListBox.Canvas.TextRect(ARect, 2, ARect.Top + 2, GetLBDataFieldLine(tmpListBox.Items.Objects[index] as TmSummaryDefinition))
    else if tmpListBox.Items.Objects[index] is TmGroupByDef then
      tmpListBox.Canvas.TextRect(ARect, 2, ARect.Top + 2, GetLBVertHorizFieldLine(tmpListBox.Items.Objects[index] as TmGroupByDef));
  end
  else
    tmpListBox.Canvas.TextRect(ARect, 2, ARect.Top+2, tmpListBox.Items[Index]);
end;

procedure TPivotFieldsSettingsFrame.OnFindField(Sender: TObject);
var
  start, i: integer;
begin
  if FFieldsFindBtn.Text = '' then
    exit;

  if (FListBoxFields.ItemIndex >= 0) then
    start := FListBoxFields.ItemIndex
  else
    start := -1;
  for i := start + 1 to FListBoxFields.Count - 1 do
  begin
    if Pos(Uppercase(FFieldsFindBtn.Text), Uppercase(FListBoxFields.Items[i])) > 0 then
    begin
      FListBoxFields.ItemIndex:= i;
      exit;
    end;
  end;
  for i := 0 to start do
  begin
    if Pos(Uppercase(FFieldsFindBtn.Text), Uppercase(FListBoxFields.Items[i])) > 0 then
    begin
      FListBoxFields.ItemIndex:= i;
      exit;
    end;
  end;

end;

procedure TPivotFieldsSettingsFrame.LBFieldsDragDrop(Sender, Source: TObject; X, Y: Integer);
begin

end;

procedure TPivotFieldsSettingsFrame.LBFieldsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin

end;

procedure TPivotFieldsSettingsFrame.LBFieldsSelectionChange(Sender: TObject; User: boolean);
begin
  ClearProperties;
  if (FListBoxFields.SelCount = 1) and (FListBoxFields.ItemIndex >= 0) then
  begin
    FCurrentField := FListBoxFields.Items.Objects[FListBoxFields.ItemIndex] as TmVirtualFieldDef;
  end;
end;

procedure TPivotFieldsSettingsFrame.LBHorizontalFieldsDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  dest, prev : integer;
  old: String;
  oldObj : TObject;
  pt : TPoint;
  newGroupBy : TmGroupByDef;
begin
  if Source = FListBoxHorizontalFields then
  begin
    prev := FListBoxHorizontalFields.ItemIndex;
    if prev < 0 then
      exit;
    pt.x:= X;
    pt.y:= Y;
    dest := FListBoxHorizontalFields.ItemAtPos(pt, true);
    old := FListBoxHorizontalFields.Items[prev];
    oldObj := FListBoxHorizontalFields.Items.Objects[prev];

    if dest >= 0 then
    begin
      FListBoxHorizontalFields.DeleteSelected;
      if dest < prev then
        FListBoxHorizontalFields.Items.InsertObject(dest, old, oldObj)
      else
        FListBoxHorizontalFields.Items.InsertObject(dest - 1, old, oldObj);
    end;
  end
  else if Source = FListBoxFields then
  begin
    prev := FListBoxFields.ItemIndex;
    if prev < 0 then
      exit;
    pt.x:= X;
    pt.y:= Y;
    dest := FListBoxHorizontalFields.ItemAtPos(pt, true);
    old := FListBoxFields.Items[prev];
    oldObj := FListBoxFields.Items.Objects[prev];

    newGroupBy := FGarbageGroupByDefs.Add;
    ImportGroupByFromField((oldObj as TmVirtualFieldDef), newGroupBy);

    if dest >= 0 then
      FListBoxHorizontalFields.Items.InsertObject(dest, old, newGroupBy)
    else
      FListBoxHorizontalFields.AddItem(old, newGroupBy);
  end;
  LBHorizontalFieldsSelectionChange(nil, true);
end;

procedure TPivotFieldsSettingsFrame.LBHorizontalFieldsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = FListBoxFields) or (Source = FListBoxHorizontalFields);
end;

procedure TPivotFieldsSettingsFrame.LBHorizontalFieldsSelectionChange(Sender: TObject; User: boolean);
begin
  ClearProperties;
  if (FListBoxHorizontalFields.SelCount = 1) and (FListBoxHorizontalFields.ItemIndex >= 0) then
  begin
    FCurrentGroupByDef:= FListBoxHorizontalFields.Items.Objects[FListBoxHorizontalFields.ItemIndex] as TmGroupByDef;
    FSummaryPropertiesPanel.Visible := false;
    FGroupDefPropertiesPanel.Visible:= true;
    UpdatePropertiesPanel;
  end;
end;

procedure TPivotFieldsSettingsFrame.LBVerticalFieldsDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  dest, prev : integer;
  old: String;
  oldObj : TObject;
  pt : TPoint;
  newGroupBy : TmGroupByDef;
begin
  if Source = FListBoxVerticalFields then
  begin
    prev := FListBoxVerticalFields.ItemIndex;
    if prev < 0 then
      exit;
    pt.x:= X;
    pt.y:= Y;
    dest := FListBoxVerticalFields.ItemAtPos(pt, true);
    old := FListBoxVerticalFields.Items[prev];
    oldObj := FListBoxVerticalFields.Items.Objects[prev];

    if dest >= 0 then
    begin
      FListBoxVerticalFields.DeleteSelected;
      if dest < prev then
        FListBoxVerticalFields.Items.InsertObject(dest, old, oldObj)
      else
        FListBoxVerticalFields.Items.InsertObject(dest - 1, old, oldObj);
    end;
  end
  else if Source = FListBoxFields then
  begin
    prev := FListBoxFields.ItemIndex;
    if prev < 0 then
      exit;
    pt.x:= X;
    pt.y:= Y;
    dest := FListBoxVerticalFields.ItemAtPos(pt, true);
    old := FListBoxFields.Items[prev];
    oldObj := FListBoxFields.Items.Objects[prev];

    newGroupBy := FGarbageGroupByDefs.Add;
    ImportGroupByFromField((oldObj as TmVirtualFieldDef), newGroupBy);

    if dest >= 0 then
      FListBoxVerticalFields.Items.InsertObject(dest, old, newGroupBy)
    else
      FListBoxVerticalFields.AddItem(old, newGroupBy);
  end;
  LBVerticalFieldsSelectionChange(nil, true);
end;

procedure TPivotFieldsSettingsFrame.LBVerticalFieldsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = FListBoxFields) or (Source = FListBoxVerticalFields);
end;

procedure TPivotFieldsSettingsFrame.LBVerticalFieldsSelectionChange(Sender: TObject; User: boolean);
begin
  ClearProperties;
  if (FListBoxVerticalFields.SelCount = 1) and (FListBoxVerticalFields.ItemIndex >= 0) then
  begin
    FCurrentGroupByDef:= FListBoxVerticalFields.Items.Objects[FListBoxVerticalFields.ItemIndex] as TmGroupByDef;
    FSummaryPropertiesPanel.Visible := false;
    FGroupDefPropertiesPanel.Visible:= true;
    UpdatePropertiesPanel;
  end;
end;


procedure TPivotFieldsSettingsFrame.LBVisibleColumnsDragDrop(Sender,Source: TObject; X, Y: Integer);
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

procedure TPivotFieldsSettingsFrame.LBVisibleColumnsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = FListBoxVisibleColumns) or (Source = FListBoxHiddenColumns);
end;

procedure TPivotFieldsSettingsFrame.LBVisibleColumnsStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  //
end;

procedure TPivotFieldsSettingsFrame.LBHiddenColumnsDragDrop(Sender, Source: TObject; X, Y: Integer);
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

procedure TPivotFieldsSettingsFrame.LBHiddenColumnsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = FListBoxVisibleColumns);
end;

procedure TPivotFieldsSettingsFrame.LBHiddenColumnsStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  //
end;

procedure TPivotFieldsSettingsFrame.LEFormatEditingDone(Sender: TObject);
begin
  (*
  if Assigned(FCurrentProperties) then
  begin
    if Trim(FLEFormat.Text) <> '' then
      FCurrentProperties.DisplayFormat.Value:= Trim(FLEFormat.Text)
    else
      FCurrentProperties.DisplayFormat.IsNull:= true;
  end;
  *)
end;

procedure TPivotFieldsSettingsFrame.LELabelEditingDone(Sender: TObject);
begin
  (*
  if Assigned(FCurrentProperties) then
  begin
    if Trim(FLELabel.Text) <> '' then
      FCurrentProperties.DisplayLabel.Value:= Trim(FLELabel.Text)
    else
      FCurrentProperties.DisplayLabel.IsNull:= true;
    FListBoxVisibleColumns.Invalidate;
  end;
  *)
end;

procedure TPivotFieldsSettingsFrame.OnFindHiddenCol(Sender: TObject);
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

procedure TPivotFieldsSettingsFrame.OnFindVisibleCol(Sender: TObject);
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

procedure TPivotFieldsSettingsFrame.OnHideAllFields(Sender: TObject);
var
  i : integer;
//  op : TmGridColumnSettings;
begin
  (*
  for i := FListBoxVisibleColumns.Count - 1 downto 0 do
  begin
    op := FListBoxVisibleColumns.Items.Objects[i] as TmGridColumnSettings;
    FListBoxHiddenColumns.Items.InsertObject(0, FListBoxVisibleColumns.Items[i], op);
  end;
  FListBoxVisibleColumns.Clear;
  *)
end;

function TPivotFieldsSettingsFrame.GetLBFieldLine(const aFieldDef: TmVirtualFieldDef): String;
begin
  Result := GenerateDisplayLabel(aFieldDef.Name) + ' [' + aFieldDef.Name + ']';
end;

function TPivotFieldsSettingsFrame.GetLBDataFieldLine(const aSummaryDef: TmSummaryDefinition): String;
begin
  Result := aSummaryDef.Caption + ' [' + aSummaryDef.FieldName + '] - ' + TmSummaryOperatorToString(aSummaryDef.SummaryOperator);
end;

function TPivotFieldsSettingsFrame.GetLBVertHorizFieldLine(const aGroupByDef: TmGroupByDef): String;
begin
  Result := aGroupByDef.FieldName + ' - ' + TmGroupByOperationKindToString(aGroupByDef.OperationKind);
end;

procedure TPivotFieldsSettingsFrame.ImportGroupByFromField(const aSource: TmVirtualFieldDef; aDestination: TmGroupByDef);
begin
  aDestination.FieldName:= aSource.Name;
  aDestination.DataType:= FromTmVirtualFieldDefTypeToTFieldType(aSource.DataType);
  aDestination.OperationKind:= gpoDistinct;
end;

procedure TPivotFieldsSettingsFrame.OnGroupDefPropertiesOperatorCBChange(aSender: TObject);
begin
  if Assigned(FCurrentGroupByDef) then
    FCurrentGroupByDef.OperationKind:= TmGroupByOperationKind(FGroupDefPropertiesOperatorCB.ItemIndex);
end;


(*
procedure TPivotFieldsSettingsFrame.CreateHiddenColsPanel;
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
*)

(*
procedure TPivotFieldsSettingsFrame.CreateVisibleColsPanel;
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

*)

procedure TPivotFieldsSettingsFrame.CreatePropertiesPanel;
var
  ok : TmGroupByOperationKind;
  tmpOp : TGroupByOperationKindShell;
begin
  FPropertiesPanel.BevelOuter := bvNone;

  FGroupDefPropertiesPanel := TPanel.Create(FPropertiesPanel);
  FGroupDefPropertiesPanel.Parent := FPropertiesPanel;
  FGroupDefPropertiesPanel.BevelOuter:= bvNone;
  FGroupDefPropertiesPanel.Align:= alClient;
  FGroupDefPropertiesPanel.Visible := false;

  FGroupDefPropertiesOperatorCB := TComboBox.Create(FGroupDefPropertiesPanel);
  FGroupDefPropertiesOperatorCB.Parent := FGroupDefPropertiesPanel;
  FGroupDefPropertiesOperatorCB.Align:= alTop;
  FGroupDefPropertiesOperatorCB.Style:= csDropDownList;
  FGroupDefPropertiesOperatorCB.OnChange:= @OnGroupDefPropertiesOperatorCBChange;
  for ok := Low(TmGroupByOperationKind) to High(TmGroupByOperationKind) do
  begin
    tmpOp := TGroupByOperationKindShell.Create(ok);
    FGarbage.Add(tmpOp);
    FGroupDefPropertiesOperatorCB.AddItem(TmGroupByOperationKindToString(ok), tmpOp);
  end;
  FGroupDefPropertiesOperatorLabel := TLabel.Create(FGroupDefPropertiesPanel);
  FGroupDefPropertiesOperatorLabel.Parent := FGroupDefPropertiesPanel;
  FGroupDefPropertiesOperatorLabel.Align:= alTop;
  FGroupDefPropertiesOperatorLabel.Caption:= SLabelGroupByDefOperator;

  FSummaryPropertiesPanel := TPanel.Create(FPropertiesPanel);
  FSummaryPropertiesPanel.Parent := FPropertiesPanel;
  FSummaryPropertiesPanel.BevelOuter:= bvNone;
  FSummaryPropertiesPanel.Align:= alClient;
  FSummaryPropertiesPanel.Visible := false;

  (*
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
  *)
end;

procedure TPivotFieldsSettingsFrame.UpdatePropertiesPanel;
begin
  if FGroupDefPropertiesPanel.Visible then
  begin
    FGroupDefPropertiesOperatorCB.ItemIndex:= 0;
    if Assigned(FCurrentGroupByDef) then
      FGroupDefPropertiesOperatorCB.ItemIndex := integer(FCurrentGroupByDef.OperationKind);
  end
  else if FSummaryPropertiesPanel.Visible then
  begin

  end;
end;

procedure TPivotFieldsSettingsFrame.ClearProperties;
begin
  FGroupDefPropertiesPanel.Visible:= false;
  FSummaryPropertiesPanel.Visible := false;
end;

procedure TPivotFieldsSettingsFrame.CreateFieldsPanel;
var
  tmpMenuItem : TMenuItem;
begin
  FFieldsPanel.BevelOuter := bvNone;

  FFieldsFindBtn:= TEditButton.Create(FFieldsPanel);
  FFieldsFindBtn.Parent:= FFieldsPanel;
  FFieldsFindBtn.ButtonWidth:= 50;
  FFieldsFindBtn.Align:= alTop;
  FFieldsFindBtn.OnButtonClick:= @OnFindField;
  FFieldsFindBtn.ButtonCaption:= SButtonFind;

  FLabelFields:= TLabel.Create(FFieldsPanel);
  FLabelFields.Parent:= FFieldsPanel;
  FLabelFields.Alignment:= taCenter;
  FLabelFields.Align:= alTop;
  FLabelFields.Caption:= SLabelFields;

  FListBoxFields:= TListBox.Create(FFieldsPanel);
  FListBoxFields.Parent:= FFieldsPanel;
  FListBoxFields.Align:= alClient;
  FListBoxFields.DragMode:= dmAutomatic;
  FListBoxFields.OnDragDrop:= @LBFieldsDragDrop;
  FListBoxFields.OnDragOver:= @LBFieldsDragOver;
  FListBoxFields.OnSelectionChange:= @LBFieldsSelectionChange;
  FListBoxFields.OnDrawItem:= @Self.LBDrawItem;
  FListBoxFields.Style:= lbOwnerDrawFixed;
  FListBoxFields.ItemHeight:= 20;

  (*
  FVisibleColsMenu := TPopupMenu.Create(FListBoxVisibleColumns);
  FListBoxVisibleColumns.PopupMenu := FVisibleColsMenu;
  tmpMenuItem := TMenuItem.Create(FVisibleColsMenu);
  FVisibleColsMenu.Items.Add(tmpMenuItem);
  tmpMenuItem.Caption:= SHideAllCommand;
  tmpMenuItem.OnClick:= @OnHideAllFields;
  *)
end;

procedure TPivotFieldsSettingsFrame.CreateFilteredFieldsPanel;
begin
  FFilteredFieldsPanel.BevelOuter := bvNone;
end;

procedure TPivotFieldsSettingsFrame.CreateHorizontalFieldsPanel;
begin
  FHorizontalFieldsPanel.BevelOuter := bvNone;
  FLabelHorizontalFields:= TLabel.Create(FFieldsPanel);
  FLabelHorizontalFields.Parent:= FHorizontalFieldsPanel;
  FLabelHorizontalFields.Alignment:= taCenter;
  FLabelHorizontalFields.Align:= alTop;
  FLabelHorizontalFields.Caption:= SLabelHorizontalFields;

  FListBoxHorizontalFields:= TListBox.Create(FHorizontalFieldsPanel);
  FListBoxHorizontalFields.Parent:= FHorizontalFieldsPanel;
  FListBoxHorizontalFields.Align:= alClient;
  FListBoxHorizontalFields.DragMode:= dmAutomatic;
  FListBoxHorizontalFields.OnDragDrop:= @LBHorizontalFieldsDragDrop;
  FListBoxHorizontalFields.OnDragOver:= @LBHorizontalFieldsDragOver;
  FListBoxHorizontalFields.OnSelectionChange:= @LBHorizontalFieldsSelectionChange;
//  FListBoxHorizontalFields.OnStartDrag:= @LBFieldsStartDrag;
  FListBoxHorizontalFields.OnDrawItem:= @Self.LBDrawItem;
  FListBoxHorizontalFields.Style:= lbOwnerDrawFixed;
  FListBoxHorizontalFields.ItemHeight:= 20;
end;

procedure TPivotFieldsSettingsFrame.CreateVerticalFieldsPanel;
begin
  FVerticalFieldsPanel.BevelOuter := bvNone;
  FLabelVerticalFields:= TLabel.Create(FFieldsPanel);
  FLabelVerticalFields.Parent:= FVerticalFieldsPanel;
  FLabelVerticalFields.Alignment:= taCenter;
  FLabelVerticalFields.Align:= alTop;
  FLabelVerticalFields.Caption:= SLabelVerticalFields;

  FListBoxVerticalFields:= TListBox.Create(FVerticalFieldsPanel);
  FListBoxVerticalFields.Parent:= FVerticalFieldsPanel;
  FListBoxVerticalFields.Align:= alClient;
  FListBoxVerticalFields.DragMode:= dmAutomatic;
//  FListBoxVerticalFields.OnDragDrop:= @LBFieldsDragDrop;
//  FListBoxVerticalFields.OnDragOver:= @LBFieldsDragOver;
//  FListBoxVerticalFields.OnSelectionChange:= @LBFieldsSelectionChange;
//  FListBoxVerticalFields.OnStartDrag:= @LBFieldsStartDrag;
  FListBoxVerticalFields.OnDrawItem:= @Self.LBDrawItem;
  FListBoxVerticalFields.Style:= lbOwnerDrawFixed;
  FListBoxVerticalFields.ItemHeight:= 20;
end;

procedure TPivotFieldsSettingsFrame.CreateDataFieldsPanel;
begin
  FDataFieldsPanel.BevelOuter := bvNone;
  FLabelDataFields:= TLabel.Create(FFieldsPanel);
  FLabelDataFields.Parent:= FDataFieldsPanel;
  FLabelDataFields.Alignment:= taCenter;
  FLabelDataFields.Align:= alTop;
  FLabelDataFields.Caption:= SLabelDataFields;

  FListBoxDataFields:= TListBox.Create(FDataFieldsPanel);
  FListBoxDataFields.Parent:= FDataFieldsPanel;
  FListBoxDataFields.Align:= alClient;
  FListBoxDataFields.DragMode:= dmAutomatic;
//  FListBoxDataFields.OnDragDrop:= @LBFieldsDragDrop;
//  FListBoxDataFields.OnDragOver:= @LBFieldsDragOver;
//  FListBoxDataFields.OnSelectionChange:= @LBFieldsSelectionChange;
//  FListBoxDataFields.OnStartDrag:= @LBFieldsStartDrag;
//  FListBoxDataFields.OnDrawItem:= @Self.LBDrawItem;
  FListBoxDataFields.Style:= lbOwnerDrawFixed;
  FListBoxDataFields.ItemHeight:= 20;
end;

constructor TPivotFieldsSettingsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FVerticalGroupByDefs := TmGroupByDefs.Create;
  FHorizontalGroupByDefs := TmGroupByDefs.Create;
  FSummaryDefinitions := TmSummaryDefinitions.Create;
  FFieldDefs := TmVirtualFieldDefs.Create;

  FGarbageGroupByDefs := TmGroupByDefs.Create;
  FGarbage := TObjectList.Create(true);


  FRootPanel := TOMultiPanel.Create(Self);
  FRootPanel.Parent := Self;
  FRootPanel.PanelType:= ptHorizontal;
  FRootPanel.Align:= alClient;

  FFieldsPanel := TPanel.Create(FRootPanel);
  FFieldsPanel.Parent := FRootPanel;
  FRootPanel.PanelCollection.AddControl(FFieldsPanel);

  FLeftPanel := TOMultiPanel.Create(FRootPanel);
  FLeftPanel.Parent := FRootPanel;
  FLeftPanel.PanelType:= ptVertical;
  FRootPanel.PanelCollection.AddControl(FLeftPanel);

  FRightPanel := TOMultiPanel.Create(FRootPanel);
  FRightPanel.Parent := FRootPanel;
  FRightPanel.PanelType:= ptVertical;
  FRootPanel.PanelCollection.AddControl(FRightPanel);

  FPropertiesPanel := TPanel.Create(FRootPanel);
  FPropertiesPanel.Parent := FRootPanel;
  FRootPanel.PanelCollection.AddControl(FPropertiesPanel);


  FFilteredFieldsPanel := TPanel.Create(FLeftPanel);
  FFilteredFieldsPanel.Parent := FLeftPanel;
  FLeftPanel.PanelCollection.AddControl(FFilteredFieldsPanel);

  FHorizontalFieldsPanel := TPanel.Create(FLeftPanel);
  FHorizontalFieldsPanel.Parent := FLeftPanel;
  FLeftPanel.PanelCollection.AddControl(FHorizontalFieldsPanel);


  FVerticalFieldsPanel := TPanel.Create(FLeftPanel);
  FVerticalFieldsPanel.Parent := FRightPanel;
  FRightPanel.PanelCollection.AddControl(FVerticalFieldsPanel);

  FDataFieldsPanel := TPanel.Create(FLeftPanel);
  FDataFieldsPanel.Parent := FRightPanel;
  FRightPanel.PanelCollection.AddControl(FDataFieldsPanel);

  CreateFieldsPanel;
  CreatePropertiesPanel;
  CreateFilteredFieldsPanel;
  CreateHorizontalFieldsPanel;
  CreateVerticalFieldsPanel;
  CreateDataFieldsPanel;


  FLeftPanel.PanelCollection.Items[1].Position:= 1;
  FLeftPanel.PanelCollection.Items[0].Position:= 0.5;

  FRightPanel.PanelCollection.Items[1].Position:= 1;
  FRightPanel.PanelCollection.Items[0].Position:= 0.5;

  FRootPanel.PanelCollection.Items[3].Position:= 1;
  FRootPanel.PanelCollection.Items[2].Position:= 0.75;
  FRootPanel.PanelCollection.Items[1].Position:= 0.50;
  FRootPanel.PanelCollection.Items[0].Position:= 0.25;
end;

destructor TPivotFieldsSettingsFrame.Destroy;
begin
  FVerticalGroupByDefs.Free;
  FHorizontalGroupByDefs.Free;
  FSummaryDefinitions.Free;
  FFieldDefs.Free;
  FGarbage.Free;
  FGarbageGroupByDefs.Free;

  inherited Destroy;
end;

procedure TPivotFieldsSettingsFrame.Init(aPivoter : TmPivoter);
var
  i : integer;
begin
  FVerticalGroupByDefs.Assign(aPivoter.VerticalGroupByDefs);
  FHorizontalGroupByDefs.Assign(aPivoter.HorizontalGroupByDefs);
  FSummaryDefinitions.Assign(aPivoter.SummaryDefinitions);

  aPivoter.DataProvider.FillVirtualFieldDefs(FFieldDefs, '');

  for i := 0 to FFieldDefs.Count - 1 do
    FListBoxFields.AddItem(GetLBFieldLine(FFieldDefs.VirtualFieldDefs[i]), FFieldDefs.VirtualFieldDefs[i]);

  for i := 0 to FHorizontalGroupByDefs.Count - 1 do
    FListBoxHorizontalFields.AddItem(FHorizontalGroupByDefs.Get(i).FieldName, FHorizontalGroupByDefs.Get(i));

  for i := 0 to FVerticalGroupByDefs.Count - 1 do
    FListBoxVerticalFields.AddItem(FVerticalGroupByDefs.Get(i).FieldName, FVerticalGroupByDefs.Get(i));

  for i := 0 to FSummaryDefinitions.Count - 1 do
    FListBoxDataFields.AddItem(FSummaryDefinitions.Get(i).FieldName, FSummaryDefinitions.Get(i));

end;

procedure TPivotFieldsSettingsFrame.UpdateSettings;
begin
  (*
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
  *)
end;

end.

