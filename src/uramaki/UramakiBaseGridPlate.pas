// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit UramakiBaseGridPlate;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, Controls, ExtCtrls, DB, ComCtrls, DBGrids,
  Forms, Menus, SysUtils, StdCtrls, contnrs,
  UramakiToolbar,
  {$IFDEF FPC}
  LCLIntf,
  LclType,
  LclProc,
  LResources,
  LMessages,
  {$ENDIF}

  UramakiBase,
  mVirtualDataSet, mFilterPanel, mFilter, mGridHelper, mDBGrid,
  mQuickReadOnlyVirtualDataSet, mXML, mVirtualDataSetInterfaces, mSummary;

resourcestring
  SConfigureChildsUpdateModeCaption = 'Update of child widgets';
  SEnableAutomaticChildsUpdateCaption = 'Refresh them automatically';
  SDisableAutomaticChildsUpdateCaption = 'Do not refresh them automatically';
  SUpdateChildWidgetsBtnHint = 'Click to update child widgets';
  SSelectAllMenuCaption = 'Select all rows';
  SGridActionsHint = 'Grid actions...';
  SCopySummaryToClipboard = 'Copy to clipboard';

const
  WM_USER_REFRESHCHILDS = WM_USER + 1;
  WM_USER_CLEARCHILDS = WM_USER + 2;

type

  TUramakiDBGridHelper = class;

  TDoFillRollFromDatasetRow = procedure (aUrakamiRoll : TUramakiRoll; const aDataset : TDataset) of object;

  TUramakiGridChildsAutomaticUpdateMode = (cuOnChangeSelection, cuDisabled);

  { TUramakiGridSummaryPanel }

  TUramakiGridSummaryPanel = class (ISummaryPanel)
  strict private
    const DEFAULT_HEIGHT = 25;
  strict private
    FPanel : TFlowPanel;
    FSubPanels : TList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LinkToPlate (aPlate : TUramakiPlate);

    procedure Hide;
    procedure Show;
    procedure SetSummaryValues (aScreenValues: TmSummaryScreenValues);
  end;

  { TUramakiBaseGridPlate }

  TUramakiBaseGridPlate = class abstract (TUramakiPlate)
  strict private
    FAutomaticChildsUpdateMode : TUramakiGridChildsAutomaticUpdateMode;
    procedure SetAutomaticChildsUpdateMode(AValue: TUramakiGridChildsAutomaticUpdateMode);
    procedure DoUpdateChilds (Sender : TObject);
    procedure DoSelectAll (Sender : TObject);
  protected
    FGrid: TmDBGrid;
    FSummaryPanel : TUramakiGridSummaryPanel;
    FDataset: TmVirtualDataset;
    FProvider : TReadOnlyVirtualDatasetProvider;
    FGridHelper : TUramakiDBGridHelper;
    FDatasource : TDatasource;
    //FToolbar : TToolBar;
    FToolbar : TUramakiToolbar;
    FLastSelectedRow : integer;
    FFilterPanel : TmFilterPanel;
    FGridCommandsPopupMenu : TPopupMenu;

    // override these:
    function GetDataProvider : IVDDataProvider; virtual; abstract;
    procedure ReloadData (aFilters : TmFilters); virtual; abstract;

    procedure CreateToolbar(aImageList : TImageList; aConfigureImageIndex, aRefreshChildsImageIndex, aGridCommandsImageIndex : integer);
    procedure ConvertSelectionToUramakiRoll (aUramakiRoll : TUramakiRoll; aDoFillRollFromDatasetRow : TDoFillRollFromDatasetRow);
    procedure ProcessRefreshChilds(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF}); message WM_USER_REFRESHCHILDS;
    procedure ProcessClearChilds(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF}); message WM_USER_CLEARCHILDS;
    procedure OnSelectEditor(Sender: TObject; Column: TColumn; var Editor: TWinControl);
    procedure InvokeChildsRefresh;
    procedure InvokeChildsClear;
    procedure GetSelectedItems (const aKeyFieldName : string; aList : TList);

    procedure OnClearFilter (Sender : TObject);
    procedure OnExecuteFilter (Sender : TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure DisableControls;
    procedure EnableControls;
    procedure RefreshDataset;

    procedure LoadConfigurationFromXML (aXMLElement : TmXmlElement); override;
    procedure SaveConfigurationToXML (aXMLElement : TmXmlElement); override;
    procedure Clear; override;

    property AutomaticChildsUpdateMode : TUramakiGridChildsAutomaticUpdateMode read FAutomaticChildsUpdateMode write SetAutomaticChildsUpdateMode;
  end;

  { TUramakiDBGridHelper }

  TUramakiDBGridHelper = class(TmDBGridHelper)
  strict private
    FEnableAutomaticChildsUpdateMI : TMenuItem;
    FDisableAutomaticChildsUpdateMI : TMenuItem;
    FConfigurePopupMenu : TPopupMenu;

    FGridPlate : TUramakiBaseGridPlate;
    procedure OnEnableAutomaticChildsUpdate(Sender : TObject);
    procedure OnDisableAutomaticChildsUpdate(Sender : TObject);
  public
    constructor Create(aPlate : TUramakiBaseGridPlate); reintroduce;

    //procedure CreateStandardConfigureMenu(aToolbar : TToolbar; const aConfigureImageIndex : integer); override;
    procedure CreateConfigureMenu(aToolbar : TUramakiToolbar; const aConfigureImageIndex : integer);
  end;

implementation

uses
  Clipbrd, variants,
  mGraphicsUtility;

type

  { TSummaryLabel }

  TSummaryLabel = class(TLabel)
  strict private
    FRawValue: variant;
    FDataType: TmSummaryValueType;
    FPopupMenu : TPopupMenu;
    procedure OnCopySummaryToClipboard(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    property RawValue: variant read FRawValue write FRawValue;
    property DataType: TmSummaryValueType read FDataType write FDataType;
  end;

{ TSummaryLabel }

procedure TSummaryLabel.OnCopySummaryToClipboard(Sender: TObject);
begin
  if VarIsNull(FRawValue) then
    Clipboard.AsText:= ''
  else
  begin
    if FDataType = svtDate then
      Clipboard.AsText:= DateToStr(FRawValue)
    else if FDataType = svtDateTime then
      Clipboard.AsText:= DateTimeToStr(FRawValue)
    else
      Clipboard.AsText:= VarToStr(FRawValue);
  end;
end;

constructor TSummaryLabel.Create(TheOwner: TComponent);
var
  tmpItem: TMenuItem;
begin
  inherited Create(TheOwner);
  FPopupMenu := TPopupMenu.Create(Self);
  Self.PopupMenu:= FPopupMenu;
  tmpItem:= TMenuItem.Create(FPopupMenu);
  tmpItem.Caption:= SCopySummaryToClipboard;
  tmpItem.OnClick:= Self.OnCopySummaryToClipboard;
  FPopupMenu.Items.Add(tmpItem);
end;

{ TUramakiGridSummaryPanel }

constructor TUramakiGridSummaryPanel.Create;
begin
  FSubPanels := TList.Create;
end;

destructor TUramakiGridSummaryPanel.Destroy;
begin
  FSubPanels.Free;
end;

procedure TUramakiGridSummaryPanel.LinkToPlate(aPlate: TUramakiPlate);
begin
  if not Assigned(FPanel) then
  begin
    FPanel := TFlowPanel.Create(aPlate);
    FPanel.Parent := aPlate;
    FPanel.AutoWrap:= true;
    FPanel.AutoSize:= true;
    FPanel.Align:= alBottom;
  end;
end;

procedure TUramakiGridSummaryPanel.Hide;
begin
  if Assigned(FPanel) then
    FPanel.Visible:= false;
end;

procedure TUramakiGridSummaryPanel.Show;
begin
  if Assigned(FPanel) then
    FPanel.Visible:= true;
end;

procedure TUramakiGridSummaryPanel.SetSummaryValues(aScreenValues: TmSummaryScreenValues);
var
  i : integer;
  tmpPanel : TPanel;
  tmpLabel : TSummaryLabel;
begin
  for i := 0 to FSubPanels.Count - 1 do
  begin
    //FPanel.RemoveControl(TControl(FSubPanels.Items[i]));
    TPanel(FSubPanels.Items[i]).Parent := nil;
  end;
  for i := 0 to FSubPanels.Count - 1 do
  begin
    TPanel(FSubPanels.Items[i]).Free;
  end;
  FSubPanels.Clear;
  FPanel.ControlList.Clear;
  for i := 0 to aScreenValues.Count - 1 do
  begin
    tmpPanel := TPanel.Create(FPanel);
    tmpPanel.Parent := FPanel;
    tmpPanel.Align:= alLeft;
    tmpPanel.Height:= ScaleForDPI(DEFAULT_HEIGHT);
    tmpPanel.BorderSpacing.Left:= 2;
    tmpPanel.BorderSpacing.Right:= 2;
    tmpPanel.BorderSpacing.Top:= 2;
    tmpPanel.BorderSpacing.Bottom:= 2;
    tmpPanel.BevelInner:= bvNone;
    tmpPanel.BevelOuter:= bvNone;
    tmpPanel.BorderStyle:= bsSingle;
    tmpLabel := TSummaryLabel.Create(tmpPanel);
    tmpLabel.Parent := tmpPanel;
    tmpLabel.Align:= alClient;
    tmpLabel.Caption:= aScreenValues.Get(i).FormattedValue;
    tmpLabel.RawValue:= aScreenValues.Get(i).RawValue;
    tmpLabel.DataType:= aScreenValues.Get(i).DataType;
    tmpLabel.AutoSize:= true;
    tmpLabel.Font.Size:= 13;
    tmpPanel.AutoSize:= true;

    FSubPanels.Add(tmpPanel);
  end;
end;

{ TUramakiDBGridHelper }

constructor TUramakiDBGridHelper.Create(aPlate: TUramakiBaseGridPlate);
begin
  FGridPlate := aPlate;
  inherited Create(FGridPlate.FGrid, FGridPlate.FProvider.FormulaFields);
end;


//procedure TUramakiDBGridHelper.CreateStandardConfigureMenu(aToolbar: TToolbar; const aConfigureImageIndex: integer);
procedure TUramakiDBGridHelper.CreateConfigureMenu(aToolbar : TUramakiToolbar; const aConfigureImageIndex : integer);
var
  itm : TMenuItem;
begin
  FConfigurePopupMenu := TPopupMenu.Create(aToolbar);

  with aToolbar.AddDropDownButton(FConfigurePopupMenu) do
  begin
    Hint := SConfigureCommandHint;
    ImageIndex:= aConfigureImageIndex;
    Kind := bkIcon;
  end;
  itm := TMenuItem.Create(FConfigurePopupMenu);
  FConfigurePopupMenu.Items.Add(itm);
  itm.OnClick:= Self.OnEditSettings;
  itm.Hint:= SConfigureGridCommandHint;
  itm.Caption:= SConfigureGridCommandCaption;
  if Assigned(FFormulaFields) then
  begin
    itm := TMenuItem.Create(FConfigurePopupMenu);
    FConfigurePopupMenu.Items.Add(itm);
    itm.OnClick:= Self.OnEditFormulaFields;
    itm.Hint:= SConfigureFormulaFieldsCommandHint;
    itm.Caption:= SConfigureFormulaFieldsCommandCaption;
  end;

  itm := TMenuItem.Create(FConfigurePopupMenu);
  itm.Caption:= '-';
  FConfigurePopupMenu.Items.Add(itm);

  itm := TMenuItem.Create(FConfigurePopupMenu);
  FConfigurePopupMenu.Items.Add(itm);
  itm.OnClick:= Self.OnExportGridAsCsv;
  itm.Hint:= SExportGridAsCsvCommandHint;
  itm.Caption:= SExportGridAsCsvCommandCaption;

  itm := TMenuItem.Create(FConfigurePopupMenu);
  FConfigurePopupMenu.Items.Add(itm);
  itm.OnClick:= Self.OnExportGridAsXls;
  itm.Hint:= SExportGridAsXlsCommandHint;
  itm.Caption:= SExportGridAsXlsCommandCaption;

  itm := TMenuItem.Create(FConfigurePopupMenu);
  FConfigurePopupMenu.Items.Add(itm);
  itm.OnClick:= Self.OnExportGridAsHtml();
  itm.Hint:= SExportGridAsHtmlCommandHint;
  itm.Caption:= SExportGridAsHtmlCommandCaption;

  aToolbar.AddSeparator;
  aToolbar.Update;

  itm := TMenuItem.Create(FConfigurePopupMenu);
  itm.Caption:= '-';
  FConfigurePopupMenu.Items.Add(itm);

  itm := TMenuItem.Create(FConfigurePopupMenu);
  itm.Caption:= SConfigureChildsUpdateModeCaption;
  FConfigurePopupMenu.Items.Add(itm);

  FEnableAutomaticChildsUpdateMI := TMenuItem.Create(itm);
  itm.Add(FEnableAutomaticChildsUpdateMI);
  FEnableAutomaticChildsUpdateMI.Caption := SEnableAutomaticChildsUpdateCaption;
  FEnableAutomaticChildsUpdateMI.Checked:= true;
  FEnableAutomaticChildsUpdateMI.OnClick:= Self.OnEnableAutomaticChildsUpdate;

  FDisableAutomaticChildsUpdateMI := TMenuItem.Create(itm);
  itm.Add(FDisableAutomaticChildsUpdateMI);
  FDisableAutomaticChildsUpdateMI.Caption := SDisableAutomaticChildsUpdateCaption;
  FDisableAutomaticChildsUpdateMI.Checked:= false;
  FDisableAutomaticChildsUpdateMI.OnClick:= Self.OnDisableAutomaticChildsUpdate;
end;


procedure TUramakiDBGridHelper.OnEnableAutomaticChildsUpdate(Sender: TObject);
begin
  FEnableAutomaticChildsUpdateMI.Checked:= true;
  FDisableAutomaticChildsUpdateMI.Checked:=false;
  FGridPlate.AutomaticChildsUpdateMode:= cuOnChangeSelection;
end;

procedure TUramakiDBGridHelper.OnDisableAutomaticChildsUpdate(Sender: TObject);
begin
  FEnableAutomaticChildsUpdateMI.Checked:= false;
  FDisableAutomaticChildsUpdateMI.Checked:= true;
  FGridPlate.AutomaticChildsUpdateMode:= cuDisabled;
end;


{ TUramakiBaseGridPlate }

procedure TUramakiBaseGridPlate.SetAutomaticChildsUpdateMode(AValue: TUramakiGridChildsAutomaticUpdateMode);
begin
  FAutomaticChildsUpdateMode:=AValue;
  if FAutomaticChildsUpdateMode = cuOnChangeSelection then
    FGrid.OnSelectEditor:= Self.OnSelectEditor
  else
    FGrid.OnSelectEditor:= nil;
end;

procedure TUramakiBaseGridPlate.DoUpdateChilds(Sender : TObject);
begin
  if FAutomaticChildsUpdateMode = cuOnChangeSelection then
  begin
    if FLastSelectedRow <> FGrid.Row then
    begin
      FLastSelectedRow := FGrid.Row;
      if Assigned(Self.Parent) then
        InvokeChildsRefresh;
    end;
  end
  else
  begin
    FLastSelectedRow:= -1;
    if Assigned(Self.Parent) then
      InvokeChildsRefresh;
  end;
end;

procedure TUramakiBaseGridPlate.DoSelectAll(Sender: TObject);
begin
  FGridHelper.SelectAllRows;
end;

procedure TUramakiBaseGridPlate.CreateToolbar(aImageList : TImageList; aConfigureImageIndex, aRefreshChildsImageIndex, aGridCommandsImageIndex : integer);
var
  mItm : TMenuItem;
begin
  FToolbar := TUramakiToolbar.Create(Self);
  FToolbar.Images := aImageList;
  FToolbar.Parent := Self;
  FGridHelper.CreateConfigureMenu(FToolbar, aConfigureImageIndex);

  FGridCommandsPopupMenu := TPopupMenu.Create(FToolbar);

  with FToolbar.AddButton do
  begin
    ImageIndex:= aRefreshChildsImageIndex;
    OnClick:= DoUpdateChilds;
    Hint:= SUpdateChildWidgetsBtnHint;
    Kind := bkIcon;
  end;
  FToolbar.AddSeparator;
  with FToolbar.AddDropDownButton(FGridCommandsPopupMenu) do
  begin
    Hint:= SGridActionsHint;
    ImageIndex:=aGridCommandsImageIndex;
    Kind := bkIcon;
  end;
  FToolbar.AddSeparator;
  FToolbar.Update;

  mItm := TMenuItem.Create(FGridCommandsPopupMenu);
  mItm.OnClick:=Self.DoSelectAll;;
  mItm.Caption:= SSelectAllMenuCaption;
  FGridCommandsPopupMenu.Items.Add(mItm);
end;



(*
procedure TUramakiBaseGridPlate.CreateToolbar(aImageList : TImageList; aConfigureImageIndex, aRefreshChildsImageIndex, aGridCommandsImageIndex : integer);
var
  tmp : TToolButton;
  mItm : TMenuItem;
begin
  FToolbar := TToolBar.Create(Self);
  FToolbar.Parent := Self;
  FToolbar.Align:= alTop;
  FToolbar.Images := aImageList;
  FToolbar.ShowHint:= true;
  FGridHelper.CreateStandardConfigureMenu(FToolbar, aConfigureImageIndex);

  tmp := TToolButton.Create(FToolbar);
  tmp.Style := tbsButton;
  tmp.OnClick:= Self.DoUpdateChilds;
  tmp.ImageIndex:= aRefreshChildsImageIndex;
  tmp.Parent := FToolbar;
  tmp.Hint := SUpdateChildWidgetsBtnHint;
  tmp.Enabled:= true;

  tmp := TToolButton.Create(FToolbar);
  tmp.Style := tbsSeparator;
  tmp.Parent := FToolbar;

  FGridCommandsPopupMenu := TPopupMenu.Create(FToolbar);

  tmp := TToolButton.Create(FToolbar);
  tmp.Style:= tbsDropDown;
  tmp.DropdownMenu := FGridCommandsPopupMenu;
  tmp.ImageIndex := aGridCommandsImageIndex;
  tmp.Parent := FToolbar;
  tmp.Hint := SGridActionsHint;

  mItm := TMenuItem.Create(FGridCommandsPopupMenu);
  mItm.OnClick:=Self.DoSelectAll;;
  mItm.Caption:= SSelectAllMenuCaption;
  FGridCommandsPopupMenu.Items.Add(mItm);

  tmp := TToolButton.Create(FToolbar);
  tmp.Style := tbsSeparator;
  tmp.Parent := FToolbar;
end;*)

procedure TUramakiBaseGridPlate.ConvertSelectionToUramakiRoll(aUramakiRoll: TUramakiRoll; aDoFillRollFromDatasetRow : TDoFillRollFromDatasetRow);
var
  tmpBookmark : TBookMark;
  i : integer;
begin
  if FDataset.ControlsDisabled then
    exit;

  if (FGrid.SelectedRows.Count > 1) then
  begin
    FGrid.BeginUpdate;
    try
      tmpBookmark := FDataset.Bookmark;
      try
        for i := 0 to FGrid.SelectedRows.Count - 1 do
        begin
          FDataset.GotoBookmark(FGrid.SelectedRows[i]);
          aDoFillRollFromDatasetRow(aUramakiRoll, FDataset);
        end;
      finally
        FDataset.GotoBookmark(tmpBookmark);
      end;
    finally
      FGrid.EndUpdate(true);
    end;
  end
  else //if FGrid.SelectedRows.Count =  1 then
  begin
    aDoFillRollFromDatasetRow(aUramakiRoll, FDataset);
  end;
end;

procedure TUramakiBaseGridPlate.ProcessRefreshChilds(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
  FGrid.OnSelectEditor:= nil;
  try
    EngineMediator.PleaseRefreshMyChilds(Self);
  finally
    if AutomaticChildsUpdateMode = cuOnChangeSelection then
      FGrid.OnSelectEditor:= Self.OnSelectEditor;
  end;
end;

procedure TUramakiBaseGridPlate.ProcessClearChilds(var Message: TLMessage);
begin
  EngineMediator.PleaseClearMyChilds(Self);
end;

procedure TUramakiBaseGridPlate.OnSelectEditor(Sender: TObject; Column: TColumn; var Editor: TWinControl);
begin
  DoUpdateChilds(nil);
end;

procedure TUramakiBaseGridPlate.InvokeChildsRefresh;
begin
  PostMessage(Self.Handle, WM_USER_REFRESHCHILDS, 0, 0);
end;

procedure TUramakiBaseGridPlate.InvokeChildsClear;
begin
  PostMessage(Self.Handle, WM_USER_CLEARCHILDS, 0, 0);
end;

procedure TUramakiBaseGridPlate.GetSelectedItems(const aKeyFieldName: string; aList: TList);
var
  tmpBookmark : TBookmark;
  i : integer;
  tmpDatum : IVDDatum;
  tmpKey : string;
begin
  FGrid.BeginUpdate;
  try
    if FGrid.SelectedRows.Count = 1 then
    begin
      tmpKey := FDataset.FieldByName(aKeyFieldName).AsString;
      tmpDatum := GetDataProvider.FindDatumByStringKey(tmpKey);
      if Assigned(tmpDatum) then
        aList.Add(tmpDatum.AsObject);
    end
    else
    begin
      FGrid.OnSelectEditor:= nil;
      try
        tmpBookmark := FDataset.Bookmark;
        try
          for i := 0 to FGrid.SelectedRows.Count - 1 do
          begin
            FDataset.GotoBookmark(FGrid.SelectedRows[i]);
            tmpDatum := GetDataProvider.FindDatumByStringKey(FDataset.FieldByName(aKeyFieldName).AsString);
            if Assigned(tmpDatum) then
              aList.Add(tmpDatum.AsObject);
          end;
        finally
          FDataset.GotoBookmark(tmpBookmark);
        end;
      finally
        if FAutomaticChildsUpdateMode = cuOnChangeSelection then
          FGrid.OnSelectEditor:= Self.OnSelectEditor;
      end;
    end;
  finally
    FGrid.EndUpdate(true);
  end;
end;

procedure TUramakiBaseGridPlate.OnClearFilter(Sender: TObject);
begin
  if Assigned(FFilterPanel) then
  begin
    FFilterPanel.ClearAll();
    Self.Clear;
  end;
end;

procedure TUramakiBaseGridPlate.OnExecuteFilter(Sender: TObject);
var
  tmpFilters : TmFilters;
  oldCursor : TCursor;
begin
  Self.DisableControls;
  try
    OldCursor := Screen.Cursor;
    try
      Screen.Cursor := crHourGlass;

      tmpFilters := TmFilters.Create;
      try
        FFilterPanel.GetFilters(tmpFilters);
        Self.ReloadData(tmpFilters);
        RefreshDataset;
      finally
        tmpFilters.Free;
      end;
    finally
      Screen.Cursor := OldCursor;
    end;
  finally
    Self.EnableControls;
  end;
  FLastSelectedRow := -1;
  if AutomaticChildsUpdateMode = cuOnChangeSelection then
    InvokeChildsRefresh
  else
    InvokeChildsClear;
end;

procedure TUramakiBaseGridPlate.DisableControls;
begin
  FDataset.DisableControls;
end;

procedure TUramakiBaseGridPlate.EnableControls;
begin
  FDataset.EnableControls;
end;

procedure TUramakiBaseGridPlate.RefreshDataset;
begin
  FGrid.SelectedRows.Clear;
  FGrid.SelectedIndex:= -1;
  FLastSelectedRow := -1;
  FDataset.Refresh;
end;

constructor TUramakiBaseGridPlate.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FSummaryPanel := TUramakiGridSummaryPanel.Create;
  FSummaryPanel.LinkToPlate(Self);

  FGrid := TmDBGrid.Create(Self);
  FGrid.Parent := Self;
  FGrid.Align:= alClient;
  FDatasource:= TDataSource.Create(Self);
  FGrid.DataSource := FDataSource;
  FGrid.SummaryPanel := FSummaryPanel;

  FDataset := TmVirtualDataset.Create(nil);
  FProvider := TReadOnlyVirtualDatasetProvider.Create;
  FDataset.DatasetDataProvider := FProvider;
  FDatasource.DataSet := FDataset;

  FGridHelper:= TUramakiDBGridHelper.Create(Self);
  FGridHelper.SetupGrid;
  FGrid.SortManager := Self.FDataset.SortManager;
  FGrid.FilterManager := Self.FDataset.FilterManager;
  FGrid.SummaryManager := Self.FDataset.SummaryManager;
  FGrid.ColumnsHeaderMenuVisible:= true;

  FLastSelectedRow:=-1;
  Self.AutomaticChildsUpdateMode:= cuOnChangeSelection;
end;

destructor TUramakiBaseGridPlate.Destroy;
begin
  FreeAndNil(FToolbar);
  FreeAndNil(FSummaryPanel);

  FGridHelper.Free;
  FProvider.Free;
  FDataset.Free;

  inherited Destroy;
end;

procedure TUramakiBaseGridPlate.LoadConfigurationFromXML(aXMLElement: TmXmlElement);
var
  Cursor : TmXmlElementCursor;
begin
  Cursor := TmXmlElementCursor.Create(aXMLElement, 'gridConfiguration');
  try
    if Cursor.Count > 0 then
      FGridHelper.LoadSettingsFromXML(Cursor.Elements[0]);
  finally
    Cursor.Free;
  end;
  Cursor := TmXmlElementCursor.Create(aXMLElement, 'refreshChildsConfiguration');
  try
    if Cursor.Count > 0 then
    begin
      if Cursor.Elements[0].GetBooleanAttribute('automatic', true) then
        Self.AutomaticChildsUpdateMode:= cuOnChangeSelection
      else
        Self.AutomaticChildsUpdateMode:= cuDisabled;
    end;
  finally
    Cursor.Free;
  end;
end;

procedure TUramakiBaseGridPlate.SaveConfigurationToXML(aXMLElement: TmXmlElement);
var
  tmpElement : TmXmlElement;
begin
  FGridHelper.SaveSettingsToXML(aXMLElement.AddElement('gridConfiguration'));
  tmpElement := aXMLElement.AddElement('refreshChildsConfiguration');
  tmpElement.SetBooleanAttribute('automatic', (Self.AutomaticChildsUpdateMode = cuOnChangeSelection));
end;

procedure TUramakiBaseGridPlate.Clear;
begin
  Self.DisableControls;
  try
    GetDataProvider.Clear();
    RefreshDataset;
  finally
    Self.EnableControls;
  end;
  FLastSelectedRow := -1;
  InvokeChildsClear;
end;



end.
