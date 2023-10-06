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
  {$interfaces corba}
{$ENDIF}

interface

uses
  Classes, Controls, ExtCtrls, DB, DBGrids, Graphics,
  Forms, Menus, SysUtils, StdCtrls,
  UramakiToolbar,
  {$IFDEF FPC}
  LCLIntf,
  LclType,
  LclProc,
  LResources,
  LMessages,
  {$ENDIF}

  UramakiBase,
  mVirtualDataSet, mFilterPanel, mFilter, mGridHelper, mDBGrid, mDrawGrid,
  mXML, mDataProviderInterfaces, mSummary, mQuickReadOnlyVirtualDataSetProvider,
  mFields;

resourcestring
  SConfigureChildsUpdateModeCaption = 'Update of child widgets';
  SEnableAutomaticChildsUpdateCaption = 'Refresh them automatically';
  SDisableAutomaticChildsUpdateCaption = 'Do not refresh them automatically';
  SUpdateChildWidgetsBtnHint = 'Click to update child widgets';
  SSelectAllMenuCaption = 'Select all rows';
  SAutoAdjustColumnsMenuCaption = 'Auto-size columns';
  SAutoAdjustColumnsMenuHint = 'Set optimal width to columns';
  SGridActionsHint = 'Grid actions...';
  SCopySummaryToClipboard = 'Copy to clipboard';
  SGridFilterPanelHeader = 'Filtered by:';

const
  WM_USER_REFRESHCHILDS = WM_USER + 1;
  WM_USER_CLEARCHILDS = WM_USER + 2;

type
  IUramakiGridHelper = interface
    ['{9FCCE502-BC8C-43F1-B4BC-D088224355B1}']
    procedure CreateConfigureMenu(aToolbar : TUramakiToolbar; const aConfigureImageIndex : integer);
  end;

  TGetFieldValueFromDatasetRow = function (const aFieldName : String) : variant of object;
  TDoFillRollFromDatasetRow = procedure (aUrakamiRoll : TUramakiRoll; const aOnGetValue : TGetFieldValueFromDatasetRow) of object;

  TUramakiGridChildsAutomaticUpdateMode = (cuOnChangeSelection, cuDisabled);


  { TUramakiGridSummaryPanel }

  TUramakiGridSummaryPanel = class (ISummaryPanel)
  strict private
    const DEFAULT_HEIGHT = 25;
  strict private
    FPanel : TFlowPanel;
    FSubPanels : TList;
    FColor : TColor;
    FTextColor : TColor;
    FBkColor : TColor;
    FHeaderTextColor : TColor;
    procedure SetBkColor(AValue: TColor);
  public
    constructor Create;
    destructor Destroy; override;

    procedure LinkToPlate (aPlate : TUramakiPlate);

    procedure Hide;
    procedure Show;
    procedure SetSummaryValues (aScreenValues: TmSummaryScreenValues);

    property Color : TColor read FColor write FColor;
    property TextColor : TColor read FTextColor write FTextColor;
    property BkColor : TColor read FBkColor write SetBkColor;
    property HeaderTextColor : TColor read FHeaderTextColor write FHeaderTextColor;
  end;

  { TUramakiGridFiltersPanel }

  TUramakiGridFiltersPanel = class (IFilterPanel)
  strict private
    const DEFAULT_HEIGHT = 25;
  strict private
    FPanel : TFlowPanel;
    FSubPanels : TList;
    FColor : TColor;
    FTextColor : TColor;
    FBkColor : TColor;
    FHeaderTextColor : TColor;
    procedure SetBkColor(AValue: TColor);
  public
    constructor Create;
    destructor Destroy; override;

    procedure LinkToPlate (aPlate : TUramakiPlate);

    procedure SetFilters (const aFilters: TmFilters; const aFields : TmFields);

    property Color : TColor read FColor write FColor;
    property TextColor : TColor read FTextColor write FTextColor;
    property BkColor : TColor read FBkColor write SetBkColor;
    property HeaderTextColor : TColor read FHeaderTextColor write FHeaderTextColor;
  end;


  { TUramakiBaseGridPlate }

  TUramakiBaseGridPlate = class abstract (TUramakiPlate)
  strict private
    procedure DoUpdateChilds(Sender : TObject);
  protected
    FSummaryPanel : TUramakiGridSummaryPanel;
    //FToolbar : TToolBar;
    FToolbar : TUramakiToolbar;
    FFilterPanel : TmFilterPanel;
    FGridCommandsPopupMenu : TPopupMenu;
    FAutomaticChildsUpdateMode : TUramakiGridChildsAutomaticUpdateMode;

    // override these:
    function GetDataProvider : IVDDataProvider; virtual; abstract;
    procedure ReloadData (aFilters : TmFilters); virtual; abstract;

    procedure CreateToolbar(aImageList : TImageList; aConfigureImageIndex, aRefreshChildsImageIndex, aGridCommandsImageIndex : integer);
    procedure ConvertSelectionToUramakiRoll (aUramakiRoll : TUramakiRoll; aDoFillRollFromDatasetRow : TDoFillRollFromDatasetRow); virtual; abstract;
    procedure ProcessRefreshChilds(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF}); message WM_USER_REFRESHCHILDS;
    procedure ProcessClearChilds(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF}); message WM_USER_CLEARCHILDS;
    procedure InvokeChildsRefresh;
    procedure InvokeChildsClear;

    procedure OnClearFilter (Sender : TObject);
    procedure UpdateChildsIfNeeded (const aUpdateThemAnyWay : boolean); virtual; abstract;
    procedure DoSelectAll (Sender : TObject); virtual; abstract;
    procedure DoAutoAdjustColumns(Sender : TObject); virtual; abstract;
    function GetUramakiGridHelper : IUramakiGridHelper; virtual; abstract;
    function GetGridHelper : TmAbstractGridHelper; virtual; abstract;
    procedure DoProcessRefreshChilds; virtual; abstract;
    procedure GetSelectedItems (const aKeyFieldName : string; aList : TList); virtual; abstract;
    procedure SetupDataStructures; virtual; abstract;
    procedure SetDisplayLabelOfField(const aFieldName, aDisplayLabel: String); virtual; abstract;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure DisableControls; virtual; abstract;
    procedure EnableControls; virtual; abstract;
    procedure RefreshDataset; virtual; abstract;

    procedure LoadConfigurationFromXML (aXMLElement : TmXmlElement); override;
    procedure SaveConfigurationToXML (aXMLElement : TmXmlElement); override;

    property AutomaticChildsUpdateMode : TUramakiGridChildsAutomaticUpdateMode read FAutomaticChildsUpdateMode write FAutomaticChildsUpdateMode;
  end;

  { TUramakiGridHelper }

  TUramakiGridHelper = class(IUramakiGridHelper)
  strict private
    FEnableAutomaticChildsUpdateMI : TMenuItem;
    FDisableAutomaticChildsUpdateMI : TMenuItem;
    FConfigurePopupMenu : TPopupMenu;

    FGridPlate : TUramakiBaseGridPlate;
    FGridHelper : TmAbstractGridHelper;
    procedure OnEnableAutomaticChildsUpdate(Sender : TObject);
    procedure OnDisableAutomaticChildsUpdate(Sender : TObject);
  public
    constructor Create(aPlate : TUramakiBaseGridPlate; aGridHelper: TmAbstractGridHelper);
    procedure CreateConfigureMenu(aToolbar : TUramakiToolbar; const aConfigureImageIndex : integer);
  end;

  { TUramakiDBGridPlate }

  TUramakiDBGridPlate = class(TUramakiBaseGridPlate)
  strict private
    FLastSelectedRow : TBookmark;
    FLastSelectedRowsCount : integer;
    FLastSelectedRowsHash : string;
    FTriggerSelectionChanges : boolean;
    FDataset: TmVirtualDataset;
  protected
    FGrid: TmDBGrid;
    FProvider : TReadOnlyVirtualDatasetProvider;
    FGridHelper : TmDBGridHelper;
    FUramakiGridHelper : TUramakiGridHelper;
    FDatasource : TDatasource;
    FRunningDoUpdateChilds : boolean;
    procedure OnFilterDataset(Sender : TObject);
    procedure UpdateChildsIfNeeded (const aUpdateThemAnyWay : boolean); override;
    procedure DoSelectAll (Sender : TObject); override;
    procedure DoAutoAdjustColumns(Sender : TObject); override;
    function GetUramakiGridHelper : IUramakiGridHelper; override;
    function GetGridHelper : TmAbstractGridHelper; override;
    function GetValueFromDatasetRow (const aFieldName : String) : variant;
    procedure ConvertSelectionToUramakiRoll (aUramakiRoll : TUramakiRoll; aDoFillRollFromDatasetRow : TDoFillRollFromDatasetRow); override;
    procedure DoProcessRefreshChilds; override;
    procedure GetSelectedItems (const aKeyFieldName : string; aList : TList); override;
    procedure OnSelectEditor(Sender: TObject; Column: TColumn; var Editor: TWinControl);
    procedure OnCellClick(Column: TColumn);
    procedure SelectItems(const aDataProvider : IVDDataProvider; const aKeyValues : TStringList);
    procedure OnExecuteFilter (Sender : TObject);
    procedure SetupDataStructures; override;
    procedure SetDisplayLabelOfField(const aFieldName, aDisplayLabel: String); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure DisableControls; override;
    procedure EnableControls; override;
    procedure RefreshDataset; override;
    procedure Clear; override;
  end;

  { TUramakiDrawGridPlate }

  TUramakiDrawGridPlate = class (TUramakiBaseGridPlate)
  protected
    FGrid : TmDrawGrid;
    FGridHelper: TmDrawGridHelper;
    FUramakiGridHelper : TUramakiGridHelper;
    FProvider : TReadOnlyVirtualDatasetProvider;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure DisableControls; override;
    procedure EnableControls; override;
    procedure RefreshDataset; override;
  end;

implementation

uses
  variants,
  mGraphicsUtility, mWaitCursor, mMagnificationFactor
  {$IFDEF DEBUG}, mLog, mUtility{$ENDIF}
  ;

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

  { TFilterLabel }

  TFilterLabel = class(TLabel)
  strict private
  public
    constructor Create(TheOwner: TComponent); override;
  end;

{$IFDEF DEBUG}
var
  logger : TmLog;
{$ENDIF}

{ TFilterLabel }

constructor TFilterLabel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

{ TUramakiGridFiltersPanel }

procedure TUramakiGridFiltersPanel.SetBkColor(AValue: TColor);
begin
  if FBkColor=AValue then Exit;
  FBkColor:=AValue;
  if Assigned(FPanel) then
    FPanel.Color:= FBkColor;
end;

constructor TUramakiGridFiltersPanel.Create;
begin
  FSubPanels := TList.Create;
  FColor := $1A81F6;
  FTextColor := $E6E6E6;
  FBkColor:= clDefault;
  FHeaderTextColor:= clDefault;
end;

destructor TUramakiGridFiltersPanel.Destroy;
begin
  FSubPanels.Free;
end;

procedure TUramakiGridFiltersPanel.LinkToPlate(aPlate: TUramakiPlate);
begin
  if not Assigned(FPanel) then
  begin
    FPanel := TFlowPanel.Create(aPlate);
    FPanel.Parent := aPlate;
    FPanel.AutoWrap:= true;
    FPanel.AutoSize:= true;
    FPanel.Align:= alTop;
    FPanel.Color:= FBkColor;
  end;
end;

procedure TUramakiGridFiltersPanel.SetFilters(const aFilters: TmFilters; const aFields : TmFields);
var
  i : integer;
  tmpPanel : TPanel;
  tmpLabel : TFilterLabel;
  headLabel : TLabel;
  tmp : String;
  tmpField : TmField;
begin
  assert(Assigned(FPanel));

  for i := 0 to FSubPanels.Count - 1 do
  begin
    FPanel.RemoveControl(FSubPanels.Items[i]);
    TPanel(FSubPanels.Items[i]).Free;
  end;
  FSubPanels.Clear;
  if aFilters.Count = 0 then
    FPanel.Visible:= false
  else
  begin
    tmpPanel := TPanel.Create(FPanel);
    tmpPanel.Parent := FPanel;
    tmpPanel.Align:= alLeft;
    tmpPanel.Height:= ScaleForMagnification(DEFAULT_HEIGHT, true);
    tmpPanel.BorderSpacing.Left:= 2;
    tmpPanel.BorderSpacing.Right:= 2;
    tmpPanel.BorderSpacing.Top:= 2;
    tmpPanel.BorderSpacing.Bottom:= 2;
    tmpPanel.BevelInner:= bvNone;
    tmpPanel.BevelOuter:= bvNone;
    tmpPanel.BorderStyle:= bsNone;
    headLabel := TLabel.Create(tmpPanel);
    headLabel.Parent := tmpPanel;
    headLabel.Align:= alClient;
    headLabel.Caption:= SGridFilterPanelHeader;
    headLabel.AutoSize:= true;
    headLabel.Font.Size:= 13;
    headLabel.Font.Color:= FHeaderTextColor;
    ScaleFontForMagnification(headLabel.Font);
    tmpPanel.AutoSize:= true;
    FSubPanels.Add(tmpPanel);

    for i := 0 to aFilters.Count - 1 do
    begin
      tmpPanel := TPanel.Create(FPanel);
      tmpPanel.Parent := FPanel;
      tmpPanel.Align:= alLeft;
      tmpPanel.Height:= ScaleForMagnification(DEFAULT_HEIGHT, true);
      tmpPanel.BorderSpacing.Left:= 2;
      tmpPanel.BorderSpacing.Right:= 2;
      tmpPanel.BorderSpacing.Top:= 2;
      tmpPanel.BorderSpacing.Bottom:= 2;
      tmpPanel.BevelInner:= bvNone;
      tmpPanel.BevelOuter:= bvNone;
      tmpPanel.BorderStyle:= bsNone;
      tmpLabel := TFilterLabel.Create(tmpPanel);
      tmpLabel.Parent := tmpPanel;
      tmpLabel.Align:= alClient;

      tmp := aFilters.Get(i).FieldName;
      if Assigned(aFields) then
      begin
        tmpField := aFields.FieldByName(aFilters.Get(i).FieldName);
        if Assigned(tmpField) then
          tmp := tmpField.DisplayLabel;
      end;

      tmpLabel.Caption:= ' ' + tmp + ' ';
      tmpLabel.AutoSize:= true;
      tmpLabel.Font.Size:= 13;
      ScaleFontForMagnification(tmpLabel.Font);
      tmpPanel.AutoSize:= true;
      tmpPanel.Color := FColor;
      tmpLabel.Font.Color:= FTextColor;

      FSubPanels.Add(tmpPanel);
    end;
    FPanel.Visible:= true;
  end;

end;


{ TUramakiDrawGridPlate }

constructor TUramakiDrawGridPlate.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FGrid := TmDrawGrid.Create(Self);
  FGrid.Parent := Self;
  FGrid.Align:= alClient;
  FGrid.SummaryPanel := FSummaryPanel;
  FProvider := TReadOnlyVirtualDatasetProvider.Create;

  FGridHelper:= TmDrawGridHelper.Create(FGrid, FProvider.FormulaFields, nil);// missing CellDecorations!!
  FUramakiGridHelper := TUramakiGridHelper.Create(Self, FGridHelper); // missing CellDecorations!!
  FGridHelper.SetupGrid;
(*  FGrid.SortManager := Self.FProvider.SortManager;
  FGrid.FilterManager := Self.FProvider.FilterManager;
  FGrid.SummaryManager := Self.FProvider.SummaryManager;
  FGrid.ColumnsHeaderMenuVisible:= true;*)

  //FRunningDoUpdateChilds := false;
  Self.AutomaticChildsUpdateMode:= cuOnChangeSelection;
end;

destructor TUramakiDrawGridPlate.Destroy;
begin
  FGridHelper.Free;
  FUramakiGridHelper.Free;
  FProvider.Free;

  inherited Destroy;
end;

procedure TUramakiDrawGridPlate.DisableControls;
begin

end;

procedure TUramakiDrawGridPlate.EnableControls;
begin

end;

procedure TUramakiDrawGridPlate.RefreshDataset;
begin

end;

{ TUramakiDBGridPlate }

constructor TUramakiDBGridPlate.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FLastSelectedRow:= nil;
  FLastSelectedRowsCount:= 0;
  FLastSelectedRowsHash := '';

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
  Self.FDataset.OnFilter:= Self.OnFilterDataset;

  FGridHelper:= TmDBGridHelper.Create(FGrid, FProvider.FormulaFields, FGrid.CellDecorations);
  FUramakiGridHelper := TUramakiGridHelper.Create(Self, FGridHelper);
  FGridHelper.SetupGrid;
  FGrid.SortManager := Self.FDataset.SortManager;
  FGrid.FilterManager := Self.FDataset.FilterManager;
  FGrid.SummaryManager := Self.FDataset.SummaryManager;
  FGrid.ColumnsHeaderMenuVisible:= true;

  FRunningDoUpdateChilds := false;
  Self.AutomaticChildsUpdateMode:= cuOnChangeSelection;
  FGrid.OnCellClick:= Self.OnCellClick;
  FGrid.OnSelectEditor:= Self.OnSelectEditor;
  FTriggerSelectionChanges:= true;
end;

destructor TUramakiDBGridPlate.Destroy;
begin
  FGridHelper.Free;
  FUramakiGridHelper.Free;
  FProvider.Free;
  FDataset.Free;

  inherited Destroy;
end;

{ TSummaryLabel }

procedure TSummaryLabel.OnCopySummaryToClipboard(Sender: TObject);
begin
  if VarIsNull(FRawValue) then
    CopyTextToClipboard('')
  else
  begin
    if FDataType = svtDate then
      CopyTextToClipboard(DateToStr(FRawValue))
    else if FDataType = svtDateTime then
      CopyTextToClipboard(DateTimeToStr(FRawValue))
    else
      CopyTextToClipboard(VarToStr(FRawValue));
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

procedure TUramakiGridSummaryPanel.SetBkColor(AValue: TColor);
begin
  if FBkColor=AValue then Exit;
  FBkColor:=AValue;
  if Assigned(FPanel) then
    FPanel.Color:= FBkColor;
end;

constructor TUramakiGridSummaryPanel.Create;
begin
  FSubPanels := TList.Create;
  FColor := $A64FA4;
  FTextColor := $E6E6E6;
  FBkColor:= clDefault;
  FHeaderTextColor:= clDefault;
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
    FPanel.Color:= FBkColor;
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
    TPanel(FSubPanels.Items[i]).Free;

  FSubPanels.Clear;
  FPanel.ControlList.Clear;
  for i := 0 to aScreenValues.Count - 1 do
  begin
    tmpPanel := TPanel.Create(FPanel);
    tmpPanel.Parent := FPanel;
    tmpPanel.Align:= alLeft;
    tmpPanel.Height:= ScaleForMagnification(DEFAULT_HEIGHT, true);
    tmpPanel.BorderSpacing.Left:= 2;
    tmpPanel.BorderSpacing.Right:= 2;
    tmpPanel.BorderSpacing.Top:= 2;
    tmpPanel.BorderSpacing.Bottom:= 2;
    tmpPanel.BevelInner:= bvNone;
    tmpPanel.BevelOuter:= bvNone;
    tmpPanel.BorderStyle:= bsNone;
    tmpLabel := TSummaryLabel.Create(tmpPanel);
    tmpLabel.Parent := tmpPanel;
    tmpLabel.Align:= alClient;
    tmpLabel.Caption:= ' ' + aScreenValues.Get(i).FormattedValue + ' ';
    tmpLabel.RawValue:= aScreenValues.Get(i).RawValue;
    tmpLabel.DataType:= aScreenValues.Get(i).DataType;
    tmpLabel.AutoSize:= true;
    tmpLabel.Font.Size:= 13;
    ScaleFontForMagnification(tmpLabel.Font);
    tmpPanel.AutoSize:= true;
    tmpPanel.Color := FColor;
    tmpLabel.Font.Color:= FTextColor;

    FSubPanels.Add(tmpPanel);
  end;
end;

{ TUramakiGridHelper }

constructor TUramakiGridHelper.Create(aPlate : TUramakiBaseGridPlate; aGridHelper: TmAbstractGridHelper);
begin
  FGridPlate := aPlate;
  FGridHelper := aGridHelper;
end;


//procedure TUramakiGridHelper.CreateStandardConfigureMenu(aToolbar: TToolbar; const aConfigureImageIndex: integer);
procedure TUramakiGridHelper.CreateConfigureMenu(aToolbar : TUramakiToolbar; const aConfigureImageIndex : integer);
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
  itm.OnClick:= FGridHelper.OnEditSettings;
  itm.Hint:= SConfigureGridCommandHint;
  itm.Caption:= SConfigureGridCommandCaption;
  if Assigned(FGridHelper.FormulaFields) then
  begin
    itm := TMenuItem.Create(FConfigurePopupMenu);
    FConfigurePopupMenu.Items.Add(itm);
    itm.OnClick:= FGridHelper.OnEditFormulaFields;
    itm.Hint:= SConfigureFormulaFieldsCommandHint;
    itm.Caption:= SConfigureFormulaFieldsCommandCaption;
  end;
  if Assigned(FGridHelper.CellDecorations) then
  begin
    itm := TMenuItem.Create(FConfigurePopupMenu);
    FConfigurePopupMenu.Items.Add(itm);
    itm.OnClick:= FGridHelper.OnEditCellDecorations();
    itm.Hint:= SConfigureCellDecorationsCommandHint;
    itm.Caption:= SConfigureCellDecorationsCommandCaption;
  end;

  itm := TMenuItem.Create(FConfigurePopupMenu);
  itm.Caption:= '-';
  FConfigurePopupMenu.Items.Add(itm);

  itm := TMenuItem.Create(FConfigurePopupMenu);
  FConfigurePopupMenu.Items.Add(itm);
  itm.OnClick:= FGridHelper.OnExportGridAsCsv;
  itm.Hint:= SExportGridAsCsvCommandHint;
  itm.Caption:= SExportGridAsCsvCommandCaption;

  itm := TMenuItem.Create(FConfigurePopupMenu);
  FConfigurePopupMenu.Items.Add(itm);
  itm.OnClick:= FGridHelper.OnExportGridAsXlsx;
  itm.Hint:= SExportGridAsXlsxCommandHint;
  itm.Caption:= SExportGridAsXlsxCommandCaption;

  itm := TMenuItem.Create(FConfigurePopupMenu);
  FConfigurePopupMenu.Items.Add(itm);
  itm.OnClick:= FGridHelper.OnExportGridAsHtml();
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


procedure TUramakiGridHelper.OnEnableAutomaticChildsUpdate(Sender: TObject);
begin
  FEnableAutomaticChildsUpdateMI.Checked:= true;
  FDisableAutomaticChildsUpdateMI.Checked:=false;
  FGridPlate.AutomaticChildsUpdateMode:= cuOnChangeSelection;
end;

procedure TUramakiGridHelper.OnDisableAutomaticChildsUpdate(Sender: TObject);
begin
  FEnableAutomaticChildsUpdateMI.Checked:= false;
  FDisableAutomaticChildsUpdateMI.Checked:= true;
  FGridPlate.AutomaticChildsUpdateMode:= cuDisabled;
end;


{ TUramakiBaseGridPlate }


procedure TUramakiDBGridPlate.UpdateChildsIfNeeded (const aUpdateThemAnyWay : boolean);
var
  newHash : string;
begin
  FRunningDoUpdateChilds := true;
  try
    if (FAutomaticChildsUpdateMode = cuOnChangeSelection) and (not aUpdateThemAnyWay) then
    begin
      if FGrid.SelectedRows.Count > 1 then
      begin
        newHash:= FGrid.SelectedRows.CalculateHashCode;
        if newHash <> FLastSelectedRowsHash then
        begin
          FLastSelectedRowsHash:= newHash;
          FLastSelectedRow := FGrid.DataSource.DataSet.Bookmark;
          FLastSelectedRowsCount := FGrid.SelectedRows.Count;
          if Assigned(Self.Parent) then
            InvokeChildsRefresh;
        end;
      end
      else begin
        if (FGrid.SelectedRows.Count = 0) then // and ((FGrid.DataSource.DataSet.BookmarkValid(FLastSelectedRow)) or (FLastSelectedRowsCount > 0)) then
        begin
          FLastSelectedRow:= nil;
          FLastSelectedRowsCount:= 0;
          if Assigned(Self.Parent) then
            InvokeChildsRefresh;
        end
        else
        begin
          {$IFDEF DEBUG}
          logger.Debug(Self.ClassName + ' FLastSelectedRow:' + TBytesToString(FLastSelectedRow) + 'FGrid.DataSource.DataSet.Bookmark:' + TBytesToString(FGrid.DataSource.DataSet.Bookmark));
          {$ENDIF}
          if FGrid.DataSource.DataSet.CompareBookmarks(FLastSelectedRow, FGrid.DataSource.DataSet.Bookmark) <> 0 then
          begin
            FLastSelectedRow := FGrid.DataSource.DataSet.Bookmark;
            FLastSelectedRowsCount := 1;
            if Assigned(Self.Parent) then
              InvokeChildsRefresh;
          end;
        end;
      end;
    end
    else
    begin
      FLastSelectedRow:= nil;
      FLastSelectedRowsCount:= 0;
      if Assigned(Self.Parent) then
        InvokeChildsRefresh;
    end;
  finally
    FRunningDoUpdateChilds := false;
  end;
end;

procedure TUramakiDBGridPlate.DoSelectAll(Sender: TObject);
begin
  FTriggerSelectionChanges:= false;
  try
    FGridHelper.SelectAllRows;
  finally
    if AutomaticChildsUpdateMode = cuOnChangeSelection then
    begin
      FTriggerSelectionChanges := true;
      if FAutomaticChildsUpdateMode = cuOnChangeSelection then
        UpdateChildsIfNeeded(true);
    end;
  end;
end;

procedure TUramakiDBGridPlate.DoAutoAdjustColumns(Sender: TObject);
begin
  if not Assigned(FGrid) then
    exit;
  FGrid.AutoAdjustColumns;
end;

function TUramakiDBGridPlate.GetUramakiGridHelper : IUramakiGridHelper;
begin
  Result := FUramakiGridHelper;
end;

function TUramakiDBGridPlate.GetGridHelper: TmAbstractGridHelper;
begin
  Result := FGridHelper;
end;

function TUramakiDBGridPlate.GetValueFromDatasetRow(const aFieldName: String): variant;
begin
  Result := FDataset.FieldByName(aFieldName).AsVariant;
end;

procedure TUramakiBaseGridPlate.DoUpdateChilds(Sender: TObject);
begin
  UpdateChildsIfNeeded(true);
end;

procedure TUramakiDBGridPlate.OnFilterDataset(Sender: TObject);
begin
  FLastSelectedRow:= nil;
  FLastSelectedRowsCount:= 0;
  if Assigned(Self.Parent) then
    InvokeChildsRefresh;
end;

procedure TUramakiBaseGridPlate.CreateToolbar(aImageList : TImageList; aConfigureImageIndex, aRefreshChildsImageIndex, aGridCommandsImageIndex : integer);
var
  mItm : TMenuItem;
begin
  FToolbar := TUramakiToolbar.Create(Self);
  FToolbar.Images := aImageList;
  FToolbar.Parent := Self;
  GetUramakiGridHelper.CreateConfigureMenu(FToolbar, aConfigureImageIndex);

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

  mItm := TMenuItem.Create(FGridCommandsPopupMenu);
  FGridCommandsPopupMenu.Items.Add(mItm);
  mItm.OnClick:= Self.DoAutoAdjustColumns;
  mItm.Hint:= SAutoAdjustColumnsMenuHint;
  mItm.Caption:= SAutoAdjustColumnsMenuCaption;

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

procedure TUramakiDBGridPlate.ConvertSelectionToUramakiRoll(aUramakiRoll: TUramakiRoll; aDoFillRollFromDatasetRow : TDoFillRollFromDatasetRow);
var
  tmpBookmark : TBookMark;
  i : integer;
begin
  if FDataset.ControlsDisabled then
    exit;

  if (FGrid.SelectedRows.Count > 1) then
  begin
    FGrid.DataSource.DataSet.DisableControls;// BeginUpdate;
    try
      tmpBookmark := FDataset.Bookmark;
      try
        for i := 0 to FGrid.SelectedRows.Count - 1 do
        begin
          // rarely odd behaviour, so better to put a check
          if FDataset.BookmarkValid(FGrid.SelectedRows[i]) then
          begin
            FDataset.GotoBookmark(FGrid.SelectedRows[i]);
            aDoFillRollFromDatasetRow(aUramakiRoll, GetValueFromDatasetRow);
          end;
        end;
      finally
        FDataset.GotoBookmark(tmpBookmark);
      end;
    finally
      FGrid.DataSource.DataSet.EnableControls; // EndUpdate(true);
    end;
  end
  else //if FGrid.SelectedRows.Count =  1 then
  begin
    aDoFillRollFromDatasetRow(aUramakiRoll, GetValueFromDatasetRow);
  end;
end;

procedure TUramakiDBGridPlate.DoProcessRefreshChilds;
begin
  FTriggerSelectionChanges:= false;
  try
    EngineMediator.PleaseRefreshMyChilds(Self);
  finally
    FTriggerSelectionChanges:= true;
  end;
end;

procedure TUramakiBaseGridPlate.ProcessRefreshChilds(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
  Self.DoProcessRefreshChilds;
end;

procedure TUramakiBaseGridPlate.ProcessClearChilds(var Message: TLMessage);
begin
  EngineMediator.PleaseClearMyChilds(Self);
end;

procedure TUramakiDBGridPlate.OnSelectEditor(Sender: TObject; Column: TColumn; var Editor: TWinControl);
begin
  if FRunningDoUpdateChilds then
    exit;
  if not FTriggerSelectionChanges then
    exit;
  if FAutomaticChildsUpdateMode = cuOnChangeSelection then
    UpdateChildsIfNeeded(false);
end;

procedure TUramakiDBGridPlate.OnCellClick(Column: TColumn);
begin
  if not FTriggerSelectionChanges then
    exit;
  if FAutomaticChildsUpdateMode = cuOnChangeSelection then
    UpdateChildsIfNeeded(false);
end;

procedure TUramakiDBGridPlate.SelectItems(const aDataProvider : IVDDataProvider; const aKeyValues: TStringList);
begin
  FTriggerSelectionChanges := false;
  try
    FGridHelper.SelectRows(aDataProvider.GetKeyFieldName, aKeyValues);
  finally
    if AutomaticChildsUpdateMode = cuOnChangeSelection then
    begin
      FTriggerSelectionChanges:= true;
      UpdateChildsIfNeeded(true);
    end;
  end;
end;

procedure TUramakiBaseGridPlate.InvokeChildsRefresh;
begin
  PostMessage(Self.Handle, WM_USER_REFRESHCHILDS, 0, 0);
end;

procedure TUramakiBaseGridPlate.InvokeChildsClear;
begin
  PostMessage(Self.Handle, WM_USER_CLEARCHILDS, 0, 0);
end;

procedure TUramakiDBGridPlate.GetSelectedItems(const aKeyFieldName: string; aList: TList);
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
      FTriggerSelectionChanges :=false;
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
        FTriggerSelectionChanges:= true;
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

procedure TUramakiDBGridPlate.OnExecuteFilter(Sender: TObject);
var
  tmpFilters : TmFilters;
begin
  Self.DisableControls;
  try
    try
      TWaitCursor.ShowWaitCursor('TUramakiDBGridPlate.OnExecuteFilter');

      tmpFilters := TmFilters.Create;
      try
        FFilterPanel.GetFilters(tmpFilters);
        Self.ReloadData(tmpFilters);
        RefreshDataset;
      finally
        tmpFilters.Free;
      end;
    finally
      TWaitCursor.UndoWaitCursor('TUramakiDBGridPlate.OnExecuteFilter');
    end;
  finally
    Self.EnableControls;
  end;
  (*
  FLastSelectedRow := nil;
  FLastSelectedRowsCount:= 0;
  FLastSelectedRowsHash := '';
  if AutomaticChildsUpdateMode = cuOnChangeSelection then
    InvokeChildsRefresh
  else
    InvokeChildsClear;
  *)
end;

procedure TUramakiDBGridPlate.SetupDataStructures;
begin
  FDataset.Active := true;
end;

procedure TUramakiDBGridPlate.SetDisplayLabelOfField(const aFieldName, aDisplayLabel: String);
begin
  FDataset.FieldByName(aFieldName).DisplayLabel:= aDisplayLabel;
end;

procedure TUramakiDBGridPlate.DisableControls;
begin
  FDataset.DisableControls;
end;

procedure TUramakiDBGridPlate.EnableControls;
begin
  FDataset.EnableControls;
end;

procedure TUramakiDBGridPlate.RefreshDataset;
begin
  FGrid.SelectedRows.Clear;
  FGrid.SelectedIndex:= -1;
  FLastSelectedRow := nil;
  FLastSelectedRowsCount:= 0;
  FLastSelectedRowsHash := '';
  FDataset.Refresh;
end;

procedure TUramakiDBGridPlate.Clear;
begin
  Self.DisableControls;
  try
    GetDataProvider.Clear();
    RefreshDataset;
  finally
    Self.EnableControls;
  end;
  FLastSelectedRow := nil;
  FLastSelectedRowsCount:= 0;
  FLastSelectedRowsHash := '';

  if AutomaticChildsUpdateMode <> cuDisabled then
    InvokeChildsClear;
end;

constructor TUramakiBaseGridPlate.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FSummaryPanel := TUramakiGridSummaryPanel.Create;
  FSummaryPanel.LinkToPlate(Self);
end;

destructor TUramakiBaseGridPlate.Destroy;
begin
  FreeAndNil(FToolbar);
  FreeAndNil(FSummaryPanel);

  inherited Destroy;
end;

procedure TUramakiBaseGridPlate.LoadConfigurationFromXML(aXMLElement: TmXmlElement);
var
  Cursor : TmXmlElementCursor;
begin
  Cursor := TmXmlElementCursor.Create(aXMLElement, 'gridConfiguration');
  try
    if Cursor.Count > 0 then
      GetGridHelper.LoadSettingsFromXML(Cursor.Elements[0]);
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
  GetGridHelper.SaveSettingsToXML(aXMLElement.AddElement('gridConfiguration'));
  tmpElement := aXMLElement.AddElement('refreshChildsConfiguration');
  tmpElement.SetBooleanAttribute('automatic', (Self.AutomaticChildsUpdateMode = cuOnChangeSelection));
end;


{$IFDEF DEBUG}
initialization
  logger := logManager.AddLog('UramakiBaseGridPlate');
{$ENDIF}
end.
