// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit UramakiDBGridPlate;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  DB, Classes, Controls, DBGrids,
  mVirtualDataSet, mDBGrid, mQuickReadOnlyVirtualDataSetProvider,
  mDBGridHelper, mGridHelper, UramakiBase, mDataProviderInterfaces,
  UramakiBaseGridPlate;

type
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
    procedure DoCopyKeyOfCurrentRowToClipboard(Sender : TObject); override;
    procedure DoCopyValueOfCurrentCellToClipboard(Sender : TObject); override;
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


implementation

uses
  Variants,
  mFilter, mWaitCursor, mGraphicsUtility
{$IFDEF DEBUG}
  , mLog, mUtility
{$ENDIF}
;

{$IFDEF DEBUG}
var
  logger : TmLog;
{$ENDIF}
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
  FSummaryPanel.SummaryManager := Self.FDataset.SummaryManager;

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

procedure TUramakiDBGridPlate.DoCopyKeyOfCurrentRowToClipboard(Sender: TObject);
var
  tmpValue : variant;
begin
  if FDataset.RecordCount > 0 then
  begin
    tmpValue := FDataset.FieldByName(FProvider.GetKeyFieldName).Value;
    CopyTextToClipboard(VarToStr(tmpValue));
  end
  else
    CopyTextToClipboard('');
end;

procedure TUramakiDBGridPlate.DoCopyValueOfCurrentCellToClipboard(Sender: TObject);
var
  tmpValue : Variant;
begin
  if (FDataset.RecordCount > 0) and (FGrid.Col > 0) then
  begin
    tmpValue := FDataset.FieldByName(FGrid.Columns[FGrid.Col - 1].FieldName).Value;
    CopyTextToClipboard(VarToStr(tmpValue));
  end
  else
    CopyTextToClipboard('');
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

procedure TUramakiDBGridPlate.OnFilterDataset(Sender: TObject);
begin
  FLastSelectedRow:= nil;
  FLastSelectedRowsCount:= 0;
  if Assigned(Self.Parent) then
    UpdateChildsIfNeeded(false); //InvokeChildsRefresh;
end;

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

{$IFDEF DEBUG}
initialization
  logger := logManager.AddLog('UramakiDBGridPlate');
{$ENDIF}

end.
