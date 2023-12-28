// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit UramakiDrawGridPlate;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, Controls, Grids, SysUtils, Graphics,
  mQuickReadOnlyVirtualDataSetProvider, mDrawGridHelper,
  mGridHelper, mDataProviderInterfaces,
  UramakiBase, UramakiBaseGridPlate

  {$IFDEF DEBUG}
  , mLog
  {$ENDIF}
  ;
type

  { TUramakiDrawGridPlate }

  TUramakiDrawGridPlate = class(TUramakiBaseGridPlate)
  strict private
    FLastSelectedRow : longint;
    FLastSelectedRowsCount : integer;
    FLastSelectedRowsHash : string;
    FTriggerSelectionChanges : boolean;

    function GetAlternateGridRowColor: TColor;
    procedure SetAlternateGridRowColor(AValue: TColor);
    function GetValueFromDatasetRow (const aFieldName : String) : variant;
  protected
    FGrid: TmDrawGrid;
    FGridFiltersPanel: TUramakiGridFiltersPanel;
    FProvider : TReadOnlyVirtualDatasetProvider;
    FGridHelper : TmDrawGridHelper;
    FUramakiGridHelper : TUramakiGridHelper;

    FCurrentRow : integer;
    FRunningDoUpdateChilds : boolean;
    procedure UpdateChildsIfNeeded (const aUpdateThemAnyWay : boolean); override;
    procedure DoSelectAll (Sender : TObject); override;
    procedure DoAutoAdjustColumns(Sender : TObject); override;
    function GetUramakiGridHelper : IUramakiGridHelper; override;
    function GetGridHelper : TmAbstractGridHelper; override;
    procedure ConvertSelectionToUramakiRoll (aUramakiRoll : TUramakiRoll; aDoFillRollFromDatasetRow : TDoFillRollFromDatasetRow); override;
    procedure DoProcessRefreshChilds; override;
    procedure GetSelectedItems (const aKeyFieldName : string; aList : TList); override;
    procedure SelectItems(const aDataProvider : IVDDataProvider; const aKeyValues : TStringList);
    procedure OnExecuteFilter (Sender : TObject);
    procedure SetupDataStructures; override;
    procedure SetDisplayLabelOfField(const aFieldName, aDisplayLabel: String); override;
    procedure OnSelectionChanged(Sender: TObject; aCol, aRow: Integer);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure DisableControls; override;
    procedure EnableControls; override;
    procedure RefreshDataset; override;
    procedure Clear; override;

    property AlternateGridRowColor : TColor read GetAlternateGridRowColor write SetAlternateGridRowColor;
  end;

implementation

uses
  Variants,
  mIntList, mFilter, mWaitCursor, mFields;

{$IFDEF DEBUG}
var
  logger: TmLog;
{$ENDIF}

{ TUramakiDrawGridPlate }

function TUramakiDrawGridPlate.GetAlternateGridRowColor: TColor;
begin
  Result := FGrid.AlternateColor;
end;

procedure TUramakiDrawGridPlate.SetAlternateGridRowColor(AValue: TColor);
begin
  FGrid.AlternateColor:= AValue;
end;

function TUramakiDrawGridPlate.GetValueFromDatasetRow(const aFieldName: String): variant;
var
  tmpValue : Variant;
begin
  FProvider.GetFieldValue(aFieldName, FCurrentRow, tmpValue);
  Result := tmpValue;
end;

procedure TUramakiDrawGridPlate.UpdateChildsIfNeeded(const aUpdateThemAnyWay: boolean);
var
  newHash : string;
  selectedRows : TIntegerList;
begin
  selectedRows := TIntegerList.Create;
  FRunningDoUpdateChilds := true;
  try
    FGridHelper.GetSelectedRows(selectedRows);
    if (FAutomaticChildsUpdateMode = cuOnChangeSelection) and (not aUpdateThemAnyWay) then
    begin
      if selectedRows.Count > 1 then
      begin
        newHash:= FGridHelper.CalculateHashCodeOfSelectedRows;
        if newHash <> FLastSelectedRowsHash then
        begin
          FLastSelectedRowsHash:= newHash;
          FLastSelectedRow := FGrid.Row;
          FLastSelectedRowsCount := selectedRows.Count;
          if Assigned(Self.Parent) then
            InvokeChildsRefresh;
        end;
      end
      else begin
        if (selectedRows.Count = 0) then
        begin
          FLastSelectedRow:= -1;
          FLastSelectedRowsCount:= 0;
          if Assigned(Self.Parent) then
            InvokeChildsRefresh;
        end
        else
        begin
          if (FGrid.Row <> FLastSelectedRow) or (FLastSelectedRowsCount <> selectedRows.Count) then
          begin
            FLastSelectedRow := FGrid.Row;
            FLastSelectedRowsCount := 1;
            if Assigned(Self.Parent) then
              InvokeChildsRefresh;
          end;
        end;
      end;
    end
    else
    begin
      FLastSelectedRow:= -1;
      FLastSelectedRowsCount:= 0;
      if Assigned(Self.Parent) then
        InvokeChildsRefresh;
    end;
  finally
    FRunningDoUpdateChilds := false;
    selectedRows.Free;
  end;
end;

procedure TUramakiDrawGridPlate.DoSelectAll(Sender: TObject);
begin
  if FProvider.GetRecordCount > 0 then
  begin
    FTriggerSelectionChanges := false;
    try
      FGridHelper.SelectAllRows;
      // FGrid.Invalidate;
    finally
      FTriggerSelectionChanges:= true;
      if AutomaticChildsUpdateMode = cuOnChangeSelection then
        UpdateChildsIfNeeded(true);
    end;
  end;
end;

procedure TUramakiDrawGridPlate.DoAutoAdjustColumns(Sender: TObject);
begin
  TWaitCursor.ShowWaitCursor('TUramakiDrawGridPlate.DoAutoAdjustColumns');
  try
    FGridHelper.AutoSizeColumns;
  finally
    TWaitCursor.UndoWaitCursor('TUramakiDrawGridPlate.DoAutoAdjustColumns');
  end;
end;

function TUramakiDrawGridPlate.GetUramakiGridHelper: IUramakiGridHelper;
begin
  Result := FUramakiGridHelper;
end;

function TUramakiDrawGridPlate.GetGridHelper: TmAbstractGridHelper;
begin
  Result := FGridHelper;
end;

procedure TUramakiDrawGridPlate.ConvertSelectionToUramakiRoll(aUramakiRoll: TUramakiRoll; aDoFillRollFromDatasetRow: TDoFillRollFromDatasetRow);
var
  i, k : integer;
  rows : TIntegerList;
begin
  rows := TIntegerList.Create;
  try
    FGridHelper.GetSelectedRows(rows);
    for i := 0 to rows.Count -1 do
    begin
      FCurrentRow:= rows.Items[i];
      aDoFillRollFromDatasetRow(aUramakiRoll, GetValueFromDatasetRow);
    end;
  finally
    rows.Free;
  end;
end;

procedure TUramakiDrawGridPlate.DoProcessRefreshChilds;
begin
  FTriggerSelectionChanges := false;
  try
    EngineMediator.PleaseRefreshMyChilds(Self);
  finally
    FTriggerSelectionChanges:= true;
  end;
end;

procedure TUramakiDrawGridPlate.GetSelectedItems(const aKeyFieldName: string; aList: TList);
var
  list : TIntegerList;
  i, k : integer;
  tmpDatum : IVDDatum;
  tmpKey : variant;
begin
  {$IFDEF DEBUG}
  logger.Debug('FGrid.Row ' + IntToStr(FGrid.Row));
  logger.Debug('FGrid.SelectedRangeCount ' + IntToStr(FGrid.SelectedRangeCount));
  {$ENDIF}
  list := TIntegerList.Create;
  try
    FGridHelper.GetSelectedRows(list);
    for i := 0 to list.Count -1 do
    begin
      FProvider.GetFieldValue(aKeyFieldName, list.Items[i], tmpKey);
      tmpDatum := GetDataProvider.FindDatumByStringKey(VarToStr(tmpKey));
      if Assigned(tmpDatum) then
        aList.Add(tmpDatum.AsObject);
    end;
  finally
    list.Free;
  end;
end;

procedure TUramakiDrawGridPlate.SelectItems(const aDataProvider: IVDDataProvider; const aKeyValues: TStringList);
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

procedure TUramakiDrawGridPlate.OnExecuteFilter(Sender: TObject);
var
  tmpFilters : TmFilters;
begin
  Self.DisableControls;
  try
    try
      TWaitCursor.ShowWaitCursor('TUramakiKGridPlate.OnExecuteFilter');

      tmpFilters := TmFilters.Create;
      try
        FFilterPanel.GetFilters(tmpFilters);
        Self.ReloadData(tmpFilters);
        RefreshDataset;
      finally
        tmpFilters.Free;
      end;
    finally
      TWaitCursor.UndoWaitCursor('TUramakiKGridPlate.OnExecuteFilter');
    end;
  finally
    Self.EnableControls;
  end;
  FLastSelectedRow := -1;
  FLastSelectedRowsCount:= 0;
  FLastSelectedRowsHash := '';
  if AutomaticChildsUpdateMode = cuOnChangeSelection then
    InvokeChildsRefresh
  else
    InvokeChildsClear;
end;

procedure TUramakiDrawGridPlate.SetupDataStructures;
begin
  FGridHelper.InitGrid;
end;

procedure TUramakiDrawGridPlate.SetDisplayLabelOfField(const aFieldName, aDisplayLabel: String);
var
  curField : TmField;
begin
  curField := FGridHelper.GetField(aFieldName);
  if Assigned(curField) then
    curField.DisplayLabel:= aDisplayLabel;
end;

procedure TUramakiDrawGridPlate.OnSelectionChanged(Sender: TObject; aCol, aRow: Integer);
begin
  if FRunningDoUpdateChilds then
    exit;
  if not FTriggerSelectionChanges then
    exit;
  if FAutomaticChildsUpdateMode = cuOnChangeSelection then
    UpdateChildsIfNeeded(false);
end;

constructor TUramakiDrawGridPlate.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FGridFiltersPanel := TUramakiGridFiltersPanel.Create;
  FGridFiltersPanel.LinkToPlate(Self);

  FLastSelectedRow:= -1;
  FLastSelectedRowsCount:= 0;
  FLastSelectedRowsHash := '';

  FGrid := TmDrawGrid.Create(Self);
  FGrid.Parent := Self;
  FGrid.Align:= alClient;

  FProvider := TReadOnlyVirtualDatasetProvider.Create;
  FGridHelper:= TmDrawGridHelper.Create(FGrid, FProvider.FormulaFields);
  FGridHelper.Provider := FProvider;
  FGridHelper.SummaryPanel := FSummaryPanel;
  FGridHelper.FiltersPanel := FGridFiltersPanel;;
  FSummaryPanel.SummaryManager := FGridHelper.GetSummaryManager;

  FUramakiGridHelper := TUramakiGridHelper.Create(Self, FGridHelper);
  FGridHelper.SetupGrid;
//  FGridHelper.OnGridFiltered := OnGridFiltered;
//  FGridHelper.OnGridSorted:= OnDataSorted;
  FGrid.OnSelection:= OnSelectionChanged;

  FRunningDoUpdateChilds := false;
  Self.AutomaticChildsUpdateMode:= cuOnChangeSelection;
  FTriggerSelectionChanges:= true;
end;

destructor TUramakiDrawGridPlate.Destroy;
begin
  FGridHelper.Free;
  FUramakiGridHelper.Free;
  FProvider.Free;
  FreeAndNil(FGridFiltersPanel);

  inherited Destroy;
end;

procedure TUramakiDrawGridPlate.DisableControls;
begin
  FGrid.BeginUpdate;
end;

procedure TUramakiDrawGridPlate.EnableControls;
begin
  FGrid.EndUpdate(true);
end;

procedure TUramakiDrawGridPlate.RefreshDataset;
begin
  FLastSelectedRow:= -1;
  FLastSelectedRowsCount:= 0;
  FLastSelectedRowsHash := '';
  FGridHelper.RefreshDataProvider(false);
end;

procedure TUramakiDrawGridPlate.Clear;
begin
  Self.DisableControls;
  try
    GetDataProvider.Clear();
    RefreshDataset;
  finally
    Self.EnableControls;
  end;
  FLastSelectedRow := -1;
  FLastSelectedRowsCount:= 0;
  FLastSelectedRowsHash := '';
  InvokeChildsClear;
end;

{$IFDEF DEBUG}
initialization
  logger := logManager.AddLog('UramakiDrawGridPlate');
{$ENDIF}
end.
