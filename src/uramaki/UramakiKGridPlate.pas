unit UramakiKGridPlate;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface
uses
  Classes, Controls, Grids,
  kgrids,
  mQuickReadOnlyVirtualDataSetProvider, mKGridHelper,
  mGridHelper, mDataProviderInterfaces,
  UramakiBase, UramakiBaseGridPlate

  {$IFDEF DEBUG}
  , mLog ,SysUtils
  {$ENDIF}
  ;

type

  { TUramakiKGridPlate }

  TUramakiKGridPlate = class(TUramakiBaseGridPlate)
  strict private
    FLastSelectedRow : longint;
    FLastSelectedRowsCount : integer;
    FLastSelectedRowsHash : string;
    FTriggerSelectionChanges : boolean;
    function GetValueFromDatasetRow (const aFieldName : String) : variant;
  protected
    FGrid: TKGrid;
    FCurrentRow : integer;
    FProvider : TReadOnlyVirtualDatasetProvider;
    FGridHelper : TmKGridHelper;
    FUramakiGridHelper : TUramakiGridHelper;
    FRunningDoUpdateChilds : boolean;
    procedure OnGridFiltered(Sender : TObject);
    procedure OnSelectionChanged(Sender : TObject);
    procedure UpdateChildsIfNeeded (const aUpdateThemAnyWay : boolean); override;
    procedure DoSelectAll (Sender : TObject); override;
    procedure DoAutoAdjustColumns(Sender : TObject); override;
    function GetUramakiGridHelper : IUramakiGridHelper; override;
    function GetGridHelper : TmAbstractGridHelper; override;
    procedure ConvertSelectionToUramakiRoll (aUramakiRoll : TUramakiRoll; aDoFillRollFromDatasetRow : TDoFillRollFromDatasetRow); override;
    procedure DoProcessRefreshChilds; override;
    procedure GetSelectedItems (const aKeyFieldName : string; aList : TList); override;
//    procedure OnSelectEditor(Sender: TObject; Column: TColumn; var Editor: TWinControl);
//    procedure OnCellClick(Column: TColumn);
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
  mKGridUtils, mWaitCursor, mFilter, mIntList, mFields;

{$IFDEF DEBUG}
var
  logger : TmLog;
{$ENDIF}

{ TUramakiKGridPlate }

function TUramakiKGridPlate.GetValueFromDatasetRow(const aFieldName: String): variant;
var
  tmpValue : Variant;
begin
  FProvider.GetFieldValue(aFieldName, FCurrentRow, tmpValue);
  Result := tmpValue;
end;

procedure TUramakiKGridPlate.OnGridFiltered(Sender: TObject);
begin
  FLastSelectedRow:= -1;
  FLastSelectedRowsCount:= 0;
  if Assigned(Self.Parent) then
    InvokeChildsRefresh;
end;

procedure TUramakiKGridPlate.OnSelectionChanged(Sender: TObject);
begin
  if FRunningDoUpdateChilds then
    exit;
  if not FTriggerSelectionChanges then
    exit;
  if FAutomaticChildsUpdateMode = cuOnChangeSelection then
    UpdateChildsIfNeeded(false);
end;


procedure TUramakiKGridPlate.UpdateChildsIfNeeded(const aUpdateThemAnyWay: boolean);
var
  newHash : string;
begin
  FRunningDoUpdateChilds := true;
  try
    if (FAutomaticChildsUpdateMode = cuOnChangeSelection) and (not aUpdateThemAnyWay) then
    begin

      if FGrid.EntireSelectedRowCount > 1 then
      begin
        newHash:= FGridHelper.CalculateHashCodeOfSelectedRows;
        if newHash <> FLastSelectedRowsHash then
        begin
          FLastSelectedRowsHash:= newHash;
          FLastSelectedRow := FGrid.Row;
          FLastSelectedRowsCount := FGrid.EntireSelectedRowCount;
          if Assigned(Self.Parent) then
            InvokeChildsRefresh;
        end;
      end
      else begin
        if (FGrid.EntireSelectedRowCount = 0) then
        begin
          FLastSelectedRow:= -1;
          FLastSelectedRowsCount:= 0;
          if Assigned(Self.Parent) then
            InvokeChildsRefresh;
        end
        else
        begin
          if FGrid.Row <> FLastSelectedRow then
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
  end;
end;

procedure TUramakiKGridPlate.DoSelectAll(Sender: TObject);
begin
  FTriggerSelectionChanges := false;
  try
    FGrid.SelectAll;
    FGrid.Invalidate;
  finally
    FTriggerSelectionChanges:= true;
    if AutomaticChildsUpdateMode = cuOnChangeSelection then
      UpdateChildsIfNeeded(true);
  end;
end;

procedure TUramakiKGridPlate.DoAutoAdjustColumns(Sender: TObject);
begin
  TWaitCursor.ShowWaitCursor('TUramakiKGridPlate.DoAutoAdjustColumns');
  try
    mKGridUtils.AutoSizeColumns(FGrid);
  finally
    TWaitCursor.UndoWaitCursor('TUramakiKGridPlate.DoAutoAdjustColumns');
  end;
end;

function TUramakiKGridPlate.GetUramakiGridHelper: IUramakiGridHelper;
begin
  Result := FUramakiGridHelper;
end;

function TUramakiKGridPlate.GetGridHelper: TmAbstractGridHelper;
begin
  Result := FGridHelper;
end;

procedure TUramakiKGridPlate.ConvertSelectionToUramakiRoll(aUramakiRoll: TUramakiRoll; aDoFillRollFromDatasetRow: TDoFillRollFromDatasetRow);
var
  i, k : integer;
  rows : TIntegerList;
begin
  rows := TIntegerList.Create;
  try
    FGrid.LockUpdate;
    try
      FGridHelper.GetSelectedRows(rows);
      for i := 0 to rows.Count -1 do
      begin
        FCurrentRow:= rows.Items[i];
        aDoFillRollFromDatasetRow(aUramakiRoll, GetValueFromDatasetRow);
      end;
    finally
      FGrid.UnlockUpdate;
    end;
  finally
    rows.Free;
  end;
end;

procedure TUramakiKGridPlate.DoProcessRefreshChilds;
begin
  FTriggerSelectionChanges := false;
  try
    EngineMediator.PleaseRefreshMyChilds(Self);
  finally
    FTriggerSelectionChanges:= true;
  end;
end;

procedure TUramakiKGridPlate.GetSelectedItems(const aKeyFieldName: string; aList: TList);
var
  list : TIntegerList;
  i, k : integer;
  tmpDatum : IVDDatum;
  tmpKey : variant;
begin
  (FGrid as TKGrid).LockUpdate;
  try
    {$IFDEF DEBUG}
    logger.Debug('FGrid.Row ' + IntToStr(FGrid.Row));
    logger.Debug('FGrid.EntireSelectedRowCount ' + IntToStr(FGrid.EntireSelectedRowCount));
    logger.Debug('FGrid.SelectionCount ' + IntToStr(FGrid.SelectionCount));
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
  finally
    (FGrid as TKGrid).UnlockUpdate;
  end;
end;

//procedure TUramakiKGridPlate.OnSelectEditor(Sender: TObject; Column: TColumn; var Editor: TWinControl);
//begin
//
//end;
//
//procedure TUramakiKGridPlate.OnCellClick(Column: TColumn);
//begin
//
//end;

procedure TUramakiKGridPlate.SelectItems(const aDataProvider: IVDDataProvider; const aKeyValues: TStringList);
begin

end;

procedure TUramakiKGridPlate.OnExecuteFilter(Sender: TObject);
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

procedure TUramakiKGridPlate.SetupDataStructures;
begin
  FGridHelper.InitGrid;
end;

procedure TUramakiKGridPlate.SetDisplayLabelOfField(const aFieldName, aDisplayLabel: String);
var
  curField : TmField;
begin
  curField := FGridHelper.GetField(aFieldName);
  if Assigned(curField) then
    curField.DisplayLabel:= aDisplayLabel;
end;

constructor TUramakiKGridPlate.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FLastSelectedRow:= -1;
  FLastSelectedRowsCount:= 0;
  FLastSelectedRowsHash := '';

  FGrid := TKGrid.Create(Self);
  FGrid.Parent := Self;
  FGrid.Align:= alClient;
  //FGrid.SummaryPanel := FSummaryPanel;

  FProvider := TReadOnlyVirtualDatasetProvider.Create;
  // FGrid.DataProvider:= FProvider;
  //Self.FDataset.OnFilter:= Self.OnFilterDataset;

  FGridHelper:= TmKGridHelper.Create(FGrid, FProvider.FormulaFields);
  FGridHelper.Provider := FProvider;
  FGridHelper.SummaryPanel := FSummaryPanel;
  FUramakiGridHelper := TUramakiGridHelper.Create(Self, FGridHelper);
  FGridHelper.SetupGrid;
  FGridHelper.OnGridFiltered := OnGridFiltered;
  FGrid.OnSelectionChanged:= OnSelectionChanged;

  FRunningDoUpdateChilds := false;
  Self.AutomaticChildsUpdateMode:= cuOnChangeSelection;
  FTriggerSelectionChanges:= true;
end;

destructor TUramakiKGridPlate.Destroy;
begin
  FGridHelper.Free;
  FUramakiGridHelper.Free;
  FProvider.Free;

  inherited Destroy;
end;

procedure TUramakiKGridPlate.DisableControls;
begin
  FGrid.LockUpdate;
end;

procedure TUramakiKGridPlate.EnableControls;
begin
  FGrid.UnlockUpdate;
  FGrid.Invalidate;
end;

procedure TUramakiKGridPlate.RefreshDataset;
begin
  FLastSelectedRow:= -1;
  FLastSelectedRowsCount:= 0;
  FLastSelectedRowsHash := '';
  FGridHelper.RefreshDataProvider;
end;

procedure TUramakiKGridPlate.Clear;
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
  logger := logManager.AddLog('UramakiKGridPlate');
{$ENDIF}
end.
