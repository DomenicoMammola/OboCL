unit UramakiKGridPlate;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface
uses
  Classes, Controls, Grids,
  kgrids,
  mQuickReadOnlyVirtualDataSetProvider, mKGridHelper,
  mGridHelper, mDataProviderInterfaces, UramakiBase,
  UramakiBaseGridPlate;

type

  { TUramakiKGridPlate }

  TUramakiKGridPlate = class(TUramakiBaseGridPlate)
  strict private
    FLastSelectedRow : longint;
    FLastSelectedRowsCount : integer;
    FLastSelectedRowsHash : string;
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
    procedure SetAutomaticChildsUpdateMode(AValue: TUramakiGridChildsAutomaticUpdateMode); override;
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
  mKGridUtils, mWaitCursor, mFilter;

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
  UpdateChildsIfNeeded(false);
end;

procedure TUramakiKGridPlate.SetAutomaticChildsUpdateMode(AValue: TUramakiGridChildsAutomaticUpdateMode);
begin
  FAutomaticChildsUpdateMode:=AValue;
  if FAutomaticChildsUpdateMode = cuOnChangeSelection then
  begin
    FGrid.OnSelectionChanged:= Self.OnSelectionChanged();
  end
  else
  begin
    FGrid.OnSelectionChanged:= nil;
  end;
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
  FGrid.OnSelectionChanged:= nil;
  try
    FGrid.SelectAll;
  finally
    if AutomaticChildsUpdateMode = cuOnChangeSelection then
    begin
      FGrid.OnSelectionChanged:= Self.OnSelectionChanged();
      UpdateChildsIfNeeded(true);
    end;
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
begin
  if FGrid.UpdateUnlocked then
    exit;

  if (FGrid.EntireSelectedRowCount > 1) then
  begin
    FGrid.LockUpdate;
    try
      for i := 0 to FGrid.SelectionCount -1 do
      begin
        for k := FGrid.Selections[i].Row1 to FGrid.Selections[i].Row2 do
        begin
          FCurrentRow:= k - FGrid.FixedRows;
          aDoFillRollFromDatasetRow(aUramakiRoll, GetValueFromDatasetRow);
        end;
      end;
    finally
      FGrid.UnlockUpdate;
    end;
  end
  else
  begin
    FCurrentRow := FGrid.Row - FGrid.FixedRows;
    aDoFillRollFromDatasetRow(aUramakiRoll, GetValueFromDatasetRow);
  end;
end;

procedure TUramakiKGridPlate.DoProcessRefreshChilds;
begin
  FGrid.OnSelectionChanged:= nil;
  try
    EngineMediator.PleaseRefreshMyChilds(Self);
  finally
    if AutomaticChildsUpdateMode = cuOnChangeSelection then
    begin
      FGrid.OnSelectionChanged:= Self.OnSelectionChanged();
    end;
  end;
end;

procedure TUramakiKGridPlate.GetSelectedItems(const aKeyFieldName: string; aList: TList);
var
  i, k : integer;
  tmpDatum : IVDDatum;
  tmpKey : variant;
begin
  (FGrid as TKGrid).LockUpdate;
  try
    if FGrid.EntireSelectedRowCount = 1 then
    begin
      FProvider.GetFieldValue(aKeyFieldName, FGrid.Row - FGrid.FixedRows, tmpKey);
      tmpDatum := GetDataProvider.FindDatumByStringKey(VarToStr(tmpKey));
      if Assigned(tmpDatum) then
        aList.Add(tmpDatum.AsObject);
    end
    else
    begin
      FGrid.OnSelectionChanged:= nil;
      try
        for i := 0 to FGrid.SelectionCount - 1 do
        begin
          for k := FGrid.Selections[i].Row1 to FGrid.Selections[i].Row2 do
          begin
            FProvider.GetFieldValue(aKeyFieldName, k - FGrid.FixedRows, tmpKey);
            tmpDatum := GetDataProvider.FindDatumByStringKey(VarToStr(tmpKey));
            if Assigned(tmpDatum) then
              aList.Add(tmpDatum.AsObject);
          end;
        end;
      finally
        if FAutomaticChildsUpdateMode = cuOnChangeSelection then
        begin
          FGrid.OnSelectionChanged:= Self.OnSelectionChanged();
        end;
      end;
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
  //FGrid.OnSelectionChanged:= OnSelectionChanged;

  FRunningDoUpdateChilds := false;
  Self.AutomaticChildsUpdateMode:= cuOnChangeSelection;
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
end;

procedure TUramakiKGridPlate.RefreshDataset;
begin
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

end.
