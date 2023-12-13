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
  mIntList, mFilter, mWaitCursor, mFields;

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
begin

end;

procedure TUramakiDrawGridPlate.DoSelectAll(Sender: TObject);
begin

end;

procedure TUramakiDrawGridPlate.DoAutoAdjustColumns(Sender: TObject);
begin

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

end;

procedure TUramakiDrawGridPlate.GetSelectedItems(const aKeyFieldName: string; aList: TList);
begin

end;

procedure TUramakiDrawGridPlate.SelectItems(const aDataProvider: IVDDataProvider; const aKeyValues: TStringList);
begin

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
  //FLastSelectedRow := -1;
  //FLastSelectedRowsCount:= 0;
  //FLastSelectedRowsHash := '';
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

constructor TUramakiDrawGridPlate.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FGridFiltersPanel := TUramakiGridFiltersPanel.Create;
  FGridFiltersPanel.LinkToPlate(Self);

  FGrid := TmDrawGrid.Create(Self);
  FGrid.Parent := Self;
  FGrid.Align:= alClient;

  FProvider := TReadOnlyVirtualDatasetProvider.Create;
  FGridHelper:= TmDrawGridHelper.Create(FGrid, FProvider.FormulaFields);
  FGridHelper.Provider := FProvider;
  FGridHelper.SummaryPanel := FSummaryPanel;
  FGridHelper.FiltersPanel := FGridFiltersPanel;;
  FUramakiGridHelper := TUramakiGridHelper.Create(Self, FGridHelper);
  FGridHelper.SetupGrid;
//  FGridHelper.OnGridFiltered := OnGridFiltered;
//  FGridHelper.OnGridSorted:= OnDataSorted;
//  FGrid.OnSelectionChanged:= OnSelectionChanged;

  Self.AutomaticChildsUpdateMode:= cuOnChangeSelection;
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
//  FLastSelectedRow:= -1;
//  FLastSelectedRowsCount:= 0;
//  FLastSelectedRowsHash := '';
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
  //FLastSelectedRow := -1;
  //FLastSelectedRowsCount:= 0;
  //FLastSelectedRowsHash := '';
  InvokeChildsClear;
end;

end.
