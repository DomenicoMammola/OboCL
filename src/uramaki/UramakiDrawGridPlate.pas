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
  Classes, Controls, Grids, SysUtils,
  Grids,
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
  protected
    FGrid: TDrawGrid;
    FGridFiltersPanel: TUramakiGridFiltersPanel;
    FProvider : TReadOnlyVirtualDatasetProvider;
    FGridHelper : TmDrawGridHelper;
    FUramakiGridHelper : TUramakiGridHelper;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure DisableControls; override;
    procedure EnableControls; override;
    procedure RefreshDataset; override;
    procedure Clear; override;
  end;

implementation

{ TUramakiDrawGridPlate }

constructor TUramakiDrawGridPlate.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FGridFiltersPanel := TUramakiGridFiltersPanel.Create;
  FGridFiltersPanel.LinkToPlate(Self);

  FGrid := TDrawGrid.Create(Self);
  FGrid.Parent := Self;
  FGrid.Align:= alClient;

  FProvider := TReadOnlyVirtualDatasetProvider.Create;
  FGridHelper:= TmDrawGridHelper.Create(FGrid, FProvider.FormulaFields);
  FGridHelper.Provider := FProvider;
  FGridHelper.SummaryPanel := FSummaryPanel;
  FGridHelper.FiltersPanel := FGridFiltersPanel;;
  FUramakiGridHelper := TUramakiGridHelper.Create(Self, FGridHelper);
  FGridHelper.SetupGrid;
  FGridHelper.OnGridFiltered := OnGridFiltered;
  FGridHelper.OnGridSorted:= OnDataSorted;
  FGrid.OnSelectionChanged:= OnSelectionChanged;

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
  FLastSelectedRow := -1;
  FLastSelectedRowsCount:= 0;
  FLastSelectedRowsHash := '';
  InvokeChildsClear;
end;

end.
