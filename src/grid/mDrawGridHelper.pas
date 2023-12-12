// This is part of the Obo Component Library

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

// This software is distributed without any warranty.

// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mDrawGridHelper;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, Contnrs, Menus, Controls, Graphics,
  Grids,
  mGrids, mGridHelper, KAParser, mVirtualDatasetFormulas, mCellDecorations,
  mDataProviderInterfaces, mVirtualDataSet, mFields, mGridColumnSettings,
  mVirtualDatasetProvider, mSummary, mIntList, mFilter, mMaps;

type
  { TmDrawGridCursor }

  TmDrawGridCursor = class(ImGridCursor)
  strict private
    FDataProvider: IVDDataProvider;
    FCurrentRecNo: longint;
  public
    constructor Create(aProvider: IVDDataProvider);

    procedure StartBrowsing;
    procedure EndBrowsing;
    procedure First;
    procedure Next;
    function EOF: boolean;
    function GetValueByFieldName(const aFieldName: string): variant;
  end;

  { TmDrawGridSummaryManager }

  TmDrawGridSummaryManager = class({$IFNDEF FPC}TJavaInterfacedObject, {$ENDIF}ISummaryDatasetManager)
  protected
    FProvider: TmVirtualDatasetDataProvider;
    FListeners: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    function GetSummaryDefinitions: TmSummaryDefinitions;
    function GetSummaryValues: TmSummaryValues;
    procedure RefreshSummaries;
    procedure NotifyChanges;
    procedure RegisterListener(aOnRefresh: TNotifyEvent);

    property Provider: TmVirtualDatasetDataProvider read FProvider write FProvider;
  end;

  { TmDrawGridHelper }

  TmDrawGridHelper = class(TmAbstractGridHelper, ImGrid)
  strict private
    FProvider: TmVirtualDatasetDataProvider;
    FCursor: TmKGridCursor;
    FFields: TmFields;
    FOwnedCellDecorations: TmCellDecorations;
    FSummaryManager: TmDrawGridSummaryManager;
    FSummaryPanel: ISummaryPanel;
    FFiltersPanel : IFilterPanel;
    FOwnedCellDecorations: TmCellDecorations;
    FParser : TKAParser;
    procedure RefreshSummaryPanel(Sender: TObject);
    procedure BuildHeaderPopupMenu;
  public
    constructor Create(aGrid: TDrawGrid; aFormulaFields: TmFormulaFields); virtual;
    destructor Destroy; override;

    procedure ReadSettings(aSettings : TmGridColumnsSettings);
    procedure ApplySettings(aSettings : TmGridColumnsSettings);
    procedure RefreshDataProvider(const aReloadFields: boolean);
    function GetSummaryManager : ISummaryDatasetManager;
    procedure GetFields(aFields : TmFields);
    function GetDataCursor : ImGridCursor;
    procedure GetColumns(aColumns : TmGridColumns);

    procedure SelectAllRows; override;
    procedure SelectRows (const aKeyField : String; const aValues : TStringList); override;
    procedure SetupGrid (const aEnableAutoSizedColumns : boolean = true); override;
  end;

implementation

{ TmDrawGridHelper }

procedure TmDrawGridHelper.RefreshSummaryPanel(Sender: TObject);
begin
  if Assigned(FSummaryPanel) then
    mDataProviderUtility.RefreshSummaryPanel(FSummaryManager, FSummaryPanel);
end;

procedure TmDrawGridHelper.BuildHeaderPopupMenu;
begin

end;

constructor TmDrawGridHelper.Create(aGrid: TDrawGrid; aFormulaFields: TmFormulaFields);
begin
  FOwnedCellDecorations := TmCellDecorations.Create;
  InternalSetup(aGrid, aFormulaFields, FOwnedCellDecorations);
  FGrid := aGrid;
  FProvider := nil;

  (FGrid as TDrawGrid).AlternateColor:= DefaultGridAlternateColor;
  (FGrid as TDrawGrid).Flat := True;
  (FGrid as TDrawGrid).Options := [goRowHighlight, goColSizing, goColMoving, goVertLine, goHorzLine, goTabs, goDrawFocusSelected, goDblClickAutoSize, goRelaxedRowSelect];

  (FGrid as TDrawGrid).DefaultRowHeight:= ScaleForMagnification((FGrid as TDrawGrid).DefaultRowHeight, true);
  ScaleFontForMagnification((FGrid as TDrawGrid).Font);

  FSummaryManager := TmDrawGridSummaryManager.Create;
  FSummaryManager.RegisterListener(Self.RefreshSummaryPanel);
end;

destructor TmDrawGridHelper.Destroy;
begin
  FreeAndNil(FCursor);
  FFields.Free;
  FOwnedCellDecorations.Free;
  FSummaryManager.Free;
  FreeAndNil(FParser);
  inherited Destroy;
end;

procedure TmDrawGridHelper.ReadSettings(aSettings: TmGridColumnsSettings);
begin

end;

procedure TmDrawGridHelper.ApplySettings(aSettings: TmGridColumnsSettings);
begin

end;

procedure TmDrawGridHelper.RefreshDataProvider(const aReloadFields: boolean);
begin

end;

function TmDrawGridHelper.GetSummaryManager: ISummaryDatasetManager;
begin

end;

procedure TmDrawGridHelper.GetFields(aFields: TmFields);
begin

end;

function TmDrawGridHelper.GetDataCursor: ImGridCursor;
begin

end;

procedure TmDrawGridHelper.GetColumns(aColumns: TmGridColumns);
begin

end;

procedure TmDrawGridHelper.SelectAllRows;
begin

end;

procedure TmDrawGridHelper.SelectRows(const aKeyField: String; const aValues: TStringList);
begin

end;

procedure TmDrawGridHelper.SetupGrid(const aEnableAutoSizedColumns: boolean);
begin
  (FGrid as TDrawGrid).Align := alClient;
  BuildHeaderPopupMenu;
end;

{ TmDrawGridSummaryManager }

constructor TmDrawGridSummaryManager.Create;
begin
  FListeners := TObjectList.Create(True);
end;

destructor TmDrawGridSummaryManager.Destroy;
begin
  FListeners.Free;
end;

function TmDrawGridSummaryManager.GetSummaryDefinitions: TmSummaryDefinitions;
begin
  Result := FProvider.SummaryDefinitions;
end;

function TmDrawGridSummaryManager.GetSummaryValues: TmSummaryValues;
begin
  Result := FProvider.SummaryValues;
end;

procedure TmDrawGridSummaryManager.RefreshSummaries;
begin
  FProvider.CalculateSummaries;
  Self.NotifyChanges;
end;

procedure TmDrawGridSummaryManager.NotifyChanges;
var
  i: integer;
begin
  for i := 0 to FListeners.Count - 1 do
    (FListeners.Items[i] as TNotifyEventShell).event(Self);
end;

procedure TmDrawGridSummaryManager.RegisterListener(aOnRefresh: TNotifyEvent);
begin
  FListeners.Add(TNotifyEventShell.Create(aOnRefresh));
end;

{ TmDrawGridCursor }

constructor TmDrawGridCursor.Create(aProvider: IVDDataProvider);
begin
  FProvider := aDataProvider;
  FCurrentRecNo := 0;
end;

procedure TmDrawGridCursor.StartBrowsing;
begin
  // nope
end;

procedure TmDrawGridCursor.EndBrowsing;
begin
  // nope
end;

procedure TmDrawGridCursor.First;
begin
  FCurrentRecNo := 0;
end;

procedure TmDrawGridCursor.Next;
begin
  Inc(FCurrentRecNo);
end;

function TmDrawGridCursor.EOF: boolean;
begin
  Result := (FCurrentRecNo >= FProvider.Count);
end;

function TmDrawGridCursor.GetValueByFieldName(const aFieldName: string): variant;
begin
  Result := Null;
  if FIndex < FProvider.GetRecordCount then
    FProvider.GetFieldValue(aFieldName, FIndex, Result);
end;


end.
