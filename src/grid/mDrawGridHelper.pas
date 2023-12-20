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

  { TmDrawGrid }

  // https://wiki.freepascal.org/Grids_Reference_Page
  TmDrawGrid = class(TCustomDrawGrid)
  private
    procedure DrawHeaderCell(aCol, aRow: Integer; aRect: TRect; aState:TGridDrawState; const aText: String);
    procedure DrawSingleCell(aCol, aRow: Integer; aRect: TRect; aState:TGridDrawState; const aText: String; const aAlignment: TAlignment);
  public
    constructor Create(AOwner: TComponent); override;
    property AlternateColor;
    property RangeSelectMode;
  end;

  { TmDrawGridCursor }

  TmDrawGridCursor = class(ImGridCursor)
  strict private
    FProvider: TmVirtualDatasetDataProvider;
    FCurrentRecNo: longint;
  public
    constructor Create(aProvider: TmVirtualDatasetDataProvider);

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
    FCursor: TmDrawGridCursor;
    FFields: TmFields;
    FOwnedCellDecorations: TmCellDecorations;
    FDataAreFiltered: boolean;
    FDataAreSorted: boolean;
    FSummaryManager: TmDrawGridSummaryManager;
    FSummaryPanel: ISummaryPanel;
    FFiltersPanel : IFilterPanel;
    FParser : TKAParser;
    FSortedVisibleCols: TList;

    FColumnsHeaderPopupMenu: TPopupMenu;
    FOriginalPopupMenu: TPopupMenu;
    FMI_EditFilters: TMenuItem;
    FMI_RemoveAllFilters: TMenuItem;
    FMI_Summaries: TMenuItem;

    FCurrentCol: integer;
    FCurrentRow: integer;

    procedure RefreshSummaryPanel(Sender: TObject);
    procedure SetProvider(AValue: TmVirtualDatasetDataProvider);
    procedure CreateFields;
    procedure UpdateFields;
    function ColToField(const aCol: integer): TmField;
    function GetValue(const aCol, aRow: integer): variant;
    function GetValueAsFormattedString(const aCol, aRow: integer; out aOutOfIndex : boolean): string;
    procedure OnDrawGridCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState:TGridDrawState);
    procedure OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BuildHeaderPopupMenu;
    procedure OnFilterValues(Sender: TObject);
    procedure OnEditSummaries(Sender: TObject);
    procedure OnRemoveSummaries(Sender: TObject);
    procedure OnRemoveAllFilters(Sender: TObject);
    procedure OnEditFilters(Sender: TObject);
    procedure OnColumnsHeaderMenuPopup(Sender: TObject);

  public
    constructor Create(aGrid: TmDrawGrid; aFormulaFields: TmFormulaFields); virtual;
    destructor Destroy; override;

    procedure ReadSettings(aSettings : TmGridColumnsSettings);
    procedure ApplySettings(aSettings : TmGridColumnsSettings);
    procedure RefreshDataProvider(const aReloadFields: boolean);
    function GetSummaryManager : ISummaryDatasetManager;
    procedure GetFields(aFields : TmFields);
    function GetDataCursor : ImGridCursor;
    procedure GetColumns(aColumns : TmGridColumns);
    procedure GetSelectedRows(aRows: TIntegerList);
    procedure AutoSizeColumns;

    procedure InitGrid;
    function GetField(const aFieldName : String): TmField;

    procedure SelectAllRows; override;
    procedure SelectRows (const aKeyField : String; const aValues : TStringList); override;
    procedure SetupGrid (const aEnableAutoSizedColumns : boolean = true); override;

    property Provider: TmVirtualDatasetDataProvider read FProvider write SetProvider;
    property SummaryPanel: ISummaryPanel read FSummaryPanel write FSummaryPanel;
    property FiltersPanel : IFilterPanel read FFiltersPanel write FFiltersPanel;
  end;

implementation

uses
  SysUtils, Math, Variants, Types,
  mDataProviderUtility, mMagnificationFactor, mDataFieldsStandardSetup,
  mDataFieldsUtility, mDateTimeUtility

  {$IFDEF DEBUG}, mLog{$ENDIF};

type

  { TNotifyEventShell }

  TNotifyEventShell = class
  public
    event: TNotifyEvent;
    constructor Create(aEvent: TNotifyEvent);
  end;


{$IFDEF DEBUG}
var
  logger : TmLog;
{$ENDIF}

{ TmDrawGrid }

procedure TmDrawGrid.DrawHeaderCell(aCol, aRow: Integer; aRect: TRect; aState:TGridDrawState; const aText: String);
begin
  Self.DrawCellGrid(aCol, aRow, aRect, aState);
  Self.DrawCellText(aCol, aRow, aRect, aState, aText);
end;

procedure TmDrawGrid.DrawSingleCell(aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState; const aText: String; const aAlignment: TAlignment);
var
  ts : TTextStyle;
begin
  ts := Self.Canvas.TextStyle;
  ts.Alignment := aAlignment;
  Self.Canvas.TextStyle := ts;
  Self.DrawCellText(aCol, aRow, aRect, aState, aText);
end;

constructor TmDrawGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;


{ TNotifyEventShell }

constructor TNotifyEventShell.Create(aEvent: TNotifyEvent);
begin
  event := aEvent;
end;



{ TmDrawGridHelper }

procedure TmDrawGridHelper.RefreshSummaryPanel(Sender: TObject);
begin
  if Assigned(FSummaryPanel) then
    mDataProviderUtility.RefreshSummaryPanel(FSummaryManager, FSummaryPanel);
end;

procedure TmDrawGridHelper.BuildHeaderPopupMenu;
var
  tmpMenuItem: TMenuItem;
  i: TmSummaryOperator;
begin

  if not Assigned(FColumnsHeaderPopupMenu) then
  begin
    FColumnsHeaderPopupMenu := TPopupMenu.Create(FGrid);
    tmpMenuItem := TMenuItem.Create(FColumnsHeaderPopupMenu);
    tmpMenuItem.Caption := SFilterValuesMenuCaption;
    tmpMenuItem.OnClick := Self.OnFilterValues;
    FColumnsHeaderPopupMenu.Items.Add(tmpMenuItem);

    FMI_EditFilters := TMenuItem.Create(FColumnsHeaderPopupMenu);
    FMI_EditFilters.Caption := SEditFiltersMenuCaption;
    FMI_EditFilters.OnClick := Self.OnEditFilters;
    FColumnsHeaderPopupMenu.Items.Add(FMI_EditFilters);

    FMI_RemoveAllFilters := TMenuItem.Create(FColumnsHeaderPopupMenu);
    FMI_RemoveAllFilters.Caption := SRemoveFiltersMenuCaption;
    FMI_RemoveAllFilters.OnClick := Self.OnRemoveAllFilters;
    FColumnsHeaderPopupMenu.Items.Add(FMI_RemoveAllFilters);

    tmpMenuItem := TMenuItem.Create(FColumnsHeaderPopupMenu);
    tmpMenuItem.Caption := '-';
    FColumnsHeaderPopupMenu.Items.Add(tmpMenuItem);

    FMI_Summaries := TMenuItem.Create(FColumnsHeaderPopupMenu);
    FMI_Summaries.Caption := SAddSummaryMenuCaption;
    FColumnsHeaderPopupMenu.Items.Add(FMI_Summaries);
    for i := Low(TmSummaryOperator) to High(TmSummaryOperator) do
    begin
      tmpMenuItem := TMenuItem.Create(FColumnsHeaderPopupMenu);
      tmpMenuItem.Caption := TmSummaryOperatorToString(i);
      tmpMenuItem.OnClick := Self.OnEditSummaries;
      tmpMenuItem.Tag := ptrInt(i);
      FMI_Summaries.Add(tmpMenuItem);
    end;

    tmpMenuItem := TMenuItem.Create(FColumnsHeaderPopupMenu);
    tmpMenuItem.Caption := SRemoveSummariesMenuCaption;
    tmpMenuItem.OnClick := Self.OnRemoveSummaries;
    FColumnsHeaderPopupMenu.Items.Add(tmpMenuItem);

    FColumnsHeaderPopupMenu.OnPopup := Self.OnColumnsHeaderMenuPopup;
  end;

end;

procedure TmDrawGridHelper.OnFilterValues(Sender: TObject);
begin

end;

procedure TmDrawGridHelper.OnEditSummaries(Sender: TObject);
begin

end;

procedure TmDrawGridHelper.OnRemoveSummaries(Sender: TObject);
begin

end;

procedure TmDrawGridHelper.OnRemoveAllFilters(Sender: TObject);
begin

end;

procedure TmDrawGridHelper.OnEditFilters(Sender: TObject);
begin

end;

procedure TmDrawGridHelper.OnColumnsHeaderMenuPopup(Sender: TObject);
var
  currentField: TmField;
  currentOperator: TmSummaryOperator;
  tmpDef: TmSummaryDefinition;
  i: integer;
begin
  //  FMI_RemoveAllFilters.Enabled := Self.FilterManager.GetFiltered;
  FMI_EditFilters.Enabled := FMI_RemoveAllFilters.Enabled;
  for i := 0 to FMI_Summaries.Count - 1 do
  begin
    FMI_Summaries.Items[i].Checked := False;
  end;
  if Assigned(FSummaryManager) then
  begin
    if (FCurrentCol >= 0) then
    begin
      currentField := ColToField(FCurrentCol);

      for i := 0 to FMI_Summaries.Count - 1 do
      begin
        currentOperator := TmSummaryOperator(FMI_Summaries.Items[i].Tag);
        tmpDef := FSummaryManager.GetSummaryDefinitions.FindByFieldNameAndOperator(currentField.FieldName, currentOperator);
        FMI_Summaries.Items[i].Checked := Assigned(tmpDef);
      end;
    end;
  end;

end;

procedure TmDrawGridHelper.SetProvider(AValue: TmVirtualDatasetDataProvider);
begin
  if FProvider=AValue then Exit;
  FProvider:=AValue;
  if Assigned(FCursor) then
    FCursor.Free;
  FCursor := TmDrawGridCursor.Create(FProvider);
  FSummaryManager.Provider := FProvider;
end;

procedure TmDrawGridHelper.CreateFields;
var
  i : integer;
begin
  FFields.Clear;
  FSortedVisibleCols.Clear;

  FProvider.FillFields(FFields);
  ApplyStandardSettingsToFields(FFields, '#,##0.00');

  for i := 0 to FFields.Count - 1 do
    if FFields.Get(i).Visible and (not IsSystemField(FFields.Get(i).FieldName)) then
      FSortedVisibleCols.Add(FFields.Get(i));
end;

procedure TmDrawGridHelper.UpdateFields;
var
  i : integer;
  tmpFields : TmFields;
  newField : TmField;
begin
  tmpFields := TmFields.Create;
  try
    FProvider.FillFields(tmpFields);
    ApplyStandardSettingsToFields(tmpFields, '#,##0.00');

    for i := 0 to tmpFields.Count - 1 do
    begin
      if not Assigned(FFields.FieldByName(tmpFields.Get(i).FieldName)) then
      begin
        newField := FFields.Add;
        newField.Assign(tmpFields.Get(i));
        if newField.Visible and (not IsSystemField(newField.FieldName)) then
          FSortedVisibleCols.Add(newField);
      end;
    end;

    for i := FFields.Count - 1 downto 0 do
    begin
      if not Assigned(tmpFields.FieldByName(FFields.Get(i).FieldName)) then
      begin
        FSortedVisibleCols.Remove(FFields.Get(i));
        FFields.Remove(i);
      end;
    end;
  finally
    tmpFields.Free;
  end;
end;

function TmDrawGridHelper.ColToField(const aCol: integer): TmField;
begin
  Result := TmField(FSortedVisibleCols.Items[aCol]);
end;

function TmDrawGridHelper.GetValue(const aCol, aRow: integer): variant;
var
  curField: TmField;
begin
  if (aRow < FProvider.GetRecordCount) and (aCol < FSortedVisibleCols.Count) then
  begin
    curField := ColToField(aCol);
    FProvider.GetFieldValue(curField.FieldName, aRow, Result);
  end
  else
    Result := Null;
end;

function TmDrawGridHelper.GetValueAsFormattedString(const aCol, aRow: integer; out aOutOfIndex: boolean): string;
var
  curField: TmField;
  Value: variant;
  isInt, isFloat, isDate: boolean;
begin
  aOutOfIndex:= false;
  if (aRow < FProvider.GetRecordCount) and (aCol < FSortedVisibleCols.Count) then
  begin
    curField := ColToField(aCol);
    isInt := FieldTypeIsInteger(curField.DataType);
    isFloat := FieldTypeIsFloat(curField.DataType);
    isDate := FieldTypeIsDate(curField.DataType) or FieldTypeIsDateTime(curField.DataType) or FieldTypeIsTime(curField.DataType);

    FProvider.GetFieldValue(curField.FieldName, aRow, Value);
    if (isInt or isFloat or isDate) and (curField.DisplayFormat <> '') and (not VarIsNull(Value)) then
    begin
      if isFloat or isInt then
        Result := FormatFloat(curField.DisplayFormat, Value)
      else if isDate then
        Result := ExtFormatDateTime(curField.DisplayFormat, Value);
    end
    else
      Result := VarToStr(Value);
  end
  else
  begin
    Result := '-';
    aOutOfIndex := true;
  end;
end;

procedure TmDrawGridHelper.OnDrawGridCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  grid: TmDrawGrid;
  curField: TmField;
  isInt, isFloat, outOfBounds: boolean;
  alignment : TAlignment;
begin
  grid := (FGrid as TmDrawGrid);
  if ACol >= Self.FSortedVisibleCols.Count then
    exit;
  curField := Self.ColToField(ACol);

  if not Assigned(curField) then
    exit;
  (Sender as TmDrawGrid).DefaultDrawCell(aCol, aRow, aRect, aState);

  if aRow = 0 then
    (Sender as TmDrawGrid).DrawHeaderCell(aCol, aRow, aRect, aState, curField.DisplayLabel)
  else
  begin
    isInt := FieldTypeIsInteger(curField.DataType);
    isFloat := FieldTypeIsFloat(curField.DataType);
    if isFloat or isInt then
      alignment:= Classes.taRightJustify
    else
      alignment:= Classes.taLeftJustify;
    (Sender as TmDrawGrid).DrawSingleCell(aCol, aRow, aRect, aState, GetValueAsFormattedString(ACol, ARow - (FGrid as TmDrawGrid).FixedRows, outOfBounds), alignment);
  end;
end;

procedure TmDrawGridHelper.OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbRight)  then
  begin
    (FGrid as TmDrawGrid).MouseToCell(X, Y, FCurrentCol, FCurrentRow);
    if FCurrentRow = 0 then
    begin
      BuildHeaderPopupMenu;
      FOriginalPopupMenu := (FGrid as TmDrawGrid).PopupMenu;
      (FGrid as TmDrawGrid).PopupMenu := FColumnsHeaderPopupMenu;
    end
    else
    begin
      if Assigned(FOriginalPopupMenu) then
        (FGrid as TmDrawGrid).PopupMenu := FOriginalPopupMenu
      else if (FGrid as TmDrawGrid).PopupMenu = FColumnsHeaderPopupMenu then
        (FGrid as TmDrawGrid).PopupMenu := nil;
    end;
  end;
end;

constructor TmDrawGridHelper.Create(aGrid: TmDrawGrid; aFormulaFields: TmFormulaFields);
begin
  FDataAreFiltered := False;
  FDataAreSorted := False;

  FOwnedCellDecorations := TmCellDecorations.Create;
  InternalSetup(aGrid, aFormulaFields, FOwnedCellDecorations);
  FGrid := aGrid;
  FProvider := nil;
  FFields := TmFields.Create;
  FSortedVisibleCols := TList.Create;

  (FGrid as TmDrawGrid).AlternateColor:= DefaultGridAlternateColor;
  (FGrid as TmDrawGrid).Flat := True;
  (FGrid as TmDrawGrid).Options := [goRowHighlight, goColSizing, goColMoving, goVertLine, goHorzLine, goFixedHorzLine, goFixedVertLine, goTabs, goDrawFocusSelected, goDblClickAutoSize, goRelaxedRowSelect, goRangeSelect, goRowSelect ];
  (FGrid as TmDrawGrid).RangeSelectMode := rsmMulti;
  (FGrid as TmDrawGrid).DefaultDrawing := true;
  (FGrid as TmDrawGrid).OnDrawCell := OnDrawGridCell;
  (FGrid as TmDrawGrid).OnMouseDown := OnMouseDown;

  (FGrid as TmDrawGrid).DefaultRowHeight:= ScaleForMagnification((FGrid as TmDrawGrid).DefaultRowHeight, true);
  ScaleFontForMagnification((FGrid as TmDrawGrid).Font);

  FSummaryManager := TmDrawGridSummaryManager.Create;
  FSummaryManager.RegisterListener(Self.RefreshSummaryPanel);
end;

destructor TmDrawGridHelper.Destroy;
begin
  FreeAndNil(FCursor);
  FFields.Free;
  FSortedVisibleCols.Free;
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
  (FGrid as TmDrawGrid).BeginUpdate;
  try
//    (FGrid as TmDrawGrid).ClearSortMode;
    if FFields.Count = 0 then
      CreateFields
    else if aReloadFields then
      UpdateFields;
    (FGrid as TmDrawGrid).FixedRows := 1;
    (FGrid as TmDrawGrid).FixedCols := 0;

    {$IFDEF DEBUG}
    logger.Debug('Actual ColCount=' + IntToStr((FGrid as TmDrawGrid).ColCount));
    logger.Debug('New ColCount=' + IntToStr(FSortedVisibleCols.Count));
    {$ENDIF}
    (FGrid as TmDrawGrid).ColCount := FSortedVisibleCols.Count;

    FDataAreSorted := False;
    FProvider.Refresh(False, FDataAreFiltered);
    (FGrid as TmDrawGrid).RowCount := max(2, FProvider.GetRecordCount + (FGrid as TmDrawGrid).FixedRows);

    FSummaryManager.RefreshSummaries;
    (FGrid as TmDrawGrid).ClearSelections;
  finally
    (FGrid as TmDrawGrid).EndUpdate();
  end;
  (FGrid as TmDrawGrid).Invalidate;
end;

function TmDrawGridHelper.GetSummaryManager: ISummaryDatasetManager;
begin
  Result := FSummaryManager;
end;

procedure TmDrawGridHelper.GetFields(aFields: TmFields);
begin
  aFields.Assign(FFields);
end;

function TmDrawGridHelper.GetDataCursor: ImGridCursor;
begin
  Result := FCursor;
end;

procedure TmDrawGridHelper.GetColumns(aColumns: TmGridColumns);
var
  i: integer;
begin
  aColumns.Clear;
  for i := 0 to FSortedVisibleCols.Count - 1 do
    aColumns.Add.Assign(TmField(FSortedVisibleCols.Items[i]));
end;

procedure TmDrawGridHelper.GetSelectedRows(aRows: TIntegerList);
var
  grid : TmDrawGrid;
  i, k : integer;
begin
  aRows.Clear;

  grid := (FGrid as TmDrawGrid);
  for i := 0 to grid.SelectedRangeCount - 1 do
  begin
    for k := grid.SelectedRange[i].Top to grid.SelectedRange[i].Bottom do
      aRows.Add(k - grid.FixedRows);
  end;
end;

procedure TmDrawGridHelper.AutoSizeColumns;
var
  grid: TmDrawGrid;
  curField: TmField;
  i, k : integer;
  w, ts : TSize;
  outOfBounds : boolean;
  str : string;
  tmpCanvas : TCanvas;
begin
  grid := (FGrid as TmDrawGrid);

  tmpCanvas := GetWorkingCanvas(grid.Canvas);
  try
    tmpCanvas.Font := grid.Font;
    for i := 0 to grid.ColCount -1 do
    begin
      w.Width := 0;

      curField := Self.ColToField(i);
      w := tmpCanvas.TextExtent(curField.DisplayLabel);

      for k := 0 to FProvider.GetRecordCount - 1 do
      begin
        str := GetValueAsFormattedString(i, k, outOfBounds);
        ts := tmpCanvas.TextExtent(str);
        if ts.Width > w.Width then
          w := ts;
      end;
      grid.ColWidths[i]:= w.Width;
    end;
  finally
    if tmpCanvas <> grid.Canvas then
      FreeWorkingCanvas(tmpCanvas);
  end;
end;

procedure TmDrawGridHelper.InitGrid;
begin
  (FGrid as TmDrawGrid).BeginUpdate;
  try
    CreateFields;
    (FGrid as TmDrawGrid).FixedRows := 1;
    (FGrid as TmDrawGrid).FixedCols := 0;

    (FGrid as TmDrawGrid).RowCount := max(2, FProvider.GetRecordCount + (FGrid as TmDrawGrid).FixedRows);
    (FGrid as TmDrawGrid).ColCount := FSortedVisibleCols.Count;
  finally
    (FGrid as TmDrawGrid).EndUpdate(true);
  end;
end;

function TmDrawGridHelper.GetField(const aFieldName: String): TmField;
begin
  Result := FFields.FieldByName(aFieldName);
end;

procedure TmDrawGridHelper.SelectAllRows;
begin

end;

procedure TmDrawGridHelper.SelectRows(const aKeyField: String; const aValues: TStringList);
begin

end;

procedure TmDrawGridHelper.SetupGrid(const aEnableAutoSizedColumns: boolean);
begin
  (FGrid as TmDrawGrid).Align := alClient;
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

constructor TmDrawGridCursor.Create(aProvider: TmVirtualDatasetDataProvider);
begin
  FProvider := aProvider;
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
  Result := (FCurrentRecNo >= FProvider.GetRecordCount);
end;

function TmDrawGridCursor.GetValueByFieldName(const aFieldName: string): variant;
begin
  Result := Null;
  if FCurrentRecNo < FProvider.GetRecordCount then
    FProvider.GetFieldValue(aFieldName, FCurrentRecNo, Result);
end;

{$IFDEF DEBUG}
initialization
  logger := logManager.AddLog('mDrawGridHelper');
{$ENDIF}
end.
