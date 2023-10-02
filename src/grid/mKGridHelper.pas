// This is part of the Obo Component Library

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

// This software is distributed without any warranty.

// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mKGridHelper;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, Contnrs, Menus, Controls,
  kgrids,
  mGrids, mGridHelper, KAParser, mVirtualDatasetFormulas, mCellDecorations,
  mDataProviderInterfaces, mVirtualDataSet, mFields, mGridColumnSettings,
  mVirtualDataSetProvider, mSummary;

type

  { TmKGridCursor }

  TmKGridCursor = class(ImGridCursor)
  strict private
    FProvider: TmVirtualDatasetDataProvider;
    FIndex: longint;
  public
    constructor Create(aProvider: TmVirtualDatasetDataProvider);

    procedure StartBrowsing;
    procedure EndBrowsing;
    procedure First;
    procedure Next;
    function EOF: boolean;
    function GetValueByFieldName(const aFieldName: string): variant;
  end;

  { TmKGridSummaryManager }

  TmKGridSummaryManager = class({$IFNDEF FPC}TJavaInterfacedObject, {$ENDIF}ISummaryDatasetManager)
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


  { TmKGridHelper }

  TmKGridHelper = class(TmAbstractGridHelper, ImGrid)
  strict private
    FProvider: TmVirtualDatasetDataProvider;
    FCursor: TmKGridCursor;
    FFields: TmFields;
    FSortedCols: TList;
    FSummaryManager: TmKGridSummaryManager;
    FSummaryPanel: ISummaryPanel;
    FOwnedCellDecorations: TmCellDecorations;
    FDataAreFiltered: boolean;
    FDataAreSorted: boolean;
    // menu
    FColumnsHeaderPopupMenu: TPopupMenu;
    FOriginalPopupMenu: TPopupMenu;
    FMI_EditFilters: TMenuItem;
    FMI_RemoveAllFilters: TMenuItem;
    FMI_Summaries: TMenuItem;
    FCurrentCol: integer;
    FCurrentRow: integer;

    procedure CreateFields;
    procedure SetProvider(AValue: TmVirtualDatasetDataProvider);
    procedure OnDrawGridCell(Sender: TObject; ACol, ARow: integer; R: TRect; State: TKGridDrawState);
    procedure OnMeasureCell(Sender: TObject; ACol, ARow: integer; R: TRect; State: TKGridDrawState; Priority: TKGridMeasureCellPriority; var Extent: TPoint);
    procedure OnKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure OnMoveColumns(Sender: TObject; Index1, Index2: integer);
    procedure OnCustomSort(Sender: TObject; ByIndex: integer; SortMode: TKGridSortMode; var Sorted: boolean);
    function GetField(const aCol: integer): TmField;
    function GetValue(const aCol, aRow: integer): variant;
    procedure RefreshSummaryPanel(Sender: TObject);
    procedure BuildHeaderPopupMenu;
    procedure OnColumnsHeaderMenuPopup(Sender: TObject);
    procedure OnFilterValues(Sender: TObject);
    procedure OnEditSummaries(Sender: TObject);
    procedure OnRemoveSummaries(Sender: TObject);
    procedure OnRemoveAllFilters(Sender: TObject);
    procedure OnEditFilters(Sender: TObject);
    procedure OnClickCell(Sender: TObject; ACol, ARow: integer);
    procedure OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure OnMouseEnterCell(Sender: TObject; ACol, ARow: integer);
    procedure OnMouseLeaveCell(Sender: TObject; ACol, ARow: integer);
  public
    constructor Create(aGrid: TKGrid; aFormulaFields: TmFormulaFields); virtual;
    destructor Destroy; override;

    procedure SetupGrid(const aEnableAutoSizedColumns: boolean = True); override;
    procedure SelectAllRows; override;
    procedure SelectRows(const aKeyField: string; const aValues: TStringList); override;

    procedure InitGrid;

    procedure ReadSettings(aSettings: TmGridColumnsSettings);
    procedure ApplySettings(aSettings: TmGridColumnsSettings);
    procedure RefreshDataProvider;
    function GetSummaryManager: ISummaryDatasetManager;
    procedure GetFields(aFields: TmFields);
    function GetDataCursor: ImGridCursor;
    procedure GetColumns(aColumns: TmGridColumns);

    property Provider: TmVirtualDatasetDataProvider read FProvider write SetProvider;
    property SummaryPanel: ISummaryPanel read FSummaryPanel write FSummaryPanel;
  end;

implementation

uses
  SysUtils, Graphics, Variants, LCLType,
  kgraphics,
  mDataProviderFieldDefs, mDataFieldsStandardSetup, mGraphicsUtility, mDataProviderUtility, mSortConditions,
  mGridFilterValuesDlg, mFilter, mFilterOperators, mWaitCursor, mDataFieldsUtility, mGridFiltersEditDlg;

type

  { TNotifyEventShell }

  TNotifyEventShell = class
  public
    event: TNotifyEvent;
    constructor Create(aEvent: TNotifyEvent);
  end;

{ TNotifyEventShell }

constructor TNotifyEventShell.Create(aEvent: TNotifyEvent);
begin
  event := aEvent;
end;


{ TmKGridSummaryManager }

constructor TmKGridSummaryManager.Create;
begin
  FListeners := TObjectList.Create(True);
end;

destructor TmKGridSummaryManager.Destroy;
begin
  FListeners.Free;
end;

function TmKGridSummaryManager.GetSummaryDefinitions: TmSummaryDefinitions;
begin
  Result := FProvider.SummaryDefinitions;
end;

function TmKGridSummaryManager.GetSummaryValues: TmSummaryValues;
begin
  Result := FProvider.SummaryValues;
end;

procedure TmKGridSummaryManager.RefreshSummaries;
begin
  FProvider.CalculateSummaries;
  Self.NotifyChanges;
end;

procedure TmKGridSummaryManager.NotifyChanges;
var
  i: integer;
begin
  for i := 0 to FListeners.Count - 1 do
    (FListeners.Items[i] as TNotifyEventShell).event(Self);
end;

procedure TmKGridSummaryManager.RegisterListener(aOnRefresh: TNotifyEvent);
begin
  FListeners.Add(TNotifyEventShell.Create(aOnRefresh));
end;

{ TmKGridCursor }

constructor TmKGridCursor.Create(aProvider: TmVirtualDatasetDataProvider);
begin
  FProvider := aProvider;
  FIndex := 0;
end;

procedure TmKGridCursor.StartBrowsing;
begin

end;

procedure TmKGridCursor.EndBrowsing;
begin

end;

procedure TmKGridCursor.First;
begin
  FIndex := 0;
end;

procedure TmKGridCursor.Next;
begin
  Inc(FIndex);
end;

function TmKGridCursor.EOF: boolean;
begin
  Result := FIndex >= FProvider.GetRecordCount;
end;

function TmKGridCursor.GetValueByFieldName(const aFieldName: string): variant;
begin
  Result := Null;
  if FIndex < FProvider.GetRecordCount then
    FProvider.GetFieldValue(aFieldName, FIndex, Result);
end;

{ TmKGridHelper }

procedure TmKGridHelper.CreateFields;
var
  i: integer;
begin
  FFields.Clear;
  FSortedCols.Clear;
  FProvider.FillFields(FFields);
  ApplyStandardSettingsToFields(FFields, '#,##0.00');

  for i := 0 to FFields.Count - 1 do
    FSortedCols.Add(FFields.Get(i));
end;

procedure TmKGridHelper.SetProvider(AValue: TmVirtualDatasetDataProvider);
begin
  if FProvider = AValue then Exit;
  FProvider := AValue;
  if Assigned(FCursor) then
    FCursor.Free;
  FCursor := TmKGridCursor.Create(FProvider);
  FSummaryManager.Provider := FProvider;
end;

procedure TmKGridHelper.OnDrawGridCell(Sender: TObject; ACol, ARow: integer; R: TRect; State: TKGridDrawState);
var
  grid: TKGrid;
  curField: TmField;
begin
  grid := (FGrid as TKGrid);
  curField := Self.GetField(ACol);

  // https://forum.lazarus.freepascal.org/index.php/topic,44833.msg315562.html#msg315562

  //grid.Cell[ACol, ARow].ApplyDrawProperties;

  if grid.CellPainter.Canvas.Brush.Style = bsClear then grid.CellPainter.Canvas.Brush.Style := bsSolid;
  if (ARow < grid.FixedRows) and (ACol >= grid.FixedCols) then
  begin
    grid.CellPainter.HAlign := halCenter;
    grid.CellPainter.VAlign := valCenter;
    grid.CellPainter.BackColor := grid.Colors.FixedCellBkGnd;
    grid.CellPainter.Canvas.Brush.Color := grid.Colors.FixedCellBkGnd;
  end
  else
  begin
    if FieldTypeIsFloat(curField.DataType) or FieldTypeIsInteger(curField.DataType) then
      grid.CellPainter.HAlign := halRight;

    if State * [gdFixed, gdSelected] = [] then
    begin
      grid.CellPainter.BackColor := LighterColor(grid.Colors.FixedCellBkGnd, 15);
      grid.CellPainter.Canvas.Brush.Color := grid.CellPainter.BackColor;
    end;
  end;

  grid.CellPainter.DrawDefaultCellBackground;
  if ARow = 0 then
  begin
    grid.CellPainter.Text := curField.DisplayLabel;
  end
  else
    grid.CellPainter.Text := VarToStr(GetValue(ACol, ARow - (FGrid as TKGrid).FixedRows));
  grid.CellPainter.DrawCellCommon;
end;

procedure TmKGridHelper.OnMeasureCell(Sender: TObject; ACol, ARow: integer; R: TRect; State: TKGridDrawState; Priority: TKGridMeasureCellPriority; var Extent: TPoint);
var
  sz: TPoint;
begin
  if ARow = 0 then
    GetTextExtend(GetField(ACol).DisplayLabel, FGrid.Font, Extent)
  else
    GetTextExtend(VarToStr(GetValue(ACol, ARow - (FGrid as TKGrid).FixedRows)), FGrid.Font, Extent);
  Extent.X := Extent.X + ((FGrid as TKGrid).CellPainter.HPadding * 2);
  Extent.Y := Extent.Y + ((FGrid as TKGrid).CellPainter.VPadding * 2);
end;

procedure TmKGridHelper.OnKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (Shift = [ssCtrl]) and (Key = VK_C) then
    CopyTextToClipboard(VarToStr(GetValue((FGrid as TKGrid).Col, (FGrid as TKGrid).Row - (FGrid as TKGrid).FixedRows)));
end;

procedure TmKGridHelper.OnMoveColumns(Sender: TObject; Index1, Index2: integer);
var
  tmpField: TmField;
begin
  tmpField := TmField(FSortedCols.Items[Index1]);
  FSortedCols.Items[Index1] := FSortedCols.Items[Index2];
  FSortedCols.Items[Index2] := tmpField;
end;

procedure TmKGridHelper.OnCustomSort(Sender: TObject; ByIndex: integer; SortMode: TKGridSortMode; var Sorted: boolean);
begin
  if SortMode = smNone then
  begin
    FProvider.SortConditions.Clear;
    FProvider.Refresh(False, FDataAreFiltered);
    Sorted := False;
    FDataAreSorted := False;
  end
  else
  begin
    if FProvider.SortConditions.Count = 0 then
      FProvider.SortConditions.Add;

    FProvider.SortConditions.Items[0].FieldName := GetField(ByIndex).FieldName;

    if SortMode = smDown then
      FProvider.SortConditions.Items[0].SortType := stDescending
    else if SortMode = smUp then
      FProvider.SortConditions.Items[0].SortType := stAscending;
    FProvider.Refresh(True, FDataAreFiltered);
    Sorted := True;
    FDataAreSorted := True;
  end;
end;

function TmKGridHelper.GetField(const aCol: integer): TmField;
begin
  Result := TmField(FSortedCols.Items[aCol]);
end;

function TmKGridHelper.GetValue(const aCol, aRow: integer): variant;
var
  curField: TmField;
begin
  if (aRow < FProvider.GetRecordCount) and (aCol < FSortedCols.Count) then
  begin
    curField := GetField(aCol);
    FProvider.GetFieldValue(curField.FieldName, aRow, Result);
  end
  else
    Result := Null;
end;

procedure TmKGridHelper.RefreshSummaryPanel(Sender: TObject);
begin
  if Assigned(FSummaryPanel) then
    mDataProviderUtility.RefreshSummaryPanel(FSummaryManager, FSummaryPanel);
end;

procedure TmKGridHelper.BuildHeaderPopupMenu;
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

procedure TmKGridHelper.OnColumnsHeaderMenuPopup(Sender: TObject);
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
      currentField := GetField(FCurrentCol);

      for i := 0 to FMI_Summaries.Count - 1 do
      begin
        currentOperator := TmSummaryOperator(FMI_Summaries.Items[i].Tag);
        tmpDef := FSummaryManager.GetSummaryDefinitions.FindByFieldNameAndOperator(currentField.FieldName, currentOperator);
        FMI_Summaries.Items[i].Checked := Assigned(tmpDef);
      end;
    end;
  end;
end;

procedure TmKGridHelper.OnFilterValues(Sender: TObject);
var
  dlg: TFilterValuesDlg;
  values: TStringList;
  checkedValues: TStringList;
  i: integer;
  tmpFilter: TmFilter;
  tmpVariant: variant;
  currentField: TmField;
begin
  if FCurrentCol >= 0 then
  begin
    currentField := GetField(FCurrentCol);
    values := TStringList.Create;
    dlg := TFilterValuesDlg.Create(FGrid);
    try
      try
        TWaitCursor.ShowWaitCursor('TmKGridHelper.OnFilterValues');
        FProvider.GetUniqueStringValuesForField(currentField.FieldName, values);
        dlg.Init(values);
      finally
        TWaitCursor.UndoWaitCursor('TmKGridHelper.OnFilterValues');
      end;
      if dlg.ShowModal = mrOk then
      begin
        checkedValues := TStringList.Create;
        try
          dlg.GetCheckedValues(checkedValues);
          FProvider.FilterConditions.ClearForField(currentField.FieldName);
          if checkedValues.Count > 0 then
          begin
            (FGrid as TKGrid).ClearSelections;
            try
              TWaitCursor.ShowWaitCursor('TmKGridHelper.OnFilterValues');
              tmpFilter := FProvider.FilterConditions.Add;
              tmpFilter.FieldName := currentField.FieldName;
              tmpFilter.FilterOperator := foIn;
              tmpVariant := variants.VarArrayCreate([0, checkedValues.Count - 1], varOleStr);
              for i := 0 to checkedValues.Count - 1 do
                VarArrayPut(tmpVariant, checkedValues.Strings[i], [i]);
              tmpFilter.Value := tmpVariant;
              (FGrid as TKGrid).LockUpdate;
              try
                FDataAreFiltered := True;
                FProvider.Refresh(FDataAreSorted, FDataAreFiltered);
                (FGrid as TKGrid).RowCount := FProvider.GetRecordCount + (FGrid as TKGrid).FixedRows;
              finally
                (FGrid as TKGrid).UnlockUpdate;
              end;
              FSummaryManager.RefreshSummaries;
            finally
              TWaitCursor.UndoWaitCursor('TmKGridHelper.OnFilterValues');
            end;
          end;
        finally
          checkedValues.Free;
        end;
      end;
    finally
      dlg.Free;
      values.Free;
    end;
  end;
end;

procedure TmKGridHelper.OnEditSummaries(Sender: TObject);
var
  CurrentField: TmField;
  currentOperator: TmSummaryOperator;
  tmpDef: TmSummaryDefinition;
begin
  if Assigned(FSummaryManager) then
  begin
    if FCurrentCol >= 0 then
    begin
      CurrentField := GetField(FCurrentCol);
      currentOperator := TmSummaryOperator((Sender as TMenuItem).Tag);

      tmpDef := FSummaryManager.GetSummaryDefinitions.FindByFieldNameAndOperator(CurrentField.FieldName, currentOperator);
      if Assigned(tmpDef) then
      begin
        FSummaryManager.GetSummaryDefinitions.Remove(tmpDef);
      end
      else
      begin
        tmpDef := FSummaryManager.GetSummaryDefinitions.Add;
        tmpDef.FieldName := CurrentField.FieldName;
        tmpDef.FieldType := CurrentField.DataType;
        tmpDef.DisplayLabel.Value := CurrentField.DisplayLabel;
        tmpDef.SummaryOperator := currentOperator;
      end;
      FSummaryManager.RefreshSummaries;
    end;
  end;
end;

procedure TmKGridHelper.OnRemoveSummaries(Sender: TObject);
begin
  FSummaryManager.GetSummaryDefinitions.Clear;
  FSummaryManager.RefreshSummaries;
end;

procedure TmKGridHelper.OnRemoveAllFilters(Sender: TObject);
begin
  if FDataAreFiltered then
  begin
    (FGrid as TKGrid).LockUpdate;
    try
      FDataAreFiltered := False;
      FProvider.FilterConditions.Clear;
      FProvider.Refresh(FDataAreSorted, False);
      (FGrid as TKGrid).RowCount := FProvider.GetRecordCount + (FGrid as TKGrid).FixedRows;
    finally
      (FGrid as TKGrid).UnlockUpdate;
    end;
    FSummaryManager.RefreshSummaries;
  end;
end;

procedure TmKGridHelper.OnEditFilters(Sender: TObject);
var
  dlg: TFiltersEditDlg;
  removedFilters: TStringList;
  i: integer;
begin

  if not FDataAreFiltered then
    exit;

  dlg := TFiltersEditDlg.Create(FGrid);
  try
    dlg.Init(FProvider.FilterConditions, FFields);
    if dlg.ShowModal = mrOk then
    begin
      removedFilters:= TStringList.Create;
      try
        dlg.GetRemovedFilterConditions(removedFilters);
        if removedFilters.Count > 0 then
        begin
          // it is necessary to clear old selection
          (FGrid as TKGrid).ClearSelections;
          for i := 0 to removedFilters.Count - 1 do
            FProvider.FilterConditions.ClearForField(removedFilters[i]);

          (FGrid as TKGrid).LockUpdate;
          try
            FDataAreFiltered := (FProvider.FilterConditions.Count > 0);
            FProvider.Refresh(FDataAreSorted, FDataAreFiltered);
            (FGrid as TKGrid).RowCount := FProvider.GetRecordCount + (FGrid as TKGrid).FixedRows;
          finally
            (FGrid as TKGrid).UnlockUpdate;
          end;
          FSummaryManager.RefreshSummaries;
        end;
      finally
        removedFilters.Free;
      end;
    end;
  finally
    dlg.Free;
  end;
end;

procedure TmKGridHelper.OnClickCell(Sender: TObject; ACol, ARow: integer);
begin
end;

procedure TmKGridHelper.OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if Button = mbRight then
  begin
    //hitPos := Point(X, Y);
    //cellFound := (FGrid as TKGrid).PointToCell(FHitPos, False, icNone, FHitCell.Col, FHitCell.Row, BaseCol, BaseRow);
    //if CellFound then

    if (FProvider.GetRecordCount > 0) and (FCurrentRow = 0) then
    begin
      FOriginalPopupMenu := (FGrid as TKGrid).PopupMenu;
      (FGrid as TKGrid).PopupMenu := FColumnsHeaderPopupMenu;
      //tmpCol := 0;
      //tmpRow := 0;
      //Self.MouseToCell(X, Y, tmpCol, tmpRow);
      //FCurrentGridCol := tmpCol;
    end
    else
    begin
      if Assigned(FOriginalPopupMenu) then
        (FGrid as TKGrid).PopupMenu := FOriginalPopupMenu
      else if (FGrid as TKGrid).PopupMenu = FColumnsHeaderPopupMenu then
        (FGrid as TKGrid).PopupMenu := nil;
    end;
  end;
end;

procedure TmKGridHelper.OnMouseEnterCell(Sender: TObject; ACol, ARow: integer);
begin
  FCurrentCol := ACol;
  FCurrentRow := ARow;
end;

procedure TmKGridHelper.OnMouseLeaveCell(Sender: TObject; ACol, ARow: integer);
begin
  FCurrentRow := -1;
  FCurrentCol := -1;
end;

constructor TmKGridHelper.Create(aGrid: TKGrid; aFormulaFields: TmFormulaFields);
begin
  FDataAreFiltered := False;
  FDataAreSorted := False;
  FOwnedCellDecorations := TmCellDecorations.Create;
  InternalSetup(aGrid, aFormulaFields, FOwnedCellDecorations);
  FGrid := aGrid;
  FProvider := nil;
  FFields := TmFields.Create;
  FSortedCols := TList.Create;
  (FGrid as TKGrid).Options := [goVirtualGrid, goColSizing, goColMoving, goRowSorting, goDrawFocusSelected, goRowSelect, goVertLine, goHeader, goHorzLine, goRangeSelect, goFixedVertLine, goMouseOverCells];
  (FGrid as TKGrid).OptionsEx := [gxMouseWheelScroll];
  (FGrid as TKGrid).RangeSelectStyle := rsMultiSelect;
  (FGrid as TKGrid).OnDrawCell := Self.OnDrawGridCell;
  (FGrid as TKGrid).OnMeasureCell := Self.OnMeasureCell;
  (FGrid as TKGrid).OnKeyDown := Self.OnKeyDown;
  (FGrid as TKGrid).OnExchangeCols := Self.OnMoveColumns;
  (FGrid as TKGrid).OnCustomSortRows := Self.OnCustomSort;
  (FGrid as TKGrid).OnMouseClickCell := Self.OnClickCell;
  (FGrid as TKGrid).OnMouseDown := Self.OnMouseDown;
  (FGrid as TKGrid).OnMouseEnterCell := Self.OnMouseEnterCell;
  (FGrid as TKGrid).Flat := True;

  FCurrentCol := -1;
  FCurrentRow := -1;

  FSummaryManager := TmKGridSummaryManager.Create;
  FSummaryManager.RegisterListener(Self.RefreshSummaryPanel);
end;

destructor TmKGridHelper.Destroy;
begin
  FreeAndNil(FCursor);
  FFields.Free;
  FSortedCols.Free;
  FOwnedCellDecorations.Free;
  FSummaryManager.Free;
  inherited Destroy;
end;

procedure TmKGridHelper.SetupGrid(const aEnableAutoSizedColumns: boolean);
begin
  (FGrid as TKGrid).Align := alClient;
  BuildHeaderPopupMenu;
  //  FKGrid.AlternateColor:= DefaultGridAlternateColor;
  //  if aEnableAutoSizedColumns then
  //    FKGrid.Options := [goColMoving, goColSizing, goColSorting, goDoubleBufferedCells, goDrawFocusSelected, goEnterMoves, goHeader, goHeaderAlignment, goRowSelect, goTabs, goVirtualGrid];
  //  else
  //    FKGrid.Options := [dgTitles, dgIndicator, dgColumnResize, dgColumnMove, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit,                    dgDisableDelete, dgDisableInsert, dgMultiselect];
end;

procedure TmKGridHelper.SelectAllRows;
begin
  (FGrid as TKGrid).SelectAll;
end;

procedure TmKGridHelper.SelectRows(const aKeyField: string; const aValues: TStringList);
begin

end;

procedure TmKGridHelper.InitGrid;
var
  curField: TmField;
begin
  (FGrid as TKGrid).LockUpdate;
  try
    CreateFields;
    (FGrid as TKGrid).FixedRows := 1;
    (FGrid as TKGrid).FixedCols := 0;

    (FGrid as TKGrid).RowCount := FProvider.GetRecordCount + (FGrid as TKGrid).FixedRows;
    (FGrid as TKGrid).ColCount := FFields.Count;
  finally
    (FGrid as TKGrid).UnlockUpdate;
  end;
end;

procedure TmKGridHelper.ReadSettings(aSettings: TmGridColumnsSettings);
begin

end;

procedure TmKGridHelper.ApplySettings(aSettings: TmGridColumnsSettings);
begin

end;

procedure TmKGridHelper.RefreshDataProvider;
begin
  (FGrid as TKGrid).LockUpdate;
  try
    (FGrid as TKGrid).ClearSortMode;
    if FFields.Count = 0 then
      CreateFields;
    (FGrid as TKGrid).FixedRows := 1;
    (FGrid as TKGrid).FixedCols := 0;

    (FGrid as TKGrid).RowCount := FProvider.GetRecordCount + (FGrid as TKGrid).FixedRows;
    (FGrid as TKGrid).ColCount := FFields.Count;
    FDataAreSorted := False;
    FProvider.Refresh(False, FDataAreFiltered);
  finally
    (FGrid as TKGrid).UnlockUpdate;
  end;
end;

function TmKGridHelper.GetSummaryManager: ISummaryDatasetManager;
begin
  Result := FSummaryManager;
end;

procedure TmKGridHelper.GetFields(aFields: TmFields);
begin
  aFields.Assign(FFields);
end;

function TmKGridHelper.GetDataCursor: ImGridCursor;
begin
  Result := FCursor;
end;

procedure TmKGridHelper.GetColumns(aColumns: TmGridColumns);
begin

end;

end.
