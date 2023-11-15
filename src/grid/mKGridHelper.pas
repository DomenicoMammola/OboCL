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
  Classes, Contnrs, Menus, Controls, Graphics,
  kgrids,
  mGrids, mGridHelper, KAParser, mVirtualDatasetFormulas, mCellDecorations,
  mDataProviderInterfaces, mVirtualDataSet, mFields, mGridColumnSettings,
  mVirtualDatasetProvider, mSummary, mIntList, mFilter, mMaps;

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
    FSortedVisibleCols: TList;
    FSummaryManager: TmKGridSummaryManager;
    FSummaryPanel: ISummaryPanel;
    FFiltersPanel : IFilterPanel;
    FOwnedCellDecorations: TmCellDecorations;
    FDataAreFiltered: boolean;
    FDataAreSorted: boolean;
    FOnGridFiltered: TNotifyEvent;
    FParser : TKAParser;
    // menu
    FColumnsHeaderPopupMenu: TPopupMenu;
    FOriginalPopupMenu: TPopupMenu;
    FMI_EditFilters: TMenuItem;
    FMI_RemoveAllFilters: TMenuItem;
    FMI_Summaries: TMenuItem;
    FCurrentCol: integer;
    FCurrentRow: integer;
    FAlternateGridRowColor : TColor;
    FCurrentDrawingRow : integer;
    FMeasureCellsCache : TmStringDictionary;

    procedure CreateFields;
    procedure UpdateFields;
    procedure SetProvider(AValue: TmVirtualDatasetDataProvider);
    procedure ApplyDecorations(aCellPainter: TKGridCellPainter; const aField : TmField; const aRow : integer; const aState: TKGridDrawState);
    procedure OnDrawGridCell(Sender: TObject; ACol, ARow: integer; R: TRect; State: TKGridDrawState);
    procedure OnMeasureCell(Sender: TObject; ACol, ARow: integer; R: TRect; State: TKGridDrawState; Priority: TKGridMeasureCellPriority; var Extent: TPoint);
    procedure OnKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure OnMoveColumns(Sender: TObject; Index1, Index2: integer);
    procedure OnCustomSort(Sender: TObject; ByIndex: integer; SortMode: TKGridSortMode; var Sorted: boolean);
    function ColToField(const aCol: integer): TmField;
    function GetValue(const aCol, aRow: integer): variant;
    function GetValueAsFormattedString(const aCol, aRow: integer; out aOutOfIndex : boolean): string;
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
    procedure RefreshFiltersPanel;
    procedure OnParserGetValue (Sender: TObject; const valueName: string; var Value: Double; out Successfull : boolean);
    procedure OnParserGetStrValue (Sender: TObject; const valueName: string; var StrValue: string; out Successfull : boolean);
  public
    constructor Create(aGrid: TKGrid; aFormulaFields: TmFormulaFields); virtual;
    destructor Destroy; override;

    procedure SetupGrid(const aEnableAutoSizedColumns: boolean = True); override;
    procedure SelectAllRows; override;
    procedure SelectRows(const aKeyField: string; const aValues: TStringList); override;

    procedure InitGrid;

    procedure ReadSettings(aSettings: TmGridColumnsSettings);
    procedure ApplySettings(aSettings: TmGridColumnsSettings);
    procedure RefreshDataProvider(const aReloadFields: boolean);
    function GetSummaryManager: ISummaryDatasetManager;
    procedure GetFields(aFields: TmFields);
    function GetField(const aFieldName : String): TmField;
    function GetDataCursor: ImGridCursor;
    procedure GetColumns(aColumns: TmGridColumns);
    function CalculateHashCodeOfSelectedRows: string;
    procedure GetSelectedRows(aRows: TIntegerList);

    property Provider: TmVirtualDatasetDataProvider read FProvider write SetProvider;
    property SummaryPanel: ISummaryPanel read FSummaryPanel write FSummaryPanel;
    property FiltersPanel : IFilterPanel read FFiltersPanel write FFiltersPanel;
    property OnGridFiltered: TNotifyEvent read FOnGridFiltered write FOnGridFiltered;
    property AlternateGridRowColor : TColor read FAlternateGridRowColor write FAlternateGridRowColor;
  end;

implementation

uses
  SysUtils, Variants, LCLType, md5, Math,
  kgraphics, kcontrols,
  mDataProviderFieldDefs, mDataFieldsStandardSetup, mGraphicsUtility, mDataProviderUtility, mSortConditions,
  mGridFilterValuesDlg, mFilterOperators, mWaitCursor, mDataFieldsUtility, mGridFiltersEditDlg,
  mDateTimeUtility, mMagnificationFactor {$IFDEF DEBUG}, mLog{$ENDIF};

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
  // nope
end;

procedure TmKGridCursor.EndBrowsing;
begin
  // nope
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
  FSortedVisibleCols.Clear;
  FProvider.FillFields(FFields);
  ApplyStandardSettingsToFields(FFields, '#,##0.00');

  for i := 0 to FFields.Count - 1 do
    if FFields.Get(i).Visible and (not IsSystemField(FFields.Get(i).FieldName)) then
      FSortedVisibleCols.Add(FFields.Get(i));
end;

procedure TmKGridHelper.UpdateFields;
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

procedure TmKGridHelper.SetProvider(AValue: TmVirtualDatasetDataProvider);
begin
  if FProvider = AValue then Exit;
  FProvider := AValue;
  if Assigned(FCursor) then
    FCursor.Free;
  FCursor := TmKGridCursor.Create(FProvider);
  FSummaryManager.Provider := FProvider;
end;

procedure TmKGridHelper.ApplyDecorations(aCellPainter: TKGridCellPainter; const aField : TmField; const aRow : integer; const aState: TKGridDrawState);
var
  tmpCellDecoration : TmCellDecoration;
  tmpListOfDecorations : TmListOfDecorations;
  PerformCustomizedDraw : boolean;
  tmpValue : double;
  i : integer;
  bkColor : TColor;
begin
  FCurrentDrawingRow := aRow;

  FOwnedCellDecorations.FindByFieldName(aField.FieldName, tmpListOfDecorations);
  if not Assigned(tmpListOfDecorations) then
    FOwnedCellDecorations.FindByFieldName(DECORATE_ALL_FIELDS_FIELDNAME, tmpListOfDecorations);
  if Assigned(tmpListOfDecorations) then
  begin
    for i := 0 to tmpListOfDecorations.Count - 1 do
    begin
     tmpCellDecoration := tmpListOfDecorations.Get(i);

     PerformCustomizedDraw := true;
     if tmpCellDecoration.Condition.NotNull then
     begin
       if not Assigned(FParser) then
       begin
         FParser := TKAParser.Create;
         FParser.OnGetValue:= Self.OnParserGetValue;
         FParser.OnGetStrValue:= OnParserGetStrValue;
       end;
       try
         if FParser.Calculate(tmpCellDecoration.Condition.Value, tmpValue) then
           PerformCustomizedDraw:= round(tmpValue) = 1;
       except
         on e:Exception do
         begin
           PerformCustomizedDraw:= false;
         end;
       end;
     end;
     if PerformCustomizedDraw then
     begin
       if tmpCellDecoration.BackgroundColor.NotNull then
       begin
         bkColor := tmpCellDecoration.BackgroundColor.Value;
         if (aState * [gdFixed, gdSelected] <> []) then
         begin
           if IsDark(tmpCellDecoration.BackgroundColor.Value) then
             bkColor := LighterColor(tmpCellDecoration.BackgroundColor.Value, 70)
           else
             bkColor := DarkerColor(tmpCellDecoration.BackgroundColor.Value, 20);
         end;
         aCellPainter.BackColor:= bkColor;
         aCellPainter.Canvas.Brush.Color:= bkColor;
       end;
       if tmpCellDecoration.TextColor.NotNull then
         aCellPainter.Canvas.Font.Color:= tmpCellDecoration.TextColor.Value;
       if tmpCellDecoration.TextBold.NotNull and tmpCellDecoration.TextBold.Value then
         aCellPainter.Canvas.Font.Style:= aCellPainter.Canvas.Font.Style + [fsBold];
       if tmpCellDecoration.TextItalic.NotNull and tmpCellDecoration.TextItalic.Value then
         aCellPainter.Canvas.Font.Style:= aCellPainter.Canvas.Font.Style + [fsItalic];
       break;
     end;
    end;
  end;
end;

procedure TmKGridHelper.OnDrawGridCell(Sender: TObject; ACol, ARow: integer; R: TRect; State: TKGridDrawState);
var
  grid: TKGrid;
  curField: TmField;
  isInt, isFloat, outOfBounds: boolean;
begin
  grid := (FGrid as TKGrid);
  if ACol >= Self.FSortedVisibleCols.Count then
    exit;
  curField := Self.ColToField(ACol);

  if not Assigned(curField) then
    exit;

  // https://forum.lazarus.freepascal.org/index.php/topic,44833.msg315562.html#msg315562

  //grid.Cell[ACol, ARow].ApplyDrawProperties;

  if grid.CellPainter.Canvas.Brush.Style = bsClear then grid.CellPainter.Canvas.Brush.Style := bsSolid;
  if (ARow < grid.FixedRows) then
  begin
    grid.CellPainter.HAlign := halCenter;
    grid.CellPainter.VAlign := valCenter;
  end
  else
  begin
    isInt := FieldTypeIsInteger(curField.DataType);
    isFloat := FieldTypeIsFloat(curField.DataType);

    if isFloat or isInt then
      grid.CellPainter.HAlign := halRight;
  end;

  if (FAlternateGridRowColor <> clNone) and (State * [gdFixed, gdSelected] = []) and Odd(ARow) then
  begin
    grid.CellPainter.BackColor:= FAlternateGridRowColor;
    grid.CellPainter.Canvas.Brush.Color:= FAlternateGridRowColor;
  end;

  if ARow >= grid.FixedRows then
    ApplyDecorations(grid.CellPainter, curField, ARow - (FGrid as TKGrid).FixedRows, State);

  grid.CellPainter.DrawDefaultCellBackground;
  outOfBounds:= false;
  if ARow = 0 then
    grid.CellPainter.Text := curField.DisplayLabel
  else
    grid.CellPainter.Text := GetValueAsFormattedString(ACol, ARow - (FGrid as TKGrid).FixedRows, outOfBounds);
  if outOfBounds then
  begin
    grid.CellPainter.BackColor:= grid.Color;
    grid.CellPainter.Canvas.Brush.Color:= grid.Color;
  end;
  grid.CellPainter.DrawCellCommon;
end;

procedure TmKGridHelper.OnMeasureCell(Sender: TObject; ACol, ARow: integer; R: TRect; State: TKGridDrawState; Priority: TKGridMeasureCellPriority; var Extent: TPoint);
var
  outOfBounds: boolean;
begin
  if ACol >= FSortedVisibleCols.Count then
    exit;
  outOfBounds := false;
  if ARow = 0 then
    GetTextExtend(ColToField(ACol).DisplayLabel, FGrid.Font, Extent)
  else
    GetTextExtend(GetValueAsFormattedString(ACol, ARow - (FGrid as TKGrid).FixedRows, outOfBounds), FGrid.Font, Extent);
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
  tmpField := TmField(FSortedVisibleCols.Items[Index1]);
  FSortedVisibleCols.Items[Index1] := FSortedVisibleCols.Items[Index2];
  FSortedVisibleCols.Items[Index2] := tmpField;
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

    FProvider.SortConditions.Items[0].FieldName := ColToField(ByIndex).FieldName;

    if SortMode = smDown then
      FProvider.SortConditions.Items[0].SortType := stDescending
    else if SortMode = smUp then
      FProvider.SortConditions.Items[0].SortType := stAscending;
    FProvider.Refresh(True, FDataAreFiltered);
    Sorted := True;
    FDataAreSorted := True;
  end;
end;

function TmKGridHelper.ColToField(const aCol: integer): TmField;
begin
  Result := TmField(FSortedVisibleCols.Items[aCol]);
end;

function TmKGridHelper.GetValue(const aCol, aRow: integer): variant;
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

function TmKGridHelper.GetValueAsFormattedString(const aCol, aRow: integer; out aOutOfIndex : boolean): string;
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
    currentField := ColToField(FCurrentCol);
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
                (FGrid as TKGrid).RowCount := max(2, FProvider.GetRecordCount + (FGrid as TKGrid).FixedRows);
              finally
                (FGrid as TKGrid).UnlockUpdate;
              end;
              FSummaryManager.RefreshSummaries;
              if Assigned(FOnGridFiltered) then
                FOnGridFiltered(Self);
            finally
              TWaitCursor.UndoWaitCursor('TmKGridHelper.OnFilterValues');
            end;
          end;
          RefreshFiltersPanel;
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
      CurrentField := ColToField(FCurrentCol);
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
      (FGrid as TKGrid).RowCount := max(2, FProvider.GetRecordCount + (FGrid as TKGrid).FixedRows);
    finally
      (FGrid as TKGrid).UnlockUpdate;
    end;
    FSummaryManager.RefreshSummaries;
    RefreshFiltersPanel;
    if Assigned(FOnGridFiltered) then
      FOnGridFiltered(Self);
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
      removedFilters := TStringList.Create;
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
            (FGrid as TKGrid).RowCount := max(2, FProvider.GetRecordCount + (FGrid as TKGrid).FixedRows);
          finally
            (FGrid as TKGrid).UnlockUpdate;
          end;
          FSummaryManager.RefreshSummaries;
          RefreshFiltersPanel;
          if Assigned(FOnGridFiltered) then
            FOnGridFiltered(Self);
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

procedure TmKGridHelper.RefreshFiltersPanel;
var
  tmpFields : TmFields;
begin
  if Assigned(FFiltersPanel) then
    FFiltersPanel.SetFilters(FProvider.FilterConditions, FFields);
end;

procedure TmKGridHelper.OnParserGetValue(Sender: TObject; const valueName: string; var Value: Double; out Successfull: boolean);
var
  tmpField : TmField;
  varValue : variant;
begin
  Successfull:= false;

  if FCurrentDrawingRow < 0 then
    exit;

  if FFields.Count > 0 then
  begin
    Value := 0;
    if FProvider.GetRecordCount = 0 then
      Successfull:= true
    else
    begin
      tmpField := Self.GetField(valueName);
      if Assigned(tmpField) then
      begin
        if FieldTypeIsInteger(tmpField.DataType) or FieldTypeIsFloat(tmpField.DataType) or
          FieldTypeIsDate(tmpField.DataType) or FieldTypeIsTime(tmpField.DataType) or FieldTypeIsDateTime(tmpField.DataType) or
          FieldTypeIsBoolean(tmpField.DataType) then
        begin
          FProvider.GetFieldValue(valueName, FCurrentDrawingRow, varValue);
          if not VarIsNull(varValue) then
          begin
            if FieldTypeIsBoolean(tmpField.DataType) then
            begin
              if varValue then
                Value := 1
              else
                Value := 0;
            end
            else
              Value := varValue;
          end;
          Successfull:= true;
        end;
      end;
    end;
  end;
end;

procedure TmKGridHelper.OnParserGetStrValue(Sender: TObject; const valueName: string; var StrValue: string; out Successfull: boolean);
var
  tmpField: TmField;
  varValue : variant;
begin
  Successfull:= false;

  if FCurrentDrawingRow < 0 then
    exit;

  if FFields.Count > 0 then
  begin
    StrValue := '';
    if FProvider.GetRecordCount = 0 then
      Successfull:= true
    else
    begin
      tmpField := GetField(valueName);
      if Assigned(tmpField) then
      begin
        FProvider.GetFieldValue(valueName, FCurrentDrawingRow, varValue);
        if not VarIsNull(varValue) then
          StrValue := VarToStr(varValue);
        Successfull:= true;
      end;
    end;
  end;
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
  FSortedVisibleCols := TList.Create;
  (FGrid as TKGrid).Options := [goVirtualGrid, goColSizing, goColMoving, goRowSorting, goDrawFocusSelected, goRowSelect, goVertLine, goHeader, goHorzLine, goRangeSelect, goFixedVertLine, goFixedHorzLine, goMouseOverCells];
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
  (FGrid as TKGrid).DefaultRowHeight:= ScaleForMagnification((FGrid as TKGrid).DefaultRowHeight, true);
  ScaleFontForMagnification((FGrid as TKGrid).Font);
  FAlternateGridRowColor:= clNone;

  FCurrentCol := -1;
  FCurrentRow := -1;

  FSummaryManager := TmKGridSummaryManager.Create;
  FSummaryManager.RegisterListener(Self.RefreshSummaryPanel);
end;

destructor TmKGridHelper.Destroy;
begin
  FreeAndNil(FCursor);
  FFields.Free;
  FSortedVisibleCols.Free;
  FOwnedCellDecorations.Free;
  FSummaryManager.Free;
  FreeAndNil(FParser);
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
var
  index : TmStringDictionary;
  i : integer;
  tmpValue : variant;
  tmpRect : TKGridRect;
begin
  (FGrid as TKGrid).ClearSelections;
  index := TmStringDictionary.Create(false);
  try
    for i := 0 to aValues.Count - 1 do
    begin
      if not index.Contains(aValues.Strings[i]) then
        index.Add(aValues.Strings[i], index);
    end;

    for i := 0 to FProvider.GetRecordCount - 1 do
    begin
      FProvider.GetFieldValue(aKeyField, i, tmpValue);
      if index.Contains(VarToStr(tmpValue)) then
      begin
        tmpRect.Cell1.Col := 0;
        tmpRect.Cell1.Row := i + (FGrid as TKGrid).FixedRows;
        tmpRect.Cell2.Col := FSortedVisibleCols.Count - 1;
        tmpRect.Cell2.Row := tmpRect.Cell1.Row;
        (FGrid as TKGrid).AddSelection(tmpRect);
      end;
    end;
  finally
    index.Free;
  end;
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

    (FGrid as TKGrid).RowCount := max(2, FProvider.GetRecordCount + (FGrid as TKGrid).FixedRows);
    (FGrid as TKGrid).ColCount := FSortedVisibleCols.Count;
  finally
    (FGrid as TKGrid).UnlockUpdate;
  end;
end;

procedure TmKGridHelper.ReadSettings(aSettings: TmGridColumnsSettings);

  procedure AssignField(colSettings: TmGridColumnSettings; aField: TmField);
  begin
    colSettings.Visible.Value := aField.Visible;
    colSettings.DisplayFormat.Value := aField.DisplayFormat;
    colSettings.DisplayLabel.Value := aField.DisplayLabel;
  end;

var
  op: TmGridColumnSettings;
  curField: TmField;
  i: integer;
  processed: TmStringDictionary;
begin
  TWaitCursor.ShowWaitCursor('TmKGridHelper.ReadSettings');
  try
    processed := TmStringDictionary.Create(False);
    try
      for i := 0 to Self.FSortedVisibleCols.Count - 1 do
      begin
        curField := TmField(FSortedVisibleCols.Items[i]);
        if not IsSystemField(curField.FieldName) then
        begin
          op := aSettings.AddSettingsForField(curField.FieldName);
          AssignField(op, curField);
          op.SortOrder.Value := i;
          op.Width.Value := max(MINIMUM_GRID_COLUMN_WIDTH, (FGrid as TKGrid).Cols[i].Extent);
          processed.Add(curField.FieldName, processed);
        end;
      end;
      for i := 0 to FFields.Count - 1 do
      begin
        curField := FFields.Get(i);
        if (not processed.Contains(curField.FieldName)) and (not IsSystemField(curField.FieldName)) then
        begin
          op := aSettings.AddSettingsForField(curField.FieldName);
          AssignField(op, curField);
          op.SortOrder.Value := aSettings.Count - 1;
          op.Width.Value := MINIMUM_GRID_COLUMN_WIDTH * 4;
          processed.Add(curField.FieldName, processed);
        end;
      end;
    finally
      processed.Free;
    end;
  finally
    TWaitCursor.UndoWaitCursor('TmKGridHelper.ReadSettings');
  end;
end;

function CompareVisibleColumns(Item1, Item2: Pointer): integer;
var
  c1, c2: TmGridColumnSettings;
begin
  c1 := TmGridColumnSettings(Item1);
  c2 := TmGridColumnSettings(Item2);
  if c1.SortOrder.AsInteger < c2.SortOrder.AsInteger then
    Result := -1
  else if c1.SortOrder.AsInteger > c2.SortOrder.AsInteger then
    Result := 1
  else
    Result := 0;
end;

procedure TmKGridHelper.ApplySettings(aSettings: TmGridColumnsSettings);
var
  i: integer;
  curField: TmField;
  visibleCols: TFPList;
  widths: TIntegerList;
begin
  try
    TWaitCursor.ShowWaitCursor('TmKGridHelper.ApplySettings');

    (FGrid as TKGrid).LockUpdate;
    try
      widths := TIntegerList.Create;
      try
        FSortedVisibleCols.Clear;

        visibleCols := TFPList.Create;
        try
          for i := 0 to aSettings.Count - 1 do
            if aSettings.Get(i).Visible.AsBoolean then
              visibleCols.Add(aSettings.Get(i));
          visibleCols.Sort(CompareVisibleColumns);

          for i := 0 to visibleCols.Count - 1 do
          begin
            curField := FFields.FieldByName(TmGridColumnSettings(visibleCols.Items[i]).FieldName);
            if Assigned(curField) then
            begin
              FSortedVisibleCols.Add(curField);
              widths.Add(TmGridColumnSettings(visibleCols.Items[i]).Width.AsInteger);
            end
            else
            begin
              {$IFDEF DEBUG}
              logger.Debug('[TmKGridHelper.ApplySettings] Missing field ' + TmGridColumnSettings(visibleCols.Items[i]).FieldName);
              {$ENDIF}
            end;
          end;
        finally
          visibleCols.Free;
        end;

        for i := 0 to FFields.Count -1 do
          FFields.Get(i).Visible := false;

        for i := 0 to aSettings.Count - 1 do
        begin
          curField := FFields.FieldByName(aSettings.Get(i).FieldName);
          if Assigned(curField) then
          begin
            curField.Visible := aSettings.Get(i).Visible.AsBoolean;
            curField.DisplayFormat := aSettings.Get(i).DisplayFormat.AsString;
            curField.DisplayLabel := aSettings.Get(i).DisplayLabel.AsString;
          end
          else
          begin
            {$IFDEF DEBUG}
            logger.Debug('[TmKGridHelper.ApplySettings] Missing field ' + aSettings.Get(i).FieldName);
            {$ENDIF}
          end;
        end;

        RefreshDataProvider(false);

        for i := 0 to widths.Count - 1 do
          (FGrid as TKGrid).Cols[i].Extent := widths.Items[i];
      finally
        widths.Free;
      end;
    finally
      (FGrid as TKGrid).UnlockUpdate;
    end;

    (FGrid as TKGrid).Invalidate;
  finally
    TWaitCursor.UndoWaitCursor('TmKGridHelper.ApplySettings');
  end;

end;

procedure TmKGridHelper.RefreshDataProvider(const aReloadFields: boolean);
begin
  (FGrid as TKGrid).LockUpdate;
  try
    (FGrid as TKGrid).ClearSortMode;
    if FFields.Count = 0 then
      CreateFields
    else if aReloadFields then
      UpdateFields;
    (FGrid as TKGrid).FixedRows := 1;
    (FGrid as TKGrid).FixedCols := 0;

    {$IFDEF DEBUG}
    logger.Debug('Actual ColCount=' + IntToStr((FGrid as TKGrid).ColCount));
    logger.Debug('New ColCount=' + IntToStr(FSortedVisibleCols.Count));
    {$ENDIF}
    (FGrid as TKGrid).ColCount := FSortedVisibleCols.Count;

    FDataAreSorted := False;
    FProvider.Refresh(False, FDataAreFiltered);
    (FGrid as TKGrid).RowCount := max(2, FProvider.GetRecordCount + (FGrid as TKGrid).FixedRows);

    FSummaryManager.RefreshSummaries;
    (FGrid as TKGrid).ClearSelections;
  finally
    (FGrid as TKGrid).UnlockUpdate;
  end;
  (FGrid as TKGrid).Invalidate;
end;

function TmKGridHelper.GetSummaryManager: ISummaryDatasetManager;
begin
  Result := FSummaryManager;
end;

procedure TmKGridHelper.GetFields(aFields: TmFields);
begin
  aFields.Assign(FFields);
end;

function TmKGridHelper.GetField(const aFieldName: String): TmField;
begin
  Result := FFields.FieldByName(aFieldName);
end;

function TmKGridHelper.GetDataCursor: ImGridCursor;
begin
  Result := FCursor;
end;

procedure TmKGridHelper.GetColumns(aColumns: TmGridColumns);
var
  i: integer;
begin
  aColumns.Clear;
  for i := 0 to FSortedVisibleCols.Count - 1 do
    aColumns.Add.Assign(TmField(FSortedVisibleCols.Items[i]));
end;

function TmKGridHelper.CalculateHashCodeOfSelectedRows: string;
var
  list: TIntegerList;
  k: integer;
  tmp: string;
begin
  Result := '';
  list := TIntegerList.Create;
  try
    tmp := '';
    for k := 0 to list.Count - 1 do
    begin
      tmp := tmp + '*' + IntToStr(k);
      if Length(tmp) > 1024 then
      begin
        Result := MD5Print(MD5String(Result + MD5Print(MD5String(tmp))));
        tmp := '';
      end;
    end;
    if Result = '' then
      Result := tmp
    else
    begin
      if tmp <> '' then
        Result := MD5Print(MD5String(Result + MD5Print(MD5String(tmp))));
    end;
  finally
    list.Free;
  end;
end;

procedure TmKGridHelper.GetSelectedRows(aRows: TIntegerList);
var
  i, k: integer;
  curRect : TKGridRect;
begin
  aRows.Clear;

  if (FGrid as TKGrid).SelectionCount > 0 then
  begin
    for i := 0 to (FGrid as TKGrid).SelectionCount - 1 do
    begin
      {$IFDEF DEBUG}
      logger.Debug('TmKGridHelper.GetSelectedRows - Col1:' + IntToStr((FGrid as TKGrid).Selections[i].Col1) + ' Col2:' + IntToStr((FGrid as TKGrid).Selections[i].Col2) +
        ' Row1:' + IntToStr((FGrid as TKGrid).Selections[i].Row1) + ' Row2:' + IntToStr((FGrid as TKGrid).Selections[i].Col1) +
        ' Cell1:' + IntToStr((FGrid as TKGrid).Selections[i].Cell1.Row) + '-' + IntToStr((FGrid as TKGrid).Selections[i].Cell1.Col) +
        ' Cell2:' + IntToStr((FGrid as TKGrid).Selections[i].Cell2.Row) + '-' + IntToStr((FGrid as TKGrid).Selections[i].Cell2.Col));
      {$ENDIF}

      curRect := (FGrid as TKGrid).Selections[i];
      NormalizeGridRect(curRect);

      for k := curRect.Cell1.Row to curRect.Cell2.Row do
      begin
        if k >= (FGrid as TKGrid).FixedRows then
          aRows.Add(k - (FGrid as TKGrid).FixedRows);
      end;
    end;
  end
  else
  begin
    if (FProvider.GetRecordCount > 0) then
    begin
      if (FGrid as TKGrid).Row >= (FGrid as TKGrid).FixedRows then
        aRows.Add((FGrid as TKGrid).Row - (FGrid as TKGrid).FixedRows);
    end;
  end;
end;

{$IFDEF DEBUG}
initialization
  logger := logManager.AddLog('mKGridHelper');
{$ENDIF}
end.
