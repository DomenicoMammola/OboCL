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

    FOnGridFiltered: TNotifyEvent;
    FOnGridSorted: TNotifyEvent;


    FColumnsHeaderPopupMenu: TPopupMenu;
    FOriginalPopupMenu: TPopupMenu;
    FMI_EditFilters: TMenuItem;
    FMI_RemoveAllFilters: TMenuItem;
    FMI_Summaries: TMenuItem;

    FCurrentCol: integer;
    FCurrentRow: integer;
    FCurrentDrawingRow : integer;

    procedure OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure RefreshSummaryPanel(Sender: TObject);
    procedure SetProvider(AValue: TmVirtualDatasetDataProvider);
    procedure CreateFields;
    procedure UpdateFields;
    function ColToField(const aCol: integer): TmField;
    function GetValue(const aCol, aRow: integer): variant;
    function GetValueAsFormattedString(const aCol, aRow: integer; out aOutOfIndex : boolean): string;
    procedure OnDrawGridCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState:TGridDrawState);
    procedure OnPrepareCanvas(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
    procedure OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnParserGetValue (Sender: TObject; const valueName: string; var Value: Double; out Successfull : boolean);
    procedure OnParserGetStrValue (Sender: TObject; const valueName: string; var StrValue: string; out Successfull : boolean);
    procedure BuildHeaderPopupMenu;
    procedure OnFilterValues(Sender: TObject);
    procedure OnEditSummaries(Sender: TObject);
    procedure OnRemoveSummaries(Sender: TObject);
    procedure OnRemoveAllFilters(Sender: TObject);
    procedure OnEditFilters(Sender: TObject);
    procedure OnColumnsHeaderMenuPopup(Sender: TObject);
    procedure RefreshFiltersPanel;
    procedure OnHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
    procedure OnColRowMoved (Sender: TObject; IsColumn:Boolean; sIndex, tIndex: Integer);
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
    function CalculateHashCodeOfSelectedRows: string;

    procedure InitGrid;
    function GetField(const aFieldName : String): TmField;

    procedure SelectAllRows; override;
    procedure SelectRows (const aKeyField : String; const aValues : TStringList); override;
    procedure SetupGrid (const aEnableAutoSizedColumns : boolean = true); override;

    property OnGridFiltered: TNotifyEvent read FOnGridFiltered write FOnGridFiltered;
    property OnGridSorted: TNotifyEvent read FOnGridSorted write FOnGridSorted;

    property Provider: TmVirtualDatasetDataProvider read FProvider write SetProvider;
    property SummaryPanel: ISummaryPanel read FSummaryPanel write FSummaryPanel;
    property FiltersPanel : IFilterPanel read FFiltersPanel write FFiltersPanel;
  end;

implementation

uses
  SysUtils, Math, Variants, Types, md5, LCLType,
  mDataProviderUtility, mMagnificationFactor, mDataFieldsStandardSetup,
  mDataFieldsUtility, mDateTimeUtility, mGridFilterValuesDlg, mWaitCursor,
  mFilterOperators, mGridFiltersEditDlg, mSortConditions, mGraphicsUtility

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

procedure TmDrawGridHelper.OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Shift = [ssCtrl]) and (Key = VK_C) then
    CopyTextToClipboard(VarToStr(GetValue((FGrid as TmDrawGrid).Col, (FGrid as TmDrawGrid).Row - (FGrid as TmDrawGrid).FixedRows)));
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
        TWaitCursor.ShowWaitCursor('TmDrawGridHelper.OnFilterValues');
        FProvider.GetUniqueStringValuesForField(currentField.FieldName, values);
        dlg.Init(values);
      finally
        TWaitCursor.UndoWaitCursor('TmDrawGridHelper.OnFilterValues');
      end;
      if dlg.ShowModal = mrOk then
      begin
        checkedValues := TStringList.Create;
        try
          dlg.GetCheckedValues(checkedValues);
          FProvider.FilterConditions.ClearForField(currentField.FieldName);
          if checkedValues.Count > 0 then
          begin
            (FGrid as TmDrawGrid).ClearSelections;
            try
              TWaitCursor.ShowWaitCursor('TmDrawGridHelper.OnFilterValues');
              tmpFilter := FProvider.FilterConditions.Add;
              tmpFilter.FieldName := currentField.FieldName;
              tmpFilter.FilterOperator := foIn;
              tmpVariant := variants.VarArrayCreate([0, checkedValues.Count - 1], varOleStr);
              for i := 0 to checkedValues.Count - 1 do
                VarArrayPut(tmpVariant, checkedValues.Strings[i], [i]);
              tmpFilter.Value := tmpVariant;
              (FGrid as TmDrawGrid).BeginUpdate;
              try
                FDataAreFiltered := True;
                FProvider.Refresh(FDataAreSorted, FDataAreFiltered);
                (FGrid as TmDrawGrid).RowCount := max(2, FProvider.GetRecordCount + (FGrid as TmDrawGrid).FixedRows);
              finally
                (FGrid as TmDrawGrid).EndUpdate(true);
              end;
              FSummaryManager.RefreshSummaries;
              if Assigned(FOnGridFiltered) then
                FOnGridFiltered(Self);
            finally
              TWaitCursor.UndoWaitCursor('TmDrawGridHelper.OnFilterValues');
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

procedure TmDrawGridHelper.OnEditSummaries(Sender: TObject);
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

procedure TmDrawGridHelper.OnRemoveSummaries(Sender: TObject);
begin
  FSummaryManager.GetSummaryDefinitions.Clear;
  FSummaryManager.RefreshSummaries;
end;

procedure TmDrawGridHelper.OnRemoveAllFilters(Sender: TObject);
begin
  if FDataAreFiltered then
  begin
    (FGrid as TmDrawGrid).BeginUpdate;
    try
      FDataAreFiltered := False;
      FProvider.FilterConditions.Clear;
      FProvider.Refresh(FDataAreSorted, False);
      (FGrid as TmDrawGrid).RowCount := max(2, FProvider.GetRecordCount + (FGrid as TmDrawGrid).FixedRows);
    finally
      (FGrid as TmDrawGrid).EndUpdate(true);
    end;
    FSummaryManager.RefreshSummaries;
    RefreshFiltersPanel;
    if Assigned(FOnGridFiltered) then
      FOnGridFiltered(Self);
  end;
end;

procedure TmDrawGridHelper.OnEditFilters(Sender: TObject);
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
          (FGrid as TmDrawGrid).ClearSelections;
          for i := 0 to removedFilters.Count - 1 do
            FProvider.FilterConditions.ClearForField(removedFilters[i]);

          (FGrid as TmDrawGrid).BeginUpdate;
          try
            FDataAreFiltered := (FProvider.FilterConditions.Count > 0);
            FProvider.Refresh(FDataAreSorted, FDataAreFiltered);
            (FGrid as TmDrawGrid).RowCount := max(2, FProvider.GetRecordCount + (FGrid as TmDrawGrid).FixedRows);
          finally
            (FGrid as TmDrawGrid).EndUpdate(true);
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

procedure TmDrawGridHelper.RefreshFiltersPanel;
var
  tmpFields : TmFields;
begin
  if Assigned(FFiltersPanel) then
    FFiltersPanel.SetFilters(FProvider.FilterConditions, FFields);
end;

procedure TmDrawGridHelper.OnHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
var
  fld : TmField;
  tmpSortType : TSortType;
begin
  fld := ColToField(Index);
  if Assigned(fld) then
  begin
    (FGrid as TmDrawGrid).ClearSelections;
    try
      TWaitCursor.ShowWaitCursor('OnHeaderClick');
      tmpSortType := stAscending;

      // analize current filter
      if (FDataAreSorted) and (FProvider.SortConditions.Count > 0) and (FProvider.SortConditions.Items[0].FieldName = fld.FieldName) then
      begin
        if FProvider.SortConditions.Items[0].SortType = stAscending then
          tmpSortType:= stDescending
        else
        begin
          FProvider.Refresh(False, FDataAreFiltered);
          FDataAreSorted:= false;
          exit;
        end
      end;

      // set new sort condition
      FProvider.SortConditions.Clear;
      with FProvider.SortConditions.Add do
      begin
        FieldName:= fld.FieldName;
        SortType:= tmpSortType;
      end;

      FProvider.Refresh(True, FDataAreFiltered);
      FDataAreSorted := True;
      FGrid.Invalidate;
    finally
      TWaitCursor.UndoWaitCursor('OnHeaderClick');
    end;
  end;

  if Assigned(FOnGridSorted) then
    FOnGridSorted(Self);
end;

procedure TmDrawGridHelper.OnColRowMoved(Sender: TObject; IsColumn: Boolean; sIndex, tIndex: Integer);
var
  tmpField: TmField;
  i : integer;
begin
  if IsColumn then
  begin
    tmpField := TmField(FSortedVisibleCols.Items[sIndex]);
    if sIndex < tIndex then
    begin
      for i := sIndex to tIndex - 1 do
        FSortedVisibleCols.Items[i] := FSortedVisibleCols.Items[i + 1];
    end
    else
    begin
      for i := sIndex downto tIndex + 1 do
        FSortedVisibleCols.Items[i] := FSortedVisibleCols.Items[i - 1];
    end;
    FSortedVisibleCols.Items[tIndex] := tmpField;
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
  outOfBounds: boolean;
  alignment : TAlignment;
  tmpRect : TRect;
  ifl : integer;
  v : variant;
begin
  grid := (FGrid as TmDrawGrid);
  if ACol >= Self.FSortedVisibleCols.Count then
    exit;
  curField := Self.ColToField(ACol);

  if not Assigned(curField) then
    exit;
  (Sender as TmDrawGrid).DefaultDrawCell(aCol, aRow, aRect, aState);

  if aRow = 0 then
  begin
    (Sender as TmDrawGrid).DrawHeaderCell(aCol, aRow, aRect, aState, curField.DisplayLabel);
    if (FDataAreSorted) and (FProvider.SortConditions.Count > 0) and (FProvider.SortConditions.Items[0].FieldName = curField.FieldName) then
    begin
      tmpRect:= aRect;
      tmpRect.Left := aRect.Right - aRect.Height;
      ifl := -1 * max( 1, trunc(tmpRect.Height * 0.2));
      InflateRect(tmpRect, ifl, ifl);
      (Sender as TmDrawGrid).Canvas.Brush.Color:= (Sender as TmDrawGrid).Font.Color;
      if FProvider.SortConditions.Items[0].SortType = stAscending then
        DrawTriangle((Sender as TmDrawGrid).Canvas, tmpRect, toTop)
      else
        DrawTriangle((Sender as TmDrawGrid).Canvas, tmpRect, toBottom);
    end;
  end
  else
  begin
    if FieldTypeIsBoolean(curField.DataType) then
    begin
      v := GetValue(ACol, ARow - (FGrid as TmDrawGrid).FixedRows);
      if VarIsNull(v) then
        (Sender as TmDrawGrid).DrawSingleCell(aCol, aRow, aRect, aState, '', Classes.taLeftJustify)
      else
      begin
        tmpRect:= aRect;
        tmpRect.Left := aRect.Left + (aRect.Width div 2) - (aRect.Height div 2);
        tmpRect.Right := tmpRect.Left + aRect.Height;
        ifl := -1 * max( 1, trunc(tmpRect.Height * 0.2));
        InflateRect(tmpRect, ifl, ifl);
        DrawCheckedBox((Sender as TmDrawGrid).Canvas, tmpRect, v, (Sender as TmDrawGrid).Font.Color, (Sender as TmDrawGrid).Color, (Sender as TmDrawGrid).GridLineColor);
      end;
    end
    else
    begin
      if FieldTypeIsFloat(curField.DataType) or FieldTypeIsInteger(curField.DataType) then
        alignment:= Classes.taRightJustify
      else
        alignment:= Classes.taLeftJustify;
      (Sender as TmDrawGrid).DrawSingleCell(aCol, aRow, aRect, aState, GetValueAsFormattedString(ACol, ARow - (FGrid as TmDrawGrid).FixedRows, outOfBounds), alignment);
    end;
    if ((Sender as TmDrawGrid).Col = ACol) and ((Sender as TmDrawGrid).Row = ARow) then
    begin
      tmpRect := aRect;
      tmpRect.Right := tmpRect.Right - 1;
      tmpRect.Top := tmpRect.Top + 1;
      tmpRect.Bottom := tmpRect.Bottom - 2;
      DrawRubberRect((Sender as TmDrawGrid).Canvas, tmpRect, (Sender as TmDrawGrid).FocusColor, BF_RECT);
    end;
  end;
end;

procedure TmDrawGridHelper.OnPrepareCanvas(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
var
  tmpCellDecoration : TmCellDecoration;
  tmpListOfDecorations : TmListOfDecorations;
  PerformCustomizedDraw : boolean;
  tmpValue : double;
  i : integer;
  grid : TmDrawGrid;
  curField : TmField;
begin
  grid := FGrid as TmDrawGrid;

  curField := Self.ColToField(ACol);
  FCurrentDrawingRow := aRow - grid.FixedRows;

  FCellDecorations.FindByFieldName(curField.FieldName, tmpListOfDecorations);
  if not Assigned(tmpListOfDecorations) then
    FCellDecorations.FindByFieldName(DECORATE_ALL_FIELDS_FIELDNAME, tmpListOfDecorations);
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
         if gdSelected in AState then
         begin
           if IsDark(tmpCellDecoration.BackgroundColor.Value) then
             grid.Canvas.Brush.Color := LighterColor(tmpCellDecoration.BackgroundColor.Value, 70)
           else
             grid.Canvas.Brush.Color := DarkerColor(tmpCellDecoration.BackgroundColor.Value, 20);
         end
         else
           grid.Canvas.Brush.Color := tmpCellDecoration.BackgroundColor.Value;
       end;
       if tmpCellDecoration.TextColor.NotNull then
         grid.Canvas.Font.Color:= tmpCellDecoration.TextColor.Value;
       if tmpCellDecoration.TextBold.NotNull and tmpCellDecoration.TextBold.Value then
         grid.Canvas.Font.Style:= grid.Canvas.Font.Style + [fsBold];
       if tmpCellDecoration.TextItalic.NotNull and tmpCellDecoration.TextItalic.Value then
         grid.Canvas.Font.Style:= grid.Canvas.Font.Style + [fsItalic];

       break;
     end;
    end;
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
      if Assigned((FGrid as TmDrawGrid).PopupMenu) and ((FGrid as TmDrawGrid).PopupMenu <> FColumnsHeaderPopupMenu) then
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

procedure TmDrawGridHelper.OnParserGetValue(Sender: TObject; const valueName: string; var Value: Double; out Successfull: boolean);
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

procedure TmDrawGridHelper.OnParserGetStrValue(Sender: TObject; const valueName: string; var StrValue: string; out Successfull: boolean);
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

constructor TmDrawGridHelper.Create(aGrid: TmDrawGrid; aFormulaFields: TmFormulaFields);
begin
  FDataAreFiltered := False;
  FDataAreSorted := False;
  FOnGridFiltered:= nil;
  FOnGridSorted:= nil;

  FOwnedCellDecorations := TmCellDecorations.Create;
  InternalSetup(aGrid, aFormulaFields, FOwnedCellDecorations);
  FGrid := aGrid;
  FProvider := nil;
  FFields := TmFields.Create;
  FSortedVisibleCols := TList.Create;

  (FGrid as TmDrawGrid).AlternateColor:= DefaultGridAlternateColor;
  (FGrid as TmDrawGrid).Flat := True;
  (FGrid as TmDrawGrid).Options := [goRowHighlight, goColSizing, goColMoving, goVertLine, goHorzLine, goFixedHorzLine, goFixedVertLine, goTabs, goDrawFocusSelected, goDblClickAutoSize, goRelaxedRowSelect, goRangeSelect, goRowSelect, goSmoothScroll];
  (FGrid as TmDrawGrid).RangeSelectMode := rsmMulti;
  (FGrid as TmDrawGrid).DefaultDrawing := true;
  (FGrid as TmDrawGrid).OnDrawCell := OnDrawGridCell;
  (FGrid as TmDrawGrid).OnPrepareCanvas := OnPrepareCanvas;
  (FGrid as TmDrawGrid).OnMouseDown := OnMouseDown;
  (FGrid as TmDrawGrid).OnHeaderClick := OnHeaderClick;
  (FGrid as TmDrawGrid).OnColRowMoved := OnColRowMoved;
  (FGrid as TmDrawGrid).OnKeyDown := OnKeyDown;

  (FGrid as TmDrawGrid).DefaultRowHeight:= ScaleForMagnification((FGrid as TmDrawGrid).DefaultRowHeight, true);
  ScaleFontForMagnification((FGrid as TmDrawGrid).Font);

  FCurrentCol := -1;
  FCurrentRow := -1;
  FCurrentDrawingRow := -1;

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
  TWaitCursor.ShowWaitCursor('TmDrawGridHelper.ReadSettings');
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
          op.Width.Value := max(MINIMUM_GRID_COLUMN_WIDTH, (FGrid as TmDrawGrid).ColWidths[i]);
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
    TWaitCursor.UndoWaitCursor('TmDrawGridHelper.ReadSettings');
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


procedure TmDrawGridHelper.ApplySettings(aSettings: TmGridColumnsSettings);
var
  i: integer;
  curField: TmField;
  visibleCols: TFPList;
  widths: TIntegerList;
begin
  try
    TWaitCursor.ShowWaitCursor('TmDrawGridHelper.ApplySettings');

    (FGrid as TmDrawGrid).BeginUpdate;
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
              logger.Debug('[TmDrawGridHelper.ApplySettings] Missing field ' + TmGridColumnSettings(visibleCols.Items[i]).FieldName);
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
            logger.Debug('[TmDrawGridHelper.ApplySettings] Missing field ' + aSettings.Get(i).FieldName);
            {$ENDIF}
          end;
        end;

        RefreshDataProvider(false);

        for i := 0 to widths.Count - 1 do
          (FGrid as TmDrawGrid).ColWidths[i] := widths.Items[i];
      finally
        widths.Free;
      end;
    finally
      (FGrid as TmDrawGrid).EndUpdate(true);
    end;

    // (FGrid as TmDrawGrid).Invalidate;
  finally
    TWaitCursor.UndoWaitCursor('TmDrawGridHelper.ApplySettings');
  end;
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
  i, k, value : integer;
  index : TmIntegerDictionary;
begin
  aRows.Clear;

  if FProvider.GetRecordCount = 0 then
    exit;

  grid := (FGrid as TmDrawGrid);
  index := TmIntegerDictionary.Create(false);
  try
    for i := 0 to grid.SelectedRangeCount - 1 do
    begin
      for k := grid.SelectedRange[i].Top to grid.SelectedRange[i].Bottom do
      begin
        value := k - grid.FixedRows;
        if not index.Contains(value) then
        begin
          aRows.Add(value);
          index.Add(value, index);
        end;
      end;
    end;
  finally
    index.Free;
  end;
end;

procedure TmDrawGridHelper.AutoSizeColumns;
var
  grid: TmDrawGrid;
  curField: TmField;
  i, k, e : integer;
  w, ts : TSize;
  outOfBounds : boolean;
  str : string;
  tmpCanvas : TCanvas;
begin
  grid := (FGrid as TmDrawGrid);
  e := varCellpadding * 2;

  tmpCanvas := GetWorkingCanvas(grid.Canvas);
  try
    tmpCanvas.Font := grid.Font;
    for i := 0 to grid.ColCount -1 do
    begin
      w.Width := 0;

      curField := Self.ColToField(i);
      w := tmpCanvas.TextExtent(curField.DisplayLabel);
      w.Width:= w.Width + e;

      for k := 0 to FProvider.GetRecordCount - 1 do
      begin
        str := GetValueAsFormattedString(i, k, outOfBounds);
        ts := tmpCanvas.TextExtent(str);
        ts.Width := ts.Width + e;
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

function TmDrawGridHelper.CalculateHashCodeOfSelectedRows: string;
var
  list: TIntegerList;
  k: integer;
  tmp: string;
begin
  Result := '';
  list := TIntegerList.Create;
  try
    GetSelectedRows(list);
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
var
  r : TRect;
begin
  (FGrid as TmDrawGrid).BeginUpdate;
  try
    (FGrid as TmDrawGrid).ClearSelections;
    if FProvider.GetRecordCount > 0 then
    begin
      r.Left:= 0;
      r.Right:= FSortedVisibleCols.Count - 1;
      r.Top := (FGrid as TmDrawGrid).FixedRows;
      r.Bottom:= FProvider.GetRecordCount - 1 + (FGrid as TmDrawGrid).FixedRows;
      (FGrid as TmDrawGrid).Selection := r;
    end;
  finally
    (FGrid as TmDrawGrid).EndUpdate(true);
  end;
end;

procedure TmDrawGridHelper.SelectRows(const aKeyField: String; const aValues: TStringList);
begin

  raise Exception.Create('unimplemented');
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

initialization
// {$I lcl_dbgrid_customimages.lrs}
{$IFDEF DEBUG}
  logger := logManager.AddLog('mDrawGridHelper');
{$ENDIF}
end.
