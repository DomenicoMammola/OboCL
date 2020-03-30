// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mDBGrid;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$interfaces corba}
{$ENDIF}

interface

uses
  db, Classes, DBGrids, StdCtrls, Graphics, Forms, Controls, Menus, Math, variants, Grids, ExtCtrls,
  LCLVersion, ImgList,
  mGridColumnSettings, mXML, mSortConditions, mGridIcons,
  mDataProviderInterfaces, mFields, mFilter, mFilterOperators, mCellDecorations,
  mSummary, KAParser, mMaps, mGrids;

type

  { TBookmarkListHelper }

  TBookmarkListHelper = class helper for TBookmarkList
    function CalculateHashCode : string;
  end;

  { TmDBGridCursor }

  TmDBGridCursor = class (ImGridCursor)
  strict private
    FDBGrid : TDBGrid;
    FRecNo : LongInt;
  public
    constructor Create(aDBGrid : TDBGrid);

    procedure StartBrowsing;
    procedure EndBrowsing;
    procedure First;
    procedure Next;
    function EOF: boolean;
    function GetValueByFieldName(const aFieldName : String): Variant;
  end;

  { TmDBGrid }
  TmDBGrid = class (TDBGrid, ImGrid)
  strict private
    // custom bitmaps
    FCustomUncheckedBitmap : TBitmap;
    FCustomCheckedBitmap : TBitmap;
    FCustomGrayedBitmap : TBitmap;
    // bridges for overrided events
    FOnExtTitleClick: TDBGridClickEvent;
    FOnExtMouseDown: TMouseEvent;
//    FVirtualDatasetDataProvider: TmVirtualDatasetDataProvider;
    //
    FAllowSort : boolean;
    FColumnsHeaderMenuVisible : boolean;
    FCurrentGridCol : longint;
    FGridIcons: TmGridIconsDataModule;
    FCellDecorations : TmCellDecorations;
    FColumnsHeaderPopupMenu : TPopupMenu;
    FOriginalPopupMenu : TPopupMenu;
    FSortManager : ISortableDatasetManager;
    FFilterManager : IFilterDatasetManager;
    FCursor : TmDBGridCursor;
    // menu
    FMI_EditFilters: TMenuItem;
    FMI_RemoveAllFilters : TMenuItem;
    FMI_Summaries : TMenuItem;
    // Summary panel
    FSummaryPanel : ISummaryPanel;
  strict private
    FParser : TKAParser;
    FOnExpPrepareCanvas: TPrepareDbGridCanvasEvent;
    FSummaryManager: ISummaryDatasetManager;
    procedure BuildHeaderPopupMenu;
    procedure InternalOnTitleClick(Column: TColumn); // inspired by http://forum.lazarus.freepascal.org/index.php?topic=24510.0
    procedure InternalOnMouseDown (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); // https://www.codeproject.com/Articles/199506/Improving-Delphi-TDBGrid
    procedure InternalOnPrepareCanvas (sender: TObject; DataCol: Integer; Column: TColumn; AState: TGridDrawState);
    procedure InternalOnGetCheckBoxBitmap(Sender: TObject; const CheckedState: TCheckboxState; var ABitmap: TBitmap);
    procedure SetColumnsHeaderMenuVisible(AValue: boolean);
    procedure ApplySettingsToField(aColumn: TColumn; aSettings : TmGridColumnSettings);
    procedure ExtractSettingsFromField(aColumn: TColumn; aSettings : TmGridColumnSettings);
    procedure SetFilterManager(AValue: IFilterDatasetManager);
    procedure SetSortManager(AValue: ISortableDatasetManager);
    procedure SetSummaryManager(AValue: ISummaryDatasetManager);
    procedure OnColumnsHeaderMenuPopup (Sender : TObject);
    procedure OnFilterValues(Sender : TObject);
    procedure OnEditSummaries(Sender : TObject);
    procedure OnRemoveSummaries(Sender : TObject);
    procedure OnRemoveAllFilters (Sender : TObject);
    procedure OnEditFilters(Sender: TObject);
    procedure OnParserGetValue (Sender: TObject; const valueName: string; var Value: Double; out Successfull : boolean);
    procedure OnParserGetStrValue (Sender: TObject; const valueName: string; var StrValue: string; out Successfull : boolean);
    procedure RefreshSummaryPanel (Sender : TObject);
    procedure SetSummaryPanel(AValue: ISummaryPanel);
  protected
//    {$IF lcl_major>=2}
//    function GetImageForCheckBox(const aCol,aRow: Integer; CheckBoxView: TCheckBoxState): TBitmap; override;
//    {$ENDIF}
  protected
    property OnTitleClick; // hide the original event
    property OnMouseDown; // hide the original event
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // interface ImGrid:
    procedure ReadSettings(aSettings : TmGridColumnsSettings);
    procedure ApplySettings(aSettings : TmGridColumnsSettings);
    procedure RefreshDataProvider;
    function GetSummaryManager : ISummaryDatasetManager;
    procedure GetFields(aFields : TmFields);
    function GetDataCursor : ImGridCursor;
    procedure GetColumns(aColumns : TmGridColumns);

    // alias for original events
    property OnExtTitleClick: TDBGridClickEvent read FOnExtTitleClick write FOnExtTitleClick;
    property OnExtMouseDown: TMouseEvent read FOnExtMouseDown write FOnExtMouseDown;
    property OnExpPrepareCanvas : TPrepareDbGridCanvasEvent read FOnExpPrepareCanvas write FOnExpPrepareCanvas;
    //
    property ColumnsHeaderMenuVisible : boolean read FColumnsHeaderMenuVisible write SetColumnsHeaderMenuVisible;
    property SortManager : ISortableDatasetManager read FSortManager write SetSortManager;
    property FilterManager : IFilterDatasetManager read FFilterManager write SetFilterManager;
    property SummaryManager : ISummaryDatasetManager read FSummaryManager write SetSummaryManager;
    property CellDecorations : TmCellDecorations read FCellDecorations;
    property SummaryPanel : ISummaryPanel read FSummaryPanel write SetSummaryPanel;
  end;

implementation

uses
  LResources, sysutils, md5,
  mGridFilterValuesDlg, mGridFiltersEditDlg, mMagnificationFactor, mDataProviderUtility,
  mGraphicsUtility, mWaitCursor;

{ TmDBGridCursor }

constructor TmDBGridCursor.Create(aDBGrid: TDBGrid);
begin
  FDBGrid := aDBGrid;
end;

procedure TmDBGridCursor.StartBrowsing;
begin
  FRecNo:= FDBGrid.DataSource.DataSet.RecNo;
  FDBGrid.DataSource.DataSet.DisableControls;
end;

procedure TmDBGridCursor.EndBrowsing;
begin
  FDBGrid.DataSource.DataSet.RecNo:= FRecNo;
  FDBGrid.DataSource.DataSet.EnableControls;
end;

procedure TmDBGridCursor.First;
begin
  FDBGrid.DataSource.DataSet.First;
end;

procedure TmDBGridCursor.Next;
begin
  FDBGrid.DataSource.DataSet.Next;
end;

function TmDBGridCursor.EOF: boolean;
begin
  Result := FDBGrid.DataSource.DataSet.EOF;
end;

{ TBookmarkListHelper }

function TBookmarkListHelper.CalculateHashCode: string;
var
  i : integer;
  tmpBookmark : TBookMark;
begin
  Result := '';
  for i := 0 to Count - 1 do
  begin
    tmpBookmark := Items[i];
    Result := MD5Print( MD5String(Result + MD5Print(MD5Buffer(tmpBookmark, SizeOf(tmpBookmark)))));
  end;
end;

{ TmDBGrid }

procedure TmDBGrid.BuildHeaderPopupMenu;
var
  tmpMenuItem : TMenuItem;
  i : TmSummaryOperator;
begin
  if not Assigned(FColumnsHeaderPopupMenu) then
  begin
    FColumnsHeaderPopupMenu:= TPopupMenu.Create(Self);
    tmpMenuItem := TMenuItem.Create(FColumnsHeaderPopupMenu);
    tmpMenuItem.Caption:= SFilterValuesMenuCaption;
    tmpMenuItem.OnClick:=Self.OnFilterValues;
    FColumnsHeaderPopupMenu.Items.Add(tmpMenuItem);

    FMI_EditFilters:= TMenuItem.Create(FColumnsHeaderPopupMenu);
    FMI_EditFilters.Caption:= SEditFiltersMenuCaption;
    FMI_EditFilters.OnClick:= Self.OnEditFilters;
    FColumnsHeaderPopupMenu.Items.Add(FMI_EditFilters);

    FMI_RemoveAllFilters := TMenuItem.Create(FColumnsHeaderPopupMenu);
    FMI_RemoveAllFilters.Caption:= SRemoveFiltersMenuCaption;
    FMI_RemoveAllFilters.OnClick:=Self.OnRemoveAllFilters;
    FColumnsHeaderPopupMenu.Items.Add(FMI_RemoveAllFilters);

    tmpMenuItem := TMenuItem.Create(FColumnsHeaderPopupMenu);
    tmpMenuItem.Caption:= '-';
    FColumnsHeaderPopupMenu.Items.Add(tmpMenuItem);

    FMI_Summaries := TMenuItem.Create(FColumnsHeaderPopupMenu);
    FMI_Summaries.Caption:= SAddSummaryMenuCaption;
    FColumnsHeaderPopupMenu.Items.Add(FMI_Summaries);
    for i := Low(TmSummaryOperator) to High(TmSummaryOperator) do
    begin
      tmpMenuItem := TMenuItem.Create(FColumnsHeaderPopupMenu);
      tmpMenuItem.Caption:= TmSummaryOperatorToString(i);
      tmpMenuItem.OnClick:= Self.OnEditSummaries;
      tmpMenuItem.Tag:= ptrInt(i);
      FMI_Summaries.Add(tmpMenuItem);
    end;

    tmpMenuItem := TMenuItem.Create(FColumnsHeaderPopupMenu);
    tmpMenuItem.Caption:= SRemoveSummariesMenuCaption;
    tmpMenuItem.OnClick:=Self.OnRemoveSummaries;
    FColumnsHeaderPopupMenu.Items.Add(tmpMenuItem);

    FColumnsHeaderPopupMenu.OnPopup := Self.OnColumnsHeaderMenuPopup;
  end;
end;

// inspired by http://forum.lazarus.freepascal.org/index.php?topic=24510.0
procedure TmDBGrid.InternalOnTitleClick(Column: TColumn);
var
  tmpSortType : TSortType;
  i : integer;
begin
  try
    if FAllowSort then
    begin
      // it is necessary to clear old selection
      Self.SelectedRows.Clear;

      try
        TWaitCursor.ShowWaitCursor('TmDBGrid.InternalOnTitleClick');
        tmpSortType := stAscending;

        // remove every arrow from column captions
        for i := 0 to Self.Columns.Count - 1 do
        begin
          case Self.Columns[i].Title.ImageIndex of
            GRID_ICON_DOWN, GRID_ICON_UP :
              Self.Columns[i].Title.ImageIndex := -1;
            GRID_ICON_UP_FILTER, GRID_ICON_DOWN_FILTER:
              Self.Columns[i].Title.ImageIndex := GRID_ICON_FILTER;
          end;
        end;

        // analize current filter
        if (FSortManager.GetSorted) and (FSortManager.GetSortByConditions.Count > 0) and (FSortManager.GetSortByConditions.Items[0].FieldName = Column.FieldName) then
        begin
          if FSortManager.GetSortByConditions.Items[0].SortType = stAscending then
            tmpSortType:= stDescending
          else
          begin
            FSortManager.ClearSort;
            exit;
          end
        end;

        // set new sort condition
        FSortManager.GetSortByConditions.Clear;
        with FSortManager.GetSortByConditions.Add do
        begin
          FieldName:= Column.FieldName;
          SortType:= tmpSortType;
        end;

        // do sort
        if FSortManager.Sort then
        begin
          if Self.Columns[Column.Index].Title.ImageIndex = GRID_ICON_FILTER then
          begin
            if tmpSortType = stAscending then
              Self.Columns[Column.Index].Title.ImageIndex := GRID_ICON_UP_FILTER
            else
              Self.Columns[Column.Index].Title.ImageIndex := GRID_ICON_DOWN_FILTER;
          end
          else
          begin
            if tmpSortType = stAscending then
              Self.Columns[Column.Index].Title.ImageIndex := GRID_ICON_UP
            else
              Self.Columns[Column.Index].Title.ImageIndex := GRID_ICON_DOWN;
          end;
        end;
      finally
        TWaitCursor.UndoWaitCursor('TmDBGrid.InternalOnTitleClick');
      end;
    end;
  finally
    if Assigned(FOnExtTitleClick) then
      FOnExtTitleClick(Column);
  end;
end;

procedure TmDBGrid.InternalOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  tmpCol, tmpRow : longint;
begin
  FCurrentGridCol := -1;
  if FColumnsHeaderMenuVisible then
  begin
    // https://www.codeproject.com/Articles/199506/Improving-Delphi-TDBGrid
    if Button = mbRight then
    begin
      if Y < Self.DefaultRowHeight then
      begin
        FOriginalPopupMenu := Self.PopupMenu;
        Self.PopupMenu := FColumnsHeaderPopupMenu;
        tmpCol := 0;
        tmpRow := 0;
        Self.MouseToCell(X, Y, tmpCol, tmpRow);
        FCurrentGridCol := tmpCol;
      end
      else
      begin
        if Assigned(FOriginalPopupMenu) then
          Self.PopupMenu := FOriginalPopupMenu
        else if Self.PopupMenu = FColumnsHeaderPopupMenu then
          Self.PopupMenu := nil;
      end;
    end;
  end;

  if Assigned(FOnExtMouseDown) then
    FOnExtMouseDown(Sender, Button, Shift, X, Y);
end;

procedure TmDBGrid.InternalOnPrepareCanvas(sender: TObject; DataCol: Integer; Column: TColumn; AState: TGridDrawState);
var
  tmpCellDecoration : TmCellDecoration;
  tmpListOfDecorations : TmListOfDecorations;
  PerformCustomizedDraw : boolean;
  tmpValue : double;
  i : integer;
begin
  FCellDecorations.FindByFieldName(Column.FieldName, tmpListOfDecorations);
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
             Canvas.Brush.Color := LighterColor(tmpCellDecoration.BackgroundColor.Value, 70)
           else
             Canvas.Brush.Color := DarkerColor(tmpCellDecoration.BackgroundColor.Value, 20);
         end
         else
           Canvas.Brush.Color := tmpCellDecoration.BackgroundColor.Value;
       end;
       if tmpCellDecoration.TextColor.NotNull then
         Canvas.Font.Color:= tmpCellDecoration.TextColor.Value;
       if tmpCellDecoration.TextBold.NotNull and tmpCellDecoration.TextBold.Value then
         Canvas.Font.Style:= Canvas.Font.Style + [fsBold];
       if tmpCellDecoration.TextItalic.NotNull and tmpCellDecoration.TextItalic.Value then
         Canvas.Font.Style:= Canvas.Font.Style + [fsItalic];

       break;
     end;
    end;
  end;

  if Assigned(FOnExpPrepareCanvas) then
    FOnExpPrepareCanvas(sender, DataCol, Column, AState);
end;


procedure TmDBGrid.SetColumnsHeaderMenuVisible(AValue: boolean);
begin
  if FColumnsHeaderMenuVisible=AValue then Exit;
  FColumnsHeaderMenuVisible:=AValue;
  if not Assigned(FColumnsHeaderPopupMenu) then
    Self.BuildHeaderPopupMenu;
end;

procedure TmDBGrid.ApplySettingsToField(aColumn: TColumn; aSettings: TmGridColumnSettings);
begin
  if aSettings.Visible.NotNull then
  begin
    if aColumn.Visible <> aSettings.Visible.Value then
    begin
      aColumn.Visible := aSettings.Visible.Value;
      if aColumn.Visible then
        aColumn.Width:= max(aColumn.Width, MINIMUM_GRID_COLUMN_WIDTH);
    end;
  end;
  if aSettings.DisplayFormat.NotNull then
    aColumn.DisplayFormat := aSettings.DisplayFormat.Value;
  if aSettings.DisplayLabel.NotNull then
  begin
    aColumn.Title.Caption := aSettings.DisplayLabel.Value;
    {$IFDEF DEBUG_COL_SET}DebugLn('[ApplySettingsToField] ' + aSettings.FieldName + ' ' +aColumn.Title.Caption);{$ENDIF}
  end;
  if aSettings.Width.NotNull then
    aColumn.Width:= max(aSettings.Width.Value, MINIMUM_GRID_COLUMN_WIDTH);
  if aSettings.SortOrder.NotNull then
    aColumn.Index := min(aSettings.SortOrder.Value, Self.Columns.Count - 1);
end;

procedure TmDBGrid.ExtractSettingsFromField(aColumn: TColumn; aSettings: TmGridColumnSettings);
begin
  aSettings.Visible.Value:= aColumn.Visible;
  aSettings.DisplayFormat.Value:= aColumn.DisplayFormat;
  aSettings.DisplayLabel.Value:= aColumn.Title.Caption;
  aSettings.SortOrder.Value:= aColumn.Index;
  aSettings.Width.Value:= max(MINIMUM_GRID_COLUMN_WIDTH, aColumn.Width);
end;

procedure TmDBGrid.SetFilterManager(AValue: IFilterDatasetManager);
begin
  if FFilterManager=AValue then Exit;
  FFilterManager:=AValue;
//  FAllowFilter:= Assigned(FFilterManager);
end;

procedure TmDBGrid.SetSortManager(AValue: ISortableDatasetManager);
begin
  if FSortManager=AValue then Exit;
  FSortManager:=AValue;
  FAllowSort:= Assigned(FSortManager);
  if FAllowSort and (not Assigned(FGridIcons)) then
  begin
    FGridIcons:= TmGridIconsDataModule.Create(Self);
    Self.TitleImageList := FGridIcons.GridImageList;
  end;
end;

procedure TmDBGrid.OnColumnsHeaderMenuPopup(Sender: TObject);
var
  currentColumn : TColumn;
  currentOperator : TmSummaryOperator;
  tmpDef : TmSummaryDefinition;
  i : integer;
begin
  FMI_RemoveAllFilters.Enabled := Self.FilterManager.GetFiltered;
  FMI_EditFilters.Enabled:= FMI_RemoveAllFilters.Enabled;
  for i := 0 to FMI_Summaries.Count - 1 do
  begin
    FMI_Summaries.Items[i].Checked:= false;
  end;
  if Assigned(FSummaryManager) then
  begin
    if (FCurrentGridCol > 0) and (FCurrentGridCol <= Columns.Count) then
    begin
      currentColumn := Columns[FCurrentGridCol-1];

      for i := 0 to FMI_Summaries.Count - 1 do
      begin
        currentOperator := TmSummaryOperator(FMI_Summaries.Items[i].Tag);
        tmpDef := FSummaryManager.GetSummaryDefinitions.FindByFieldNameAndOperator(currentColumn.FieldName, currentOperator);
        FMI_Summaries.Items[i].Checked := Assigned(tmpDef);
      end;
    end;
  end;
end;

procedure TmDBGrid.OnFilterValues(Sender: TObject);
var
  dlg : TFilterValuesDlg;
  values : TStringList;
  checkedValues : TStringList;
  i : integer;
  tmpFilter : TmFilter;
  tmpVariant : Variant;
  currentColumn : TColumn;
begin
  if Assigned(FFilterManager) then
  begin
    if FCurrentGridCol > 0 then
    begin
      currentColumn := Columns[FCurrentGridCol-1];
      values := TStringList.Create;
      dlg := TFilterValuesDlg.Create(Self);
      try
        try
          TWaitCursor.ShowWaitCursor('TmDBGrid.OnFilterValues');
          FFilterManager.GetUniqueStringValuesForField(currentColumn.FieldName, values);
          dlg.Init(values);
        finally
          TWaitCursor.UndoWaitCursor('TmDBGrid.OnFilterValues');
        end;
        if dlg.ShowModal = mrOk then
        begin
          checkedValues := TStringList.Create;
          try
            dlg.GetCheckedValues(checkedValues);
            Self.FilterManager.GetFilters.ClearForField(currentColumn.FieldName);
            if checkedValues.Count > 0 then
            begin
              // it is necessary to clear old selection
              Self.SelectedRows.Clear;
              try
                TWaitCursor.ShowWaitCursor('TmDBGrid.OnFilterValues');
                tmpFilter := Self.FilterManager.GetFilters.Add;
                tmpFilter.FieldName:= currentColumn.FieldName;
                tmpFilter.FilterOperator:= foIn;
                tmpVariant := variants.VarArrayCreate([0, checkedValues.Count - 1], varOleStr);
                for i := 0 to checkedValues.Count -1 do
                  VarArrayPut(tmpVariant, checkedValues.Strings[i], [i]);
                tmpFilter.Value:= tmpVariant;
                if Self.FilterManager.DoFilter then
                begin
                  // update icons..
                  if currentColumn.Title.ImageIndex = GRID_ICON_UP then
                    currentColumn.Title.ImageIndex:= GRID_ICON_UP_FILTER
                  else if currentColumn.Title.ImageIndex = GRID_ICON_DOWN then
                    currentColumn.Title.ImageIndex:= GRID_ICON_DOWN_FILTER
                  else
                    currentColumn.Title.ImageIndex:= GRID_ICON_FILTER;
                end;
              finally
                TWaitCursor.UndoWaitCursor('TmDBGrid.OnFilterValues');
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
end;

procedure TmDBGrid.OnEditSummaries(Sender: TObject);
var
  currentColumn : TColumn;
  currentOperator : TmSummaryOperator;
  tmpDef : TmSummaryDefinition;
begin
  if Assigned(FSummaryManager) then
  begin
    if FCurrentGridCol > 0 then
    begin
      currentColumn := Columns[FCurrentGridCol-1];
      currentOperator := TmSummaryOperator((Sender as TMenuItem).Tag);

      tmpDef := FSummaryManager.GetSummaryDefinitions.FindByFieldNameAndOperator(currentColumn.FieldName, currentOperator);
      if Assigned(tmpDef) then
      begin
        FSummaryManager.GetSummaryDefinitions.Remove(tmpDef);
      end
      else
      begin
        tmpDef := FSummaryManager.GetSummaryDefinitions.Add;
        tmpDef.FieldName:= currentColumn.FieldName;
        tmpDef.FieldType:= currentColumn.Field.DataType;
        tmpDef.Caption := currentColumn.Title.Caption;
        tmpDef.SummaryOperator:= currentOperator;
      end;
      FSummaryManager.RefreshSummaries;
    end;
  end;
end;

procedure TmDBGrid.OnRemoveSummaries(Sender: TObject);
begin
  FSummaryManager.GetSummaryDefinitions.Clear;
  FSummaryManager.RefreshSummaries;
end;

procedure TmDBGrid.OnRemoveAllFilters(Sender: TObject);
var
  i : integer;
begin
  Self.FilterManager.RemoveFilter;
  for i := 0 to Columns.Count - 1 do
  begin
    if Columns[i].Title.ImageIndex = GRID_ICON_UP_FILTER then
      Columns[i].Title.ImageIndex := GRID_ICON_UP
    else if Columns[i].Title.ImageIndex = GRID_ICON_DOWN_FILTER then
      Columns[i].Title.ImageIndex := GRID_ICON_DOWN
    else
      Columns[i].Title.ImageIndex := -1;
  end;
end;

procedure TmDBGrid.OnEditFilters(Sender: TObject);
var
  dlg: TFiltersEditDlg;
  removedFilters: TStringList;
  i: integer;
begin
  if not Assigned(Self.FilterManager) then
    exit;

  if not Self.FilterManager.GetFiltered then
    exit;
  dlg := TFiltersEditDlg.Create(Self);
  try
    dlg.Init(FFilterManager.GetFilters, Self.DataSource.DataSet);
    if dlg.ShowModal = mrOk then
    begin
      removedFilters:= TStringList.Create;
      try
        dlg.GetRemovedFilterConditions(removedFilters);
        if removedFilters.Count > 0 then
        begin
          // it is necessary to clear old selection
          Self.SelectedRows.Clear;
          FFilterManager.RemoveFilterForFields(removedFilters);
          for i := 0 to Columns.Count - 1 do
          begin
            if removedFilters.IndexOf(Columns[i].Field.FieldName) >= 0 then
            begin
              if Columns[i].Title.ImageIndex = GRID_ICON_UP_FILTER then
                Columns[i].Title.ImageIndex := GRID_ICON_UP
              else if Columns[i].Title.ImageIndex = GRID_ICON_DOWN_FILTER then
                Columns[i].Title.ImageIndex := GRID_ICON_DOWN
              else
                Columns[i].Title.ImageIndex := -1;
            end;
          end;
        end;
      finally
        removedFilters.Free;
      end;
    end;
  finally
    dlg.Free;
  end;
end;

procedure TmDBGrid.OnParserGetValue(Sender: TObject; const valueName: string; var Value: Double; out Successfull: boolean);
var
  tmpField : TField;
begin
  Successfull:= false;
  if Assigned(Self.DataSource) and Assigned(Self.DataSource.DataSet) then
  begin
    Value := 0;
    if DataSource.DataSet.IsEmpty then
      Successfull:= true
    else
    begin
      tmpField := DataSource.DataSet.FieldByName(valueName);
      if Assigned(tmpField) then
      begin
        if not tmpField.IsNull then
        begin
          if tmpField.DataType = ftBoolean then
          begin
            if tmpField.AsBoolean then
              Value := 1
            else
              Value := 0;
          end
          else
            Value := tmpField.AsFloat;
        end;
        Successfull:= true;
      end;
    end;
  end;
end;

procedure TmDBGrid.OnParserGetStrValue(Sender: TObject; const valueName: string; var StrValue: string; out Successfull: boolean);
var
  tmpField: TField;
begin
  Successfull:= false;
  if Assigned(Self.DataSource) and Assigned(Self.DataSource.DataSet) then
  begin
    StrValue := '';
    if DataSource.DataSet.IsEmpty then
      Successfull:= true
    else
    begin
      tmpField := DataSource.DataSet.FieldByName(valueName);
      if Assigned(tmpField) then
      begin
        if not tmpField.IsNull then
          StrValue := tmpField.AsString;
        Successfull:= true;
      end;
    end;
  end;
end;

procedure TmDBGrid.InternalOnGetCheckBoxBitmap(Sender: TObject; const CheckedState: TCheckboxState; var ABitmap: TBitmap);
begin
  if CheckedState=cbUnchecked then
    ABitmap := FCustomUncheckedBitmap
  else if CheckedState=cbChecked then
    ABitmap := FCustomCheckedBitmap
  else
    ABitmap := FCustomGrayedBitmap;
end;

procedure TmDBGrid.RefreshSummaryPanel(Sender : TObject);
begin
  mDataProviderUtility.RefreshSummaryPanel(FSummaryManager, FSummaryPanel);
end;

procedure TmDBGrid.SetSummaryPanel(AValue: ISummaryPanel);
begin
  if FSummaryPanel=AValue then Exit;
  FSummaryPanel:=AValue;
  Self.RefreshSummaryPanel(nil);
end;

procedure TmDBGrid.SetSummaryManager(AValue: ISummaryDatasetManager);
begin
  if FSummaryManager=AValue then Exit;
  FSummaryManager:=AValue;
  FSummaryManager.RegisterListener(Self.RefreshSummaryPanel);
end;

(*
function TmDBGrid.GetImageForCheckBox(const aCol, aRow: Integer; CheckBoxView: TCheckBoxState): TBitmap;
begin
  if CheckboxView=cbUnchecked then
    Result := FCustomUncheckedBitmap
  else if CheckboxView=cbChecked then
    Result := FCustomCheckedBitmap
  else
    Result := FCustomGrayedBitmap;
end;*)

constructor TmDBGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCustomUnCheckedBitmap := TBitmap.Create;
  FCustomUnCheckedBitmap.LoadFromLazarusResource('dbgridcustomuncheckedcb');
  FCustomCheckedBitmap := TBitmap.Create;
  FCustomCheckedBitmap.LoadFromLazarusResource('dbgridcustomcheckedcb');
  FCustomGrayedBitmap := TBitmap.Create;
  FCustomGrayedBitmap.LoadFromLazarusResource('dbgridcustomgrayedcb');
  FCellDecorations := TmCellDecorations.Create;
  Self.FAllowSort := false;
  Self.OnTitleClick:= InternalOnTitleClick;
  Self.OnMouseDown:= InternalOnMouseDown;
  Self.OnPrepareCanvas:= InternalOnPrepareCanvas;
  Self.OnUserCheckboxBitmap := InternalOnGetCheckBoxBitmap;
  ScaleFontForMagnification(Self.Font);
  FCursor := TmDBGridCursor.Create(Self);
end;

destructor TmDBGrid.Destroy;
begin
  FCellDecorations.Free;
  FCustomUncheckedBitmap.Free;
  FCustomCheckedBitmap.Free;
  FCustomGrayedBitmap.Free;
  FreeAndNil(FParser);
  FCursor.Free;
  inherited Destroy;
end;


procedure TmDBGrid.ReadSettings(aSettings: TmGridColumnsSettings);
var
  op : TmGridColumnSettings;
  i : integer;
begin
  for i := 0 to Self.Columns.Count - 1 do
  begin
    if not IsSystemField(Self.Columns.Items[i].Field.FieldName) then
    begin
      op := aSettings.AddSettingsForField(Self.Columns.Items[i].FieldName);
      ExtractSettingsFromField( Self.Columns.Items[i], op);
    end;
  end;
end;

procedure TmDBGrid.ApplySettings(aSettings: TmGridColumnsSettings);
var
  i : integer;
  tmpDictionary : TmStringDictionary;
  tmpList : TList;
  tmpObj : TObject;
begin
  try
    TWaitCursor.ShowWaitCursor('TmDBGrid.ApplySettings');

    Self.BeginUpdate;
    try
      tmpDictionary := TmStringDictionary.Create();
      tmpList := TList.Create;
      try
        for i := 0 to Self.Columns.Count - 1 do
        begin
          if not IsSystemField (Self.Columns.Items[i].Field.FieldName) then
          begin
            if Assigned(aSettings.GetSettingsForField(Self.Columns.Items[i].Field.FieldName)) then
              tmpDictionary.Add(Self.Columns.Items[i].Field.FieldName, Self.Columns.Items[i])
            else
              tmpList.Add(Self.Columns.Items[i]);
          end
          else
            tmpList.Add(Self.Columns.Items[i]);
        end;

        for i := 0 to tmpList.Count - 1 do
          TColumn(tmpList.Items[i]).Index:= Columns.Count - 1;

        for i := aSettings.Count - 1 downto 0 do
        begin
          tmpObj :=  tmpDictionary.Find(aSettings.Get(i).FieldName);
          if Assigned(tmpObj) then
            ApplySettingsToField(tmpObj as TColumn, aSettings.Get(i));
        end;
      finally
        tmpList.Free;
        tmpDictionary.Free;;
      end;
    finally
      Self.EndUpdate;
    end;
  finally
    TWaitCursor.UndoWaitCursor('TmDBGrid.ApplySettings');
  end;
end;

procedure TmDBGrid.RefreshDataProvider;
begin
  Self.DataSource.DataSet.Close;
  Self.DataSource.DataSet.Open;
end;

function TmDBGrid.GetSummaryManager: ISummaryDatasetManager;
begin
  Result := FSummaryManager;
end;

procedure TmDBGrid.GetFields(aFields: TmFields);
var
  i : integer;
begin
  aFields.Clear;
  for i := 0 to Self.DataSource.DataSet.Fields.Count - 1 do
    aFields.Add.AssignFromField(Self.DataSource.DataSet.Fields[i]);
end;

function TmDBGrid.GetDataCursor: ImGridCursor;
begin
  Result := FCursor;
end;

procedure TmDBGrid.GetColumns(aColumns: TmGridColumns);
var
  i : integer;
begin
  aColumns.Clear;
  for i := 0 to Self.Columns.Count - 1 do
    aColumns.Add.Assign(Self.Columns.Items[i]);
end;

function TmDBGridCursor.GetValueByFieldName(const aFieldName: String): Variant;
begin
  Result := FDBGrid.DataSource.DataSet.FieldByName(aFieldName).Value;
end;


initialization
  {$I lcl_dbgrid_customimages.lrs}
end.
