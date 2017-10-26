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
  db, Classes, DBGrids, StdCtrls, Graphics, Forms, Controls, Menus, Math, contnrs, variants, Grids,
  mGridColumnSettings, mXML, mGridSettingsForm, mSortConditions, mGridIcons,
  mDatasetInterfaces, mSystemColumns, mGridFilterValuesDlg, mFilter, mFilterOperators, mCellDecorations,
  KAParser;

resourcestring
  SFilterValuesMenuCaption = 'Filter by value..';
  SRemoveFiltersMenuCaption = 'Remove all filters';
  SAddSummaryMenuCaption = 'Add summary..';


type
  { TmDBGrid }

  TmDBGrid = class(TDBGrid)
  strict private
    // custom bitmaps
    FCustomUncheckedBitmap : TBitmap;
    FCustomCheckedBitmap : TBitmap;
    FCustomGrayedBitmap : TBitmap;
    // bridges for overrided events
    FOnExtTitleClick: TDBGridClickEvent;
    FOnExtMouseDown: TMouseEvent;
    FOnExtPrepareCanvas : TPrepareDbGridCanvasEvent;
    //
    FAllowSort : boolean;
    FAllowFilter : boolean;
    FColumnsHeaderMenuVisible : boolean;
    FCurrentGridCol : longint;
    FGridIcons: TmGridIconsDataModule;
    FCellDecorations : TmCellDecorations;
    FColumnsHeaderPopupMenu : TPopupMenu;
    FOriginalPopupMenu : TPopupMenu;
    FSortManager : ISortableDatasetManager;
    FFilterManager : IFilterDatasetManager;
    // menu
    FMI_RemoveAllFilters : TMenuItem;
  strict private
    FParser : TKAParser;
    FOnExpPrepareCanvas: TPrepareDbGridCanvasEvent;
    procedure BuildHeaderPopupMenu;
    procedure InternalOnTitleClick(Column: TColumn); // inspired by http://forum.lazarus.freepascal.org/index.php?topic=24510.0
    procedure InternalOnMouseDown (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); // https://www.codeproject.com/Articles/199506/Improving-Delphi-TDBGrid
    procedure InternalOnPrepareCanvas (sender: TObject; DataCol: Integer; Column: TColumn; AState: TGridDrawState);
    procedure SetColumnsHeaderMenuVisible(AValue: boolean);
    procedure ApplySettingsToField(aColumn: TColumn; aSettings : TmGridColumnSettings);
    procedure ExtractSettingsFromField(aColumn: TColumn; aSettings : TmGridColumnSettings);
    procedure SetFilterManager(AValue: IFilterDatasetManager);
    procedure SetSortManager(AValue: ISortableDatasetManager);
    procedure OnColumnsHeaderMenuPopup (Sender : TObject);
    procedure OnFilterValues(Sender : TObject);
    procedure OnRemoveAllFilters (Sender : TObject);
    procedure OnParserGetValue (Sender: TObject; const valueName: string; var Value: Double; out Successfull : boolean);
    procedure OnParserGetStrValue (Sender: TObject; const valueName: string; var StrValue: string; out Successfull : boolean);
  protected
    function GetImageForCheckBox(const aCol,aRow: Integer; CheckBoxView: TCheckBoxState): TBitmap; override;
  protected
    property OnTitleClick; // hide the original event
    property OnMouseDown; // hide the original event
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ReadSettings(aSettings : TmGridColumnsSettings);
    procedure ApplySettings(aSettings : TmGridColumnsSettings);

    // alias for original events
    property OnExtTitleClick: TDBGridClickEvent read FOnExtTitleClick write FOnExtTitleClick;
    property OnExtMouseDown: TMouseEvent read FOnExtMouseDown write FOnExtMouseDown;
    property OnExpPrepareCanvas : TPrepareDbGridCanvasEvent read FOnExpPrepareCanvas write FOnExpPrepareCanvas;
    //
    property ColumnsHeaderMenuVisible : boolean read FColumnsHeaderMenuVisible write SetColumnsHeaderMenuVisible;
    property SortManager : ISortableDatasetManager read FSortManager write SetSortManager;
    property FilterManager : IFilterDatasetManager read FFilterManager write SetFilterManager;
    property Row;
    property CellDecorations : TmCellDecorations read FCellDecorations;
  end;

implementation

uses
  LResources, sysutils;

{ TmDBGrid }

procedure TmDBGrid.BuildHeaderPopupMenu;
var
  tmpMenuItem : TMenuItem;
begin
  if not Assigned(FColumnsHeaderPopupMenu) then
  begin
    FColumnsHeaderPopupMenu:= TPopupMenu.Create(Self);
    tmpMenuItem := TMenuItem.Create(FColumnsHeaderPopupMenu);
    tmpMenuItem.Caption:= SFilterValuesMenuCaption;
    tmpMenuItem.OnClick:=Self.OnFilterValues;
    FColumnsHeaderPopupMenu.Items.Add(tmpMenuItem);

    FMI_RemoveAllFilters := TMenuItem.Create(FColumnsHeaderPopupMenu);
    FMI_RemoveAllFilters.Caption:= SRemoveFiltersMenuCaption;
    FMI_RemoveAllFilters.OnClick:=Self.OnRemoveAllFilters;
    FColumnsHeaderPopupMenu.Items.Add(FMI_RemoveAllFilters);

    tmpMenuItem := TMenuItem.Create(FColumnsHeaderPopupMenu);
    tmpMenuItem.Caption:= SAddSummaryMenuCaption;
    //tmpMenuItem.OnClick:=Self.OnFilterValues;
    FColumnsHeaderPopupMenu.Items.Add(tmpMenuItem);

    FColumnsHeaderPopupMenu.OnPopup := Self.OnColumnsHeaderMenuPopup;
  end;
end;

// inspired by http://forum.lazarus.freepascal.org/index.php?topic=24510.0
procedure TmDBGrid.InternalOnTitleClick(Column: TColumn);
var
  tmpSortType : TSortType;
  i : integer;
  OldCursor : TCursor;
begin
  try
    if FAllowSort then
    begin
      OldCursor := Screen.Cursor;
      try
        Screen.Cursor := crHourGlass;
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
        Screen.Cursor := OldCursor;
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
  PerformCustomizedDraw : boolean;
  tmpGrid : TDBGrid;
  tmpValue : double;
begin
  tmpCellDecoration := FCellDecorations.FindByFieldName(Column.FieldName);
  if Assigned(tmpCellDecoration) then
  begin
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
      tmpGrid := (Sender as TDBGrid);
      if tmpCellDecoration.BackgroundColor.NotNull then
        Canvas.Brush.Color := tmpCellDecoration.BackgroundColor.Value;
      if tmpCellDecoration.TextColor.NotNull then
        Canvas.Font.Color:= tmpCellDecoration.TextColor.Value;
      if tmpCellDecoration.TextBold.NotNull and tmpCellDecoration.TextBold.Value then
        Canvas.Font.Style:= Canvas.Font.Style + [fsBold];
      if tmpCellDecoration.TextItalic.NotNull and tmpCellDecoration.TextItalic.Value then
        Canvas.Font.Style:= Canvas.Font.Style + [fsItalic];
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
    aColumn.Index := aSettings.SortOrder.Value;
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
  FAllowFilter:= Assigned(FFilterManager);
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
begin
  FMI_RemoveAllFilters.Enabled := Self.FilterManager.GetFiltered;
end;

procedure TmDBGrid.OnFilterValues(Sender: TObject);
var
  dlg : TFilterValuesDlg;
  values : TStringList;
  checkedValues : TStringList;
  OldCursor : TCursor;
  i : integer;
  tmpFilter : TmFilter;
  tmpVariant : Variant;
  currentColumn : TColumn;
begin
  if Assigned(FFilterManager) then
  begin
    if FCurrentGridCol >= -1 then
    begin
      currentColumn := Columns[FCurrentGridCol-1];
      values := TStringList.Create;
      dlg := TFilterValuesDlg.Create(Self);
      try
        OldCursor := Screen.Cursor;
        try
          Screen.Cursor := crHourGlass;
          FFilterManager.GetUniqueStringValuesForField(currentColumn.FieldName, values);
          dlg.Init(values);
        finally
          Screen.Cursor:= OldCursor;
        end;
        if dlg.ShowModal = mrOk then
        begin
          checkedValues := TStringList.Create;
          try
            dlg.GetCheckedValues(checkedValues);
            Self.FilterManager.GetFilters.ClearForField(currentColumn.FieldName);
            if checkedValues.Count > 0 then
            begin
              OldCursor := Screen.Cursor;
              try
                Screen.Cursor := crHourGlass;
                tmpFilter := Self.FilterManager.GetFilters.Add;
                tmpFilter.FieldName:= currentColumn.FieldName;
                tmpFilter.FilterOperator:= foIn;
                tmpVariant := variants.VarArrayCreate([0, checkedValues.Count - 1], varOleStr);
                for i := 0 to checkedValues.Count -1 do
                  VarArrayPut(tmpVariant, checkedValues.Strings[i], [i]);
                tmpFilter.Value:= tmpVariant;
                if Self.FilterManager.Filter then
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
                Screen.Cursor:= OldCursor;
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

procedure TmDBGrid.OnRemoveAllFilters(Sender: TObject);
var
  i : integer;
begin
  Self.FilterManager.ClearFilter;
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
          Value := tmpField.AsFloat;
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

function TmDBGrid.GetImageForCheckBox(const aCol, aRow: Integer; CheckBoxView: TCheckBoxState): TBitmap;
begin
  if CheckboxView=cbUnchecked then
    Result := FCustomUncheckedBitmap
  else if CheckboxView=cbChecked then
    Result := FCustomCheckedBitmap
  else
    Result := FCustomGrayedBitmap;
end;

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
end;

destructor TmDBGrid.Destroy;
begin
  FCellDecorations.Free;
  FCustomUncheckedBitmap.Free;
  FCustomCheckedBitmap.Free;
  FCustomGrayedBitmap.Free;
  FreeAndNil(FParser);
  inherited Destroy;
end;


procedure TmDBGrid.ReadSettings(aSettings: TmGridColumnsSettings);
var
  op : TmGridColumnSettings;
  i : integer;
begin
  for i := 0 to Self.Columns.Count - 1 do
  begin
    if not IsSystemField(Self.Columns.Items[i].Field) then
    begin
      op := aSettings.AddSettingsForField(Self.Columns.Items[i].FieldName);
      ExtractSettingsFromField( Self.Columns.Items[i], op);
    end;
  end;
end;

procedure TmDBGrid.ApplySettings(aSettings: TmGridColumnsSettings);
var
  op : TmGridColumnSettings;
  i : integer;
  tmpList : TObjectList;
begin
  tmpList := TObjectList.Create(false);
  try
    for i := 0 to Self.Columns.Count - 1 do
    begin
      tmpList.Add(Self.Columns.Items[i]);
    end;
    for i := 0 to tmpList.Count - 1 do
    begin
      if not IsSystemField ((tmpList.Items[i] as TColumn).Field) then
      begin
        op := aSettings.GetSettingsForField((tmpList.Items[i] as TColumn).FieldName);
        if Assigned(op) then
          ApplySettingsToField(tmpList.Items[i] as TColumn, op);
      end;
    end;
  finally
    tmpList.Free;
  end;
end;


initialization
  {$I lcl_dbgrid_customimages.lrs}
end.
