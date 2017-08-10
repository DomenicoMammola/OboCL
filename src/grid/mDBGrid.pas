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
{$ENDIF}

interface

uses
  Classes, DBGrids, StdCtrls, Graphics, Forms, Controls, Menus, Math, contnrs,
  mGridColumnSettings, mXML, mGridSettingsForm, mSortConditions, mGridIcons,
  mDatasetInterfaces, mSystemColumns, mGridFilterValuesDlg;

resourcestring
  SFilterValuesMenuCaption = 'Filter values..';
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
    //
    FAllowSort : boolean;
    FAllowFilter : boolean;
    FColumnsHeaderMenuVisible : boolean;
    FCurrentGridCol : longint;
    FGridIcons: TmGridIconsDataModule;
    FColumnsHeaderPopupMenu : TPopupMenu;
    FOriginalPopupMenu : TPopupMenu;
    FSortManager : ISortableDatasetManager;
    FFilterManager : IFilterDatasetManager;
  strict private
    procedure BuildHeaderPopupMenu;
    procedure InternalOnTitleClick(Column: TColumn); // inspired by http://forum.lazarus.freepascal.org/index.php?topic=24510.0
    procedure InternalOnMouseDown (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); // https://www.codeproject.com/Articles/199506/Improving-Delphi-TDBGrid
    procedure SetColumnsHeaderMenuVisible(AValue: boolean);
    procedure ApplySettingsToField(aColumn: TColumn; aSettings : TmGridColumnSettings);
    procedure ExtractSettingsFromField(aColumn: TColumn; aSettings : TmGridColumnSettings);
    procedure SetFilterManager(AValue: IFilterDatasetManager);
    procedure SetSortManager(AValue: ISortableDatasetManager);
    procedure OnPColumnsHeaderMenuPopup (Sender : TObject);
    procedure OnFilterValues(Sender : TObject);
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
    //
    property ColumnsHeaderMenuVisible : boolean read FColumnsHeaderMenuVisible write SetColumnsHeaderMenuVisible;
    property SortManager : ISortableDatasetManager read FSortManager write SetSortManager;
    property FilterManager : IFilterDatasetManager read FFilterManager write SetFilterManager;
  end;

implementation

uses
  LResources;

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
    tmpMenuItem := TMenuItem.Create(FColumnsHeaderPopupMenu);
    tmpMenuItem.Caption:= SAddSummaryMenuCaption;
    tmpMenuItem.OnClick:=Self.OnFilterValues;
    FColumnsHeaderPopupMenu.Items.Add(tmpMenuItem);
    FColumnsHeaderPopupMenu.OnPopup := Self.OnPColumnsHeaderMenuPopup;
  end;
end;

// inspired by http://forum.lazarus.freepascal.org/index.php?topic=24510.0
procedure TmDBGrid.InternalOnTitleClick(Column: TColumn);
var
  tmpSortType : TSortType;
  i, idx : integer;
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
          Self.Columns[i].Title.ImageIndex := -1;

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
          if tmpSortType = stAscending then
            idx := 0
          else
            idx := 1;
          Self.Columns[Column.Index].Title.ImageIndex := idx;
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

procedure TmDBGrid.OnPColumnsHeaderMenuPopup(Sender: TObject);
begin
  //
end;

procedure TmDBGrid.OnFilterValues(Sender: TObject);
var
  dlg : TFilterValuesDlg;
  values : TStringList;
begin
  if Assigned(FFilterManager) then
  begin
    if Col >= -1 then
    begin
      values := TStringList.Create;
      dlg := TFilterValuesDlg.Create(Self);
      try
        FFilterManager.GetUniqueStringValuesForField(Columns[Col].FieldName, values);
        dlg.Init(values);
        if dlg.ShowModal = mrOk then
        begin

        end;
      finally
        dlg.Free;
        values.Free;
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
  Self.FAllowSort := false;
  Self.OnTitleClick:= InternalOnTitleClick;
  Self.OnMouseDown:= InternalOnMouseDown;
end;

destructor TmDBGrid.Destroy;
begin
  FCustomUncheckedBitmap.Free;
  FCustomCheckedBitmap.Free;
  FCustomGrayedBitmap.Free;
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
