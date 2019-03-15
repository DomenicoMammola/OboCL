// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mDrawGrid;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$interfaces corba}
{$ENDIF}

interface

uses
  db, Classes, Grids, StdCtrls, Graphics, Forms, Controls, Menus, Math, variants, ExtCtrls,
  LCLVersion, ImgList, contnrs,
  mGridColumnSettings, mXML, mSortConditions, mGridIcons,
  mDataProviderInterfaces, mFields, mFilter, mFilterOperators, mCellDecorations,
  mSummary, KAParser, mMaps, mGrids, mDataProviderFieldDefs;

type

  { TmDrawGridCursor }

  TmDrawGridCursor = class (ImGridCursor)
  strict private
    FDataProvider : IVDDataProvider;
    FCurrentRecNo : Longint;
  public
    constructor Create(aDataProvider : IVDDataProvider);

    procedure StartBrowsing;
    procedure EndBrowsing;
    procedure First;
    procedure Next;
    function EOF: boolean;
    function GetValueByFieldName(const aFieldName : String): Variant;
  end;


  { TmDrawGrid }
  TmDrawGrid = class (TCustomDrawGrid, ImGrid)
  strict private
    FDataProvider : IVDDataProvider;
    FFields : TmFields;
    FSummaryManager: ISummaryDatasetManager;
    // custom bitmaps
    FCustomUncheckedBitmap : TBitmap;
    FCustomCheckedBitmap : TBitmap;
    FCustomGrayedBitmap : TBitmap;
    //
    FGarbage : TObjectList;
    FCursor : TmDrawGridCursor;
    // Summary panel
    FSummaryPanel : ISummaryPanel;

    procedure SetDataProvider(AValue: IVDDataProvider);
    procedure CreateFields;
    function ExtractFieldNameFromColumn(const aColumn : TGridColumn): String;
    procedure ExtractSettingsFromField(aColumn: TGridColumn; aSettings : TmGridColumnSettings);
    procedure ApplySettingsToField(aColumn: TGridColumn; aSettings : TmGridColumnSettings);

    procedure InitGrid;
    procedure RefreshData;
    procedure RefreshSummaryPanel (Sender : TObject);
    procedure SetSummaryManager(AValue: ISummaryDatasetManager);
    procedure SetSummaryPanel(AValue: ISummaryPanel);
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
  public
    property DataProvider : IVDDataProvider read FDataProvider write SetDataProvider;
    property SummaryManager : ISummaryDatasetManager read FSummaryManager write SetSummaryManager;
    property SummaryPanel : ISummaryPanel read FSummaryPanel write SetSummaryPanel;
    property AlternateColor;
  end;


implementation

uses
  LResources, sysutils,
  mDatasetStandardSetup, mBaseClassesAsObjects, mDataProviderUtility;

{ TmDrawGridCursor }

constructor TmDrawGridCursor.Create(aDataProvider: IVDDataProvider);
begin
  FDataProvider := aDataProvider;
  FCurrentRecNo:= 0;
end;

procedure TmDrawGridCursor.StartBrowsing;
begin
  FCurrentRecNo:= 0;
end;

procedure TmDrawGridCursor.EndBrowsing;
begin
  //
end;

procedure TmDrawGridCursor.First;
begin
  FCurrentRecNo:= 0;
end;

procedure TmDrawGridCursor.Next;
begin
  inc (FCurrentRecNo);
end;

function TmDrawGridCursor.EOF: boolean;
begin
  Result := (FCurrentRecNo >= FDataProvider.Count);
end;

function TmDrawGridCursor.GetValueByFieldName(const aFieldName: String): Variant;
begin
  Result := FDataProvider.GetDatum(FCurrentRecNo).GetPropertyByFieldName(aFieldName);
end;

{ TmDrawGrid }

procedure TmDrawGrid.SetDataProvider(AValue: IVDDataProvider);
begin
  if FDataProvider=AValue then Exit;
  FDataProvider:=AValue;
  if Assigned(FCursor) then
    FCursor.Free;
  FCursor := TmDrawGridCursor.Create(FDataProvider);
end;

procedure TmDrawGrid.CreateFields;
var
  tmpFieldDefs : TmVirtualFieldDefs;
  i : integer;
  curField : TmField;
begin
  FFields.Clear;
  tmpFieldDefs := TmVirtualFieldDefs.Create;
  try
    FDataProvider.FillVirtualFieldDefs(tmpFieldDefs, '');

    for i:= 0 to tmpFieldDefs.Count - 1 do
    begin
      curField := FFields.Add;
      curField.FieldName := tmpFieldDefs.VirtualFieldDefs[i].Name;
      curField.DataType:= FromTmVirtualFieldDefTypeToTFieldType(tmpFieldDefs.VirtualFieldDefs[i].DataType);
      curField.DisplayLabel:= GenerateDisplayLabel(tmpFieldDefs.VirtualFieldDefs[i].Name);
      curField.Visible:= not IsSystemField(curField.FieldName);
      curField.DisplayFormat:= tmpFieldDefs.VirtualFieldDefs[i].DefaultFormat;
    end;
  finally
    tmpFieldDefs.Free;
  end;
end;

function TmDrawGrid.ExtractFieldNameFromColumn(const aColumn: TGridColumn): String;
begin
  Result := TStringObject(aColumn.Tag).Value;
end;

procedure TmDrawGrid.ExtractSettingsFromField(aColumn: TGridColumn; aSettings: TmGridColumnSettings);
var
  tmpField : TmField;
begin
  tmpField := Self.FFields.FieldByName(ExtractFieldNameFromColumn(aColumn));
  aSettings.Visible.Value:= tmpField.Visible;
  aSettings.DisplayFormat.Value:= tmpField.DisplayFormat;
  aSettings.DisplayLabel.Value:= tmpField.DisplayLabel;
  aSettings.SortOrder.Value:= aColumn.Index;
  aSettings.Width.Value:= max(MINIMUM_GRID_COLUMN_WIDTH, aColumn.Width);
end;

procedure TmDrawGrid.ApplySettingsToField(aColumn: TGridColumn; aSettings: TmGridColumnSettings);
var
  curField : TmField;
begin
  curField := FFields.FieldByName(ExtractFieldNameFromColumn(aColumn));
  if aSettings.Visible.NotNull then
  begin
    if aColumn.Visible <> aSettings.Visible.Value then
    begin
      aColumn.Visible := aSettings.Visible.Value;
      curField.Visible:= aSettings.Visible.Value;
      if aColumn.Visible then
        aColumn.Width:= max(aColumn.Width, MINIMUM_GRID_COLUMN_WIDTH);
    end;
  end;
  if aSettings.DisplayFormat.NotNull then
    curField.DisplayFormat := aSettings.DisplayFormat.Value;
  if aSettings.DisplayLabel.NotNull then
  begin
    aColumn.Title.Caption := aSettings.DisplayLabel.Value;
    curField.DisplayLabel:=  aSettings.DisplayLabel.Value;
    {$IFDEF DEBUG_COL_SET}DebugLn('[ApplySettingsToField] ' + aSettings.FieldName + ' ' +aColumn.Title.Caption);{$ENDIF}
  end;
  if aSettings.Width.NotNull then
    aColumn.Width:= max(aSettings.Width.Value, MINIMUM_GRID_COLUMN_WIDTH);
  if aSettings.SortOrder.NotNull then
    aColumn.Index := min(aSettings.SortOrder.Value, Self.Columns.Count - 1);
end;

procedure TmDrawGrid.InitGrid;
var
  i : integer;
  curCol : TGridColumn;
  curField : TmField;
  tmp : TStringObject;
begin
  Self.RowCount:= 0;
  Self.Columns.Clear;
  Self.FixedRows := 1;

  CreateFields;
  FGarbage.Clear;

  for i := 0 to FFields.Count - 1 do
  begin
    curField := FFields.Get(i);
    curCol := Self.Columns.Add;
    if (curField.DataType = ftFloat) or (curField.DataType = ftInteger) or (curField.DataType = ftLargeint) then
      curCol.Alignment:= taRightJustify
    else
      curCol.Alignment:= taLeftJustify;
    curCol.Title.Caption:= curField.DisplayLabel;
    curCol.Visible:= curField.Visible;
    tmp := TStringObject.Create(curField.FieldName);
    FGarbage.Add(tmp);
    curCol.Tag:= NativeInt(@tmp);
  end;

  RefreshData;
end;

procedure TmDrawGrid.RefreshData;
begin
  Self.RowCount:= FDataProvider.Count;
  Self.Invalidate;
end;

procedure TmDrawGrid.RefreshSummaryPanel(Sender: TObject);
begin
  mDataProviderUtility.RefreshSummaryPanel(FSummaryManager, FSummaryPanel);
end;

procedure TmDrawGrid.SetSummaryManager(AValue: ISummaryDatasetManager);
begin
  if FSummaryManager=AValue then Exit;
  FSummaryManager:=AValue;
  FSummaryManager.RegisterListener(Self.RefreshSummaryPanel);
end;

procedure TmDrawGrid.SetSummaryPanel(AValue: ISummaryPanel);
begin
  if FSummaryPanel=AValue then Exit;
  FSummaryPanel:=AValue;
  Self.RefreshSummaryPanel(nil);
end;

constructor TmDrawGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFields := TmFields.Create;
  FGarbage := TObjectList.Create(true);
end;

destructor TmDrawGrid.Destroy;
begin
  FFields.Free;
  FGarbage.Free;
  FreeAndNil(FCursor);
  inherited Destroy;
end;

procedure TmDrawGrid.ReadSettings(aSettings: TmGridColumnsSettings);
var
  op : TmGridColumnSettings;
  i : integer;
  fn : String;
begin
  for i := 0 to Self.Columns.Count - 1 do
  begin
    fn := ExtractFieldNameFromColumn(Self.Columns.Items[i]);
    if not IsSystemField(fn) then
    begin
      op := aSettings.AddSettingsForField(fn);
      ExtractSettingsFromField(Self.Columns.Items[i], op);
    end;
  end;
end;

procedure TmDrawGrid.ApplySettings(aSettings: TmGridColumnsSettings);
var
  i : integer;
  tmpDictionary : TmStringDictionary;
  tmpList : TList;
  tmpObj : TObject;
  curField : TmField;
begin
  tmpDictionary := TmStringDictionary.Create();
  tmpList := TList.Create;
  try
    for i := 0 to Self.Columns.Count - 1 do
    begin
      curField := FFields.FieldByName(ExtractFieldNameFromColumn(Self.Columns.Items[i]));
      if not IsSystemField (curField.FieldName) then
      begin
        if Assigned(aSettings.GetSettingsForField(curField.FieldName)) then
          tmpDictionary.Add(curField.FieldName, Self.Columns.Items[i])
        else
          tmpList.Add(Self.Columns.Items[i]);
      end
      else
        tmpList.Add(Self.Columns.Items[i]);
    end;

    for i := 0 to tmpList.Count - 1 do
      TGridColumn(tmpList.Items[i]).Index:= Columns.Count - 1;

    for i := aSettings.Count - 1 downto 0 do
    begin
      tmpObj :=  tmpDictionary.Find(aSettings.Get(i).FieldName);
      if Assigned(tmpObj) then
        ApplySettingsToField(tmpObj as TGridColumn, aSettings.Get(i));
    end;
  finally
    tmpList.Free;
    tmpDictionary.Free;;
  end;
end;

procedure TmDrawGrid.RefreshDataProvider;
begin
  Self.RefreshData;
end;

function TmDrawGrid.GetSummaryManager: ISummaryDatasetManager;
begin
  Result := FSummaryManager;
end;

procedure TmDrawGrid.GetFields(aFields: TmFields);
begin
  aFields.Assign(FFields);
end;

function TmDrawGrid.GetDataCursor: ImGridCursor;
begin
  Result := FCursor;
end;

procedure TmDrawGrid.GetColumns(aColumns: TmGridColumns);
var
  i : integer;
begin
  aColumns.Clear;
  for i := 0 to Self.Columns.Count - 1 do
    aColumns.Add.Assign(Self.Columns.Items[i]);
end;

initialization
  {$I lcl_dbgrid_customimages.lrs}
end.
