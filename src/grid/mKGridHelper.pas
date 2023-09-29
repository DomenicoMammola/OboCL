unit mKGridHelper;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes,
  kgrids,
  mGrids, mGridHelper, KAParser, mVirtualDatasetFormulas, mCellDecorations,
  mDataProviderInterfaces, mVirtualDataSet, mFields, mGridColumnSettings;

type

  { TmKGridCursor }

  TmKGridCursor = class(ImGridCursor)
  strict private
    FDataProvider : IVDDataProvider;
    FIndex : LongInt;
  public
    constructor Create(aDataProvider : IVDDataProvider);

    procedure StartBrowsing;
    procedure EndBrowsing;
    procedure First;
    procedure Next;
    function EOF: boolean;
    function GetValueByFieldName(const aFieldName : String): Variant;
  end;


  { TmKGridHelper }

  TmKGridHelper = class(TmAbstractGridHelper, ImGrid)
  strict private
    FDataProvider : IVDDataProvider;
    FCursor : TmKGridCursor;
    FFields : TmFields;
    FSortedCols : TList;
    FSummaryManager: ISummaryDatasetManager;
    FOwnedCellDecorations : TmCellDecorations;

    procedure CreateFields;
    procedure SetDataProvider(AValue: IVDDataProvider);
    procedure SetSummaryManager(AValue: ISummaryDatasetManager);
    procedure OnDrawGridCell (Sender: TObject; ACol, ARow: Integer; R: TRect; State: TKGridDrawState);
    procedure OnMeasureCell(Sender: TObject; ACol, ARow: Integer; R: TRect; State: TKGridDrawState; Priority: TKGridMeasureCellPriority; var Extent: TPoint);
    procedure OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnMoveColumns(Sender: TObject; Index1, Index2: Integer);
    function GetField(const aCol : integer): TmField;
    function GetValue(const aCol, aRow : integer): variant;
  public
    constructor Create(aGrid : TKGrid; aFormulaFields : TmFormulaFields); virtual;
    destructor Destroy; override;

    procedure SetupGrid (const aEnableAutoSizedColumns : boolean = true); override;
    procedure SelectAllRows; override;
    procedure SelectRows (const aKeyField : String; const aValues : TStringList); override;

    procedure InitGrid;

    procedure ReadSettings(aSettings : TmGridColumnsSettings);
    procedure ApplySettings(aSettings : TmGridColumnsSettings);
    procedure RefreshDataProvider;
    function GetSummaryManager : ISummaryDatasetManager;
    procedure GetFields(aFields : TmFields);
    function GetDataCursor : ImGridCursor;
    procedure GetColumns(aColumns : TmGridColumns);

    property DataProvider : IVDDataProvider read FDataProvider write SetDataProvider;
    property SummaryManager : ISummaryDatasetManager read FSummaryManager write SetSummaryManager;
  end;

implementation

uses
  Controls, SysUtils, Graphics, Variants, LCLType,
  kgraphics,
  mDataProviderFieldDefs, mDatasetStandardSetup, mGraphicsUtility;

{ TmKGridCursor }

constructor TmKGridCursor.Create(aDataProvider: IVDDataProvider);
begin
  FDataProvider:= aDataProvider;
  FIndex:= 0;
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
  inc(FIndex);
end;

function TmKGridCursor.EOF: boolean;
begin
  Result := FIndex >= FDataProvider.Count;
end;

function TmKGridCursor.GetValueByFieldName(const aFieldName: String): Variant;
begin
  Result := Null;
  if FIndex < FDataProvider.Count then
    FDataProvider.GetDatum(FIndex).GetPropertyByFieldName(aFieldName);
end;

{ TmKGridHelper }

procedure TmKGridHelper.CreateFields;
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
      FSortedCols.Add(curField);
    end;
  finally
    tmpFieldDefs.Free;
  end;
end;

procedure TmKGridHelper.SetDataProvider(AValue: IVDDataProvider);
begin
  if FDataProvider=AValue then Exit;
  FDataProvider:=AValue;
  if Assigned(FCursor) then
    FCursor.Free;
  FCursor := TmKGridCursor.Create(FDataProvider);
end;

procedure TmKGridHelper.SetSummaryManager(AValue: ISummaryDatasetManager);
begin
  if FSummaryManager=AValue then Exit;
  FSummaryManager:=AValue;
end;

procedure TmKGridHelper.OnDrawGridCell(Sender: TObject; ACol, ARow: Integer; R: TRect; State: TKGridDrawState);
var
  d : boolean;
  tmpRect : TRect;
  grid : TKGrid;
begin
  grid := (FGrid as TKGrid);

  // https://forum.lazarus.freepascal.org/index.php/topic,44833.msg315562.html#msg315562

  //grid.Cell[ACol, ARow].ApplyDrawProperties;

  if grid.CellPainter.Canvas.Brush.Style = bsClear then grid.CellPainter.Canvas.Brush.Style := bsSolid;

  d := true;

  if (ARow < grid.FixedRows) and (ACol >= grid.FixedCols) then
  begin
    grid.CellPainter.HAlign:=halCenter;
    grid.CellPainter.VAlign:=valCenter;
    grid.CellPainter.BackColor:= grid.Colors.FixedCellBkGnd;
    grid.CellPainter.Canvas.Brush.Color:= grid.Colors.FixedCellBkGnd;
  end
  (*
  else if (aCol < FKGrid.FixedCols) and (ARow >= FKGrid.FixedRows) then
  begin
    tmpRect := R;
    tmpRect.Bottom:= Min(FKGrid.Height - 1, tmpRect.Bottom);
    tmpRect.Top := Max(FKGrid.DefaultRowHeight * FKGrid.FixedRows, tmpRect.Top);
    if tmpRect.Height <= (FKGrid.DefaultRowHeight * 2) then
      if tmpRect.Bottom < R.Bottom then
        FKGrid.CellPainter.VAlign:= valTop
      else if tmpRect.Top > R.Top then
        FKGrid.CellPainter.VAlign:= valBottom;
    FKGrid.CellPainter.DrawCellText(tmpRect);
    d := false;
  end*)
  else
  begin

    //if FNumericColumnsIndex.Contains(ACol) then
    //  FKGrid.CellPainter.HAlign:=halRight;
    if State * [gdFixed, gdSelected] = [] then
    begin
      grid.CellPainter.BackColor:= LighterColor(grid.Colors.FixedCellBkGnd, 15);
      grid.CellPainter.Canvas.Brush.Color:= grid.CellPainter.BackColor;
    end;
  end;

  grid.CellPainter.DrawDefaultCellBackground;
  if ARow = 0 then
  begin
    grid.CellPainter.Text := GetField(ACol).DisplayLabel;
  end
  else
    grid.CellPainter.Text:= VarToStr(GetValue(ACol, ARow - 1));
  grid.CellPainter.DrawCellText(R);
end;

procedure TmKGridHelper.OnMeasureCell(Sender: TObject; ACol, ARow: Integer; R: TRect; State: TKGridDrawState; Priority: TKGridMeasureCellPriority; var Extent: TPoint);
var
  sz : TPoint;
begin
  if ARow = 0 then
    GetTextExtend(GetField(ACol).DisplayLabel, FGrid.Font, Extent)
  else
    GetTextExtend(VarToStr(GetValue(ACol, ARow)), FGrid.Font, Extent);
  Extent.X := Extent.X + ((FGrid as TKGrid).CellPainter.HPadding * 2);
  Extent.Y := Extent.Y + ((FGrid as TKGrid).CellPainter.VPadding * 2);
end;

procedure TmKGridHelper.OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Shift = [ssCtrl]) and (Key = VK_C) then
    CopyTextToClipboard(VarToStr(GetValue((FGrid as TKGrid).Col, (FGrid as TKGrid).Row - (FGrid as TKGrid).FixedRows)));
end;

procedure TmKGridHelper.OnMoveColumns(Sender: TObject; Index1, Index2: Integer);
var
  tmpField : TmField;
begin
  tmpField := TmField(FSortedCols.Items[Index1]);
  FSortedCols.Items[Index1] := FSortedCols.Items[Index2];
  FSortedCols.Items[Index2] := tmpField;
end;

function TmKGridHelper.GetField(const aCol: integer): TmField;
begin
  Result := TmField(FSortedCols.Items[aCol]);
end;

function TmKGridHelper.GetValue(const aCol, aRow: integer): variant;
var
  curField : TmField;
begin
  if (aRow < FDataProvider.Count) and (aCol < FSortedCols.Count) then
  begin
    curField := GetField(aCol);
    Result := FDataProvider.GetDatum(aRow).GetPropertyByFieldName(curField.FieldName)
  end
  else
    Result := Null;
end;

constructor TmKGridHelper.Create(aGrid: TKGrid; aFormulaFields: TmFormulaFields);
begin
  FOwnedCellDecorations := TmCellDecorations.Create;
  InternalCreate(aGrid, aFormulaFields, FOwnedCellDecorations);
  FGrid := aGrid;
  FDataProvider := nil;
  FFields := TmFields.Create;
  FSortedCols := TList.Create;
  (FGrid as TKGrid).Options:= [goVirtualGrid, goColSizing, goColMoving, goRowSorting, goDrawFocusSelected, goRowSelect, goVertLine, goHeader, goHorzLine, goRangeSelect];
  (FGrid as TKGrid).OptionsEx:= [gxMouseWheelScroll];
  (FGrid as TKGrid).RangeSelectStyle := rsMultiSelect;
  (FGrid as TKGrid).OnDrawCell:= Self.OnDrawGridCell;
  (FGrid as TKGrid).OnMeasureCell:= Self.OnMeasureCell;
  (FGrid as TKGrid).OnKeyDown:= Self.OnKeyDown;
  (FGrid as TKGrid).OnExchangeCols := Self.OnMoveColumns;
  (FGrid as TKGrid).Flat := True;
end;

destructor TmKGridHelper.Destroy;
begin
  FreeAndNil(FCursor);
  FFields.Free;
  FSortedCols.Free;
  FOwnedCellDecorations.Free;
  inherited Destroy;
end;

procedure TmKGridHelper.SetupGrid(const aEnableAutoSizedColumns: boolean);
begin
  (FGrid as TKGrid).Align:= alClient;
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

procedure TmKGridHelper.SelectRows(const aKeyField: String; const aValues: TStringList);
begin
  //
end;

procedure TmKGridHelper.InitGrid;
var
  curField : TmField;
begin
  (FGrid as TKGrid).LockUpdate;
  try
    CreateFields;
    (FGrid as TKGrid).FixedRows := 1;
    (FGrid as TKGrid).FixedCols := 0;

    (FGrid as TKGrid).RowCount:= FDataProvider.Count + (FGrid as TKGrid).FixedRows;
    (FGrid as TKGrid).ColCount:=FFields.Count;
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
    if FFields.Count = 0 then
      CreateFields;
    (FGrid as TKGrid).FixedRows := 1;
    (FGrid as TKGrid).FixedCols := 0;

    (FGrid as TKGrid).RowCount:= FDataProvider.Count + (FGrid as TKGrid).FixedRows;
    (FGrid as TKGrid).ColCount:=FFields.Count;
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
