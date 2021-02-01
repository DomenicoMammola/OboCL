// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mPivoterToKGrid;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes,
  kgrids,
  mPivoter, mSummary, mIntList, mMaps;

type

  { TmKGridAsPivotHelper }

  TmKGridAsPivotHelper = class
  strict private
    FPivoter : TmPivoter;
    FGrid : TKGrid;
    FNumericColumns : TCardinalList;
    FNumericColumnsIndex : TmIntegerDictionary;
    procedure OnDrawGridCell (Sender: TObject; ACol, ARow: Integer; R: TRect; State: TKGridDrawState);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Init(aPivoter : TmPivoter; aGrid : TKGrid);
    procedure Clear;

    procedure ApplyPivotToKGrid;
  end;

implementation

uses
  sysutils, LCLType,
  kfunctions, kgraphics,
  mLog, mUtility, mDatasetStandardSetup;

var
  logger : TmLog;

procedure CalculateHeaderSize (const aKeyValues : TmKeysIndex; var aLevels : integer; const aTopLevel : boolean; const aTopHeader : boolean; const aSummaryDefs : TmSummaryDefinitions);
var
  i : integer;
begin
  if not Assigned(aKeyValues) then
    exit;
  if aTopHeader and aKeyValues.Terminal then
    aLevels := aLevels + (aKeyValues.KeyValuesCount * aSummaryDefs.Count)
  else
    aLevels := aLevels + aKeyValues.KeyValuesCount;
  if not aTopLevel then
    aLevels := aLevels - 1;
  if not aKeyValues.Terminal then
  begin
    for i := 0 to aKeyValues.KeyValuesCount - 1 do
      CalculateHeaderSize(aKeyValues.GetSubIndex(i), aLevels, false, aTopHeader, aSummaryDefs);
  end;
end;

procedure FillRowHeader(const aGroupByDefs : TmGroupByDefs; const aKeyValues : TmKeysIndex; const aCol : integer; var aRow : integer; aGrid : TKGrid; out aAdditiveCellSpan : integer);
var
  i: integer;
  childsCellSpan : integer;
  curR, curC : integer;
begin
  aAdditiveCellSpan:= 0;
  if not Assigned(aKeyValues) then
    exit;

  for i := 0 to aKeyValues.KeyValuesCount - 1 do
  begin
    aGrid.Cells[aCol, aRow] := aGroupByDefs.Get(aKeyValues.Level).FormatValue(aKeyValues.GetKeyValue(i));
    logger.Debug('ROW HEADER Row ' + IntToStr(aRow) + ' Col ' + IntToStr(aCol) +  ' value ' + aKeyValues.GetKeyValue(i).KeyValueAsString);
    inc(aAdditiveCellSpan);
    if aKeyValues.Terminal then
    begin
      aGrid.CellSpan[aCol, aRow] := MakeCellSpan(2, 1);
      inc(aRow);
    end
    else
    begin
      curR := aRow;
      curC := aCol;
      FillRowHeader(aGroupByDefs, aKeyValues.GetSubIndex(i), curC + 1, aRow, aGrid, childsCellSpan);
      if childsCellSpan > 1 then
      begin
        logger.Debug('ROW HEADER cell span ' + IntToStr(childsCellSpan) + ' starting from  Row ' + IntToStr(curR) + ' Col ' + IntToStr(curC));
        aGrid.CellSpan[curC, curR] := MakeCellSpan(1, childsCellSpan);
        aAdditiveCellSpan:= aAdditiveCellSpan + childsCellSpan  - 1;
      end;
      aRow := curR + childsCellSpan;
    end;
  end;
end;


procedure FillColHeader(const aGroupByDefs : TmGroupByDefs; const aKeyValues : TmKeysIndex; var aCol : integer; const aRow : integer; aGrid : TKGrid; const aSummaryDefs : TmSummaryDefinitions; out aAdditiveCellSpan : integer; aNumericColumns : TCardinalList);
var
  i, k: integer;
  childsCellSpan : integer;
  curR, curC : integer;
begin
  aAdditiveCellSpan:= 0;
  if not Assigned(aKeyValues) then
    exit;
  for i := 0 to aKeyValues.KeyValuesCount - 1 do
  begin
    aGrid.Cells[aCol, aRow] := aGroupByDefs.Get(aKeyValues.Level).FormatValue(aKeyValues.GetKeyValue(i));
    logger.Debug('COL HEADER Row ' + IntToStr(aRow) + ' Col ' + IntToStr(aCol) +  ' value ' + aKeyValues.GetKeyValue(i).KeyValueAsString);
    inc(aAdditiveCellSpan);
    if aKeyValues.Terminal then
    begin
      if aSummaryDefs.Count > 1 then
        aGrid.CellSpan[aCol, aRow] := MakeCellSpan(aSummaryDefs.Count, 1);
      aAdditiveCellSpan:= aAdditiveCellSpan + aSummaryDefs.Count - 1;
      for k := 0 to aSummaryDefs.Count - 1 do
      begin
        if aSummaryDefs.Get(k).DisplayLabel.NotNull then
          aGrid.Cells[aCol + k, aRow + 1] := aSummaryDefs.Get(k).DisplayLabel.AsString
        else
          aGrid.Cells[aCol + k, aRow + 1] := GenerateDisplayLabel(aSummaryDefs.Get(k).FieldName) + ' [' + TmSummaryOperatorToString(aSummaryDefs.Get(k).SummaryOperator) + ']';
        if FieldTypeIsFloat(aSummaryDefs.Get(k).FieldType) or FieldTypeIsInteger(aSummaryDefs.Get(k).FieldType) then
          aNumericColumns.Add(aCol + k);
      end;
      aCol := aCol + aSummaryDefs.Count;
    end
    else
    begin
      curR := aRow;
      curC := aCol;
      FillColHeader(aGroupByDefs, aKeyValues.GetSubIndex(i), aCol, curR + 1, aGrid, aSummaryDefs, childsCellSpan, aNumericColumns);
      if childsCellSpan > 1 then
      begin
        logger.Debug('COL HEADER cell span ' + IntToStr(childsCellSpan) + ' starting from  Row ' + IntToStr(curR) + ' Col ' + IntToStr(curC));
        aGrid.CellSpan[curC, curR] := MakeCellSpan(childsCellSpan, 1);
        aAdditiveCellSpan:= aAdditiveCellSpan + childsCellSpan - 1;
      end;
      aCol := curC + childsCellSpan;
    end;
  end;
end;

procedure FillFieldsHeader(const aHorizontalGroupByDefs, aVerticalGroupByDefs : TmGroupByDefs; aGrid: TKGrid);
var
  i : integer;
begin
  if aVerticalGroupByDefs.Count > 0 then
  begin
    for i := 0 to aHorizontalGroupByDefs.Count - 1 do
    begin
      if aHorizontalGroupByDefs.Get(i).DisplayLabel.NotNull then
        aGrid.Cells[i, aVerticalGroupByDefs.Count] := aHorizontalGroupByDefs.Get(i).DisplayLabel.AsString
      else
        aGrid.Cells[i, aVerticalGroupByDefs.Count] := GenerateDisplayLabel(aHorizontalGroupByDefs.Get(i).FieldName);
    end;
  end;
  if aHorizontalGroupByDefs.Count > 0 then
  begin
    for i := 0 to aVerticalGroupByDefs.Count - 1 do
    begin
      if aVerticalGroupByDefs.Get(i).DisplayLabel.NotNull then
        aGrid.Cells[aHorizontalGroupByDefs.Count, i] := aVerticalGroupByDefs.Get(i).DisplayLabel.AsString
      else
        aGrid.Cells[aHorizontalGroupByDefs.Count, i] := GenerateDisplayLabel(aVerticalGroupByDefs.Get(i).FieldName);
    end;
  end;
end;

procedure RecursiveExploreVertical(aKeysIndex : TmKeysIndex; aVerticalValues, aHorizontalValues : TStringList; const aPivoter : TmPivoter; aGrid: TKGrid; var r, c : integer);
var
  i, k : integer;
  vList : TStringList;
  tmp : String;
  value : TmSummaryValue;
begin
  if not Assigned(aKeysIndex) then
    exit;

  for i := 0 to aKeysIndex.KeyValuesCount -1 do
  begin
    vList := TStringList.Create;
    try
      vList.AddStrings(aVerticalValues);
      vList.Add(aKeysIndex.GetKeyValue(i).KeyValueAsString);
      if aKeysIndex.Terminal then
      begin
        for k := 0 to aPivoter.SummaryDefinitions.Count - 1 do
        begin
          value := aPivoter.GetValue(vList, aHorizontalValues, aPivoter.SummaryDefinitions.Get(k));
          if Assigned(value) then
          begin
            tmp := value.ValueAsString;
//            aGrid.Objects[];
            aGrid.Cells[c, r] := tmp;
            logger.Debug('Writing in R ' + IntToStr(r) + ' C ' + IntToStr(c) + ' value ' + tmp);
          end;
          inc(c);
        end;
      end
      else
        RecursiveExploreVertical(aKeysIndex.GetSubIndexOfKey(aKeysIndex.GetKeyValue(i).KeyValueAsString), vList, aHorizontalValues, aPivoter, aGrid, r, c);
    finally
      vList.Free;
    end;
  end;
end;

procedure RecursiveExploreHorizontal(aKeysIndex, aVerticalKeysIndex : TmKeysIndex; aVerticalValues, aHorizontalValues : TStringList; const aPivoter : TmPivoter; aGrid: TKGrid; var r, c : integer);
var
  i, c2 : integer;
  hList : TStringList;
begin
  for i := 0 to aKeysIndex.KeyValuesCount - 1 do
  begin
    hList := TStringList.Create;
    try
      hList.AddStrings(aHorizontalValues);
      hList.Add(aKeysIndex.GetKeyValue(i).KeyValueAsString);
      if aKeysIndex.Terminal then
      begin
        c2 := c;
        RecursiveExploreVertical(aVerticalKeysIndex, aVerticalValues, hList, aPivoter, aGrid, r, c2);
        inc(r);
      end
      else
        RecursiveExploreHorizontal(aKeysIndex.GetSubIndexOfKey(aKeysIndex.GetKeyValue(i).KeyValueAsString), aVerticalKeysIndex, aVerticalValues, hList, aPivoter, aGrid, r, c);
    finally
      hList.Free;
    end;
  end;
end;

procedure FillValues (const aPivoter : TmPivoter; aGrid : TKGrid);
var
  r, c : integer;
  i, k : integer;
  hList, vList: TStringList;
begin
  r := aGrid.FixedRows;
  c := aGrid.FixedCols;

  vList := TStringList.Create;
  hList := TStringList.Create;
  try
    RecursiveExploreHorizontal(aPivoter.HorizontalKeysIndex, aPivoter.VerticalKeysIndex, vList, hList, aPivoter, aGrid, r, c);
  finally
    vList.Free;
    hList.Free;
  end;
end;

procedure TmKGridAsPivotHelper.OnDrawGridCell(Sender: TObject; ACol, ARow: Integer; R: TRect; State: TKGridDrawState);
begin
  // https://forum.lazarus.freepascal.org/index.php/topic,44833.msg315562.html#msg315562

  FGrid.Cell[ACol, ARow].ApplyDrawProperties;

  if (ARow < FGrid.FixedRows) and (ACol >= FGrid.FixedCols) then
  begin
    FGrid.CellPainter.HAlign:=halCenter;
    FGrid.CellPainter.VAlign:=valCenter;
    FGrid.CellPainter.BackColor:= COLOR_clButton;
  end
  else if FNumericColumnsIndex.Contains(ACol) then
  begin
    FGrid.CellPainter.HAlign:=halRight;
  end;

  FGrid.CellPainter.DefaultDraw;
end;

constructor TmKGridAsPivotHelper.Create;
begin
  FNumericColumns := TCardinalList.Create;
  FNumericColumnsIndex := TmIntegerDictionary.Create(false);
end;

destructor TmKGridAsPivotHelper.Destroy;
begin
  FNumericColumns.Free;
  FNumericColumnsIndex.Free;
  inherited Destroy;
end;

procedure TmKGridAsPivotHelper.Init(aPivoter: TmPivoter; aGrid: TKGrid);
begin
  FNumericColumns.Clear;
  FPivoter := aPivoter;
  FPivoter.Options:= FPivoter.Options + [poEnableSort];
  FGrid := aGrid;
  FGrid.OnDrawCell:= Self.OnDrawGridCell;
  FGrid.Options := FGrid.Options - [goThemes, goThemedCells];
  FGrid.OptionsEx:= [gxMouseWheelScroll];
end;

procedure TmKGridAsPivotHelper.Clear;
begin
  FNumericColumns.Clear;
  FNumericColumnsIndex.Clear;
end;

procedure TmKGridAsPivotHelper.ApplyPivotToKGrid;
var
  rc, cc : integer;
  i : integer;
  r, c, acs : integer;
begin
  Self.Clear;

  FGrid.LockUpdate;
  try
    FGrid.FixedCols := FPivoter.HorizontalGroupByDefs.Count + 1;
    FGrid.FixedRows:= FPivoter.VerticalGroupByDefs.Count + 1;

    rc := 0;
    CalculateHeaderSize(FPivoter.HorizontalKeysIndex, rc, true, false, FPivoter.SummaryDefinitions);
    logger.Debug('horizontal levels: ' + IntToStr(rc));
    cc := 0;
    CalculateHeaderSize(FPivoter.VerticalKeysIndex, cc, true, true, FPivoter.SummaryDefinitions);
    logger.Debug('vertical levels: ' + IntToStr(cc));
    FGrid.RowCount:= FGrid.FixedRows + rc;
    FGrid.ColCount:= FGrid.FixedCols + cc;
    logger.Debug('total rows: ' + IntToStr(FGrid.RowCount));
    logger.Debug('total cols: ' + IntToStr(FGrid.ColCount));

    acs := 0;
    r :=  FGrid.FixedRows;
    FillRowHeader(FPivoter.HorizontalGroupByDefs, FPivoter.HorizontalKeysIndex, 0, r, FGrid, acs);
    acs := 0;
    c :=  FGrid.FixedCols;
    FillColHeader(FPivoter.VerticalGroupByDefs, FPivoter.VerticalKeysIndex, c, 0, FGrid, FPivoter.SummaryDefinitions, acs, FNumericColumns);

    FillFieldsHeader(FPivoter.HorizontalGroupByDefs, FPivoter.VerticalGroupByDefs, FGrid);

    FillValues (FPivoter, FGrid);

    FGrid.AutoSizeGrid(mpColWidth);
  finally
    FGrid.UnlockUpdate;
  end;

  for i := 0 to FNumericColumns.Count - 1 do
    FNumericColumnsIndex.Add(FNumericColumns.Nums[i], FNumericColumnsIndex);

end;

initialization
  logger := logManager.AddLog('mPivoterToKGrid');

end.
