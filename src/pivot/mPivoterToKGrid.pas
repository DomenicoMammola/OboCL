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

resourcestring
  SGrandTotalHeader = 'Grand Total';

type

  { TmKGridAsPivotHelper }

  TmKGridAsPivotHelper = class
  strict private
    FPivoter : TmPivoter;
    FGrid : TKGrid;
    FNumericColumns : TCardinalList;
    FNumericColumnsIndex : TmIntegerDictionary;
    procedure OnDrawGridCell (Sender: TObject; ACol, ARow: Integer; R: TRect; State: TKGridDrawState);

    procedure FillRowHeaderGrandTotal;
    procedure FillRowHeader(const aKeyValues : TmKeysIndex; const aCol : integer; var aRow : integer; out aAdditiveCellSpan : integer);
    procedure FillColHeaderGrandTotal;
    procedure FillColHeader(const aKeyValues : TmKeysIndex; var aCol : integer; const aRow : integer; out aAdditiveCellSpan : integer);
    procedure FillFieldsHeader;
    procedure CalculateHeaderSize (const aKeyValues : TmKeysIndex; var aLevels : integer; const aTopLevel : boolean; const aTopHeader : boolean);
    procedure FillValues;
    procedure FillGrandTotalsValues;
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

procedure TmKGridAsPivotHelper.CalculateHeaderSize (const aKeyValues : TmKeysIndex; var aLevels : integer; const aTopLevel : boolean; const aTopHeader : boolean);
var
  i : integer;
begin
  if not Assigned(aKeyValues) then
    exit;
  if aTopHeader and aKeyValues.Terminal then
    aLevels := aLevels + (aKeyValues.KeyValuesCount * FPivoter.SummaryDefinitions.Count)
  else
    aLevels := aLevels + aKeyValues.KeyValuesCount;
  if not aTopLevel then
    aLevels := aLevels - 1;
  if not aKeyValues.Terminal then
  begin
    for i := 0 to aKeyValues.KeyValuesCount - 1 do
      CalculateHeaderSize(aKeyValues.GetSubIndex(i), aLevels, false, aTopHeader);
  end;
end;

procedure TmKGridAsPivotHelper.FillRowHeader(const aKeyValues : TmKeysIndex; const aCol : integer; var aRow : integer; out aAdditiveCellSpan : integer);
var
  i: integer;
  childsCellSpan : integer;
  curR, curC : integer;
begin
  aAdditiveCellSpan:= 0;
  childsCellSpan := 0;
  if not Assigned(aKeyValues) then
    exit;

  for i := 0 to aKeyValues.KeyValuesCount - 1 do
  begin
    FGrid.Cells[aCol, aRow] := FPivoter.HorizontalGroupByDefs.Get(aKeyValues.Level).FormatValue(aKeyValues.GetKeyValue(i));
    logger.Debug('ROW HEADER Row ' + IntToStr(aRow) + ' Col ' + IntToStr(aCol) +  ' value ' + aKeyValues.GetKeyValue(i).KeyValueAsString);
    inc(aAdditiveCellSpan);
    if aKeyValues.Terminal then
    begin
      FGrid.CellSpan[aCol, aRow] := MakeCellSpan(2, 1);
      inc(aRow);
    end
    else
    begin
      curR := aRow;
      curC := aCol;
      FillRowHeader(aKeyValues.GetSubIndex(i), curC + 1, aRow, childsCellSpan);
      if childsCellSpan > 1 then
      begin
        logger.Debug('ROW HEADER cell span ' + IntToStr(childsCellSpan) + ' starting from  Row ' + IntToStr(curR) + ' Col ' + IntToStr(curC));
        FGrid.CellSpan[curC, curR] := MakeCellSpan(1, childsCellSpan);
        aAdditiveCellSpan:= aAdditiveCellSpan + childsCellSpan  - 1;
      end;
      aRow := curR + childsCellSpan;
    end;
  end;
end;

procedure TmKGridAsPivotHelper.FillColHeaderGrandTotal;
var
  k, c : integer;
begin
  if FGrid.ColCount > 1 then
  begin
    FGrid.Cells[FGrid.ColCount - FPivoter.SummaryDefinitions.Count, 0] := sGrandTotalHeader;
    FGrid.CellSpan[FGrid.ColCount - FPivoter.SummaryDefinitions.Count, 0] := MakeCellSpan(FPivoter.SummaryDefinitions.Count, FGrid.FixedRows - 1);
    for k := 0 to FPivoter.SummaryDefinitions.Count - 1 do
    begin
      c := FGrid.ColCount - FPivoter.SummaryDefinitions.Count + k;
      if FPivoter.SummaryDefinitions.Get(k).DisplayLabel.NotNull then
        FGrid.Cells[c, FGrid.FixedRows - 1] := FPivoter.SummaryDefinitions.Get(k).DisplayLabel.AsString
      else
        FGrid.Cells[c, FGrid.FixedRows - 1] := GenerateDisplayLabel(FPivoter.SummaryDefinitions.Get(k).FieldName) + ' [' + TmSummaryOperatorToString(FPivoter.SummaryDefinitions.Get(k).SummaryOperator) + ']';
      if FieldTypeIsFloat(FPivoter.SummaryDefinitions.Get(k).FieldType) or FieldTypeIsInteger(FPivoter.SummaryDefinitions.Get(k).FieldType) then
        FNumericColumns.Add(c);
    end;
  end;
end;


procedure TmKGridAsPivotHelper.FillColHeader(const aKeyValues : TmKeysIndex; var aCol : integer; const aRow : integer; out aAdditiveCellSpan : integer);
var
  i, k: integer;
  childsCellSpan : integer;
  curR, curC : integer;
begin
  aAdditiveCellSpan:= 0;
  childsCellSpan := 0;
  if not Assigned(aKeyValues) then
    exit;
  for i := 0 to aKeyValues.KeyValuesCount - 1 do
  begin
    FGrid.Cells[aCol, aRow] := FPivoter.VerticalGroupByDefs.Get(aKeyValues.Level).FormatValue(aKeyValues.GetKeyValue(i));
    logger.Debug('COL HEADER Row ' + IntToStr(aRow) + ' Col ' + IntToStr(aCol) +  ' value ' + aKeyValues.GetKeyValue(i).KeyValueAsString);
    inc(aAdditiveCellSpan);
    if aKeyValues.Terminal then
    begin
      if FPivoter.SummaryDefinitions.Count > 1 then
        FGrid.CellSpan[aCol, aRow] := MakeCellSpan(FPivoter.SummaryDefinitions.Count, 1);
      aAdditiveCellSpan:= aAdditiveCellSpan + FPivoter.SummaryDefinitions.Count - 1;
      for k := 0 to FPivoter.SummaryDefinitions.Count - 1 do
      begin
        if FPivoter.SummaryDefinitions.Get(k).DisplayLabel.NotNull then
          FGrid.Cells[aCol + k, aRow + 1] := FPivoter.SummaryDefinitions.Get(k).DisplayLabel.AsString
        else
          FGrid.Cells[aCol + k, aRow + 1] := GenerateDisplayLabel(FPivoter.SummaryDefinitions.Get(k).FieldName) + ' [' + TmSummaryOperatorToString(FPivoter.SummaryDefinitions.Get(k).SummaryOperator) + ']';
        if FieldTypeIsFloat(FPivoter.SummaryDefinitions.Get(k).FieldType) or FieldTypeIsInteger(FPivoter.SummaryDefinitions.Get(k).FieldType) then
          FNumericColumns.Add(aCol + k);
      end;
      aCol := aCol + FPivoter.SummaryDefinitions.Count;
    end
    else
    begin
      curR := aRow;
      curC := aCol;
      FillColHeader(aKeyValues.GetSubIndex(i), aCol, curR + 1, childsCellSpan);
      if childsCellSpan > 1 then
      begin
        logger.Debug('COL HEADER cell span ' + IntToStr(childsCellSpan) + ' starting from  Row ' + IntToStr(curR) + ' Col ' + IntToStr(curC));
        FGrid.CellSpan[curC, curR] := MakeCellSpan(childsCellSpan, 1);
        aAdditiveCellSpan:= aAdditiveCellSpan + childsCellSpan - 1;
      end;
      aCol := curC + childsCellSpan;
    end;
  end;
end;

procedure TmKGridAsPivotHelper.FillFieldsHeader;
var
  i : integer;
begin
  if FPivoter.VerticalGroupByDefs.Count > 0 then
  begin
    for i := 0 to FPivoter.HorizontalGroupByDefs.Count - 1 do
    begin
      if FPivoter.HorizontalGroupByDefs.Get(i).DisplayLabel.NotNull then
        FGrid.Cells[i, FPivoter.VerticalGroupByDefs.Count] := FPivoter.HorizontalGroupByDefs.Get(i).DisplayLabel.AsString
      else
        FGrid.Cells[i, FPivoter.VerticalGroupByDefs.Count] := GenerateDisplayLabel(FPivoter.HorizontalGroupByDefs.Get(i).FieldName);
    end;
  end;
  if FPivoter.HorizontalGroupByDefs.Count > 0 then
  begin
    for i := 0 to FPivoter.VerticalGroupByDefs.Count - 1 do
    begin
      if FPivoter.VerticalGroupByDefs.Get(i).DisplayLabel.NotNull then
        FGrid.Cells[FPivoter.HorizontalGroupByDefs.Count, i] := FPivoter.VerticalGroupByDefs.Get(i).DisplayLabel.AsString
      else
        FGrid.Cells[FPivoter.HorizontalGroupByDefs.Count, i] := GenerateDisplayLabel(FPivoter.VerticalGroupByDefs.Get(i).FieldName);
    end;
  end;

//  if poHorizontalGrandTotal in aPivoter.Options then
//    aGrid.Cells(aPivoter.HorizontalGroupByDefs.Count, ;
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

procedure RecursiveExploreHorizontalGrandTotals(aVerticalKeysIndex : TmKeysIndex; aVerticalValues : TStringList; const aPivoter : TmPivoter; aGrid: TKGrid; var c : integer);
var
  i, k, r : integer;
  vList : TStringList;
  value : TmSummaryValue;
  tmp : String;
begin
  for i := 0 to aVerticalKeysIndex.KeyValuesCount - 1 do
  begin
    vList := TStringList.Create;
    try
      vList.AddStrings(aVerticalValues);
      vList.Add(aVerticalKeysIndex.GetKeyValue(i).KeyValueAsString);
      if aVerticalKeysIndex.Terminal then
      begin
        for k := 0 to aPivoter.SummaryDefinitions.Count - 1 do
        begin
          value := aPivoter.GetHorizontalGrandTotalValue(vList, aPivoter.SummaryDefinitions.Get(k));
          if Assigned(value) then
          begin
            tmp := value.ValueAsString;
            r := aGrid.RowCount - 1;
            aGrid.Cells[c, r] := tmp;
            logger.Debug('Writing in R ' + IntToStr(r) + ' C ' + IntToStr(c) + ' value ' + tmp);
          end;
          inc(c);
        end;
      end
      else
        RecursiveExploreHorizontalGrandTotals(aVerticalKeysIndex.GetSubIndexOfKey(aVerticalKeysIndex.GetKeyValue(i).KeyValueAsString), vList, aPivoter, aGrid, c);
    finally
      vList.Free;
    end;
  end;
end;


procedure RecursiveExploreVerticalGrandTotals(aHorizontalKeysIndex : TmKeysIndex; aHorizontalValues : TStringList; const aPivoter : TmPivoter; aGrid: TKGrid; var r : integer);
var
  i, k, c : integer;
  hList : TStringList;
  value : TmSummaryValue;
  tmp : String;
begin
  for i := 0 to aHorizontalKeysIndex.KeyValuesCount - 1 do
  begin
    hList := TStringList.Create;
    try
      hList.AddStrings(aHorizontalValues);
      hList.Add(aHorizontalKeysIndex.GetKeyValue(i).KeyValueAsString);
      if aHorizontalKeysIndex.Terminal then
      begin
        for k := 0 to aPivoter.SummaryDefinitions.Count - 1 do
        begin
          value := aPivoter.GetVerticalGrandTotalValue(hList, aPivoter.SummaryDefinitions.Get(k));
          if Assigned(value) then
          begin
            tmp := value.ValueAsString;
            c := aGrid.ColCount - aPivoter.SummaryDefinitions.Count;
            aGrid.Cells[c + k, r] := tmp;
            logger.Debug('Writing GRAND TOTAL in R ' + IntToStr(r) + ' C ' + IntToStr(c) + ' value ' + tmp);
          end;
        end;
        inc(r);
      end
      else
        RecursiveExploreVerticalGrandTotals(aHorizontalKeysIndex.GetSubIndexOfKey(aHorizontalKeysIndex.GetKeyValue(i).KeyValueAsString), hList, aPivoter, aGrid, r);
    finally
      hList.Free;
    end;
  end;
end;

procedure TmKGridAsPivotHelper.FillValues;
var
  r, c : integer;
  i, k : integer;
  hList, vList: TStringList;
begin
  r := FGrid.FixedRows;
  c := FGrid.FixedCols;

  vList := TStringList.Create;
  hList := TStringList.Create;
  try
    RecursiveExploreHorizontal(FPivoter.HorizontalKeysIndex, FPivoter.VerticalKeysIndex, vList, hList, FPivoter, FGrid, r, c);
  finally
    vList.Free;
    hList.Free;
  end;
end;

procedure TmKGridAsPivotHelper.FillGrandTotalsValues;
var
  r, c : integer;
  i, k : integer;
  gList: TStringList;
begin
  r := FGrid.FixedRows;

  if poVerticalGrandTotal in FPivoter.Options then
  begin
    c := FGrid.FixedCols;

    gList := TStringList.Create;
    try
      RecursiveExploreHorizontalGrandTotals(FPivoter.VerticalKeysIndex, gList, FPivoter, FGrid, c);
    finally
      gList.Free;
    end;
  end;

  if poHorizontalGrandTotal in FPivoter.Options then
  begin
    r := FGrid.FixedRows;

    gList := TStringList.Create;
    try
      RecursiveExploreVerticalGrandTotals(FPivoter.HorizontalKeysIndex, gList, FPivoter, FGrid, r);
    finally
      gList.Free;
    end;
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

procedure TmKGridAsPivotHelper.FillRowHeaderGrandTotal;
begin
  if FGrid.RowCount > 1 then
  begin
    FGrid.Cells[0, FGrid.RowCount - 1] := sGrandTotalHeader;
    FGrid.CellSpan[0, FGrid.RowCount - 1] := MakeCellSpan(FGrid.FixedCols, 1);
  end;
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
  FPivoter.EnableSort:= true;
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
    CalculateHeaderSize(FPivoter.HorizontalKeysIndex, rc, true, false);
    if poHorizontalGrandTotal in FPivoter.Options then
      rc := rc + 1;
    logger.Debug('horizontal levels: ' + IntToStr(rc));
    cc := 0;
    CalculateHeaderSize(FPivoter.VerticalKeysIndex, cc, true, true);
    if poVerticalGrandTotal in FPivoter.Options then
      cc := cc + FPivoter.SummaryDefinitions.Count;
    logger.Debug('vertical levels: ' + IntToStr(cc));
    FGrid.RowCount:= FGrid.FixedRows + rc;
    FGrid.ColCount:= FGrid.FixedCols + cc;
    logger.Debug('total rows: ' + IntToStr(FGrid.RowCount));
    logger.Debug('total cols: ' + IntToStr(FGrid.ColCount));

    acs := 0;
    r :=  FGrid.FixedRows;
    FillRowHeader(FPivoter.HorizontalKeysIndex, 0, r, acs);
    FillRowHeaderGrandTotal;

    acs := 0;
    c :=  FGrid.FixedCols;
    FillColHeader(FPivoter.VerticalKeysIndex, c, 0, acs);
    FillColHeaderGrandTotal;

    FillFieldsHeader;

    FillValues;
    FillGrandTotalsValues;

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
