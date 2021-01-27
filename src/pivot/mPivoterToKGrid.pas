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
  mPivoter, mSummary, mIntList;

procedure ApplyPivotToKGrid(const aPivoter : TmPivoter; aGrid : TKGrid; aNumericColumns : TCardinalList);

implementation

uses
  sysutils,
  kfunctions,
  mLog, mUtility;

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

procedure FillRowHeader(const aKeyValues : TmKeysIndex; const aCol : integer; var aRow : integer; aGrid : TKGrid; out aAdditiveCellSpan : integer);
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
    aGrid.Cells[aCol, aRow] := aKeyValues.GetKeyValue(i);
    logger.Debug('ROW HEADER Row ' + IntToStr(aRow) + ' Col ' + IntToStr(aCol) +  ' value ' + aKeyValues.GetKeyValue(i));
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
      FillRowHeader(aKeyValues.GetSubIndex(i), curC + 1, aRow, aGrid, childsCellSpan);
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


procedure FillColHeader(const aKeyValues : TmKeysIndex; var aCol : integer; const aRow : integer; aGrid : TKGrid; const aSummaryDefs : TmSummaryDefinitions; out aAdditiveCellSpan : integer; aNumericColumns : TCardinalList);
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
    aGrid.Cells[aCol, aRow] := aKeyValues.GetKeyValue(i);
    logger.Debug('COL HEADER Row ' + IntToStr(aRow) + ' Col ' + IntToStr(aCol) +  ' value ' + aKeyValues.GetKeyValue(i));
    inc(aAdditiveCellSpan);
    if aKeyValues.Terminal then
    begin
      if aSummaryDefs.Count > 1 then
        aGrid.CellSpan[aCol, aRow] := MakeCellSpan(aSummaryDefs.Count, 1);
      aAdditiveCellSpan:= aAdditiveCellSpan + aSummaryDefs.Count - 1;
      for k := 0 to aSummaryDefs.Count - 1 do
      begin
        aGrid.Cells[aCol + k, aRow + 1] := aSummaryDefs.Get(k).Caption;
        if FieldTypeIsFloat(aSummaryDefs.Get(k).FieldType) or FieldTypeIsInteger(aSummaryDefs.Get(k).FieldType) then
          aNumericColumns.Add(aCol + k);
      end;
      aCol := aCol + aSummaryDefs.Count;
    end
    else
    begin
      curR := aRow;
      curC := aCol;
      FillColHeader(aKeyValues.GetSubIndex(i), aCol, curR + 1, aGrid, aSummaryDefs, childsCellSpan, aNumericColumns);
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
      aGrid.Cells[i, aVerticalGroupByDefs.Count] := aHorizontalGroupByDefs.Get(i).FieldName;
  end;
  if aHorizontalGroupByDefs.Count > 0 then
  begin
    for i := 0 to aVerticalGroupByDefs.Count - 1 do
      aGrid.Cells[aHorizontalGroupByDefs.Count, i] := aVerticalGroupByDefs.Get(i).FieldName;
  end;
end;

procedure RecursiveExploreVertical(aKeysIndex : TmKeysIndex; aVerticalValues, aHorizontalValues : TStringList; const aPivoter : TmPivoter; aGrid: TKGrid; var r, c : integer);
var
  i, k : integer;
  vList : TStringList;
  tmp : String;
  value : TmSummaryValue;
begin
  for i := 0 to aKeysIndex.KeyValuesCount -1 do
  begin
    vList := TStringList.Create;
    try
      vList.AddStrings(aVerticalValues);
      vList.Add(aKeysIndex.GetKeyValue(i));
      if aKeysIndex.Terminal then
      begin
        for k := 0 to aPivoter.SummaryDefinitions.Count - 1 do
        begin
          value := aPivoter.GetValue(vList, aHorizontalValues, aPivoter.SummaryDefinitions.Get(k));
          if Assigned(value) then
          begin
            tmp := value.ValueAsString;
            aGrid.Cells[c, r] := tmp;
            logger.Debug('Writing in R ' + IntToStr(r) + ' C ' + IntToStr(c) + ' value ' + tmp);
          end;
          inc(c);
        end;
      end
      else
        RecursiveExploreVertical(aKeysIndex.GetSubIndexOfKey(aKeysIndex.GetKeyValue(i)), vList, aHorizontalValues, aPivoter, aGrid, r, c);
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
      hList.Add(aKeysIndex.GetKeyValue(i));
      if aKeysIndex.Terminal then
      begin
        c2 := c;
        RecursiveExploreVertical(aVerticalKeysIndex, aVerticalValues, hList, aPivoter, aGrid, r, c2);
        inc(r);
      end
      else
        RecursiveExploreHorizontal(aKeysIndex.GetSubIndexOfKey(aKeysIndex.GetKeyValue(i)), aVerticalKeysIndex, aVerticalValues, hList, aPivoter, aGrid, r, c);
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

procedure ApplyPivotToKGrid(const aPivoter: TmPivoter; aGrid: TKGrid; aNumericColumns : TCardinalList);
var
  rc, cc : integer;
  i : integer;
  r, c, acs : integer;
begin
  aGrid.LockUpdate;
  try
    aGrid.FixedCols := aPivoter.HorizontalGroupByDefs.Count + 1;
    aGrid.FixedRows:= aPivoter.VerticalGroupByDefs.Count + 1;

    rc := 0;
    CalculateHeaderSize(aPivoter.HorizontalKeysIndex, rc, true, false, aPivoter.SummaryDefinitions);
    logger.Debug('horizontal levels: ' + IntToStr(rc));
    cc := 0;
    CalculateHeaderSize(aPivoter.VerticalKeysIndex, cc, true, true, aPivoter.SummaryDefinitions);
    logger.Debug('vertical levels: ' + IntToStr(cc));
    aGrid.RowCount:= aGrid.FixedRows + rc;
    aGrid.ColCount:= aGrid.FixedCols + cc;
    logger.Debug('total rows: ' + IntToStr(aGrid.RowCount));
    logger.Debug('total cols: ' + IntToStr(aGrid.ColCount));

    acs := 0;
    r :=  aGrid.FixedRows;
    FillRowHeader(aPivoter.HorizontalKeysIndex, 0, r, aGrid, acs);
    acs := 0;
    c :=  aGrid.FixedCols;
    FillColHeader(aPivoter.VerticalKeysIndex, c, 0, aGrid, aPivoter.SummaryDefinitions, acs, aNumericColumns);

    FillFieldsHeader(aPivoter.HorizontalGroupByDefs, aPivoter.VerticalGroupByDefs, aGrid);

    FillValues (aPivoter, aGrid);

    aGrid.AutoSizeGrid(mpColWidth);
  finally
    aGrid.UnlockUpdate;
  end;

end;

initialization
  logger := logManager.AddLog('mPivoterToKGrid');

end.
