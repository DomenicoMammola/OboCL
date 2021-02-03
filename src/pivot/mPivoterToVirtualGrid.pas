// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mPivoterToVirtualGrid;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes,
  mPivoter, mSummary, mIntList, mMaps, mVirtualGrid;

resourcestring
  SGrandTotalHeader = 'Grand Total';

type

  { TmVirtualGridAsPivotHelper }

  TmVirtualGridAsPivotHelper = class abstract
  strict private
    procedure FillRowHeaderGrandTotal;
    procedure FillRowHeader(const aKeyValues : TmKeysIndex; const aCol : integer; var aRow : integer; out aAdditiveCellSpan : integer);
    procedure FillColHeaderGrandTotal;
    procedure FillColHeader(const aKeyValues : TmKeysIndex; var aCol : integer; const aRow : integer; out aAdditiveCellSpan : integer);
    procedure FillFieldsHeader;
    procedure CalculateHeaderSize (const aKeyValues : TmKeysIndex; var aLevels : integer; const aTopLevel : boolean; const aTopHeader : boolean);
    procedure FillValues;
    procedure FillGrandTotalsValues;
    procedure FillSuperGrandTotalValues;
  protected
    FPivoter : TmPivoter;
    FGrid : ImVirtualGrid;

    FNumericColumns : TCardinalList;
    FNumericColumnsIndex : TmIntegerDictionary;

    procedure InternalInit(aPivoter : TmPivoter; aGrid : ImVirtualGrid);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Clear;

    procedure ApplyPivotToGrid;
  end;

implementation

uses
  sysutils, LCLType,
  {$IFDEF DEBUG}mLog, {$ENDIF}mUtility, mDatasetStandardSetup;

{$IFDEF DEBUG}
var
  logger : TmLog;
{$ENDIF}

procedure TmVirtualGridAsPivotHelper.CalculateHeaderSize (const aKeyValues : TmKeysIndex; var aLevels : integer; const aTopLevel : boolean; const aTopHeader : boolean);
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

procedure TmVirtualGridAsPivotHelper.FillRowHeader(const aKeyValues : TmKeysIndex; const aCol : integer; var aRow : integer; out aAdditiveCellSpan : integer);
var
  i: integer;
  childsCellSpan : integer;
  curR, curC : integer;
  s : String;
begin
  aAdditiveCellSpan:= 0;
  childsCellSpan := 0;
  if not Assigned(aKeyValues) then
    exit;

  for i := 0 to aKeyValues.KeyValuesCount - 1 do
  begin
    s := FPivoter.HorizontalGroupByDefs.Get(aKeyValues.Level).FormatValue(aKeyValues.GetKeyValue(i));
    FGrid.SetStringCellValue(aCol, aRow, s, s);
    {$IFDEF DEBUG}
    logger.Debug('ROW HEADER Row ' + IntToStr(aRow) + ' Col ' + IntToStr(aCol) +  ' value ' + aKeyValues.GetKeyValue(i).KeyValueAsString);
    {$ENDIF}
    inc(aAdditiveCellSpan);
    if aKeyValues.Terminal then
    begin
      FGrid.SetCellSpan(aCol, aRow, 2, 1);
      inc(aRow);
    end
    else
    begin
      curR := aRow;
      curC := aCol;
      FillRowHeader(aKeyValues.GetSubIndex(i), curC + 1, aRow, childsCellSpan);
      if childsCellSpan > 1 then
      begin
        {$IFDEF DEBUG}
        logger.Debug('ROW HEADER cell span ' + IntToStr(childsCellSpan) + ' starting from  Row ' + IntToStr(curR) + ' Col ' + IntToStr(curC));
        {$ENDIF}
        FGrid.SetCellSpan(curC, curR, 1, childsCellSpan);
        aAdditiveCellSpan:= aAdditiveCellSpan + childsCellSpan  - 1;
      end;
      aRow := curR + childsCellSpan;
    end;
  end;
end;

procedure TmVirtualGridAsPivotHelper.FillColHeaderGrandTotal;
var
  k, c : integer;
  s : String;
begin
  if (FGrid.ColCount > 1) and (poVerticalGrandTotal in FPivoter.Options) then
  begin
    FGrid.SetStringCellValue(FGrid.ColCount - FPivoter.SummaryDefinitions.Count, 0, sGrandTotalHeader, sGrandTotalHeader);
    FGrid.SetCellSpan(FGrid.ColCount - FPivoter.SummaryDefinitions.Count, 0, FPivoter.SummaryDefinitions.Count, FGrid.FixedRows - 1);
    for k := 0 to FPivoter.SummaryDefinitions.Count - 1 do
    begin
      c := FGrid.ColCount - FPivoter.SummaryDefinitions.Count + k;
      if FPivoter.SummaryDefinitions.Get(k).DisplayLabel.NotNull then
        FGrid.SetStringCellValue(c, FGrid.FixedRows - 1, FPivoter.SummaryDefinitions.Get(k).DisplayLabel.AsString, FPivoter.SummaryDefinitions.Get(k).DisplayLabel.AsString)
      else
      begin
        s := GenerateDisplayLabel(FPivoter.SummaryDefinitions.Get(k).FieldName) + ' [' + TmSummaryOperatorToString(FPivoter.SummaryDefinitions.Get(k).SummaryOperator) + ']';
        FGrid.SetStringCellValue(c, FGrid.FixedRows - 1, s, s);
      end;
      if FieldTypeIsFloat(FPivoter.SummaryDefinitions.Get(k).FieldType) or FieldTypeIsInteger(FPivoter.SummaryDefinitions.Get(k).FieldType) then
        FNumericColumns.Add(c);
    end;
  end;
end;


procedure TmVirtualGridAsPivotHelper.FillColHeader(const aKeyValues : TmKeysIndex; var aCol : integer; const aRow : integer; out aAdditiveCellSpan : integer);
var
  i, k: integer;
  childsCellSpan : integer;
  curR, curC : integer;
  s : String;
begin
  aAdditiveCellSpan:= 0;
  childsCellSpan := 0;
  if not Assigned(aKeyValues) then
    exit;
  for i := 0 to aKeyValues.KeyValuesCount - 1 do
  begin
    s := FPivoter.VerticalGroupByDefs.Get(aKeyValues.Level).FormatValue(aKeyValues.GetKeyValue(i));
    FGrid.SetStringCellValue(aCol, aRow, s, s);
    {$IFDEF DEBUG}
    logger.Debug('COL HEADER Row ' + IntToStr(aRow) + ' Col ' + IntToStr(aCol) +  ' value ' + aKeyValues.GetKeyValue(i).KeyValueAsString);
    {$ENDIF}
    inc(aAdditiveCellSpan);
    if aKeyValues.Terminal then
    begin
      if FPivoter.SummaryDefinitions.Count > 1 then
        FGrid.SetCellSpan(aCol, aRow, FPivoter.SummaryDefinitions.Count, 1);
      aAdditiveCellSpan:= aAdditiveCellSpan + FPivoter.SummaryDefinitions.Count - 1;
      for k := 0 to FPivoter.SummaryDefinitions.Count - 1 do
      begin
        if FPivoter.SummaryDefinitions.Get(k).DisplayLabel.NotNull then
          FGrid.SetStringCellValue(aCol + k, aRow + 1, FPivoter.SummaryDefinitions.Get(k).DisplayLabel.AsString, FPivoter.SummaryDefinitions.Get(k).DisplayLabel.AsString)
        else
        begin
          s := GenerateDisplayLabel(FPivoter.SummaryDefinitions.Get(k).FieldName) + ' [' + TmSummaryOperatorToString(FPivoter.SummaryDefinitions.Get(k).SummaryOperator) + ']';
          FGrid.SetStringCellValue(aCol + k, aRow + 1, s, s);
        end;
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
        {$IFDEF DEBUG}
        logger.Debug('COL HEADER cell span ' + IntToStr(childsCellSpan) + ' starting from  Row ' + IntToStr(curR) + ' Col ' + IntToStr(curC));
        {$ENDIF}
        FGrid.SetCellSpan(curC, curR, childsCellSpan, 1);
        aAdditiveCellSpan:= aAdditiveCellSpan + childsCellSpan - 1;
      end;
      aCol := curC + childsCellSpan;
    end;
  end;
end;

procedure TmVirtualGridAsPivotHelper.FillFieldsHeader;
var
  i : integer;
  s : String;
begin
  if FPivoter.VerticalGroupByDefs.Count > 0 then
  begin
    for i := 0 to FPivoter.HorizontalGroupByDefs.Count - 1 do
    begin
      if FPivoter.HorizontalGroupByDefs.Get(i).DisplayLabel.NotNull then
        FGrid.SetStringCellValue(i, FPivoter.VerticalGroupByDefs.Count, FPivoter.HorizontalGroupByDefs.Get(i).DisplayLabel.AsString, FPivoter.HorizontalGroupByDefs.Get(i).DisplayLabel.AsString)
      else
      begin
        s := GenerateDisplayLabel(FPivoter.HorizontalGroupByDefs.Get(i).FieldName);
        FGrid.SetStringCellValue(i, FPivoter.VerticalGroupByDefs.Count, s, s);
      end;
    end;
  end;
  if FPivoter.HorizontalGroupByDefs.Count > 0 then
  begin
    for i := 0 to FPivoter.VerticalGroupByDefs.Count - 1 do
    begin
      if FPivoter.VerticalGroupByDefs.Get(i).DisplayLabel.NotNull then
        FGrid.SetStringCellValue(FPivoter.HorizontalGroupByDefs.Count, i, FPivoter.VerticalGroupByDefs.Get(i).DisplayLabel.AsString, FPivoter.VerticalGroupByDefs.Get(i).DisplayLabel.AsString)
      else
      begin
        s := GenerateDisplayLabel(FPivoter.VerticalGroupByDefs.Get(i).FieldName);
        FGrid.SetStringCellValue(FPivoter.HorizontalGroupByDefs.Count, i, s, s);
      end;
    end;
  end;

//  if poHorizontalGrandTotal in aPivoter.Options then
//    aGrid.Cells(aPivoter.HorizontalGroupByDefs.Count, ;
end;

procedure WriteValueToGrid(const aValue : TmSummaryValue; aGrid: ImVirtualGrid; const aCol, aRow : integer);
begin
  // svtDouble, svtInteger, svtString, svtDate, svtDateTime
  if aValue.DataType = svtDouble then
    aGrid.SetFloatCellValue(aCol, aRow, aValue.ValueAsVariant, aValue.ValueAsString)
  else if aValue.DataType = svtInteger then
    aGrid.SetIntegerCellValue(aCol, aRow, aValue.ValueAsVariant, aValue.ValueAsString)
  else if aValue.DataType = svtDate then
    aGrid.SetDateCellValue(aCol, aRow, aValue.ValueAsVariant, aValue.ValueAsString)
  else if aValue.DataType = svtDateTime then
    aGrid.SetDateTimeCellValue(aCol, aRow, aValue.ValueAsVariant, aValue.ValueAsString)
  else
    aGrid.SetStringCellValue(aCol, aRow, aValue.ValueAsVariant, aValue.ValueAsString);
end;

procedure RecursiveExploreVertical(aKeysIndex : TmKeysIndex; aVerticalValues, aHorizontalValues : TStringList; const aPivoter : TmPivoter; aGrid: ImVirtualGrid; var r, c : integer);
var
  i, k : integer;
  vList : TStringList;
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
            WriteValueToGrid(value, aGrid, c, r);
            {$IFDEF DEBUG}
            logger.Debug('Writing in R ' + IntToStr(r) + ' C ' + IntToStr(c) + ' value ' + value.ValueAsString);
            {$ENDIF}
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

procedure RecursiveExploreHorizontal(aKeysIndex, aVerticalKeysIndex : TmKeysIndex; aVerticalValues, aHorizontalValues : TStringList; const aPivoter : TmPivoter; aGrid: ImVirtualGrid; var r, c : integer);
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

procedure RecursiveExploreHorizontalGrandTotals(aVerticalKeysIndex : TmKeysIndex; aVerticalValues : TStringList; const aPivoter : TmPivoter; aGrid: ImVirtualGrid; var c : integer);
var
  i, k, r : integer;
  vList : TStringList;
  value : TmSummaryValue;
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
            r := aGrid.RowCount - 1;
            WriteValueToGrid(value, aGrid, c, r);
            {$IFDEF DEBUG}
            logger.Debug('Writing in R ' + IntToStr(r) + ' C ' + IntToStr(c) + ' value ' + value.ValueAsString);
            {$ENDIF}
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

procedure RecursiveExploreVerticalGrandTotals(aHorizontalKeysIndex : TmKeysIndex; aHorizontalValues : TStringList; const aPivoter : TmPivoter; aGrid: ImVirtualGrid; var r : integer);
var
  i, k, c : integer;
  hList : TStringList;
  value : TmSummaryValue;
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
            c := aGrid.ColCount - aPivoter.SummaryDefinitions.Count;
            WriteValueToGrid(value, aGrid, c + k, r);
            {$IFDEF DEBUG}
            logger.Debug('Writing GRAND TOTAL in R ' + IntToStr(r) + ' C ' + IntToStr(c) + ' value ' + value.ValueAsString);
            {$ENDIF}
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

procedure TmVirtualGridAsPivotHelper.FillValues;
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

procedure TmVirtualGridAsPivotHelper.FillGrandTotalsValues;
var
  r, c : integer;
  i, k : integer;
  gList: TStringList;
begin
  r := FGrid.FixedRows;

  if poHorizontalGrandTotal in FPivoter.Options then
  begin
    c := FGrid.FixedCols;

    gList := TStringList.Create;
    try
      RecursiveExploreHorizontalGrandTotals(FPivoter.VerticalKeysIndex, gList, FPivoter, FGrid, c);
    finally
      gList.Free;
    end;
  end;

  if poVerticalGrandTotal in FPivoter.Options then
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

procedure TmVirtualGridAsPivotHelper.FillSuperGrandTotalValues;
var
  value : TmSummaryValue;
  k, r, c : integer;
begin
  if (poVerticalGrandTotal in FPivoter.Options) and (poHorizontalGrandTotal in FPivoter.Options) then
  begin
    r := FGrid.RowCount - 1;
    for k := 0 to FPivoter.SummaryDefinitions.Count - 1 do
    begin
      value := FPivoter.SuperGrandTotal.FindByDefinition(FPivoter.SummaryDefinitions.Get(k));
      if Assigned(value) then
      begin
        c := FGrid.ColCount - FPivoter.SummaryDefinitions.Count + k;
        WriteValueToGrid(value, FGrid, c, r);
        {$IFDEF DEBUG}
        logger.Debug('Writing SUPERGRANDTOTAL in R ' + IntToStr(r) + ' C ' + IntToStr(c) + ' value ' + value.ValueAsString);
        {$ENDIF}
      end;
      inc(c);
    end;
  end;
end;


procedure TmVirtualGridAsPivotHelper.FillRowHeaderGrandTotal;
begin
  if (FGrid.RowCount > 1) and (poHorizontalGrandTotal in FPivoter.Options) then
  begin
    FGrid.SetStringCellValue(0, FGrid.RowCount - 1, sGrandTotalHeader, SGrandTotalHeader);
    FGrid.SetCellSpan(0, FGrid.RowCount - 1, FGrid.FixedCols, 1);
  end;
end;

constructor TmVirtualGridAsPivotHelper.Create;
begin
  FNumericColumns := TCardinalList.Create;
  FNumericColumnsIndex := TmIntegerDictionary.Create(false);
end;

destructor TmVirtualGridAsPivotHelper.Destroy;
begin
  FNumericColumns.Free;
  FNumericColumnsIndex.Free;
  inherited Destroy;
end;

procedure TmVirtualGridAsPivotHelper.InternalInit(aPivoter: TmPivoter; aGrid: ImVirtualGrid);
begin
  FNumericColumns.Clear;
  FPivoter := aPivoter;
  FPivoter.EnableSort:= true;
  FGrid := aGrid;
end;

procedure TmVirtualGridAsPivotHelper.Clear;
begin
  FNumericColumns.Clear;
  FNumericColumnsIndex.Clear;
end;

procedure TmVirtualGridAsPivotHelper.ApplyPivotToGrid;
var
  rc, cc : integer;
  i : integer;
  r, c, acs : integer;
begin
  Self.Clear;

  FGrid.StartUpdate;
  try
    FGrid.SetFixedCols(FPivoter.HorizontalGroupByDefs.Count + 1);
    FGrid.SetFixedRows(FPivoter.VerticalGroupByDefs.Count + 1);

    rc := 0;
    CalculateHeaderSize(FPivoter.HorizontalKeysIndex, rc, true, false);
    if poHorizontalGrandTotal in FPivoter.Options then
      rc := rc + 1;
    {$IFDEF DEBUG}
    logger.Debug('horizontal levels: ' + IntToStr(rc));
    {$ENDIF}
    cc := 0;
    CalculateHeaderSize(FPivoter.VerticalKeysIndex, cc, true, true);
    if poVerticalGrandTotal in FPivoter.Options then
      cc := cc + FPivoter.SummaryDefinitions.Count;
    {$IFDEF DEBUG}
    logger.Debug('vertical levels: ' + IntToStr(cc));
    {$ENDIF}
    FGrid.SetRowCount(FGrid.FixedRows + rc);
    FGrid.SetColCount(FGrid.FixedCols + cc);
    {$IFDEF DEBUG}
    logger.Debug('total rows: ' + IntToStr(FGrid.RowCount));
    logger.Debug('total cols: ' + IntToStr(FGrid.ColCount));
    {$ENDIF}

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
    FillSuperGrandTotalValues;

    FGrid.AutosizeColumns;
  finally
    FGrid.EndUpdate;
  end;

  for i := 0 to FNumericColumns.Count - 1 do
    FNumericColumnsIndex.Add(FNumericColumns.Nums[i], FNumericColumnsIndex);

end;

{$IFDEF DEBUG}
initialization
  logger := logManager.AddLog('mPivoterToVirtualGrid');
{$ENDIF}

end.
