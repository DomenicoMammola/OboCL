// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mVirtualGridSpreadsheet;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
  fpstypes, fpspreadsheet,
  fpsallformats, // necessary to register all the input/output formats that fpspreadsheet can handle
  {$ENDIF}
  mVirtualGrid, mSpreadsheetUtils;

type

  { TmSpreadsheetAsVirtualGrid }

  TmSpreadsheetAsVirtualGrid = class(ImVirtualGrid)
  strict private
    FHelper : TmSpreadsheetHelper;
    FWorksheet: TsWorksheet;
    FFixedRows : integer;
    FFixedCols : integer;
    FRowCount : integer;
    FColCount : integer;
    function GetDefaultFont: TmSpreadsheetFont;
    function GetDefaultRowHeight: integer;
    procedure SetDefaultRowHeight(AValue: integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Init(aWorksheet : TsWorksheet);

    procedure SetStringCellValue(const aColumn, aRow: integer; const aValue : Variant; const aStringValue : String);
    procedure SetFloatCellValue(const aColumn, aRow: integer; const aValue : Variant; const aStringValue : String);
    procedure SetIntegerCellValue(const aColumn, aRow: integer; const aValue : Variant; const aStringValue : String);
    procedure SetDateTimeCellValue(const aColumn, aRow: integer; const aValue : Variant; const aStringValue : String);
    procedure SetDateCellValue(const aColumn, aRow: integer; const aValue : Variant; const aStringValue : String);

    procedure SetCellSpan(const aColumn, aRow, aColumns, aRows : integer);
    procedure SetRowCount(const aValue : integer);
    function RowCount : integer;
    procedure SetColCount(const aValue : integer);
    function ColCount : integer;
    procedure SetFixedRows(const aValue : integer);
    function FixedRows : integer;
    procedure SetFixedCols(const aValue : integer);
    function FixedCols : integer;
    procedure StartUpdate;
    procedure EndUpdate;
    procedure AutosizeColumns;

    property DefaultRowHeight : integer read GetDefaultRowHeight write SetDefaultRowHeight;
    property DefaultFont : TmSpreadsheetFont read GetDefaultFont;
  end;

implementation

uses
  sysutils, variants;

{ TmSpreadsheetAsVirtualGrid }

function TmSpreadsheetAsVirtualGrid.GetDefaultFont: TmSpreadsheetFont;
begin
  Result := FHelper.DefaultFont;
end;

function TmSpreadsheetAsVirtualGrid.GetDefaultRowHeight: integer;
begin
  Result := FHelper.DefaultRowHeight;
end;

procedure TmSpreadsheetAsVirtualGrid.SetDefaultRowHeight(AValue: integer);
begin
  FHelper.DefaultRowHeight:= aValue;
end;

constructor TmSpreadsheetAsVirtualGrid.Create;
begin
  FWorksheet := nil;
  FHelper := TmSpreadsheetHelper.Create;
end;

destructor TmSpreadsheetAsVirtualGrid.Destroy;
begin
  FHelper.Free;
  inherited;
end;

procedure TmSpreadsheetAsVirtualGrid.Init(aWorksheet: TsWorksheet);
begin
  FWorksheet := aWorksheet;
  FHelper.Sheet := FWorksheet;
  FFixedRows := 0;
  FFixedCols := 0;
  FRowCount := 0;
  FColCount := 0;
end;

procedure TmSpreadsheetAsVirtualGrid.SetStringCellValue(const aColumn, aRow: integer; const aValue : Variant; const aStringValue : String);
begin
  if not VarIsNull(aValue) then
  begin
    if (aRow < FFixedRows) or (aColumn < FFixedCols) then
      FHelper.WriteText(aRow, aColumn, aStringValue, false, true)
    else
      FHelper.WriteText(aRow, aColumn, aStringValue);
  end;
end;

procedure TmSpreadsheetAsVirtualGrid.SetFloatCellValue(const aColumn, aRow: integer; const aValue: Variant; const aStringValue: String);
begin
  if not VarIsNull(aValue) then
    FHelper.WriteFloat(aRow, aColumn, aValue);
end;

procedure TmSpreadsheetAsVirtualGrid.SetIntegerCellValue(const aColumn, aRow: integer; const aValue: Variant; const aStringValue: String);
begin
  if not VarIsNull(aValue) then
    FHelper.WriteInteger(aRow, aColumn, aValue);
end;

procedure TmSpreadsheetAsVirtualGrid.SetDateTimeCellValue(const aColumn, aRow: integer; const aValue: Variant; const aStringValue: String);
begin
  if not VarIsNull(aValue) then
    FHelper.WriteDateTime(aRow, aColumn, aValue);
end;

procedure TmSpreadsheetAsVirtualGrid.SetDateCellValue(const aColumn, aRow: integer; const aValue: Variant; const aStringValue: String);
begin
  if not VarIsNull(aValue) then
    FHelper.WriteDate(aRow, aColumn, aValue);
end;

procedure TmSpreadsheetAsVirtualGrid.SetCellSpan(const aColumn, aRow, aColumns, aRows: integer);
begin
  FWorksheet.MergeCells(aRow, aColumn, aRow + aRows - 1, aColumn + aColumns - 1);
end;

procedure TmSpreadsheetAsVirtualGrid.SetRowCount(const aValue: integer);
begin
  FRowCount:= aValue;
end;

function TmSpreadsheetAsVirtualGrid.RowCount: integer;
begin
  Result := FRowCount;
end;

procedure TmSpreadsheetAsVirtualGrid.SetColCount(const aValue: integer);
begin
  FColCount:= aValue;
end;

function TmSpreadsheetAsVirtualGrid.ColCount: integer;
begin
  Result := FColCount;
end;

procedure TmSpreadsheetAsVirtualGrid.SetFixedRows(const aValue: integer);
begin
  FFixedRows:= aValue;
end;

function TmSpreadsheetAsVirtualGrid.FixedRows: integer;
begin
  Result := FFixedRows;
end;

procedure TmSpreadsheetAsVirtualGrid.SetFixedCols(const aValue: integer);
begin
  FFixedCols:= aValue;
end;

function TmSpreadsheetAsVirtualGrid.FixedCols: integer;
begin
  Result := FFixedCols;
end;

procedure TmSpreadsheetAsVirtualGrid.StartUpdate;
begin
  //
end;

procedure TmSpreadsheetAsVirtualGrid.EndUpdate;
begin
  //
end;

procedure TmSpreadsheetAsVirtualGrid.AutosizeColumns;
begin
  //
end;

end.
