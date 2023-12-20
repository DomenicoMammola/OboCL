unit mVirtualGridKGrid;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
  {$INTERFACES CORBA}
{$ENDIF IF}

uses
  kgrids,
  mVirtualGrid;

type

  { TmKGridAsVirtualGrid }

  TmKGridAsVirtualGrid = class(ImVirtualGrid)
  strict private
    FGrid : TKGrid;
  public
    constructor Create(aGrid : TKGrid);

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
  end;

implementation

uses
  kfunctions,
  mKGridHelper;

{ TmKGridAsVirtualGrid }

constructor TmKGridAsVirtualGrid.Create(aGrid: TKGrid);
begin
  FGrid := aGrid;
end;

procedure TmKGridAsVirtualGrid.SetStringCellValue(const aColumn, aRow: integer; const aValue: Variant; const aStringValue: String);
begin
  FGrid.Cells[aColumn, aRow] := aStringValue;
end;

procedure TmKGridAsVirtualGrid.SetFloatCellValue(const aColumn, aRow: integer; const aValue: Variant; const aStringValue: String);
begin
  FGrid.Cells[aColumn, aRow] := aStringValue;
end;

procedure TmKGridAsVirtualGrid.SetIntegerCellValue(const aColumn, aRow: integer; const aValue: Variant; const aStringValue: String);
begin
  FGrid.Cells[aColumn, aRow] := aStringValue;
end;

procedure TmKGridAsVirtualGrid.SetDateTimeCellValue(const aColumn, aRow: integer; const aValue: Variant; const aStringValue: String);
begin
  FGrid.Cells[aColumn, aRow] := aStringValue;
end;

procedure TmKGridAsVirtualGrid.SetDateCellValue(const aColumn, aRow: integer; const aValue: Variant; const aStringValue: String);
begin
  FGrid.Cells[aColumn, aRow] := aStringValue;
end;

procedure TmKGridAsVirtualGrid.SetCellSpan(const aColumn, aRow, aColumns, aRows: integer);
begin
  FGrid.CellSpan[aColumn, aRow] := MakeCellSpan(aColumns, aRows);
end;

procedure TmKGridAsVirtualGrid.SetRowCount(const aValue: integer);
begin
  FGrid.RowCount:= aValue;
end;

function TmKGridAsVirtualGrid.RowCount: integer;
begin
  Result := FGrid.RowCount;
end;

procedure TmKGridAsVirtualGrid.SetColCount(const aValue: integer);
begin
  FGrid.ColCount:= aValue;
end;

function TmKGridAsVirtualGrid.ColCount: integer;
begin
  Result := FGrid.ColCount;
end;

procedure TmKGridAsVirtualGrid.SetFixedRows(const aValue: integer);
begin
  FGrid.FixedRows:= aValue;
end;

function TmKGridAsVirtualGrid.FixedRows: integer;
begin
  Result := FGrid.FixedRows;
end;

procedure TmKGridAsVirtualGrid.SetFixedCols(const aValue: integer);
begin
  FGrid.FixedCols:= aValue;
end;

function TmKGridAsVirtualGrid.FixedCols: integer;
begin
  Result := FGrid.FixedCols;
end;

procedure TmKGridAsVirtualGrid.StartUpdate;
begin
  FGrid.LockUpdate;
end;

procedure TmKGridAsVirtualGrid.EndUpdate;
begin
  FGrid.UnlockUpdate;
end;

procedure TmKGridAsVirtualGrid.AutosizeColumns;
begin
  AutoSizeKGridColumns(FGrid);
end;

end.

