// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mVirtualGrid;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$INTERFACES CORBA}
{$ENDIF IF}

interface

type
  ImVirtualGrid = interface
  ['{2F750FAB-22B0-4852-8E6D-698637380862}']
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

end.
