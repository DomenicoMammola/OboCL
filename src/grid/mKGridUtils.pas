// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mKGridUtils;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  kgrids;

procedure AutoSizeColumns(aGrid : TKGrid);

implementation

uses
  math;

procedure AutoSizeColumns(aGrid: TKGrid);
var
  i : integer;
  total, w : integer;
  ratio : double;
begin
  aGrid.AutoSizeGrid(mpColWidth);
  total := 0;
  for i := 0 to aGrid.FixedCols - 1 do
    total := total + aGrid.ColWidths[i];
  w := trunc (aGrid.Width * 0.95);
  if total >= w then
  begin
    ratio := total / w;
    for i := 0 to aGrid.FixedCols - 1 do
      aGrid.ColWidths[i] := max(5, trunc(aGrid.ColWidths[i] / ratio));
  end;
end;

end.
