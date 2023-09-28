// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mKGridAsPivotHelper;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, Graphics,
  kgrids,

  mPivoter,
  mPivoterToVirtualGrid, mVirtualGridKGrid;

type

  { TmKGridAsPivotHelper }

  TmKGridAsPivotHelper = class (TmVirtualGridAsPivotHelper)
  strict private
    FKGrid : TKGrid;
    FKGridAsVirtualGrid: TmKGridAsVirtualGrid;
    procedure OnDrawGridCell (Sender: TObject; ACol, ARow: Integer; R: TRect; State: TKGridDrawState);
    procedure OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Init(aPivoter : TmPivoter; aGrid : TKGrid);
    procedure AutosizeColumns;
  end;

implementation

uses
  sysutils, LCLType, math,
  kfunctions, kgraphics,
  mGraphicsUtility, mKGridUtils;


procedure TmKGridAsPivotHelper.OnDrawGridCell(Sender: TObject; ACol, ARow: Integer; R: TRect; State: TKGridDrawState);
var
  d : boolean;
  tmpRect : TRect;
begin
  // https://forum.lazarus.freepascal.org/index.php/topic,44833.msg315562.html#msg315562

  FKGrid.Cell[ACol, ARow].ApplyDrawProperties;

  if FKGrid.CellPainter.Canvas.Brush.Style = bsClear then FKGrid.CellPainter.Canvas.Brush.Style := bsSolid;

  d := true;

  if (ARow < FKGrid.FixedRows) and (ACol >= FKGrid.FixedCols) then
  begin
    FKGrid.CellPainter.HAlign:=halCenter;
    FKGrid.CellPainter.VAlign:=valCenter;
    FKGrid.CellPainter.BackColor:= FKGrid.Colors.FixedCellBkGnd;
    FKGrid.CellPainter.Canvas.Brush.Color:= FKGrid.Colors.FixedCellBkGnd;
  end
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
  end
  else
  begin
    if FNumericColumnsIndex.Contains(ACol) then
      FKGrid.CellPainter.HAlign:=halRight;
    if ((ACol >= FKGrid.ColCount - FPivoter.SummaryDefinitions.Count) and (poVerticalGrandTotal in FPivoter.Options)) or ((ARow = FKGrid.RowCount -1) and (poHorizontalGrandTotal in FPivoter.Options) and (ACol >= FGrid.FixedCols)) then
    begin
      if State * [gdFixed, gdSelected] = [] then
      begin
        FKGrid.CellPainter.BackColor:= LighterColor(FKGrid.Colors.FixedCellBkGnd, 15);
        FKGrid.CellPainter.Canvas.Brush.Color:= FKGrid.CellPainter.BackColor;
      end;
    end;
  end;

  if d then
    FKGrid.CellPainter.DefaultDraw;
end;

procedure TmKGridAsPivotHelper.OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Shift = [ssCtrl]) and (Key = VK_C) then
  begin
    CopyTextToClipboard(FKGrid.Cells[FKGrid.Col, FKGrid.Row]);
  end;
end;


constructor TmKGridAsPivotHelper.Create;
begin
  inherited;
end;

destructor TmKGridAsPivotHelper.Destroy;
begin
  FreeAndNil(FKGridAsVirtualGrid);
  inherited Destroy;
end;

procedure TmKGridAsPivotHelper.Init(aPivoter: TmPivoter; aGrid: TKGrid);
begin
  assert(not Assigned(FKGridAsVirtualGrid));
  FKGrid := aGrid;
  FKGrid.OnDrawCell:= Self.OnDrawGridCell;
  FKGrid.Options := FKGrid.Options - [goThemes, goThemedCells] + [goColSizing];
  FKGrid.OptionsEx:= [gxMouseWheelScroll];
  FKGridAsVirtualGrid := TmKGridAsVirtualGrid.Create(FKGrid);
  FKGrid.OnKeyDown:= OnKeyDown;
  Self.InternalInit(aPivoter, FKGridAsVirtualGrid);
end;

procedure TmKGridAsPivotHelper.AutosizeColumns;
begin
  mKGridUtils.AutoSizeColumns(FKGrid);
end;


end.
