// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mGanttGraphics;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes,
  {$ifdef fpc}LCLIntf, LCLType, LCLProc, InterfaceBase,{$else}Types, WinTypes, WinProcs,{$endif}
  {$ifdef windows}Windows,{$endif}
  Graphics,
  mGanttDataProvider;

procedure DrawBucketBox(ACanvas: TCanvas; const ARect: TRect; const AText: string; const ATextAlignment: TAlignment);
procedure DrawHeadBox(ACanvas: TCanvas; const ARect: TRect; const AText: string; const ATextAlignment: TAlignment; const AIsFirst : boolean);
procedure DrawBar(ACanvas: TCanvas; aBar : TmGanttBarDatum);
procedure DrawHatch(ACanvas: TCanvas; aHatch : TmGanttHatchDatum);
{$ifdef fpc}
function IsDoubleBufferedNeeded: boolean;
{$endif}


implementation

uses
  SysUtils,
  mGraphicsUtility;

var
  DottedBrush : HBrush;

procedure DrawBar(ACanvas: TCanvas; aBar: TmGanttBarDatum);
begin
  ACanvas.Pen.Style:= psSolid;
  ACanvas.Brush.Color:= aBar.Color;
  ACanvas.Brush.Style:= bsSolid;
  ACanvas.FillRect(aBar.BarRect);
  ACanvas.Pen.Color:= aBar.BorderColor;
  ACanvas.Rectangle(aBar.BarRect.Left, aBar.BarRect.Top, aBar.BarRect.Right, aBar.BarRect.Bottom);
end;

procedure DrawHatch(ACanvas: TCanvas; aHatch: TmGanttHatchDatum);
const
  Bits: array[0..5] of Word = (0, 0, 9, 0, 0 , 9);
var
  tmpBitmap: HBitmap;
begin
  if DottedBrush = 0 then
  begin
    tmpBitmap := CreateBitmap(6, 6, 1, 1, @Bits);
    DottedBrush := CreatePatternBrush(tmpBitmap);
    DeleteObject(tmpBitmap);
  end;

  ACanvas.Font.Color := clWhite;
  ACanvas.Brush.Color := aHatch.Color;
  FillRect(ACanvas.Handle, aHatch.HatchRect, DottedBrush);
end;


{$ifdef fpc}
function IsDoubleBufferedNeeded: boolean;
begin
  Result:= WidgetSet.GetLCLCapability(lcCanDrawOutsideOnPaint) = LCL_CAPABILITY_YES;
end;
{$endif}

procedure DrawLine(aCanvas: TCanvas; aX, aY, aX2, aY2 : integer);
begin
  {$ifdef fpc}
    aCanvas.Line(aX, aY, aX2, aY2);
  {$else}
    aCanvas.MoveTo(aX, aY);
    aCanvas.LineTo(aX2, aY2);
  {$endif}
end;

procedure DrawBucketBox(ACanvas: TCanvas; const ARect: TRect; const AText: string; const ATextAlignment: TAlignment);
  procedure DrawBox(ACanvas: TCanvas; const ARect: TRect);
  var
    lack : integer;
  begin
    ACanvas.FillRect(ARect);
    ACanvas.Pen.Color:= DarkerColor(ACanvas.Brush.Color, 20);
    DrawLine(ACanvas, ARect.Left, ARect.Bottom-1, ARect.Right, ARect.Bottom-1);
    lack := (ARect.Top - ARect.Bottom) div 4;
    DrawLine(ACanvas, ARect.Left, ARect.Bottom + lack, ARect.Left, ARect.Top - lack);
  end;
var
  BoxRect : TRect;
begin
  BoxRect := ARect;
  DrawBox(ACanvas, BoxRect);
  InflateRect(BoxRect, -2, -2);
  WriteText(ACanvas, BoxRect, AText, ATextAlignment, true);
end;

procedure DrawHeadBox(ACanvas: TCanvas; const ARect: TRect; const AText: string; const ATextAlignment: TAlignment; const AIsFirst : boolean);


  procedure DrawBox;
  begin
    ACanvas.FillRect(ARect);
    ACanvas.Pen.Color:= DarkerColor(ACanvas.Brush.Color, 20);
    DrawLine(ACanvas, ARect.Left, ARect.Bottom-1, ARect.Right, ARect.Bottom-1);
    DrawLine(ACanvas, ARect.Left, ARect.Bottom, ARect.Left, ARect.Top);
    DrawLine(ACanvas, ARect.Right-1, ARect.Bottom, ARect.Right-1, ARect.Top);
    if AIsFirst then
      DrawLine(ACanvas, ARect.Left, ARect.Top, ARect.Right, ARect.Top);
  end;
var
  BoxRect : TRect;
begin
  BoxRect := ARect;
  DrawBox;
  InflateRect(BoxRect, -2, -2);
  WriteText(ACanvas, BoxRect, AText, ATextAlignment, true);
end;

initialization

finalization
  if DottedBrush <> 0 then
    DeleteObject(DottedBrush);

end.
