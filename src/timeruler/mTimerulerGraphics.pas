// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mTimerulerGraphics;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes,
  {$ifdef fpc}LCLIntf, LCLType, LCLProc, InterfaceBase,{$endif}
  Graphics,
  mGanttDataProvider;

procedure DrawBucketBox(ACanvas: TCanvas; const ARect: TRect; const AText: string; const ATextAlignment: TAlignment);
procedure DrawHeadBox(ACanvas: TCanvas; const ARect: TRect; const AText: string; const ATextAlignment: TAlignment; const AIsFirst : boolean);
procedure DrawBar(ACanvas: TCanvas; const ARect: TRect; aBar : TmGanttBarDatum);
{$ifdef fpc}
function IsDoubleBufferedNeeded: boolean;
{$endif}


implementation

uses
  mGraphicsUtility, SysUtils, Math {$IFDEF WINDOWS},Windows{$ENDIF} {$IFDEF FPC},graphutil{$ENDIF};

{$ifdef fpc}

procedure DrawBar(ACanvas: TCanvas; const ARect: TRect; aBar: TmGanttBarDatum);
begin
  ACanvas.Brush.Color:= aBar.Color;
  ACanvas.FillRect(ARect);
  ACanvas.Pen.Color:= DarkerColor(ACanvas.Brush.Color, 20);
  ACanvas.Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
end;

function IsDoubleBufferedNeeded: boolean;
begin
  Result:= WidgetSet.GetLCLCapability(lcCanDrawOutsideOnPaint) = LCL_CAPABILITY_YES;
end;
{$endif}

procedure WriteText(ACanvas: TCanvas; const ARect: TRect; const AText: string; ATextAlignment: TAlignment);
var
  TempFlags: cardinal;
  {$ifndef windows}
  xPos, tw : integer;
  newText : string;
  {$else}
  tmpRect : TRect;
  {$endif}
begin
  SetBkMode(ACanvas.Handle, TRANSPARENT);
  ACanvas.Font.Size := max(8, (ARect.Bottom - ARect.Top) - 10);
  {$ifndef windows}
  newText := AText;
  if (ARect.Width < ACanvas.TextWidth('..')) then
    exit;
  tw := ACanvas.TextWidth(newText);
  while tw > (ARect.Right - ARect.Left) do
  begin
    newText := Copy(newText, 1, Length(newText) - 3) + '..';
    tw := ACanvas.TextWidth(newText);
  end;
  case ATextAlignment of
    taLeftJustify: xPos := ARect.Left;
    taRightJustify: xPos := ARect.Right - tw;
    taCenter: xPos := ARect.Left + ((ARect.Width - tw) div 2);
  end;
  ACanvas.TextOut(xPos, ARect.Top, newText);
  //DebugLn(IntToStr(xPos) + ' ' + newText + ' ' + IntToStr(ACanvas.Font.Size));
  {$else}
  TempFlags := 0;
  case ATextAlignment of
    taLeftJustify: TempFlags := DT_LEFT;
    taRightJustify: TempFlags := DT_RIGHT;
    taCenter: TempFlags := DT_CENTER;
  end;
  TempFlags := TempFlags or (DT_VCENTER + DT_SINGLELINE {$ifndef fpc}DT_WORD_ELLIPSIS{$endif});

  tmpRect := ARect;
  if DrawText(ACanvas.Handle, PChar(AText), -1, tmpRect, TempFlags) = 0 then
    RaiseLastOSError;
  {$endif}
end;



procedure DrawBucketBox(ACanvas: TCanvas; const ARect: TRect; const AText: string; const ATextAlignment: TAlignment);
  procedure DrawBox(ACanvas: TCanvas; const ARect: TRect);
  var
    lack : integer;
  begin
    ACanvas.FillRect(ARect);
    ACanvas.Pen.Color:= DarkerColor(ACanvas.Brush.Color, 20);
    ACanvas.Line(ARect.Left, ARect.Bottom-1, ARect.Right, ARect.Bottom-1);
    lack := (ARect.Top - ARect.Bottom) div 4;
    ACanvas.Line(ARect.Left, ARect.Bottom + lack, ARect.Left, ARect.Top - lack);
  end;
var
  BoxRect : TRect;
begin
  BoxRect := ARect;
  DrawBox(ACanvas, BoxRect);
  InflateRect(BoxRect, -2, -2);
  WriteText(ACanvas, BoxRect, AText, ATextAlignment);
end;

procedure DrawHeadBox(ACanvas: TCanvas; const ARect: TRect; const AText: string; const ATextAlignment: TAlignment; const AIsFirst : boolean);


  procedure DrawBox;
  begin
    ACanvas.FillRect(ARect);
    ACanvas.Pen.Color:= DarkerColor(ACanvas.Brush.Color, 20);
    ACanvas.Line(ARect.Left, ARect.Bottom-1, ARect.Right, ARect.Bottom-1);
    ACanvas.Line(ARect.Left, ARect.Bottom, ARect.Left, ARect.Top);
    ACanvas.Line(ARect.Right-1, ARect.Bottom, ARect.Right-1, ARect.Top);
    if AIsFirst then
      ACanvas.Line(ARect.Left, ARect.Top, ARect.Right, ARect.Top);;
  end;
var
  BoxRect : TRect;
begin
  BoxRect := ARect;
  DrawBox;
  InflateRect(BoxRect, -2, -2);
  WriteText(ACanvas, BoxRect, AText, ATextAlignment);
end;


end.
