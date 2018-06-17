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
  Graphics;

procedure DrawBucketBox(ACanvas: TCanvas; ARect: TRect; const Text: string; TextAlignment: TAlignment);
{$ifdef fpc}
function IsDoubleBufferedNeeded: boolean;
{$endif}


implementation

uses
  mGraphicsUtility, SysUtils, Math {$IFDEF WINDOWS},Windows{$ENDIF} {$IFDEF FPC},graphutil{$ENDIF};

{$ifdef fpc}
function IsDoubleBufferedNeeded: boolean;
begin
  Result:= WidgetSet.GetLCLCapability(lcCanDrawOutsideOnPaint) = LCL_CAPABILITY_YES;
end;
{$endif}


procedure DrawBox(ACanvas: TCanvas; ARect: TRect);
var
  lack : integer;
begin
  ACanvas.FillRect(ARect);
  ACanvas.Pen.Color:= DarkerColor(ACanvas.Brush.Color, 20);
  ACanvas.Line(ARect.Left, ARect.Bottom-1, ARect.Right, ARect.Bottom-1);
  lack := (ARect.Top - ARect.Bottom) div 4;
  ACanvas.Line(ARect.Left, ARect.Bottom + lack, ARect.Left, ARect.Top - lack);
end;

procedure WriteText(ACanvas: TCanvas; ARect: TRect; const Text: string; TextAlignment: TAlignment);
var
  TempFlags: cardinal;
  {$ifndef windows}
  xPos, tw : integer;
  newText : string;
  {$endif}
begin
  SetBkMode(ACanvas.Handle, TRANSPARENT);
  ACanvas.Font.Size := max(8, (ARect.Bottom - ARect.Top) - 10);
  {$ifndef windows}
  newText := Text;
  if (ARect.Width < ACanvas.TextWidth('..')) then
    exit;
  tw := ACanvas.TextWidth(newText);
  while tw > (ARect.Right - ARect.Left) do
  begin
    newText := Copy(newText, 1, Length(newText) - 3) + '..';
    tw := ACanvas.TextWidth(newText);
  end;
  case TextAlignment of
    taLeftJustify: xPos := ARect.Left;
    taRightJustify: xPos := ARect.Right - tw;
    taCenter: xPos := ARect.Left + ((ARect.Width - tw) div 2);
  end;
  ACanvas.TextOut(xPos, ARect.Top, newText);
  //DebugLn(IntToStr(xPos) + ' ' + newText + ' ' + IntToStr(ACanvas.Font.Size));
  {$else}
  TempFlags := 0;
  case TextAlignment of
    taLeftJustify: TempFlags := DT_LEFT;
    taRightJustify: TempFlags := DT_RIGHT;
    taCenter: TempFlags := DT_CENTER;
  end;
  TempFlags := TempFlags or (DT_VCENTER + DT_SINGLELINE {$ifndef fpc}DT_WORD_ELLIPSIS{$endif});

  if DrawText(ACanvas.Handle, PChar(Text), -1, ARect, Flags) = 0 then
    RaiseLastOSError;
  {$endif}
end;



procedure DrawBucketBox(ACanvas: TCanvas; ARect: TRect; const Text: string; TextAlignment: TAlignment);
begin
  DrawBox(ACanvas, ARect);
  InflateRect(ARect, -2, -2);
  WriteText(ACanvas, ARect, Text, TextAlignment);
end;

end.
