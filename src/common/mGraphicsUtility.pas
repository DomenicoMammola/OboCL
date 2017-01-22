// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mGraphicsUtility;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Windows, Graphics, Types;

type
  TRectangleSide = (rsCenter, rsTop, rsLeft, rsBottom, rsRight, rsOutside);

  procedure GetScreenShot (aBitmap : Graphics.TBitmap);
  procedure CropBitmap (aSourceBitmap, aDestBitmap : Graphics.TBitmap; X, Y : integer);
  function PointInTriangle (pt, v1, v2, v3 : TPoint) : boolean;
  function RectangleSide (aRect : TRect; aX, aY : integer) : TRectangleSide;

  procedure CopyDraw(ACanvas: TCanvas; Bitmap: TBitmap; const ARect: TRect);

  function DarkerColor(aColor: TColor; percent: Byte): TColor;
  function LighterColor(aColor: TColor; percent: Byte): TColor;

implementation

uses
  SysUtils, Math {$IFDEF FPC},graphutil{$ENDIF};

{$IFDEF FPC}
const
  HLSMAX = 255;
{$ENDIF}

  procedure RGBToHLS(aColor: TColor; var H, L, S: Double);
  var
    HW, SW, LW: Word;
  begin
    ColorRGBToHLS(ColorToRgb(aColor), HW, LW, SW);
    H := HW / HLSMAX;
    L := LW / HLSMAX;
    S := SW / HLSMAX;
  end;

  function HLSToRGB(H, L, S: Double): TColor;
  begin
    Result := ColorHLSToRGB(Round(H * HLSMAX),
      Round(L * HLSMAX),
      Round(S * HLSMAX))
  end;

  function GetLuminance(aColor: TColor): double;
  var
    H, S, L: Double;
  begin
    RGBToHLS(ColorToRgb(aColor), H, L, S);
    Result := L;
  end;

  function SetLuminance(aColor: TColor; luminance: double): TColor;
  var
    H, S, L: double;
  begin
    RGBToHLS(ColorToRgb(aColor), H, L, S);
    luminance := Max(0, Min(luminance, 1));
    Result := HLSToRGB(H, luminance, S);
  end;

  function DarkerColor(aColor: TColor; percent: Byte): TColor;
  var
    l: double;
  begin
    l := Max(GetLuminance(aColor) - (percent/100), 0.0);
    Result := SetLuminance(aColor, l);
  end;

  (* lighter color of thisColor by thePercent value *)
  function LighterColor(aColor: TColor; percent: Byte): TColor;
  var
    cRed,
    cGreen,
    cBlue: Byte;
  begin
    cRed := GetRValue(aColor);
    cGreen := GetGValue(aColor);
    cBlue := GetBValue(aColor);
    (* a byte's range is from 0 to 255
       so Red, Green and Blue can have
       a value between 0 and 255 *)
    cRed :=
      Round(cRed * percent / 100) +
      Round(255 - percent / 100 * 255);
    cGreen :=
      Round(cGreen * percent / 100) +
      Round(255 - percent / 100 * 255);
    cBlue :=
      Round(cBlue * percent / 100) +
      Round(255 - percent / 100 * 255);
    Result := RGB(cRed, cGreen, cBlue);
  end;

procedure GetScreenShot (aBitmap : Graphics.TBitmap);
var
  DC : HDC;
begin
  DC := GetDC (GetDesktopWindow) ;
  try
    aBitmap.Width := GetDeviceCaps (DC, HORZRES) ;
    aBitmap.Height := GetDeviceCaps (DC, VERTRES) ;
    BitBlt(aBitmap.Canvas.Handle, 0, 0, aBitmap.Width, aBitmap.Height, DC, 0, 0, SRCCOPY) ;
  finally
    ReleaseDC (GetDesktopWindow, DC) ;
  end;
end;

procedure CropBitmap (aSourceBitmap, aDestBitmap : Graphics.TBitmap; X, Y : integer);
begin
  BitBlt(aDestBitmap.Canvas.Handle, 0, 0, aDestBitmap.Width, aDestBitmap.Height, aSourceBitmap.Canvas.Handle, X, Y, SRCCOPY);
end;

function Orientation(const x1, y1, x2, y2, Px, Py: Double): Integer;
var
  Orin: Double;
begin
  (* Linear determinant of the 3 points *)
  Orin := (x2 - x1) * (py - y1) - (px - x1) * (y2 - y1);

  if Orin > 0.0 then
    Result := +1             (* Orientaion is to the right-hand side *)
  else if Orin < 0.0 then
    Result := -1             (* Orientaion is to the left-hand side  *)
  else
    Result := 0;             (* Orientaion is neutral aka collinear  *)
end;
(* End of Orientation *)


function PointInTriangle (pt, v1, v2, v3 : TPoint) : boolean;
var
  Or1: Integer;
  Or2: Integer;
  Or3: Integer;
begin
  Or1 := Orientation(v1.x, v1.y, v2.x, v2.y, pt.x, pt.y);
  Or2 := Orientation(v2.x, v2.y, v3.x, v3.y, pt.x, pt.y);
  Or3 := Orientation(v3.x, v3.y, v1.x, v1.y, pt.x, pt.y);

  if (Or1 = Or2) and (Or2 = Or3) then
    Result := True
  else if Or1 = 0 then
    Result := (Or2 = 0) or (Or3 = 0)
  else if Or2 = 0 then
    Result := (Or1 = 0) or (Or3 = 0)
  else if Or3 = 0 then
    Result := (Or2 = 0) or (Or1 = 0)
  else
    Result := False;
end;

function RectangleSide (aRect : TRect; aX, aY : integer) : TRectangleSide;
var
  MidX, MidY : integer;
  TempPoint, V1, V2, V3 : TPoint;
begin
  if (aX < aRect.Left) or (aX > aRect.Right) or (aY < aRect.Top) or (aY > aRect.Bottom) then
  begin
    Result := rsOutside;
    exit;
  end; 
 
  MidX := aRect.Left + ((aRect.Right - aRect.Left) div 2);
  MidY := aRect.Top + ((aRect.Bottom - aRect.Top) div 2);
  TempPoint.X := aX;
  TempPoint.Y := aY;
  V1.X := MidX;
  V1.Y := MidY;
  V2.X := aRect.Left;
  V2.Y := aRect.Top;
  V3.X := aRect.Left;
  V3.Y := aRect.Bottom;
  
  if PointInTriangle(TempPoint, V1, V2, V3)  then
  begin
    Result := rsLeft;
    exit;
  end;

  V3.X := aRect.Right;
  V3.Y := aRect.Top;
  if PointInTriangle(TempPoint, V1, V2, V3) then
  begin
    Result := rsTop;
    exit;    
  end;

  V2.X := aRect.Right;
  V2.Y := aRect.Bottom;
  if PointInTriangle(TempPoint, V1, V2, V3) then
  begin
    Result := rsRight;
    exit;    
  end;

  Result := rsBottom;  
end;

// Copies bitmap onto ACanvas
procedure CopyDraw(ACanvas: TCanvas; Bitmap: TBitmap; const ARect: TRect);
begin
  BitBlt(ACanvas.Handle,
         ARect.Left, ARect.Top, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top,
         Bitmap.Canvas.Handle, 0, 0, SRCCOPY);
end;

end.
