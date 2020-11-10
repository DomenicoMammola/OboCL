// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mMagnificationFactor;

interface

uses
  Graphics;

function GetMagnificationFactor : double;
procedure SetMagnificationFactor (const aValue : double);

function ScaleForMagnification (const aOriginalSize : integer; const aScaleForDPI : boolean): integer;
procedure ScaleFontForMagnification (aFont : TFont);


implementation

uses
  mGraphicsUtility;

var
  _MagnificationFactor : double = 1;

function GetMagnificationFactor: double;
begin
  Result := _MagnificationFactor;
end;

procedure SetMagnificationFactor(const aValue: double);
begin
  if aValue > 0 then
    _MagnificationFactor:= aValue;
end;

function ScaleForMagnification(const aOriginalSize: integer; const aScaleForDPI : boolean): integer;
begin
  Result := round (aOriginalSize * _MagnificationFactor);
  if aScaleForDPI then
    Result := ScaleForDPI(Result);
end;

procedure ScaleFontForMagnification (aFont : TFont);
var
  RealFontSize : integer;
begin
  if (aFont.Size = 0) and (_MagnificationFactor = 1) then
    exit
  else
  begin
    if aFont.Size = 0 then
    begin
      // http://forum.lazarus.freepascal.org/index.php?topic=39283.0
      RealFontSize := Round((- GetFontData(aFont.Handle).Height * 72 / aFont.PixelsPerInch));
      aFont.Size := ScaleForDPI(trunc (RealFontSize * _MagnificationFactor));
    end
    else
      aFont.Height := ScaleForDPI(trunc(aFont.Height * _MagnificationFactor));
  end;
end;




end.
