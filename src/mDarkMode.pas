// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mDarkMode;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Graphics;

type
  TmColorsTheme = record
    ColorBg,
    ColorCellBg,
    ColorAlternateCellBg,
    ColorCellText,
    ColorGridLines,
    ColorSelectedCellBg,
    ColorSelectedCellText,
    ColorFocusedCellBg,
    ColorFocusedCellText,
    ColorTitleText,
    ColorDataModifiedTitleText,
    ColorLabelText,
    ColorDisabledCellText,
    ColorStandardEditorText : TColor;
  end;

procedure SetDarkMode (const aValue : boolean);
function IsDarkModeEnabled : boolean;

function GetActiveTheme : TmColorsTheme;

implementation

var
  _DarkModeEnabled : boolean;
  _ActiveTheme : TmColorsTheme;

procedure SetDarkMode(const aValue: boolean);
begin
  _DarkModeEnabled:= aValue;
end;

function IsDarkModeEnabled: boolean;
begin
  Result := _DarkModeEnabled;
end;

function GetActiveTheme: TmColorsTheme;
begin
  Result := _ActiveTheme;
end;

procedure FillDefaultDarkTheme;
begin
  _ActiveTheme.ColorBg := $3d3c3b;
  _ActiveTheme.ColorCellBg := $303030;
  _ActiveTheme.ColorAlternateCellBg := $414141;
  _ActiveTheme.ColorCellText := $d4d4d4;
  _ActiveTheme.ColorGridLines := $404040;
  _ActiveTheme.ColorSelectedCellBg := $787878; //$5c5c5c; // $444343;
  _ActiveTheme.ColorSelectedCellText := $dadada; // $cccccc;
  _ActiveTheme.ColorFocusedCellBg := $89633d;
  _ActiveTheme.ColorFocusedCellText := $f9e5dd;
  _ActiveTheme.ColorTitleText := $b5b5b4;
  _ActiveTheme.ColorDataModifiedTitleText := $64bc73;
  _ActiveTheme.ColorLabelText := $8e8e8e;
  _ActiveTheme.ColorDisabledCellText:= $ffffff;
  _ActiveTheme.ColorStandardEditorText := $000000;
end;

initialization
  _DarkModeEnabled := false;
  FillDefaultDarkTheme;


end.
