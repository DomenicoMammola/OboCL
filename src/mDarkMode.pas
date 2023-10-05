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

procedure SetDarkMode (const aValue : boolean);
function IsDarkModeEnabled : boolean;

implementation

var
  _DarkModeEnabled : boolean;

procedure SetDarkMode(const aValue: boolean);
begin
  _DarkModeEnabled:= aValue;
end;

function IsDarkModeEnabled: boolean;
begin
  Result := _DarkModeEnabled;
end;


initialization
  _DarkModeEnabled := falsE;

end.
