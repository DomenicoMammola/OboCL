// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mFormSetup;

interface

uses
  Forms;

procedure SetupFormAndCenter (aForm : TCustomForm; const aScaleToScreenPerc: double = 1.0);

implementation

uses
  sysutils, math,
  {$IFDEF DEBUG} mLog,{$ENDIF}
  mMagnificationFactor;

{$IFDEF DEBUG}
var
  logger : TmLog;
{$ENDIF}

procedure SetupFormAndCenter(aForm: TCustomForm; const aScaleToScreenPerc: double);
var
  minWidth, minHeight, i : integer;
begin
  if (aScaleToScreenPerc < 1) and (aScaleToScreenPerc > 0) then
  begin
    minWidth:= MaxInt;
    minHeight:= MaxInt;

    for i := 0 to Screen.MonitorCount - 1 do
    begin
      minWidth:= min(minWidth, trunc((Screen.Monitors[i].Width / (Screen.Monitors[i].PixelsPerInch / 96))));
      minHeight:= min(minHeight, trunc((Screen.Monitors[i].Height / (Screen.Monitors[i].PixelsPerInch / 96))));
    end;
    aForm.Width := trunc (minWidth * aScaleToScreenPerc);
    aForm.Height:= trunc (minHeight * aScaleToScreenPerc);
  end;

  aForm.Position:= poScreenCenter;
  aForm.DefaultMonitor:= dmActiveForm;
  ScaleFontForMagnification(aForm.Font);
end;

initialization
{$IFDEF DEBUG}
  logger := logManager.AddLog('mFormSetup');
{$ENDIF}

end.
