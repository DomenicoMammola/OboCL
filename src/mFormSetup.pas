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
  sysutils,
  {$IFDEF DEBUG} mLog,{$ENDIF}
  mMagnificationFactor;

{$IFDEF DEBUG}
var
  logger : TmLog;
{$ENDIF}

procedure SetupFormAndCenter(aForm: TCustomForm; const aScaleToScreenPerc: double);
var
  tmpMonitor : TMonitor;
begin
  if Screen.MonitorCount > 1 then
  begin
    {$IFDEF DEBUG}
    logger.Debug('Monitors:' + IntToStr(Screen.MonitorCount));
    logger.Debug('Screen.Width:' + IntToStr(Screen.Width));
    logger.Debug('Screen.Height:' + IntToStr(Screen.Height));
    logger.Debug('aForm.Monitor.Width:' + IntToStr(aForm.Monitor.Width));
    logger.Debug('aForm.Monitor.Height:' + IntToStr(aForm.Monitor.Height));
    logger.Debug('aForm.Left:' + IntToStr(aForm.Left));
    {$ENDIF}
    tmpMonitor := nil;
    if Assigned(Screen.ActiveForm) then
      tmpMonitor := Screen.MonitorFromWindow(Screen.ActiveForm.Handle);
    if not Assigned(tmpMonitor) then
      tmpMonitor := aForm.Monitor;
    if (aScaleToScreenPerc < 1) and (aScaleToScreenPerc > 0) then
    begin
      aForm.Width := trunc(tmpMonitor.Width * aScaleToScreenPerc);
      aForm.Height:= trunc(tmpMonitor.Height * aScaleToScreenPerc);
    end;
  end
  else
  begin
    if (aScaleToScreenPerc < 1) and (aScaleToScreenPerc > 0) then
    begin
      aForm.Width := trunc (aForm.Monitor.Width * aScaleToScreenPerc);
      aForm.Height:= trunc (aForm.Monitor.Height * aScaleToScreenPerc);
    end;
  end;

  aForm.Position:= poScreenCenter;
  ScaleFontForMagnification(aForm.Font);
end;

initialization
{$IFDEF DEBUG}
  logger := logManager.AddLog('mFormSetup');
{$ENDIF}

end.
