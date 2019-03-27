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
  mMagnificationFactor;

procedure SetupFormAndCenter(aForm: TCustomForm; const aScaleToScreenPerc: double);
begin
  if Screen.MonitorCount > 1 then
  begin
    aForm.Left := Screen.Monitors[0].Left;
    aForm.Top := Screen.Monitors[0].Top;
  end;

  if (aScaleToScreenPerc < 1) and (aScaleToScreenPerc > 0) then
  begin
    aForm.Width:= trunc(Screen.Width * aScaleToScreenPerc);
    aForm.Height:= trunc (Screen.Height * aScaleToScreenPerc);
  end;

  aForm.Position:= poScreenCenter;
  ScaleFontForMagnification(aForm.Font);
end;

end.
