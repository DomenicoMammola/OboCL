// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mToast;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  PopupNotifier, Classes, ExtCtrls;

type

  { TmToast }

  TmToast = class (TPopupNotifier)
  strict private
    FTimer: TTimer;
    procedure OnTimer(Sender : TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class procedure ShowText(const aText: String; const aDuration: integer = 1500);
  end;

implementation

uses
  sysutils, Forms;

var
  applicationToaster : TmToast;

{ TmToast }

procedure TmToast.OnTimer(Sender: TObject);
begin
  FTimer.Enabled:= false;
  Self.Hide;
end;

constructor TmToast.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimer := TTimer.Create(Self);
  FTimer.Interval:= 500;
  FTimer.OnTimer:= Self.OnTimer;
end;

destructor TmToast.Destroy;
begin
  inherited Destroy;
end;

class procedure TmToast.ShowText(const aText: String; const aDuration: integer = 1500);
begin
  if not Assigned(applicationToaster) then
    applicationToaster:= TmToast.Create(nil)
  else
  begin
    if applicationToaster.FTimer.Enabled then
      exit;
  end;
  applicationToaster.FTimer.Interval:= aDuration;
  applicationToaster.Title:= '';
  applicationToaster.Text:= aText;
  applicationToaster.FTimer.Enabled:= true;
  applicationToaster.ShowAtPos((Screen.Width - applicationToaster.vNotifierForm.Width) div 2, Screen.Height - 200);
end;


initialization
  applicationToaster:= nil;

finalization
  FreeAndNil(applicationToaster);
end.
