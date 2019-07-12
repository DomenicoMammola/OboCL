// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mMenus;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Menus;

procedure SetupMainMenu(aMenu : TMainMenu);

implementation

uses
  Classes, Graphics, sysutils, contnrs, Forms,
  {$IFDEF FPC}
  LCLType,
  {$ENDIF}
  mMagnificationFactor, mGraphicsUtility;

type

  { TMenuHelper }

  TMenuHelper = class
  private
    procedure MainMenuDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; AState: TOwnerDrawState);
    procedure MainMenuMeasureItem(Sender: TObject; ACanvas: TCanvas; var AWidth, AHeight: Integer);
  end;

var
  _Helper : TMenuHelper;


procedure SetupMainMenu(aMenu: TMainMenu);
begin
  if GetMagnificationFactor <> 1 then
  begin
    aMenu.OwnerDraw:= true;
    aMenu.OnMeasureItem:= _Helper.MainMenuMeasureItem;
    aMenu.OnDrawItem:= _Helper.MainMenuDrawItem;
  end
  else
  begin
    aMenu.OwnerDraw:= false;
    aMenu.OnDrawItem:= nil;
    aMenu.OnMeasureItem:= nil;
  end;
end;

{ TMenuHelper }

procedure TMenuHelper.MainMenuDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; AState: TOwnerDrawState);
var
  s : String;
begin
  ScaleFontForMagnification(ACanvas.Font);
  (*
  ACanvas.Font.Name := 'Consolas';
  ACanvas.Font.Size := 14;
  ACanvas.Font.Style := [fsBold];
  ACanvas.Font.Color := clYellow;
  // change background
  ACanvas.Brush.Color := clBlack;
  ACanvas.Rectangle(ARect);*)
  // write caption/text
  ACanvas.Brush.Style:= bsSolid;
  ACanvas.FillRect(aRect);
  s := (Sender as TMenuItem).Caption;
  ACanvas.TextOut(ARect.Left + 2, ARect.Top + 2 , s);
end;

procedure TMenuHelper.MainMenuMeasureItem(Sender: TObject; ACanvas: TCanvas; var AWidth, AHeight: Integer);
var
  s: String;
begin
  ScaleFontForMagnification(ACanvas.Font);
  s := (Sender as TMenuItem).Caption;
  AWidth:= trunc(ACanvas.TextWidth(s) * 1.2);
  AHeight:= trunc(ACanvas.TextHeight(s) * 1.2);
end;


initialization
  _Helper := TMenuHelper.Create;

finalization
  FreeAndNil(_Helper);

end.
