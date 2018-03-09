// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
//
// CREDITS:
// This code was adapted from PopupNotifier.TNotifierForm
// of LCL
//
unit mToast;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, ExtCtrls, Forms,
  StdCtrls, Graphics;

type

  { TmToastForm }

  TmToastForm = class(THintWindow)
  private
    lblText: TLabel;
    procedure HideForm(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  end;

  { TmToast }

  TmToast = class (TComponent)
  strict private
    FTimer: TTimer;
    FNotifierForm: TmToastForm;

    function GetColor: TColor;
    function GetOnClose: TCloseEvent;
    function GetVisible: Boolean;
    procedure SetColor(const Value: TColor);
    function GetText: string;
    procedure SetOnClose(AValue: TCloseEvent);
    procedure SetText(const Value: string);

    procedure OnTimer(Sender : TObject);
    procedure SetVisible(AValue: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Hide;
    procedure Show;
    procedure ShowAtPos(x: Integer; y: Integer);

    class procedure ShowText(const aText: String; const aDuration: integer = 2500);
  published
    property Color: TColor  read GetColor write SetColor;
    property Text: string read GetText write SetText;
    property Visible: Boolean read GetVisible write SetVisible;
    property OnClose: TCloseEvent  read GetOnClose write SetOnClose;
  end;

implementation

uses
  sysutils, Controls;

const
  INT_NOTIFIER_FORM_WIDTH  = 325;
  INT_NOTIFIER_FORM_HEIGHT = 60;
  INT_NOTIFIER_SCREEN_SPACING = 10;
  INT_NOTIFIER_SPACING = 5;

var
  applicationToaster : TmToast;

{ TmToastForm }

procedure TmToastForm.HideForm(Sender: TObject);
Var
  NoValue :TCloseAction;
begin
  if Assigned(OnClose) then
     OnClose(Self, NoValue);
    Hide;
end;

constructor TmToastForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := bsNone;

  Width := INT_NOTIFIER_FORM_WIDTH;
  Height := INT_NOTIFIER_FORM_HEIGHT;

  // Check for small screens. An extra spacing is necessary
  // in the Windows Mobile 5 emulator
  if Screen.Width - INT_NOTIFIER_SCREEN_SPACING < Width then
    Width := Screen.Width - INT_NOTIFIER_SCREEN_SPACING;

  lblText := TLabel.Create(Self);
  lblText.Parent := Self;
  lblText.Align:= alClient;
  lblText.Alignment:= taCenter;
  lblText.Transparent := True;
  lblText.Caption := 'Text';
  lblText.WordWrap := True;
  lblText.ParentColor := True;
  lblText.OnClick := HideForm;
  lblText.Font.Color:= clWhite;
  lblText.Layout:= tlCenter;

  // $DCFFFF
  Color := clDkGray; // Doesn't work on Gtk

  // Connects the methods to events
  OnClick := HideForm;
end;

destructor TmToastForm.Destroy;
begin
  lblText.Free;
  inherited Destroy;
end;

procedure TmToastForm.Paint;
begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect(0,0,width,height));
end;

{ TmToast }

function TmToast.GetColor: TColor;
begin
  FNotifierForm.Hide;
end;

function TmToast.GetOnClose: TCloseEvent;
begin
  Result := FNotifierForm.OnClose;
end;

function TmToast.GetVisible: Boolean;
begin
  Result := FNotifierForm.Visible;
end;

procedure TmToast.SetColor(const Value: TColor);
begin
  FNotifierForm.Color := Value;
end;

function TmToast.GetText: string;
begin
  Result := FNotifierForm.lblText.Caption;
end;

procedure TmToast.SetOnClose(AValue: TCloseEvent);
begin
  FNotifierForm.OnClose := AValue;
end;

procedure TmToast.SetText(const Value: string);
begin
  FNotifierForm.lblText.Caption := Value;
end;

procedure TmToast.OnTimer(Sender: TObject);
begin
  FTimer.Enabled:= false;
  Self.Hide;
end;

procedure TmToast.SetVisible(AValue: Boolean);
begin
  FNotifierForm.Visible := AValue;
end;

constructor TmToast.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNotifierForm := TmToastForm.Create(nil);
  FNotifierForm.Visible := False;

  FTimer := TTimer.Create(Self);
  FTimer.Interval:= 500;
  FTimer.OnTimer:= Self.OnTimer;
end;

destructor TmToast.Destroy;
begin
  FNotifierForm.Hide;
  FNotifierForm.Free;

  inherited Destroy;
end;

procedure TmToast.Hide;
begin
  FNotifierForm.Hide;
end;

procedure TmToast.Show;
begin
  FNotifierForm.Show;
end;

procedure TmToast.ShowAtPos(x: Integer; y: Integer);
begin
  if x + FNotifierForm.Width > Screen.Width then
  begin
    FNotifierForm.left := x - FNotifierForm.Width;
    if FNotifierForm.Left < 0 then FNotifierForm.Left := 0;
  end
  else
    FNotifierForm.left := x;

  if y + FNotifierForm.Height > Screen.Height then
  begin
    FNotifierForm.top := y - FNotifierForm.Height;
    if FNotifierForm.top < 0 then FNotifierForm.top := 0;
  end
  else
    FNotifierForm.top := y;

  FNotifierForm.Show;
end;

class procedure TmToast.ShowText(const aText: String; const aDuration: integer = 2500);
begin
  if not Assigned(applicationToaster) then
    applicationToaster:= TmToast.Create(nil)
  else
  begin
    if applicationToaster.FTimer.Enabled then
      exit;
  end;
  applicationToaster.FTimer.Interval:= aDuration;
  applicationToaster.Text:= aText;
  applicationToaster.FTimer.Enabled:= true;
  applicationToaster.ShowAtPos((Screen.Width - applicationToaster.FNotifierForm.Width) div 2, Screen.Height - 200);
end;


initialization
  applicationToaster:= nil;

finalization
  FreeAndNil(applicationToaster);
end.
