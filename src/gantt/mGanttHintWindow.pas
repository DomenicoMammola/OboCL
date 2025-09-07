// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mGanttHintWindow;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, ExtCtrls, Forms, Controls,
  StdCtrls, Graphics
  {$IFDEF DELPHI}
  ,Types
  {$ENDIF}
  ;

const
  INT_GANTT_HINT_SCREEN_SPACING = 10;

type

  { TmGanttHintForm }

  TmGanttHintForm = class(THintWindow)
  strict private
    FLblText: TLabel;
    procedure HideForm(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;

    property LblText : TLabel read FLblText;
  end;

  procedure ShowGanttHintAtPos(const aText: String; aParent : TWinControl;  const x, y: Integer);
  procedure HideGanttHint;

implementation

uses
  sysutils,
  mMagnificationFactor;

const
  INT_GANTT_HINT_FORM_WIDTH  = 125;
  INT_GANTT_HINT_FORM_HEIGHT = 20;

var
  _HintForm : TmGanttHintForm;



procedure ShowGanttHintAtPos(const aText: String; aParent : TWinControl; const x, y: Integer);
var
  curParent : TWinControl;
  pt : TPoint;
begin
  if not Assigned(_HintForm) then
  begin
    _HintForm := TmGanttHintForm.Create(nil);
    _HintForm.Visible := false;
  end;

  curParent := aParent;
  while Assigned(curParent) and (not (curParent is TCustomForm)) do
    curParent := curParent.Parent;
  {$IFDEF FPC}
  if Assigned(curParent) then
  begin
    if _HintForm.Monitor.MonitorNum <> (curParent as TCustomForm).Monitor.MonitorNum then
      _HintForm.MakeFullyVisible((curParent as TCustomForm).Monitor);
  end;
  {$ENDIF}

  pt := aParent.ClientToScreen(TPoint.Create(x, y));
  if pt.x + _HintForm.Width > Screen.Width then
  begin
    _HintForm.left := pt.x - _HintForm.Width;
    if _HintForm.Left < 0 then _HintForm.Left := 0;
  end
  else
    _HintForm.left := pt.x;

  if pt.y + _HintForm.Height > Screen.Height then
  begin
    _HintForm.Top := pt.y - _HintForm.Height;
    if _HintForm.top < 0 then _HintForm.top := 0;
  end
  else
    _HintForm.top := pt.y;

  _HintForm.LblText.Caption:= aText;
  _HintForm.Show;
end;

procedure HideGanttHint;
begin
  if Assigned(_HintForm) then
    _HintForm.Hide;
end;

{ TmGanttHintForm }

procedure TmGanttHintForm.HideForm(Sender: TObject);
Var
  NoValue :TCloseAction;
begin
  NoValue := caNone;
  {$IFDEF FPC}
  if Assigned(OnClose) then
     OnClose(Self, NoValue);
  {$ENDIF}
  Hide;
end;

constructor TmGanttHintForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF FPC}
  BorderStyle := bsNone;
  {$ENDIF}

  Width := ScaleForMagnification(INT_GANTT_HINT_FORM_WIDTH, true);
  Height := ScaleForMagnification(INT_GANTT_HINT_FORM_HEIGHT, true);

  // Check for small screens. An extra spacing is necessary
  // in the Windows Mobile 5 emulator
  if Screen.Width - INT_GANTT_HINT_SCREEN_SPACING < Width then
    Width := Screen.Width - INT_GANTT_HINT_SCREEN_SPACING;
  if Screen.Height - INT_GANTT_HINT_SCREEN_SPACING < Height then
    Height := Screen.Height - INT_GANTT_HINT_SCREEN_SPACING;

  FLblText := TLabel.Create(Self);
  FLblText.Parent := Self;
  FLblText.Align:= alClient;
  FLblText.Alignment:= taCenter;
  FLblText.Transparent := True;
  FLblText.Caption := 'Text';
  FLblText.WordWrap := True;
  FLblText.ParentColor := True;
  FLblText.OnClick := HideForm;
  FLblText.Font.Color:= clWhite;
  FLblText.Layout:= tlCenter;
  ScaleFontForMagnification(FLblText.Font);

  // $DCFFFF
  Color := clDkGray; // Doesn't work on Gtk

  // Connects the methods to events
  OnClick := HideForm;

end;

destructor TmGanttHintForm.Destroy;
begin
  FLblText.Free;
  inherited Destroy;
end;

procedure TmGanttHintForm.Paint;
begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect(0,0,width,height));
end;

initialization
  _HintForm := nil;

finalization
  FreeAndNil(_HintForm);
end.
