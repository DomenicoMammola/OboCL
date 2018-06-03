// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mGantt;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, Controls, Graphics,
  {$IFDEF FPC}
  InterfaceBase,
  LCLIntf,
  LclType,
  LclProc,
  LResources,
  LMessages,
  {$ENDIF}
  mTimeruler;

type

  { TmGantt }

  TmGantt = class(TCustomControl)
  strict private
    FTimeRuler : TmTimeruler;
    FRowHeight : integer;
    FRowColor : TColor;
    procedure SetTimeRuler(AValue: TmTimeruler);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property TimeRuler : TmTimeruler read FTimeRuler write SetTimeRuler;
    property RowHeight : integer read FRowHeight write FRowHeight;
  end;


implementation

uses
  mTimerulerEvents;

type

  { TmGanttTimerulerEventsSubscription }

  TmGanttTimerulerEventsSubscription = class (TmTimerulerEventsSubscription)
  private
    FGantt : TmGantt;
  public
    procedure LayoutChanged; override;
    procedure DateChanged(OldDate: TDateTime); override;
  end;

{ TmGanttTimerulerEventsSubscription }

procedure TmGanttTimerulerEventsSubscription.LayoutChanged;
begin
  FGantt.Invalidate;
end;

procedure TmGanttTimerulerEventsSubscription.DateChanged(OldDate: TDateTime);
begin
  FGantt.Invalidate;
end;

{ TmGantt }

procedure TmGantt.SetTimeRuler(AValue: TmTimeruler);
begin
  if FTimeRuler=AValue then Exit;
  FTimeRuler := AValue;
  (FTimeRuler.SubscribeToEvents(TmGanttTimerulerEventsSubscription) as TmGanttTimerulerEventsSubscription).FGantt := Self;
end;

procedure TmGantt.Paint;
var
  drawingRect, actualBarsRect, noBarZoneRect, tmpRect : TRect;
  startDate, endDate, tmpDate : TDateTime;
  TopRowIndex, i : integer;
begin
  if not Assigned(FTimeRuler) then
    exit;

  if not Assigned(FTimeRuler.MainTimeline) then
    exit;

  drawingRect := Canvas.ClipRect;
  // drawingRect := ClientRect;

  //TopRowIndex := PixelsToBarIndex(drawingRect.Top);

  startDate := FTimeRuler.PixelsToDateTime(drawingRect.Left);
  endDate := FTimeRuler.PixelsToDateTime(drawingRect.Right);

  actualBarsRect := drawingRect;
  SetRectEmpty(noBarZoneRect);

  Canvas.Pen.Mode := pmCopy;
  Canvas.Brush.Color := FRowColor;
  Canvas.Brush.Style := bsSolid;
  IntersectRect(tmpRect, drawingRect, actualBarsRect);
  Canvas.FillRect(tmpRect);

  Canvas.Brush.Color := Self.Color;
  IntersectRect(tmpRect, drawingRect, noBarZoneRect);
  Canvas.FillRect(tmpRect);

  Canvas.Pen.Color:= clBlack;

  tmpDate := FTimeRuler.MainTimeline.Scale.NextBucket(startDate);
  while tmpDate < endDate do
  begin
    i := FTimeRuler.DateTimeToPixels(tmpDate);
    Canvas.MoveTo(i, actualBarsRect.Top);
    Canvas.LineTo(i, actualBarsRect.Bottom);

    tmpDate := FTimeRuler.MainTimeline.Scale.NextBucket(tmpDate);
  end;

end;

constructor TmGantt.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRowHeight:= 18;
  FRowColor := clYellow;
end;

destructor TmGantt.Destroy;
begin
  inherited Destroy;
end;

end.
