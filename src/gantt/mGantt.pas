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
  mTimeruler, mGanttDataProvider;

type

  { TmGantt }

  TmGantt = class(TCustomControl)
  strict private
    FTimeRuler : TmTimeruler;
    FVerticalLinesColor : TColor;
    FDoubleBufferedBitmap: Graphics.TBitmap;
    FTopRow: integer;
    procedure SetTimeRuler(AValue: TmTimeruler);
    procedure PaintVerticalLines (aCanvas : TCanvas; const aDrawingRect : TRect);
    procedure SetTopRow(AValue: integer);
    procedure DoPaintTo(aCanvas: TCanvas; aRect: TRect);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property TimeRuler : TmTimeruler read FTimeRuler write SetTimeRuler;
    property Color;
    property VerticalLinesColor : TColor read FVerticalLinesColor write FVerticalLinesColor;
    property TopRow: integer read FTopRow write SetTopRow;
  end;


implementation

uses
  math, Forms,
  mGanttEvents, mTimerulerGraphics;

type

  { TmGanttTimerulerEventsSubscription }

  TmGanttTimerulerEventsSubscription = class (TmTimerulerEventsSubscription)
  private
    FGantt : TmGantt;
  public
    procedure LayoutChanged; override;
    procedure DateChanged(const OldDate: TDateTime); override;
  end;

{ TmGanttTimerulerEventsSubscription }

procedure TmGanttTimerulerEventsSubscription.LayoutChanged;
begin
  FGantt.Invalidate;
end;

procedure TmGanttTimerulerEventsSubscription.DateChanged(const OldDate: TDateTime);
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

procedure TmGantt.PaintVerticalLines(aCanvas: TCanvas; const aDrawingRect : TRect);
var
  startDate, endDate, tmpDate : TDateTime;
  i : integer;
begin
  aCanvas.Pen.Color:= FVerticalLinesColor;

  startDate := FTimeRuler.PixelsToDateTime(aDrawingRect.Left);
  endDate := FTimeRuler.PixelsToDateTime(aDrawingRect.Right);

  tmpDate := FTimeRuler.MainTimeline.Scale.NextBucket(startDate);
  while tmpDate < endDate do
  begin
    i := FTimeRuler.DateTimeToPixels(tmpDate);
    aCanvas.MoveTo(i, aDrawingRect.Top);
    aCanvas.LineTo(i, aDrawingRect.Bottom);

    tmpDate := FTimeRuler.MainTimeline.Scale.NextBucket(tmpDate);
  end;
end;

procedure TmGantt.SetTopRow(AValue: integer);
begin
  if FTopRow = AValue then Exit;
  FTopRow:= max(0,AValue);
  Invalidate;
end;

procedure TmGantt.DoPaintTo(aCanvas: TCanvas; aRect: TRect);
begin
  aCanvas.Lock;
  try
    aCanvas.Pen.Mode := pmCopy;
    aCanvas.Brush.Color := Self.Color;
    aCanvas.Brush.Style := bsSolid;
    aCanvas.FillRect(aRect);

    PaintVerticalLines(aCanvas, aRect);
  finally
    aCanvas.Unlock;
  end;
end;

procedure TmGantt.Paint;
var
  drawingRect : TRect;
  tmpCanvas : TCanvas;
begin
  if not Assigned(FTimeRuler) then
    exit;

  if not Assigned(FTimeRuler.MainTimeline) then
    exit;

  drawingRect := ClientRect;

  if DoubleBuffered then
  begin
    tmpCanvas := FDoubleBufferedBitmap.Canvas;
    tmpCanvas.Font.Assign(Self.Font);
  end
  else
    tmpCanvas := Self.Canvas;

  DoPaintTo(tmpCanvas, drawingRect);

  if DoubleBuffered then
  begin
    Brush.Style := bsClear;
    Canvas.CopyRect(drawingRect, tmpCanvas, drawingRect);
  end;
end;

constructor TmGantt.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDoubleBufferedBitmap := Graphics.TBitmap.Create;
  {$ifdef fpc}
  DoubleBuffered:= IsDoubleBufferedNeeded;
  {$endif}

  FDoubleBufferedBitmap.SetSize(max(Screen.Width,3000), max(Screen.Height,2000));
  Self.Color:= clWhite;
  FVerticalLinesColor:= clDkGray;
end;

destructor TmGantt.Destroy;
begin
  FDoubleBufferedBitmap.Free;
  inherited Destroy;
end;

end.
