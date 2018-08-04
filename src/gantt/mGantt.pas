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
  mTimeruler, mGanttDataProvider, mGanttHead;

type

  TmGanttRowDrawingAction = procedure (aCanvas : TCanvas; const aDrawingRect : TRect; const aRowIndex : integer) of object;

  { TmGantt }

  TmGantt = class(TCustomControl)
  strict private
    FTimeRuler : TmTimeruler;
    FHead : TmGanttHead;
    FVerticalLinesColor : TColor;
    FHorizontalLinesColor : TColor;
    FDoubleBufferedBitmap: Graphics.TBitmap;
    FTopRow: integer;
    FCurrentDrawingStartDate, FCurrentDrawingEndDate : TDateTime;

    procedure SetGanttHead(AValue: TmGanttHead);
    procedure SetTimeRuler(AValue: TmTimeruler);
    procedure PaintVerticalLines (aCanvas : TCanvas; const aDrawingRect : TRect);
    procedure PaintHorizontalLines (aCanvas : TCanvas; const aDrawingRect : TRect);
    procedure PaintBars (aCanvas : TCanvas; const aDrawingRect : TRect);
    procedure SetTopRow(AValue: integer);
    procedure DoPaintTo(aCanvas: TCanvas; aRect: TRect);
    procedure DoForEveryRow(aCanvas: TCanvas; const aDrawingRect : TRect; aDrawingAction : TmGanttRowDrawingAction);
    procedure DrawRowBottomLine(aCanvas : TCanvas; const aDrawingRect : TRect; const aRowIndex : integer);
    procedure DrawRowBars(aCanvas : TCanvas; const aDrawingRect : TRect; const aRowIndex : integer);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property TimeRuler : TmTimeruler read FTimeRuler write SetTimeRuler;
    property Head : TmGanttHead read FHead write SetGanttHead;
    property Color;
    property VerticalLinesColor : TColor read FVerticalLinesColor write FVerticalLinesColor;
    property HorizontalLinesColor : TColor read FHorizontalLinesColor write FHorizontalLinesColor;
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

  { TmGanttGanttHeadEventsSubscription }

  TmGanttGanttHeadEventsSubscription = class (TmGanttHeadEventsSubscription)
  private
    FGantt : TmGantt;
  public
    procedure LayoutChanged; override;
    procedure Scrolled; override;
  end;

{ TmGanttGanttHeadEventsSubscription }

procedure TmGanttGanttHeadEventsSubscription.LayoutChanged;
begin
  FGantt.Invalidate;
end;

procedure TmGanttGanttHeadEventsSubscription.Scrolled;
begin
  FGantt.Invalidate;
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

procedure TmGantt.SetGanttHead(AValue: TmGanttHead);
begin
  if FHead = AValue then Exit;
  FHead := AValue;
  (FHead.SubscribeToEvents(TmGanttGanttHeadEventsSubscription) as TmGanttGanttHeadEventsSubscription).FGantt := Self;
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

procedure TmGantt.PaintHorizontalLines(aCanvas: TCanvas; const aDrawingRect: TRect);
begin
  if (not Assigned(FHead.DataProvider)) or (FHead.DataProvider.RowCount = 0) then
    exit;

  aCanvas.Pen.Color:= FHorizontalLinesColor;
  DoForEveryRow(aCanvas, aDrawingRect, Self.DrawRowBottomLine);
end;

procedure TmGantt.PaintBars(aCanvas: TCanvas; const aDrawingRect: TRect);
begin
  FCurrentDrawingStartDate := FTimeRuler.MainTimeline.Scale.TruncDate(FTimeRuler.PixelsToDateTime(aDrawingRect.Left));
  FCurrentDrawingEndDate := FTimeRuler.MainTimeline.Scale.TruncDate(FTimeRuler.PixelsToDateTime(aDrawingRect.Right));
  DoForEveryRow(aCanvas, aDrawingRect, DrawRowBars);
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
    PaintHorizontalLines(aCanvas, aRect);

    PaintBars(aCanvas, aRect);
  finally
    aCanvas.Unlock;
  end;
end;

procedure TmGantt.DoForEveryRow(aCanvas: TCanvas; const aDrawingRect: TRect; aDrawingAction: TmGanttRowDrawingAction);
var
  i, k, limit : integer;
  rowRect : TRect;
begin
  limit := FHead.DataProvider.RowCount - FHead.TopRow;

  rowRect := aDrawingRect;
  rowRect.Bottom:= aDrawingRect.Top + FHead.RowHeight -1;
  k := 0;
  while (rowRect.Bottom < aDrawingRect.Bottom) and (k < limit) do
  begin
    aDrawingAction(aCanvas, rowRect, FHead.TopRow + k);

    rowRect.Top := rowRect.Bottom + 1;
    rowRect.Bottom := rowRect.Bottom + FHead.RowHeight;
    inc(k);
  end;

end;

procedure TmGantt.DrawRowBottomLine(aCanvas: TCanvas; const aDrawingRect: TRect; const aRowIndex : integer);
begin
  aCanvas.MoveTo(aDrawingRect.Left, aDrawingRect.Bottom);
  aCanvas.LineTo(aDrawingRect.Right, aDrawingRect.Bottom);
end;

procedure TmGantt.DrawRowBars(aCanvas: TCanvas; const aDrawingRect: TRect; const aRowIndex: integer);
var
  bars : TList;
  i : integer;
  currentBar : TmGanttBarDatum;
begin
  if not Assigned(FHead) then
    exit;
  if not Assigned(FHead.DataProvider) then
    exit;

  bars := TList.Create;
  try
    FHead.DataProvider.GetGanttBars(aRowIndex, FCurrentDrawingStartDate, FCurrentDrawingEndDate, bars);
    for i := 0 to bars.Count -1 do
    begin
      currentBar := TmGanttBarDatum(bars.Items[i]);


    end;
  finally
    bars.Free;
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
  FHorizontalLinesColor:= clLtGray;
end;

destructor TmGantt.Destroy;
begin
  FDoubleBufferedBitmap.Free;
  inherited Destroy;
end;

end.
