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
  {$IFDEF WINDOWS}Windows,{$ENDIF}
  mTimeruler, mGanttDataProvider, mGanttHead, mGanttGUIClasses;

type

  TmGanttStartMovingBarEvent = procedure (aBar: TmGanttBarDatum; var aAllow: boolean) of object;
  TmGanttMovingBarEvent = procedure (aBar: TmGanttBarDatum) of object;
  TmGanttEndMovingBarEvent = procedure (aBar: TmGanttBarDatum) of object;

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
    FMouseMoveData : TmGanttMouseMoveData;
    FResizingBar : boolean;
    FMovingBar : boolean;
    // external events
    FOnStartMovingBar: TmGanttStartMovingBarEvent;
    FOnMovingBar : TmGanttMovingBarEvent;
    FOnEndMovingBar : TmGanttEndMovingBarEvent;

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
    procedure SaveMouseMoveData(X, Y: integer);
    procedure NotifyBarsChanged(const AMustInvalidateGantt : boolean);
  protected
    procedure Paint; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
  public
    const DELIMITER_CLICKING_AREA : integer = 4;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property TimeRuler : TmTimeruler read FTimeRuler write SetTimeRuler;
    property Head : TmGanttHead read FHead write SetGanttHead;
    property Color;
    property VerticalLinesColor : TColor read FVerticalLinesColor write FVerticalLinesColor;
    property HorizontalLinesColor : TColor read FHorizontalLinesColor write FHorizontalLinesColor;
    property TopRow: integer read FTopRow write SetTopRow;
    // external events
    property OnStartMovingBar: TmGanttStartMovingBarEvent read FOnStartMovingBar write FOnStartMovingBar;
    property OnMovingBar : TmGanttMovingBarEvent read FOnMovingBar write FOnMovingBar;
    property OnEndMovingBar : TmGanttEndMovingBarEvent read FOnEndMovingBar write FOnEndMovingBar;
  end;


implementation

uses
  math, Forms, sysutils,
  mGanttEvents, mGanttGraphics;

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
  FCurrentDrawingEndDate := FTimeRuler.MainTimeline.Scale.CeilDate(FTimeRuler.PixelsToDateTime(aDrawingRect.Right));
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
  barRect : TRect;
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
      barRect.Left := FTimeRuler.DateTimeToPixels(currentBar.StartTime);
      barRect.Right := FTimeRuler.DateTimeToPixels(currentBar.EndTime);
      barRect.Top := trunc(aDrawingRect.Height * 0.1) + aDrawingRect.Top;
      barRect.Bottom := aDrawingRect.Bottom - trunc(aDrawingRect.Height * 0.1);
      DrawBar(aCanvas, barRect, currentBar);
    end;
  finally
    bars.Free;
  end;
end;

procedure TmGantt.SaveMouseMoveData(X, Y: integer);
var
  tempHeight : integer;
  bars : TList;
  currentBar : TmGanttBarDatum;
  left, right : integer;
begin
  FMouseMoveData.Clear;

  if not PtInRect(ClientRect, Classes.Point(X, Y)) then
    exit;

  FMouseMoveData.CurrentInstant := FTimeRuler.PixelsToDateTime(X);

  if Assigned(FHead.DataProvider) and (FHead.DataProvider.RowCount > 0) then
  begin
    tempHeight:= (FHead.DataProvider.RowCount - FHead.TopRow + 1) * FHead.RowHeight;

    if (Y >= 0) and ( Y <= tempHeight) then
    begin
      FMouseMoveData.RowIndex := Y  div FHead.RowHeight;
      {$IFDEF DEBUG}
      DebugLn('Y:' + IntToStr(Y));
      DebugLn('Row index:' + IntToStr(FMouseMoveData.RowIndex));
      {$ENDIF}
    end;

    if FMouseMoveData.RowIndex < FHead.DataProvider.RowCount then
    begin
      bars := TList.Create;
      try
        FHead.DataProvider.GetGanttBars(FMouseMoveData.RowIndex, FMouseMoveData.CurrentInstant, FMouseMoveData.CurrentInstant, bars);
        if bars.Count > 0 then
        begin
          FMouseMoveData.MouseOnBar:= true;

          currentBar := TmGanttBarDatum(bars.Items[0]);
          FMouseMoveData.CurrentBar:= currentBar;
          FMouseMoveData.CurrentBarOriginalStartTime:= currentBar.StartTime;
          FMouseMoveData.CurrentBarOriginalEndTime:= currentBar.EndTime;

          right := FTimeRuler.DateTimeToPixels(currentBar.EndTime);
          if (abs (X - right) <= DELIMITER_CLICKING_AREA) then
          begin
            FMouseMoveData.MouseOnBarDelimiter:= true;
            FMouseMoveData.MouseOnBar:= false;
          end;
          {$IFDEF DEBUG}
          DebugLn('Click on bar');
          {$ENDIF}
        end;
      finally
        bars.Free;
      end;
    end;
  end;
end;

procedure TmGantt.NotifyBarsChanged(const AMustInvalidateGantt: boolean);
begin
  if AMustInvalidateGantt then
    Self.Invalidate;
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

procedure TmGantt.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if FResizingBar or FMovingBar then
  begin
    FResizingBar := false;
    FMovingBar := false;
    if Assigned(FOnEndMovingBar) then
      FOnEndMovingBar(FMouseMoveData.CurrentBar);
    NotifyBarsChanged(false);
  end;
  Self.Cursor:= crDefault;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TmGantt.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  allow : boolean;
begin
  if (Button = mbLeft) then
  begin
    SaveMouseMoveData(X, Y);
    if FMouseMoveData.MouseOnBarDelimiter then
    begin
      FResizingBar:= true;
    end
    else if FMouseMoveData.MouseOnBar then
    begin
      if Assigned(FOnStartMovingBar) then
      begin
        allow := true;
        FOnStartMovingBar(FMouseMoveData.CurrentBar, allow);
        if allow then
          FMovingBar := true;
      end
      else
        FMovingBar := true;
    end
  end;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TmGantt.MouseMove(Shift: TShiftState; X, Y: integer);
var
  curTime : TDateTime;
  delta : Double;
begin
  if FMovingBar and ({$ifdef windows}GetAsyncKeyState{$else}GetKeyState{$endif}(VK_LBUTTON) and $8000 <> 0) then
  begin
    if Assigned(FMouseMoveData.CurrentBar) then
    begin
      curTime := FTimeRuler.PixelsToDateTime(X);
      delta := curTime - FMouseMoveData.CurrentInstant;
      FMouseMoveData.CurrentBar.StartTime := FMouseMoveData.CurrentBarOriginalStartTime + delta;
      FMouseMoveData.CurrentBar.EndTime:= FMouseMoveData.CurrentBarOriginalEndTime + delta;
      if Assigned(FOnMovingBar) then
        FOnMovingBar(FMouseMoveData.CurrentBar);
      NotifyBarsChanged(true);
    end;
  end
  else if FResizingBar and ({$ifdef windows}GetAsyncKeyState{$else}GetKeyState{$endif}(VK_LBUTTON) and $8000 <> 0) then
  begin
  end
  else
  begin
    SaveMouseMoveData(X, Y);
    if FMouseMoveData.MouseOnBarDelimiter then
      Cursor := crSizeWE
    else if FMouseMoveData.MouseOnBar then
      Cursor := crSizeAll
    else
      Cursor := crDefault;
  end;

  inherited MouseMove(Shift, X, Y);
end;

constructor TmGantt.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDoubleBufferedBitmap := Graphics.TBitmap.Create;
  {$ifdef fpc}
  DoubleBuffered:= IsDoubleBufferedNeeded;
  {$endif}
  // 4096 * 2160 = 4K
  // 7680 * 4320 = 8K
  FDoubleBufferedBitmap.SetSize(max(Screen.Width,4096), max(Screen.Height,2160));
  Self.Color:= clWhite;
  FVerticalLinesColor:= clDkGray;
  FHorizontalLinesColor:= clLtGray;
  FMouseMoveData:= TmGanttMouseMoveData.Create;
  FResizingBar:= false;
  FMovingBar:= false;
  FOnStartMovingBar := nil;
  FOnMovingBar := nil;
  FOnEndMovingBar := nil;
end;

destructor TmGantt.Destroy;
begin
  FDoubleBufferedBitmap.Free;
  FMouseMoveData.Free;
  inherited Destroy;
end;

end.
