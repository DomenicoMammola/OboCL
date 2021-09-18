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

{$I mDefines.inc}

interface

uses
  Classes, Controls, Graphics, Menus,
  {$IFDEF FPC}
  InterfaceBase,
  LCLIntf,
  LclType,
  LclProc,
  LResources,
  LMessages,
  {$ELSE}
  Types,
  {$ENDIF}
  {$IFDEF WINDOWS}Windows,{$ENDIF}
  mTimeruler, mGanttDataProvider, mGanttHead, mGanttGUIClasses;

type

  TmGanttAllowMovingBar = function (aBar: TmGanttBarDatum) : boolean of object;
  TmGanttAllowResizeBar = function (aBar: TmGanttBarDatum): boolean of object;
  TmGanttStartMovingBarEvent = procedure (aBar: TmGanttBarDatum) of object;
  TmGanttMovingBarEvent = procedure (aBar: TmGanttBarDatum) of object;
  TmGanttEndMovingBarEvent = procedure (aBar: TmGanttBarDatum) of object;
  TmGanttStartResizingBarEvent = procedure (aBar: TmGanttBarDatum) of object;
  TmGanttResizingBarEvent = procedure (aBar: TmGanttBarDatum) of object;
  TmGanttEndResizingBarEvent = procedure (aBar: TmGanttBarDatum) of object;
  TmGanttClickOnBarEvent = procedure (aBar: TmGanttBarDatum) of object;
  TmGanttDblClickOnBarEvent = procedure (aBar: TmGanttBarDatum) of object;


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
    FOnStartResizingBar: TmGanttStartResizingBarEvent;
    FOnResizingBar : TmGanttResizingBarEvent;
    FOnEndResizingBar : TmGanttEndResizingBarEvent;
    FOnClickOnBar : TmGanttClickOnBarEvent;
    FOnDblClickOnBar : TmGanttDblClickOnBarEvent;

    // external checks
    FAllowMovingBar : TmGanttAllowMovingBar;
    FAllowResizeBar : TmGanttAllowResizeBar;

    // popup menus
    FBarsPopupMenu : TPopupmenu;

    function GetSelectedBar: TmGanttBarDatum;
    procedure SetGanttHead(AValue: TmGanttHead);
    procedure SetTimeRuler(AValue: TmTimeruler);
    procedure PaintVerticalLines (aCanvas : TCanvas; const aDrawingRect : TRect);
    procedure PaintHorizontalLines (aCanvas : TCanvas; const aDrawingRect : TRect);
    procedure PaintBars (aCanvas : TCanvas; const aDrawingRect : TRect);
    procedure PaintHatches (aCanvas : TCanvas; const aDrawingRect : TRect);
    procedure SetTopRow(AValue: integer);
    procedure DoPaintTo(aCanvas: TCanvas; aRect: TRect);
    procedure DoForEveryRow(aCanvas: TCanvas; const aDrawingRect : TRect; aDrawingAction : TmGanttRowDrawingAction);
    procedure DrawRowBottomLine(aCanvas : TCanvas; const aDrawingRect : TRect; const aRowIndex : integer);
    procedure DrawRowBars(aCanvas : TCanvas; const aDrawingRect : TRect; const aRowIndex : integer);
    procedure DrawHatches(aCanvas : TCanvas; const aDrawingRect : TRect; const aRowIndex : integer);
    procedure SaveMouseMoveData(X, Y: integer);
    procedure NotifyBarsChanged(const AMustInvalidateGantt : boolean);
  protected
    procedure Paint; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure Click; override;
    procedure DblClick; override;
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
    property SelectedBar : TmGanttBarDatum read GetSelectedBar;
    property BarsPopupMenu : TPopupMenu read FBarsPopupMenu write FBarsPopupMenu;
    // external events
    property OnStartMovingBar: TmGanttStartMovingBarEvent read FOnStartMovingBar write FOnStartMovingBar;
    property OnMovingBar : TmGanttMovingBarEvent read FOnMovingBar write FOnMovingBar;
    property OnEndMovingBar : TmGanttEndMovingBarEvent read FOnEndMovingBar write FOnEndMovingBar;
    property OnStartResizingBar : TmGanttStartResizingBarEvent read FOnStartResizingBar write FOnStartResizingBar;
    property OnResizingBar : TmGanttResizingBarEvent read FOnResizingBar write FOnResizingBar;
    property OnEndResizingBar : TmGanttEndResizingBarEvent read FOnEndResizingBar write FOnEndResizingBar;
    property OnClickOnBar : TmGanttClickOnBarEvent read FOnClickOnBar write FOnClickOnBar;
    property OnDblClickOnBar : TmGanttDblClickOnBarEvent read FOnDblClickOnBar write FOnDblClickOnBar;

    // external checks
    property  AllowMovingBar : TmGanttAllowMovingBar read FAllowMovingBar write FAllowMovingBar;
    property  AllowResizeBar : TmGanttAllowResizeBar read FAllowResizeBar write FAllowResizeBar;
  end;


implementation

uses
  math, Forms, sysutils,
  mGanttEvents, mGanttGraphics, mGanttHintWindow;

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

function TmGantt.GetSelectedBar: TmGanttBarDatum;
begin
  if Assigned(FMouseMoveData) then
    Result := FMouseMoveData.CurrentBar
  else
    Result := nil;
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

procedure TmGantt.PaintHatches(aCanvas: TCanvas; const aDrawingRect: TRect);
begin
  FCurrentDrawingStartDate := FTimeRuler.MainTimeline.Scale.TruncDate(FTimeRuler.PixelsToDateTime(aDrawingRect.Left));
  FCurrentDrawingEndDate := FTimeRuler.MainTimeline.Scale.CeilDate(FTimeRuler.PixelsToDateTime(aDrawingRect.Right));
  DoForEveryRow(aCanvas, aDrawingRect, DrawHatches);
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

    PaintHatches(aCanvas, aRect);

    PaintVerticalLines(aCanvas, aRect);
    PaintHorizontalLines(aCanvas, aRect);

    PaintBars(aCanvas, aRect);
  finally
    aCanvas.Unlock;
  end;
end;

procedure TmGantt.DoForEveryRow(aCanvas: TCanvas; const aDrawingRect: TRect; aDrawingAction: TmGanttRowDrawingAction);
var
 k, limit : integer;
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
  bars : TmGanttBarDataList;
  i : integer;
  currentBar : TmGanttBarDatum;
  curRect : TRect;
begin
  if not Assigned(FHead) then
    exit;
  if not Assigned(FHead.DataProvider) then
    exit;

  bars := TmGanttBarDataList.Create;
  try
    FHead.DataProvider.GetGanttBars(aRowIndex, FCurrentDrawingStartDate, FCurrentDrawingEndDate, bars);
    for i := 0 to bars.Count -1 do
    begin
      currentBar := bars.Get(i);
      curRect.Left := FTimeRuler.DateTimeToPixels(currentBar.StartTime);
      curRect.Right := FTimeRuler.DateTimeToPixels(currentBar.EndTime);
      curRect.Top := trunc(aDrawingRect.Height * 0.1) + aDrawingRect.Top;
      curRect.Bottom := aDrawingRect.Bottom - trunc(aDrawingRect.Height * 0.1);
      currentBar.BarRect := curRect;
      DrawBar(aCanvas, currentBar);
    end;
  finally
    bars.Free;
  end;
end;

procedure TmGantt.DrawHatches(aCanvas: TCanvas; const aDrawingRect: TRect; const aRowIndex: integer);
var
  hatches : TmGanttHatchDataList;
  i : integer;
  currentHatch : TmGanttHatchDatum;
  curRect : TRect;
begin
  if not Assigned(FHead) then
    exit;
  if not Assigned(FHead.DataProvider) then
    exit;

  hatches := TmGanttHatchDataList.Create;
  try
    FHead.DataProvider.GetHatches(aRowIndex, FCurrentDrawingStartDate, FCurrentDrawingEndDate, hatches);
    for i := 0 to hatches.Count -1 do
    begin
      currentHatch := hatches.Get(i);
      curRect.Left := FTimeRuler.DateTimeToPixels(currentHatch.StartTime);
      curRect.Right := FTimeRuler.DateTimeToPixels(currentHatch.EndTime);
      curRect.Top := aDrawingRect.Top;
      curRect.Bottom := aDrawingRect.Bottom;
      currentHatch.HatchRect := curRect;
      DrawHatch(aCanvas, currentHatch);
    end;
  finally
    hatches.Free;
  end;
end;

procedure TmGantt.SaveMouseMoveData(X, Y: integer);
var
  tempHeight : integer;
  bars : TmGanttBarDataList;
  currentBar : TmGanttBarDatum;
  right : integer;
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
      {$IFDEF FPC}{$IFDEF DEBUG}
      DebugLn('Y:' + IntToStr(Y));
      DebugLn('Row index:' + IntToStr(FMouseMoveData.RowIndex));
      {$ENDIF}{$ENDIF}
    end;

    if FMouseMoveData.RowIndex < FHead.DataProvider.RowCount then
    begin
      bars := TmGanttBarDataList.Create;
      try
        FHead.DataProvider.GetGanttBars(FMouseMoveData.RowIndex, FMouseMoveData.CurrentInstant, FMouseMoveData.CurrentInstant, bars);
        if bars.Count > 0 then
        begin
          FMouseMoveData.MouseOnBar:= true;

          currentBar := bars.Get(0);
          FMouseMoveData.CurrentBar:= currentBar;
          FMouseMoveData.CurrentBarOriginalStartTime:= currentBar.StartTime;
          FMouseMoveData.CurrentBarOriginalEndTime:= currentBar.EndTime;

          right := FTimeRuler.DateTimeToPixels(currentBar.EndTime);
          if (abs (X - right) <= DELIMITER_CLICKING_AREA) then
          begin
            FMouseMoveData.MouseOnBarDelimiter:= true;
            FMouseMoveData.MouseOnBar:= false;
          end;
          {$IFDEF FPC}{$IFDEF DEBUG}
          DebugLn('Click on bar');
          {$ENDIF}{$ENDIF}
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
  if FResizingBar then
  begin
    FResizingBar := false;
    if Assigned(FOnEndResizingBar) then
      FOnEndResizingBar  (FMouseMoveData.CurrentBar);
  end;
  if FMovingBar then
  begin
    FMovingBar := false;
    if Assigned(FOnEndMovingBar) then
      FOnEndMovingBar(FMouseMoveData.CurrentBar);
  end;
  if FResizingBar or FMovingBar then
    NotifyBarsChanged(false);
  Self.Cursor:= crDefault;
  HideGanttHint;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TmGantt.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  pt1, pt2 : TPoint;
begin
  if (Button = mbLeft) then
  begin
    SaveMouseMoveData(X, Y);
    if FMouseMoveData.MouseOnBarDelimiter then
    begin
      if Assigned(FAllowResizeBar) then
      begin
        if AllowResizeBar(FMouseMoveData.CurrentBar) then
          FResizingBar := true;
      end
      else
        FResizingBar:= true;
    end
    else if FMouseMoveData.MouseOnBar then
    begin
      if Assigned(FAllowMovingBar) then
      begin
        if AllowMovingBar(FMouseMoveData.CurrentBar) then
          FMovingBar := true;
      end
      else
        FMovingBar := true;
    end
  end
  else if (Button = mbRight) then
  begin
    SaveMouseMoveData(X, Y);
    if Assigned(FMouseMoveData.CurrentBar) and Assigned(FBarsPopupMenu) then
    begin
      pt1 := TPoint.Create(X, Y);
      pt2 := ClientToScreen(pt1);
      FBarsPopupMenu.PopUp(pt2.x, pt2.y);
    end;
  end;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TmGantt.MouseMove(Shift: TShiftState; X, Y: integer);
var
  curTime : TDateTime;
  delta : Double;
  allow : boolean;
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
      ShowGanttHintAtPos(DateTimeToStr(FMouseMoveData.CurrentBar.StartTime), Self, FTimeRuler.DateTimeToPixels(FMouseMoveData.CurrentBar.EndTime) + INT_GANTT_HINT_SCREEN_SPACING, ((Y div FHead.RowHeight) * FHead.RowHeight) - INT_GANTT_HINT_SCREEN_SPACING);
      NotifyBarsChanged(true);
    end;
  end
  else if FResizingBar and ({$ifdef windows}GetAsyncKeyState{$else}GetKeyState{$endif}(VK_LBUTTON) and $8000 <> 0) then
  begin
    if Assigned(FMouseMoveData.CurrentBar) then
    begin
      curTime := FTimeRuler.PixelsToDateTime(X);
      delta := curTime - FMouseMoveData.CurrentInstant;
      FMouseMoveData.CurrentBar.EndTime:= FMouseMoveData.CurrentBarOriginalEndTime + delta;
      if Assigned(FOnResizingBar) then
        FOnResizingBar(FMouseMoveData.CurrentBar);
      ShowGanttHintAtPos(DateTimeToStr(FMouseMoveData.CurrentBar.EndTime), Self, FTimeRuler.DateTimeToPixels(FMouseMoveData.CurrentBar.EndTime) + INT_GANTT_HINT_SCREEN_SPACING, ((Y div FHead.RowHeight) * FHead.RowHeight) - INT_GANTT_HINT_SCREEN_SPACING);
      NotifyBarsChanged(true);
    end;
  end
  else
  begin
    SaveMouseMoveData(X, Y);
    HideGanttHint;
    if FMouseMoveData.MouseOnBarDelimiter then
    begin
      allow := true;
      if Assigned(FAllowResizeBar) then
        allow := FAllowResizeBar(FMouseMoveData.CurrentBar);
      if allow then
        Cursor := crSizeWE
      else
        Cursor := crDefault;
    end
    else if FMouseMoveData.MouseOnBar then
    begin
      allow := true;
      if Assigned(FAllowMovingBar) then
        allow := FAllowMovingBar(FMouseMoveData.CurrentBar);
      if allow then
        Cursor := crSizeAll
      else
        Cursor := crDefault;
    end
    else
      Cursor := crDefault;
  end;

  inherited MouseMove(Shift, X, Y);
end;

procedure TmGantt.Click;
begin
  if Assigned(FMouseMoveData.CurrentBar) and Assigned(FOnClickOnBar) then
    FOnClickOnBar(FMouseMoveData.CurrentBar);
  inherited Click;
end;

procedure TmGantt.DblClick;
begin
  if Assigned(FMouseMoveData.CurrentBar) and Assigned(FOnDblClickOnBar) then
    FOnDblClickOnBar(FMouseMoveData.CurrentBar);
  inherited DblClick;
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
  FOnStartResizingBar := nil;
  FOnResizingBar := nil;
  FOnEndResizingBar := nil;
  FAllowMovingBar := nil;
  FAllowResizeBar := nil;
  FOnClickOnBar := nil;
  FOnDblClickOnBar := nil;
end;

destructor TmGantt.Destroy;
begin
  FDoubleBufferedBitmap.Free;
  FMouseMoveData.Free;
  inherited Destroy;
end;

end.
