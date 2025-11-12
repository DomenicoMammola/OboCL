// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mGanttHead;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface
uses
  Classes, Controls, Graphics, contnrs,
  {$IFDEF FPC}
  LCLIntf,
  LclType,
  LResources,
  LMessages,
  {$ELSE}
  Types,
  {$ENDIF}
  {$IFDEF WINDOWS}Windows,{$ENDIF}
  mGanttDataProvider, mTimeruler, mGanttGUIClasses, mGanttEvents;

type

  TmGanttHeadEventKind = (tghLayoutChanged, tghScrolled);


  { TmGanttHead }

  TmGanttHead = class(TCustomControl)
  strict private
    FEventsSubscriptions: TObjectList;
    FDoubleBufferedBitmap: Graphics.TBitmap;
    FRowHeight : integer;
    FDataProvider : TmGanttDataProvider;
    FTimeruler : TmTimeruler;
    FTopRow : integer;
    FCellsColor : TColor;
    FResizingRows : boolean;
    FMouseMoveData : TmGanttHeadMouseMoveData;
    procedure DoPaintTo(aCanvas: TCanvas; aRect: TRect);
    procedure SetRowHeight(AValue: integer);
    procedure SetTopRow(AValue: integer);
    procedure NotifyScrolled(const AMustInvalidateHead : boolean);
    procedure NotifySubscribers(EventKind: TmGanttHeadEventKind);
    procedure NotifyLayoutChanged(const AMustInvalidateHead : boolean);
    procedure SaveMouseMoveData(X, Y: integer);
    {$ifdef fpc}
    procedure CMMouseWheel(var Message: TLMMouseEvent); message LM_MOUSEWHEEL;
    {$else}
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    {$endif}
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
    function SubscribeToEvents(SubscriberClass: TmGanttHeadEventsSubscriptionClass) : TmGanttHeadEventsSubscription;
    procedure UnsubscribeFromEvents(Subscription: TmGanttHeadEventsSubscription);
  public
    property DataProvider : TmGanttDataProvider read FDataProvider write FDataProvider;
    property Timeruler : TmTimeruler read FTimeruler write FTimeruler;
    property RowHeight : integer read FRowHeight write SetRowHeight;
    property Color;
    property CellsColor : TColor read FCellsColor write FCellsColor;
    property TopRow : integer read FTopRow write SetTopRow;
  end;

implementation

uses
  math, Forms, sysutils,
  {$IFDEF FPC}
  {$IFDEF DEBUG}
  LazLoggerBase,
  {$ELSE}
  LazLoggerDummy,
  {$ENDIF}
  {$ENDIF}
  mGanttGraphics;

{ TmGanttHead }

procedure TmGanttHead.DoPaintTo(aCanvas: TCanvas; aRect: TRect);
var
  RowRect : TRect;
  CurrentRow : integer;
  EndPos : integer;
  isFirst : boolean;
begin
  aCanvas.Lock;
  try
    aCanvas.Pen.Mode := pmCopy;
    aCanvas.Brush.Color := Self.Color;
    aCanvas.Brush.Style := bsSolid;
    aCanvas.FillRect(aRect);

    if Assigned(FDataProvider) then
    begin
      if FDataProvider.RowCount = 0 then
        Self.FTopRow:= -1
      else
      begin
        if Self.FTopRow = -1 then
          Self.FTopRow:= 0
        else
          Self.FTopRow:= min(Self.FTopRow, FDataProvider.RowCount);
      end;

      CurrentRow := FTopRow;
      EndPos := aRect.Bottom;

      RowRect.Top := max(aRect.Top, 0);
      RowRect.Left:= max(aRect.Left, 0);
      RowRect.Right:= aRect.Right;
      if Assigned (FTimeruler) then
        RowRect.Top := RowRect.Top + FTimeruler.Height;

      isFirst := true;
      while (RowRect.Top < EndPos) and (CurrentRow < DataProvider.RowCount) do
      begin
        aCanvas.Brush.Color:= FCellsColor;
        RowRect.Bottom:= RowRect.Top + (RowHeight * DataProvider.GetRowFlex(CurrentRow));
        //if RowRect.Top = RowRect.Bottom then
        //  continue;

        DrawHeadBox(ACanvas, RowRect, FDataProvider.GetHeadText(CurrentRow), taCenter, isFirst);
        isFirst:= false;

        RowRect.Top := RowRect.Bottom;
        inc (CurrentRow);
      end;
    end;
  finally
    aCanvas.Unlock;
  end;
end;

procedure TmGanttHead.SetRowHeight(AValue: integer);
begin
  if FRowHeight=AValue then Exit;
  FRowHeight:=AValue;
end;

procedure TmGanttHead.SetTopRow(AValue: integer);
var
  tmpValue : integer;
begin
  if not Assigned(FDataProvider) then
    exit;

  tmpValue := min(max(0,AValue), FDataProvider.RowCount - 1);
  if FTopRow=tmpValue then Exit;
  FTopRow:= tmpValue;
  NotifyScrolled(true);
end;

procedure TmGanttHead.NotifyScrolled(const AMustInvalidateHead : boolean);
begin
  Self.NotifySubscribers(tghScrolled);
  if AMustInvalidateHead then
    Self.Invalidate;
end;

procedure TmGanttHead.NotifySubscribers(EventKind: TmGanttHeadEventKind);
var
  f : integer;
begin
  for f := 0 to FEventsSubscriptions.Count - 1 do
  begin
    case EventKind of
      tghLayoutChanged:
        (FEventsSubscriptions.Items[f] as TmGanttHeadEventsSubscription).LayoutChanged;
      tghScrolled:
        (FEventsSubscriptions.Items[f] as TmGanttHeadEventsSubscription).Scrolled;
    end;
  end;
end;

procedure TmGanttHead.NotifyLayoutChanged(const AMustInvalidateHead: boolean);
begin
  Self.NotifySubscribers(tghLayoutChanged);
  if AMustInvalidateHead then
    Self.Invalidate;
end;

procedure TmGanttHead.SaveMouseMoveData(X, Y: integer);
var
  timerulHeight : integer;
  r, CellTop, CellBottom : integer;
begin
  FMouseMoveData.Clear;
  if not PtInRect(ClientRect, Classes.Point(X, Y)) then
    exit;
  if Assigned(FDataProvider) and (FDataProvider.RowCount > 0) then
  begin
    if Assigned(FTimeruler) then
      timerulHeight:= FTimeruler.Height
    else
      timerulHeight:= 0;

    if (Y >= timerulHeight) and (Y <= Self.Height) then
    begin
      CellTop := timerulHeight;
      r := FTopRow;
      CellBottom := CellTop + (DataProvider.GetRowFlex(r) * RowHeight);
      while (Y > CellBottom) do
      begin
        if (r < DataProvider.RowCount) then
        begin
          inc(r);
          CellTop := CellBottom;
          CellBottom := CellBottom + (DataProvider.GetRowFlex(r) * RowHeight);
        end
        else
        begin
          r := -1;
          break;
        end;
      end;

      if r >= 0 then
      begin
        FMouseMoveData.ClickOnCell:= true;
        FMouseMoveData.RowIndex := r;
        {$IFDEF FPC}
        DebugLn('Y:' + IntToStr(Y));
        DebugLn('Row index:' + IntToStr(FMouseMoveData.RowIndex));
        {$ENDIF}
      end;
    end;

    if (FMouseMoveData.ClickOnCell) then
    begin
      if (abs (Y - CellBottom) <= DELIMITER_CLICKING_AREA) then
      begin
        FMouseMoveData.ClickOnCellDelimiter:= true;
        FMouseMoveData.Distance:= Y - CellBottom;
        FMouseMoveData.Origin := CellBottom;
        {$IFDEF FPC}
        DebugLn('SaveMouseMoveData - Distance [REDUCE]:' + IntToStr(FMouseMoveData.Distance));
        {$ENDIF}
      end
      else
      if (FMouseMoveData.RowIndex <> FTopRow) and (abs (Y - CellTop ) <= DELIMITER_CLICKING_AREA) then
      begin
        FMouseMoveData.ClickOnCellDelimiter:= true;
        FMouseMoveData.Distance:= Y - CellTop;
        FMouseMoveData.Origin:= CellTop;
        {$IFDEF FPC}
        DebugLn('SaveMouseMoveData - Distance [INCREASE]:' + IntToStr(FMouseMoveData.Distance));
        {$ENDIF}
      end;
    end;
  end;
end;

{$ifdef fpc}
procedure TmGanttHead.CMMouseWheel(var Message: TLMMouseEvent);
{$else}
procedure TmGanttHead.WMMouseWheel(var Message: TWMMouseWheel); //message WM_MOUSEWHEEL;
{$endif}
var
  ScrollCount, ScrollLines: integer;
begin
  SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @ScrollLines, 0);
  ScrollCount := -ScrollLines * Message.WheelDelta div {$ifdef fpc}120{$else}WHEEL_DELTA{$endif}; // http://forum.lazarus.freepascal.org/index.php?topic=22722.0
  SetTopRow(Self.TopRow + ScrollCount);
end;


procedure TmGanttHead.Paint;
var
  drawingRect: TRect;
  tmpCanvas: TCanvas;
begin
  inherited Paint;

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

procedure TmGanttHead.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if FResizingRows then
  begin
    FResizingRows := false;
    NotifyLayoutChanged(false);
  end;
  Self.Cursor:= crDefault;
end;

procedure TmGanttHead.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if (Button = mbLeft) then
  begin
    SaveMouseMoveData(X, Y);
    if FMouseMoveData.ClickOnCellDelimiter then
    begin
      FMouseMoveData.OriginalRowHeight := Self.RowHeight;
      FMouseMoveData.CalculatedIncrement := 0;
      FResizingRows := true;
    end;
  end;
  inherited;
end;

procedure TmGanttHead.MouseMove(Shift: TShiftState; X, Y: integer);
var
  i, k : integer;
begin
  if FResizingRows and ({$ifdef windows}GetAsyncKeyState{$else}GetKeyState{$endif}(VK_LBUTTON) and $8000 <> 0) then
  begin
    if (FMouseMoveData.OriginalRowHeight = 0) then
      FMouseMoveData.OriginalRowHeight := FDataProvider.GetRowFlex(FMouseMoveData.RowIndex) * Self.RowHeight;
    if (FMouseMoveData.CalculatedIncrement = 0) then
    begin
      k := 0;
      for i := FTopRow to FMouseMoveData.RowIndex do
        inc(k, DataProvider.GetRowFlex(i));
      if FMouseMoveData.Distance > 0 then
        dec(k);
      FMouseMoveData.CalculatedIncrement:= 1 / k;
      {$IFDEF FPC}
      DebugLn('Calculated increment:' + FloatToStr(FMouseMoveData.CalculatedIncrement));
      {$ENDIF}
    end;
    RowHeight:= max(5, FMouseMoveData.OriginalRowHeight + trunc((Y - FMouseMoveData.Origin) * FMouseMoveData.CalculatedIncrement));
    NotifyLayoutChanged(true);
  end
  else
  begin
    SaveMouseMoveData(X, Y);
    if FMouseMoveData.ClickOnCellDelimiter then
      Cursor := crSizeNS
    else
      Cursor := crDefault;
  end;
  inherited;
end;

constructor TmGanttHead.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRowHeight:= 28;
  FTopRow:= 0;
  FDoubleBufferedBitmap := Graphics.TBitmap.Create;
  {$ifdef fpc}
  DoubleBuffered:= IsDoubleBufferedNeeded;
  {$endif}

  FResizingRows := false;
  Color:= clBtnFace;
  FCellsColor:= clWhite;
  FMouseMoveData := TmGanttHeadMouseMoveData.Create;

  FEventsSubscriptions := TObjectList.Create(true);

  FDoubleBufferedBitmap.SetSize(max(Screen.Width,3000), max(Screen.Height,2000));
end;

destructor TmGanttHead.Destroy;
begin
  FEventsSubscriptions.Free;
  FDoubleBufferedBitmap.Free;
  FMouseMoveData.Free;
  inherited Destroy;
end;

function TmGanttHead.SubscribeToEvents(SubscriberClass: TmGanttHeadEventsSubscriptionClass): TmGanttHeadEventsSubscription;
var
  newSubscription : TmGanttHeadEventsSubscription;
begin
  newSubscription := SubscriberClass.Create();
  FEventsSubscriptions.Add(newSubscription);
  Result := newSubscription;
end;

procedure TmGanttHead.UnsubscribeFromEvents(Subscription: TmGanttHeadEventsSubscription);
var
  i : integer;
begin
  i := FEventsSubscriptions.IndexOf(Subscription);
  if (i >= 0) then
    FEventsSubscriptions.Delete(i);
end;

end.
