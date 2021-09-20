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

{$I mDefines.inc}

interface
uses
  Classes, Controls, Graphics, contnrs,
  {$IFDEF FPC}
  LCLIntf,
  LclType,
  LclProc,
  LResources,
  {$IFDEF DEBUG}LazLogger,{$ENDIF}
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
        RowRect.Bottom:= RowRect.Top + RowHeight;
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
begin
  if FTopRow=AValue then Exit;
  FTopRow:= max(0,AValue);
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
  tempHeight, timerulHeight : integer;
  CurrentPixelTop, CurrentPixelBottom : integer;
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

    tempHeight:= (FDataProvider.RowCount - FTopRow + 1) * RowHeight;

    if (Y >= timerulHeight) and ( Y <= tempHeight) then
    begin
      FMouseMoveData.ClickOnCell:= true;
      FMouseMoveData.RowIndex := max(0,(Y - timerulHeight - DELIMITER_CLICKING_AREA))  div RowHeight;
      {$IFDEF FPC}{$IFDEF DEBUG}
      DebugLn('Y:' + IntToStr(Y));
      DebugLn('Row index:' + IntToStr(FMouseMoveData.RowIndex));
      {$ENDIF}{$ENDIF}
    end;

    if (FMouseMoveData.ClickOnCell) then
    begin
      CurrentPixelTop := timerulHeight + (FMouseMoveData.RowIndex * RowHeight);
      CurrentPixelBottom:= CurrentPixelTop + RowHeight - 1;
      if (abs (Y - CurrentPixelBottom) <= DELIMITER_CLICKING_AREA) then
      begin
        FMouseMoveData.ClickOnCellDelimiter:= true;
        FMouseMoveData.Distance:= Y - CurrentPixelBottom;
        FMouseMoveData.Origin := CurrentPixelBottom;
        {$IFDEF FPC}{$IFDEF DEBUG}
        DebugLn('SaveMouseMoveData - Distance [REDUCE]:' + IntToStr(FMouseMoveData.Distance));
        {$ENDIF}{$ENDIF}
      end
      else
      if ((FMouseMoveData.RowIndex <> 0) or (TopRow > 0)) and (abs (Y - CurrentPixelTop) <= DELIMITER_CLICKING_AREA) then
      begin
        FMouseMoveData.ClickOnCellDelimiter:= true;
        FMouseMoveData.Distance:= Y - CurrentPixelTop;
        FMouseMoveData.Origin:= CurrentPixelTop;
        {$IFDEF FPC}{$IFDEF DEBUG}
        DebugLn('SaveMouseMoveData - Distance [INCREASE]:' + IntToStr(FMouseMoveData.Distance));
        {$ENDIF}{$ENDIF}
      end;

    end;
  end;
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
begin
  if FResizingRows and ({$ifdef windows}GetAsyncKeyState{$else}GetKeyState{$endif}(VK_LBUTTON) and $8000 <> 0) then
  begin
    if (FMouseMoveData.OriginalRowHeight = 0) then
      FMouseMoveData.OriginalRowHeight := RowHeight;
    if (FMouseMoveData.CalculatedIncrement = 0) then
    begin
      FMouseMoveData.CalculatedIncrement:= 1 / (FMouseMoveData.RowIndex + 1);
      {$IFDEF FPC}{$IFDEF DEBUG}
      DebugLn('Calculated increment:' + FloatToStr(FMouseMoveData.CalculatedIncrement));
      {$ENDIF}{$ENDIF}
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
