// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mTimeruler;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Controls, Classes, Graphics,
  {$IFDEF WINDOWS}Windows,{$ENDIF} DateUtils, contnrs,

  {$IFDEF FPC}
  InterfaceBase,
  LCLIntf,
  LclType,
  LclProc,
  {$IFDEF DEBUG}LazLogger,{$ENDIF}
  {$ENDIF}

  mDateTimeUtility,

  mTimerulerScales, mTimerulerTimelines, mTimerulerDefs, mTimerulerEvents,
  mTimerulerGraphics,

  Dialogs, Messages;
type
  TmTimeruler = class;

  TOnDrawTimelineEvent = procedure(Sender: TmTimeruler; Timeline: TmTimeline; ARect: TRect; StartDate: TDateTime) of object;
  TOnDrawTimelineBucketEvent = procedure(Sender: TmTimeruler; Timeline: TmTimeline; ARect: TRect; ADate: TDateTime) of object;
  TOnDateChangingEvent = procedure(Sender: TmTimeruler; var NewDate: TDateTime) of object;

  TmTimerulerEventKind = (trLayoutChanged, trCurrentDateChanged);

  { TmTimeruler }
  TmTimeruler = class(TCustomControl)
  strict private
    FEventsSubscriptions: TObjectList;
    FCurrentDate: TDateTime;
    FMouseMoveData: _TmMouseMoveData;

    FTimelines : TmTimelines;
    FMainTimelineRef : TmTimeline;

    FOnDateChanged: TNotifyEvent;
    FOnBeforeDateChange: TOnDateChangingEvent;
    FOnLayoutChanged: TNotifyEvent;
    FOnDrawTimeline: TOnDrawTimelineEvent;
    FOnDrawBucket: TOnDrawTimelineBucketEvent;
    FOneBucketWidth: Integer;

    FResizingBuckets : boolean;

    procedure NotifySubscribers(EventKind: TmTimerulerEventKind; Info: TDateTime);
    procedure SetCurrentDate(Value: TDateTime);
    function CalculateTimelineHeight (Timeline : TmTimeline) : integer;

    procedure SetOneBucketWidth(AValue: Integer);
    procedure NotifyLayoutChanged(MustInvalidateTimebar : boolean);

    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;

    procedure PaintTimeline(ARect: TRect; StartDate: TDateTime; Timeline: TmTimeline);
    procedure SaveMouseMoveData(X, Y: integer);

  protected
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    function PixelsToDateTime(X: integer): TDateTime;
    function DateTimeToPixels(D: TDateTime): integer;
    procedure Rebuild;
    function SubscribeToEvents(SubscriberClass: TmTimerulerEventsSubscriptionClass) : TmTimerulerEventsSubscription;
    procedure UnsubscribeFromEvents(Subscription: TmTimerulerEventsSubscription);
    function AddTimeline (ScaleClass : TmScaleClass) : TmTimeline;
  public
    // events
    property OnBeforeDateChange: TOnDateChangingEvent read FOnBeforeDateChange write FOnBeforeDateChange;
    property OnDateChanged: TNotifyEvent read FOnDateChanged write FOnDateChanged;
    property OnLayoutChanged: TNotifyEvent read FOnLayoutChanged write FOnLayoutChanged;

    property OneBucketWidth: Integer read FOneBucketWidth write SetOneBucketWidth default 30;
    property CurrentDate: TDateTime read FCurrentDate write SetCurrentDate;
    property OnDrawTimeline: TOnDrawTimelineEvent read FOnDrawTimeline write FOnDrawTimeline;
    property OnDrawBucket: TOnDrawTimelineBucketEvent read FOnDrawBucket write FOnDrawBucket;
    property ResizingBuckets: boolean read FResizingBuckets;
  end;


implementation

uses Math, SysUtils;

const
  DELIMITER_CLICKING_AREA : integer = 4;

function TmTimeruler.SubscribeToEvents(SubscriberClass: TmTimerulerEventsSubscriptionClass) : TmTimerulerEventsSubscription;
var
  newSubscription : TmTimerulerEventsSubscription;
begin
  newSubscription := SubscriberClass.Create();
  FEventsSubscriptions.Add(newSubscription);
  Result := newSubscription;
end;


procedure TmTimeruler.WMMouseWheel(var Message: TWMMouseWheel);
var
  ScrollCount, ScrollLines: integer;
begin
  if Assigned(FMainTimelineRef) then
  begin
    SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @ScrollLines, 0);
    ScrollCount := -ScrollLines * Message.WheelDelta div WHEEL_DELTA;
    CurrentDate := FMainTimelineRef.Scale.AddTicks(Self.CurrentDate, ScrollCount);
  end;
end;

procedure TmTimeruler.UnsubscribeFromEvents(Subscription: TmTimerulerEventsSubscription);
var
  i : integer;
begin
  i := FEventsSubscriptions.IndexOf(Subscription);
  if (i >= 0) then
  begin
    FEventsSubscriptions.Delete(i);
  end;
end;

function TmTimeruler.AddTimeline(ScaleClass: TmScaleClass) : TmTimeline;
var
  newTimeline : TmTimeline;
begin
  newTimeline := FTimelines.Add;
  newTimeline.Scale := ScaleClass.Create;

  if Assigned(FMainTimelineRef) then
  begin
    if (FMainTimelineRef.Scale.isMajorThan(newTimeline.Scale)) then
      FMainTimelineRef := newTimeline;
  end
  else
  begin
    FMainTimelineRef := newTimeline;
  end;
  Result := newTimeline;
end;

procedure TmTimeruler.SetOneBucketWidth(AValue: Integer);
begin
  if FOneBucketWidth = AValue then
    Exit;
  FOneBucketWidth := AValue;
  if not (csDestroying in ComponentState) then
     Self.Rebuild;
end;

procedure TmTimeruler.Rebuild;
begin
  Self.Invalidate;
  NotifyLayoutChanged(true);
end;

procedure TmTimeruler.NotifySubscribers(EventKind: TmTimerulerEventKind; Info: TDateTime);
var
  f : integer;
begin
  for f := 0 to FEventsSubscriptions.Count - 1 do
  begin
    case EventKind of
      trLayoutChanged:
        (FEventsSubscriptions.Items[f] as TmTimerulerEventsSubscription).LayoutChanged;
      trCurrentDateChanged:
        (FEventsSubscriptions.Items[f] as TmTimerulerEventsSubscription).DateChanged(Info);
    end;
  end;
end;

procedure TmTimeruler.Assign(Source: TPersistent);
var
  CopyFrom: TmTimeruler;
begin
  if (Source is TmTimeruler) then
  begin
    CopyFrom := Source as TmTimeruler;
    FTimelines := CopyFrom.FTimelines;
    FMainTimelineRef := CopyFrom.FMainTimelineRef;
    Font.Assign(CopyFrom.Font);
    Color := CopyFrom.Color;
    Self.Rebuild;
    if Showing then
      invalidate;
    NotifyLayoutChanged(false);
  end;
end;

procedure TmTimeruler.SaveMouseMoveData(X, Y: integer);
var
  NextDate: TDateTime;
  idxTimeline, dummy, tempHeight : integer;
  CurrentScale : TmScale;
  CurrentPixelLeft, CurrentPixelRight : integer;
begin
  FMouseMoveData.Clear;;
  if not PtInRect(ClientRect, Classes.Point(X, Y)) then
    exit;
  if FTimelines.Count > 0 then
  begin
    dummy := 0;
    idxTimeline := 0;

    while (FMouseMoveData.Timeline = nil) and (idxTimeline < FTimelines.Count) do
    begin
      tempHeight:= CalculateTimelineHeight(FTimelines[idxTimeline]);

      if (Y >= dummy) and (Y < dummy + tempHeight) then
      begin
        FMouseMoveData.ClickOnTimelines := true;
        FMouseMoveData.Timeline := FTimelines[idxTimeline];
      end
      else
      begin
        dummy := dummy + tempHeight;
        Inc(idxTimeline);
      end;
    end;

    if (FMouseMoveData.ClickOnTimelines) and Assigned(FMouseMoveData.Timeline) then
    begin
      CurrentScale := FMouseMoveData.Timeline.Scale;
      NextDate := CurrentScale.TruncDate(Self.CurrentDate);
      CurrentPixelLeft := DateTimeToPixels(NextDate);

      while (CurrentPixelLeft < Self.Width) and (not FMouseMoveData.ClickOnBucket) and (not FMouseMoveData.ClickOnBucketDelimiter) do
      begin
        NextDate := CurrentScale.AddTicks(NextDate, 1);
        CurrentPixelRight := DateTimeToPixels(NextDate);

        if (X >= CurrentPixelLeft + DELIMITER_CLICKING_AREA) and (X < CurrentPixelRight - DELIMITER_CLICKING_AREA) then
        begin
          FMouseMoveData.ClickOnBucket := true;
          FMouseMoveData.DistanceInTicks := FMainTimelineRef.Scale.TicksBetween(Self.CurrentDate, PixelsToDateTime(CurrentPixelRight));
        end
        else if (X >= CurrentPixelRight - DELIMITER_CLICKING_AREA) and (X < CurrentPixelRight + DELIMITER_CLICKING_AREA) then
        begin
          FMouseMoveData.ClickOnBucketDelimiter := true;
          FMouseMoveData.DistanceInTicks := max(1, FMainTimelineRef.Scale.TicksBetween(Self.CurrentDate, PixelsToDateTime(CurrentPixelRight)));
          //DebugLn(FloatToStr(TempDate) + ' ' + FloatToStr(Self.CurrentDate) + ' ' + FloatToStr(TempDate2));
          FMouseMoveData.Distance := DateTimeToPixels(CurrentScale.AddTicks(CurrentScale.TruncDate(Self.CurrentDate) , 1)) - DateTimeToPixels(Self.CurrentDate);
        end
        else
        begin
          CurrentPixelLeft := CurrentPixelRight;
        end;
      end;
    end;
  end;
end;

function TmTimeruler.DateTimeToPixels(D: TDateTime): integer;
begin
  Result := FMainTimelineRef.Scale.intervalToPixels(FCurrentDate, D, OneBucketWidth);
end;


constructor TmTimeruler.Create(AOwner: TComponent);
begin
  inherited;
  Align := alTop;

  FEventsSubscriptions := TObjectList.Create(true);

  Height := 40;

  FOneBucketWidth := 30;
  FTimelines := TmTimelines.Create();
  DoubleBuffered := True;
  FResizingBuckets := false;
  FMainTimelineRef:= nil;

  FMouseMoveData := _TmMouseMoveData.Create;
  Color := clBtnFace;

  FCurrentDate := Floor(Now);
end;

destructor TmTimeruler.Destroy;
begin
  FEventsSubscriptions.Destroy;
  FTimelines.Free;
  FMouseMoveData.Free;
  inherited;
end;



procedure TmTimeruler.NotifyLayoutChanged(MustInvalidateTimebar : boolean);
begin
  NotifySubscribers(trLayoutChanged, 0);
  if Assigned(FOnLayoutChanged) then
    FOnLayoutChanged(Self);
  if MustInvalidateTimebar then
    Self.Invalidate();
end;


procedure TmTimeruler.MouseMove(Shift: TShiftState; X, Y: integer);
var
  fattore : Double;
begin
  if FResizingBuckets and (GetAsyncKeyState(VK_LBUTTON) and $8000 <> 0) then
  begin
    if (FMouseMoveData.LastCalculatedOneBucketWidth = 0) then
      FMouseMoveData.LastCalculatedOneBucketWidth := OneBucketWidth;
    //DebugLn('x:' + IntToStr(X) + ' bucket:' + IntToStr(FMouseMoveData.DistanceInTicks));
    if (FMouseMoveData.Distance < FMouseMoveData.LastCalculatedOneBucketWidth) then
    begin
      fattore := 1 / FMouseMoveData.LastCalculatedOneBucketWidth;
      FMouseMoveData.LastCalculatedOneBucketWidth := max(5,FMouseMoveData.LastCalculatedOneBucketWidth + (X - ((FMouseMoveData.DistanceInTicks - 1) * OneBucketWidth) - FMouseMoveData.Distance));
      OneBucketWidth := round(FMouseMoveData.LastCalculatedOneBucketWidth);
      FMouseMoveData.Distance := (FMouseMoveData.LastCalculatedOneBucketWidth * fattore) * FMouseMoveData.Distance;
    end
    else
    begin
      FMouseMoveData.LastCalculatedOneBucketWidth := max(5, X/ FMouseMoveData.DistanceInTicks );
      OneBucketWidth := round(FMouseMoveData.LastCalculatedOneBucketWidth);
      FMouseMoveData.Distance := FMouseMoveData.LastCalculatedOneBucketWidth;
    end;
    //DebugLn('dopo OneBucketWidth:' + IntToStr(FTimeScalesHeader.OneBucketWidth));
  end
  else
  begin
    SaveMouseMoveData(X, Y);
    if FMouseMoveData.ClickOnBucketDelimiter then
      Cursor := crSizeWE
    else
      Cursor := crDefault;
  end;
  inherited;
end;

procedure TmTimeruler.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if FResizingBuckets then
  begin
    FResizingBuckets := false;
    NotifyLayoutChanged(false);
  end;
  Self.Cursor:= crDefault;
end;

procedure TmTimeruler.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if (Button = mbLeft) then
  begin
    SaveMouseMoveData(X, Y);
    if FMouseMoveData.ClickOnBucketDelimiter then
    begin
      FMouseMoveData.LastCalculatedOneBucketWidth := 0;
      MouseCapture;
      FResizingBuckets := true;
    end;
  end;
  inherited;
end;


procedure TmTimeruler.Paint;
var
  CurrentTimelineHeight: integer;
  StartDate: TDateTime;
  CurrentRect: TRect;
  i, p : integer;
  CurrentTimeline: TmTimeline;
begin
  if (FTimelines.Count > 0) and Assigned(FMainTimelineRef) then
  begin
    p := 0;
    for i := 0 to FTimelines.Count - 1 do
    begin
      CurrentTimeline := FTimelines[i];
      CurrentTimelineHeight := CalculateTimelineHeight(CurrentTimeline);

      if (P < Canvas.ClipRect.Bottom) and (P + CurrentTimelineHeight > Canvas.ClipRect.Top) then
      begin
        StartDate := CurrentTimeline.Scale.TruncDate(PixelsToDateTime(Canvas.ClipRect.Left));
        SetRect(CurrentRect, DateTimeToPixels(StartDate), P, Canvas.ClipRect.Right, P + CurrentTimelineHeight);
        if Assigned(FOnDrawTimeline) and CurrentTimeline.OwnerDraw then
          FOnDrawTimeline(Self, CurrentTimeline, CurrentRect, StartDate)
        else
          PaintTimeline(CurrentRect, StartDate, CurrentTimeline);
      end;
      P := P + CurrentTimelineHeight;
    end;
  end;
end;

procedure TmTimeruler.PaintTimeline(ARect: TRect; StartDate: TDateTime; Timeline: TmTimeline);
var
  DummyRect: TRect;
  DummyDate: TDateTime;
  EndPos: integer;
begin
  if Timeline.ParentColor then
    Canvas.Brush.Color := Color
  else
    Canvas.Brush.Color := Timeline.Color;

  if Timeline.ParentFont then
    Canvas.Font := Font
  else
    Canvas.Font := Timeline.Font;

  EndPos := ARect.Right;
  ARect.Left := max(ARect.Left, 0);

  while ARect.Left < EndPos do
  begin
    DummyDate := Timeline.Scale.RoundDate(StartDate);
    StartDate := Timeline.Scale.NextBucket(StartDate);

    ARect.Right := DateTimeToPixels(StartDate);
    if ARect.Left = ARect.Right then
      continue;

    DummyRect := ARect;

    DummyRect.Left := max(ARect.Left, 0);
    DummyRect.Right := min(Width, ARect.Right);

    if Assigned(FOnDrawBucket) and Timeline.OwnerDraw then
      FOnDrawBucket(Self, Timeline, ARect, DummyDate)
    else
      DrawBucketBox(Canvas, DummyRect, ExtFormatDateTime(Timeline.Scale.DisplayFormat, DummyDate), Timeline.Alignment);

    ARect.Left := ARect.Right;
  end;
end;

procedure TmTimeruler.SetCurrentDate(Value: TDateTime);
var
  Saved: TDateTime;
begin
  Value := FMainTimelineRef.Scale.TruncDate(Value);
  if FCurrentDate <> Value then
  begin
    Saved := Value;

    if Assigned(FOnBeforeDateChange) then
      FOnBeforeDateChange(Self, Value);

    if Saved <> Value then
    begin
      Value := FMainTimelineRef.Scale.TruncDate(Value);
      if FCurrentDate = Value then
        Exit;
    end;
    Saved := FCurrentDate;
    FCurrentDate := Value;
    Invalidate;
    NotifySubscribers(trCurrentDateChanged, Saved);

    if Assigned(FOnDateChanged) then
      FOnDateChanged(Self);
  end;
end;

function TmTimeruler.CalculateTimelineHeight(Timeline: TmTimeline): integer;
begin
  Result := trunc((Height / FTimelines.GetFlexTotal) * Timeline.Flex);
end;


function TmTimeruler.PixelsToDateTime(X: integer): TDateTime;
begin
  Result := FMainTimelineRef.Scale.pixelsToDateTime(X, FCurrentDate, OneBucketWidth);
end;

end.
