// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mCalendar;

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses
  Classes, Controls, Graphics,
  mMaps, mCalendarGUIClasses, mIntList;

type

  TmCalendarItemType = (itMonth);

  TOnClickOnDay  = procedure (const Button: TMouseButton; const Shift: TShiftState; const aDay : TDateTime);


  { TmCalendar }

  TmCalendar = class (TCustomControl)
  strict private
    FCols : integer;
    FRows : integer;
    FItemType : TmCalendarItemType;
    //FBorderSize : integer;
    FCaptionSize : integer;
    FTitleSize : integer;
    FStartDate : TDateTime;
    FDayAlignment: TAlignment;
    FWeekdaysColor: TColor;
    FDaysColor : TColor;
    FSelectedDaysColor : TColor;
    FSelectedColor : TColor;
    FCaptionsColor : TColor;
    FLinesColor : TColor;

    // internal properties
    FItemWidth : integer;
    FItemHeight : integer;
    FDayWidth : integer;
    FDayHeight : integer;
    FMustCalculate : boolean;
    FDoubleBufferedBitmap: Graphics.TBitmap;
    FSelectedBucketsDictionary : TmIntegerDictionary;
    FSelectedBuckets: TIntegerList;
    FMouseMoveData : TmCalendarMouseMoveData;

    // events
    FOnClickOnDay : TOnClickOnDay;

    procedure Paint_Box(ACanvas: TCanvas; const ARect: TRect; const AText: string; const ATextAlignment: TAlignment);

    procedure GetMonthCaptionRect(out aRect : TRect; const aRow, aCol : integer);
    procedure GetMonthWeekDaysTitleRect(out aRect : TRect; const aRow, aCol : integer);

    procedure Paint_FillBackground(aCanvas : TCanvas; const aRect : TRect);
    procedure Paint_Items(aCanvas : TCanvas);
    procedure Paint_Month_Caption (aCanvas: TCanvas; aRow, aCol : integer);
    procedure Paint_Month_Weekdays (aCanvas: TCanvas; aRow, aCol : integer);
    procedure Paint_Month_Days(aCanvas: TCanvas; aRow, aCol : integer);
    function GetItemRefDate (aRow, aCol : integer) : TDateTime;
    procedure DoInternalSizeCalculation;
//    procedure SetBorderSize(AValue: integer);
    procedure SetCaptionsColor(AValue: TColor);
    procedure SetDayAlignment(AValue: TAlignment);
    procedure SetDaysColor(AValue: TColor);
    procedure SetRows(AValue: integer);
    procedure SetItemType(AValue: TmCalendarItemType);
    procedure SetLinesColor(AValue: TColor);
    procedure SetSelectedColor(AValue: TColor);
    procedure SetSelectedDaysColor(AValue: TColor);
    procedure SetWeekdaysColor(AValue: TColor);
    procedure SetCols(AValue: integer);

    procedure InternalSelectDay(const aDay: integer);
    procedure InternalUnselectDay(const aDay: integer);
    procedure InternalClearSelection;

    procedure DoPaintTo(aCanvas: TCanvas; aRect: TRect);
    procedure SaveMouseMoveData(X, Y: integer);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$ifdef linux}
    // explanation here: https://forum.lazarus.freepascal.org/index.php?topic=38041.0
    procedure SetBounds(aLeft, aTop, aWidth, aHeight: integer); override;
    {$endif}

    property Cols : integer read FCols write SetCols default 1;
    property Rows : integer read FRows write SetRows default 1;
    property ItemType : TmCalendarItemType read FItemType write SetItemType;
    property DayAlignment : TAlignment read FDayAlignment write SetDayAlignment;
    // colors
    property WeekdaysColor : TColor read FWeekdaysColor write SetWeekdaysColor;
    property DaysColor : TColor read FDaysColor write SetDaysColor;
    property CaptionsColor : TColor read FCaptionsColor write SetCaptionsColor;
    property LinesColor : TColor read FLinesColor write SetLinesColor;
    property SelectedColor : TColor read FSelectedColor write SetSelectedColor;
    property SelectedDaysColor : TColor read FSelectedDaysColor write SetSelectedDaysColor;

    property OnClickOnDay : TOnClickOnDay read FOnClickOnDay write FOnClickOnDay;
  end;

implementation

uses
  SysUtils, DateUtils, Math, Forms

  {$ifdef fpc}
  ,LCLIntf
  ,LclType
  ,LclProc
  ,LResources
  ,LMessages
  {$ifdef debug}, LazLogger
  {$endif}
  {$else}
  {$ifdef windows}, Windows{$endif}
  {$endif}
  , mGraphicsUtility, mUtility;


{ TmCalendar }

procedure TmCalendar.Paint_Box(ACanvas: TCanvas; const ARect: TRect; const AText: string; const ATextAlignment: TAlignment);
  procedure DrawBox;
  begin
    ACanvas.Brush.Style:= bsSolid;
    ACanvas.FillRect(aRect);
    ACanvas.Pen.Color:= DarkerColor(ACanvas.Brush.Color, 20);
    ACanvas.Line(aRect.Left, aRect.Bottom, aRect.Right, aRect.Bottom);
    ACanvas.Line(aRect.Left, aRect.Bottom, aRect.Left, aRect.Top);
    ACanvas.Line(aRect.Right, aRect.Bottom, aRect.Right, aRect.Top);
    aCanvas.Line(aRect.Left, aRect.Top, aRect.Right, aRect.Top);
  end;
var
  BoxRect : TRect;
begin
  DrawBox;
  if AText <> '' then
  begin
    BoxRect := aRect;
    InflateRect(BoxRect, -2, -2);
    WriteText(ACanvas, BoxRect, AText, ATextAlignment);
  end;
end;

procedure TmCalendar.GetMonthCaptionRect(out aRect: TRect; const aRow, aCol: integer);
begin
  aRect := Classes.Rect (aCol * FItemWidth, aRow * FItemHeight, (aCol + 1) * FItemWidth, aRow * FItemHeight + FCaptionSize);
end;

procedure TmCalendar.GetMonthWeekDaysTitleRect(out aRect: TRect; const aRow, aCol: integer);
begin
  GetMonthCaptionRect(aRect, aRow, aCol);
  aRect.Top:= aRect.Bottom;
  aRect.Bottom := aRect.Bottom + FTitleSize;
end;

procedure TmCalendar.Paint_FillBackground(aCanvas : TCanvas; const aRect : TRect);
begin
  aCanvas.Brush.Color:= Self.Color;
  aCanvas.Pen.Mode := pmCopy;
  aCanvas.Brush.Style := bsSolid;

  aCanvas.FillRect(aRect);
end;

procedure TmCalendar.Paint_Items(aCanvas : TCanvas);
var
  tmpRow, tmpCol : integer;
begin
  for tmpRow := 0 to FRows - 1 do
  begin
    for tmpCol := 0 to FCols - 1 do
    begin
      if FItemType = itMonth then
      begin
        Paint_Month_Caption (aCanvas, tmpRow, tmpCol);
        Paint_Month_Weekdays (aCanvas, tmpRow, tmpCol);
        Paint_Month_Days (aCanvas, tmpRow, tmpCol);
      end;
    end;
  end;
end;

procedure TmCalendar.Paint_Month_Caption(aCanvas: TCanvas; aRow, aCol: integer);
var
  tmpRect : TRect;
  str : String;
  tmpRefDate : TDateTime;
  tmpYear, tmpMonth, tmpDay : word;
begin
  GetMonthCaptionRect(tmpRect, aRow, aCol);
  tmpRefDate:= GetItemRefDate(aRow, aCol);
  DecodeDate(tmpRefDate, tmpYear, tmpMonth, tmpDay);
  tmpRefDate:= GetItemRefDate(aRow, aCol);
  DecodeDate(tmpRefDate, tmpYear, tmpMonth, tmpDay);
  str := Uppercase(FormatSettings.LongMonthNames[tmpMonth] + ' ' + IntToStr(tmpYear));
  aCanvas.Brush.Color:= Self.Color;
  aCanvas.Font := Self.Font;
  aCanvas.Font.Color := FCaptionsColor;
  Paint_Box(aCanvas, tmpRect, str, taCenter);
end;

procedure TmCalendar.Paint_Month_Weekdays(aCanvas: TCanvas; aRow, aCol: integer);
var
  x1 , y1 , x2 , y2 , i : integer;
  r : TRect;
begin
  aCanvas.Font := Self.Font;
  aCanvas.Font.Color := FWeekdaysColor;
  aCanvas.Brush.Style := bsClear;
  for i := 0 to 6 do
  begin
    x1 := i * FDayWidth + aCol * FItemWidth;
    y1 := aRow * FItemHeight + FCaptionSize;
    x2 := x1 + FDayWidth;
    y2 := y1 + FTitleSize;
    r := Classes.Rect (x1, y1, x2, y2);
    if r.Width < 30 then
      WriteText(aCanvas, r, LeftStr(Uppercase(FormatSettings.ShortDayNames [((i + 1)mod 7)+1]),1), FDayAlignment)
    else
      WriteText(aCanvas, r, Uppercase(FormatSettings.ShortDayNames [((i + 1)mod 7)+1]), FDayAlignment);
  end;

  aCanvas.Pen.Color := FLinesColor;
  aCanvas.Pen.Style := psSolid;
  aCanvas.Pen.Width := 0;
  GetMonthWeekDaysTitleRect(r, aRow, aCol);
  aCanvas.Line(r.Left, r.Bottom, r.Right, r.Bottom);
  aCanvas.Line(r.Left, r.Top, r.Left, r.Bottom);
  aCanvas.Line(r.Right, r.Top, r.Right, r.Bottom);
end;

procedure TmCalendar.Paint_Month_Days(aCanvas: TCanvas; aRow, aCol: integer);
var
  i, k : integer;
  x1 , y1 , x2 , y2 : integer;
  r : TRect;
  tmpYear, tmpMonth, tmpDay : word;
  curDate : TDateTime;
begin
  aCanvas.Font := Self.Font;
  aCanvas.Font.Color := FDaysColor;
  if FItemType = itMonth then
  begin
    curDate := GetItemRefDate(aRow, aCol);
    DecodeDate(curDate, tmpYear, tmpMonth, tmpDay);
    for k := 0 to 5 do
    begin
      y1 := (aRow * FItemHeight) + FCaptionSize + FTitleSize + (k * FDayHeight);
      y2 := y1 + FDayHeight;
      for i := 0 to 6 do
      begin
        x1 := (i * FDayWidth) + (aCol * FItemWidth);
        x2 := x1 + FDayWidth;
        r := Classes.Rect (x1, y1, x2, y2);
        if DayOfTheWeek(curDate) = i + 1 then
        begin
          if FSelectedBucketsDictionary.Contains(round(curDate)) then
          begin
            aCanvas.Font.Color:= FSelectedDaysColor;
            aCanvas.Brush.Color:= FSelectedColor;
          end
          else
          begin
            aCanvas.Font.Color := FDaysColor;
            aCanvas.Brush.Color:= Self.Color;
          end;
          Paint_Box(aCanvas, r, IntToStr(tmpDay), FDayAlignment);

          curDate := curDate + 1;
          inc(tmpDay);
        end;
        if MonthOf(curDate) <> tmpMonth then
          break;
      end;
    end;
  end;

end;

function TmCalendar.GetItemRefDate(aRow, aCol: integer): TDateTime;
var
  tmpDay, tmpMonth, tmpYear : word;
  distance: integer;
begin
  if (FItemType = itMonth) then
  begin
    DecodeDate(FStartDate, tmpYear, tmpMonth, tmpDay);
    distance := (aRow * FCols) + aCol;
    Result := StartOfTheMonth(FStartDate);
    if distance > 0 then
      Result := IncMonth(Result, distance);
  end;
end;


destructor TmCalendar.Destroy;
begin
  FDoubleBufferedBitmap.Free;
  FSelectedBucketsDictionary.Free;
  FSelectedBuckets.Free;
  FMouseMoveData.Free;
  inherited Destroy;
end;

{$ifdef linux}
procedure TmCalendar.SetBounds(aLeft, aTop, aWidth, aHeight: integer);
begin
  inherited SetBounds(aLeft, aTop, aWidth, aHeight);
  Invalidate;
end;
{$endif}

procedure TmCalendar.DoInternalSizeCalculation;
var
  tmpX, tmpY : integer;
begin
  if not Self.HandleAllocated then
    exit;

  FMustCalculate:= false;
  if FItemType = itMonth then
  begin
    FDayWidth:= (Self.ClientWidth) div (7 * FCols);
    FDayHeight:= (Self.ClientHeight - (FCaptionSize + FTitleSize) * FRows) div (6 * FRows);
  end;

  tmpX := ((FDayWidth * 7)) * FCols;
  tmpY := ((FDayHeight * 6) + FCaptionSize + FTitleSize) * FRows;

  FItemWidth:= tmpX div FCols;
  FItemHeight:= tmpY div FRows;

  // Self.SetBounds(Self.Left, Self.Top, (Self.Width - Self.ClientWidth) + tmpX, (Self.Height - Self.ClientHeight) + tmpY);
end;

(*
procedure TmCalendar.SetBorderSize(AValue: integer);
begin
  if FBorderSize=AValue then Exit;
  if AValue > 0 then
  begin
    FBorderSize:=AValue;
    DoInternalSizeCalculation;
    Invalidate;
  end;
end;
*)

procedure TmCalendar.SetCaptionsColor(AValue: TColor);
begin
  if FCaptionsColor=AValue then Exit;
  FCaptionsColor:=AValue;
  Invalidate;
end;

procedure TmCalendar.SetDayAlignment(AValue: TAlignment);
begin
  if FDayAlignment=AValue then Exit;
  FDayAlignment:=AValue;
  Invalidate;
end;

procedure TmCalendar.SetDaysColor(AValue: TColor);
begin
  if FDaysColor=AValue then Exit;
  FDaysColor:=AValue;
  Invalidate;
end;

procedure TmCalendar.SetRows(AValue: integer);
begin
  if FRows=AValue then Exit;
  if AValue >= 1 then
  begin
    FRows:=AValue;
    DoInternalSizeCalculation;
    Invalidate;
  end;
end;

procedure TmCalendar.SetItemType(AValue: TmCalendarItemType);
begin
  if FItemType=AValue then Exit;
  FItemType:=AValue;
  DoInternalSizeCalculation;
  Invalidate;
end;

procedure TmCalendar.SetLinesColor(AValue: TColor);
begin
  if FLinesColor=AValue then Exit;
  FLinesColor:=AValue;
  Invalidate;
end;

procedure TmCalendar.SetSelectedColor(AValue: TColor);
begin
  if FSelectedColor=AValue then Exit;
  FSelectedColor:=AValue;
  Invalidate;
end;

procedure TmCalendar.SetSelectedDaysColor(AValue: TColor);
begin
  if FSelectedDaysColor=AValue then Exit;
  FSelectedDaysColor:=AValue;
  Invalidate;
end;

procedure TmCalendar.SetWeekdaysColor(AValue: TColor);
begin
  if FWeekdaysColor=AValue then Exit;
  FWeekdaysColor:=AValue;
  Invalidate;
end;

procedure TmCalendar.SetCols(AValue: integer);
begin
  if FCols=AValue then Exit;
  if AValue >= 1 then
  begin
    FCols:=AValue;
    DoInternalSizeCalculation;
    Invalidate;
  end;
end;

procedure TmCalendar.InternalSelectDay(const aDay: integer);
begin
  FSelectedBucketsDictionary.Add(aDay, FSelectedBucketsDictionary);
  FSelectedBuckets.Add(aDay);
end;

procedure TmCalendar.InternalUnselectDay(const aDay: integer);
begin
  FSelectedBucketsDictionary.Remove(aDay);
  FSelectedBuckets.Remove(aDay);
end;

procedure TmCalendar.InternalClearSelection;
begin
  FSelectedBucketsDictionary.Clear;
  FSelectedBuckets.Clear;
end;

procedure TmCalendar.DoPaintTo(aCanvas: TCanvas; aRect: TRect);
begin
  aCanvas.Lock;
  try
    Paint_FillBackground(aCanvas, aRect);
    Paint_Items(aCanvas);
  finally
    aCanvas.Unlock;
  end;
end;

procedure TmCalendar.SaveMouseMoveData(X, Y: integer);
var
  singleItemWidth, singleItemHeight : integer;
  row, col, monthRow, monthCol : integer;
  refDate, clickedDate : TDateTime;
  refYear, refMonth, refDay : word;
  day : integer;
begin
  FMouseMoveData.Clear;
  if not PtInRect(ClientRect, Classes.Point(X, Y)) then
    exit;

  singleItemWidth := ClientRect.Width div FCols;
  singleItemHeight := ClientRect.Height div FRows;

  col := (X - ClientRect.Left) div singleItemWidth;
  row := (Y - ClientRect.Top) div singleItemHeight;
  refDate := GetItemRefDate(row, col);

  if FItemType = itMonth then
  begin
    DecodeDate(refDate, refYear, refMonth, refDay);
    {$ifdef debug}
    debugln('Click - Item ref date: ' + DateToStr(refDate));
    {$ifdef linux}
    writeln('Click - Item ref date: ' + DateToStr(refDate));
    {$endif}
    {$endif}

    monthRow := (Y - 1 - ClientRect.Top - (row * FItemHeight) - FCaptionSize - FTitleSize) div FDayHeight;
    monthCol := (X - 2 -  ClientRect.Left - (col * FItemWidth)) div FDayWidth;

    day := monthCol + (monthRow * 7) - DayOfTheWeek(refDate) + 2;
    {$ifdef debug}
    debugln('Click - day: ' + IntToStr(day));
    {$ifdef linux}
    writeln('Row: ' + IntToStr(row));
    writeln('Col: ' + IntToStr(row));
    writeln('Click - day: ' + IntToStr(day));
    {$endif}
    {$endif}

    if (day >= 1) and (day <= DaysInAMonth(refYear, refMonth)) then
    begin
      clickedDate := EncodeDate(refYear, refMonth, day);
      FMouseMoveData.ClickOnDays:= true;
      FMouseMoveData.Day:= round(clickedDate);
      {$ifdef debug}
      debugln('Click - clicked date: ' + DateToStr(clickedDate));
      {$ifdef linux}
      writeln('Click - clicked date: ' + DateToStr(clickedDate));
      {$endif}
      {$endif}
    end;
  end;
end;

procedure TmCalendar.Paint;
var
  drawingRect : TRect;
  tmpCanvas : TCanvas;
begin
  drawingRect := ClientRect;

  if DoubleBuffered then
  begin
    tmpCanvas := FDoubleBufferedBitmap.Canvas;
    tmpCanvas.Font.Assign(Self.Font);
  end
  else
    tmpCanvas := Self.Canvas;

  DoInternalSizeCalculation;
  DoPaintTo(tmpCanvas, drawingRect);

  if DoubleBuffered then
  begin
    Brush.Style := bsClear;
    Self.Canvas.CopyRect(drawingRect, tmpCanvas, drawingRect);
  end;
end;

procedure TmCalendar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if (Button = mbLeft) then
    SaveMouseMoveData(X, Y);
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TmCalendar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  mustPaint : boolean;
  i, curDay : integer;
begin
  mustPaint := false;
  if FMouseMoveData.ClickOnDays then
  begin
    if Button = mbLeft then
    begin
      if CtrlPressed then
      begin
        if FSelectedBucketsDictionary.Contains(FMouseMoveData.Day) then
          InternalUnselectDay(FMouseMoveData.Day)
        else
          InternalSelectDay(FMouseMoveData.Day);
        mustPaint := true;
      end
      else if ShiftPressed then
      begin
        if FSelectedBuckets.Count = 0 then
          InternalSelectDay(FMouseMoveData.Day)
        else if FSelectedBuckets.Count >= 1 then
        begin
          i := FSelectedBuckets.Count;
          while i > 1 do
          begin
            InternalUnselectDay(FSelectedBuckets.Items[i - 1]);
            dec(i);
          end;
          curDay := FSelectedBuckets.Items[0];
          if curDay < FMouseMoveData.Day then
          begin
            {$ifdef debug}
            debugln('Select from ' + DateToStr(curDay + 1) + ' to ' + DateToStr(FMouseMoveData.Day));
            {$endif}
            for i := curDay + 1 to FMouseMoveData.Day do
              InternalSelectDay(i);;
          end
          else if curDay > FMouseMoveData.Day then
          begin
            {$ifdef debug}
            debugln('Select from ' + DateToStr(FMouseMoveData.Day + 1) + ' to ' + DateToStr(curDay));
            {$endif}
            for i := FMouseMoveData.Day + 1 to curDay do
              InternalSelectDay(i);
          end;
          mustPaint := true;
        end;
      end
      else
      begin
        InternalClearSelection;
        InternalSelectDay(FMouseMoveData.Day);
        mustPaint := true;
      end;
    end;
  end;
  inherited MouseUp(Button, Shift, X, Y);
  if mustPaint then
    Self.Paint;
  if FMouseMoveData.ClickOnDays and Assigned(FOnClickOnDay) then
    FOnClickOnDay(Button, Shift, FMouseMoveData.Day);
end;

procedure TmCalendar.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited MouseMove(Shift, X, Y);
end;

constructor TmCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDoubleBufferedBitmap := Graphics.TBitmap.Create;
  {$ifdef fpc}
  DoubleBuffered:= IsDoubleBufferedNeeded;
  {$endif}
  // 4096 * 2160 = 4K
  // 7680 * 4320 = 8K
  FDoubleBufferedBitmap.SetSize(max(Screen.Width,4096), max(Screen.Height,2160));
  FSelectedBucketsDictionary := TmIntegerDictionary.Create(false);
  FSelectedBuckets:= TIntegerList.Create;
  FMouseMoveData := TmCalendarMouseMoveData.Create;

  FMustCalculate := true;
  FCols:= 1;
  FRows:= 1;
  FItemType:= itMonth;
  FCaptionSize:= 16;
  FTitleSize:= 16;
  FStartDate:= Date;
  FDayAlignment:= taCenter;
  FWeekdaysColor:= clBlack;
  FDaysColor:= clBlack;
  FSelectedColor := clBlue;
  FSelectedDaysColor:= clWhite;
  FCaptionsColor := clBlack;
  Self.Color:= clWhite;
  FLinesColor:= clLtGray;

  FOnClickOnDay:= nil;
end;



end.
