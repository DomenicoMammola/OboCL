// This is part of the Obo Component Library

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

// This software is distributed without any warranty.

// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mCalendar;

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses
  Classes, Controls, Graphics,
  mMaps, mCalendarGUIClasses, mCalendarClasses, mIntList;

type

  TmCalendarItemType = (itMonth);
  TmCalendarStyle = (csSimple, csAppointmentsList);
  TmCalendarOption = (coHideSunday, coHideSaturdayAndSunday, coSundayIsHoliday, coSaturdayIsHoliday);
  TmCalendarOptions = set of TmCalendarOption;

  TOnClickOnDay = procedure(const Button: TMouseButton; const Shift: TShiftState; const aDay: TDateTime) of object;
  TOnGetAppointments = procedure(const aDate: TDate; const aAppointments: tmCalendarAppointments) of object;
  TOnCheckIsHoliday = function(const aDate: TDate): boolean of object;
  TOnGetAppointmentHint = function(const aAppointment: TmCalendarAppointment): string of object;
  TOnGetDateHint = function(const aDate: TDate): string of object;


  { TmCalendar }

  TmCalendar = class(TCustomControl)
  strict private
  const
    MIN_DAY_HEADER_HEIGHT = 1;
  const
    APPOINTMENT_MARGIN = 2;
  strict private
    FCols: integer;
    FRows: integer;
    FItemType: TmCalendarItemType;
    //FBorderSize : integer;
    FCaptionSize: integer;

    FTitleSize: integer;
    FStartDate: TDateTime;
    FDayAlignment: TAlignment;
    FWeekdaysColor: TColor;
    FDaysColor: TColor;
    FHolidaysColor: TColor;
    FSelectedTextColor: TColor;
    FSelectedColor: TColor;
    FCaptionsColor: TColor;
    FLinesColor: TColor;
    FStyle: TmCalendarStyle;
    FOptions: TmCalendarOptions;
    FMinAppointmentHeight: integer;
    FMaxAppointmentHeight: integer;

    // internal properties
    FItemWidth: integer;
    FItemHeight: integer;
    FDayWidth: integer;
    FDayHeight: integer;
    FMustCalculate: boolean;
    FDoubleBufferedBitmap: Graphics.TBitmap;
    FSelectedBucketsDictionary: TmIntegerDictionary;
    FSelectedBuckets: TIntegerList;
    FSelectedAppointmentsDictionary: TmStringDictionary;
    FSelectedAppointments: TStringList;
    FMouseMoveData: TmCalendarMouseMoveData;
    FAppointmentsPerDay: TmIntegerDictionary;

    // events
    FOnClickOnDay: TOnClickOnDay;
    FOnGetAppointments: TOnGetAppointments;
    FOnCheckIsHoliday: TOnCheckIsHoliday;
    FOnGetAppointmentHint: TOnGetAppointmentHint;
    FOnGetDateHint: TOnGetDateHint;

    procedure Paint_BoxWithText(ACanvas: TCanvas; const ARect: TRect; const AText: string; const ATextAlignment: TAlignment; const aSelected: boolean); overload;
    procedure Paint_BoxWithText(ACanvas: TCanvas; const ARect: TRect; const ARows: TStringList; const ATextAlignment: TAlignment; const aSelected: boolean); overload;
    procedure Paint_Box(ACanvas: TCanvas; const ARect: TRect; const aSelected: boolean);
    procedure Paint_Appointments(aCanvas: TCanvas; const aRect: TRect; const aAppointments: TmCalendarAppointments);

    procedure GetMonthCaptionRect(out aRect: TRect; const aRow, aCol: integer);
    procedure GetMonthWeekDaysTitleRect(out aRect: TRect; const aRow, aCol: integer);

    procedure Paint_FillBackground(aCanvas: TCanvas; const aRect: TRect);
    procedure Paint_Items(aCanvas: TCanvas);
    procedure Paint_Month_Caption(aCanvas: TCanvas; aRow, aCol: integer);
    procedure Paint_Month_Weekdays(aCanvas: TCanvas; aRow, aCol: integer);
    procedure Paint_Month_Days(aCanvas: TCanvas; aRow, aCol: integer);
    function GetItemRefDate(aRow, aCol: integer): TDateTime;
    procedure DoInternalSizeCalculation;
    //    procedure SetBorderSize(AValue: integer);
    procedure SetCaptionsColor(AValue: TColor);
    procedure SetCaptionSize(AValue: integer);
    procedure SetDayAlignment(AValue: TAlignment);
    procedure SetDaysColor(AValue: TColor);
    procedure SetHolidaysColor(AValue: TColor);
    procedure SetMaxAppointmentHeight(AValue: integer);
    procedure SetMinAppointmentHeight(AValue: integer);
    procedure SetOptions(AValue: TmCalendarOptions);
    procedure SetRows(AValue: integer);
    procedure SetItemType(AValue: TmCalendarItemType);
    procedure SetLinesColor(AValue: TColor);
    procedure SetSelectedColor(AValue: TColor);
    procedure SetSelectedTextColor(AValue: TColor);
    procedure SetStartDate(AValue: TDateTime);
    procedure SetStyle(AValue: TmCalendarStyle);
    procedure SetWeekdaysColor(AValue: TColor);
    procedure SetCols(AValue: integer);

    procedure InternalSelectDay(const aDay: integer);
    procedure InternalUnselectDay(const aDay: integer);
    procedure InternalClearDaysSelection;
    procedure InternalClearAppointmentsSelection;

    procedure InternalSelectAppointment(const aAppointmentUniqueId: string);
    procedure InternalUnselectAppointment(const aAppointmentUniqueId: string);

    procedure DoPaintTo(aCanvas: TCanvas; aRect: TRect);
    procedure SaveMouseMoveData(X, Y: integer);
    procedure HideHint;
    procedure ShowHintWindow(const aText: string; const aPoint: TPoint);
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

    property Cols: integer read FCols write SetCols;
    property Rows: integer read FRows write SetRows;
    property ItemType: TmCalendarItemType read FItemType write SetItemType;
    property DayAlignment: TAlignment read FDayAlignment write SetDayAlignment;
    property StartDate: TDateTime read FStartDate write SetStartDate;
    property Style: TmCalendarStyle read FStyle write SetStyle;
    property CaptionSize: integer read FCaptionSize write SetCaptionSize;
    property Options: TmCalendarOptions read FOptions write SetOptions;
    property MinAppointmentHeight: integer read FMinAppointmentHeight write SetMinAppointmentHeight;
    property MaxAppointmentHeight: integer read FMaxAppointmentHeight write SetMaxAppointmentHeight;

    // colors
    property WeekdaysColor: TColor read FWeekdaysColor write SetWeekdaysColor;
    property DaysColor: TColor read FDaysColor write SetDaysColor;
    property HolidaysColor: TColor read FHolidaysColor write SetHolidaysColor;
    property CaptionsColor: TColor read FCaptionsColor write SetCaptionsColor;
    property LinesColor: TColor read FLinesColor write SetLinesColor;
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor;
    property SelectedTextColor: TColor read FSelectedTextColor write SetSelectedTextColor;

    property SelectedBuckets: TIntegerList read FSelectedBuckets;
    property SelectedAppointments: TStringList read FSelectedAppointments;

    property OnClickOnDay: TOnClickOnDay read FOnClickOnDay write FOnClickOnDay;
    property OnGetAppointments: TOnGetAppointments read FOnGetAppointments write FOnGetAppointments;
    property OnCheckIsHoliday: TOnCheckIsHoliday read FOnCheckIsHoliday write FOnCheckIsHoliday;
    property OnGetAppointmentHint: TOnGetAppointmentHint read FOnGetAppointmentHint write FOnGetAppointmentHint;
    property OnGetDateHint: TOnGetDateHint read FOnGetDateHint write FOnGetDateHint;

  end;

implementation

uses
  SysUtils, DateUtils, Math, Forms

  {$ifdef fpc}
  ,LCLIntf
  ,LclType
  ,LclProc
  ,LResources
  {$ifdef debug}, LazLogger
  {$endif}
  {$else}
  {$ifdef windows}, Windows{$endif}
  {$endif}
  , mGraphicsUtility, mUtility;


{ TmCalendar }

procedure TmCalendar.Paint_BoxWithText(ACanvas: TCanvas; const ARect: TRect; const AText: string; const ATextAlignment: TAlignment; const aSelected: boolean);
var
  BoxRect: TRect;
  inf: integer;
begin
  Paint_Box(aCanvas, ARect, aSelected);
  if AText <> '' then
  begin
    BoxRect := aRect;

    if BoxRect.Height <= 15 then
      inf := -1
    else
      inf := -2;
    InflateRect(BoxRect, inf, inf);
    WriteText(ACanvas, BoxRect, AText, ATextAlignment, True);
  end;
end;

procedure TmCalendar.Paint_BoxWithText(ACanvas: TCanvas; const ARect: TRect; const ARows: TStringList; const ATextAlignment: TAlignment; const aSelected: boolean);
var
  delta, r: integer;
  ar: TRect;
begin
  Paint_Box(aCanvas, ARect, aSelected);
  if ARows.Count > 0 then
  begin
    delta := (ARect.Height div 2) - 1;
    for r := 0 to ARows.Count - 1 do
    begin
      ar := Classes.Rect(ARect.Left, ARect.Top + (delta * r), aRect.Right, aRect.Top + (delta * (r + 1)));
      WriteText(ACanvas, ar, ARows.Strings[r], ATextAlignment, True);
    end;
  end;
end;

procedure TmCalendar.Paint_Box(ACanvas: TCanvas; const ARect: TRect; const aSelected: boolean);
begin
  ACanvas.Brush.Style := bsSolid;
  ACanvas.FillRect(aRect);
  if aSelected then
  begin
    ACanvas.Pen.Color := FSelectedColor;
    ACanvas.Pen.Style := psDash;
    ACanvas.Pen.Width := 4;
  end
  else
  begin
    ACanvas.Pen.Color := DarkerColor(ACanvas.Brush.Color, 20);
    ACanvas.Pen.Style := psSolid;
    ACanvas.Pen.Width := 1;
  end;
  ACanvas.Line(aRect.Left, aRect.Bottom, aRect.Right, aRect.Bottom);
  ACanvas.Line(aRect.Left, aRect.Bottom, aRect.Left, aRect.Top);
  ACanvas.Line(aRect.Right, aRect.Bottom, aRect.Right, aRect.Top);
  ACanvas.Line(aRect.Left, aRect.Top, aRect.Right, aRect.Top);
end;

procedure TmCalendar.GetMonthCaptionRect(out aRect: TRect; const aRow, aCol: integer);
begin
  aRect := Classes.Rect(aCol * FItemWidth, aRow * FItemHeight, (aCol + 1) * FItemWidth, aRow * FItemHeight + FCaptionSize);
end;

procedure TmCalendar.GetMonthWeekDaysTitleRect(out aRect: TRect; const aRow, aCol: integer);
begin
  GetMonthCaptionRect(aRect, aRow, aCol);
  aRect.Top := aRect.Bottom;
  aRect.Bottom := aRect.Bottom + FTitleSize;
end;

procedure TmCalendar.Paint_FillBackground(aCanvas: TCanvas; const aRect: TRect);
begin
  aCanvas.Brush.Color := Self.Color;
  aCanvas.Pen.Mode := pmCopy;
  aCanvas.Brush.Style := bsSolid;

  aCanvas.FillRect(aRect);
end;

procedure TmCalendar.Paint_Items(aCanvas: TCanvas);
var
  tmpRow, tmpCol: integer;
begin
  for tmpRow := 0 to FRows - 1 do
  begin
    for tmpCol := 0 to FCols - 1 do
    begin
      if FItemType = itMonth then
      begin
        Paint_Month_Caption(aCanvas, tmpRow, tmpCol);
        Paint_Month_Weekdays(aCanvas, tmpRow, tmpCol);
        Paint_Month_Days(aCanvas, tmpRow, tmpCol);
      end;
    end;
  end;
end;

procedure TmCalendar.Paint_Month_Caption(aCanvas: TCanvas; aRow, aCol: integer);
var
  tmpRect: TRect;
  str: string;
  tmpRefDate: TDateTime;
  tmpYear, tmpMonth, tmpDay: word;
begin
  GetMonthCaptionRect(tmpRect, aRow, aCol);
  tmpRefDate := GetItemRefDate(aRow, aCol);
  DecodeDate(tmpRefDate, tmpYear, tmpMonth, tmpDay);
  tmpRefDate := GetItemRefDate(aRow, aCol);
  DecodeDate(tmpRefDate, tmpYear, tmpMonth, tmpDay);
  str := Uppercase(FormatSettings.LongMonthNames[tmpMonth] + ' ' + IntToStr(tmpYear));
  aCanvas.Brush.Color := Self.Color;
  aCanvas.Font := Self.Font;
  aCanvas.Font.Color := FCaptionsColor;
  Paint_BoxWithText(aCanvas, tmpRect, str, taCenter, False);
end;

procedure TmCalendar.Paint_Month_Weekdays(aCanvas: TCanvas; aRow, aCol: integer);
var
  x1, y1, x2, y2, i, we, idx: integer;
  r: TRect;
begin
  aCanvas.Font := Self.Font;
  aCanvas.Font.Color := FWeekdaysColor;
  aCanvas.Brush.Style := bsClear;
  we := 6;
  if coHideSaturdayAndSunday in FOptions then
    Dec(we, 2)
  else if coHideSunday in FOptions then
    Dec(we);

  for i := 0 to we do
  begin
    x1 := i * FDayWidth + aCol * FItemWidth;
    y1 := aRow * FItemHeight + FCaptionSize;
    x2 := x1 + FDayWidth;
    y2 := y1 + FTitleSize;
    r := Classes.Rect(x1, y1, x2, y2);
    idx := ((i + 1) mod 7) + 1;

    if (coSundayIsHoliday in FOptions) and (idx = 1) then
      aCanvas.Font.Color := FHolidaysColor
    else if (coSaturdayIsHoliday in FOptions) and (idx = 7) then
      aCanvas.Font.Color := FHolidaysColor
    else
      aCanvas.Font.Color := FWeekdaysColor;

    if r.Width < 30 then
      WriteText(aCanvas, r, LeftStr(Uppercase(FormatSettings.ShortDayNames[idx]), 1), FDayAlignment, True)
    else
      WriteText(aCanvas, r, Uppercase(FormatSettings.ShortDayNames[idx]), FDayAlignment, True);
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
  i, k: integer;
  x1, y1, x2, y2, yheader, dow: integer;
  r, hr, ar: TRect;
  tmpYear, tmpMonth, tmpDay: word;
  curDate: TDateTime;
  mr: integer;
  curDayAppointments: TmCalendarAppointments;
begin
  aCanvas.Font := Self.Font;
  aCanvas.Font.Color := FDaysColor;
  if FItemType = itMonth then
  begin
    curDate := GetItemRefDate(aRow, aCol);
    DecodeDate(curDate, tmpYear, tmpMonth, tmpDay);
    if coHideSaturdayAndSunday in FOptions then
    begin
      mr := 4;
      dow := DayOfTheWeek(curDate);
      while (dow = DaySaturday) or (dow = DaySunday) do
      begin
        curDate := curDate + 1;
        Inc(tmpDay);
        dow := DayOfTheWeek(curDate);
      end;
    end
    else
      mr := 5;
    for k := 0 to mr do
    begin
      y1 := (aRow * FItemHeight) + FCaptionSize + FTitleSize + (k * FDayHeight);
      y2 := y1 + FDayHeight;
      dow := DayOfTheWeek(curDate);
      for i := 0 to 6 do
      begin
        x1 := (i * FDayWidth) + (aCol * FItemWidth);
        x2 := x1 + FDayWidth;
        r := Classes.Rect(x1, y1, x2, y2);

        if dow = i + 1 then
        begin
          if not (((coHideSaturdayAndSunday in FOptions) and ((dow = DaySaturday) or (dow = DaySunday))) or ((coHideSunday in FOptions) and (dow = DaySunday))) then
          begin
            if FSelectedBucketsDictionary.Contains(round(curDate)) then
            begin
              aCanvas.Font.Color := FSelectedTextColor;
              aCanvas.Brush.Color := FSelectedColor;
            end
            else
            begin
              if (coSundayIsHoliday in FOptions) and (dow = DaySunday) then
                aCanvas.Font.Color := FHolidaysColor
              else if (coSaturdayIsHoliday in FOptions) and (dow = DaySaturday) then
                aCanvas.Font.Color := FHolidaysColor
              else
              begin
                if Assigned(FOnCheckIsHoliday) and FOnCheckIsHoliday(curDate) then
                  aCanvas.Font.Color := FHolidaysColor
                else
                  aCanvas.Font.Color := FDaysColor;
              end;
              aCanvas.Brush.Color := Self.Color;
            end;
            if (FStyle = csAppointmentsList) and Assigned(FOnGetAppointments) then
            begin
              Paint_Box(aCanvas, r, False);
              FAppointmentsPerDay.Remove(trunc(curDate));
              curDayAppointments := TmCalendarAppointments.Create;
              FAppointmentsPerDay.Add(trunc(curDate), curDayAppointments);
              FOnGetAppointments(curDate, curDayAppointments);
              yheader := y1 + max(MIN_DAY_HEADER_HEIGHT, trunc(FDayHeight * 0.2));
              hr := Classes.Rect(x1, y1, x2, yheader);
              ar := Classes.Rect(x1, yheader + 1, x2, y2);
              Paint_BoxWithText(aCanvas, hr, IntToStr(tmpDay), FDayAlignment, False); // draw header
              if curDayAppointments.Count > 0 then
                Paint_Appointments(aCanvas, ar, curDayAppointments);
            end
            else
              Paint_BoxWithText(aCanvas, r, IntToStr(tmpDay), FDayAlignment, False);
          end;

          curDate := curDate + 1;
          dow := DayOfTheWeek(curDate);
          Inc(tmpDay);
        end;
        if MonthOf(curDate) <> tmpMonth then
          exit;
      end;

    end;
  end;
end;

procedure TmCalendar.Paint_Appointments(aCanvas: TCanvas; const aRect: TRect; const aAppointments: TmCalendarAppointments);
var
  availableHeight: integer;
  i, r, c, cols, maxInACol: integer;
  x1, y1, x2, y2, h, w, hi, wi: integer;
  scaleFactor: double;
  paintRect, iconRect: TRect;
  tmpRows: TStringList;
begin
  if aAppointments.Count = 0 then
    exit;
  paintRect := aRect;
  InflateRect(paintRect, -1 * APPOINTMENT_MARGIN, -1 * APPOINTMENT_MARGIN);
  availableHeight := aRect.Height - APPOINTMENT_MARGIN;
  maxInACol := availableHeight div (FMinAppointmentHeight + APPOINTMENT_MARGIN);
  if maxInACol <> 0 then
    cols := Ceil(aAppointments.Count / maxInACol)
  else
    cols := 1;

  if cols = 0 then
    exit;

  h := (paintRect.Height - (APPOINTMENT_MARGIN * (aAppointments.Count - 1))) div aAppointments.Count;
  h := min(FMaxAppointmentHeight, max(FMinAppointmentHeight, h));
  w := (paintRect.Right - paintRect.Left - ((cols - 1) * APPOINTMENT_MARGIN)) div cols;

  x1 := paintRect.Left;
  x2 := x1 + w;
  y1 := paintRect.Top;

  i := 0;
  r := 0;
  c := 0;

  while i <= aAppointments.Count - 1 do
  begin
    if r >= maxInACol then
    begin
      r := 0;
      Inc(c);
      x1 := x2 + APPOINTMENT_MARGIN;
      x2 := x1 + w;
      y1 := paintRect.Top;
    end;
    y2 := y1 + h;
    aAppointments.Get(i).DrawnRect := Classes.Rect(x1, y1, x2, y2);
    //if FSelectedAppointmentsDictionary.Contains(aAppointments.Get(i).UniqueId) then
    //begin
    //  aCanvas.Brush.Color := FSelectedColor;
    //  aCanvas.Font.Color:= FSelectedTextColor;
    //end;
    aCanvas.Brush.Color := aAppointments.Get(i).Color;
    if IsDark(aAppointments.Get(i).Color) then
      aCanvas.Font.Color := LighterColor(aAppointments.Get(i).Color, 40)
    else
      aCanvas.Font.Color := DarkerColor(aAppointments.Get(i).Color, 40);
    aCanvas.Pen.Color := aCanvas.Font.Color;
    if aCanvas.TextWidth(aAppointments.Get(i).Description) > aAppointments.Get(i).DrawnRect.Width then
    begin
      tmpRows := TStringList.Create;
      try
        WordwrapStringByRows(aAppointments.Get(i).Description, 2, tmpRows);
        Paint_BoxWithText(aCanvas, aAppointments.Get(i).DrawnRect, tmpRows, taLeftJustify, FSelectedAppointmentsDictionary.Contains(aAppointments.Get(i).UniqueId));
      finally
        tmpRows.Free;
      end;
    end
    else
      Paint_BoxWithText(aCanvas, aAppointments.Get(i).DrawnRect, aAppointments.Get(i).Description, taLeftJustify, FSelectedAppointmentsDictionary.Contains(aAppointments.Get(i).UniqueId));
    if Assigned(aAppointments.Get(i).Icon) then //.Width > 0 then
    begin
      iconRect := aAppointments.Get(i).DrawnRect;
      InflateRect(iconRect, -2, -2);
      wi := aAppointments.Get(i).Icon.Width;
      hi := aAppointments.Get(i).Icon.Height;
      if (iconRect.Width >= wi) and (iconRect.Height >= hi) then
        aCanvas.Draw(iconRect.Right - wi, iconRect.Top, aAppointments.Get(i).Icon)
      else
      begin
        scaleFactor := max(wi / iconRect.Width, hi / iconRect.Height);
        wi := trunc(wi / scaleFactor);
        hi := trunc(hi / scaleFactor);
        iconRect.Left := iconRect.Right - wi + 1;
        iconRect.Bottom := iconRect.Top + hi - 1;
        {$IFDEF FPC}
        aCanvas.AntialiasingMode := amON;
        {$ENDIF}
        aCanvas.StretchDraw(iconRect, aAppointments.Get(i).Icon);
      end;
    end;
    y1 := y2 + 2;
    Inc(r);
    Inc(i);
  end;
end;

function TmCalendar.GetItemRefDate(aRow, aCol: integer): TDateTime;
var
  tmpDay, tmpMonth, tmpYear: word;
  distance: integer;
begin
  Result := 0;
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
  FSelectedAppointmentsDictionary.Free;
  FSelectedBuckets.Free;
  FSelectedAppointments.Free;
  FMouseMoveData.Free;
  FAppointmentsPerDay.Free;
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
  tmpX, tmpY, we, mr: integer;
begin
  if not Self.HandleAllocated then
    exit;

  FMustCalculate := False;
  if FItemType = itMonth then
  begin
    we := 7;
    mr := 6;
    if coHideSaturdayAndSunday in FOptions then
    begin
      Dec(we, 2);
      Dec(mr);
    end
    else if coHideSunday in FOptions then
      Dec(we);
    FDayWidth := (Self.ClientWidth) div (we * FCols);
    FDayHeight := (Self.ClientHeight - (FCaptionSize + FTitleSize) * FRows) div (mr * FRows);

    tmpX := ((FDayWidth * we)) * FCols;
    tmpY := ((FDayHeight * mr) + FCaptionSize + FTitleSize) * FRows;

    FItemWidth := tmpX div FCols;
    FItemHeight := tmpY div FRows;
  end;

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
  if FCaptionsColor = AValue then Exit;
  FCaptionsColor := AValue;
  Invalidate;
end;

procedure TmCalendar.SetCaptionSize(AValue: integer);
begin
  if FCaptionSize = AValue then Exit;
  FCaptionSize := AValue;
  Invalidate;
end;

procedure TmCalendar.SetDayAlignment(AValue: TAlignment);
begin
  if FDayAlignment = AValue then Exit;
  FDayAlignment := AValue;
  Invalidate;
end;

procedure TmCalendar.SetDaysColor(AValue: TColor);
begin
  if FDaysColor = AValue then Exit;
  FDaysColor := AValue;
  Invalidate;
end;

procedure TmCalendar.SetHolidaysColor(AValue: TColor);
begin
  if FHolidaysColor = AValue then Exit;
  FHolidaysColor := AValue;
  Invalidate;
end;

procedure TmCalendar.SetMaxAppointmentHeight(AValue: integer);
begin
  if FMaxAppointmentHeight = AValue then Exit;
  FMaxAppointmentHeight := AValue;
  Invalidate;
end;

procedure TmCalendar.SetMinAppointmentHeight(AValue: integer);
begin
  if FMinAppointmentHeight = AValue then Exit;
  FMinAppointmentHeight := AValue;
  Invalidate;
end;

procedure TmCalendar.SetOptions(AValue: TmCalendarOptions);
begin
  if FOptions = AValue then Exit;
  FOptions := AValue;
  DoInternalSizeCalculation;
  Invalidate;
end;

procedure TmCalendar.SetRows(AValue: integer);
begin
  if FRows = AValue then Exit;
  if AValue >= 1 then
  begin
    FRows := AValue;
    DoInternalSizeCalculation;
    Invalidate;
  end;
end;

procedure TmCalendar.SetItemType(AValue: TmCalendarItemType);
begin
  if FItemType = AValue then Exit;
  FItemType := AValue;
  DoInternalSizeCalculation;
  Invalidate;
end;

procedure TmCalendar.SetLinesColor(AValue: TColor);
begin
  if FLinesColor = AValue then Exit;
  FLinesColor := AValue;
  Invalidate;
end;

procedure TmCalendar.SetSelectedColor(AValue: TColor);
begin
  if FSelectedColor = AValue then Exit;
  FSelectedColor := AValue;
  Invalidate;
end;

procedure TmCalendar.SetSelectedTextColor(AValue: TColor);
begin
  if FSelectedTextColor = AValue then Exit;
  FSelectedTextColor := AValue;
  Invalidate;
end;

procedure TmCalendar.SetStartDate(AValue: TDateTime);
begin
  if FStartDate = AValue then Exit;
  FStartDate := AValue;
  InternalClearDaysSelection;
  InternalClearAppointmentsSelection;
  Invalidate;
end;

procedure TmCalendar.SetStyle(AValue: TmCalendarStyle);
begin
  if FStyle = AValue then Exit;
  FStyle := AValue;
  Invalidate;
end;

procedure TmCalendar.SetWeekdaysColor(AValue: TColor);
begin
  if FWeekdaysColor = AValue then Exit;
  FWeekdaysColor := AValue;
  Invalidate;
end;

procedure TmCalendar.SetCols(AValue: integer);
begin
  if FCols = AValue then Exit;
  if AValue >= 1 then
  begin
    FCols := AValue;
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

procedure TmCalendar.InternalClearDaysSelection;
begin
  FSelectedBucketsDictionary.Clear;
  FSelectedBuckets.Clear;
end;

procedure TmCalendar.InternalClearAppointmentsSelection;
begin
  FSelectedAppointmentsDictionary.Clear;
  FSelectedAppointments.Clear;
end;

procedure TmCalendar.InternalSelectAppointment(const aAppointmentUniqueId: string);
begin
  FSelectedAppointmentsDictionary.Add(aAppointmentUniqueId, FSelectedAppointmentsDictionary);
  FSelectedAppointments.Add(aAppointmentUniqueId);
end;

procedure TmCalendar.InternalUnselectAppointment(const aAppointmentUniqueId: string);
begin
  FSelectedAppointmentsDictionary.Remove(aAppointmentUniqueId);
  FSelectedAppointments.Delete(FSelectedAppointments.IndexOf(aAppointmentUniqueId));
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
  singleItemWidth, singleItemHeight, dow: integer;
  row, col, monthRow, monthCol, i: integer;
  refDate, targetDate: TDateTime;
  refYear, refMonth, refDay: word;
  day: integer;
  appointments: TmCalendarAppointments;
begin
  FMouseMoveData.Clear;
  if not PtInRect(ClientRect, Classes.Point(X, Y)) then
  begin
    HideHint;
    exit;
  end;

  if (FDayWidth = 0) or (FDayHeight = 0) or (FCols = 0) or (FRows = 0) then
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
    monthCol := (X - 2 - ClientRect.Left - (col * FItemWidth)) div FDayWidth;

    day := monthCol + 1 + (monthRow * 7);
    dow := DayOfTheWeek(refDate);
    if (coHideSaturdayAndSunday in FOptions) and ((dow = DaySunday) or (dow = DaySaturday)) then
      day := day + 8 - dow
    else
      day := day - dow + 1;

    {$ifdef debug}
    debugln('Click - day: ' + IntToStr(day));
    {$ifdef linux}
    writeln('Click - day: ' + IntToStr(day));
    {$endif}
    {$endif}

    if (day >= 1) and (day <= DaysInAMonth(refYear, refMonth)) then
    begin
      targetDate := EncodeDate(refYear, refMonth, day);
      FMouseMoveData.MouseOnDays := True;
      FMouseMoveData.Day := round(targetDate);
      if FStyle = csAppointmentsList then
      begin
        appointments := FAppointmentsPerDay.Find(round(FMouseMoveData.Day)) as TmCalendarAppointments;
        if Assigned(appointments) and (appointments.Count > 0) then
        begin
          for i := 0 to appointments.Count - 1 do
          begin
            if PtInRect(appointments.Get(i).DrawnRect, Classes.Point(X, Y)) then
            begin
              FMouseMoveData.Appointment.Assign(appointments.Get(i));
              FMouseMoveData.MouseOnDays := False;
              FMouseMoveData.MouseOnAppointments := True;
              break;
            end;
          end;
        end;
      end;
      {$ifdef debug}
      debugln('Click - clicked date: ' + DateToStr(targetDate));
      {$ifdef linux}
      writeln('Click - clicked date: ' + DateToStr(targetDate));
      {$endif}
      {$endif}
    end;
  end;
end;

procedure TmCalendar.HideHint;
begin
  Self.ShowHint := False;
  Application.CancelHint;
end;

procedure TmCalendar.ShowHintWindow(const aText: string; const aPoint: TPoint);
begin
  //  if UseApplicationHint then begin
  Self.Hint := aText;
  Self.ShowHint := Self.Hint <> '';
  if not Self.ShowHint then exit;
  Application.HintPause := 0;
  Application.ActivateHint(aPoint);
(*  end
  else begin
    if FHintWindow = nil then
      FHintWindow := THintWindow.Create(nil);
    if h = '' then exit;
    r := FHintWindow.CalcHintRect(FChart.Width, h, Nil);
    if Assigned(OnHintLocation) then begin
      sz.CX := r.Right - r.Left;
      sz.CY := r.Bottom - r.Top;
      OnHintLocation(Self, sz, APoint);
    end;
    OffsetRect(r, APoint.X, APoint.Y);
    FHintWindow.ActivateHint(r, h);
  end;*)
end;

procedure TmCalendar.Paint;
var
  drawingRect: TRect;
  tmpCanvas: TCanvas;
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
  mustPaint: boolean;
  i, curDay: integer;
begin
  mustPaint := False;
  if FMouseMoveData.MouseOnDays then
  begin
    if Button = mbLeft then
    begin
      InternalClearAppointmentsSelection;

      if CtrlPressed then
      begin
        if FSelectedBucketsDictionary.Contains(FMouseMoveData.Day) then
          InternalUnselectDay(FMouseMoveData.Day)
        else
          InternalSelectDay(FMouseMoveData.Day);
        mustPaint := True;
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
            Dec(i);
          end;
          curDay := FSelectedBuckets.Items[0];
          if curDay < FMouseMoveData.Day then
          begin
            {$ifdef debug}
            debugln('Select from ' + DateToStr(curDay + 1) + ' to ' + DateToStr(FMouseMoveData.Day));
            {$endif}
            for i := curDay + 1 to FMouseMoveData.Day do
              InternalSelectDay(i);
            ;
          end
          else if curDay > FMouseMoveData.Day then
          begin
            {$ifdef debug}
            debugln('Select from ' + DateToStr(FMouseMoveData.Day + 1) + ' to ' + DateToStr(curDay));
            {$endif}
            for i := FMouseMoveData.Day + 1 to curDay do
              InternalSelectDay(i);
          end;
          mustPaint := True;
        end;
      end
      else
      begin
        InternalClearDaysSelection;
        InternalSelectDay(FMouseMoveData.Day);
        mustPaint := True;
      end;
    end;
  end;
  if FMouseMoveData.MouseOnAppointments then
  begin
    if Button = mbLeft then
    begin
      InternalClearDaysSelection;
      if CtrlPressed then
      begin
        if FSelectedAppointmentsDictionary.Contains(FMouseMoveData.Appointment.UniqueId) then
          InternalUnselectAppointment(FMouseMoveData.Appointment.UniqueId)
        else
          InternalSelectAppointment(FMouseMoveData.Appointment.UniqueId);
        mustPaint := True;
      end
      else
      begin
        InternalClearAppointmentsSelection;
        InternalSelectAppointment(FMouseMoveData.Appointment.UniqueId);
        mustPaint := True;
      end;
    end;
  end;
  inherited MouseUp(Button, Shift, X, Y);
  if mustPaint then
    Self.Paint;
  if FMouseMoveData.MouseOnDays and Assigned(FOnClickOnDay) then
    FOnClickOnDay(Button, Shift, FMouseMoveData.Day);
end;

procedure TmCalendar.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  if Assigned(OnGetDateHint) or (Assigned(OnGetAppointmentHint) and (FStyle = csAppointmentsList)) then
  begin
    SaveMouseMoveData(X, Y);
    if Assigned(OnGetDateHint) and FMouseMoveData.MouseOnDays then
    begin
      ShowHintWindow(OnGetDateHint(FMouseMoveData.Day), Self.ClientToScreen(TPoint.Create(X, Y)));
    end
    else if Assigned(OnGetAppointmentHint) and FMouseMoveData.MouseOnAppointments then
    begin
      ShowHintWindow(OnGetAppointmentHint(FMouseMoveData.Appointment), Self.ClientToScreen(TPoint.Create(X, Y)));
    end
    else
      HideHint;
  end
  else
    HideHint;

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
  FDoubleBufferedBitmap.SetSize(max(Screen.Width, 4096), max(Screen.Height, 2160));
  FSelectedBucketsDictionary := TmIntegerDictionary.Create(False);
  FSelectedAppointmentsDictionary := TmStringDictionary.Create(False);
  FSelectedBuckets := TIntegerList.Create;
  FSelectedAppointments := TStringList.Create;
  FMouseMoveData := TmCalendarMouseMoveData.Create;
  FAppointmentsPerDay := TmIntegerDictionary.Create(True);

  FMustCalculate := True;
  FItemType := itMonth;
  FCols := 1;
  FRows := 1;
  FCaptionSize := 16;
  FTitleSize := 16;
  FStartDate := Date;
  FDayAlignment := taCenter;
  FWeekdaysColor := clBlack;
  FDaysColor := clBlack;
  FHolidaysColor := clRed;
  FSelectedColor := clSkyBlue; // TColor($F0CAA6);
  FSelectedTextColor := clWhite;
  FCaptionsColor := clBlack;
  Self.Color := clWhite;
  FLinesColor := clLtGray;
  FStyle := csSimple;
  FOptions := [];
  FMinAppointmentHeight := 5;
  FMaxAppointmentHeight := 40;

  FOnClickOnDay := nil;
  FOnGetAppointments := nil;
  FOnCheckIsHoliday := nil;
  FOnGetAppointmentHint := nil;
  FOnGetDateHint := nil;
end;



end.
