// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mTimerulerScales;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

type

  TmScale = class abstract
  protected
    FDisplayFormat: string;
    procedure SetDisplayFormat(Value: string);

  public
    class function TicksBetween(start, stop: TDateTime): integer; virtual; abstract;
    class function AddTicks(start: TDateTime; ticks: integer): TDateTime; virtual; abstract;
    class function TruncDate(start: TDateTime): TDateTime; virtual; abstract;
    class function NextBucket(start: TDateTime) : TDateTime;
    class function RoundDate(start: TDateTime): TDateTime;
    class function isMajorThan(other : TmScale): boolean; virtual; abstract;
    class function intervalToPixels(startDate, endDate: TDateTime; tickWidth : integer): integer; virtual; abstract;
    class function pixelsToDateTime(pixels : integer; start : TDateTime; tickWidth : integer): TDateTime; virtual; abstract;

    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat;
  end;

  TmScaleClass = class of TmScale;

  { TmScaleYear }

  TmScaleYear = class(TmScale)
  public
    constructor Create();

    class function TicksBetween(start, stop: TDateTime): integer; override;
    class function AddTicks(start: TDateTime; ticks: integer): TDateTime; override;
    class function TruncDate(start: TDateTime): TDateTime; override;
    class function isMajorThan(other : TmScale): boolean; override;
    class function intervalToPixels(startDate, endDate: TDateTime; tickWidth : integer): integer; override;
    class function pixelsToDateTime(pixels : integer; start : TDateTime; tickWidth : integer): TDateTime; override;
  end;

  { TmScaleQuarter }

  TmScaleQuarter = class(TmScale)
  public
    constructor Create();

    class function TicksBetween(start, stop: TDateTime): integer; override;
    class function AddTicks(start: TDateTime; ticks: integer): TDateTime; override;
    class function TruncDate(start: TDateTime): TDateTime; override;
    class function isMajorThan(other : TmScale): boolean; override;
    class function intervalToPixels(startDate, endDate: TDateTime; tickWidth : integer): integer; override;
    class function pixelsToDateTime(pixels : integer; start : TDateTime; tickWidth : integer): TDateTime; override;
  end;

  { TmScaleMonth }

  TmScaleMonth = class(TmScale)
  public
    constructor Create();

    class function TicksBetween(start, stop: TDateTime): integer; override;
    class function AddTicks(start: TDateTime; ticks: integer): TDateTime; override;
    class function TruncDate(start: TDateTime): TDateTime; override;
    class function isMajorThan(other : TmScale): boolean; override;
    class function intervalToPixels(startDate, endDate: TDateTime; tickWidth : integer): integer; override;
    class function pixelsToDateTime(pixels : integer; start : TDateTime; tickWidth : integer): TDateTime; override;
  end;

  { TmScaleWeek }

  TmScaleWeek = class(TmScale)
  public
    constructor Create();

    class function TicksBetween(start, stop: TDateTime): integer; override;
    class function AddTicks(start: TDateTime; ticks: integer): TDateTime; override;
    class function TruncDate(start: TDateTime): TDateTime; override;
    class function isMajorThan(other : TmScale): boolean; override;
    class function intervalToPixels(startDate, endDate: TDateTime; tickWidth : integer): integer; override;
    class function pixelsToDateTime(pixels : integer; start : TDateTime; tickWidth : integer): TDateTime; override;
  end;

  { TmScaleDay }

  TmScaleDay = class(TmScale)
  public
    constructor Create();

    class function TicksBetween(start, stop: TDateTime): integer; override;
    class function AddTicks(start: TDateTime; ticks: integer): TDateTime; override;
    class function TruncDate(start: TDateTime): TDateTime; override;
    class function isMajorThan(other : TmScale): boolean; override;
    class function intervalToPixels(startDate, endDate: TDateTime; tickWidth : integer): integer; override;
    class function pixelsToDateTime(pixels : integer; start : TDateTime; tickWidth : integer): TDateTime; override;
  end;

   { TmScaleDayNotSunday }

   TmScaleDayNotSunday = class (TmScaleDay)
   public
     class function TicksBetween(start, stop: TDateTime): integer; override;
     class function AddTicks(start: TDateTime; ticks: integer): TDateTime; override;
     class function TruncDate(start: TDateTime): TDateTime; override;
     class function intervalToPixels(startDate, endDate: TDateTime; tickWidth : integer): integer; override;
     class function pixelsToDateTime(pixels : integer; start : TDateTime; tickWidth : integer): TDateTime; override;
   end;

   { TmScaleNotSaturdaySunday }

   TmScaleDayNotSaturdaySunday = class (TmScaleDay)
   public
     class function TicksBetween(start, stop: TDateTime): integer; override;
     class function AddTicks(start: TDateTime; ticks: integer): TDateTime; override;
     class function TruncDate(start: TDateTime): TDateTime; override;
     class function intervalToPixels(startDate, endDate: TDateTime; tickWidth : integer): integer; override;
     class function pixelsToDateTime(pixels : integer; start : TDateTime; tickWidth : integer): TDateTime; override;
   end;

  { TmScaleHour }

  TmScaleHour = class(TmScale)
  public
    constructor Create();

    class function TicksBetween(start, stop: TDateTime): integer; override;
    class function AddTicks(start: TDateTime; ticks: integer): TDateTime; override;
    class function TruncDate(start: TDateTime): TDateTime; override;
    class function isMajorThan(other : TmScale): boolean; override;
    class function intervalToPixels(startDate, endDate: TDateTime; tickWidth : integer): integer; override;
    class function pixelsToDateTime(pixels : integer; start : TDateTime; tickWidth : integer): TDateTime; override;
  end;

  { TmScaleMinute }

  TmScaleMinute = class(TmScale)
  public
    constructor Create();

    class function TicksBetween(start, stop: TDateTime): integer; override;
    class function AddTicks(start: TDateTime; ticks: integer): TDateTime; override;
    class function TruncDate(start: TDateTime): TDateTime; override;
    class function isMajorThan(other : TmScale): boolean; override;
    class function intervalToPixels(startDate, endDate: TDateTime; tickWidth : integer): integer; override;
    class function pixelsToDateTime(pixels : integer; start : TDateTime; tickWidth : integer): TDateTime; override;
  end;

  { TmScaleSecond }

  TmScaleSecond = class(TmScale)
  public
    constructor Create();

    class function TicksBetween(start, stop: TDateTime): integer; override;
    class function AddTicks(start: TDateTime; ticks: integer): TDateTime; override;
    class function TruncDate(start: TDateTime): TDateTime; override;
    class function isMajorThan(other : TmScale): boolean; override;
    class function intervalToPixels(startDate, endDate: TDateTime; tickWidth : integer): integer; override;
    class function pixelsToDateTime(pixels : integer; start : TDateTime; tickWidth : integer): TDateTime; override;
  end;

implementation

uses
  dateutils, SysUtils,
  mDateTimeUtility, mTimerulerDefs;

{ TmScaleDayNotSaturdaySunday }

class function TmScaleDayNotSaturdaySunday.TicksBetween(start, stop: TDateTime): integer;
var
  current, newStop, dow : integer;
  dummy : integer;
begin
  current := trunc(start);
  newStop := trunc(stop);
  dummy := 0;
  if (current <= newStop) then
  begin
    while (current < newStop) do
    begin
      dow := DayOfTheWeek(current);
      if (dow <> 7) and (dow <> 6) then
         inc(dummy);
      inc(current);
    end;
  end
  else
  begin
    while (current > newStop) do
    begin
      dow := DayOfTheWeek(current);
      if (dow <> 7) and (dow <> 6) then
         dec(dummy);
      dec(current);
    end;
  end;
  Result := dummy;
end;

class function TmScaleDayNotSaturdaySunday.AddTicks(start: TDateTime; ticks: integer): TDateTime;
var
  dummy, dow : integer;
  current : TDateTime;
  multiplier : integer;
begin
  if (ticks < 0) then
    multiplier := -1
  else
    multiplier := 1;
  dummy := abs(ticks);
  current := start;
  while (dummy > 0) do
  begin
    current := current + multiplier;
    dow := DayOfTheWeek(current);
    if (dow <> 7) and (dow <> 6) then
       dec(dummy);
  end;
  Result := current;
end;

class function TmScaleDayNotSaturdaySunday.TruncDate(start: TDateTime): TDateTime;
var
  dow : integer;
begin
  dow := DayOfTheWeek(start);
  if (dow = 7) then
    Result := TmScaleDay.TruncDate(start + 1)
  else
  if (dow = 6) then
    Result := TmScaleDay.TruncDate(start + 2)
  else
    Result := TmScaleDay.TruncDate(start);

end;

class function TmScaleDayNotSaturdaySunday.intervalToPixels(startDate, endDate: TDateTime; tickWidth: integer): integer;
var
  dow, f : integer;
  skip : integer;
begin
  skip := 0;
  for f := trunc(startDate) to trunc(endDate) do
  begin
    dow := DayOfTheWeek(f);
    if (dow = 7) or (dow = 6) then
      inc (skip);
  end;

  Result := Round((endDate - startDate - skip) * tickWidth);
end;

class function TmScaleDayNotSaturdaySunday.pixelsToDateTime(pixels: integer; start: TDateTime; tickWidth: integer): TDateTime;
var
  remaining, dow : integer;
  currentDate : TDateTime;
begin
  remaining := pixels;
  currentDate := start;
  while (remaining >= tickWidth) do
  begin
    currentDate := currentDate + 1;
    dow := DayOfTheWeek(currentDate);
    if (dow <> 7) and (dow <> 6) then
      remaining := remaining - tickWidth;
  end;
  Result := currentDate + remaining / tickWidth;
end;

{ TmScaleDayNotSunday }

class function TmScaleDayNotSunday.TicksBetween(start, stop: TDateTime): integer;
var
  current, newStop : integer;
  dummy : integer;
begin
  current := trunc(start);
  newStop := trunc(stop);
  dummy := 0;
  if (current <= newStop) then
  begin
    while (current < newStop) do
    begin
      if DayOfTheWeek(current) <> 7 then
         inc(dummy);
      inc(current);
    end;
  end
  else
  begin
    while (current > newStop) do
    begin
      if DayOfTheWeek(current) <> 7 then
         dec(dummy);
      dec(current);
    end;
  end;
  Result := dummy;
end;

class function TmScaleDayNotSunday.AddTicks(start: TDateTime; ticks: integer): TDateTime;
var
  dummy, dow : integer;
  current : TDateTime;
  multiplier : integer;
begin
  if (ticks < 0) then
    multiplier := -1
  else
    multiplier := 1;
  dummy := abs(ticks);
  current := start;
  while (dummy > 0) do
  begin
    current := current + multiplier;
    dow := DayOfTheWeek(current);
    if (dow <> 7) then
       dec(dummy);
  end;
  Result := current;

end;

class function TmScaleDayNotSunday.TruncDate(start: TDateTime): TDateTime;
begin
  if (DayOfTheWeek(start) = 7) then
    Result := TmScaleDay.TruncDate(start + 1)
  else
    Result := TmScaleDay.TruncDate(start);
end;

class function TmScaleDayNotSunday.intervalToPixels(startDate, endDate: TDateTime; tickWidth: integer): integer;
var
  dow, f : integer;
  skip : integer;
begin
  skip := 0;
  for f := trunc(startDate) to trunc(endDate) do
  begin
    dow := DayOfTheWeek(f);
    if (dow = 7) then
      inc (skip);
  end;

  Result := Round((endDate - startDate - skip) * tickWidth);
end;

class function TmScaleDayNotSunday.pixelsToDateTime(pixels: integer; start: TDateTime; tickWidth: integer): TDateTime;
var
  remaining, dow : integer;
  currentDate : TDateTime;
begin
  remaining := pixels;
  currentDate := start;
  while (remaining >= tickWidth) do
  begin
    currentDate := currentDate + 1;
    dow := DayOfTheWeek(currentDate);
    if (dow <> 7) then
      remaining := remaining - tickWidth;
  end;
  Result := currentDate + remaining / tickWidth;
end;

{ TmScaleSecond }

constructor TmScaleSecond.Create;
begin
  FDisplayFormat := 'dd';
end;

class function TmScaleSecond.TicksBetween(start, stop: TDateTime): integer;
begin
  Result := SecondsBetween(start, stop);
end;

class function TmScaleSecond.AddTicks(start: TDateTime; ticks: integer): TDateTime;
begin
    Result := start + (OneSecond * ticks);
end;

class function TmScaleSecond.TruncDate(start: TDateTime): TDateTime;
var
  Hour, Min, Sec, MSec: word;
begin
  DecodeTime(start, Hour, Min, Sec, MSec);
  Result :=  RecodeTime(start, Hour, Min, Sec, 0);
end;

class function TmScaleSecond.isMajorThan(other: TmScale): boolean;
begin
  Result :=  false;
end;

class function TmScaleSecond.intervalToPixels(startDate, endDate: TDateTime; tickWidth: integer): integer;
begin
  Result := Round((endDate - startDate) * tickWidth * 24 * 3600);
end;

class function TmScaleSecond.pixelsToDateTime(pixels: integer; start: TDateTime; tickWidth: integer): TDateTime;
begin
  Result := start + pixels / (tickWidth * 24 * 3600);
end;

{ TmScaleMinute }

constructor TmScaleMinute.Create;
begin
  FDisplayFormat := 'dd';
end;

class function TmScaleMinute.TicksBetween(start, stop: TDateTime): integer;
begin
  Result := MinutesBetween(start, stop);
end;

class function TmScaleMinute.AddTicks(start: TDateTime; ticks: integer): TDateTime;
begin
  Result := start + (OneMinute * ticks);
end;

class function TmScaleMinute.TruncDate(start: TDateTime): TDateTime;
var
  Hour, Min, Sec, MSec: word;
begin
  DecodeTime(start, Hour, Min, Sec, MSec);
  Result :=  RecodeTime(start, Hour, Min, 0, 0);
end;

class function TmScaleMinute.isMajorThan(other: TmScale): boolean;
begin
  Result :=  (other is TmScaleSecond);
end;

class function TmScaleMinute.intervalToPixels(startDate, endDate: TDateTime; tickWidth: integer): integer;
begin
  Result := Round((endDate - startDate) * tickWidth * 24 * 60);
end;

class function TmScaleMinute.pixelsToDateTime(pixels: integer; start: TDateTime; tickWidth: integer): TDateTime;
begin
  Result := start + pixels / (tickWidth * 24 * 60);
end;

{ TmScaleHour }

constructor TmScaleHour.Create;
begin
  FDisplayFormat := 'dd';
end;

class function TmScaleHour.TicksBetween(start, stop: TDateTime): integer;
begin
  Result := HoursBetween(start, stop);
end;

class function TmScaleHour.AddTicks(start: TDateTime; ticks: integer): TDateTime;
begin
  Result := start + (OneHour * ticks);
end;

class function TmScaleHour.TruncDate(start: TDateTime): TDateTime;
var
  Hour, Min, Sec, MSec: word;
begin
  DecodeTime(start, Hour, Min, Sec, MSec);
  Result :=  RecodeTime(start, Hour, 0, 0, 0);
end;

class function TmScaleHour.isMajorThan(other: TmScale): boolean;
begin
  Result :=  (other is TmScaleSecond) or (other is TmScaleMinute);
end;

class function TmScaleHour.intervalToPixels(startDate, endDate: TDateTime; tickWidth: integer): integer;
begin
  Result := Round((endDate - startDate) * tickWidth * 24);
end;

class function TmScaleHour.pixelsToDateTime(pixels: integer; start: TDateTime;tickWidth: integer): TDateTime;
begin
  Result := start + pixels / (tickWidth * 24);
end;

{ TmScaleDay }

constructor TmScaleDay.Create;
begin
  FDisplayFormat:= 'dd';
end;

class function TmScaleDay.TicksBetween(start, stop: TDateTime): integer;
begin
  Result := DaysBetween(start, stop);
end;

class function TmScaleDay.AddTicks(start: TDateTime; ticks: integer): TDateTime;
begin
  Result := start + ticks;
end;

class function TmScaleDay.TruncDate(start: TDateTime): TDateTime;
begin
  Result := StartOfTheDay(start);
end;

class function TmScaleDay.isMajorThan(other: TmScale): boolean;
begin
  Result :=  (other is TmScaleSecond) or (other is TmScaleMinute) or (other is TmScaleHour);
end;

class function TmScaleDay.intervalToPixels(startDate, endDate: TDateTime; tickWidth: integer): integer;
begin
  Result := Round((endDate - startDate) * tickWidth);
end;

class function TmScaleDay.pixelsToDateTime(pixels: integer; start: TDateTime; tickWidth: integer): TDateTime;
begin
  Result := start + pixels / tickWidth;
end;

{ TmScaleWeek }

constructor TmScaleWeek.Create;
begin
  FDisplayFormat:='x (yyyy)';
end;

class function TmScaleWeek.TicksBetween(start, stop: TDateTime): integer;
begin
  Result := WeeksBetween(start, stop);
end;

class function TmScaleWeek.AddTicks(start: TDateTime; ticks: integer): TDateTime;
begin
  Result := start + 7 * ticks;
end;

class function TmScaleWeek.TruncDate(start: TDateTime): TDateTime;
begin
  Result := StartOfTheWeek(start);
end;

class function TmScaleWeek.isMajorThan(other: TmScale): boolean;
begin
  Result := (other is TmScaleSecond) or (other is TmScaleMinute) or (other is TmScaleHour) or (other is TmScaleDay);
end;

class function TmScaleWeek.intervalToPixels(startDate, endDate: TDateTime; tickWidth: integer): integer;
begin
  Result := Round((endDate - startDate) * (tickWidth / 7));
end;

class function TmScaleWeek.pixelsToDateTime(pixels: integer; start: TDateTime; tickWidth: integer): TDateTime;
begin
  Result := start + pixels / (tickWidth / 7);
end;

{ TmScaleMonth }

constructor TmScaleMonth.Create;
begin
  FDisplayFormat:= 'mm/yyyy';
end;

class function TmScaleMonth.TicksBetween(start, stop: TDateTime): integer;
begin
  Result := MonthsBetween(start, stop);
end;

class function TmScaleMonth.AddTicks(start: TDateTime; ticks: integer): TDateTime;
begin
  Result := AddMonths(start, ticks);
end;

class function TmScaleMonth.TruncDate(start: TDateTime): TDateTime;
begin
  Result := StartOfTheMonth(start);
end;

class function TmScaleMonth.isMajorThan(other: TmScale): boolean;
begin
  Result := not ((other is TmScaleYear) or (other is TmScaleQuarter));
end;

class function TmScaleMonth.intervalToPixels(startDate, endDate: TDateTime; tickWidth: integer): integer;
begin
  Result := Round((endDate - startDate) * (tickWidth / DAYS_IN_A_MONTH));
end;

class function TmScaleMonth.pixelsToDateTime(pixels: integer; start: TDateTime; tickWidth: integer): TDateTime;
begin
  Result := start + pixels / (tickWidth / DAYS_IN_A_MONTH);
end;

{ TmScaleQuarter }

constructor TmScaleQuarter.Create;
begin
  FDisplayFormat:= 'mm/yyyy';
end;

class function TmScaleQuarter.TicksBetween(start, stop: TDateTime): integer;
begin
   Result := TmScaleMonth.TicksBetween(start, stop) div 3;
end;

class function TmScaleQuarter.AddTicks(start: TDateTime; ticks: integer): TDateTime;
begin
  Result := AddMonths(start, ticks * 3);
end;

class function TmScaleQuarter.TruncDate(start: TDateTime): TDateTime;
var
  Day, Month, Year: word;
  M: integer;
begin
  DecodeDate(start, Year, Month, Day);
  Dec(Month, (Month - 1) mod 3);
  M := (Month - 1 ) mod 3;
  if M <> 0 then
    Dec(Month, M);
  Day := 1;
  Result := EncodeDate(Year, Month, Day);
end;

class function TmScaleQuarter.isMajorThan(other: TmScale): boolean;
begin
  Result :=  not (other is TmScaleYear);
end;

class function TmScaleQuarter.intervalToPixels(startDate, endDate: TDateTime; tickWidth: integer): integer;
begin
  Result := Round((endDate - startDate) * (tickWidth / (DAYS_IN_A_MONTH * 3)));
end;

class function TmScaleQuarter.pixelsToDateTime(pixels: integer; start: TDateTime; tickWidth: integer): TDateTime;
begin
  Result := start + pixels / (tickWidth / (DAYS_IN_A_MONTH * 3));
end;

{ TmScaleYear }

constructor TmScaleYear.Create;
begin
  FDisplayFormat:= 'yyyy';
end;

class function TmScaleYear.TicksBetween(start, stop: TDateTime): integer;
begin
  Result := YearsBetween(start, stop);
end;

class function TmScaleYear.AddTicks(start: TDateTime; ticks: integer): TDateTime;
begin
  Result := AddYears(start, ticks);
end;

class function TmScaleYear.TruncDate(start: TDateTime): TDateTime;
begin
  Result := StartOfTheYear(start);
end;

class function TmScaleYear.isMajorThan(other: TmScale): boolean;
begin
  Result :=  true;
end;

class function TmScaleYear.intervalToPixels(startDate, endDate: TDateTime; tickWidth : integer): integer;
begin
  Result := Round((endDate - startDate) * (tickWidth / 365));
end;

class function TmScaleYear.pixelsToDateTime(pixels: integer; start : TDateTime; tickWidth : integer): TDateTime;
begin
  Result := start + pixels / (tickWidth / 365);
end;

procedure TmScale.SetDisplayFormat(Value: string);
begin
  FDisplayFormat := Value;
end;

class function TmScale.NextBucket(start: TDateTime): TDateTime;
var
  temp : TDateTime;
begin
  temp := RoundDate(start);
  Result := AddTicks(temp, 1);
end;

class function TmScale.RoundDate(start: TDateTime): TDateTime;
var
  Next: TDateTime;
begin
  Result := TruncDate(start);
  Next := AddTicks(Result, 1);
  if (start - Result) >= (Next - start) then
    Result := Next;
end;

end.
