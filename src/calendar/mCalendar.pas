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
  mDoubleList, mCalendarGUIClasses;

type

  TmCalendarItemType = (itMonth);


  { TmCalendar }

  TmCalendar = class (TCustomControl)
  strict private
    FVerticalItems : integer;
    FHorizontalItems : integer;
    FItemType : TmCalendarItemType;
    FBorderSize : integer;
    FCaptionSize : integer;
    FTitleSize : integer;
    FStartDate : TDateTime;
    FDayAlignment: TAlignment;
    FTitlesColor: TColor;
    FDaysColor : TColor;
    FCaptionsColor : TColor;

    // internal properties
    FItemWidth : integer;
    FItemHeight : integer;
    FDayWidth : integer;
    FDayHeight : integer;
    FMustCalculate : boolean;
    FDoubleBufferedBitmap: Graphics.TBitmap;
    FSelectedBuckets : TDoubleList;
    FMouseMoveData : TmCalendarMouseMoveData;

    procedure Paint_FillBackground(aCanvas : TCanvas; const aRect : TRect);
    procedure Paint_Items(aCanvas : TCanvas);
    procedure Paint_MonthCaptions (aCanvas: TCanvas; aX, aY : integer);
    procedure Paint_Titles_WeekDays (aCanvas: TCanvas; aX, aY : integer);
    procedure Paint_MonthDays(aCanvas: TCanvas; aX, aY : integer);
    function GetItemRefDate (aX, aY : integer) : TDateTime;
    procedure DrawFlatFrame (aCanvas : TCanvas ; const Rect : TRect );
    procedure DoInternalSizeCalculation;
    procedure SetBorderSize(AValue: integer);
    procedure SetCaptionsColor(AValue: TColor);
    procedure SetDayAlignment(AValue: TAlignment);
    procedure SetDaysColor(AValue: TColor);
    procedure SetHorizontalItems(AValue: integer);
    procedure SetItemType(AValue: TmCalendarItemType);
    procedure SetTitlesColor(AValue: TColor);
    procedure SetVerticalItems(AValue: integer);

    procedure DoPaintTo(aCanvas: TCanvas; aRect: TRect);
    procedure SaveMouseMoveData(X, Y: integer);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$ifdef linux}
    // explanation here: https://forum.lazarus.freepascal.org/index.php?topic=38041.0
    procedure SetBounds(aLeft, aTop, aWidth, aHeight: integer); override;
    {$endif}

    property VerticalItems : integer read FVerticalItems write SetVerticalItems default 1;
    property HorizontalItems : integer read FHorizontalItems write SetHorizontalItems default 1;
    property ItemType : TmCalendarItemType read FItemType write SetItemType;
    property BorderSize : integer read FBorderSize write SetBorderSize;
    property DayAlignment : TAlignment read FDayAlignment write SetDayAlignment;
    property TitlesColor : TColor read FTitlesColor write SetTitlesColor;
    property DaysColor : TColor read FDaysColor write SetDaysColor;
    property CaptionsColor : TColor read FCaptionsColor write SetCaptionsColor;
  end;

implementation

uses
  SysUtils, DateUtils, Math, Forms
  {$ifdef windows}, Windows{$endif}
  {$ifdef fpc}{$ifdef debug}, LazLogger{$endif}{$endif}
  , mGraphicsUtility;


{ TmCalendar }

procedure TmCalendar.Paint_FillBackground(aCanvas : TCanvas; const aRect : TRect);
begin
  aCanvas.Brush.Color:= Self.Color;
  aCanvas.Pen.Mode := pmCopy;
  aCanvas.Brush.Style := bsSolid;

  aCanvas.FillRect(aRect);
end;

procedure TmCalendar.Paint_Items(aCanvas : TCanvas);
var
  x, y : integer;
begin
  for y := 0 to FVerticalItems - 1 do
  begin
    for x := 0 to FHorizontalItems - 1 do
    begin
      if FItemType = itMonth then
      begin
        Paint_MonthCaptions (aCanvas, x, y);
        Paint_Titles_WeekDays (aCanvas, x, y);
        Paint_MonthDays (aCanvas, x, y);
      end;
    end;
  end;
end;

procedure TmCalendar.Paint_MonthCaptions(aCanvas: TCanvas; aX, aY: integer);
var
  tmpRect : TRect;
  str : String;
  tmpRefDate : TDateTime;
  tmpYear, tmpMonth, tmpDay : word;
  w, h : integer;
begin
  aCanvas.Pen.Color := cl3DDkShadow;
  aCanvas.Pen.Style := psSolid;
  tmpRect := Classes.Rect (aX * FItemWidth, aY * FItemHeight, (aX + 1) * FItemWidth, aY * FItemHeight + FCaptionSize);
  Self.DrawFlatFrame(aCanvas, tmpRect);
  aCanvas.MoveTo(tmpRect.Left, tmpRect.Top -1);
  aCanvas.LineTo(tmpRect.Right, tmpRect.Top -1);

  str := '';

  tmpRefDate:= GetItemRefDate(aX, aY);
  DecodeDate(tmpRefDate, tmpYear, tmpMonth, tmpDay);
  str := DefaultFormatSettings.LongMonthNames[tmpMonth] + ' ' + IntToStr(tmpYear);

  aCanvas.Font := Self.Font;
  aCanvas.Font.Color := FCaptionsColor;
  aCanvas.Brush.Style := bsClear;
  w := 0;
  h := 0;
  aCanvas.GetTextSize(str, w, h);
  aCanvas.TextOut(tmpRect.Left + ((FItemWidth - w) div 2), tmpRect.Top, str);
end;

procedure TmCalendar.Paint_Titles_WeekDays(aCanvas: TCanvas; aX, aY: integer);
var
  x1 , y1 , x2 , y2 , i : integer;
  r : TRect;
begin
  aCanvas.Font := Self.Font;
  aCanvas.Font.Color := FTitlesColor;
  aCanvas.Brush.Style := bsClear;
  for i := 0 to 6 do
  begin
    x1 := i*FDayWidth + FBorderSize + aX*FItemWidth;
    y1 := aY*FItemHeight + FCaptionSize;
    x2 := x1+FDayWidth;
    y2 := y1+FTitleSize;
    r := Classes.Rect ( x1 , y1 , x2 , y2 );
    WriteText(aCanvas, r, Uppercase(DefaultFormatSettings.ShortDayNames [((i + 1)mod 7)+1]), FDayAlignment);
  end;

  aCanvas.Pen.Color := clWhite; //FLinesColor;
  aCanvas.Pen.Style := psSolid;
  aCanvas.Pen.Width := 0;
  aCanvas.MoveTo ( aX*FItemWidth + FBorderSize-1, aY*FItemHeight + FCaptionSize + FTitleSize - 1 );
  aCanvas.LineTo ( (aX+1)*FItemWidth - FBorderSize, aY*FItemHeight + FCaptionSize + FTitleSize - 1 );
end;

procedure TmCalendar.Paint_MonthDays(aCanvas: TCanvas; aX, aY: integer);
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
    curDate := GetItemRefDate(aX, aY);
    DecodeDate(curDate, tmpYear, tmpMonth, tmpDay);
    for k := 0 to 5 do
    begin
      y1 := (aY * FItemHeight) + FCaptionSize + FTitleSize + (k * FDayHeight);
      y2 := y1 + FDayHeight;
      for i := 0 to 6 do
      begin
        x1 := (i * FDayWidth) + FBorderSize + (aX * FItemWidth);
        x2 := x1 + FDayWidth;
        r := Classes.Rect (x1, y1, x2, y2);
        if DayOfTheWeek(curDate) = i + 1 then
        begin
          WriteText(aCanvas, r, IntToStr(tmpDay), FDayAlignment);
          curDate := curDate + 1;
          inc(tmpDay);
        end;
        if MonthOf(curDate) <> tmpMonth then
          break;
      end;
    end;
  end;

end;

function TmCalendar.GetItemRefDate(aX, aY: integer): TDateTime;
var
  tmpDay, tmpMonth, tmpYear : word;
  distance: integer;
begin
  if (FItemType = itMonth) then
  begin
    DecodeDate(FStartDate, tmpYear, tmpMonth, tmpDay);
    distance := (aY * FHorizontalItems) + aX;
    Result := StartOfTheMonth(FStartDate);
    if distance > 0 then
      Result := IncMonth(Result, distance);
  end;
end;


destructor TmCalendar.Destroy;
begin
  FDoubleBufferedBitmap.Free;
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

procedure TmCalendar.DrawFlatFrame(aCanvas: TCanvas; const Rect: TRect);
begin
  with aCanvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clBtnFace;
    FillRect ( Rect );
    Pen.Style := psSolid;
    Pen.Width := 0;
    Pen.Color := clBtnHighlight;
    MoveTo ( Rect.Right-1 , Rect.Top );
    LineTo ( Rect.Left , Rect.Top );
    LineTo ( Rect.Left , Rect.Bottom-1 );
    Pen.Color := clBtnShadow;
    LineTo ( Rect.Right-1 , Rect.Bottom-1 );
    LineTo ( Rect.Right-1 , Rect.Top);
  end;
end;

procedure TmCalendar.DoInternalSizeCalculation;
var
  tmpX, tmpY : integer;
begin
  if not Self.HandleAllocated then
    exit;

  FMustCalculate:= false;
  if FItemType = itMonth then
  begin
    FDayWidth:= (Self.ClientWidth - (2* FBorderSize * FHorizontalItems)) div (7 * FHorizontalItems);
    FDayHeight:= (Self.ClientHeight - (FCaptionSize + FTitleSize + 3) * FVerticalItems) div (6 * FVerticalItems);
  end;

  tmpX := ((FDayWidth * 7) + (2* FBorderSize)) * FHorizontalItems;
  tmpY := ((FDayHeight * 6) + FCaptionSize + FTitleSize + 3) * FVerticalItems;

  FItemWidth:= tmpX div FHorizontalItems;
  FItemHeight:= tmpY div FVerticalItems;

  Self.SetBounds(Self.Left, Self.Top, (Self.Width - Self.ClientWidth) + tmpX, (Self.Height - Self.ClientHeight) + tmpY);
end;

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

procedure TmCalendar.SetHorizontalItems(AValue: integer);
begin
  if FHorizontalItems=AValue then Exit;
  if AValue >= 1 then
  begin
    FHorizontalItems:=AValue;
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

procedure TmCalendar.SetTitlesColor(AValue: TColor);
begin
  if FTitlesColor=AValue then Exit;
  FTitlesColor:=AValue;
  Invalidate;
end;

procedure TmCalendar.SetVerticalItems(AValue: integer);
begin
  if FVerticalItems=AValue then Exit;
  if AValue >= 1 then
  begin
    FVerticalItems:=AValue;
    DoInternalSizeCalculation;
    Invalidate;
  end;
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
  itemX, itemY, row, col : integer;
  refDate, clickedDate : TDateTime;
  refYear, refMonth, refDay : word;
  day : integer;
begin
  FMouseMoveData.Clear;
  if not PtInRect(ClientRect, Classes.Point(X, Y)) then
    exit;

  if FItemType = itMonth then
  begin
    singleItemWidth := ClientRect.Width div FHorizontalItems;
    singleItemHeight := ClientRect.Height div FVerticalItems;

    itemX := (X - ClientRect.Left) div singleItemWidth;
    itemY := (Y - ClientRect.Top) div singleItemHeight;
    refDate := GetItemRefDate(itemX, itemY);
    DecodeDate(refDate, refYear, refMonth, refDay);
    {$ifdef debug}
    debugln('Click - Item ref date: ' + DateToStr(refDate));
    {$endif}

    row := (Y - ClientRect.Top - (itemY * FItemHeight) - FCaptionSize - FTitleSize) div FDayHeight;
    col := (X - ClientRect.Left - (itemX * FItemWidth)) div FDayWidth;

    day := col + (row * 7) - DayOfTheWeek(refDate) + 1;
    {$ifdef debug}
    debugln('Click - day: ' + IntToStr(day));
    {$endif}

    if (day >= 1) and (day <= DaysInAMonth(refYear, refMonth)) then
    begin
      clickedDate := EncodeDate(refYear, refMonth, day);
      {$ifdef debug}
      debugln('Click - clicked date: ' + DateToStr(clickedDate));
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
  FSelectedBuckets := TDoubleList.Create;
  FMouseMoveData := TmCalendarMouseMoveData.Create;

  FMustCalculate := true;
  FHorizontalItems:= 1;
  FVerticalItems:= 1;
  FItemType:= itMonth;
  FBorderSize:= 10;
  FCaptionSize:= 16;
  FTitleSize:= 16;
  FStartDate:= Date;
  FDayAlignment:= taCenter;
  FTitlesColor:= clBlack;
  FDaysColor:= clBlack;
  FCaptionsColor := clBlack;
  Self.Color:= clWhite;
end;



end.
