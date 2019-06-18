unit mCalendar;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, Controls, Graphics;

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

    // internal properties
    FItemWidth : integer;
    FItemHeight : integer;
    FDayWidth : integer;
    FDayHeight : integer;
    FMustCalculate : boolean;

    procedure Paint_FillBackground;
    procedure Paint_Items;
    procedure Paint_Caption (aX, aY : integer);
    procedure Paint_Titles (aX, aY : integer);
    procedure Paint_Month(aX, aY : integer);
    function GetItemRefDate (aX, aY : integer) : TDateTime;
    procedure DrawFlatFrame ( aCanvas : TCanvas ; const Rect : TRect );
    procedure DoInternalSizeCalculation;
    procedure SetBorderSize(AValue: integer);
    procedure SetDayAlignment(AValue: TAlignment);
    procedure SetDaysColor(AValue: TColor);
    procedure SetHorizontalItems(AValue: integer);
    procedure SetItemType(AValue: TmCalendarItemType);
    procedure SetTitlesColor(AValue: TColor);
    procedure SetVerticalItems(AValue: integer);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property VerticalItems : integer read FVerticalItems write SetVerticalItems default 1;
    property HorizontalItems : integer read FHorizontalItems write SetHorizontalItems default 1;
    property ItemType : TmCalendarItemType read FItemType write SetItemType;
    property BorderSize : integer read FBorderSize write SetBorderSize;
    property DayAlignment : TAlignment read FDayAlignment write SetDayAlignment;
    property TitlesColor : TColor read FTitlesColor write SetTitlesColor;
    property DaysColor : TColor read FDaysColor write SetDaysColor;
  end;

implementation

uses
  SysUtils, DateUtils
  {$ifdef windows}, Windows{$endif}
  , mGraphicsUtility;

{ TmCalendar }

procedure TmCalendar.Paint_FillBackground;
begin
  Self.Canvas.Brush.Color:= Self.Color;
  Self.Canvas.FillRect(Self.ClientRect);
end;

procedure TmCalendar.Paint_Items;
var
  x, y : integer;
begin
  for y := 0 to FVerticalItems - 1 do
  begin
    for x := 0 to FHorizontalItems - 1 do
    begin
      Paint_Caption (x, y);
      Paint_Titles (x, y);
      Paint_Month (x, y);
      //Paint_Dividers(x, y);
    end;
  end;

end;

procedure TmCalendar.Paint_Caption(aX, aY: integer);
var
  tmpRect : TRect;
  str : String;
  tmpRefDate : TDateTime;
  tmpYear, tmpMonth, tmpDay : word;
  w, h : integer;
begin
  Canvas.Pen.Color := cl3DDkShadow;
  Canvas.Pen.Style := psSolid;
  tmpRect := Classes.Rect (aX * FItemWidth, aY * FItemHeight, (aX + 1) * FItemWidth, aY * FItemHeight + FCaptionSize);
  Self.DrawFlatFrame(Self.Canvas, tmpRect);
  Self.Canvas.MoveTo(tmpRect.Left, tmpRect.Top -1);
  Self.Canvas.LineTo(tmpRect.Right, tmpRect.Top -1);

  str := '';
  if FItemType = itMonth then
  begin
    tmpRefDate:= GetItemRefDate(aX, aY);
    DecodeDate(tmpRefDate, tmpYear, tmpMonth, tmpDay);
    str := DefaultFormatSettings.LongMonthNames[tmpMonth] + ' ' + IntToStr(tmpYear);
  end;
  Self.Canvas.Font := Self.Font;
  Self.Canvas.Font.Color := clBlack; //FCaptionsColor;
  Self.Canvas.Brush.Style := bsClear;
  Canvas.GetTextSize(str, w, h);
  Canvas.TextOut(tmpRect.Left + ((FItemWidth - w) div 2), tmpRect.Top, str);

  (*
  DrawTextEx ( Handle , PChar(str) , Length(str) ,tmpRect , DT_CENTER or
                 DT_END_ELLIPSIS or DT_RTLREADING or DT_VCENTER or
                 DT_SINGLELINE , nil );*)
end;

procedure TmCalendar.Paint_Titles(aX, aY: integer);
var
  x1 , y1 , x2 , y2 , i : integer;
  r : TRect;
begin
  Canvas.Font := Self.Font;
  Canvas.Font.Color := FTitlesColor;
  Canvas.Brush.Style := bsClear;
  for i := 0 to 6 do
  begin
    x1 := i*FDayWidth + FBorderSize + aX*FItemWidth;
    y1 := aY*FItemHeight + FCaptionSize;
    x2 := x1+FDayWidth;
    y2 := y1+FTitleSize;
    r := Classes.Rect ( x1 , y1 , x2 , y2 );
    WriteText(Canvas, r, Uppercase(DefaultFormatSettings.ShortDayNames [((i + 1)mod 7)+1]), FDayAlignment);
  end;

  Canvas.Pen.Color := clWhite; //FLinesColor;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Width := 0;
  Canvas.MoveTo ( aX*FItemWidth + FBorderSize-1 ,
           aY*FItemHeight + FCaptionSize + FTitleSize - 1 );
  Canvas.LineTo ( (aX+1)*FItemWidth - FBorderSize ,
           aY*FItemHeight + FCaptionSize + FTitleSize - 1 );
end;

procedure TmCalendar.Paint_Month(aX, aY: integer);
var
  i, k : integer;
  x1 , y1 , x2 , y2 : integer;
  r : TRect;
  tmpYear, tmpMonth, tmpDay : word;
  curDate : TDateTime;
begin
  Canvas.Font := Self.Font;
  Canvas.Font.Color := FDaysColor;
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
          WriteText(Canvas, r, IntToStr(tmpDay), FDayAlignment);
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
  inherited Destroy;
end;

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

procedure TmCalendar.SetDayAlignment(AValue: TAlignment);
begin
  if FDayAlignment=AValue then Exit;
  FDayAlignment:=AValue;
  Self.Invalidate;
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

procedure TmCalendar.Paint;
begin
  //if FMustCalculate then
    DoInternalSizeCalculation;
  Paint_FillBackground;
  Paint_Items;

  //PaintPrevMonthButton;
  //PaintNextMonthButton;
end;

constructor TmCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
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
end;



end.
