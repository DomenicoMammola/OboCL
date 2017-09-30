unit Biru;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}
interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, ExtCtrls, contnrs;

type

  TBiruAnimationType = (tatBouncing, tatSizing, tatScrolling);
  TBiruShape = (bsBanana, bsKiwi);

  { TBiru }

  TBiru = class(TGraphicControl)
  strict private
    const SQUARE_LENGTH = 120;
  strict private
    FBiruShape: TBitmap;
    FBiruImage: TBitmap;
    DefImage: TBitmap;
    FFixedBackground: TBitmap;
    FScrollingBackground: TBitmap;
    FAnimateTimer: TTimer;
    XPos: integer;
    YPos: integer;
    ShiftX: integer;
    ShiftY: integer;
    RollingX: integer;
    BiruDefaultX: integer;
    BiruDefaultY: integer;
    FStretchingDirection: integer;
    StretchingX: integer;
    StretchingY: integer;
    FPlayingAnimation: boolean;
    FAnimation: TBiruAnimationType;
    FShape: TBiruShape;
    FImages : TObjectList;
    FMasks : TObjectList;
    procedure SetAnimation(Value: TBiruAnimationType);
    procedure SetShape(Value: TBiruShape);
    procedure FAnimateTimerTimer(Sender: TObject);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PlayAnimation;
    procedure StopAnimation;
  published
    property Animation: TBiruAnimationType read FAnimation write SetAnimation default tatBouncing;
    property Shape: TBiruShape read FShape write SetShape default bsBanana;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Visible;
    property OnClick;
    property OnDblClick;
  end;


implementation

uses
  LResources;

{$R biru.res}

procedure TBiru.Paint;
var
  Temp: TBitmap;
begin
  Temp := TBitmap.Create;
  try
    if (not FPlayingAnimation) then
    begin
      Temp.Width := SQUARE_LENGTH;
      Temp.Height := SQUARE_LENGTH;
      BitBlt(Temp.Canvas.Handle, 0, 0, SQUARE_LENGTH, SQUARE_LENGTH,
        FFixedBackground.Canvas.Handle, 0, 0, SRCCOPY);
      BitBlt(Temp.Canvas.Handle, BiruDefaultX,
        BiruDefaultY, FBiruImage.Width, FBiruImage.Height, FBiruShape.Canvas.Handle, 0, 0, SRCAND);
      BitBlt(Temp.Canvas.Handle, BiruDefaultX,
        BiruDefaultY, FBiruImage.Width, FBiruImage.Height, FBiruImage.Canvas.Handle, 0, 0, SRCPAINT);
      BitBlt(DefImage.Canvas.Handle, 0, 0, FBiruImage.Width,
        FBiruImage.Height, Temp.Canvas.Handle, 0, 0, SRCCOPY);
      Canvas.Draw(0, 0, Temp);
    end;
  finally
    Temp.Free;
  end;
end;

procedure TBiru.SetShape(Value: TBiruShape);
begin
  if Value = FShape then
    exit;

  if Value = bsKiwi then
  begin
    FBiruImage := FImages.Items[1] as TBitmap;
    FBiruShape := FMasks.Items[1] as TBitmap;
  end
  else
  begin
    FBiruImage := FImages.Items[0] as TBitmap;
    FBiruShape := FMasks.Items[0] as TBitmap;
  end;
  Refresh;
end;

procedure TBiru.SetAnimation(Value: TBiruAnimationType);
begin
  if (Value <> FAnimation) and (not FPlayingAnimation) then
  begin
    FAnimation := Value;
    Invalidate;
  end;
end;

procedure TBiru.FAnimateTimerTimer(Sender: TObject);
var
  Risultato: boolean;
  R: TRect;
  StrX, StrY: integer;
  StretchBiru: TBitmap;
  StretchShape: TBitmap;
begin
  case FAnimation of
    tatBouncing:
    begin
      if ((XPos + FBiruImage.Width) = FFixedBackground.Width) then
        ShiftX := -1
      else
      if (XPos = 0) then
        ShiftX := 1;
      if (YPos = 0) then
        ShiftY := 1
      else
      if ((YPos + FBiruImage.Height) = FFixedBackground.Height) then
        ShiftY := -1;
      XPos := XPos + ShiftX;
      YPos := YPos + ShiftY;
      risultato := BitBlt(DefImage.Canvas.Handle, 0, 0, FFixedBackground.Width,
        FFixedBackground.Height, FFixedBackground.Canvas.Handle, 0, 0, SRCCOPY);
      risultato := BitBlt(DefImage.Canvas.Handle, XPos, YPos, FBiruImage.Width,
        FBiruImage.Height, FBiruShape.Canvas.Handle, 0, 0, SRCAND);
      risultato := BitBlt(DefImage.Canvas.Handle, XPos, YPos, FBiruImage.Width,
        FBiruImage.Height, FBiruImage.Canvas.Handle, 0, 0, SRCPAINT);
      Canvas.Draw(0, 0, DefImage);

    end;
    tatScrolling:
    begin
      risultato := BitBlt(DefImage.Canvas.Handle, RollingX, 0,
        (FFixedBackground.Width - RollingX), FFixedBackground.Height, FScrollingBackground.Canvas.Handle, 0, 0, SRCCOPY);
      if (RollingX > 0) then
        risultato := BitBlt(DefImage.Canvas.Handle, 0, 0, RollingX,
          FFixedBackground.Height, FScrollingBackground.Canvas.Handle, (FFixedBackground.Width - RollingX), 0, SRCCOPY);
      risultato := BitBlt(DefImage.Canvas.Handle, BiruDefaultX,
        BiruDefaultY, FBiruImage.Width, FBiruImage.Height, FBiruShape.Canvas.Handle, 0, 0, SRCAND);
      risultato := BitBlt(DefImage.Canvas.Handle, BiruDefaultX,
        BiruDefaultY, FBiruImage.Width, FBiruImage.Height, FBiruImage.Canvas.Handle, 0, 0, SRCPAINT);
      Canvas.Draw(0, 0, DefImage);
      Inc(RollingX);
      if (RollingX > DefImage.Width) then
        RollingX := 0;
    end;
    tatSizing:
    begin
      StretchingX := StretchingX + FStretchingDirection;
      StretchingY := StretchingY + FStretchingDirection;
      R := Rect(0, 0, StretchingX, StretchingY);
      StretchBiru := TBitmap.Create;
      StretchShape := TBitmap.Create;
      try
        StretchBiru.Width := StretchingX;
        StretchBiru.Height := StretchingY;
        StretchShape.Width := StretchingX;
        StretchShape.Height := StretchingY;
        StretchBiru.Canvas.StretchDraw(R, FBiruImage);
        StretchShape.Canvas.StretchDraw(R, FBiruShape);
        StrX := (FFixedBackground.Width - StretchingX) div 2;
        StrY := (FFixedBackground.Height - StretchingY) div 2;

        risultato := BitBlt(DefImage.Canvas.Handle, 0, 0, FFixedBackground.Width,
          FFixedBackground.Height, FFixedBackground.Canvas.Handle, 0, 0, SRCCOPY);
        risultato := BitBlt(DefImage.Canvas.Handle, StrX, StrY,
          StretchingX, StretchingY, StretchShape.Canvas.Handle, 0, 0, SRCAND);
        risultato := BitBlt(DefImage.Canvas.Handle, StrX, StrY,
          StretchingX, StretchingY, StretchBiru.Canvas.Handle, 0, 0, SRCPAINT);
        Canvas.Draw(0, 0, DefImage);
        if ((StretchingX = 2) or (StretchingY = 2)) then
          FStretchingDirection := 1;
        if ((StretchingX = FFixedBackground.Width) or (StretchingY = FFixedBackground.Height)) then
          FStretchingDirection := -1;
      finally
        StretchBiru.Free;
        StretchShape.Free;
      end;
    end;
  end;
end;

constructor TBiru.Create(AOwner: TComponent);
var
  R: TRect;
  tmpBitmap: TBitmap;
  risultato: boolean;
begin
  inherited Create(AOwner);
  FImages:= TObjectList.Create(true);
  FMasks := TObjectList.Create(true);
  { default values }
  FShape := bsBanana;
  XPos := 0;
  YPos := 0;
  ShiftX := 1;
  ShiftY := 1;
  RollingX := 0;
  FAnimation := tatBouncing;
  FStretchingDirection := -1;
  FPlayingAnimation := False;
  Self.Height:= SQUARE_LENGTH;
  Self.Width:= SQUARE_LENGTH;
  DefImage := TBitmap.Create;
  DefImage.Width := SQUARE_LENGTH;
  DefImage.Height := SQUARE_LENGTH;
  R := Rect(0, 0, SQUARE_LENGTH, SQUARE_LENGTH);
  DefImage.Canvas.Brush.Color := clWhite;
  DefImage.Canvas.FillRect(R);

  FFixedBackground := TBitmap.Create;
  FFixedBackground.LoadFromLazarusResource('fixedbackground');
  FScrollingBackground := TBitmap.Create;
  FScrollingBackground.LoadFromLazarusResource('scrollingbackground');
  tmpBitmap := TBitmap.Create;
  FImages.Add(tmpBitmap);
  tmpBitmap.LoadFromLazarusResource('banana');
  tmpBitmap := TBitmap.Create;
  FMasks.Add(tmpBitmap);
  tmpBitmap.LoadFromLazarusResource('banana_mask');

  tmpBitmap := TBitmap.Create;
  FImages.Add(tmpBitmap);
  tmpBitmap.LoadFromLazarusResource('kiwi');
  tmpBitmap := TBitmap.Create;
  FMasks.Add(tmpBitmap);
  tmpBitmap.LoadFromLazarusResource('kiwi_mask');

  FBiruImage := FImages.Items[0] as TBitmap;
  FBiruShape := FMasks.Items[0] as TBitmap;
  StretchingX := FBiruImage.Width;
  StretchingY := FBiruImage.Height;
  BiruDefaultX := (FFixedBackground.Width - FBiruImage.Width) div 2;
  BiruDefaultY := (FFixedBackground.Height - FBiruImage.Height) div 2;

  FAnimateTimer := TTimer.Create(self);
  FAnimateTimer.Enabled := False;
  FAnimateTimer.Interval := 5;
  FAnimateTimer.OnTimer := Self.FAnimateTimerTimer;
end;

destructor TBiru.Destroy;
begin
  DefImage.Free;
  FFixedBackground.Free;
  FScrollingBackground.Free;
  FImages.Free;
  FMasks.Free;

  inherited Destroy;

end;

procedure TBiru.PlayAnimation;
begin
  FPlayingAnimation := True;
  FAnimateTimer.Enabled := True;
end;

procedure TBiru.StopAnimation;
begin
  FPlayingAnimation := False;
  FAnimateTimer.Enabled := False;
  Refresh;
end;

initialization
  {$I lcl_biru_images.lrs}

end.
