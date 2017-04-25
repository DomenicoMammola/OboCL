unit Biru;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}
interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, ExtCtrls;

type

  TBiruAnimationType = (tatBouncing, tatSizing, tatScrolling);

  { TBiru }

  TBiru = class(TGraphicControl)
  private
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
    MFace: integer;
    procedure SetAnimation(Value: TBiruAnimationType);
    procedure SetFace(Value: integer);
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
    property Face: integer read MFace write SetFace default 1;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Visible;
    property OnClick;
    property OnDblClick;
  end;


implementation

{$R biru.res}

procedure TBiru.Paint;
var
  Temp: TBitmap;
begin
  Temp := TBitmap.Create;
  try
    if (not FPlayingAnimation) then
    begin
      Temp.Width := 80;
      Temp.Height := 80;
      BitBlt(Temp.Canvas.Handle, 0, 0, 80, 80,
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

procedure TBiru.SetFace(Value: integer);
begin
  if ((Value <= 2) and (Value > 0) and (Value <> MFace)) then
  begin
    MFace := Value;
    FreeAndNil(FBiruImage);
    FreeAndNil(FBiruShape);
    if (MFace = 1) then
    begin
      FBiruImage := TBitmap.Create;
      FBiruShape := TBitmap.Create;
      FBiruImage.LoadFromResourceName(hInstance, 'BIRU1');
      FBiruShape.LoadFromResourceName(hInstance, 'SAGOMA1');
    end
    else
    begin
      FBiruImage := TBitmap.Create;
      FBiruShape := TBitmap.Create;
      FBiruImage.LoadFromResourceName(hInstance, 'BIRU2');
      FBiruShape.LoadFromResourceName(hInstance, 'SAGOMA2');
    end;
    Refresh;
  end;
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
  Temp: TBitmap;
  risultato: boolean;
begin
  inherited Create(AOwner);
  { default values }
  MFace := 1;
  XPos := 0;
  YPos := 0;
  ShiftX := 1;
  ShiftY := 1;
  RollingX := 0;
  FAnimation := tatBouncing;
  FStretchingDirection := -1;
  FPlayingAnimation := False;
  Self.Height:= 80;
  Self.Width:= 80;
  //HintMessageList := TStrings.Create;
  DefImage := TBitmap.Create;
  DefImage.Width := 80;
  DefImage.Height := 80;
  R := Rect(0, 0, 80, 80);
  DefImage.Canvas.Brush.Color := clWhite;
  DefImage.Canvas.FillRect(R);
  FFixedBackground := TBitmap.Create;
  FFixedBackground.LoadFromResourceName(hInstance, 'SFONDO1');
  FScrollingBackground := TBitmap.Create;
  //FScrollingBackground.LoadFromFile ('FFixedBackground5.bmp');
  FScrollingBackground.LoadFromResourceName(hInstance, 'SFONDO2');
  FBiruImage := TBitmap.Create;
  FBiruImage.LoadFromResourceName(hInstance, 'BIRU1');
  StretchingX := FBiruImage.Width;
  StretchingY := FBiruImage.Height;
  FBiruShape := TBitmap.Create;
  FBiruShape.LoadFromResourceName(hInstance, 'SAGOMA1');
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
  FBiruShape.Free;
  FBiruImage.Free;

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

end.
