// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit Biru;

{$ifdef fpc}
  {$mode delphi}
{$endif}
interface

uses
  {$ifndef fpc}{$ifdef windows}Windows,{$endif}{$endif}
  {$ifdef fpc}LCLIntf, LCLType,{$endif}
  SysUtils, Classes, Graphics, Controls, ExtCtrls, contnrs;

type

  TBiruAnimationType = (tatBouncing, tatSizing, tatScrolling);

  { TBiru }

  TBiru = class (TGraphicControl)
//  strict private
//    const SQUARE_LENGTH = 120;
  strict private
    DefImage: TBitmap;
    FAnimateTimer: TTimer;
    FImageIndex: integer;
    FSpeed: integer;
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
    FFixedBackground: TBitmap;
    FScrollingBackground: TBitmap;
    FImages : TObjectList;
    FMasks : TObjectList;
    FBiruMask: TBitmap;
    FBiruImage: TBitmap;
    FBiruImageIndex : integer;
    FInitDone : boolean;

    procedure SetAnimation(Value: TBiruAnimationType);
    procedure FAnimateTimerTimer(Sender: TObject);
    procedure SetImageIndex(AValue: integer);
    procedure SetSpeed(AValue: integer);
  protected
    FBorder: integer;
    FBorderColor : TColor;

    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PlayAnimation;
    procedure StopAnimation;
    procedure AddBiruImageAndMask(const aImage, aMask : TBitmap);
    procedure Init;
  published
    property Animation: TBiruAnimationType read FAnimation write SetAnimation default tatBouncing;
    property Speed: integer read FSpeed write SetSpeed;
    property FixedBackground : TBitmap read FFixedBackground;
    property ScrollingBackground : TBitmap read FScrollingBackground;
    property ImageIndex : integer read FImageIndex write SetImageIndex;
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
      Temp.Width := DefImage.Width;
      Temp.Height := DefImage.Height;
      if FBorder > 0 then
      begin
        Temp.Canvas.Pen.Color:= FBorderColor;
        Temp.Canvas.Pen.Width:= FBorder;
        Temp.Canvas.Brush.Color:= clWhite;
        Temp.Canvas.Rectangle(0, 0, DefImage.Width - 1, DefImage.Height - 1);
      end;

      BitBlt(Temp.Canvas.Handle, FBorder, FBorder, FFixedBackground.Width, FFixedBackground.Height, FFixedBackground.Canvas.Handle, 0, 0, SRCCOPY);
      BitBlt(Temp.Canvas.Handle, BiruDefaultX, BiruDefaultY, FBiruImage.Width, FBiruImage.Height, FBiruMask.Canvas.Handle, 0, 0, SRCAND);
      BitBlt(Temp.Canvas.Handle, BiruDefaultX, BiruDefaultY, FBiruImage.Width, FBiruImage.Height, FBiruImage.Canvas.Handle, 0, 0, SRCPAINT);
      Canvas.Draw(0, 0, Temp);
    end;
  finally
    Temp.Free;
  end;
end;

procedure TBiru.Init;
var
  R: TRect;
begin
  Self.Height:= FFixedBackground.Height + (FBorder * 2);
  Self.Width:= FFixedBackground.Width + (FBorder * 2);

  FBiruImage := FImages.Items[0] as TBitmap;
  FBiruMask := FMasks.Items[0] as TBitmap;
  StretchingX := FBiruImage.Width;
  StretchingY := FBiruImage.Height;
  BiruDefaultX := (FFixedBackground.Width - FBiruImage.Width) div 2;
  BiruDefaultY := (FFixedBackground.Height - FBiruImage.Height) div 2;
  XPos := (FFixedBackground.Width - FBiruImage.Width) div 2;
  DefImage.Width := FFixedBackground.Width + (FBorder  * 2);
  DefImage.Height := FFixedBackground.Height + (FBorder * 2);
  R := Rect(0, 0, DefImage.Width, DefImage.Height);
  DefImage.Canvas.Brush.Color := clWhite;
  DefImage.Canvas.FillRect(R);
  if FBorder > 0 then
  begin
    DefImage.Canvas.Pen.Color:= FBorderColor;
    DefImage.Canvas.Pen.Width:= FBorder;
    DefImage.Canvas.Brush.Color:= clWhite;
    DefImage.Canvas.Rectangle(0, 0, DefImage.Width, DefImage.Height);
  end;
  FInitDone:= true;
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
      if (XPos = FBorder) then
        ShiftX := 1;
      if (YPos = FBorder) then
        ShiftY := 1
      else
      if ((YPos + FBiruImage.Height) = FFixedBackground.Height) then
        ShiftY := -1;
      XPos := XPos + ShiftX;
      YPos := YPos + ShiftY;
      BitBlt(DefImage.Canvas.Handle, FBorder, FBorder, FFixedBackground.Width, FFixedBackground.Height, FFixedBackground.Canvas.Handle, 0, 0, SRCCOPY);
      BitBlt(DefImage.Canvas.Handle, XPos, YPos, FBiruImage.Width, FBiruImage.Height, FBiruMask.Canvas.Handle, 0, 0, SRCAND);
      BitBlt(DefImage.Canvas.Handle, XPos, YPos, FBiruImage.Width, FBiruImage.Height, FBiruImage.Canvas.Handle, 0, 0, SRCPAINT);
      Canvas.Draw(0, 0, DefImage);
    end;
    tatScrolling:
    begin
      BitBlt(DefImage.Canvas.Handle, RollingX, FBorder,
        (FFixedBackground.Width - RollingX), FFixedBackground.Height, FScrollingBackground.Canvas.Handle, 0, 0, SRCCOPY);
      if (RollingX > 0) then
        BitBlt(DefImage.Canvas.Handle, FBorder, FBorder, RollingX,
          FFixedBackground.Height, FScrollingBackground.Canvas.Handle, (FFixedBackground.Width - RollingX), 0, SRCCOPY);
      BitBlt(DefImage.Canvas.Handle, BiruDefaultX,
        BiruDefaultY, FBiruImage.Width, FBiruImage.Height, FBiruMask.Canvas.Handle, 0, 0, SRCAND);
      BitBlt(DefImage.Canvas.Handle, BiruDefaultX,
        BiruDefaultY, FBiruImage.Width, FBiruImage.Height, FBiruImage.Canvas.Handle, 0, 0, SRCPAINT);
      Canvas.Draw(0, 0, DefImage);
      Inc(RollingX);
      if (RollingX > FFixedBackground.Width) then
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
        StretchShape.Canvas.StretchDraw(R, FBiruMask);
        StrX := (FFixedBackground.Width - StretchingX) div 2;
        StrY := (FFixedBackground.Height - StretchingY) div 2;

        BitBlt(DefImage.Canvas.Handle, FBorder, FBorder, FFixedBackground.Width, FFixedBackground.Height, FFixedBackground.Canvas.Handle, 0, 0, SRCCOPY);
        BitBlt(DefImage.Canvas.Handle, StrX, StrY, StretchingX, StretchingY, StretchShape.Canvas.Handle, 0, 0, SRCAND);
        BitBlt(DefImage.Canvas.Handle, StrX, StrY, StretchingX, StretchingY, StretchBiru.Canvas.Handle, 0, 0, SRCPAINT);
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

procedure TBiru.SetImageIndex(AValue: integer);
begin
  if FImageIndex=AValue then Exit;
  FImageIndex:=AValue;
  FBiruImage := FImages.Items[FImageIndex] as TBitmap;
  FBiruMask := FMasks.Items[FImageIndex] as TBitmap;
end;

procedure TBiru.SetSpeed(AValue: integer);
begin
  if FSpeed=AValue then Exit;
  FSpeed:=AValue;
  FAnimateTimer.Interval:= FSpeed;
end;

constructor TBiru.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInitDone:= false;
  FImages:= TObjectList.Create(true);
  FMasks := TObjectList.Create(true);
  { default values }
  XPos := 0;
  YPos := 0;
  ShiftX := 1;
  ShiftY := 1;
  RollingX := 0;
  FAnimation := tatBouncing;
  FStretchingDirection := -1;
  FPlayingAnimation := False;
  FBorder := 1;
  FBorderColor := clBlack;
  DefImage := TBitmap.Create;

  FFixedBackground := TBitmap.Create;
  FScrollingBackground := TBitmap.Create;

  FAnimateTimer := TTimer.Create(self);
  FAnimateTimer.Enabled := False;
  FSpeed := 5;
  FAnimateTimer.Interval := FSpeed;
  FAnimateTimer.OnTimer := Self.FAnimateTimerTimer;
  FBiruImageIndex:= 0;
end;

destructor TBiru.Destroy;
begin
  FreeAndNil(DefImage);
  FreeAndNil(FFixedBackground);
  FreeAndNil(FScrollingBackground);
  FImages.Free;
  FMasks.Free;

  inherited Destroy;

end;

procedure TBiru.PlayAnimation;
begin
  if not FInitDone then
    Self.Init;
  FPlayingAnimation := True;
  FAnimateTimer.Enabled := True;
end;

procedure TBiru.StopAnimation;
begin
  FPlayingAnimation := False;
  FAnimateTimer.Enabled := False;
  Refresh;
end;

procedure TBiru.AddBiruImageAndMask(const aImage, aMask: TBitmap);
begin
  FImages.Add(aImage);
  FMasks.Add(aMask);
end;


end.
