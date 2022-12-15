// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
//
// It needs BGRABITMAP LIBRARY:
// https://github.com/bgrabitmap/bgrabitmap
//
unit BiruBgra;

{$ifdef fpc}
  {$mode delphi}
{$endif}
interface

uses
  {$ifndef fpc}{$ifdef windows}Windows,{$endif}{$endif}
  {$ifdef fpc}LCLIntf, LCLType,{$endif}
  SysUtils, Classes, Graphics, Controls, ExtCtrls, contnrs,
  BGRABitmap;

type

  TBiruAnimationType = (tatBouncing, tatSizing, tatScrolling, tatPsychedelic);

  { TBiru }
  TBiru = class (TGraphicControl)
  strict private
    FDefImage: TBGRABitmap;
    FAnimateTimer: TTimer;
    FImageIndex: integer;
    FSpeed: integer;
    XPos: integer;
    YPos: integer;
    ShiftX: integer;
    ShiftY: integer;
    FRollingX: integer;
    BiruDefaultX: integer;
    BiruDefaultY: integer;
    FStretchingDirection: integer;
    FStretchingX: integer;
    FStretchingY: integer;
    FPlayingAnimation: boolean;
    FAnimation: TBiruAnimationType;
    FFixedBackground: TBitmap;
    FDefFixedBackground : TBGRABitmap;
    FScrollingBackground: TBitmap;
    FDefScrollingBackground: TBGRABitmap;
    FImages : TObjectList;
    FBiruImage: TBGRABitmap;
    FInitDone : boolean;

    procedure SetAnimation(Value: TBiruAnimationType);
    procedure FAnimateTimerTimer(Sender: TObject);
    procedure SetImageIndex(AValue: integer);
    procedure SetSpeed(AValue: integer);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PlayAnimation;
    procedure StopAnimation;
    procedure AddBiruImage(const aImage: TPortableNetworkGraphic);
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

uses
  BGRABitmapTypes, BGRAGraphics;


procedure TBiru.Paint;
var
  Temp: TBGRABitmap;
begin
  if (not FPlayingAnimation) then
  begin
    Temp := TBGRABitmap.Create(FDefImage.Width, FDefImage.Height);
    try
      Temp.Canvas.Draw(0, 0, FFixedBackground);
      Temp.CanvasBGRA.Draw(BiruDefaultX, BiruDefaultY, FBiruImage);
      Temp.Draw(Canvas, 0, 0);
    finally
      Temp.Free;
    end;
  end;
end;

procedure TBiru.Init;
var
  R: TRect;
begin
  Self.Height:= FFixedBackground.Height;
  Self.Width:= FFixedBackground.Width;

  Self.ImageIndex:= 0;

  FStretchingX := FBiruImage.Width;
  FStretchingY := FBiruImage.Height;
  BiruDefaultX := (FFixedBackground.Width - FBiruImage.Width) div 2;
  BiruDefaultY := (FFixedBackground.Height - FBiruImage.Height) div 2;
  XPos := (FFixedBackground.Width - FBiruImage.Width) div 2;

  FDefImage := TBGRABitmap.Create(FFixedBackground.Width, FFixedBackground.Height, BGRAWhite);

  FDefFixedBackground := TBGRABitmap.Create(FFixedBackground);
  FDefScrollingBackground := TBGRABitmap.Create(FScrollingBackground);

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
  RS, RD: TRect;
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
      FDefImage.CanvasBGRA.Draw(0, 0, FDefFixedBackground);
      FDefImage.CanvasBGRA.Draw(XPos, YPos, FBiruImage);
      FDefImage.Draw(Canvas, 0, 0);
    end;
    tatScrolling:
    begin
      RS.Top:= 0;
      RS.Bottom:= FDefScrollingBackground.Height;
      RS.Left := FRollingX;
      RS.Right := FDefScrollingBackground.Width;

      RD.Top := RS.Top;
      RD.Bottom := RS.Bottom;
      RD.Left:= 0;
      RD.Right:= FDefScrollingBackground.Width - FRollingX;

      FDefImage.CanvasBGRA.CopyRect(RD, FDefScrollingBackground, RS);
      if (FRollingX > 0) then
      begin
        RS.Left := 0;
        RS.Right:= FRollingX - 1;
        RD.Left := FDefScrollingBackground.Width - FRollingX + 1;
        RD.Right:= FDefScrollingBackground.Width;

        FDefImage.CanvasBGRA.CopyRect(RD, FDefScrollingBackground, RS);
      end;
      FDefImage.CanvasBGRA.Draw(BiruDefaultX, BiruDefaultY, FBiruImage);
      FDefImage.Draw(Canvas, 0, 0);

      (*
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
      *)
      Inc(FRollingX);
      if (FRollingX > FFixedBackground.Width) then
        FRollingX := 0;
    end;
    tatPsychedelic:
    begin
      RS.Top:= 0;
      RS.Bottom:= FDefScrollingBackground.Height;
      RS.Left := FRollingX;
      RS.Right := FDefScrollingBackground.Width - FRollingX;

      RD.Top := RS.Top;
      RD.Bottom := RS.Bottom;
      RD.Left:= 0;
      RD.Right:= FDefScrollingBackground.Width - FRollingX;

      FDefImage.CanvasBGRA.CopyRect(RD, FDefScrollingBackground, RS);
      if (FRollingX > 0) then
      begin
        RS.Left := 0;
        RS.Right:= FRollingX - 1;
        RD.Left := FRollingX + 1;
        RD.Right:= FDefScrollingBackground.Width;

        FDefImage.CanvasBGRA.CopyRect(RD, FDefScrollingBackground, RS);
      end;
      FDefImage.CanvasBGRA.Draw(BiruDefaultX, BiruDefaultY, FBiruImage);
      FDefImage.Draw(Canvas, 0, 0);
      Inc(FRollingX);
      if (FRollingX > FFixedBackground.Width) then
        FRollingX := 0;
    end;
    tatSizing:
    begin
      FStretchingX := FStretchingX + FStretchingDirection;
      FStretchingY := FStretchingY + FStretchingDirection;

      FDefImage.CanvasBGRA.Draw(0, 0, FDefFixedBackground);
      RD.Top:= (FDefFixedBackground.Height - FStretchingY) div 2;
      RD.Bottom := RD.Top + FStretchingY;
      RD.Left:= (FDefFixedBackground.Width - FStretchingX) div 2;
      RD.Right:= RD.Left + FStretchingX;

      FDefImage.CanvasBGRA.StretchDraw(RD, FBiruImage, false, false);

      if ((FStretchingX = 2) or (FStretchingY = 2)) then
        FStretchingDirection := 1;
      if ((FStretchingX = FFixedBackground.Width) or (FStretchingY = FFixedBackground.Height)) then
        FStretchingDirection := -1;
      FDefImage.Draw(Canvas, 0, 0);
    end;
  end;
end;

procedure TBiru.SetImageIndex(AValue: integer);
var
  str : TMemoryStream;
begin
  if FImageIndex=AValue then Exit;
  FImageIndex:=AValue;
  FBiruImage := TBGRABitmap.Create((FImages.Items[FImageIndex] as TPortableNetworkGraphic).Width, (FImages.Items[0] as TPortableNetworkGraphic).Height);
  str := TMemoryStream.Create;
  try
    (FImages.Items[FImageIndex] as TPortableNetworkGraphic).SaveToStream(str);
    str.Position:= 0;
    FBiruImage.LoadFromStream(str);
  finally
    str.Free;
  end;
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
  FImageIndex := -1;
  FImages:= TObjectList.Create(true);
  { default values }
  XPos := 0;
  YPos := 0;
  ShiftX := 1;
  ShiftY := 1;

  FAnimation := tatBouncing;
  FStretchingDirection := -1;
  FPlayingAnimation := False;
  FRollingX := 0;
  FDefImage := nil;
  FDefFixedBackground := nil;
  FDefScrollingBackground := nil;

  FFixedBackground := TBitmap.Create;
  FScrollingBackground := TBitmap.Create;

  FAnimateTimer := TTimer.Create(self);
  FAnimateTimer.Enabled := False;
  FSpeed := 5;
  FAnimateTimer.Interval := FSpeed;
  FAnimateTimer.OnTimer := Self.FAnimateTimerTimer;
end;

destructor TBiru.Destroy;
begin
  FreeAndNil(FDefImage);
  FreeAndNil(FFixedBackground);
  FreeAndNil(FScrollingBackground);
  FreeAndNil(FDefFixedBackground);
  FreeAndNil(FDefScrollingBackground);
  FreeAndNil(FBiruImage);
  FImages.Free;

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

procedure TBiru.AddBiruImage(const aImage: TPortableNetworkGraphic);
begin
  FImages.Add(aImage);
end;


end.
