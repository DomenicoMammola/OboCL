unit Biru_FreshFruit;

interface

uses
  Classes, Graphics,
  Biru;

type

  TBiruShape = (bsBanana, bsKiwi, bsApple, bsCherry);

  { TBiruFreshFruit }

  TBiruFreshFruit = class (TBiru)
  strict private
    FShape: TBiruShape;
    procedure SetShape(Value: TBiruShape);
  public
    constructor Create(AOwner: TComponent); override;
    property Shape: TBiruShape read FShape write SetShape default bsBanana;
  end;

implementation


uses
  LResources;

{ TBiruFreshFruit }

procedure TBiruFreshFruit.SetShape(Value: TBiruShape);
begin
  if Value = FShape then
    exit;

  if Value = bsCherry then
    Self.ImageIndex:= 3
  else
  if Value = bsApple then
    Self.ImageIndex := 2
  else
  if Value = bsKiwi then
    Self.ImageIndex := 1
  else
    Self.ImageIndex := 0;
  Refresh;
end;

constructor TBiruFreshFruit.Create(AOwner: TComponent);
var
  tmpBitmap, tmpBitmapMask : TBitmap;
begin
  inherited Create(AOwner);
  FShape := bsBanana;


  FixedBackground.LoadFromLazarusResource('fixedbackground');
  ScrollingBackground.LoadFromLazarusResource('scrollingbackground_sea');

  tmpBitmap := TBitmap.Create;
  tmpBitmap.LoadFromLazarusResource('banana');
  tmpBitmapMask := TBitmap.Create;
  tmpBitmapMask.LoadFromLazarusResource('banana_mask');

  Self.AddBiruImageAndMask(tmpBitmap, tmpBitmapMask);

  tmpBitmap := TBitmap.Create;
  tmpBitmap.LoadFromLazarusResource('kiwi');
  tmpBitmapMask := TBitmap.Create;
  tmpBitmapMask.LoadFromLazarusResource('kiwi_mask');

  Self.AddBiruImageAndMask(tmpBitmap, tmpBitmapMask);

  tmpBitmap := TBitmap.Create;
  tmpBitmap.LoadFromLazarusResource('apple');
  tmpBitmapMask := TBitmap.Create;
  tmpBitmapMask.LoadFromLazarusResource('apple_mask');

  Self.AddBiruImageAndMask(tmpBitmap, tmpBitmapMask);

  tmpBitmap := TBitmap.Create;
  tmpBitmap.LoadFromLazarusResource('cherry');
  tmpBitmapMask := TBitmap.Create;
  tmpBitmapMask.LoadFromLazarusResource('cherry_mask');

  Self.AddBiruImageAndMask(tmpBitmap, tmpBitmapMask);

  Self.FBorderColor:= clBlack;
  Self.FBorder:= 1;
  Self.Init;
end;

initialization
  {$I lcl_biru_freshfruit_images.lrs}

end.
