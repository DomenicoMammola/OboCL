// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
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
  begin
    FBiruImage := FImages.Items[3] as TBitmap;
    FBiruShape := FMasks.Items[3] as TBitmap;
  end
  else
  if Value = bsApple then
  begin
    FBiruImage := FImages.Items[2] as TBitmap;
    FBiruShape := FMasks.Items[2] as TBitmap;
  end
  else
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

constructor TBiruFreshFruit.Create(AOwner: TComponent);
var
  tmpBitmap : TBitmap;
begin
  inherited Create(AOwner);
  FShape := bsBanana;


  FFixedBackground.LoadFromLazarusResource('fixedbackground');
  FScrollingBackground.LoadFromLazarusResource('scrollingbackground_sea');

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

  tmpBitmap := TBitmap.Create;
  FImages.Add(tmpBitmap);
  tmpBitmap.LoadFromLazarusResource('apple');
  tmpBitmap := TBitmap.Create;
  FMasks.Add(tmpBitmap);
  tmpBitmap.LoadFromLazarusResource('apple_mask');

  tmpBitmap := TBitmap.Create;
  FImages.Add(tmpBitmap);
  tmpBitmap.LoadFromLazarusResource('cherry');
  tmpBitmap := TBitmap.Create;
  FMasks.Add(tmpBitmap);
  tmpBitmap.LoadFromLazarusResource('cherry_mask');

  Self.FBorderColor:= clBlack;
  Self.FBorder:= 1;
  Self.Init;
end;

initialization
  {$I lcl_biru_freshfruit_images.lrs}

end.
