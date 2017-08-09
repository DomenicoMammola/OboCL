// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mDBGrid;

interface

uses
  Classes, DBGrids, StdCtrls, Graphics;

type

  { TmDBGrid }

  TmDBGrid = class(TDBGrid)
  strict private
    FCustomUncheckedBitmap : TBitmap;
    FCustomCheckedBitmap : TBitmap;
    FCustomGrayedBitmap : TBitmap;
  protected
    function  GetImageForCheckBox(const aCol,aRow: Integer; CheckBoxView: TCheckBoxState): TBitmap; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

//    property OnSelection;
  end;

implementation

uses
  LResources;

{ TmDBGrid }

function TmDBGrid.GetImageForCheckBox(const aCol, aRow: Integer; CheckBoxView: TCheckBoxState): TBitmap;
begin
  if CheckboxView=cbUnchecked then
    Result := FCustomUncheckedBitmap
  else if CheckboxView=cbChecked then
    Result := FCustomCheckedBitmap
  else
    Result := FCustomGrayedBitmap;
end;

constructor TmDBGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCustomUnCheckedBitmap := TBitmap.Create;
  FCustomUnCheckedBitmap.LoadFromLazarusResource('dbgridcustomuncheckedcb');
  FCustomCheckedBitmap := TBitmap.Create;
  FCustomCheckedBitmap.LoadFromLazarusResource('dbgridcustomcheckedcb');
  FCustomGrayedBitmap := TBitmap.Create;
  FCustomGrayedBitmap.LoadFromLazarusResource('dbgridcustomgrayedcb');
end;

destructor TmDBGrid.Destroy;
begin
  FCustomUncheckedBitmap.Free;
  FCustomCheckedBitmap.Free;
  FCustomGrayedBitmap.Free;
  inherited Destroy;
end;


initialization
  {$I lcl_dbgrid_customimages.lrs}
end.
