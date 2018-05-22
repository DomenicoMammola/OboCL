// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mGridIcons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Controls;

const
  GRID_ICON_UP = 0;
  GRID_ICON_DOWN = 1;
  GRID_ICON_FILTER = 2;
  GRID_ICON_UP_FILTER = 3;
  GRID_ICON_DOWN_FILTER = 4;

  GRID_EDITORS_ICON_DOTS = 0;
  GRID_EDITORS_ICON_CALENDAR = 1;
  GRID_EDITORS_ICON_MAGICWAND = 2;

type

  { TmGridIconsDataModule }

  TmGridIconsDataModule = class(TDataModule)
    GridImageList: TImageList;
    GridEditorsImageList: TImageList;
  private
    { private declarations }
  public
    { public declarations }
  end;


var
  GridIconsDataModule : TmGridIconsDataModule;

implementation

{$R *.lfm}

initialization
  GridIconsDataModule := TmGridIconsDataModule.Create(nil);

finalization
  GridIconsDataModule.Free;

end.

