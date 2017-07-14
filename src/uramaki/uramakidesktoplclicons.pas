// ------------------------------------------------------------
// Disclaimer:
// ------------------------------------------------------------
// Images from "24x24 Free Pixel Icons"
// http://www.small-icons.com/packs/24x24-free-pixel-icons.htm
// Copyright © 2009 Aha-Soft. All rights reserved.
// License: http://creativecommons.org/licenses/by/3.0/us/
// ------------------------------------------------------------
unit UramakiDesktopLCLIcons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Controls;

const
  ICON_ADD = 0;
  ICON_OPEN = 1;
  ICON_SAVE = 2;
  ICON_CONFIGURE = 3;

type

  { TUramakiDesktopDataModule }

  TUramakiDesktopDataModule = class(TDataModule)
    UramakiDesktopImageList: TImageList;
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

end.

