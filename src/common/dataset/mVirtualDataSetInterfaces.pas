// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mVirtualDataSetInterfaces;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

type

(*  TVDDatum = class abstract
  public
    function GetPropertyByFieldName(aFieldName : String) : Variant; virtual; abstract;
  end;

  TVDListDataProvider = class abstract
  public
    function Count : integer; virtual; abstract;
    function GetDatum(aIndex : integer) : TVDDatum; virtual; abstract;
  end;*)

  IVDDatum = interface
    function GetPropertyByFieldName(aFieldName : String) : Variant;
  end;

  IVDListDataProvider = interface
    function Count : integer;
    function GetDatum(aIndex : integer) : IVDDatum;
  end;

implementation

end.
