// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mMapsImplDelphi;

interface

uses
  Generics.Collections,
  mMapsImpl;

type

  TmStringDictionaryImplDelphi = class(TmStringDictionaryImpl)
  strict private
    FInternalDictionary : TObjectDictionary<String, TObject>;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure _Add(const aStringKey : String; aObject : TObject); override;
    procedure _Clear; override;
    function _Find(const aStringKey : String) : TObject; override;
  end;

implementation

end.
