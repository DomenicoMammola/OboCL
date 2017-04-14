// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mMapsImpl;

interface

type

  { TmStringDictionaryImpl }

  TmStringDictionaryImpl = class
  public
    constructor Create; virtual; abstract;
    procedure _Add(const aStringKey : String; aObject : TObject); virtual; abstract;
    procedure _Clear; virtual; abstract;
    function _Find(const aStringKey : String) : TObject; virtual; abstract;
    function _Count : integer; virtual; abstract;
    procedure _Remove(const aStringKey : String); virtual; abstract;
  end;

  { TmIntegerDictionaryImpl }

  TmIntegerDictionaryImpl = class
  public
    constructor Create; virtual; abstract;
    procedure _Add(const aIntegerKey : integer; aObject : TObject); virtual; abstract;
    procedure _Clear; virtual; abstract;
    function _Find(const aIntegerKey : integer) : TObject; virtual; abstract;
    function _Count : integer; virtual; abstract;
    procedure _Remove(const aIntegerKey : integer); virtual; abstract;
  end;

  function CreateTmStringDictionary : TmStringDictionaryImpl;
  function CreateTmIntegerDictionary : TmIntegerDictionaryImpl;


implementation

uses
  {$IFDEF FPC}
  mMapsImplFPC
  {$ELSE}
  SysUtils,
  mMapsImplDelphi
  {$ENDIF}
  ;


function CreateTmStringDictionary: TmStringDictionaryImpl;
begin
  {$IFDEF FPC}
    Result := TmStringDictionaryImplFPC.Create;
  {$ELSE}
    Result := TmStringDictionaryImplDelphi.Create;
  {$ENDIF}
end;

function CreateTmIntegerDictionary : TmIntegerDictionaryImpl;
begin
  {$IFDEF FPC}
    Result := TmIntegerDictionaryImplFPC.Create;
  {$ELSE}
    raise Exception.Create('not implemented');
  {$ENDIF}
end;



end.
