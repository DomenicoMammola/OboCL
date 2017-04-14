// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mInterfaces;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

type

  TJavaInterfacedObject = class(TObject, IInterface)
  public
    { IInterface }
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  end;


implementation

function TJavaInterfacedObject.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;
begin
  Result := E_NOINTERFACE;
end;

function TJavaInterfacedObject._AddRef: Integer;
begin
  Result := 1;
end;

function TJavaInterfacedObject._Release: Integer;
begin
  Result := 1;
end;

end.
