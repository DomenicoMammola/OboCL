// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)


unit mSystemColumns;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  DB;

function GetSystemFieldName (aOriginalFieldName : String) : String;
function IsSystemField (aField : TField) : boolean; overload;

implementation

const
  SYSTEM_FIELD_NAME_PREFIX = '_';


function GetSystemFieldName(aOriginalFieldName: String): String;
begin
  Result := SYSTEM_FIELD_NAME_PREFIX + aOriginalFieldName;
end;

function IsSystemField (aField : TField) : boolean;
begin
  Result := (aField.FieldName[1] = SYSTEM_FIELD_NAME_PREFIX);
end;


end.
