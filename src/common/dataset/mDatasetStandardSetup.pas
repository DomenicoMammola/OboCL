// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mDatasetStandardSetup;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  DB,
  mSystemColumns;

function GenerateDisplayLabel(aSourceString : String) : String;
procedure ApplyStandardSettingsToFields (aDataset : TDataset; aStandardFloatFormat : String);

implementation

uses
  SysUtils, StrUtils;

function GenerateDisplayLabel(aSourceString: String): String;
begin
  Result := SysUtils.StringReplace(aSourceString, '_', ' ', [rfReplaceAll]);
  Result := StrUtils.AnsiPropercase(Result,[' ', #9, '\',#13,#10]);;
end;

procedure ApplyStandardSettingsToFields(aDataset: TDataset; aStandardFloatFormat : String);
var
  i : integer;
begin
  for i := 0 to aDataset.Fields.Count - 1 do
  begin
    if aDataset.Fields[i] is TFloatField then
    begin
      (aDataset.Fields[i] as TFloatField).EditFormat:= aStandardFloatFormat;
      (aDataset.Fields[i] as TFloatField).DisplayFormat:= aStandardFloatFormat;
    end;

    if aDataset.Fields[i].DisplayLabel <> '' then
      aDataset.Fields[i].DisplayLabel := GenerateDisplayLabel(aDataset.Fields[i].DisplayLabel)
    else
      aDataset.Fields[i].DisplayLabel := GenerateDisplayLabel(aDataset.Fields[i].FieldName);

    if IsSystemField(aDataset.Fields[i]) then
      aDataset.Fields[i].Visible:= false;
  end;

end;

end.
