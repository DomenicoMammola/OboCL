// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mPivotSettings;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
  {$interfaces corba}
{$ENDIF}

uses
  mPivoter, mXML, mSummary, mNullables;

procedure SavePivotConfigurationToXML(const aPivoter : TmPivoter; aXMLElement : TmXmlElement);
procedure LoadPivotConfigurationToXML(const aPivoter : TmPivoter; aXMLElement : TmXmlElement);

implementation

uses
  typinfo, DB,
  mSummaryToXml;

procedure SaveGroupByDef (const aGroupByDef : TmGroupByDef; aXMLElement : TmXmlElement);
begin
  aXMLElement.SetAttribute('fieldName', aGroupByDef.FieldName);
  aXmlElement.SetAttribute('dataType', GetEnumName(TypeInfo(TFieldType), integer(aGroupByDef.DataType)));
  aXmlElement.SetAttribute('operationKind', GetEnumName(TypeInfo(TmGroupByOperationKind), integer(aGroupByDef.OperationKind)));
  aXMLElement.SetAttribute('formula', aGroupByDef.Formula);
  aXMLElement.SetAttribute('displayLabel', aGroupByDef.DisplayLabel);
  aXMLElement.SetAttribute('displayFormat', aGroupByDef.DisplayFormat);
  aXMLElement.SetAttribute('sortBy', GetEnumName(TypeInfo(TmSortByCondition), integer(aGroupByDef.SortBy)));
end;

procedure LoadGroupByDef (aGroupByDef : TmGroupByDef; const aXMLElement : TmXmlElement);
begin
  aGroupByDef.FieldName := aXMLElement.GetAttribute('fieldName');
  aGroupByDef.DataType:= TFieldType(GetEnumValue(TypeInfo(TFieldType), aXmlElement.GetAttribute('dataType')));
  aGroupByDef.OperationKind:= TmGroupByOperationKind(GetEnumValue(TypeInfo(TmGroupByOperationKind), aXmlElement.GetAttribute('operationKind')));
  aXMLElement.GetAttribute('formula', aGroupByDef.Formula);
  aXMLElement.GetAttribute('displayLabel', aGroupByDef.DisplayLabel);
  aXMLElement.GetAttribute('displayFormat', aGroupByDef.DisplayFormat);
  aGroupByDef.SortBy:= TmSortByCondition(GetEnumValue(TypeInfo(TmSortByCondition), aXmlElement.GetAttribute('sortBy')));
end;

procedure SavePropertiesToXmlElement(aPivoter: TmPivoter; const aXMLElement : TmXmlElement);
begin
  aXMLElement.SetBooleanAttribute('showVerticalGrandTotal', (poVerticalGrandTotal in aPivoter.Options));
  aXMLElement.SetBooleanAttribute('showHorizontalGrandTotal', (poHorizontalGrandTotal in aPivoter.Options));
end;

procedure LoadPropertiesFromXmlElement(aPivoter: TmPivoter; const aXMLElement : TmXmlElement);
begin
  if aXMLElement.GetBooleanAttribute('showVerticalGrandTotal', false) then
    aPivoter.Options:= aPivoter.Options + [poVerticalGrandTotal]
  else
    aPivoter.Options:= aPivoter.Options - [poVerticalGrandTotal];

  if aXMLElement.GetBooleanAttribute('showHorizontalGrandTotal', false) then
    aPivoter.Options:= aPivoter.Options + [poHorizontalGrandTotal]
  else
    aPivoter.Options:= aPivoter.Options - [poHorizontalGrandTotal];
end;

procedure SavePivotConfigurationToXML(const aPivoter : TmPivoter; aXMLElement: TmXmlElement);
var
  i : integer;
  curElement : TmXmlElement;
begin
  for i:= 0 to aPivoter.VerticalGroupByDefs.Count - 1 do
  begin
    curElement := aXMLElement.AddElement('verticalGroupByDef');
    SaveGroupByDef(aPivoter.VerticalGroupByDefs.Get(i), curElement);
  end;
  for i:= 0 to aPivoter.HorizontalGroupByDefs.Count - 1 do
  begin
    curElement := aXMLElement.AddElement('horizontalGroupByDef');
    SaveGroupByDef(aPivoter.HorizontalGroupByDefs.Get(i), curElement);
  end;
  SaveSummaryDefinitionsToXmlElement(aPivoter.SummaryDefinitions, aXMLElement);

  SavePropertiesToXmlElement(aPivoter, aXMLElement);
end;

procedure LoadPivotConfigurationToXML(const aPivoter : TmPivoter; aXMLElement: TmXmlElement);
var
  cursor : TmXmlElementCursor;
  i : integer;
begin
  aPivoter.VerticalGroupByDefs.Clear;
  cursor := TmXmlElementCursor.Create(aXMLElement, 'verticalGroupByDef');
  try
    for i := 0 to cursor.Count - 1 do
      LoadGroupByDef(aPivoter.VerticalGroupByDefs.Add, cursor.Elements[i]);
  finally
    cursor.Free;
  end;
  aPivoter.HorizontalGroupByDefs.Clear;
  cursor := TmXmlElementCursor.Create(aXMLElement, 'horizontalGroupByDef');
  try
    for i := 0 to cursor.Count - 1 do
      LoadGroupByDef(aPivoter.HorizontalGroupByDefs.Add, cursor.Elements[i]);
  finally
    cursor.Free;
  end;
  LoadSummaryDefinitionsFromXmlElement(aPivoter.SummaryDefinitions, aXMLElement);
  LoadPropertiesFromXmlElement(aPivoter, aXMLElement);
end;

end.
