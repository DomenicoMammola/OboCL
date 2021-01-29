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


type


  TPivotConfiguration = class
  strict private
    FVerticalGroupByDefs : TmGroupByDefs;
    FHorizontalGroupByDefs : TmGroupByDefs;
    FSummaryDefinitions : TmSummaryDefinitions;
  public

  end;

procedure SavePivotConfigurationToXML(const aVerticalGroupByDefs, aHorizontalGroupByDefs : TmGroupByDefs; const aSummaryDefinitions : TmSummaryDefinitions; aXMLElement : TmXmlElement);
procedure LoadPivotConfigurationToXML(aVerticalGroupByDefs, aHorizontalGroupByDefs : TmGroupByDefs; aSummaryDefinitions : TmSummaryDefinitions; aXMLElement : TmXmlElement);


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
end;

procedure LoadGroupByDef (aGroupByDef : TmGroupByDef; const aXMLElement : TmXmlElement);
begin
  aGroupByDef.FieldName := aXMLElement.GetAttribute('fieldName');
  aGroupByDef.DataType:= TFieldType(GetEnumValue(TypeInfo(TFieldType), aXmlElement.GetAttribute('dataType')));
  aGroupByDef.OperationKind:= TmGroupByOperationKind(GetEnumValue(TypeInfo(TmGroupByOperationKind), aXmlElement.GetAttribute('operationKind')));
  aXMLElement.GetAttribute('formula', aGroupByDef.Formula);
  aXMLElement.GetAttribute('displayLabel', aGroupByDef.DisplayLabel);
  aXMLElement.GetAttribute('displayFormat', aGroupByDef.DisplayFormat);
end;

procedure SavePivotConfigurationToXML(const aVerticalGroupByDefs, aHorizontalGroupByDefs: TmGroupByDefs; const aSummaryDefinitions: TmSummaryDefinitions; aXMLElement: TmXmlElement);
var
  i : integer;
  curElement : TmXmlElement;
begin
  for i:= 0 to aVerticalGroupByDefs.Count - 1 do
  begin
    curElement := aXMLElement.AddElement('verticalGroupByDef');
    SaveGroupByDef(aVerticalGroupByDefs.Get(i), curElement);
  end;
  for i:= 0 to aHorizontalGroupByDefs.Count - 1 do
  begin
    curElement := aXMLElement.AddElement('horizontalGroupByDef');
    SaveGroupByDef(aHorizontalGroupByDefs.Get(i), curElement);
  end;
  SaveSummaryDefinitionsToXmlElement(aSummaryDefinitions, aXMLElement);
end;

procedure LoadPivotConfigurationToXML(aVerticalGroupByDefs, aHorizontalGroupByDefs: TmGroupByDefs; aSummaryDefinitions: TmSummaryDefinitions; aXMLElement: TmXmlElement);
var
  cursor : TmXmlElementCursor;
  i : integer;
begin
  aVerticalGroupByDefs.Clear;
  cursor := TmXmlElementCursor.Create(aXMLElement, 'verticalGroupByDef');
  try
    for i := 0 to cursor.Count - 1 do
      LoadGroupByDef(aVerticalGroupByDefs.Add, cursor.Elements[i]);
  finally
    cursor.Free;
  end;
  aHorizontalGroupByDefs.Clear;
  cursor := TmXmlElementCursor.Create(aXMLElement, 'horizontalGroupByDef');
  try
    for i := 0 to cursor.Count - 1 do
      LoadGroupByDef(aHorizontalGroupByDefs.Add, cursor.Elements[i]);
  finally
    cursor.Free;
  end;
  LoadSummaryDefinitionsFromXmlElement(aSummaryDefinitions, aXMLElement);
end;

end.
