// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mCellDecorationsToXml;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  mCellDecorations, mXML;

procedure SaveCellDecorationToXmlElement (const aSource : TmCellDecoration; aXmlElement : TmXmlElement);
procedure LoadCellDecorationFromXmlElement (aDestination : TmCellDecoration; aXmlElement : TmXmlElement);

procedure SaveCellDecorationsToXmlElement (const aSource : TmCellDecorations; aXmlElement : TmXmlElement);
procedure LoadCellDecorationsFromXmlElement (aDestination : TmCellDecorations; aXmlElement : TmXmlElement);


implementation

procedure SaveCellDecorationToXmlElement(const aSource: TmCellDecoration; aXmlElement: TmXmlElement);
begin
  aXmlElement.SetAttribute('fieldName', aSource.FieldName);
  if aSource.Condition.NotNull then
    aXmlElement.SetAttribute('condition', aSource.Condition.ToString);
  if aSource.BackgroundColor.NotNull then
    aXmlElement.SetAttribute('backgroundColor', aSource.BackgroundColor.AsString);
  if aSource.TextColor.NotNull then
    aXmlElement.SetAttribute('textColor', aSource.TextColor.AsString);
  if aSource.TextBold.NotNull then
    aXmlElement.SetBooleanAttribute('textBold', aSource.TextBold.Value);
  if aSource.TextItalic.NotNull then
    aXmlElement.SetBooleanAttribute('textItalic', aSource.TextItalic.Value);
end;

procedure LoadCellDecorationFromXmlElement(aDestination: TmCellDecoration; aXmlElement: TmXmlElement);
begin
  aDestination.FieldName:= aXmlElement.GetAttribute('fieldName');
  aDestination.Condition.Assign(aXmlElement.GetAttribute('condition', ''), true);
  aDestination.BackgroundColor.Assign(aXmlElement.GetAttribute('backgroundColor', ''), true);
  aDestination.TextColor.Assign(aXmlElement.GetAttribute('textColor', ''), true);
  aDestination.TextBold.Assign(aXmlElement.GetBooleanAttribute('textBold', ''), true);
  aDestination.TextItalic.Assign(aXmlElement.GetBooleanAttribute('textItalic', ''), true);
end;

procedure SaveCellDecorationsToXmlElement(const aSource: TmCellDecorations; aXmlElement: TmXmlElement);
var
  i : integer;
begin
  for i := 0 to aSource.Count - 1 do
    SaveCellDecorationToXmlElement(aSource.Get(i), aXmlElement.AddElement('cellDecoration'));
end;

procedure LoadCellDecorationsFromXmlElement(aDestination: TmCellDecorations; aXmlElement: TmXmlElement);
var
  cursor : TmXmlElementCursor;
  i : integer;
begin
  aDestination.Clear;
  cursor := TmXmlElementCursor.Create(aXmlElement, 'cellDecoration');
  try
    for i := 0 to cursor.Count -1 do
      LoadCellDecorationFromXmlElement(aDestination.Add, cursor.Elements[i]);
  finally
    cursor.Free;
  end;
end;

end.
