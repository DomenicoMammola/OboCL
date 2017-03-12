// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
//
// This is the implementation of the mXML unit on the OXml library
// which can be found at:
// http://www.kluug.net/oxml.php

unit mXML_oxml;

interface

uses
  Classes, Contnrs,
  mXML, OXmlPDOM;

type
  Tmxml_oxml_PointerShell = class
  public
    MyPointer : pointer;
    constructor Create(aPointer : pointer);
  end;

  TImpl_oxml_mXmlElement = class (TImpl_mXmlElement)
  private
    FNode : PXMLNode;
    procedure RaiseMissingAttributeException(Name :TmXMLString);
    procedure RaiseWrongDateTimeException(Name, Value :TmXMLString);
  public
    constructor Create; override;
  public
    function _AddElement(Name: TmXMLString): TmXmlElement; override;
    function _HasAttribute(const Name: TmXMLString): boolean; override;
    procedure _SetAttribute(Name, Value: TmXmlString); override;

    function _GetAttribute(Name: TmXmlString): TmXmlString; overload; override;
    function _GetAttribute(Name: TmXmlString; Default: TmXmlString): TmXmlString; overload; override;
    procedure _SetDateTimeAttribute(Name : TmXmlString; Value: TDateTime); override;
    function _GetDateTimeAttribute(Name: TmXmlString): TDateTime; overload; override;
    function _GetDateTimeAttribute(Name: TmXmlString; Default : TDateTime): TDateTime; overload; override;
    procedure _SetIntegerAttribute(Name : TmXmlString; Value: integer); override;
    function _GetIntegerAttribute(Name: TmXmlString): integer; overload; override;
    function _GetIntegerAttribute(Name: TmXmlString; Default : integer): integer; overload; override;

    procedure _SetOwner(aOwner : TObject); override;
  end;

  TImpl_oxml_mXmlDocument = class (TImpl_mXmlDocument)
  private
    FXML: IXMLDocument;
    FRoot : PXMLNode;
    FRootElement : TmXmlElement;
  public
    constructor Create; override;
    destructor Destroy; override;
    function _RootElement: TmXmlElement; override;
    function _CreateRootElement(Name: string): TmXmlElement; override;
    procedure _Clear; override;
    procedure _SaveToStream(Stream: TStream); override;
    procedure _LoadFromStream(Stream: TStream); override;
    procedure _SaveToFile(FileName: string); override;
    procedure _LoadFromFile(FileName: string); override;
  end;

  TImpl_oxml_mXmlElementCursor = class (TImpl_mXmlElementCursor)
  private
    FList : TObjectList;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure _SetParent(aParent : TImpl_mXmlElement; aFilter : TmXMLString); override;
    function _GetElement(I : integer) : TmXmlElement; override;
    function _Count : integer; override;
  end;

implementation

uses
  SysUtils, mISOTime;

{ TImpl_oxml_mXmlElement }

constructor TImpl_oxml_mXmlElement.Create;
begin
  FNode := nil;
end;

procedure TImpl_oxml_mXmlElement.RaiseMissingAttributeException (Name :TmXMLString);
begin
  raise EmXmlError.Create('XML attribute ' + Name + ' not found!');
end;

procedure TImpl_oxml_mXmlElement.RaiseWrongDateTimeException (Name, Value :TmXMLString);
begin
  raise EmXmlError.Create('XML attribute ' + Name + ' has value ' + Value + ' not in correct ISO format!');
end;

function TImpl_oxml_mXmlElement._AddElement(Name: TmXMLString): TmXmlElement;
var
  NewNode : PXMLNode;
begin
  NewNode := Self.FNode^.AddChild(Name);
  Result := TmXmlElement.Create(Tmxml_oxml_PointerShell.Create(NewNode));
end;

function TImpl_oxml_mXmlElement._GetAttribute(Name, Default: TmXmlString): TmXmlString;
begin
  Result := Self.FNode^.GetAttributeDef(Name, Default);
end;

function TImpl_oxml_mXmlElement._GetDateTimeAttribute(Name: TmXmlString): TDateTime;
var
  tmp : string;
  tempValue : TDateTime;
begin
  if FNode^.HasAttribute(Name) then
  begin
    tmp := FNode^.GetAttribute(Name);
    if TryISOStrToDateTime(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongDateTimeException(Name, tmp);
  end
  else
    RaiseMissingAttributeException(Name);
end;

function TImpl_oxml_mXmlElement._GetDateTimeAttribute(Name: TmXmlString; Default: TDateTime): TDateTime;
var
  tmp : string;
  tempValue : TDateTime;
begin
  if FNode^.HasAttribute(Name) then
  begin
    tmp := FNode^.GetAttribute(Name);
    if TryISOStrToDateTime(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongDateTimeException(Name, tmp);
  end
  else
    Result := Default;
end;

function TImpl_oxml_mXmlElement._GetIntegerAttribute(Name: TmXmlString): integer;
begin
  if not Self.FNode^.HasAttribute(Name) then
    RaiseMissingAttributeException(Name);
  Result := StrToInt(Self.FNode^.GetAttribute(Name));
end;

function TImpl_oxml_mXmlElement._GetIntegerAttribute(Name: TmXmlString; Default: integer): integer;
begin
  Result := StrToIntDef(Self.FNode^.GetAttribute(Name), Default);
end;

function TImpl_oxml_mXmlElement._GetAttribute(Name: TmXmlString): TmXmlString;
begin
  if not Self.FNode^.HasAttribute(Name) then
    RaiseMissingAttributeException(Name);
  Result := Self.FNode^.GetAttribute(Name);
end;

function TImpl_oxml_mXmlElement._HasAttribute(const Name: TmXMLString): boolean;
begin
  Result := Self.FNode^.HasAttribute(Name);
end;


procedure TImpl_oxml_mXmlElement._SetAttribute(Name, Value: TmXmlString);
begin
  FNode^.SetAttribute(Name, Value);
end;

procedure TImpl_oxml_mXmlElement._SetDateTimeAttribute(Name: TmXmlString; Value : TDateTime);
begin
  FNode^.SetAttribute(Name, mISOTime.ISODateTimeToStr(Value));
end;

procedure TImpl_oxml_mXmlElement._SetIntegerAttribute(Name: TmXmlString; Value: integer);
begin
  FNode^.AddAttribute(Name, IntToStr(Value));
end;

procedure TImpl_oxml_mXmlElement._SetOwner(aOwner: TObject);
begin
  FNode := PXMLNode(Tmxml_oxml_PointerShell(aOwner).MyPointer);
end;

{ TImpl_oxml_mXmlDocument }

constructor TImpl_oxml_mXmlDocument.Create;
begin
  FXML := CreateXMLDoc;
  FXML.Encoding := 'utf-8';
  FXML.WriterSettings.WriteBOM := False;
  FRoot := nil;
  FRootElement := nil;
end;

destructor TImpl_oxml_mXmlDocument.Destroy;
begin
  inherited;
end;

procedure TImpl_oxml_mXmlDocument._Clear;
begin
  FXML.Clear(true);
end;

function TImpl_oxml_mXmlDocument._CreateRootElement(Name: string): TmXmlElement;
begin
  assert (not Assigned(FRoot), 'XML RootElement already assigned!');
  FRoot := FXML.AddChild(Name);
  Result := TmXmlElement.Create(Tmxml_oxml_PointerShell.Create(FRoot));
  FRootElement := Result;
end;

procedure TImpl_oxml_mXmlDocument._LoadFromFile(FileName: string);
begin
  FXML.LoadFromFile(FileName);
  if Assigned(FRootElement) then
    FRootElement.Free;
  FRootElement := nil;
  FRoot := FXML.DocumentElement;
end;

procedure TImpl_oxml_mXmlDocument._LoadFromStream(Stream: TStream);
begin
  FXML.LoadFromStream(Stream);
  if Assigned(FRootElement) then
    FRootElement.Free;
  FRootElement := nil;
  FRoot := FXML.DocumentElement;
end;

function TImpl_oxml_mXmlDocument._RootElement: TmXmlElement;
begin
  if Assigned(FRoot) and (not Assigned(FRootElement)) then
    FRootElement := TmXmlElement.Create(Tmxml_oxml_PointerShell.Create(FRoot));
  Result := FRootElement;
end;

procedure TImpl_oxml_mXmlDocument._SaveToFile(FileName: string);
begin
  FXML.SaveToFile(Filename);
end;

procedure TImpl_oxml_mXmlDocument._SaveToStream(Stream: TStream);
begin
  FXML.SaveToStream(Stream);
end;

{ Tmxml_oxml_PointerShell }

constructor Tmxml_oxml_PointerShell.Create(aPointer: pointer);
begin
  MyPointer := aPointer;
end;

{ TImpl_oxml_mXmlElementCursor }

constructor TImpl_oxml_mXmlElementCursor.Create;
begin
  FList := TObjectList.Create(true);
end;

destructor TImpl_oxml_mXmlElementCursor.Destroy;
begin
  FList.Free;
  inherited;
end;

function TImpl_oxml_mXmlElementCursor._Count: integer;
begin
  Result := FList.Count;
end;

function TImpl_oxml_mXmlElementCursor._GetElement(I: integer): TmXmlElement;
begin
  Result := FList.Items[i] as TmXmlElement;
end;

procedure TImpl_oxml_mXmlElementCursor._SetParent(aParent: TImpl_mXmlElement; aFilter : TmXMLString);
var
  ParentNode : PXMLNode;
  i : integer;
  NewElement : TmXmlElement;
begin
  ParentNode := (aParent as TImpl_oxml_mXmlElement).FNode;
  if CompareText(aFilter, '') = 0 then
  begin
    for i := 0 to ParentNode^.ChildCount -1 do
    begin
      NewElement := TmXmlElement.Create(Tmxml_oxml_PointerShell.Create(ParentNode^.ChildNodes[i]));
      FList.Add(NewElement);
    end;
  end
  else
  begin
    for i := 0 to ParentNode^.ChildCount -1 do
    begin
      if CompareText(aFilter, ParentNode^.ChildNodes[i]^.NodeName) = 0 then
      begin
        NewElement := TmXmlElement.Create(Tmxml_oxml_PointerShell.Create(ParentNode^.ChildNodes[i]));
        FList.Add(NewElement);
      end;
    end;
  end;
end;

end.
